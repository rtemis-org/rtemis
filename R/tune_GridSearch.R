# tune_GridSearch.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% tune_GridSearch ----
#' \pkg{rtemis} internal: Grid Search for Hyperparameter Tuning of \pkg{rtemis} Learners
#'
#' Train models using a combination of parameter values for model selection
#'
#' @details
#' Note that weights, if defined (and not NULL), should be passed directly to `grid_search`
#' as they need to be resampled along `x` and `y`, and should not be passed along with
#' `grid_params`. `ifw` and `ifw_type` should be passed as part of `grid_params`
#' and will be passed on to the learner.
#' Includes a algorithm-specific extraction of config that are determined internally,
#' such as `lambda` for `GLMNET`, `nrounds` for `LightGBM`, etc.
#'
#' The current implementation allows running sequentially either directly using lapply + cli
#' progress, or using a sequential future plan. The former may give better debugging information.
#' The latter may be helpful to test that the future parallelization setup works correctly.
#'
#' @param x tabular data: Training set.
#' @param hyperparameters `Hyperparameters` object created with a learner's `setup_*` function.
#' @param tuner_config `TunerConfig` object created with [setup_GridSearch].
#' @param preprocessor_config Optional `PreprocessorConfig` object: Applied within each tuning
#' fold so hyperparameters are evaluated on preprocessed data.
#' @param decomposition_config Optional `DecompositionConfig` object: Setup using a decomposition
#' `setup_`*` function.
#' @param weights Vector: Class weights.
#' @param save_mods Logical: Save models in tuning results.
#' @param n_workers Integer: Number of workers to use for parallel processing.
#' @param backend Character: Type of parallelization to use. Options are "none", "future",
#' or "mirai".
#' @param future_plan Character: Future plan to use if `backend` is "future".
#' @param verbosity Integer: Verbosity level.
#'
#' @return `GridSearch` object.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
tune_GridSearch <- function(
  x,
  hyperparameters,
  tuner_config,
  preprocessor_config = NULL,
  decomposition_config = NULL,
  weights = NULL,
  save_mods = FALSE,
  n_workers = 1L,
  backend = NULL,
  future_plan = NULL,
  verbosity = 1L,
  on_error = "continue"
) {
  check_is_S7(hyperparameters, Hyperparameters)
  check_is_S7(tuner_config, TunerConfig)
  stopifnot(needs_tuning(hyperparameters))

  # Dependencies ----
  if (backend == "future") {
    check_dependencies("futurize", "future.apply", "progressr")
    if (!is.null(future_plan) && future_plan == "sequential") {
      if (n_workers > 1L) {
        rtemis.core::abort(
          "Requested 'sequential' future plan, which supports 1 worker, but ",
          n_workers,
          " workers were requested.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
    }
  } else if (backend == "mirai") {
    check_dependencies("mirai")
  }

  # Intro ----
  start_time <- intro(
    newline_pre = TRUE,
    caller = "tune_GridSearch",
    verbosity = verbosity - 1L
  )

  # Arguments ----
  algorithm <- hyperparameters@algorithm

  # Parallel Processing Strategy ----
  # If backend is NULL, default to "none"
  if (is.null(backend)) {
    backend <- "none"
  }

  # If backend is "future" or "mirai" with n_workers = 1, we execute
  # sequentially using the respective backend just to test that the
  # parallelization setup works.
  # If the user wants standard sequential execution, they should use/leave
  # backend = "none" (default).
  if (backend != "none" && n_workers == 1L) {
    if (verbosity > 0L) {
      msg0(
        "Using ",
        backend,
        " with 1 worker"
      )
    }
  }

  # Make Grid ----
  grid_params <- get_hyperparams_need_tuning(hyperparameters)
  n_resamples <- tuner_config[["resampler_config"]][["n"]]
  search_type <- tuner_config[["search_type"]]
  # expand_grid converts NULL to "null" for expansion to work.
  param_grid <- expand_grid(grid_params, stringsAsFactors = FALSE)
  param_grid <- cbind(param_combo_id = seq_len(NROW(param_grid)), param_grid)
  n_param_combinations <- NROW(param_grid)
  res_param_grid <- expand_grid(
    c(list(resample_id = seq_len(n_resamples)), grid_params),
    stringsAsFactors = FALSE
  )
  n_res_x_comb <- NROW(res_param_grid)
  if (search_type == "randomized") {
    index_per_resample <- sample(
      n_param_combinations,
      round(tuner_config[["randomize_p"]] * n_param_combinations)
    )
    res_param_grid <- res_param_grid[rep(index_per_resample, n_resamples), ]
  }

  # Intro pt. 2 ----
  if (verbosity > 0L) {
    msg0(
      fmt("<> ", col = col_tuner, bold = TRUE),
      "Tuning ",
      algorithm,
      " by ",
      search_type,
      " grid search with ",
      desc(tuner_config@config[["resampler_config"]]),
      "..."
    )
    msg0(
      fmt(n_param_combinations, col = col_tuner, bold = TRUE),
      ngettext(
        n_param_combinations,
        " parameter combination x ",
        " parameter combinations x "
      ),
      fmt(n_resamples, col = col_tuner, bold = TRUE),
      " resamples: ",
      fmt(n_res_x_comb, col = col_tuner, bold = TRUE),
      " models total",
      " (",
      Sys.getenv("R_PLATFORM"),
      ")."
    )
  }

  # Resamples ----
  res <- resample(
    x = x,
    config = tuner_config[["resampler_config"]],
    verbosity = verbosity
  )

  # learner1 ----
  # `ptn` is the progressr progressor ticked by learner1 under the future
  # backend. It is created inside the `with_progress()` block below (in this
  # same frame, so learner1's closure sees it) - progressr requires the
  # progressor to be created within an active progression session.
  ptn <- NULL
  learner1 <- function(
    index,
    x,
    res,
    res_param_grid,
    hyperparameters,
    preprocessor_config,
    decomposition_config,
    weights,
    verbosity,
    save_mods,
    n_res_x_comb,
    on_error = "continue"
  ) {
    if (verbosity > 1L) {
      info(
        "Running grid line #",
        fmt(index, bold = TRUE),
        "/",
        NROW(res_param_grid),
        "...",
        caller = "tune_GridSearch"
      )
    }
    res1 <- res[[res_param_grid[index, "resample_id"]]]
    dat_train1 <- x[res1, ]
    weights1 <- weights[res1]
    dat_valid1 <- x[-res1, ]
    hyperparams1 <- hyperparameters
    hyperparams1 <- update(
      hyperparams1,
      as.list(res_param_grid[index, 2:NCOL(res_param_grid), drop = FALSE]),
      tuned = TUNED_STATUS_TUNING # Hyperparameters are being tuned
    )

    # Detach any active observability session so the inner train() is opaque to the host
    # graph; the host host-synthesizes one grid_cell node per cell (uniform across
    # backends). In daemons the session is already NULL, so this is a harmless no-op.
    saved_session <- live[["session"]]
    live[["session"]] <- NULL
    on.exit(live[["session"]] <- saved_session, add = TRUE)
    run_cell <- function() {
      do_call(
        "train",
        args = list(
          x = dat_train1,
          dat_validation = dat_valid1,
          algorithm = hyperparams1@algorithm,
          preprocessor_config = preprocessor_config,
          decomposition_config = decomposition_config,
          hyperparameters = hyperparams1,
          weights = weights1,
          verbosity = verbosity - 1L
        )
      )
    }
    # Failure policy (specs/observability.md section 7): under "continue" a grid-cell
    # failure is captured and returned as a marker (non-fatal); otherwise it propagates.
    # Timestamps bracket the actual cell run so the host can record real durations on the
    # synthesized grid_cell nodes (rather than a zero-width now/now interval).
    cell_t_start <- Sys.time()
    mod1 <- if (identical(on_error, "continue")) {
      tryCatch(run_cell(), error = function(e) e)
    } else {
      run_cell()
    }
    cell_t_end <- Sys.time()
    if (inherits(mod1, "condition")) {
      return(list(
        id = index,
        resample_id = res_param_grid[index, "resample_id"],
        metrics_training = NULL,
        metrics_validation = NULL,
        type = NA_character_,
        hyperparameters = hyperparams1,
        failed = TRUE,
        error = conditionMessage(mod1),
        t_start = cell_t_start,
        t_end = cell_t_end
      ))
    }

    out1 <- list(
      id = index,
      resample_id = res_param_grid[index, "resample_id"],
      metrics_training = mod1@metrics_training,
      metrics_validation = mod1@metrics_validation,
      type = mod1@type,
      hyperparameters = hyperparams1,
      failed = FALSE,
      t_start = cell_t_start,
      t_end = cell_t_end
    )

    # Algorithm-specific params ----
    # => add to hyperparameters
    if (algorithm == "GLMNET") {
      out1[["hyperparameters"]]@hyperparameters[["lambda.min"]] <- mod1@model[[
        "lambda.min"
      ]]
      out1[["hyperparameters"]]@hyperparameters[["lambda.1se"]] <- mod1@model[[
        "lambda.1se"
      ]]
    }
    if (algorithm == "LightGBM") {
      # Check best_iter is meaningful, otherwise issue message and set to 100L
      best_iter <- mod1@model[["best_iter"]]
      if (is.null(best_iter) || best_iter == -1 || best_iter == 0) {
        info(
          paste(
            "best_iter returned from lightgbm:",
            best_iter,
            "- setting to 100L"
          )
        )
        best_iter <- 100L
      }
      out1[["hyperparameters"]]@hyperparameters[["best_iter"]] <- best_iter
    }
    # if (algorithm %in% c("LINAD", "LINOA")) {
    #   out1$est.n.leaves <- mod1$mod$n.leaves
    # }
    # if (algorithm == "LIHADBoost") {
    #   out1$sel.n.steps <- mod1$mod$selected.n.steps
    # }
    if (save_mods) {
      out1[["mod1"]] <- mod1
    }
    if (backend == "future") {
      ptn(sprintf("Tuning resample %i/%i", index, n_res_x_comb))
    }
    out1
  } # /learner1

  # Train Grid ----
  if (backend == "none") {
    if (verbosity > 0L) {
      msg("Tuning in sequence")
    }
    # Sequential execution with rtemis.core progress. `learner1` is closed
    # over rather than forwarded through `...` because its `x` and
    # `verbosity` arguments would collide with progress_lapply()'s own
    # parameters.
    grid_run <- progress_lapply(
      seq_len(n_res_x_comb),
      function(index) {
        learner1(
          index,
          x = x,
          res = res,
          hyperparameters = hyperparameters,
          res_param_grid = res_param_grid,
          preprocessor_config = preprocessor_config,
          decomposition_config = decomposition_config,
          weights = weights,
          verbosity = verbosity,
          save_mods = save_mods,
          n_res_x_comb = n_res_x_comb,
          on_error = on_error
        )
      },
      label = "Tuning",
      kind = "tune",
      verbosity = verbosity
    )
  } else if (backend == "future") {
    # Future parallelization
    future_plan <- set_preferred_plan(
      requested_plan = future_plan,
      n_workers = n_workers,
      envir = parent.frame(),
      verbosity = verbosity
    )
    if (verbosity > 0L) {
      msg0(
        "Tuning using future (",
        bold(future_plan),
        "); N workers: ",
        bold(n_workers)
      )
    }
    if (verbosity > 1L) {
      # verify plan set by set_preferred_plan with envir
      info("Current future plan:")
      print(future::plan())
    }
    # Workers signal `progression` conditions via `ptn()`; future relays
    # them to this session, where handler_rtemis() renders them through the
    # rtemis progress system (breadcrumb line, sink envelopes). Relay
    # granularity is bounded by chunk resolution.
    grid_run <- progressr::with_progress(
      {
        ptn <- progressr::progressor(steps = NROW(res_param_grid))
        lapply(
          X = seq_len(n_res_x_comb),
          FUN = learner1,
          x = x,
          res = res,
          hyperparameters = hyperparameters,
          res_param_grid = res_param_grid,
          preprocessor_config = preprocessor_config,
          decomposition_config = decomposition_config,
          weights = weights,
          verbosity = verbosity,
          save_mods = save_mods,
          n_res_x_comb = n_res_x_comb,
          on_error = on_error
        ) |>
          futurize::futurize(seed = TRUE, globals = FALSE)
      },
      handlers = handler_rtemis(
        label = "Tuning",
        kind = "tune",
        verbosity = verbosity
      ),
      # progressr gates delivery on `progressr.enable` (FALSE in
      # non-interactive sessions); force it - the rtemis progress system
      # does its own verbosity gating and non-interactive rendering.
      enable = TRUE
    )
  } else if (backend == "mirai") {
    if (verbosity > 0L) {
      msg("Tuning using mirai; N workers:", bold(n_workers))
    }
    mirai::daemons(n_workers, dispatcher = TRUE)
    on.exit(mirai::daemons(0L))
    grid_run <- mirai::mirai_map(
      .x = seq_len(n_res_x_comb),
      .f = learner1,
      .args = list(
        x = x,
        res = res,
        hyperparameters = hyperparameters,
        res_param_grid = res_param_grid,
        preprocessor_config = preprocessor_config,
        decomposition_config = decomposition_config,
        weights = weights,
        verbosity = verbosity,
        save_mods = save_mods,
        n_res_x_comb = n_res_x_comb,
        on_error = on_error
      )
    )
  }

  # Metric ----
  type <- supervised_type(x)
  metric <- tuner_config@config[["metric"]]
  maximize <- tuner_config@config[["maximize"]]
  if (is.null(metric)) {
    if (type == "Classification") {
      metric <- "balanced_accuracy"
    } else if (type == "Regression") {
      metric <- "mse"
    } else {
      metric <- "Concordance"
    }
    tuner_config@config[["metric"]] <- metric
  }
  if (is.null(maximize)) {
    maximize <- metric %in%
      c("accuracy", "balanced_accuracy", "rsq", "r")
    tuner_config@config[["maximize"]] <- maximize
  }
  select_fn <- if (maximize) which.max else which.min
  verb <- if (maximize) "maximize" else "minimize"

  # Aggregate ----
  # Average test errors
  # if using mirai, wait for all to finish. mirai does not relay progressr
  # conditions, so poll resolution on this session and report through the
  # rtemis progress system (replaces mirai's own `[.progress]` cli bar).
  if (backend == "mirai") {
    tune_progress <- progress_begin(
      n_res_x_comb,
      label = "Tuning",
      kind = "tune",
      verbosity = verbosity
    )
    poll_interval <- max(
      as.numeric(getOption("rtemis.progress_throttle", 0.1)),
      0.05
    )
    repeat {
      n_done <- n_res_x_comb -
        sum(vapply(grid_run, mirai::unresolved, logical(1L)))
      progress_update(tune_progress, current = n_done)
      if (n_done >= n_res_x_comb) {
        break
      }
      Sys.sleep(poll_interval)
    }
    grid_run <- grid_run[]
    progress_end(tune_progress, status = "done")
  }
  # Host-synthesize one grid_cell node per cell under the active "tune" node, with status
  # and error filled from the returned results. See specs/observability.md section 4.
  for (r in grid_run) {
    failed_cell <- isTRUE(r[["failed"]])
    session_add_node(
      "grid_cell",
      label = paste0(
        "#",
        r[["id"]],
        " (resample ",
        r[["resample_id"]],
        ")"
      ),
      status = if (failed_cell) "error" else "ok",
      meta = list(resample_id = r[["resample_id"]], error = r[["error"]]),
      t_start = r[["t_start"]],
      t_end = r[["t_end"]]
    )
  }
  node_meta(list(n_combos = n_param_combinations, n_inner = n_resamples))
  # Tolerant metric extraction: failed cells (only possible under on_error = "continue")
  # become NA rows so the combo aggregation excludes them. With no failures this is
  # identical to the previous extraction.
  ok_idx <- which(
    !vapply(
      grid_run,
      function(r) isTRUE(r[["failed"]]),
      logical(1L)
    )
  )
  if (length(ok_idx) == 0L) {
    rtemis.core::abort(
      "All ",
      n_res_x_comb,
      " tuning grid cells failed; cannot select hyperparameters.",
      class = c("rtemis_error", "rtemis_runtime_error")
    )
  }
  extract_row <- function(r, slot) {
    m <- r[[slot]]
    if (type == "Classification") {
      unlist(m@metrics[["overall"]])
    } else {
      unlist(m@metrics)
    }
  }
  tmpl_tr <- names(extract_row(grid_run[[ok_idx[1L]]], "metrics_training"))
  tmpl_va <- names(extract_row(grid_run[[ok_idx[1L]]], "metrics_validation"))
  row_or_na <- function(r, slot, tmpl) {
    if (isTRUE(r[["failed"]]) || is.null(r[[slot]])) {
      stats::setNames(rep(NA_real_, length(tmpl)), tmpl)
    } else {
      extract_row(r, slot)
    }
  }
  metrics_training_all <- as.data.table(do.call(
    rbind,
    lapply(grid_run, row_or_na, "metrics_training", tmpl_tr)
  ))
  metrics_validation_all <- as.data.table(do.call(
    rbind,
    lapply(grid_run, row_or_na, "metrics_validation", tmpl_va)
  ))
  # appease R CMD check
  param_combo_id <- NULL
  metrics_validation_all[,
    param_combo_id := rep(
      seq_len(n_param_combinations),
      each = n_resamples
    )
  ]
  metrics_training_all[,
    param_combo_id := rep(
      seq_len(n_param_combinations),
      each = n_resamples
    )
  ]
  metrics_training_by_combo_id <- metrics_training_all[,
    lapply(
      .SD,
      get(tuner_config[["metrics_aggregate_fn"]])
    ),
    by = param_combo_id
  ]
  metrics_validation_by_combo_id <- metrics_validation_all[,
    lapply(
      .SD,
      get(tuner_config[["metrics_aggregate_fn"]])
    ),
    by = param_combo_id
  ]

  tune_results <- list(
    param_grid = param_grid,
    metrics_training = metrics_training_by_combo_id,
    metrics_validation = metrics_validation_by_combo_id
  )

  # Algorithm-specific collection ----
  # N of iterations is the one hyperparameter that may be determined
  # automatically, we therefore need to extract it and average it
  ## GLMNET ----
  if (algorithm == "GLMNET") {
    if (is.null(grid_params[["lambda"]])) {
      # if lambda was NULL, cv.glmnet was run and optimal lambda was estimated
      # For each i in grid_run, get grid_run[[i]]$hyperparameters[[grid_run[[i]]$hyperparameters$which_lambda_cv]]
      if (verbosity > 1L) {
        info("Extracting best lambda from GLMNET models...")
      }
      lambda_cv2 <- data.table(
        lambda = sapply(
          grid_run,
          function(x) {
            x[["hyperparameters"]][[x[["hyperparameters"]][[
              "which_lambda_cv"
            ]]]]
          }
        )
      )
      lambda_cv2[,
        param_combo_id := rep(
          seq_len(n_param_combinations),
          each = n_resamples
        )
      ]
      lambda_by_param_combo_id <- lambda_cv2[,
        lapply(.SD, get(tuner_config[["metrics_aggregate_fn"]])),
        by = param_combo_id
      ]
      # Replace NULL lambda in tune_results$param_grid with average value of CV-squared lambda
      stopifnot(tune_results[["param_grid"]][["lambda"]] == "null")
      param_grid[["lambda"]] <- tune_results[["param_grid"]][[
        "lambda"
      ]] <- lambda_by_param_combo_id[["lambda"]]
    }
  } # /GLMNET

  ## LightGBM ----
  if (algorithm == "LightGBM") {
    if (is.null(grid_params[["nrounds"]])) {
      if (verbosity > 1L) {
        info("Extracting best N of iterations from LightGBM models...")
      }
      nrounds_cv <- data.table(
        nrounds = sapply(grid_run, \(x) x[["hyperparameters"]][["best_iter"]])
      )
      nrounds_cv[["param_combo_id"]] <- rep(
        seq_len(n_param_combinations),
        each = n_resamples
      )
      nrounds_by_param_combo_id <- nrounds_cv[,
        lapply(.SD, get(tuner_config[["metrics_aggregate_fn"]])),
        by = param_combo_id
      ]
      # Replace NULL nrounds in tune_results$param_grid with average value of Res nrounds
      stopifnot(tune_results[["param_grid"]][["nrounds"]] == "null")
      param_grid[["nrounds"]] <- tune_results[["param_grid"]][["nrounds"]] <-
        as.integer(round(nrounds_by_param_combo_id[["nrounds"]]))
    }
  } # /LightGBM

  ## GBM, H2OGBM ----
  # if (algorithm %in% c("H2OGBM", "GBM", "GBM3")) {
  #   est.n.trees.all <- data.frame(n.trees = plyr::laply(
  #     grid_run,
  #     function(x) x$est.n.trees
  #   ))
  #   est.n.trees.all$param_combo_id <- rep(seq_len(n_param_combinations), each = n_resamples)
  #   est.n.trees.by.param_combo_id <- aggregate(
  #     n.trees ~ param_combo_id, est.n.trees.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     n.trees = round(est.n.trees.by.param_combo_id$n.trees),
  #     tune_results
  #   )
  #   n_params <- n_params + 1
  # } # /GBM, H2OGBM

  ## XGBoost ----
  # if (algorithm == "XGBoost") {
  #   if (verbosity > 1L) {
  #     msg(highlight("Extracting best N of iterations from XGBoost models..."))
  #   }
  #   est.nrounds.all <- data.frame(nrounds = plyr::laply(
  #     grid_run,
  #     \(m) m$best_iteration
  #   ))
  #   est.nrounds.all$param_combo_id <- rep(seq_len(n_param_combinations),
  #     each = n_resamples
  #   )
  #   est.nrounds.by.param_combo_id <- aggregate(
  #     nrounds ~ param_combo_id, est.nrounds.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     nrounds = round(est.nrounds.by.param_combo_id$nrounds),
  #     tune_results
  #   )
  #   n_params <- n_params + 1
  # } /XGBoost

  ## LINAD ----
  # if (algorithm %in% c("LINAD", "LINOA")) {
  #   if (verbosity > 1L) {
  #     info("Extracting best N leaves from LINAD models...")
  #   }
  #   est.n.leaves.all <- data.frame(n.leaves = plyr::laply(
  #     grid_run,
  #     \(x) ifelse(length(x$est.n.leaves) == 0, 1, x$est.n.leaves)
  #   ))
  #   est.n.leaves.all$param_combo_id <- rep(seq_len(n_param_combinations),
  #     each = n_resamples
  #   )
  #   est.n.leaves.by.param_combo_id <- aggregate(
  #     n.leaves ~ param_combo_id, est.n.leaves.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     n.leaves =
  #       round(est.n.leaves.by.param_combo_id$n.leaves), tune_results
  #   )
  #   n_params <- n_params + 1
  # } # /LINAD, LINOA

  ## LIHADBoost ----
  # if (algorithm == "LIHADBoost") {
  #   if (verbosity > 1L) {
  #     msg(highlight("Extracting best N steps from LIHADBoost models..."))
  #   }
  #   est.n.steps.all <- data.frame(n.steps = plyr::laply(
  #     grid_run,
  #     \(x) x$sel.n.steps
  #   ))
  #   est.n.steps.all$param_combo_id <- rep(seq_len(n_param_combinations),
  #     each = n_resamples
  #   )
  #   est.n.steps.by.param_combo_id <- aggregate(
  #     n.steps ~ param_combo_id, est.n.steps.all,
  #     metrics_aggregate_fn
  #   )
  #   tune_results <- cbind(
  #     n.steps = round(est.n.steps.by.param_combo_id$n.steps),
  #     tune_results
  #   )
  #   n_params <- n_params + 1
  # } # /LIHADBoost

  # Consider explicitly sorting hyperparam values in increasing order,
  # so that in case of tie, lowest value is chosen -
  # if that makes sense, e.g. n.leaves, etc.
  best_param_combo_id <- as.integer(
    tune_results[["metrics_validation"]][
      select_fn(tune_results[["metrics_validation"]][[metric]]),
      1
    ]
  )
  best_param_combo <- as.list(param_grid[best_param_combo_id, -1, drop = FALSE])
  if (verbosity > 0L) {
    msg(
      paste0("Best config to ", paste(verb, metric), ":")
    )
    print_tune_finding(grid_params, best_param_combo)
  }

  # Outro ----
  # Since this is always called from within `train()`, we don't want to print "Completed..."
  outro(start_time, verbosity = verbosity - 1L)

  if (verbosity > 0L) {
    msg(
      fmt("</>", col = col_tuner, bold = TRUE),
      "Tuning done."
    )
  }

  # => add optional mods field to GridSearch
  # if (save_mods) mods <- grid_run
  GridSearch(
    hyperparameters = hyperparameters,
    tuner_config = tuner_config,
    tuning_results = list(
      param_grid = param_grid,
      training = metrics_training_by_combo_id,
      validation = metrics_validation_by_combo_id
    ),
    best_hyperparameters = best_param_combo
  )
} # /rtemis::tune_GridSearch


# %% print_tune_finding ----
#' Print tuning results
#'
#' Prints set of search values and best value in the form {1, 3, 5} => 3
#' for each hyperparameter that was tuned.
#'
#' @author EDG
#' @keywords internal
#' @noRd
print_tune_finding <- function(grid_params, best_param_combo, pad = 22L) {
  # Make list of search values and best value
  tfl <- lapply(seq_along(grid_params), function(i) {
    paste0(
      "{",
      paste(grid_params[[i]], collapse = ", "),
      "}",
      " => ",
      bold(best_param_combo[[names(grid_params)[i]]])
    )
  })
  names(tfl) <- names(grid_params)
  # Capture output to sync with msg stream (stderr)
  out <- utils::capture.output(printls(tfl, print_class = FALSE, pad = pad))
  message(paste(out, collapse = "\n"))
} # /rtemis::print_tune_finding
