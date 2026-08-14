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
  seed = NULL,
  shared_memory = "none",
  verbosity = 1L,
  on_error = "continue"
) {
  check_is_S7(hyperparameters, Hyperparameters)
  check_is_S7(tuner_config, TunerConfig)
  stopifnot(needs_tuning(hyperparameters))

  # Intro ----
  start_time <- intro(
    newline_pre = TRUE,
    caller = "tune_GridSearch",
    verbosity = verbosity - 1L
  )

  # Arguments ----
  algorithm <- hyperparameters@algorithm
  # Backend validation and worker-count handling live in `progress_plapply()`, which
  # owns dispatch for both this path and outer resampling.
  if (is.null(backend)) {
    backend <- "none"
  }

  # Make Grid ----
  grid_params <- get_hyperparams_need_tuning(hyperparameters)
  n_resamples <- tuner_config[["resampler_config"]][["n_resamples"]]
  search_type <- tuner_config[["search_type"]]
  # The single source of the combinations to fit, gated and deduplicated, and
  # what `tuning_grid()` previews.
  n_combinations_expanded <- prod(pmax(lengths(grid_params), 1L))
  param_grid <- tuning_grid(hyperparameters)
  n_combinations_gated <- NROW(param_grid)
  if (search_type == "randomized") {
    # Sampled here, before anything is derived from the grid, so that each
    # selected combination is still run on every resample. Rounding can reach 0
    # on a small grid.
    n_sampled <- max(
      1L,
      round(tuner_config[["randomize_p"]] * n_combinations_gated)
    )
    param_grid <- param_grid[
      sort(sample.int(n_combinations_gated, n_sampled)),
      ,
      drop = FALSE
    ]
    rownames(param_grid) <- NULL
  }
  n_param_combinations <- NROW(param_grid)
  # Resample varies fastest within a combination, which is the order the
  # per-combination aggregation below indexes with `each = n_resamples`.
  res_param_grid <- cbind(
    resample_id = rep(seq_len(n_resamples), times = n_param_combinations),
    param_grid[
      rep(seq_len(n_param_combinations), each = n_resamples),
      ,
      drop = FALSE
    ]
  )
  rownames(res_param_grid) <- NULL
  # NA marks a combination a gate excluded the hyperparameter from, so the
  # columns holding one are exactly those the grid made conditional.
  gated_params <- names(param_grid)[vapply(param_grid, anyNA, logical(1L))]
  param_grid <- cbind(
    param_combo_id = seq_len(n_param_combinations),
    param_grid
  )
  n_res_x_comb <- NROW(res_param_grid)

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
    # Reported against the gated count, which isolates the gate's reduction from
    # a randomized search's sampling.
    if (n_combinations_gated < n_combinations_expanded) {
      msg0(
        "Conditional grid: ",
        fmt(n_combinations_expanded, col = col_tuner, bold = TRUE),
        " combinations reduced to ",
        fmt(n_combinations_gated, col = col_tuner, bold = TRUE),
        "; ",
        oxfordcomma(gated_params),
        ngettext(
          length(gated_params),
          " does not apply to every combination.",
          " do not apply to every combination."
        )
      )
    }
  }

  # Resamples ----
  res <- resample(
    x = x,
    config = tuner_config[["resampler_config"]],
    verbosity = verbosity
  )

  # Grid cells ----
  # `res@resamples` rather than the `Resampler`: the cell only ever indexes the list, an
  # S7 object cannot be placed in shared memory, and the index list is itself a
  # meaningful share of the payload (1.7 MB at n = 50,000, k = 10).
  resamples <- share_payload(
    res@resamples,
    mode = shared_memory,
    backend = backend,
    future_plan = future_plan,
    n_workers = n_workers,
    label = "resample indices",
    verbosity = verbosity
  )
  x_shared <- share_payload(
    x,
    mode = shared_memory,
    backend = backend,
    future_plan = future_plan,
    n_workers = n_workers,
    label = "training data",
    verbosity = verbosity
  )
  weights_shared <- share_payload(
    weights,
    mode = shared_memory,
    backend = backend,
    future_plan = future_plan,
    n_workers = n_workers,
    label = "case weights",
    verbosity = verbosity
  )
  run_grid_cell <- make_grid_cell_runner(
    x = x_shared,
    resamples = resamples,
    res_param_grid = res_param_grid,
    hyperparameters = hyperparameters,
    preprocessor_config = preprocessor_config,
    decomposition_config = decomposition_config,
    weights = weights_shared,
    algorithm = algorithm,
    save_mods = save_mods,
    # Failure policy (specs/observability.md section 7): a grid-cell failure is fatal
    # only under "stop". "stop_outer" tolerates cells and is fatal for outer folds alone.
    fatal = identical(on_error, "stop"),
    verbosity = verbosity
  )

  # Train Grid ----
  # One dispatcher for every backend, shared with outer resampling. Cells receive one RNG
  # substream each, keyed by cell index, so a grid search gives the same answer under
  # "none", "future" and "mirai" at any worker count.
  grid_run <- progress_plapply(
    seq_len(n_res_x_comb),
    run_grid_cell,
    backend = backend,
    n_workers = n_workers,
    future_plan = future_plan,
    seeds = rng_substreams(seed, n_res_x_comb),
    label = "Tuning",
    kind = "tune",
    stop_on_error = identical(on_error, "stop"),
    verbosity = verbosity
  )
  grid_run <- Map(
    normalize_cell_result,
    grid_run,
    seq_len(n_res_x_comb),
    res_param_grid[, "resample_id"]
  )

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
  # Average test errors.
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
  best_row <- select_fn(tune_results[["metrics_validation"]][[metric]])
  # A combination whose aggregated metric is NA or NaN cannot be ranked, and when that is
  # every combination `which.max`/`which.min` select nothing.
  #
  # An empty selection does not stay empty: `as.integer()` turns the resulting zero-row
  # slice into `NA_integer_`, and indexing the grid with that yields a row of NAs. Every
  # tuned hyperparameter is then set to NA -- not left at the value it came in with. What
  # happens next is algorithm-dependent and none of it is what the user asked for: an
  # algorithm that tolerates NA falls back to its own default (LightGBM reverts to
  # `max_nrounds` with early stopping), and one that does not fails its validator with a
  # message naming NULL, which points nowhere near the real problem.
  #
  # A cell failure is one way to get here; a degenerate inner resample is another and does
  # not require any failure at all. `balanced_accuracy` averages per-class recall, so a
  # validation fold missing a class scores NaN, and `mean()` carries that NaN to the
  # combination -- reachable with small multiclass data long before anything errors.
  #
  # Warned rather than fatal: aborting would turn runs that presently return a model into
  # errors. What must not survive is the silence.
  if (length(best_row) == 0L) {
    n_failed <- sum(vapply(
      grid_run,
      function(r) isTRUE(r[["failed"]]),
      logical(1L)
    ))
    rtemis.core::warn(
      "No hyperparameter combination could be scored on ",
      metric,
      " across every inner resample (",
      n_failed,
      " of ",
      n_res_x_comb,
      " grid cells failed), so tuning cannot select a winner: every tuned ",
      "hyperparameter is left as NA and the algorithm falls back to its own default. ",
      "Check for degenerate resamples -- a metric undefined on any one of them, such ",
      "as balanced_accuracy where a fold is missing a class, makes its whole ",
      "combination unrankable."
    )
  }
  best_param_combo_id <- as.integer(
    tune_results[["metrics_validation"]][best_row, 1]
  )
  best_param_combo <- grid_row_values(param_grid, best_param_combo_id, -1)
  if (verbosity > 0L) {
    msg(
      paste0("Best config to ", paste(verb, metric), ":")
    )
    print_tune_finding(param_grid[, -1, drop = FALSE], best_param_combo)
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


# %% make_grid_cell_runner ----
#' Build the per-grid-cell body
#'
#' Returns the closure `progress_plapply()` dispatches once per (hyperparameter
#' combination x inner resample) cell.
#'
#' @details
#' Built by a factory, and taking only `index`, for two reasons. Serializing a closure
#' walks its enclosing environments, so a body defined in `tune_GridSearch()`'s frame
#' would ship that whole frame to every worker. And a body taking `...` could not be fed
#' through `progress_plapply()`, whose own `verbosity`, `label` and `kind` parameters
#' would swallow same-named arguments meant for the cell.
#'
#' Under a tolerant failure policy a failure is captured and returned as a marker the
#' aggregation excludes; under a fatal one it propagates, and the dispatcher stops the
#' run.
#'
#' @param x Tabular data: Training set; each cell slices its own rows.
#' @param resamples List: Inner resample index vectors.
#' @param res_param_grid data.frame: One row per cell, `resample_id` plus the
#' hyperparameter values.
#' @param hyperparameters `Hyperparameters` object.
#' @param preprocessor_config Optional `PreprocessorConfig` object.
#' @param decomposition_config Optional `DecompositionConfig` object.
#' @param weights Optional vector of case weights.
#' @param algorithm Character: Algorithm name, for the algorithm-specific collection.
#' @param save_mods Logical: If TRUE, the fitted model rides back with the result.
#' @param fatal Logical: If TRUE, a cell failure is raised rather than returned.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Function of `(index)` returning the cell result list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_grid_cell_runner <- function(
  x,
  resamples,
  res_param_grid,
  hyperparameters,
  preprocessor_config,
  decomposition_config,
  weights,
  algorithm,
  save_mods,
  fatal,
  verbosity
) {
  force(x)
  force(resamples)
  force(res_param_grid)
  force(hyperparameters)
  force(preprocessor_config)
  force(decomposition_config)
  force(weights)
  force(algorithm)
  force(save_mods)
  force(fatal)
  force(verbosity)
  function(index) {
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
    res1 <- resamples[[res_param_grid[index, "resample_id"]]]
    dat_train1 <- x[res1, ]
    weights1 <- weights[res1]
    dat_valid1 <- x[-res1, ]
    hyperparams1 <- update(
      hyperparameters,
      grid_row_values(res_param_grid, index, 2:NCOL(res_param_grid)),
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
          preprocessor_config = preprocessor_config,
          decomposition_config = decomposition_config,
          hyperparameters = hyperparams1,
          weights = weights1,
          verbosity = verbosity - 1L
        )
      )
    }
    # Failure policy (specs/observability.md section 7): under a tolerant policy a
    # grid-cell failure is captured and returned as a marker (non-fatal); otherwise it
    # propagates. Timestamps bracket the actual cell run so the host can record real
    # durations on the synthesized grid_cell nodes (rather than a zero-width interval).
    cell_t_start <- Sys.time()
    mod1 <- if (fatal) {
      run_cell()
    } else {
      tryCatch(run_cell(), error = function(e) e)
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
    if (save_mods) {
      out1[["mod1"]] <- mod1
    }
    out1
  }
} # /rtemis::make_grid_cell_runner


# %% normalize_cell_result ----
#' Normalize one grid cell result
#'
#' The cell body returns a structured result, failure included. A condition arrives
#' instead only when the dispatch itself failed -- a payload that could not be serialized,
#' or a worker that died -- which is outside the body's own `tryCatch`. Both shapes are
#' flattened here so the node synthesis and metric aggregation downstream have one thing
#' to read.
#'
#' @param res List or condition: One element of the `progress_plapply()` result.
#' @param index Integer: Cell index, used when the result carries no id of its own.
#' @param resample_id Integer: Inner resample the cell belonged to.
#'
#' @return List in the cell result shape.
#'
#' @author EDG
#' @keywords internal
#' @noRd
normalize_cell_result <- function(res, index, resample_id) {
  if (!inherits(res, "condition")) {
    return(res)
  }
  now <- Sys.time()
  list(
    id = index,
    resample_id = resample_id,
    metrics_training = NULL,
    metrics_validation = NULL,
    type = NA_character_,
    hyperparameters = NULL,
    failed = TRUE,
    error = conditionMessage(res),
    t_start = now,
    t_end = now
  )
} # /rtemis::normalize_cell_result


# %% print_tune_finding ----
#' Print tuning results
#'
#' Prints set of search values and best value in the form {1, 3, 5} => 3
#' for each hyperparameter that was tuned.
#'
#' Reads the searched values off the grid, so that a hyperparameter a gate added
#' to it -- one held at a single value, but dropped from the combinations that
#' cannot use it -- is reported too. Both grid markers for an unset value print
#' as the NULL they become: NA from a closed gate, and `expand_grid()`'s "null"
#' sentinel from a hyperparameter left to be determined by tuning.
#'
#' @param param_grid data.frame: The tuning grid, without its `param_combo_id`
#'   column.
#' @param best_param_combo Named list: The winning combination.
#' @param pad Integer: Label column width.
#'
#' @author EDG
#' @keywords internal
#' @noRd
print_tune_finding <- function(param_grid, best_param_combo, pad = 22L) {
  show <- function(x) {
    # A container tunable's candidate is a whole vector -- one architecture, one
    # per grid cell -- so it is parenthesized to stay legible beside the scalar
    # hyperparameters printed on the same line.
    if (length(x) == 0L) {
      return("NULL")
    }
    if (length(x) > 1L) {
      return(paste0("(", paste(x, collapse = ", "), ")"))
    }
    if (is.na(x) || identical(x, "null")) "NULL" else as.character(x)
  }
  # Make list of search values and best value
  tfl <- lapply(names(param_grid), function(nm) {
    paste0(
      "{",
      paste(
        vapply(unique(param_grid[[nm]]), show, character(1L)),
        collapse = ", "
      ),
      "}",
      " => ",
      bold(show(best_param_combo[[nm]]))
    )
  })
  names(tfl) <- names(param_grid)
  # Capture output to sync with msg stream (stderr)
  out <- utils::capture.output(printls(tfl, print_class = FALSE, pad = pad))
  message(paste(out, collapse = "\n"))
} # /rtemis::print_tune_finding
