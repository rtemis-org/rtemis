# train.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train ----
#' Train Supervised Learning Models
#'
#' @description
#' Preprocess, tune, train, and test supervised learning models using nested resampling in a single
#' call.
#'
#' @param x Tabular data, i.e. data.frame, data.table, or tbl_df (tibble): Training set data.
#' @param dat_validation Tabular data: Validation set data.
#' @param dat_test Tabular data: Test set data.
#' @param weights Optional vector of case weights.
#' @param algorithm Character: Algorithm to use. Can be left NULL, if `hyperparameters` is defined.
#' @param preprocessor_config Optional PreprocessorConfig object: Setup using [setup_Preprocessor].
#' @param decomposition_config Optional DecompositionConfig object: Setup using a decomposition
#'  `setup_*` function.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param tuner_config TunerConfig object: Setup using [setup_GridSearch].
#' @param outer_resampling_config Optional ResamplerConfig object: Setup using [setup_Resampler].
#' This defines the outer resampling method, i.e. the splitting into training and test sets for the
#' purpose of assessing model performance. If NULL, no outer resampling is performed, in which case
#' you might want to use a `dat_test` dataset to assess model performance on a single test set.
#' @param execution_config `ExecutionConfig` object: Setup using [setup_ExecutionConfig]. This
#' allows you to set backend ("future", "mirai", or "none"), number of workers, and future plan if
#' using `backend = "future"`.
#' @param question Optional character string defining the question that the model is trying to
#' answer.
#' @param outdir Character, optional: String defining the output directory.
#' @param verbosity Integer: Verbosity level.
#' @param progress Optional function: Callback invoked at progress
#'   checkpoints during training. When supplied, called at each outer
#'   resampling fold boundary as
#'   `progress(stage, current, total, message)`:
#'   \describe{
#'     \item{`stage`}{Character. Currently `"outer_fold"`.}
#'     \item{`current`}{Integer. 1-based index of the fold about to run.}
#'     \item{`total`}{Integer. Total number of outer folds.}
#'     \item{`message`}{Character. Human-readable line, e.g.
#'       `"Outer fold 2/5"`.}
#'   }
#'   When `NULL`, the existing `cli::cli_progress_along()`
#'   interactive progress bar runs untouched. Designed for non-interactive
#'   callers (e.g. `rtemis.server`) that need to forward fold progress
#'   over a wire protocol; errors raised by the callback are swallowed
#'   so a broken sink cannot interrupt training.
#' @param ... Not used.
#'
#' @details
#' **Online book & documentation**
#'
#' See [docs.rtemis.org/r](https://docs.rtemis.org/r/) for detailed documentation.
#'
#' **Preprocessing**
#'
#' There are many different stages at which preprocessing could be applied, when running a
#' supervised learning pipeline with nested resampling. Some operations are best done before
#' passing data to `train()`:
#'
#' - Duplicate rows should be removed before resampling, so that duplicates don't end up in
#' different resamples, e.g. one in training and one in test.
#' - Constant columns should be removed before resampling. A column may appear constant in a small
#' resample, even if it is not constant in the full dataset. Removing it inconsistently will
#' throw an error during prediction.
#' - All data-dependent preprocessing steps need to be performed on training data only and applied
#' on validation and test data, e.g. scaling, centering, imputation.
#'
#' User-defined preprocessing through `preprocessor_config` is applied on training set data,
#' the learned parameters are stored in the returned Supervised or SupervisedRes object, and the
#' preprocessing is applied on validation and test data.
#'
#' **Binary Classification**
#'
#' For binary classification, the outcome should be a factor where *the 2nd level
#' corresponds to the positive class*.
#'
#' **Resampling**
#'
#' Note that you should not use an outer resampling method with
#' replacement if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the
#' training and test sets of the inner resamples, leading to underestimated
#' test error.
#'
#' **Reproducibility**
#'
#' If using ***outer resampling***, you can set a seed when defining `outer_resampling_config`, e.g.
#' ```r
#' outer_resampling_config = setup_Resampler(n_resamples = 10L, type = "KFold", seed = 2026L)
#' ```
#' If using ***tuning with inner resampling***, you can set a seed when defining `tuner_config`,
#' e.g.
#' ```r
#' tuner_config = setup_GridSearch(
#'   resampler_config = setup_Resampler(n_resamples = 5L, type = "KFold", seed = 2027L)
#' )
#' ```
#'
#' **Parallelization**
#'
#' There are three levels of parallelization that may be used during training:
#'
#' 1. Algorithm training (e.g. a parallelized learner like LightGBM)
#' 2. Tuning (inner resampling, where multiple resamples can be processed in parallel)
#' 3. Outer resampling (where multiple outer resamples can be processed in parallel)
#'
#' The `train()` function will automatically manage parallelization depending
#' on:
#' - The number of workers specified by the user using `n_workers`
#' - Whether the training algorithm supports parallelization itself
#' - Whether hyperparameter tuning is needed
#'
#' @return Object of class `Regression(Supervised)`, `RegressionRes(SupervisedRes)`,
#' `Classification(Supervised)`, or `ClassificationRes(SupervisedRes)`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \donttest{
#' iris_c_lightRF <- train(
#'    iris,
#'    algorithm = "LightRF",
#'    outer_resampling_config = setup_Resampler(),
#' )
#' }
train <- function(
  x,
  dat_validation = NULL,
  dat_test = NULL,
  weights = NULL,
  algorithm = NULL,
  preprocessor_config = NULL, # PreprocessorConfig
  decomposition_config = NULL, # DecompositionConfig
  hyperparameters = NULL, # Hyperparameters
  tuner_config = NULL, # TunerConfig
  outer_resampling_config = NULL, # ResamplerConfig
  execution_config = setup_ExecutionConfig(), # ExecutionConfig
  question = NULL,
  outdir = NULL,
  verbosity = 1L,
  progress = NULL,
  ...
) {
  # SuperConfigLive dispatch ----
  if (S7_inherits(x, SuperConfigLive)) {
    train_args <- list(
      x = x@dat_training,
      dat_validation = x@dat_validation,
      dat_test = x@dat_test,
      weights = x@weights,
      preprocessor_config = x@preprocessor_config,
      decomposition_config = x@decomposition_config,
      algorithm = x@algorithm,
      hyperparameters = x@hyperparameters,
      tuner_config = x@tuner_config,
      outer_resampling_config = x@outer_resampling_config,
      execution_config = x@execution_config,
      question = x@question,
      outdir = x@outdir,
      verbosity = x@verbosity,
      progress = progress
    )
    # `positive_class` is handled via `...` (not a formal arg) and aborts if
    # passed as NULL, so include it only when set.
    if (!is.null(x@positive_class)) {
      train_args[["positive_class"]] <- x@positive_class
    }
    return(do.call(train, train_args))
  } # / train.SuperConfigLive

  # SuperConfig dispatch ----
  if (S7_inherits(x, SuperConfig)) {
    # `SuperConfig` is a recipe: `dat_training_path` may be unbound. Require it
    # at train time (the CLI sets it from its data argument before calling).
    if (is.null(x@dat_training_path)) {
      rtemis.core::abort(
        "This `SuperConfig` has no `dat_training_path`; set it before training ",
        '(e.g. `x@dat_training_path <- "data.parquet"`) or bind in-memory ',
        "data via `SuperConfigLive`.",
        class = c("rtemis_null_input", "rtemis_input_error")
      )
    }
    dat_training <- read(x@dat_training_path, character2factor = TRUE)
    dat_validation <- if (!is.null(x@dat_validation_path)) {
      read(x@dat_validation_path)
    } else {
      NULL
    }
    dat_test <- if (!is.null(x@dat_test_path)) {
      read(x@dat_test_path)
    } else {
      NULL
    }
    # Call train() with data and other parameters from config
    train_args <- list(
      x = dat_training,
      dat_validation = dat_validation,
      dat_test = dat_test,
      weights = x@weights,
      preprocessor_config = x@preprocessor_config,
      decomposition_config = x@decomposition_config,
      algorithm = x@algorithm,
      hyperparameters = x@hyperparameters,
      tuner_config = x@tuner_config,
      outer_resampling_config = x@outer_resampling_config,
      execution_config = x@execution_config,
      question = x@question,
      outdir = x@outdir,
      verbosity = x@verbosity,
      progress = progress
    )
    # `positive_class` is handled via `...` (not a formal arg) and aborts if
    # passed as NULL, so include it only when set.
    if (!is.null(x@positive_class)) {
      train_args[["positive_class"]] <- x@positive_class
    }
    return(do.call(train, train_args))
  } # / train.SuperConfig

  # Checks ----
  if (is.null(hyperparameters) && is.null(algorithm)) {
    rtemis.core::abort(
      "You must define either `hyperparameters` or `algorithm`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Defense against invalid args
  extra_args <- list(...)
  if (!is.null(extra_args[["positive_class"]])) {
    positive_class <- extra_args[["positive_class"]]
    x <- set_positive_class(x, positive_class)
    extra_args[["positive_class"]] <- NULL
  }
  if (length(extra_args) > 0L) {
    rtemis.core::abort(
      "Unused extra arguments were provided: ",
      paste(names(extra_args), collapse = ", "),
      ". Please check your function call.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  if (is.null(algorithm) && !is.null(hyperparameters)) {
    algorithm <- hyperparameters@algorithm
  }

  # Initial check targetting non-numeric or factor columns
  # Will be checked again by individual learners;
  # this stops sending to all resamples and failing, which fits our stop early design.
  check_supervised(
    x = x,
    dat_validation = dat_validation,
    dat_test = dat_test,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  ncols <- ncol(x)

  if (is.null(hyperparameters) && !is.null(algorithm)) {
    hyperparameters <- get_default_hyperparameters(
      algorithm,
      type = type,
      ncols = ncols
    )
  }

  if (
    !is.null(algorithm) &&
      tolower(algorithm) != tolower(hyperparameters@algorithm)
  ) {
    rtemis.core::abort(
      "You defined algorithm to be '",
      algorithm,
      "', but defined hyperparameters for ",
      hyperparameters@algorithm,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  check_is_S7(hyperparameters, Hyperparameters)

  # Set default tuner_config if tuning is needed but none specified
  if (needs_tuning(hyperparameters) && is.null(tuner_config)) {
    tuner_config <- setup_GridSearch()
  }

  if (!is.null(tuner_config)) {
    check_is_S7(tuner_config, TunerConfig)
  }

  if (!is.null(preprocessor_config)) {
    check_is_S7(preprocessor_config, PreprocessorConfig)
  }

  # Can only use algorithms whose output can be applied on new data: we need to apply the
  # transformation learned on the training data to validation and test sets.
  if (!is.null(decomposition_config)) {
    check_is_S7(decomposition_config, DecompositionConfig)
    if (!decomposition_config@algorithm %in% decom_algorithms_applicable) {
      rtemis.core::abort(
        "Decomposition algorithm '",
        decomposition_config@algorithm,
        "' cannot be applied on new data and is not supported in `train()`.\n",
        "Supported decomposition algorithms: ",
        paste(decom_algorithms_applicable, collapse = ", "),
        ".",
        class = "rtemis_unsupported_error"
      )
    }
  }

  # execution_config must always be set
  check_is_S7(execution_config, ExecutionConfig)
  # Override parallelization parameters with those from execution_config
  backend <- execution_config@backend
  n_workers <- execution_config@n_workers
  future_plan <- execution_config@future_plan

  # If outer_resampling_config is set, dat_validation and dat_test must be NULL
  if (!is.null(outer_resampling_config)) {
    if (!is.null(dat_validation) || !is.null(dat_test)) {
      rtemis.core::abort(
        "If `outer_resampling_config` is set, `dat_validation` and `dat_test` must be NULL.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }

  if (backend == "future" && future_plan == "mirai_multisession") {
    future_plan <- "future.mirai::mirai_multisession"
  }
  if (!is.null(outer_resampling_config)) {
    check_is_S7(outer_resampling_config, ResamplerConfig)
    if (!is.null(outer_resampling_config[["id_strat"]])) {
      stopifnot(length(outer_resampling_config[["id_strat"]]) == NROW(x))
    }
  }

  algorithm <- get_alg_name(algorithm)
  if (!is.null(outdir)) {
    outdir <- make_path(outdir)
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (verbosity > 1L) {
      info("Output directory set to ", outdir, ".")
    }
  }

  logfile <- if (!is.null(outdir)) {
    paste0(
      outdir,
      "/",
      "train_",
      algorithm,
      "_",
      format(Sys.time(), "%Y%m%d.%H%M%S"),
      ".log"
    )
  } else {
    NULL
  }

  # Start timer & logfile ----
  start_time <- intro(verbosity = verbosity, logfile = logfile)

  # Observability session ----
  # The top-level call creates the ambient session; nested (per outer-fold) calls reuse
  # it so their step nodes nest under the fold node. See specs/observability.md.
  on_error <- execution_config@on_error
  session_created <- session_start(verbosity = verbosity)
  # Safety net: on any exit (incl. error) the top-level call clears the ambient slot.
  on.exit(if (session_created) session_clear(), add = TRUE)
  root_node <- if (session_created) {
    node_enter("train", label = paste(algorithm, type))
  } else {
    NULL
  }

  # Data ----
  if (type == "Classification") {
    classes <- levels(outcome(x))
  }

  ## Print data summary ----
  if (verbosity > 0L) {
    summarize_supervised(
      x = x,
      dat_validation = dat_validation,
      dat_test = dat_test
    )
  }

  # Init ----
  workers <- get_n_workers(
    algorithm = algorithm,
    hyperparameters = hyperparameters,
    outer_resampling_config = outer_resampling_config,
    n_workers = n_workers,
    verbosity = verbosity
  )
  hyperparameters@n_workers <- workers[["algorithm"]]
  tuner <- NULL

  # Set backend to "none" if workers[["tuning"]] == 1L
  backend <- if (workers[["tuning"]] == 1L) {
    "none"
  } else {
    backend
  }

  # Preprocessors ----
  # `preprocessor`: User-level preprocessing (Preprocessor object created from
  #   `setup_Preprocessor`). Handles scaling, imputation, encoding, etc.
  # `preprocessor_internal`: Algorithm-level preprocessing (Preprocessor object
  #   returned by each train_*() method). Handles transformations the algorithm
  #   requires internally (e.g. factor-to-integer conversion for LightGBM).
  # Both are stored on the trained model so predict() can re-apply them in order:
  # user-level first, then algorithm-level.
  # Initialized to NULL here; set in the single-model path below.
  # In the outer resampling path, each sub-model carries its own pair.
  preprocessor <- preprocessor_internal <- NULL
  # `decomposition`: fitted Decomposition (from `decomposition_config`) learned on
  # the training features and re-applied to validation/test here, and to new data
  # at predict() time. Stored on the returned model. NULL when no decomposition.
  decomposition <- NULL

  # === Outer Resampling ===
  # Splits data into multiple training-test folds and calls train() recursively
  # on each. Each recursive call enters the Single Model path below (which may
  # itself tune via inner resampling). After all folds complete, execution falls
  # through to the Outer Aggregation path.
  if (!is.null(outer_resampling_config)) {
    msg0(
      fmt("<> ", col = col_outer, bold = TRUE),
      "Training ",
      highlight(paste(algorithm, type)),
      " using ",
      desc(outer_resampling_config),
      "...",
      verbosity = verbosity
    )
    outer_resampler <- resample(
      x,
      config = outer_resampling_config,
      verbosity = verbosity
    )
    n_outer <- outer_resampler@config@n
    # When a `progress` callback is supplied (typically by rtemis.server to
    # forward fold boundaries over the wire), use a plain lapply and invoke
    # it per fold; the interactive `cli_progress_along` UI is replaced.
    # Otherwise, keep the existing terminal progress UI for interactive
    # users.
    iter <- if (is.function(progress)) {
      seq_len(n_outer)
    } else {
      cli::cli_progress_along(
        seq_len(n_outer),
        name = "Training outer resamples...",
        type = "tasks"
      )
    }
    models <- lapply(
      iter,
      function(i) {
        if (is.function(progress)) {
          tryCatch(
            progress(
              stage = "outer_fold",
              current = i,
              total = n_outer,
              message = paste0("Outer fold ", i, "/", n_outer)
            ),
            error = function(e) NULL
          )
        }
        fold_node <- node_enter(
          "outer_fold",
          label = paste0(i, "/", n_outer),
          meta = list(fold = i)
        )
        # Failure policy (specs/observability.md section 7): under "continue" an outer
        # fold failure is non-fatal (recorded, warned, excluded); under "stop"/
        # "stop_outer" it is recorded then re-raised.
        out <- tryCatch(
          train(
            x = x[outer_resampler[[i]], ],
            dat_test = x[-outer_resampler[[i]], ],
            algorithm = algorithm,
            preprocessor_config = preprocessor_config,
            decomposition_config = decomposition_config,
            hyperparameters = hyperparameters,
            tuner_config = tuner_config,
            outer_resampling_config = NULL, # This model is one of the outer resamples.
            execution_config = execution_config,
            weights = if (!is.null(weights)) {
              weights[outer_resampler[[i]]]
            } else {
              NULL
            },
            question = question,
            verbosity = verbosity - 1L
          ),
          error = function(e) {
            node_exit(fold_node, status = "error", error = e)
            if (identical(on_error, "continue")) {
              rtemis.core::warn(
                "Outer fold ",
                i,
                "/",
                n_outer,
                " failed: ",
                conditionMessage(e)
              )
              return(NULL)
            }
            stop(e)
          }
        )
        if (!is.null(out)) {
          node_exit(fold_node, status = "ok")
        }
        out
      }
    )
    names(models) <- names(outer_resampler@resamples)
    # Drop failed folds (only possible under on_error = "continue").
    failed_folds <- vapply(models, is.null, logical(1L))
    if (all(failed_folds)) {
      rtemis.core::abort(
        "All ",
        n_outer,
        " outer folds failed.",
        class = c("rtemis_error", "rtemis_runtime_error")
      )
    }
    if (any(failed_folds)) {
      rtemis.core::warn(
        sum(failed_folds),
        " of ",
        n_outer,
        " outer folds failed; aggregating over ",
        sum(!failed_folds),
        " surviving folds."
      )
      models <- models[!failed_folds]
    }
    hyperparameters@resampled <- 1L
    msg(
      fmt("</>", col = col_outer, bold = TRUE),
      "Outer resampling done.",
      verbosity = verbosity
    )
  } # /Outer Resampling

  if (hyperparameters@resampled == 0L) {
    # === Inner path ===
    # Trains one model: optionally tune (inner resampling) → preprocess →
    # train algorithm → predict → returns Supervised.
    # Skipped when outer resampling was performed (resampled == 1L).

    # Tune ----
    # Inner resampling for hyperparameter optimization.
    if (needs_tuning(hyperparameters)) {
      # The "tune" node is the parent under which tune_GridSearch() host-synthesizes one
      # grid_cell node per (combo x inner resample). See specs/observability.md section 4.
      tune_node <- node_enter("tune")
      tuner <- tune(
        x = x,
        hyperparameters = hyperparameters,
        tuner_config = tuner_config,
        preprocessor_config = preprocessor_config,
        decomposition_config = decomposition_config,
        weights = weights,
        backend = backend,
        future_plan = future_plan,
        n_workers = workers[["tuning"]],
        verbosity = verbosity,
        on_error = on_error
      )
      # Update hyperparameters
      hyperparameters <- update(
        hyperparameters,
        tuner@best_hyperparameters,
        tuned = 1L
      )
      node_exit(tune_node, status = "ok")
    } # /Tune

    # User-level preprocessing ----
    if (!is.null(preprocessor_config)) {
      prep_node <- node_enter("preprocess")
      if (verbosity == 1L) {
        msg("Preprocessing...")
      }
      preprocessor <- preprocess(
        x = x,
        config = preprocessor_config,
        dat_validation = dat_validation,
        dat_test = dat_test,
        verbosity = verbosity - 1L
      )
      x <- if (is.null(dat_validation) && is.null(dat_test)) {
        preprocessor@preprocessed
      } else {
        preprocessor@preprocessed[["training"]]
      }
      if (!is.null(dat_validation)) {
        dat_validation <- preprocessor@preprocessed[["validation"]]
      }
      if (!is.null(dat_test)) {
        dat_test <- preprocessor@preprocessed[["test"]]
      }
      node_exit(prep_node, status = "ok")
    } else {
      preprocessor <- NULL
    } # /User-level preprocessing

    # Decomposition ----
    # Learn the decomposition on the selected training features, then apply the
    # learned transformation to the validation and test features. Features not
    # selected for decomposition are kept as-is, in front of the components, and
    # the outcome column is re-attached last: layout `[kept features, components,
    # outcome]`. The fitted Decomposition (carrying the resolved feature names)
    # is stored on the returned model so predict() can re-apply it to new data.
    if (!is.null(decomposition_config)) {
      decomp_node <- node_enter(
        "decompose",
        label = decomposition_config@algorithm
      )
      outcome_nm <- names(x)[ncols]
      feat <- as.data.frame(features(x))
      # Resolve the columns to decompose: NULL -> all numeric features.
      decomp_features <- decomposition_config@features
      if (is.null(decomp_features)) {
        decomp_features <- names(numeric_features(x))
      }
      # Validate the selection against the actual (post-preprocessing) data.
      missing_cols <- setdiff(decomp_features, names(feat))
      if (length(missing_cols) > 0L) {
        rtemis.core::abort(
          "`decomposition_config` selects columns that are not features.\n",
          "Not found among features: ",
          paste(missing_cols, collapse = ", "),
          ".",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      non_numeric <- decomp_features[
        !vapply(feat[decomp_features], is.numeric, logical(1L))
      ]
      if (length(non_numeric) > 0L) {
        rtemis.core::abort(
          "Decomposition can only be applied to numeric features.\n",
          "Non-numeric columns selected: ",
          paste(non_numeric, collapse = ", "),
          ".",
          class = c("rtemis_type_error", "rtemis_input_error")
        )
      }
      if (length(decomp_features) < 2L) {
        rtemis.core::abort(
          "Decomposition requires at least 2 numeric feature columns.\n",
          length(decomp_features),
          " available to decompose.",
          class = c("rtemis_length_error", "rtemis_input_error")
        )
      }
      # Persist the resolved names so apply_decomp() replays the same selection
      # on validation/test here and on new data at predict() time.
      decomposition_config@features <- decomp_features
      decomposition <- decomp(
        x = feat[, decomp_features, drop = FALSE],
        algorithm = decomposition_config@algorithm,
        config = decomposition_config,
        verbosity = verbosity
      )
      # Columns not decomposed are kept as-is, in front of the components.
      kept_features <- setdiff(names(feat), decomp_features)
      # Decomposed training data: [kept features, components, outcome].
      x_outcome <- x[[ncols]]
      components <- as.data.frame(decomposition@transformed)
      x <- if (length(kept_features) > 0L) {
        cbind(feat[, kept_features, drop = FALSE], components)
      } else {
        components
      }
      x[[outcome_nm]] <- x_outcome
      # Apply decomposition to validation data
      if (!is.null(dat_validation)) {
        val_outcome <- dat_validation[[ncols]]
        dat_validation <- as.data.frame(
          apply_decomp(decomposition, features(dat_validation), verbosity = 0L)
        )
        dat_validation[[outcome_nm]] <- val_outcome
      }
      # Apply decomposition to test data
      if (!is.null(dat_test)) {
        test_outcome <- dat_test[[ncols]]
        dat_test <- as.data.frame(
          apply_decomp(decomposition, features(dat_test), verbosity = 0L)
        )
        dat_test[[outcome_nm]] <- test_outcome
      }
      # Number of columns changes after decomposition.
      ncols <- ncol(x)
      node_exit(decomp_node, status = "ok")
    }

    # IFW ----
    # Weight calculation must follow preprocessing since N cases may change.
    if (type == "Classification" && hyperparameters[["ifw"]]) {
      if (!is.null(weights)) {
        rtemis.core::abort(
          "Custom weights are defined, but IFW is set to TRUE.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      } else {
        weights <- ifw(x[[ncols]], type = "case_weights", verbosity = verbosity)
      }
    } # /IFW

    # Train algorithm ----
    if (is_tuned(hyperparameters)) {
      msg(
        "Training",
        highlight(paste(algorithm, type)),
        "with tuned hyperparameters...",
        verbosity = verbosity
      )
    } else {
      msg0(
        "Training ",
        highlight(paste(algorithm, type)),
        "...",
        verbosity = verbosity
      )
    }
    # Validation data is only passed to learners that use early stopping.
    # For other learners, validation metrics are collected during tuning.
    dat_validation_for_training <- if (algorithm %in% early_stopping_algs) {
      dat_validation
    } else {
      NULL
    }

    algo_node <- node_enter(
      "train_alg",
      label = algorithm,
      meta = list(algorithm = algorithm, n = NROW(x))
    )
    trained <- train_(
      hyperparameters = hyperparameters,
      x = x,
      weights = weights,
      dat_validation = dat_validation_for_training,
      execution_config = execution_config, # used by LightRuleFit
      verbosity = verbosity
    )
    node_exit(algo_node, status = "ok")

    model <- trained[["model"]]
    # Algorithm-level preprocessing (e.g. factor-to-integer for LightGBM),
    # returned by train_*() if needed.
    preprocessor_internal <- trained[["preprocessor"]]

    # Predictions ----
    predict_node <- node_enter("predict")
    predicted_prob_training <- predicted_prob_validation <- predicted_prob_test <- NULL

    # Re-apply algorithm-level preprocessing before predicting on each dataset.
    x_features <- features(x)
    if (!is.null(preprocessor_internal)) {
      x_features <- preprocess(
        x_features,
        preprocessor_internal,
        verbosity = verbosity - 1L
      ) |>
        preprocessed()
    }

    predicted_training <- predict_super(
      model = model,
      newdata = x_features,
      type = type
    )

    if (type == "Classification") {
      predicted_prob_training <- predicted_training
      predicted_training <- prob2categorical(
        predicted_prob_training,
        levels = classes
      )
    }

    predicted_validation <- predicted_test <- NULL
    if (!is.null(dat_validation)) {
      dat_validation_features <- features(dat_validation)
      if (!is.null(preprocessor_internal)) {
        dat_validation_features <- preprocess(
          dat_validation_features,
          preprocessor_internal,
          verbosity = verbosity - 1L
        ) |>
          preprocessed()
      }

      predicted_validation <- predict_super(
        model = model,
        newdata = dat_validation_features,
        type = type
      )

      if (type == "Classification") {
        predicted_prob_validation <- predicted_validation
        predicted_validation <- prob2categorical(
          predicted_prob_validation,
          levels = classes
        )
      }
    }

    if (!is.null(dat_test)) {
      dat_test_features <- features(dat_test)
      if (!is.null(preprocessor_internal)) {
        dat_test_features <- preprocess(
          dat_test_features,
          preprocessor_internal,
          verbosity = verbosity - 1L
        ) |>
          preprocessed()
      }

      predicted_test <- predict_super(
        model = model,
        newdata = dat_test_features,
        type = type
      )

      if (type == "Classification") {
        predicted_prob_test <- predicted_test
        predicted_test <- prob2categorical(
          predicted_prob_test,
          levels = classes
        )
      }
    }

    # Standard Errors ----
    # Use the same (algorithm-level preprocessed) features as predictions.
    se_training <- se_validation <- se_test <- NULL
    if (type == "Regression" && algorithm %in% se_compat_algorithms) {
      se_training <- se_super(model = model, newdata = x_features)
      if (!is.null(dat_validation)) {
        se_validation <- se_super(
          model = model,
          newdata = dat_validation_features
        )
      }
      if (!is.null(dat_test)) {
        se_test <- se_super(model = model, newdata = dat_test_features)
      }
    }
    node_exit(predict_node, status = "ok")

    # Variable importance ----
    # Skipped during tuning (inner resampling): those models are discarded and only
    # the outer folds' / final model's varimp is used. For kept models it can be a
    # major cost (e.g. LightGBM tree -> data.table extraction), so record a node.
    varimp <- NULL
    if (hyperparameters@tuned != TUNED_STATUS_TUNING) {
      varimp_node <- node_enter("varimp", label = algorithm)
      varimp <- varimp_super(model = model)
      node_exit(varimp_node, status = "ok")
    }

    # Inner path: Return Supervised ----
    # The Classification/Regression constructor computes train/validation/test
    # metrics (e.g. AUC, which can dominate on large data), so wrap it in a node.
    metrics_node <- node_enter("metrics")
    mod <- make_Supervised(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      preprocessor_internal = preprocessor_internal,
      decomposition = decomposition,
      hyperparameters = hyperparameters,
      tuner = tuner,
      execution_config = execution_config,
      y_training = x[[ncols]],
      y_validation = if (!is.null(dat_validation)) dat_validation[[ncols]],
      y_test = if (!is.null(dat_test)) dat_test[[ncols]],
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_test = predicted_prob_test,
      se_training = se_training,
      se_validation = se_validation,
      se_test = se_test,
      xnames = names(x)[-ncols],
      varimp = varimp,
      question = question
    )
    node_exit(metrics_node, status = "ok")
  } else {
    # === Outer Aggregation path ===
    # Reached after outer resampling. Each sub-model (Supervised) in `models`
    # carries its own preprocessor pair. Aggregate results → SupervisedRes.
    y_training <- lapply(models, function(mod) mod@y_training)
    y_test <- lapply(models, function(mod) mod@y_test)
    predicted_training <- lapply(models, function(mod) mod@predicted_training)
    predicted_test <- lapply(models, function(mod) mod@predicted_test)
    if (type == "Classification") {
      predicted_prob_training <- lapply(
        models,
        function(mod) mod@predicted_prob_training
      )
      predicted_prob_test <- lapply(
        models,
        function(mod) mod@predicted_prob_test
      )
    } else {
      predicted_prob_training <- predicted_prob_test <- NULL
    }
    # Return SupervisedRes ----
    mod <- make_SupervisedRes(
      algorithm = algorithm,
      type = type,
      models = models,
      preprocessor_config = preprocessor_config,
      decomposition_config = decomposition_config,
      hyperparameters = hyperparameters,
      tuner_config = tuner_config,
      outer_resampler = outer_resampler,
      execution_config = execution_config,
      y_training = y_training,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_test = predicted_test,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_test = predicted_prob_test,
      xnames = names(x)[-ncols],
      varimp = lapply(models, \(mod) mod@varimp),
      question = question
    )
  }

  # Finalize observability session ----
  # Only the top-level call finalizes and attaches the session; nested (per outer-fold)
  # calls leave their sub-model's `session` NULL, as the root session already contains
  # their nodes. See specs/observability.md sections 3 and 10.
  if (session_created) {
    node_exit(root_node, status = "ok")
    mod@session <- session_finalize()
  }

  # Outro ----
  if (verbosity > 0L) {
    message()
    print(mod)
    message()
  }
  if (session_created && verbosity >= 2L) {
    session_report(mod@session, verbosity = verbosity)
  }
  if (!is.null(outdir)) {
    rt_save(mod, outdir = outdir, file_prefix = paste0("train_", algorithm))
  }
  outro(
    start_time,
    logfile = logfile,
    verbosity = verbosity
  )
  # Print object to logfile
  if (!is.null(logfile)) {
    cat(
      "\n",
      repr(mod, output_type = "plain"),
      file = logfile,
      append = TRUE,
      sep = ""
    )
  }
  mod
} # /rtemis::train


# %% get_n_workers ----
# Function to assign number of workers to algorithm, tuning, or outer resampling
# based on whether algorithm is parallelized, tuning is needed, and outer resampling is set.

#' Get Number of Workers
#'
#' Distribute workers across different parallelization levels: algorithm training,
#' tuning (inner resampling), and outer resampling. Assigns workers to the innermost
#' available parallelization level to avoid over-subscription.
#'
#' @param algorithm Character: Algorithm name.
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param outer_resampling_config Optional ResamplerConfig object: Setup using [setup_Resampler].
#' @param n_workers Integer: Total number of workers you want to use.
#' @param verbosity Integer: Verbosity level.
#'
#' @details
#' The function prioritizes parallelization levels as follows:
#' 1. If algorithm is parallelized (e.g., LightGBM, Ranger): all workers go to algorithm
#' 2. Else if tuning is needed: all workers go to tuning (inner resampling)
#' 3. Else if outer resampling is set: all workers go to outer resampling
#' 4. Else: sequential execution (1 worker each)
#'
#' @return Named list with the number of workers for each level:
#' - `algorithm`: Number of workers for algorithm training.
#' - `tuning`: Number of workers for tuning (if applicable).
#' - `outer_resampling_config`: Number of workers for outer resampling (if applicable).
#'
#' @keywords internal
#' @noRd
get_n_workers <- function(
  algorithm,
  hyperparameters,
  outer_resampling_config,
  n_workers,
  verbosity = 1L
) {
  # Input validation
  stopifnot(
    is.character(algorithm),
    length(algorithm) == 1L,
    is.numeric(n_workers),
    n_workers >= 1L,
    n_workers == as.integer(n_workers)
  )

  # Check parallelization conditions
  is_parallelized <- algorithm %in% live[["parallelized_learners"]]
  requires_tuning <- needs_tuning(hyperparameters)
  requires_resampling <- !is.null(outer_resampling_config)

  # Assign workers to innermost parallelization level to avoid over-subscription
  if (is_parallelized) {
    # Parallelized algorithms get all workers, disable other parallelization
    workers_algorithm <- n_workers
    workers_tuning <- 1L
    workers_outer_resampling <- 1L
    if (verbosity > 1L && (requires_tuning || requires_resampling)) {
      msg(
        bold(algorithm),
        "is parallelized. Disabling tuning parallelization."
      )
    }
  } else if (requires_tuning) {
    # Tuning gets all workers if algorithm is not parallelized
    workers_algorithm <- 1L
    workers_tuning <- n_workers
    workers_outer_resampling <- 1L
    if (requires_resampling) {
      msg(
        "Tuning parallelization enabled.",
        verbosity = verbosity
      )
    }
  } else if (requires_resampling) {
    # Outer resampling gets all workers if no tuning needed
    workers_algorithm <- 1L
    workers_tuning <- 1L
    workers_outer_resampling <- n_workers
  } else {
    # Sequential execution
    workers_algorithm <- 1L
    workers_tuning <- 1L
    workers_outer_resampling <- 1L
  }

  msg0(
    bold("//"),
    " Max workers: ",
    highlight(n_workers),
    " { ",
    gray("Algorithm: "),
    highlight(workers_algorithm),
    gray("; Tuning: "),
    highlight(workers_tuning),
    gray("; Outer Resampling: "),
    highlight(workers_outer_resampling),
    " }",
    verbosity = verbosity
  )

  list(
    algorithm = workers_algorithm,
    tuning = workers_tuning,
    outer_resampling = workers_outer_resampling
  )
} # /rtemis::get_n_workers
