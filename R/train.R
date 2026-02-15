# train.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train.class_tabular ----
#' Train Supervised Learning Models
#'
#' @description
#' Preprocess, tune, train, and test supervised learning models in a single call
#' using nested resampling.
#'
#' @param x tabular data, i.e. data.frame, data.table, or tbl_df (tibble): Training set data.
#' @param dat_validation tabular data: Validation set data.
#' @param dat_test tabular data: Test set data.
#' @param weights Optional vector of case weights.
#' @param algorithm Character: Algorithm to use. Can be left NULL, if `hyperparameters` is defined.
#' @param preprocessor_config PreprocessorConfig object or NULL: Setup using [setup_Preprocessor].
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param tuner_config TunerConfig object: Setup using [setup_GridSearch].
#' @param outer_resampling_config ResamplerConfig object or NULL: Setup using [setup_Resampler].
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
#' @param ... Not used.
#'
#' @details
#' **Online book & documentation**
#'
#' See [rdocs.rtemis.org/train](https://rdocs.rtemis.org/train) for detailed documentation.
#'
#' **Binary Classification**
#'
#' For binary classification, the outcome should be a factor where the 2nd level
#' corresponds to the positive class.
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
  hyperparameters = NULL, # Hyperparameters
  tuner_config = NULL, # TunerConfig
  outer_resampling_config = NULL, # ResamplerConfig
  execution_config = setup_ExecutionConfig(), # ExecutionConfig
  question = NULL,
  outdir = NULL,
  verbosity = 1L,
  ...
) {
  # SuperConfig dispatch ----
  if (S7_inherits(x, SuperConfig)) {
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
    return(train(
      x = dat_training,
      dat_validation = dat_validation,
      dat_test = dat_test,
      weights = x@weights,
      preprocessor_config = x@preprocessor_config,
      algorithm = x@algorithm,
      hyperparameters = x@hyperparameters,
      tuner_config = x@tuner_config,
      outer_resampling_config = x@outer_resampling_config,
      execution_config = x@execution_config,
      question = x@question,
      outdir = x@outdir,
      verbosity = x@verbosity
    ))
  } # / train.SuperConfig

  # Checks ----
  if (is.null(hyperparameters) && is.null(algorithm)) {
    cli::cli_abort(
      "You must define either `hyperparameters` or `algorithm`."
    )
  }

  extra_args <- list(...)
  if (length(extra_args) > 0L) {
    cli::cli_abort(
      "Unused extra arguments were provided: {.val {names(extra_args)}}. Please check your function call."
    )
  }

  if (is.null(algorithm) && !is.null(hyperparameters)) {
    algorithm <- hyperparameters@algorithm
  }

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
    cli::cli_abort(
      "You defined algorithm to be '",
      algorithm,
      "', but defined hyperparameters for ",
      hyperparameters@algorithm,
      "."
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

  # execution_config must always be set
  check_is_S7(execution_config, ExecutionConfig)
  # Override parallelization parameters with those from execution_config
  backend <- execution_config@backend
  n_workers <- execution_config@n_workers
  future_plan <- execution_config@future_plan

  # If outer_resampling_config is set, dat_validation and dat_test must be NULL
  if (!is.null(outer_resampling_config)) {
    if (!is.null(dat_validation) || !is.null(dat_test)) {
      cli::cli_abort(
        "If outer_resampling_config is set, dat_validation and dat_test must be NULL."
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
      msg_info("Output directory set to ", outdir, ".")
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

  # Outer Resampling ----
  # if outer_resampling_config is set, this function calls itself
  # on multiple outer resamples (training-test sets), each of which may call itself
  # on multiple inner resamples (training-validation sets) for hyperparameter tuning.
  if (!is.null(outer_resampling_config)) {
    if (verbosity > 0L) {
      msg0(
        fmt("<> ", col = col_outer, bold = TRUE),
        "Training ",
        highlight(paste(algorithm, type)),
        " using ",
        desc(outer_resampling_config),
        "..."
      )
    }
    outer_resampler <- resample(
      x,
      config = outer_resampling_config,
      verbosity = verbosity
    )
    models <- lapply(
      cli::cli_progress_along(
        seq_len(outer_resampler@config@n),
        name = "Training outer resamples...",
        type = "tasks"
      ),
      function(i) {
        train(
          x = x[outer_resampler[[i]], ],
          dat_test = x[-outer_resampler[[i]], ],
          algorithm = algorithm,
          preprocessor_config = preprocessor_config,
          hyperparameters = hyperparameters,
          tuner_config = tuner_config,
          outer_resampling_config = NULL,
          weights = weights,
          question = question,
          verbosity = verbosity - 1L
        )
      }
    )
    names(models) <- names(outer_resampler@resamples)
    hyperparameters@resampled <- 1L
    if (verbosity > 0L) {
      msg(fmt("</>", col = col_outer, bold = TRUE), "Outer resampling done.")
    }
  } # /Outer Resampling

  if (hyperparameters@resampled == 0L) {
    # Path 1: Normal training path for a single model.
    # This needs to be skipped if multiple single models have already been trained
    # in the outer resampling loop above, which calls train() recursively.
    # Tune ----
    if (needs_tuning(hyperparameters)) {
      tuner <- tune(
        x = x,
        hyperparameters = hyperparameters,
        tuner_config = tuner_config,
        weights = weights,
        backend = backend,
        future_plan = future_plan,
        n_workers = workers[["tuning"]],
        verbosity = verbosity
      )
      # Update hyperparameters
      hyperparameters <- update(
        hyperparameters,
        tuner@best_hyperparameters,
        tuned = 1L
      )
    } # /Tune

    # Preprocess ----
    if (!is.null(preprocessor_config)) {
      preprocessor <- preprocess(
        x = x,
        config = preprocessor_config,
        dat_validation = dat_validation,
        dat_test = dat_test
      )
      x <- if (is.null(dat_validation) && is.null(dat_test)) {
        preprocessor@preprocessed
      } else {
        preprocessor@preprocessed[["training"]]
      }
      if (!is.null(dat_validation)) {
        dat_validation <- preprocessor@preprocessed[["validation"]]
      }
      if (!is.null(dat_test)) dat_test <- preprocessor@preprocessed[["test"]]
    } else {
      preprocessor <- NULL
    } # /Preprocess

    # IFW ----
    # Weight calculation must follow preprocessing since N cases may change
    if (type == "Classification" && hyperparameters[["ifw"]]) {
      if (!is.null(weights)) {
        cli::cli_abort("Custom weights are defined, but IFW is set to TRUE.")
      } else {
        weights <- ifw(x[[ncols]], type = "case_weights", verbosity = verbosity)
      }
    } # /IFW

    # Train ALG ----
    if (verbosity > 0L) {
      if (is_tuned(hyperparameters)) {
        msg(
          "Training",
          highlight(paste(algorithm, type)),
          "with tuned hyperparameters..."
        )
      } else {
        msg0("Training ", highlight(paste(algorithm, type)), "...")
      }
    } # /Print training message
    # Only algorithms with early stopping can use dat_validation.
    # Note: All training, validation, and test metrics are calculated by Supervised or SupervisedRes.
    # => Introduce supports_weights() if any algorithms do NOT support case weights
    # or only support class weights

    # Validation data is only passed to learners using early stopping.
    # Otherwise, tuning functions collect validation metrics.
    dat_validation_for_training <- if (algorithm %in% early_stopping_algs) {
      dat_validation
    } else {
      NULL
    }

    model <- train_super(
      hyperparameters = hyperparameters,
      x = x,
      weights = weights,
      dat_validation = dat_validation_for_training,
      verbosity = verbosity
    )
    # each train_* method checks output is the correct model class.

    # Predicted Values ----
    predicted_prob_training <- predicted_prob_validation <- predicted_prob_test <- NULL
    predicted_training <- predict_super(
      model = model,
      newdata = features(x),
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
      predicted_validation <- predict_super(
        model = model,
        newdata = features(dat_validation),
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
      predicted_test <- predict_super(
        model = model,
        newdata = features(dat_test),
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
    se_training <- se_validation <- se_test <- NULL
    if (type == "Regression" && algorithm %in% se_compat_algorithms) {
      se_training <- se_super(model = model, newdata = features(x))
      if (!is.null(dat_validation)) {
        se_validation <- se_super(
          model = model,
          newdata = features(dat_validation)
        )
      }
      if (!is.null(dat_test)) {
        se_test <- se_super(model = model, newdata = features(dat_test))
      }
    }

    # Make Supervised/Res ----
    mod <- make_Supervised(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
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
      varimp = varimp_super(model = model),
      question = question
    )
  } else {
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
    mod <- make_SupervisedRes(
      algorithm = algorithm,
      type = type,
      models = models,
      preprocessor = preprocessor,
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

  # Outro ----
  if (verbosity > 0L) {
    message()
    print(mod)
    message()
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
#' @param outer_resampling_config ResamplerConfig object or NULL: Setup using [setup_Resampler].
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
        "is parallelized. Disabling tuning and outer resampling parallelization."
      )
    }
  } else if (requires_tuning) {
    # Tuning gets all workers if algorithm is not parallelized
    workers_algorithm <- 1L
    workers_tuning <- n_workers
    workers_outer_resampling <- 1L
    if (verbosity > 0L && requires_resampling) {
      msg(
        "Tuning parallelization enabled. Disabling outer resampling parallelization."
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

  if (verbosity > 0L) {
    msg0(
      bold("//"),
      " Max workers: ",
      highlight(n_workers),
      " => ",
      "Algorithm: ",
      highlight(workers_algorithm),
      "; Tuning: ",
      highlight(workers_tuning),
      "; Outer Resampling: ",
      highlight(workers_outer_resampling)
    )
  }

  list(
    algorithm = workers_algorithm,
    tuning = workers_tuning,
    outer_resampling = workers_outer_resampling
  )
} # /rtemis::get_n_workers
