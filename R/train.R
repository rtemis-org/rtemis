# train.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Train Supervised Learning Models
#'
#' Preprocess, tune, train, and test supervised learning models with a single function
#' using nested resampling
#'
#' @details
#' See [rdocs.rtemis.org/train](https://rdocs.rtemis.org/train) for detailed documentation.
#'
#' Important: For binary classification, the outcome should be a factor where the 2nd level
#' corresponds to the positive class.
#'
#' Note on resampling: You should never use an outer resampling method with
#' replacement if you will also be using an inner resampling (for tuning).
#' The duplicated cases from the outer resampling may appear both in the
#' training and test sets of the inner resamples, leading to underestimated
#' test error.
#'
#' Important note on parallelization:
#' There are three levels of parallelization that may be used during training:
#'
#' 1. Algorithm training (e.g. a parallelized learner like LightGBM)
#' 2. Tuning (inner resampling, where multiple resamples can be processed in parallel)
#' 3. Outer resampling (where multiple outer resamples can be processed in parallel)
#'
#' The `train()` function and its sub-functions will automatically manage parallelization depending
#' on:
#' - The number of workers specified by the user using `n_workers`
#' - Whether the training algorithm supports parallelization itself
#' - Whether hyperparameter tuning is needed
#'
#' @param x data.frame or similar: Training set data.
#' @param dat_validation data.frame or similar: Validation set data.
#' @param dat_test data.frame or similar: Test set data.
#' @param algorithm Character: Algorithm to use. Can be left NULL, if `hyperparameters` is defined.
#' @param preprocessor_config PreprocessorConfig object or NULL: Setup using [setup_Preprocessor].
#' @param hyperparameters `Hyperparameters` object: Setup using one of `setup_*` functions.
#' @param tuner_config TunerConfig object: Setup using [setup_GridSearch].
#' @param outer_resampling_config ResamplerConfig object or NULL: Setup using [setup_Resampler]. This
#' defines the outer resampling method, i.e. the splitting into training and test sets for the
#' purpose of assessing model performance. If NULL, no outer resampling is performed, in which case
#' you might want to use a `dat_test` dataset to assess model performance on a single test set.
#' @param weights Optional vector of case weights.
#' @param question Optional character string defining the question that the model is trying to
#' answer.
#' @param outdir Character, optional: String defining the output directory.
#' @param parallel_type Character: "none", "future", or "mirai".
#' @param future_plan Character: Future plan to use for parallel processing.
#' @param n_workers Integer: Number of workers to use for parallel processing in total.
#' Parallelization may happen at three different levels, from innermost to outermost:
#' 1. Algorithm training (e.g. a parallelized learner like LightGBM)
#' 2. Tuning (inner resampling, where multiple resamples can be processed in parallel)
#' 3. Outer resampling (where multiple outer resamples can be processed in parallel)
#' The `train()` function will assign the number of workers to the innermost available
#' parallelization level. Best to leave a few cores for the OS and other processes, especially
#' on shared systems or when working with large datasets, since parallelization will increase
#' memory usage.
#' @param verbosity Integer: Verbosity level.
# @param ... Additional arguments to pass to the hyperparameters setup function. Only used if
#' `hyperparameters` is not defined. Avoid relying on this, instead use the appropriate `setup_*`
#' function with the `hyperparameters` argument.
#'
#' @return Object of class `Regression(Supervised)`, `RegressionRes(SupervisedRes)`,
#' `Classification(Supervised)`, or `ClassificationRes(SupervisedRes)`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
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
  algorithm = NULL,
  preprocessor_config = NULL, # PreprocessorConfig
  hyperparameters = NULL, # Hyperparameters
  tuner_config = NULL, # TunerConfig
  outer_resampling_config = NULL, # ResamplerConfig
  weights = NULL,
  question = NULL,
  outdir = NULL,
  parallel_type = c("future", "mirai", "none"),
  future_plan = getOption("future.plan", "multicore"),
  n_workers = parallelly::availableCores(omit = 3L),
  verbosity = 1L
) {
  # Checks ----
  if (is.null(hyperparameters) && is.null(algorithm)) {
    cli::cli_abort(
      "You must define either `hyperparameters` or `algorithm`."
    )
  }

  if (is.null(algorithm) && !is.null(hyperparameters)) {
    algorithm <- hyperparameters@algorithm
  }

  type <- supervised_type(x)
  ncols <- ncol(x)

  if (is.null(hyperparameters) && !is.null(algorithm)) {
    # without extra args
    hyperparameters <- get_default_hyperparameters(
      algorithm,
      type = type,
      ncols = ncols
    )
    # with extra args
    # setup_fn <- get_alg_setup(algorithm)
    # hyperparameters <- do_call(setup_fn, hpr_args)
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

  # If outer_resampling_config is set, dat_validation and dat_test must be NULL
  if (!is.null(outer_resampling_config)) {
    if (!is.null(dat_validation) || !is.null(dat_test)) {
      cli::cli_abort(
        "If outer_resampling_config is set, dat_validation and dat_test must be NULL."
      )
    }
  }

  ## Arguments ----
  parallel_type <- match.arg(parallel_type)
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
      message("Output directory set to ", outdir, ".")
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
    msg(fmt("</>", col = col_outer, bold = TRUE), "Outer resampling done.")
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
        parallel_type = parallel_type,
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
    args <- list(
      x = x,
      weights = weights,
      hyperparameters = hyperparameters,
      verbosity = verbosity
    )
    # Validation data is only passed to learners using early stopping.
    # Otherwise, tuning functions collect validation metrics.
    if (algorithm %in% early_stopping_algs) {
      args[["dat_validation"]] <- dat_validation
    }

    model <- do_call(get_train_fn(algorithm), args)
    # each train_* function checks output is the correct model class.

    # Predicted Values ----
    predict_fn <- get_predict_fn(algorithm)
    varimp_fn <- get_varimp_fn(algorithm)
    predicted_prob_training <- predicted_prob_validation <- predicted_prob_test <- NULL
    predicted_training <- do_call(
      predict_fn,
      list(model, newdata = features(x), type = type)
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
      predicted_validation <- do_call(
        predict_fn,
        list(model, newdata = features(dat_validation), type = type)
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
      predicted_test <- do_call(
        predict_fn,
        list(model, newdata = features(dat_test), type = type)
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
      se_fn <- get_se_fn(algorithm)
      se_training <- do_call(se_fn, list(model, newdata = features(x)))
      if (!is.null(dat_validation)) {
        se_validation <- do_call(
          se_fn,
          list(model, newdata = features(dat_validation))
        )
      }
      if (!is.null(dat_test)) {
        se_test <- do_call(se_fn, list(model, newdata = features(dat_test)))
      }
    }

    # Make Supervised/Res ----
    mod <- make_Supervised(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
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
      varimp = do_call(varimp_fn, list(model)),
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
    # sink_off = ifelse(is.null(logfile), FALSE, TRUE)
  )
  # Print object to logfile
  if (!is.null(logfile)) {
    cat("\n", repr(mod, output_type = "plain"), file = logfile, append = TRUE)
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
