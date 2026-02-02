# tune.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Get Tuner Function
#'
#' @param type Character: Type of tuner.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
get_tuner_fn <- function(type = "GridSearch") {
  type <- match_arg(type, c("GridSearch"))
  switch(type, "GridSearch" = "tune_GridSearch")
} # /rtemis::get_tuner_fn


#' Tune Supervised Learning Model
#'
#' @param x data.frame or similar: Training set data.
#' @param hyperparameters `Hyperparameters` object: make using each learner's `setup_*` function.
#' @param tuner_config `TunerConfig` object: created with [setup_GridSearch].
#' @param weights Numeric vector: Optional case weights.
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tune <- function(
  x,
  hyperparameters,
  tuner_config,
  weights = NULL,
  verbosity = 1L,
  parallel_type = "none",
  future_plan = "multicore",
  n_workers = 1L
) {
  check_is_S7(hyperparameters, Hyperparameters)
  check_is_S7(tuner_config, TunerConfig)
  stopifnot(needs_tuning(hyperparameters))

  if (tuner_config@type == "GridSearch") {
    tune_GridSearch(
      x = x,
      hyperparameters = hyperparameters,
      tuner_config = tuner_config,
      weights = weights,
      verbosity = verbosity,
      parallel_type = parallel_type,
      future_plan = future_plan,
      n_workers = n_workers
    )
  } else {
    cli::cli_abort("Unsupported tuner type: {tuner_config@type}")
  }
} # /rtemis::tune
