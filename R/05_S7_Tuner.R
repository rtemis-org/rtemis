# S7_Tuner.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# S7
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7
# future
# https://www.futureverse.org/backends.html

# Description
# `TunerParams` class and subclasses create objects that store tuner parameters.
# They are set by `setup_GridSearch()` and perform type checking and validation.
# They are used by `tune()`.
# `Tuner` class and subclasses create objects that store tuning results.
# They are created by `tune()`.

# Dev
# Should both class constructors (e.g. GridSearch@constructor) and setup functions
# (e.g. setup_GridSearch) perform type checking and validation?

# TunerParameters ----
#' TunerParameters
#'
#' Superclass for tuner parameters.
#'
#' @field type Character: Type of tuner.
#' @field parameters Named list of tuner parameters.
#'
#' @author EDG
#' @noRd
TunerParameters <- new_class(
  name = "TunerParameters",
  properties = list(
    type = class_character,
    parameters = class_list
  )
) # /TunerParameters

# Print TunerParameters ----
#' Print TunerParameters
#'
#' Print TunerParameters object
#'
#' @param x TunerParameters object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, TunerParameters) <- function(x, pad = 0L, ...) {
  objcat(paste(x@type, "TunerParameters"))
  printls(x@parameters, pad = pad + 2L)
  invisible(x)
}

# Describe Tuner ----
method(desc, TunerParameters) <- function(x) {
  if (x@type == "GridSearch") {
    paste(x@parameters[["search_type"]], "grid search")
  }
} # /rtemis::describe.Tuner

# Make TunerParameters@parameters `$`-accessible
method(`$`, TunerParameters) <- function(x, name) {
  x@parameters[[name]]
}

# `$`-autocomplete TunerParameters@parameters ----
method(`.DollarNames`, TunerParameters) <- function(x, pattern = "") {
  all_names <- names(x@parameters)
  grep(pattern, all_names, value = TRUE)
}

# Make TunerParameters@parameters `[[`-accessible
method(`[[`, TunerParameters) <- function(x, name) {
  x@parameters[[name]]
}

# GridSearchParams ----
#' @title GridSearchParams
#'
#' @description
#' TunerParameters subclass for grid search parameters.
#'
#' @author EDG
#' @noRd
GridSearchParams <- new_class(
  name = "GridSearchParams",
  parent = TunerParameters,
  constructor = function(
    resampler_parameters = NULL,
    search_type = NULL,
    randomize_p = NULL,
    metrics_aggregate_fn = NULL,
    metric = NULL,
    maximize = NULL,
    parallel_type = NULL,
    n_workers = NULL
  ) {
    check_is_S7(resampler_parameters, ResamplerParameters)
    check_inherits(search_type, "character")
    check_float01exc(randomize_p)
    check_character(metrics_aggregate_fn)
    check_inherits(metric, "character")
    check_inherits(maximize, "logical")
    check_inherits(parallel_type, "character")
    n_workers <- clean_posint(n_workers)
    # Only assign randomize_p if search_type is "randomized"
    params <- list(
      search_type = search_type,
      resampler_parameters = resampler_parameters,
      metrics_aggregate_fn = metrics_aggregate_fn,
      metric = metric,
      maximize = maximize,
      parallel_type = parallel_type,
      n_workers = n_workers
    )
    if (search_type == "randomized") {
      params[["randomize_p"]] <- randomize_p
    }
    new_object(
      TunerParameters(
        type = "GridSearch",
        parameters = params
      )
    )
  }
) # /GridSearchParams

# setup_GridSearch() ----
#' Setup Grid Search Parameters
#'
#' Create a `GridSearchParams` object that can be passed to [train].
#'
#' @param resampler_parameters `ResamplerParameters` set by [setup_Resampler].
#' @param search_type Character: "exhaustive" or "randomized". Type of
#' grid search to use. Exhaustive search will try all combinations of
#' parameters. Randomized will try a random sample of size
#' `randomize_p` * `N of total combinations`
#' @param randomize_p Float (0, 1): For `search_type == "randomized"`,
#' randomly test this proportion of combinations.
#' @param metrics_aggregate_fn Character: Name of function to use to aggregate error metrics.
#' @param metric Character: Metric to minimize or maximize.
#' @param maximize Logical: If TRUE, maximize `metric`, otherwise minimize it.
#' @param parallel_type Character: Parallel backend to use.
#' @param n_workers Integer: Number of workers to use.
#'
#' @return A `GridSearchParams` object.
#'
#' @author EDG
#' @export
setup_GridSearch <- function(
  resampler_parameters = setup_Resampler(n_resamples = 5L, type = "KFold"),
  search_type = "exhaustive",
  randomize_p = NULL,
  metrics_aggregate_fn = "mean",
  metric = NULL,
  maximize = NULL,
  parallel_type = "future",
  n_workers = rtemis_workers
) {
  # Arguments ----
  check_is_S7(resampler_parameters, ResamplerParameters)
  check_inherits(search_type, "character")
  check_float01exc(randomize_p)
  if (search_type == "exhaustive" && !is.null(randomize_p)) {
    cli::cli_abort("search_type is 'exhaustive': do not set randomize_p.")
  }
  # check_inherits(metrics_aggregate_fn, "function")
  check_character(metrics_aggregate_fn)
  check_inherits(metric, "character")
  check_inherits(maximize, "logical")
  check_inherits(parallel_type, "character")
  n_workers <- clean_int(n_workers)
  GridSearchParams(
    resampler_parameters = resampler_parameters,
    search_type = search_type,
    randomize_p = randomize_p,
    metrics_aggregate_fn = metrics_aggregate_fn,
    metric = metric,
    maximize = maximize,
    parallel_type = parallel_type,
    n_workers = n_workers
  )
} # /setup_GridSearch

# Tuner ----
#' Tuner Class
#'
#' @field type Character: Type of tuner.
#' @field hyperparameters Named list of tunable and fixed hyperparameters.
#' @field tuning_results Data.frame: Tuning results.
#' @field best_hyperparameters Named list of best hyperparameter values. Includes only
#' hyperparameters that were tuned.
#'
#' @author EDG
#' @noRd
Tuner <- new_class(
  name = "Tuner",
  properties = list(
    type = class_character,
    hyperparameters = Hyperparameters,
    tuner_parameters = TunerParameters,
    tuning_results = class_list, # with 2 elements: metrics_training, metrics_validation
    best_hyperparameters = class_list
  )
) # /Tuner

# Describe Tuner ----
method(desc, Tuner) <- function(x) {
  if (x@type == "GridSearch") {
    paste(x@tuner_parameters[["search_type"]], "grid search")
  }
} # /rtemis::describe.Tuner

# GridSearch ----
#' GridSearch Class
#'
#' Tuner subclass for grid search.
#'
#' @author EDG
#' @noRd
GridSearch <- new_class(
  name = "GridSearch",
  parent = Tuner,
  constructor = function(
    hyperparameters,
    tuner_parameters,
    tuning_results,
    best_hyperparameters
  ) {
    type <- "GridSearch"
    new_object(
      Tuner(
        type = type,
        hyperparameters = hyperparameters,
        tuner_parameters = tuner_parameters,
        tuning_results = tuning_results,
        best_hyperparameters = best_hyperparameters
      )
    )
  }
) # /GridSearch

# Print GridSearch ----
#' Print GridSearch
#'
#' Print GridSearch object
#'
#' @param x GridSearch object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, GridSearch) <- function(x, header = TRUE, ...) {
  if (header) {
    objcat(paste(x@type))
  }
  type <- if (x@tuner_parameters[["search_type"]] == "exhaustive") {
    "An exhaustive grid search"
  } else {
    paste0(
      "A randomized grid search (p = ",
      x@tuner_parameters[["randomize_p"]],
      ")"
    )
  }
  n_param_combs <- NROW(x@tuning_results[["param_grid"]])
  cat(
    type,
    " of ",
    singorplu(n_param_combs, "parameter combination"),
    " was performed using ",
    desc(x@tuner_parameters[["resampler_parameters"]]),
    ".\n",
    sep = ""
  )
  cat(
    x@tuner_parameters[["metric"]],
    "was",
    ifelse(x@tuner_parameters[["maximize"]], "maximized", "minimized"),
    "with the following parameters:\n"
  )
  printls(x@best_hyperparameters)
  invisible(x)
} # /print.GridSearch

# describe.GridSearch ----
method(describe, GridSearch) <- function(x) {
  print(x, header = FALSE)
} # /describe.GridSearch

# Show GridSearch ----
#' Show GridSearch
#'
#' Show GridSearch object
#'
#' @param x GridSearch object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(repr, GridSearch) <- function(
  x,
  header = TRUE,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  output_type <- match.arg(output_type)
  out <- character()
  if (header) {
    out <- paste0(out, show_S7name(x@type), "\n")
  }
  type <- if (x@tuner_parameters[["search_type"]] == "exhaustive") {
    "An exhaustive grid search"
  } else {
    paste0(
      "A randomized grid search (p = ",
      x@tuner_parameters[["randomize_p"]],
      ")"
    )
  }
  n_param_combs <- NROW(x@tuning_results[["param_grid"]])
  out <- paste0(
    out,
    type,
    " of ",
    singorplu(n_param_combs, "parameter combination"),
    " was performed using ",
    desc(x@tuner_parameters[["resampler_parameters"]]),
    ".\n"
  )
  out <- paste(
    out,
    x@tuner_parameters[["metric"]],
    "was",
    ifelse(x@tuner_parameters[["maximize"]], "maximized", "minimized"),
    "with the following parameters:\n"
  )
  out <- paste(
    out,
    show_ls(x@best_hyperparameters, output_type = output_type),
    sep = ""
  )
  out
} # /show.GridSearch
