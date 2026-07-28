# 110_Tuner.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# S7
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7
# future
# https://www.futureverse.org/backends.html

# Description
# `TunerConfig` class and subclasses create objects that store tuner config.
# They are set by `setup_GridSearch()` and perform type checking and validation.
# They are used by `tune()`.
# `Tuner` class and subclasses create objects that store tuning results.
# They are created by `tune()`.

# Dev
# Should both class constructors (e.g. GridSearch@constructor) and setup functions
# (e.g. setup_GridSearch) perform type checking and validation?

# %% TunerConfig ----
#' TunerConfig
#'
#' Superclass for tuner config.
#'
#' @field type Character: Type of tuner.
#' @field config Named list of tuner config.
#'
#' @author EDG
#' @keywords internal
#' @noRd
TunerConfig <- new_class(
  name = "TunerConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    type = class_character,
    config = new_property(
      class_list,
      getter = function(self) {
        own_prop_values(self, TunerConfig)
      },
      setter = function(self, value) {
        route_config_assignment(self, TunerConfig, value)
      }
    )
  )
) # /rtemis::TunerConfig


# %% serializable_props.TunerConfig ----
# Serialize as {type, config} (the public shape); the per-tuner properties
# are redundant with the computed `config`.
method(serializable_props, TunerConfig) <- function(x) {
  list(type = x@type, config = config_prop_values(x, TunerConfig))
} # /rtemis::serializable_props.TunerConfig


# %% repr.TunerConfig ----
method(repr, TunerConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  paste0(
    repr_S7name(
      paste(x@type, "TunerConfig"),
      pad = pad,
      output_type = output_type
    ),
    repr_ls(x@config, pad = pad, output_type = output_type)
  )
} # /rtemis::repr.TunerConfig


# %% print.TunerConfig ----
method(print, TunerConfig) <- function(x, pad = 0L, ...) {
  cat(repr(x, pad = pad), "\n")
  invisible(x)
}

# %% desc.TunerConfig ----
method(desc, TunerConfig) <- function(x) {
  if (x@type == "GridSearch") {
    paste(x@config[["search_type"]], "grid search")
  }
}


# %% `$`.TunerConfig ----
# Make TunerConfig@config `$`-accessible
method(`$`, TunerConfig) <- function(x, name) {
  x@config[[name]]
}


# %% `.DollarNames`.TunerConfig ----
# `$`-autocomplete TunerConfig@config
method(`.DollarNames`, TunerConfig) <- function(x, pattern = "") {
  all_names <- names(x@config)
  grep(pattern, all_names, value = TRUE)
}


# %% `[[`.TunerConfig ----
# Make TunerConfig@config `[[`-accessible
method(`[[`, TunerConfig) <- function(x, name) {
  x@config[[name]]
}


# %% GridSearchConfig ----
#' GridSearchConfig
#'
#' @description
#' TunerConfig subclass for grid search config.
#'
#' @author EDG
#' @noRd
GridSearchConfig <- new_class(
  name = "GridSearchConfig",
  parent = TunerConfig,
  package = "rtemis",
  properties = list(
    type = prop_algorithm("GridSearch"),
    # Nested config object; serialized/validated as a ResamplerConfig, so it
    # is a plain property (excluded from generated schemas, where it is a
    # `$ref` to the resampler schema).
    resampler_config = NULL | ResamplerConfig,
    search_type = prop_string(
      "exhaustive",
      enum = c("exhaustive", "randomized"),
      description = "Grid search strategy."
    ),
    randomize_p = prop_float(
      NULL,
      exclusive_min = 0,
      exclusive_max = 1,
      nullable = TRUE,
      description = "Fraction of combinations to test when search_type is 'randomized'."
    ),
    metrics_aggregate_fn = prop_string(
      "mean",
      description = "Name of the function used to aggregate metrics across resamples."
    ),
    metric = prop_string(
      NULL,
      nullable = TRUE,
      description = "Metric to minimize or maximize. NULL = set from outcome type."
    ),
    maximize = prop_boolean(
      NULL,
      nullable = TRUE,
      description = "Maximize `metric` (otherwise minimize). NULL = set from the metric."
    )
  ),
  validator = function(self) {
    # `randomize_p` applies to, and is required by, a randomized search: it is
    # the sampling fraction `tune_GridSearch()` multiplies the combination
    # count by, so leaving it unset would fail there instead of here.
    if (self@search_type == "exhaustive" && !is.null(self@randomize_p)) {
      "@randomize_p must not be set when @search_type is 'exhaustive'."
    } else if (self@search_type == "randomized" && is.null(self@randomize_p)) {
      "@randomize_p must be set when @search_type is 'randomized'."
    }
  }
) # /rtemis::GridSearchConfig


# %% setup_GridSearch ----
#' Setup Grid Search Config
#'
#' Create a `GridSearchConfig` object that can be passed to [train].
#'
#' @param resampler_config `ResamplerConfig` set by [setup_Resampler].
#' @param search_type Character \{"exhaustive", "randomized"\}: Type of
#' grid search to use. Exhaustive search will try all combinations of
#' config. Randomized will try a random sample of size
#' `randomize_p` * `N of total combinations`
#' @param randomize_p Optional Numeric (0, 1): Randomly test this proportion of
#' combinations. Required when `search_type` is `"randomized"`; must be left
#' `NULL` when it is `"exhaustive"`.
#' @param metrics_aggregate_fn Character: Name of function to use to aggregate error metrics.
#' @param metric Optional Character: Metric to minimize or maximize. NULL sets
#' it from the outcome type.
#' @param maximize Optional Logical: If TRUE, maximize `metric`, otherwise
#' minimize it. NULL sets it from the metric.
#'
#' @return A `GridSearchConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' gridsearch_config <- setup_GridSearch(
#'   resampler_config = setup_Resampler(n_resamples = 5L, type = "KFold"),
#'   search_type = "exhaustive"
#' )
#' gridsearch_config
setup_GridSearch <- function(
  resampler_config = setup_Resampler(n_resamples = 5L, type = "KFold"),
  search_type = "exhaustive",
  randomize_p = NULL,
  metrics_aggregate_fn = "mean",
  metric = NULL,
  maximize = NULL
) {
  # Arguments ----
  # Per-field validation and the exhaustive/randomize_p rule are enforced by
  # the property specs and the `GridSearchConfig` validator.
  check_is_S7(resampler_config, ResamplerConfig)
  GridSearchConfig(
    resampler_config = resampler_config,
    search_type = search_type,
    randomize_p = randomize_p,
    metrics_aggregate_fn = metrics_aggregate_fn,
    metric = metric,
    maximize = maximize
  )
} # /rtemis::setup_GridSearch


# %% Tuner ----
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
  package = "rtemis",
  properties = list(
    type = class_character,
    hyperparameters = Hyperparameters,
    tuner_config = TunerConfig,
    tuning_results = class_list, # with 2 elements: metrics_training, metrics_validation
    best_hyperparameters = class_list
  )
) # /rtemis::Tuner


# %% desc.Tuner ----
method(desc, Tuner) <- function(x) {
  if (x@type == "GridSearch") {
    paste(x@tuner_config[["search_type"]], "grid search")
  }
} # /rtemis::describe.Tuner


# %% GridSearch ----
#' GridSearch Class
#'
#' Tuner subclass for grid search.
#'
#' @author EDG
#' @noRd
GridSearch <- new_class(
  name = "GridSearch",
  parent = Tuner,
  package = "rtemis",
  constructor = function(
    hyperparameters,
    tuner_config,
    tuning_results,
    best_hyperparameters
  ) {
    type <- "GridSearch"
    new_object(
      Tuner(
        type = type,
        hyperparameters = hyperparameters,
        tuner_config = tuner_config,
        tuning_results = tuning_results,
        best_hyperparameters = best_hyperparameters
      )
    )
  }
) # /rtemis::GridSearch


# print.GridSearch ----
#' Print GridSearch
#'
#' Print GridSearch object
#'
#' @param x GridSearch object.
#' @param header Logical: If TRUE, print header with type of tuner.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, GridSearch) <- function(x, header = TRUE, ...) {
  if (header) {
    objcat(paste(x@type))
  }
  type <- if (x@tuner_config[["search_type"]] == "exhaustive") {
    "An exhaustive grid search"
  } else {
    paste0(
      "A randomized grid search (p = ",
      x@tuner_config[["randomize_p"]],
      ")"
    )
  }
  n_param_combs <- NROW(x@tuning_results[["param_grid"]])
  cat(
    type,
    " of ",
    singorplu(n_param_combs, "parameter combination"),
    " was performed using ",
    desc(x@tuner_config[["resampler_config"]]),
    ".\n",
    sep = ""
  )
  cat(
    x@tuner_config[["metric"]],
    "was",
    ifelse(x@tuner_config[["maximize"]], "maximized", "minimized"),
    "with the following config:\n"
  )
  printls(x@best_hyperparameters)
  invisible(x)
} # /rtemis::print.GridSearch


# %% repr.GridSearch ----
method(repr, GridSearch) <- function(
  x,
  header = TRUE,
  pad = 0L,
  output_type = NULL,
  ...
) {
  out <- character()
  if (header) {
    out <- paste0(out, repr_S7name(x@type, pad = pad), "\n")
  }
  type <- if (x@tuner_config[["search_type"]] == "exhaustive") {
    "An exhaustive grid search"
  } else {
    paste0(
      "A randomized grid search (p = ",
      x@tuner_config[["randomize_p"]],
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
    desc(x@tuner_config[["resampler_config"]]),
    ".\n"
  )
  out <- paste(
    out,
    x@tuner_config[["metric"]],
    "was",
    ifelse(x@tuner_config[["maximize"]], "maximized", "minimized"),
    "with the following config:\n"
  )
  out <- paste(
    out,
    repr_ls(x@best_hyperparameters, pad = pad, output_type = output_type),
    sep = ""
  )
  out
} # /rtemis::repr.GridSearch


# %% .list_to_TunerConfig ----
#' Convert a list to a TunerConfig object
#'
#' Internal function used by `rtemis.server` and `SuperConfig` deserialization
#' to reconstruct a `TunerConfig` object from a named list. Not intended for
#' direct use by end users.
#'
#' @param x Named list with two elements:
#'   \describe{
#'     \item{`type`}{Character: tuner type. Currently only `"GridSearch"` is
#'       supported.}
#'     \item{`config`}{Named list of tuner configuration fields. For
#'       `"GridSearch"`: `resampler_config` (a list accepted by
#'       [.list_to_ResamplerConfig()]), `search_type`, `randomize_p`,
#'       `metrics_aggregate_fn`, `metric`, and `maximize`.}
#'   }
#'
#' @return A `TunerConfig` object (currently a `GridSearchConfig`).
#'
#' @author EDG
#' @keywords internal
#' @export
#' @examples
#' .list_to_TunerConfig(list(
#'   type = "GridSearch",
#'   config = list(
#'     resampler_config = list(type = "KFold", n = 5L),
#'     search_type = "exhaustive"
#'   )
#' ))
.list_to_TunerConfig <- function(x) {
  if (x[["type"]] == "GridSearch") {
    check_wire_keys(x, c("type", "config"), "tuner")
    config <- .drop_meta_keys(x[["config"]])
    check_wire_keys(
      config,
      names(GridSearchConfig@properties),
      "GridSearch tuner"
    )
    # Drop absent (NULL) elements so that `setup_GridSearch`'s own argument
    # defaults apply to whatever the config omits (e.g. `search_type`,
    # `metrics_aggregate_fn`, `resampler_config`) instead of passing NULL.
    args <- Filter(
      Negate(is.null),
      list(
        search_type = config[["search_type"]],
        randomize_p = config[["randomize_p"]],
        metrics_aggregate_fn = config[["metrics_aggregate_fn"]],
        metric = config[["metric"]],
        maximize = config[["maximize"]]
      )
    )
    if (!is.null(config[["resampler_config"]])) {
      args[["resampler_config"]] <- .list_to_ResamplerConfig(
        config[["resampler_config"]]
      )
    }
    do.call(setup_GridSearch, args)
  } else {
    rtemis.core::abort(
      "Unsupported tuner type: ",
      x[["type"]],
      class = "rtemis_unsupported_error"
    )
  }
} # /rtemis::.list_to_TunerConfig
