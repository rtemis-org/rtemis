# 240_S7utils.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% SuperWorkers ----
#' @keywords internal
#' @noRd
SuperWorkers <- new_class(
  name = "SuperWorkers",
  properties = list(
    algorithm = class_character,
    max_workers = class_integer,
    max_workers_algorithm = class_integer,
    max_workers_tuning = class_integer,
    max_workers_resampling = class_integer
  ),
  constructor = function(
    algorithm,
    max_workers,
    max_workers_algorithm,
    max_workers_tuning,
    max_workers_resampling
  ) {
    max_workers <- clean_posint(max_workers)
    max_workers_algorithm <- clean_posint(max_workers_algorithm)
    max_workers_tuning <- clean_posint(max_workers_tuning)
    max_workers_resampling <- clean_posint(max_workers_resampling)
    # Validate input
    if (
      max_workers_algorithm + max_workers_tuning + max_workers_resampling >
        max_workers
    ) {
      rtemis.core::abort(
        "Total workers for algorithm, tuning, and resampling cannot exceed max_workers.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    new_object(
      S7_object(),
      algorithm = algorithm,
      max_workers = max_workers,
      max_workers_algorithm = max_workers_algorithm,
      max_workers_tuning = max_workers_tuning,
      max_workers_resampling = max_workers_resampling
    )
  }
) # /rtemis::SuperWorkers


# %% BiasVariance ----
BiasVariance <- new_class(
  name = "BiasVariance",
  properties = list(
    bias_squared = class_numeric,
    mean_bias_squared = class_numeric,
    sd_bias_squared = class_numeric,
    variance = class_numeric,
    mean_variance = class_numeric,
    sd_variance = class_numeric
  )
)

# %% `$`.BiasVariance ----
method(`$`, BiasVariance) <- function(x, name) {
  prop_by_name(x, name)
}


# %% `.DollarNames`.BiasVariance ----
method(`.DollarNames`, BiasVariance) <- function(x, pattern = "") {
  prop_names_matching(x, pattern)
}


# %% `[[`.BiasVariance ----
method(`[[`, BiasVariance) <- function(x, name) {
  prop_by_name(x, name)
}


# %% repr.BiasVariance ----
method(repr, BiasVariance) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  paste0(
    repr_S7name("BiasVariance"),
    "Mean squared bias: ",
    highlight(ddSci(x[["mean_bias_squared"]]), output_type = output_type),
    " (",
    ddSci(x[["sd_bias_squared"]]),
    ")\n",
    "Mean variance: ",
    highlight(
      ddSci(x[["mean_variance"]]),
      output_type = output_type
    ),
    " (",
    ddSci(x[["sd_variance"]]),
    ")\n"
  )
} # /rtemis::repr.BiasVariance


# %% print.BiasVariance ----
#' Print method for BiasVariance
#'
#' @param x BiasVariance object.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @author EDG
#' @noRd
method(print, BiasVariance) <- function(x, ...) {
  cat(repr(x))
  invisible(x)
} # /rtemis::print.BiasVariance


# %% prop_by_name ----
#' Read an S7 property by name
#'
#' The `$` and `[[` methods of result objects read properties through this
#' function, so that both operators accept exactly the property names, which
#' `.DollarNames` also completes.
#'
#' @param x S7 object.
#' @param name Character: Property name.
#'
#' @return The value of the property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_by_name <- function(x, name) {
  available <- prop_names(x)
  if (!is.character(name) || length(name) != 1L || !name %in% available) {
    abort(
      "No property named ",
      encodeString(paste(format(name), collapse = " "), quote = "'"),
      " in ",
      S7_class(x)@name,
      " object. Available properties: ",
      paste(available, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  prop(x, name)
} # /rtemis::prop_by_name


# %% prop_names_matching ----
#' Property names matching a pattern
#'
#' The `.DollarNames` methods of result objects complete property names
#' through this function.
#'
#' @param x S7 object.
#' @param pattern Character: Regular expression.
#'
#' @return Character vector of property names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_names_matching <- function(x, pattern = "") {
  grep(pattern, prop_names(x), value = TRUE)
} # /rtemis::prop_names_matching
