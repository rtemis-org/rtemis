# 005_HyperparameterDomain.R
# ::rtemis::
# 2026- EDG rtemis.org

# The values a tuner may choose among for one hyperparameter. Three levels of
# vocabulary, each with exactly one home:
#
#   domain  the candidates for ONE hyperparameter          -- this class
#   space   the product of the domains in a config         -- implicit
#   grid    the space enumerated, one row per combination  -- `tuning_grid()`
#
# A domain is a value like any other: it is stored in the property it was
# assigned to, serialized by `to_json()`, and read back by the `.list_to_*()`
# reconstructors. `tune_over()` is the only way to build one, which is what
# makes a search space unambiguous in R -- a bare vector is always a value.
#
# On the wire there are no function calls, so a domain is written structurally,
# one level of nesting above the value: an array of values for a scalar
# hyperparameter, an array of arrays for a vector-valued one. That nesting rule
# is the wire contract; `HyperparameterDomain` is its R form, and the two are
# generated from one `PropertySpec` (see `make_prop()` and `spec_to_schema()`).

# %% HyperparameterDomain ----
#' The values a tuner may choose among for one hyperparameter
#'
#' Built by [tune_over()] and stored in the hyperparameter it is assigned to, so
#' that a search space is a distinct type rather than a vector that has to be
#' told apart from a value by counting.
#'
#' @param candidates List: The values to search over, one per element.
#' @param from_vector Logical: TRUE when the candidates were read out of a
#' single bare vector. A vector is also how one value of a vector-valued
#' hyperparameter is written, so that one combination cannot be inferred; this
#' records it so the hyperparameter can reject it with a correction rather than
#' silently take each element as a candidate.
#'
#' @return `HyperparameterDomain` object.
#'
#' @author EDG
#' @export
#' @examples
#' # Built by tune_over(), not directly.
#' d <- tune_over(3L, 4L, 5L)
#' d@candidates
HyperparameterDomain <- new_class(
  name = "HyperparameterDomain",
  package = "rtemis",
  properties = list(
    candidates = class_list,
    from_vector = new_property(class_logical, default = FALSE)
  ),
  validator = function(self) {
    if (length(self@candidates) < 2L) {
      return(
        "must hold at least two candidates: a one-value search costs a full resampling pass and can only return the value it was given."
      )
    }
    NULL
  }
) # /rtemis::HyperparameterDomain


# %% tune_over ----
#' Mark the values to tune a hyperparameter over
#'
#' Declares that a hyperparameter should be *searched over* rather than set. It
#' is the only way to express a search space: a bare vector is always a value,
#' so nothing has to be inferred from how many elements it happens to have.
#'
#' @details
#' Give it the candidates. Several arguments are one candidate each; a single
#' argument holding several values *is* the candidates, so a grid you computed
#' can be passed straight in:
#'
#' ```r
#' setup_LightRF(max_depth = tune_over(3L, 4L, 5L))
#' setup_LightRF(lambda_l2 = tune_over(10^seq(-4, 0, length.out = 5)))
#' setup_MLP(hidden_units = tune_over(c(12L, 6L, 2L), c(14L, 12L, 6L, 2L)))
#' ```
#'
#' For a **vector-valued** hyperparameter one candidate is itself a vector, so
#' pass the candidates as separate arguments or as a list --
#' `tune_over(list(c(12L, 6L), c(24L, 12L)))`. A single bare vector there would
#' be one architecture rather than a search, and is an error saying so; nothing
#' is guessed.
#'
#' Passing it to a hyperparameter that is not tunable is an error: that
#' hyperparameter accepts a value only.
#'
#' @param ... The candidates to search over: one per argument, or a single list
#' or vector read against the type the hyperparameter declares.
#'
#' @return `HyperparameterDomain` object.
#'
#' @author EDG
#' @export
#' @examples
#' hp <- setup_LightRF(max_depth = tune_over(3L, 4L, 5L))
#' hp@max_depth
#' # A bare vector is a value, never a search space.
#' setup_LightRF(max_depth = 3L)@max_depth
tune_over <- function(...) {
  args <- list(...)
  if (length(args) == 0L) {
    rtemis.core::abort(
      "`tune_over()` was given no candidates, so there is nothing to search.\n",
      "It marks the values a tuner should choose among, so it needs at least two.\n",
      "Set the hyperparameter to a value directly if it should not be tuned.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (length(args) > 1L) {
    return(HyperparameterDomain(candidates = args))
  }
  # One argument holding the candidates. A list is unambiguous. A bare vector is
  # not, because that is also how a single value of a vector-valued
  # hyperparameter is written, so the reading is recorded and checked against
  # the hyperparameter rather than assumed.
  if (is.list(args[[1L]])) {
    return(HyperparameterDomain(candidates = args[[1L]]))
  }
  HyperparameterDomain(
    candidates = as.list(args[[1L]]),
    from_vector = TRUE
  )
} # /rtemis::tune_over


# %% is_domain ----
#' Is a value a hyperparameter domain?
#'
#' @param x Value to test.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_domain <- function(x) {
  S7_inherits(x, HyperparameterDomain)
} # /rtemis::is_domain


# %% clean_int ----
#' Coerce to integer, reaching inside a hyperparameter domain
#'
#' Shadows `rtemis.core::clean_int()` within this package. A `setup_*()` cleans
#' its arguments before constructing its class, and by then a tunable
#' hyperparameter may hold a domain; cleaning that object rather than the values
#' inside it would abort on the type. Each candidate is cleaned instead, so
#' `tune_over(3, 4, 5)` reaches an integer hyperparameter exactly as `3L` does.
#'
#' @param x Value or `HyperparameterDomain` object to coerce.
#' @param ... Passed to `rtemis.core::clean_int()`.
#'
#' @return `x` coerced, or a domain whose candidates are.
#'
#' @author EDG
#' @keywords internal
#' @noRd
clean_int <- function(x, ...) {
  if (is_domain(x)) {
    x@candidates <- lapply(x@candidates, rtemis.core::clean_int, ...)
    return(x)
  }
  rtemis.core::clean_int(x, ...)
} # /rtemis::clean_int


# %% clean_posint ----
#' Coerce to positive integer, reaching inside a hyperparameter domain
#'
#' Shadows `rtemis.core::clean_posint()` within this package, for the reason
#' given on `clean_int()`.
#'
#' @param x Value or `HyperparameterDomain` object to coerce.
#' @param ... Passed to `rtemis.core::clean_posint()`.
#'
#' @return `x` coerced, or a domain whose candidates are.
#'
#' @author EDG
#' @keywords internal
#' @noRd
clean_posint <- function(x, ...) {
  if (is_domain(x)) {
    x@candidates <- lapply(x@candidates, rtemis.core::clean_posint, ...)
    return(x)
  }
  rtemis.core::clean_posint(x, ...)
} # /rtemis::clean_posint


# %% domain_values ----
#' Every value a hyperparameter may take
#'
#' A domain's candidates, or the value itself when there is no domain. A check
#' that must hold for whatever the tuner eventually picks is then written once
#' over the values, rather than branching on whether tuning is in play.
#'
#' Candidates are flattened, so this suits a scalar hyperparameter. A
#' vector-valued one needs `@candidates` directly, since flattening would merge
#' its values.
#'
#' @param x Value or `HyperparameterDomain` object.
#'
#' @return Vector of the values `x` may take.
#'
#' @author EDG
#' @keywords internal
#' @noRd
domain_values <- function(x) {
  if (is_domain(x)) {
    unlist(x@candidates, use.names = FALSE)
  } else {
    x
  }
} # /rtemis::domain_values
