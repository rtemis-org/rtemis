# 005_HyperparameterCandidates.R
# ::rtemis::
# 2026- EDG rtemis.org

# The values a tuner may choose among for one hyperparameter. Four levels of
# vocabulary, each with exactly one home:
#
#   domain      what one hyperparameter may take    -- the general idea
#   candidates  a domain given by enumeration       -- this class
#   space       the product of the domains          -- implicit
#   grid        the space enumerated, one row each  -- `tuning_grid()`
#
# Only the enumerated kind of domain exists today. A domain a tuner samples
# rather than enumerates -- a log-uniform range, say -- is a sibling of this
# class rather than a variant of it, sharing no structure with a list of
# values, which is why the class names the enumeration and not the idea.
#
# A domain is a value like any other: it is stored in the property it was
# assigned to, serialized by `to_json()`, and read back by the `.list_to_*()`
# reconstructors. `tune_over()` is the only way to build one, which is what
# makes a search space unambiguous in R -- a bare vector is always a value.
#
# On the wire it is tagged -- `{"candidates": [...]}` -- so a reader tells a
# search space from a value with no reference to the declared type. The tag
# names the *kind* of domain, leaving a sampled one to arrive beside it rather
# than in place of it. Both forms are generated from one `PropertySpec` (see
# `make_prop()` and `spec_to_schema()`).

# %% HyperparameterCandidates ----
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
#' @return `HyperparameterCandidates` object.
#'
#' @author EDG
#' @export
#' @examples
#' # Built by tune_over(), not directly.
#' d <- tune_over(3L, 4L, 5L)
#' d@candidates
HyperparameterCandidates <- new_class(
  name = "HyperparameterCandidates",
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
) # /rtemis::HyperparameterCandidates


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
#' @return `HyperparameterCandidates` object.
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
    return(HyperparameterCandidates(candidates = args))
  }
  # One argument holding the candidates. A list is unambiguous. A bare vector is
  # not, because that is also how a single value of a vector-valued
  # hyperparameter is written, so the reading is recorded and checked against
  # the hyperparameter rather than assumed.
  if (is.list(args[[1L]])) {
    return(HyperparameterCandidates(candidates = args[[1L]]))
  }
  HyperparameterCandidates(
    candidates = as.list(args[[1L]]),
    from_vector = TRUE
  )
} # /rtemis::tune_over


# %% is_candidates ----
#' Is a value a hyperparameter domain?
#'
#' @param x Value to test.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_candidates <- function(x) {
  S7_inherits(x, HyperparameterCandidates)
} # /rtemis::is_candidates


# %% clean_int ----
#' Coerce to integer, reaching inside a hyperparameter domain
#'
#' Shadows `rtemis.core::clean_int()` within this package. A `setup_*()` cleans
#' its arguments before constructing its class, and by then a tunable
#' hyperparameter may hold a domain; cleaning that object rather than the values
#' inside it would abort on the type. Each candidate is cleaned instead, so
#' `tune_over(3, 4, 5)` reaches an integer hyperparameter exactly as `3L` does.
#'
#' @param x Value or `HyperparameterCandidates` object to coerce.
#' @param ... Passed to `rtemis.core::clean_int()`.
#'
#' @return `x` coerced, or a domain whose candidates are.
#'
#' @author EDG
#' @keywords internal
#' @noRd
clean_int <- function(x, ...) {
  if (is_candidates(x)) {
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
#' @param x Value or `HyperparameterCandidates` object to coerce.
#' @param ... Passed to `rtemis.core::clean_posint()`.
#'
#' @return `x` coerced, or a domain whose candidates are.
#'
#' @author EDG
#' @keywords internal
#' @noRd
clean_posint <- function(x, ...) {
  if (is_candidates(x)) {
    x@candidates <- lapply(x@candidates, rtemis.core::clean_posint, ...)
    return(x)
  }
  rtemis.core::clean_posint(x, ...)
} # /rtemis::clean_posint


# %% candidate_values ----
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
#' @param x Value or `HyperparameterCandidates` object.
#'
#' @return Vector of the values `x` may take.
#'
#' @author EDG
#' @keywords internal
#' @noRd
candidate_values <- function(x) {
  if (is_candidates(x)) {
    unlist(x@candidates, use.names = FALSE)
  } else {
    x
  }
} # /rtemis::candidate_values


# %% is_wire_candidates ----
#' Is this the wire form of a hyperparameter domain?
#'
#' The tag `wire_value()` writes, `{"candidates": [...]}`. Shared so the key is
#' named in one place: `from_wire()` rebuilds the R object from it, and the
#' record writer reads it to tell a hyperparameter the run *searched* from one
#' it derived -- that record works in wire values, never in R objects.
#'
#' @param x Value to test, as parsed from JSON or produced by `wire_value()`.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_wire_candidates <- function(x) {
  identical(names(x), "candidates")
} # /rtemis::is_wire_candidates


# %% repr.HyperparameterCandidates ----
#' Represent a hyperparameter's candidates
#'
#' Plain language, not R syntax: the reader wants to know which values are being
#' tried, and `3L` or `c(48, 24)` puts literal spelling in the way of that. A
#' candidate that is itself a vector is shown parenthesized, so a list of
#' architectures stays legible as a list of architectures.
#'
#' One line, so that `repr_ls()` prints it on the hyperparameter's own line
#' alongside the values, and short, so that it fits there. The `<tune>` tag
#' occupies the slot the type tag does for a value, in the tuner color: the
#' hyperparameter holds a search space rather than a number.
#'
#' @param x `HyperparameterCandidates` object.
#' @param limit Integer: Most candidates to show before eliding; -1 shows all.
#' @param output_type Character: Passed to `fmt()`.
#' @param ... Not used.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, HyperparameterCandidates) <- function(
  x,
  limit = 6L,
  output_type = NULL,
  ...
) {
  shown <- vapply(
    x@candidates,
    function(v) {
      one <- paste(format(v, trim = TRUE), collapse = ", ")
      if (length(v) > 1L) paste0("(", one, ")") else one
    },
    character(1L)
  )
  elided <- limit > 0L && length(shown) > limit
  if (elided) {
    shown <- shown[seq_len(limit)]
  }
  fmt(
    paste0(
      "<tune> ",
      paste(shown, collapse = ", "),
      # The count only tells the reader something the list does not when the
      # list is cut short.
      if (elided) paste0(", ... (", length(x@candidates), " values)") else ""
    ),
    col = col_tuner,
    output_type = output_type
  )
} # /rtemis::repr.HyperparameterCandidates


# %% print.HyperparameterCandidates ----
#' @author EDG
#' @keywords internal
#' @noRd
method(print, HyperparameterCandidates) <- function(
  x,
  output_type = NULL,
  ...
) {
  cat(repr(x, output_type = output_type), "\n")
  invisible(x)
} # /rtemis::print.HyperparameterCandidates
