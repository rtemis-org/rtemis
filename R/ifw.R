# ifw.R
# ::rtemis::
# 2025- EDG rtemis.org

#' Inverse Frequency Weighting
#'
#' @param y Vector: Outcome
#' @param type Character: "case_weights" or "class_weights". What to return.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric vector of weights.
#'
#' @keywords internal
#' @noRd
#' @author EDG
#'
#' @examples
#' y <- factor(sample(c("A", "B"), size = 100, replace = TRUE, prob = c(.1, .9)))
#' ifw(y)
#' ifw(y, type = "class_weights")
ifw <- function(y, type = c("case_weights", "class_weights"), verbosity = 1L) {
  stopifnot(is.factor(y))
  type <- match.arg(type)
  if (verbosity > 0L) {
    msg(
      "Calculating",
      sub("_", " ", type),
      "using Inverse Frequency Weighting."
    )
  }

  # Class weights ----
  inverse_proportions <- 1 / (table(y) / NROW(y))
  class_weights <- structure(
    as.numeric(inverse_proportions / min(inverse_proportions)),
    names = names(inverse_proportions)
  )

  if (type == "class_weights") {
    out <- class_weights
    stopifnot(length(out) == nlevels(y))
  } else {
    out <- class_weights[as.integer(y)]
    stopifnot(length(out) == length(y))
  }
  out
} # /rtemis::ifw
