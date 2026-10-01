# 210_MassUni.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% MassGLM ----
#' MassGLM
#'
#' @description
#' Superclass for mass-univariate models.
#'
#' @author EDG
#' @keywords internal
#' @noRd
MassGLM <- new_class(
  name = "MassGLM",
  package = "rtemis",
  properties = list(
    summary = class_data.table,
    ynames = class_character,
    xnames = class_character,
    coefnames = class_character,
    family = class_character
  )
) # /rtemis::MassGLM


# %% `$`.MassGLM ----
# Make MassGLM@name `$`-accessible ----
method(`$`, MassGLM) <- function(x, name) {
  prop(x, name)
}


# %% `.DollarNames`.MassGLM ----
# `$`-autocomplete MassGLM ----
method(`.DollarNames`, MassGLM) <- function(x, pattern = "") {
  prop_names <- names(props(x))
  grep(pattern, prop_names, value = TRUE)
}


# %% `[[`.MassGLM ----
# Make MassGLM@name `[[`-accessible ----
method(`[[`, MassGLM) <- function(x, name) {
  prop(x, name)
}


# %% repr.MassGLM ----
method(repr, MassGLM) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  paste0(
    repr_S7name("MassGLM", pad = pad),
    highlight(length(x@ynames)),
    " GLMs of family ",
    bold(x@family),
    " with ",
    highlight(length(x@xnames)),
    ngettext(length(x@xnames), " predictor", " predictors"),
    " each.",
    "\nAvailable coefficients: ",
    paste(highlight(x@coefnames), collapse = ", "),
    "\n"
  )
} # /rtemis::repr.MassGLM


# %% print.MassGLM ----
#' Print MassGLM
#'
#' @param x MassGLM object.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @author EDG
#' @noRd
method(print, MassGLM) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.MassGLM


# %% summary.MassGLM ----
method(summary, MassGLM) <- function(object, ...) {
  object@summary
} # /rtemis::summary.MassGLM
