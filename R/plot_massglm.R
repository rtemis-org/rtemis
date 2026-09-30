# plot_massglm.R
# ::rtemis::
# 2026- EDG rtemis.org

#' Plot a MassGLM Result
#'
#' `plot(model)` draws a volcano plot using rtemis.draw.
#' `plot_manhattan(model)` draws categorical significance bars,
#' preserving outcome order. Both select one coefficient and use the shared
#' [rtemis.draw::SignificanceConfig] statistical contract via [rtemis.draw::draw_volcano()] or
#' [rtemis.draw::draw_manhattan()].
#'
#' `plot_manhattan.MassGLM()` is deprecated; use `plot_manhattan()`.
#'
#' @param x `rtemis::MassGLM`: Fitted mass-univariate model.
#' @param coefname Optional Character scalar: Coefficient to plot. Unset selects
#'   the first entry in the object's `coefnames`.
#' @param ... Named settings for [rtemis.draw::draw_volcano()] or [rtemis.draw::draw_manhattan()].
#' @return An ECharts htmlwidget.
#' @export
#' @examplesIf interactive()
#' model <- massGLM(mtcars["wt"], mtcars[c("mpg", "hp", "disp")],
#'                          verbosity = 0L)
#' plot(model, coefname = "wt")
#' plot_manhattan(model, coefname = "wt")
plot_manhattan <- new_generic(
  "plot_manhattan",
  "x",
  function(x, coefname = NULL, ...) {
    force_supplied()
    S7_dispatch()
  }
)


#' Extract and align a MassGLM coefficient across outcomes
#'
#' The summary's Variable column identifies rows; align it to ynames before
#' selecting values so a reordered summary cannot mislabel an outcome.
#'
#' @inheritParams plot_manhattan
#' @return List containing `data` and the selected `coefname`.
#' @keywords internal
#' @noRd
massglm_plot_data <- new_generic("massglm_plot_data", "x")

#' @rdname massglm_plot_data
#' @keywords internal
#' @noRd
extract_massglm_plot_data <- function(x, coefname = NULL) {
  available <- x@coefnames
  if (!length(available)) {
    abort(
      "The MassGLM object has no coefficients; fit at least one estimable term.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  coefname <- coefname %||% available[[1L]]
  if (
    !is.character(coefname) ||
      length(coefname) != 1L ||
      is.na(coefname) ||
      !coefname %in% available
  ) {
    abort(
      "Select one `coefname` from: ",
      paste(available, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  columns <- c(
    "Variable",
    paste0("Coefficient_", coefname),
    paste0("p_value_", coefname)
  )
  table <- x@summary
  labels <- x@ynames
  if (!all(columns %in% names(table)) || anyDuplicated(names(table))) {
    abort(
      "The MassGLM summary must contain unique Variable, coefficient, and p-value columns; rebuild the model summary.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  keys <- table[["Variable"]]
  if (
    !is.character(keys) ||
      anyNA(keys) ||
      anyDuplicated(keys) ||
      !length(labels) ||
      anyNA(labels) ||
      anyDuplicated(labels) ||
      any(!nzchar(labels)) ||
      !setequal(keys, labels)
  ) {
    abort(
      "Match unique summary Variable names to the MassGLM outcome names before plotting.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  at <- match(labels, keys)
  data <- data.frame(
    estimate = table[[columns[[2L]]]][at],
    p_value = table[[columns[[3L]]]][at],
    label = labels
  )
  list(data = data, coefname = coefname)
}


#' Draw the default volcano view of a MassGLM object
#' @inheritParams plot_manhattan
#' @param title Optional Character: Chart title; unset names the coefficient.
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
draw_massglm_volcano <- function(x, coefname = NULL, ..., title = NULL) {
  extracted <- massglm_plot_data(x, coefname)
  data <- extracted[["data"]]
  if (missing(title)) {
    title <- paste("MassGLM:", extracted[["coefname"]])
  }
  rtemis.draw::draw_volcano(
    data[["estimate"]],
    data[["p_value"]],
    data[["label"]],
    title = title,
    ...
  )
}


#' Draw the categorical Manhattan view of a MassGLM object
#' @inheritParams draw_massglm_volcano
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
draw_massglm_manhattan <- function(x, coefname = NULL, ..., title = NULL) {
  extracted <- massglm_plot_data(x, coefname)
  data <- extracted[["data"]]
  if (missing(title)) {
    title <- paste("MassGLM:", extracted[["coefname"]])
  }
  rtemis.draw::draw_manhattan(
    data[["estimate"]],
    data[["p_value"]],
    data[["label"]],
    title = title,
    ...
  )
}


# %% plot_manhattan.MassGLM ----
#' @rdname plot_manhattan
#' @export
plot_manhattan.MassGLM <- function(x, coefname = NULL, ...) {
  .Deprecated("plot_manhattan", package = "rtemis")
  plot_manhattan(x, coefname = coefname, ...)
}


# %% plot.MassGLM ----
#' @rdname plot_manhattan
#' @export
plot.MassGLM <- function(x, ...) {
  draw_massglm_volcano(x, ...)
}
