# print_ops.R
# ::rtemis::
# 2016- EDG rtemis.org

# printdf1
# ::rtemis::
# 2016 rtemis.org

#' List to HTML
#'
#' @author EDG
#' @keywords internal
#' @noRd
list2html <- function(
  x,
  sep = ": ",
  col = "#16A0AC",
  key_weight = 100,
  value_weight = 300,
  line = "<br>"
) {
  .names <- names(x)
  sapply(seq_along(x), \(i) {
    paste0(
      span(.names[i], style = paste0("font-weight:", key_weight, ";")),
      sep,
      span(
        x[[i]],
        style = paste0("color:", col, "; font-weight:", value_weight, ";")
      ),
      line
    )
  }) |>
    paste0(collapse = "") |>
    htmltools::HTML()
} # /rtemis::list2html


# %% inspect.class_data.frame ----
method(inspect, class_data.frame) <- function(x) {
  out <- paste0(
    fmt("<", col = "#808080"),
    fmt(class(x)[[1L]], col = highlight_col, bold = TRUE),
    fmt(">", col = "#808080"),
    " ",
    fmt(NROW(x), bold = TRUE),
    fmt(" x ", col = "#808080"),
    fmt(NCOL(x), bold = TRUE),
    "\n",
    repr_ls(x, pad = 0L, print_class = TRUE, print_df = TRUE)
  )
  cat(out)
  invisible(out)
} # /rtemis::inspect.class_data.frame
