# html_ops.R
# ::rtemis::
# 2023- EDG rtemis.org

#' @keywords internal
#' @noRd
html_highlight <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #16A0AC; font-weight: 700;")
  } else {
    span(..., style = "color: #16A0AC;")
  }
}


#' @keywords internal
#' @noRd
html_orange <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #FA6E1E; font-weight: 700;")
  } else {
    span(..., style = "color: #FA6E1E;")
  }
}


#' @keywords internal
#' @noRd
html_red <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #E61048; font-weight: 700;")
  } else {
    span(..., style = "color: #E61048;")
  }
}


#' @keywords internal
#' @noRd
html_success <- function(..., bold = TRUE) {
  if (bold) {
    span(..., style = "color: #32A03E; font-weight: 700;")
  } else {
    span(..., style = "color: #32A03E;")
  }
}
