# msg.R
# ::rtemis::
# 2016- EDG rtemis.org

#' Info msg
#'
#' @author EDG
#' @keywords internal
#' @noRd
msg_info <- function(..., format_fn = highlight2, verbosity = 1L) {
  msg0(..., format_fn = format_fn, caller_id = 2, verbosity = verbosity)
}
