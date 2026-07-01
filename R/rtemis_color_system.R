# %% rtemis colors ----
highlight_col <- rtemis_colors[["teal"]]
col_success <- "#00cc8f" # session node ok
col_info <- rtemis_colors[["blue"]]
col_error <- rtemis_colors[["red"]] # session node error
col_warn <- rtemis_colors[["orange"]] # session node aborted/warning

col_preprocessor <- rtemis_colors[["juniper"]]
col_decom <- rtemis_colors[["green"]]
col_tuner <- rtemis_colors[["pink"]]
col_outer <- rtemis_colors[["magenta"]]
col_calibrator <- rtemis_colors[["purple"]]


# %% show_color_key() ----
#' Show rtemis color key
#'
#' @return Used for side-effects: prints color key to console. Returns invisible NULL.
#'
#' @author EDG
#' @export
#' @examples
#' show_color_key()
show_color_key <- function() {
  x <- list(
    `General highlight` = highlight_col,
    Info = col_info,
    Warn = col_warn,
    Error = col_error,
    Success = col_success,
    Preprocessing = col_preprocessor,
    Decomposition = col_decom,
    `Outer Resampling` = col_outer,
    Tuning = col_tuner
  )
  out <- show_col(x, title = "rtemis Color System")
  cat(out)
  invisible(NULL)
} # /show_color_key
