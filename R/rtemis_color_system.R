#' rtemis Color System
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
rtemis_light_teal <- "#00fdfd"
rtemis_light_blue <- "#30cefe"
rtemis_teal <- "#00b2b2"
kaimana_red <- "#ff004c"
kaimana_blue <- "#0067e0"
kaimana_light_blue <- "#479cff"
coastside_orange <- "#ff9f20"
rtemis_orange <- "#ff4f36"
kaimana_green <- "#00ffb3"
kaimana_med_green <- "#00996b"
rtemis_purple <- "#6125f7"
rtemis_magenta <- "#912ac8"
rtemis_magenta_light <- "#b25bd6"
magenta <- "#ff00ff"
lmd_burgundy <- "#a92459"
rtms_gray <- "#808080"

rt_gray <- rtms_gray
rt_red <- kaimana_red
rt_blue <- kaimana_light_blue
rt_green <- kaimana_med_green
rt_orange <- coastside_orange
rt_teal <- rtemis_teal
rt_purple <- rtemis_purple
rt_magenta <- rtemis_magenta_light

# %% rtemis colors ----
highlight_col <- rt_orange
col_object <- rt_gray # object name in repr_S7name
col_info <- highlight2_col <- lmd_burgundy
col_outer <- rt_green
col_tuner <- rt_blue
col_calibrator <- rt_magenta


#' rtemis Color System
#'
#' A named list of colors used consistently across all packages
#' in the rtemis ecosystem.
#'
#' Colors are provided as hex strings.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{red}{"kaimana red"}
#'   \item{blue}{"kaimana light blue"}
#'   \item{green}{"kaimana medium green"}
#'   \item{orange}{"coastside orange"}
#'   \item{teal}{"rtemis teal"}
#'   \item{purple}{"rtemis purple"}
#'   \item{magenta}{"rtemis magenta"}
#'   \item{highlight_col}{"highlight color"}
#'   \item{object}{"rtemis teal"}
#'   \item{info}{"lmd burgundy"}
#'   \item{outer}{"kaimana red"}
#'   \item{tuner}{"coastside orange"}
#' }
#'
#' @examples
#' rtemis_colors[["orange"]]
#'
#' @author EDG
#'
#' @export
#'
#' @examples
#' rtemis_colors[["teal"]]
rtemis_colors <- list(
  red = rt_red,
  blue = rt_blue,
  green = rt_green,
  orange = rt_orange,
  teal = rt_teal,
  purple = rt_purple,
  magenta = rt_magenta,
  highlight_col = highlight_col,
  object = col_object,
  info = col_info,
  outer = col_outer,
  tuner = col_tuner
) # /rtemis.utils::rtemis_colors
