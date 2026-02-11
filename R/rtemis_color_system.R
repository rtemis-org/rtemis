# rtemis color system
# ::rtemis.utils::
# 2025- EDG rtemis.org

# Violet: Class names (structure)
# Blue: Outer resampling (evaluation)
# Orange: Hyperparameter tuning (optimization)
# Green: Model training + important highlights (execution)
# Cyan: Info messages (communication)

# References
# ANSI escape codes
# https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
# Xterm color names: https://jonasjacek.github.io/colors/
# CSS color keywords: https://www.uxgem.com/docs/css-color-keywords
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html
# UTF-8 icons: https://www.utf8icons.com/

rtemis_light_teal <- "#00fdfd"
rtemis_light_blue <- "#30cefe"
rtemis_teal <- "#00b2b2"
kaimana_red <- "#ff004c"
kaimana_blue <- "#0067e0"
kaimana_light_blue <- "#479cff"
coastside_orange <- "#ff9f20"
kaimana_green <- "#00ffb3"
kaimana_med_green <- "#00996b"
rtemis_purple <- "#6125f7"
rtemis_magenta <- "#912ac8"
rtemis_magenta_light <- "#b25bd6"
magenta <- "#ff00ff"
lmd_burgundy <- "#a92459"

rt_red <- kaimana_red
rt_blue <- kaimana_light_blue
rt_green <- kaimana_med_green
rt_orange <- coastside_orange
rt_teal <- rtemis_teal
rt_purple <- rtemis_purple
rt_magenta <- rtemis_magenta

highlight_col <- rt_teal
col_object <- rt_teal
col_info <- highlight2_col <- lmd_burgundy
col_outer <- rt_red
col_tuner <- rt_orange # coastside orange


#' rtemis Color System
#'
#' A named list of colors used consistently across all packages
#' in the rtemis ecosystem.
#'
#' Colors are provided as hex strings.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{rt_red}{"kaimana red"}
#'   \item{rt_blue}{"kaimana light blue"}
#'   \item{rt_green}{"kaimana medium green"}
#'   \item{rt_orange}{"coastside orange"}
#'   \item{rt_teal}{"rtemis teal"}
#'   \item{rt_purple}{"rtemis purple"}
#'   \item{rt_magenta}{"rtemis magenta"}
#'   \item{highlight_col}{"rtemis teal"}
#'   \item{col_object}{"rtemis teal"}
#'   \item{col_info}{"lmd burgundy"}
#'   \item{col_outer}{"kaimana red"}
#'   \item{col_tuner}{"coastside orange"}
#' }
#'
#' @examples
#' rtemis_colors[["rt_teal"]]
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
