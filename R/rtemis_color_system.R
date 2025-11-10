# rtemis color system
# ::rtemis::
# 2025 EDG rtemis.org

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
genlib_orange <- "#ff9f20"
kaimana_green <- "#00ffb3"
rtemis_purple <- "#6125f7"
rtemis_magenta <- "#912ac8"
rtemis_magenta_light <- "#b25bd6"
magenta <- "#ff00ff"
lmd_burgundy <- "#a92459"

rt_red <- kaimana_red
rt_blue <- kaimana_light_blue
rt_green <- kaimana_green
rt_orange <- genlib_orange
rt_teal <- rtemis_teal
rt_purple <- rtemis_purple
rt_magenta <- rtemis_magenta

highlight_col <- rt_teal
col_object <- rt_teal
col_info <- highlight2_col <- lmd_burgundy
col_outer <- rt_red
col_tuner <- rt_orange # genlib orange
