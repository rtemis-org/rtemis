# S7_Themes.R
# ::rtemis::
# 2025 EDG rtemis.org

# %% Theme ----
#' Theme
#'
#' @field name Character: Name of theme.
#' @field config Named list of theme config.
#'
#' @author EDG
#' @noRd
Theme <- new_class(
  name = "Theme",
  properties = list(
    name = class_character,
    config = class_list
  )
) # /Theme


# %% print.Theme ----
#' Print Theme
#'
#' Print Theme object
#'
#' @param x `Theme` object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, Theme) <- function(x, ...) {
  objcat(paste(x@name, "Theme"))
  printls(props(x)[["config"]])
  invisible(x)
}


# %% `$`.Theme ----
# Make Theme@config `$`-accessible with autocomplete ----
method(`$`, Theme) <- function(x, name) {
  x@config[[name]]
} # /rtemis::Theme$


# %% `.DollarNames`.Theme ----
method(`.DollarNames`, Theme) <- function(x, pattern = "") {
  all_names <- names(x@config)
  grep(pattern, all_names, value = TRUE)
} # /rtemis::Theme.DollarNames


# %% `[[`.Theme ----
# Make Theme@config `[[`-accessible ----
method(`[[`, Theme) <- function(x, name) {
  x@config[[name]]
} # /rtemis::Theme[[]]


# %% names.Theme ----
#' Get names of Theme object
#'
#' @param x `Theme` object.
#'
#' @return Character vector of names of `Theme` object.
#'
#' @author EDG
#' @noRd
names.Theme <- function(x) {
  names(x@config)
} # /rtemis::names.Theme


# %% names.Theme method ----
method(names, Theme) <- function(x) {
  names.Theme(x)
} # /rtemis::names.Theme
