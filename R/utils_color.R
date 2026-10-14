# utils_color.R
# ::rtemis::
# 2016- EDG rtemis.org

# %% sign_colors ----
#' Colors for a coefficient's sign
#'
#' The package-wide meaning of a coefficient's sign. Every plot that colors by
#' sign reads from here, so the same hue means the same thing to a reader moving
#' between a volcano plot, a Manhattan plot, and a LINAD coefficient table.
#'
#' The colors are rtemis.draw's, read at call time: `plot()` and
#' `plot_manhattan()` on a `MassGLM` draw through rtemis.draw, so that package
#' owns the pair and every other rtemis plot follows it. A `Theme` that
#' declares sign colors supplies them; otherwise the defaults of
#' `rtemis.draw::setup_SignificanceConfig()` do.
#'
#' @return Named character vector: `negative`, `neutral`, `positive`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
sign_colors <- function() {
  props <- c("negative_color", "neutral_color", "positive_color")
  theme <- rtemis.draw::theme_light()
  source <- if (all(props %in% S7::prop_names(theme))) {
    theme
  } else {
    rtemis.draw::setup_SignificanceConfig()
  }
  stats::setNames(
    vapply(props, function(p) S7::prop(source, p), character(1L)),
    c("negative", "neutral", "positive")
  )
} # /rtemis::sign_colors


# %% split_colors ----
#' Data split colors
#'
#' Fixed color map for the training, validation and test data splits, so a
#' split has the same color in every model plot: training teal, validation
#' purple, test magenta. A plot that shows one split keeps that split's color.
#'
#' @param splits Optional Character: Split names, in display order, matched
#'   case-insensitively against "training", "validation" and "test". `NULL`
#'   returns the whole map.
#'
#' @return Named character vector of hex colors, one per element of `splits`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
split_colors <- function(splits = NULL) {
  rtemis.core::check_character(splits, allow_null = TRUE)
  fixed <- c(
    training = rtemis_colors[["teal"]],
    validation = rtemis_colors[["purple"]],
    test = rtemis_colors[["magenta"]]
  )
  if (is.null(splits)) {
    return(fixed)
  }
  key <- tolower(splits)
  unknown <- setdiff(key, names(fixed))
  if (length(unknown)) {
    abort(
      "Name splits as training, validation, or test; received: ",
      paste(unknown, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  stats::setNames(unname(fixed[key]), splits)
} # /rtemis::split_colors


# %% split_palette ----
#' Add split colors to drawing arguments
#'
#' Sets `palette` to the colors of `splits` unless the caller supplies one.
#'
#' @param args List: Named drawing arguments.
#' @param splits Character: Split names, in series order.
#' @param extra Optional Character: Colors for series that follow the splits.
#'
#' @return List: `args`, with `palette` set.
#'
#' @author EDG
#' @keywords internal
#' @noRd
split_palette <- function(args, splits, extra = NULL) {
  if (is.null(args[["palette"]])) {
    args[["palette"]] <- c(unname(split_colors(splits)), extra)
  }
  args
} # /rtemis::split_palette


# %% confusion_colors ----
#' Colors for correct and incorrect predictions
#'
#' Confusion matrices shade correct predictions toward blue and incorrect
#' predictions toward orange. The pair stays distinct under color-vision
#' deficiencies and from every `split_colors()` hue. It is rtemis.draw's sign
#' pair from version 0.5.4; the hexes are fixed here so that model plots use
#' them with any supported rtemis.draw version.
#'
#' @return Named character vector: `correct`, `incorrect`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
confusion_colors <- function() {
  c(correct = "#2D83D3", incorrect = "#C55E28")
} # /rtemis::confusion_colors


# %% col2grayscale ----
#' Color to grayscale
#'
#' Uses the NTSC grayscale conversion: 0.299 * R + 0.587 * G + 0.114 * B.
#'
#' @param x Character: Colors.
#' @param what Character \{"color", "decimal"\}: "color" returns a hexadecimal
#' color, "decimal" a perceived luminance in `[0, 1]`.
#'
#' @return Character or Numeric vector, parallel to `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
col2grayscale <- function(x, what = c("color", "decimal")) {
  what <- match.arg(what)
  col <- col2rgb(x)
  gs <- (0.299 * col[1, ] + 0.587 * col[2, ] + 0.114 * col[3, ]) / 255
  if (what == "color") {
    grDevices::gray(gs)
  } else {
    gs
  }
} # /rtemis::col2grayscale


# %% css2hex ----
#' Convert a CSS color to a hexadecimal color
#'
#' Reads `rgb()` and `rgba()` functional notation, which rtemis.draw themes use
#' for text colors, and any color `grDevices::col2rgb()` reads.
#'
#' @param x Character: Colors.
#'
#' @return Character vector of `#RRGGBBAA` colors, parallel to `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
css2hex <- function(x) {
  vapply(
    x,
    function(color) {
      functional <- regmatches(
        color,
        regexec(
          "^\\s*rgba?\\(\\s*([0-9.]+)\\s*,\\s*([0-9.]+)\\s*,\\s*([0-9.]+)\\s*(?:,\\s*([0-9.]+)\\s*)?\\)\\s*$",
          color,
          perl = TRUE
        )
      )[[1L]]
      if (length(functional) == 0L) {
        rgba <- col2rgb(color, alpha = TRUE)[, 1L]
        return(grDevices::rgb(
          rgba[[1L]],
          rgba[[2L]],
          rgba[[3L]],
          rgba[[4L]],
          maxColorValue = 255
        ))
      }
      channels <- as.numeric(functional[2:4])
      alpha <- if (nzchar(functional[[5L]])) as.numeric(functional[[5L]]) else 1
      grDevices::rgb(
        channels[[1L]],
        channels[[2L]],
        channels[[3L]],
        round(alpha * 255),
        maxColorValue = 255
      )
    },
    character(1L),
    USE.NAMES = FALSE
  )
} # /rtemis::css2hex
