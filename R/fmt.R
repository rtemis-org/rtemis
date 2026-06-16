# fmt.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% map_value_to_color ----
#' Map numeric value to color
#'
#' Maps a numeric value to a color based on a specified range and color palette using `fmt`
#' for formatting. Useful for visualizing numeric values in text output.
#'
#' @param x Numeric: Value to map to a color.
#' @param range Numeric vector of length 2: Minimum and maximum values for mapping.
#' @param colors Character vector: Colors to use for the gradient mapping.
#' @param space Character {"rgb", "Lab"}: Color space for gradient interpolation.
#' @param bold Logical: If TRUE, make text bold.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with color corresponding to the numeric value.
#'
#' @author EDG
#' @keywords internal
#' @noRd
map_value_to_color <- function(
  x,
  range = c(0, 1),
  colors = c("#ff9f20", "#00b2b2"),
  space = "Lab",
  bold = TRUE,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)
  if (output_type == "plain") {
    return(as.character(x))
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    rtemis.core::abort(
      "`x` must be a single non-missing numeric value.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.numeric(range) || length(range) != 2L || anyNA(range)) {
    rtemis.core::abort(
      "`range` must be a numeric vector of length 2 with no missing values.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (range[1] >= range[2]) {
    rtemis.core::abort(
      "`range[1]` must be strictly less than `range[2]`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!is.character(colors) || length(colors) < 2L || anyNA(colors)) {
    rtemis.core::abort(
      "`colors` must be a character vector of at least 2 non-missing colors.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  # Check x is within range
  if (x < range[1] || x > range[2]) {
    rtemis.core::abort(
      "Value ",
      x,
      " is out of range [",
      range[1],
      ", ",
      range[2],
      "].",
      class = c("rtemis_range_error", "rtemis_input_error")
    )
  }

  n_colors <- 256L
  gradient <- tryCatch(
    {
      grDevices::colorRampPalette(colors, space = space)(n_colors)
    },
    error = function(e) {
      rtemis.core::abort(
        "Invalid `colors` specification.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  )

  p <- (x - range[1]) / (range[2] - range[1])
  idx <- as.integer(round(p * (n_colors - 1L))) + 1L
  idx <- max(1L, min(n_colors, idx))

  fmt(
    as.character(x),
    col = gradient[idx],
    bold = bold,
    output_type = output_type
  )
} # /rtemis::map_value_to_color
