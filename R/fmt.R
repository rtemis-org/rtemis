# fmt.R
# ::rtemis::
# 2025 EDG rtemis.org

# %% fmt ----
#' Text formatting
#'
#' Formats text with specified color, styles, and background using ANSI escape codes or HTML, with support for plain text output.
#'
#' @param x Character: Text to format.
#' @param col Character: Color (hex code, named color, or NULL for no color).
#' @param bold Logical: If TRUE, make text bold.
#' @param italic Logical: If TRUE, make text italic.
#' @param underline Logical: If TRUE, underline text.
#' @param thin Logical: If TRUE, make text thin/light.
#' @param muted Logical: If TRUE, make text muted/dimmed.
#' @param bg Character: Background color (hex code, named color, or NULL).
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with specified styling.
#'
#' @details
#' This function combines multiple formatting options into a single call,
#' making it more efficient than nested function calls. It generates
#' optimized ANSI escape sequences and clean HTML output.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' # Simple color
#' fmt("Hello", col = "red")
#'
#' # Bold red text
#' fmt("Error", col = "red", bold = TRUE)
#'
#' # Multiple styles
#' fmt("Warning", col = "yellow", bold = TRUE, italic = TRUE)
#'
#' # With background
#' fmt("Highlight", col = "white", bg = "blue", bold = TRUE)
fmt <- function(
  x,
  col = NULL,
  bold = FALSE,
  italic = FALSE,
  underline = FALSE,
  thin = FALSE,
  muted = FALSE,
  bg = NULL,
  pad = 0L,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  out <- switch(
    output_type,
    "ansi" = {
      codes <- character()

      # Style codes
      if (bold) {
        codes <- c(codes, "1")
      } else {
        # Explicitly set normal weight to override message() bold default
        codes <- c(codes, "22")
      }
      if (thin || muted) {
        codes <- c(codes, "2")
      } # Both use dim/faint
      if (italic) {
        codes <- c(codes, "3")
      }
      if (underline) {
        codes <- c(codes, "4")
      }

      # Foreground color
      if (!is.null(col)) {
        tryCatch(
          {
            col_rgb <- col2rgb(col)
            codes <- c(
              codes,
              paste0("38;2;", col_rgb[1], ";", col_rgb[2], ";", col_rgb[3])
            )
          },
          error = function(e) {
            warning("Invalid color '", col, "', ignoring color")
          }
        )
      }

      # Background color
      if (!is.null(bg)) {
        tryCatch(
          {
            bg_rgb <- col2rgb(bg)
            codes <- c(
              codes,
              paste0("48;2;", bg_rgb[1], ";", bg_rgb[2], ";", bg_rgb[3])
            )
          },
          error = function(e) {
            warning("Invalid background color '", bg, "', ignoring background")
          }
        )
      }

      # Generate ANSI sequence
      if (length(codes) > 0) {
        paste0("\033[", paste(codes, collapse = ";"), "m", x, "\033[0m")
      } else {
        x
      }
    },
    "html" = {
      styles <- character()

      # Colors
      if (!is.null(col)) {
        styles <- c(styles, paste0("color: ", col))
      }
      if (!is.null(bg)) {
        styles <- c(styles, paste0("background-color: ", bg))
      }

      # Styles
      if (bold) {
        styles <- c(styles, "font-weight: bold")
      }
      if (thin) {
        styles <- c(styles, "font-weight: lighter")
      }
      if (muted) {
        styles <- c(styles, "color: gray")
      } # Override color for muted
      if (italic) {
        styles <- c(styles, "font-style: italic")
      }
      if (underline) {
        styles <- c(styles, "text-decoration: underline")
      }

      # Generate HTML span
      if (length(styles) > 0) {
        paste0(
          '<span style="',
          paste(styles, collapse = "; "),
          '">',
          x,
          "</span>"
        )
      } else {
        x
      }
    },
    "plain" = x
  ) # /switch
  if (pad > 0L) {
    out <- paste0(strrep(" ", pad), out)
  }
  out
} # /rtemis::fmt


# %% highlight ----
#' Highlight text
#'
#' A `fmt()` convenience wrapper for highlighting text.
#'
#' @param x Character: Text to highlight.
#' @param pad Integer: Number of spaces to pad before text.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with highlight.
#'
#' @author EDG
#' @keywords internal
#' @noRd
highlight <- function(
  x,
  pad = 0L,
  output_type = c("ansi", "html", "plain")
) {
  fmt(x, col = highlight_col, bold = TRUE, pad = pad, output_type = output_type)
} # /rtemis::highlight


# %% highlight2 ----
highlight2 <- function(
  x,
  output_type = c("ansi", "html", "plain")
) {
  fmt(x, col = highlight2_col, bold = FALSE, output_type = output_type)
} # /rtemis::highlight2


# %% bold ----
#' Make text bold
#'
#' A `fmt()` convenience wrapper for making text bold.
#'
#' @param text Character: Text to make bold
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with bold styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
bold <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, bold = TRUE, output_type = output_type)
} # /rtemis::bold


# %% italic ----
#' Make text italic
#'
#' A `fmt()` convenience wrapper for making text italic.
#'
#' @param text Character: Text to make italic
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with italic styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
italic <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, italic = TRUE, output_type = output_type)
} # /rtemis::italic


# %% underline ----
#' Make text underlined
#'
#' A `fmt()` convenience wrapper for making text underlined.
#'
#' @param text Character: Text to underline
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with underline styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
underline <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, underline = TRUE, output_type = output_type)
} # /rtemis::underline


# %% thin ----
#' Make text thin/light
#'
#' A `fmt()` convenience wrapper for making text thin/light.
#'
#' @param text Character: Text to make thin
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with thin/light styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
thin <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, thin = TRUE, output_type = output_type)
} # /rtemis::thin


# %% muted ----
#' Muted text
#'
#' A `fmt()` convenience wrapper for making text muted.
#'
#' @param x Character: Text to format
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with muted styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
muted <- function(x, output_type = c("ansi", "html", "plain")) {
  fmt(x, muted = TRUE, output_type = output_type)
} # /rtemis::muted


# %% gray ----
#' Gray text
#'
#' A `fmt()` convenience wrapper for making text gray.
#'
#' @param x Character: Text to format
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with gray styling
#'
#' @details
#' Can be useful in contexts where muted is not supported.
#'
#' @author EDG
#' @keywords internal
#' @noRd
gray <- function(x, output_type = c("ansi", "html", "plain")) {
  fmt(x, col = "#808080", output_type = output_type)
} # /rtemis::gray


# %% col256 ----
#' Apply 256-color formatting
#'
#' @param text Character: Text to color
#' @param col Character or numeric: Color (ANSI 256-color code, hex for HTML)
#' @param bg Logical: If TRUE, apply as background color
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted text with 256-color styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
col256 <- function(
  text,
  col = "79",
  bg = FALSE,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  switch(
    output_type,
    "ansi" = {
      if (bg) {
        paste0("\033[48;5;", col, "m", text, "\033[0m")
      } else {
        paste0("\033[38;5;", col, "m", text, "\033[0m")
      }
    },
    "html" = {
      # Convert ANSI color codes to hex colors if needed
      hex_col <- if (
        is.numeric(col) || (is.character(col) && !grepl("^#", col))
      ) {
        ansi256_to_hex(col)
      } else {
        col
      }
      if (bg) {
        paste0(
          '<span style="background-color: ',
          hex_col,
          '">',
          text,
          "</span>"
        )
      } else {
        paste0('<span style="color: ', hex_col, '">', text, "</span>")
      }
    },
    "plain" = text
  )
} # /rtemis::col256


# %% ansi256_to_hex ----
#' Convert ANSI 256 color code to HEX
#'
#' @param code Integer: ANSI 256 color code (0-255).
#'
#' @return Character: HEX color string.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ansi256_to_hex <- function(code) {
  code <- as.integer(code)
  if (is.na(code) || code < 0 || code > 255) {
    return("#000000") # Return black for invalid codes
  }

  # Standard and high-intensity colors (0-15)
  if (code < 16) {
    return(c(
      "#000000",
      "#cd0000",
      "#00cd00",
      "#cdcd00",
      "#0000ee",
      "#cd00cd",
      "#00cdcd",
      "#e5e5e5",
      "#7f7f7f",
      "#ff0000",
      "#00ff00",
      "#ffff00",
      "#5c5cff",
      "#ff00ff",
      "#00ffff",
      "#ffffff"
    )[code + 1])
  }

  # 6x6x6 color cube (16-231)
  if (code >= 16 && code <= 231) {
    code <- code - 16
    r <- floor(code / 36)
    g <- floor((code %% 36) / 6)
    b <- code %% 6
    levels <- c(0, 95, 135, 175, 215, 255) # xterm levels
    return(grDevices::rgb(
      levels[r + 1],
      levels[g + 1],
      levels[b + 1],
      maxColorValue = 255
    ))
  }

  # Grayscale ramp (232-255)
  gray_level <- (code - 232) * 10 + 8
  grDevices::rgb(
    gray_level,
    gray_level,
    gray_level,
    maxColorValue = 255
  )
} # /rtemis::ansi256_to_hex


# %% fmt_gradient ----
#' Gradient text
#'
#' @param x Character: Text to colorize.
#' @param colors Character vector: Colors to use for the gradient.
#' @param bold Logical: If TRUE, make text bold.
#' @param space Character {"rgb", "Lab"}: Color space for gradient interpolation.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Text with gradient color applied.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fmt_gradient <- function(
  x,
  colors,
  bold = FALSE,
  space = "Lab",
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  if (output_type == "plain") {
    return(x)
  }

  # Split text into individual characters
  chars <- strsplit(x, "")[[1]]
  n_chars <- length(chars)

  if (n_chars <= 1) {
    # For single character or empty string, use first color
    return(fmt(x, col = colors[1], output_type = output_type))
  }

  # Generate gradient colors using colorRampPalette
  tryCatch(
    {
      gradient_colors <- grDevices::colorRampPalette(colors, space = space)(
        n_chars
      )
    },
    error = function(e) {
      warning("Invalid gradient colors, using default")
      x
    }
  )

  # Apply gradient colors to each character
  gradient_chars <- character(n_chars)
  for (i in seq_len(n_chars)) {
    gradient_chars[i] <- fmt(
      chars[i],
      col = gradient_colors[i],
      bold = bold,
      output_type = output_type
    )
  }

  # Combine all colored characters
  paste(gradient_chars, collapse = "")
} # /rtemis::fmt_gradient


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
    cli::cli_abort("`x` must be a single non-missing numeric value.")
  }
  if (!is.numeric(range) || length(range) != 2L || anyNA(range)) {
    cli::cli_abort(
      "`range` must be a numeric vector of length 2 with no missing values."
    )
  }
  if (range[1] >= range[2]) {
    cli::cli_abort("`range[1]` must be strictly less than `range[2]`.")
  }
  if (!is.character(colors) || length(colors) < 2L || anyNA(colors)) {
    cli::cli_abort(
      "`colors` must be a character vector of at least 2 non-missing colors."
    )
  }
  # Check x is within range
  if (x < range[1] || x > range[2]) {
    cli::cli_abort(
      "Value {x} is out of range [{range[1]}, {range[2]}]"
    )
  }

  n_colors <- 256L
  gradient <- tryCatch(
    {
      grDevices::colorRampPalette(colors, space = space)(n_colors)
    },
    error = function(e) {
      cli::cli_abort("Invalid `colors` specification.")
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
