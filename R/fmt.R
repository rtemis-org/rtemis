# fmt.R
# ::rtemis::
# 2025 EDG rtemis.org

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
#' @param output_type Character: Output type ("ansi", "html", "plain").
#'
#' @return Character: Formatted text with specified styling.
#'
#' @details
#' This function combines multiple formatting options into a single call,
#' making it more efficient than nested function calls. It generates
#' optimized ANSI escape sequences and clean HTML output.
#'
#' @examples
#' \dontrun{
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
#' }
#'
#' @author EDG
#' @keywords internal
#' @noRd
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


#' Highlight text
#'
#' A `fmt()` convenience wrapper for highlighting text.
#'
#' @param x Character: Text to highlight.
#' @param pad Integer: Number of spaces to pad before text.
#' @param output_type Character: Output type ("ansi", "html", "plain").
#'
#' @return Character: Formatted text with highlight.
#'
#' @author EDG
#' @export
#' @keywords internal
highlight <- function(
  x,
  pad = 0L,
  output_type = c("ansi", "html", "plain")
) {
  fmt(x, col = highlight_col, bold = TRUE, pad = pad, output_type = output_type)
} # /rtemis::highlight

highlight2 <- function(
  x,
  output_type = c("ansi", "html", "plain")
) {
  fmt(x, col = highlight2_col, bold = FALSE, output_type = output_type)
} # /rtemis::highlight2


#' Make text bold
#'
#' A `fmt()` convenience wrapper for making text bold.
#'
#' @param text Character: Text to make bold
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with bold styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
bold <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, bold = TRUE, output_type = output_type)
} # /rtemis::bold

#' Make text italic
#'
#' A `fmt()` convenience wrapper for making text italic.
#'
#' @param text Character: Text to make italic
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with italic styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
italic <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, italic = TRUE, output_type = output_type)
} # /rtemis::italic


#' Make text underlined
#'
#' A `fmt()` convenience wrapper for making text underlined.
#'
#' @param text Character: Text to underline
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with underline styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
underline <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, underline = TRUE, output_type = output_type)
} # /rtemis::underline


#' Make text thin/light
#'
#' A `fmt()` convenience wrapper for making text thin/light.
#'
#' @param text Character: Text to make thin
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with thin/light styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
thin <- function(text, output_type = c("ansi", "html", "plain")) {
  fmt(text, thin = TRUE, output_type = output_type)
} # /rtemis::thin


#' Muted text
#'
#' A `fmt()` convenience wrapper for making text muted.
#'
#' @param x Character: Text to format
#' @param output_type Character: Output type ("ansi", "html", "plain")
#'
#' @return Character: Formatted text with muted styling
#'
#' @author EDG
#' @keywords internal
#' @noRd
muted <- function(x, output_type = c("ansi", "html", "plain")) {
  fmt(x, muted = TRUE, output_type = output_type)
} # /rtemis::muted


#' Gray text
#'
#' A `fmt()` convenience wrapper for making text gray.
#'
#' @param x Character: Text to format
#' @param output_type Character: Output type ("ansi", "html", "plain")
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


#' Apply 256-color formatting
#'
#' @param text Character: Text to color
#' @param col Character or numeric: Color (ANSI 256-color code, hex for HTML)
#' @param bg Logical: If TRUE, apply as background color
#' @param output_type Character: Output type ("ansi", "html", "plain")
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


#' Gradient text
#'
#' @param x Character: Text to colorize.
#' @param colors Character vector: Colors to use for the gradient.
#' @param output_type Character: Output type ("ansi", "html", "plain").
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
      gradient_colors <- grDevices::colorRampPalette(colors)(
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
