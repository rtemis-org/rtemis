# utils_art.R
# ::rtemis::
# 2025- EDG rtemis.org

#' Color columns of text art
#'
#' This function accepts text input of 1 or more lines and two colors.
#' It will:
#' a) generate a color gradient between the two colors
#' b) apply the gradient to each column of the text, creating a left to right color gradient.
#'
#' @param x Character vector of text to colorize.
#' @param color_left Color for the left side of the gradient.
#' @param color_right Color for the right side of the gradient.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character vector with color formatting applied to each column.
#'
#' @author EDG
#' @keywords internal
#' @noRd
color_txt_columns <- function(
  x,
  color_left,
  color_right,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)
  # Count number of columns in input text
  ncols <- max(nchar(x, type = "width"))

  if (ncols == 0) {
    return(x)
  }

  # Create color gradient from color_left to color_right with ncols steps
  gradient <- grDevices::colorRampPalette(c(color_left, color_right))(ncols)

  # Apply the colors to each column of the text
  result <- character(length(x))

  for (i in seq_along(x)) {
    line <- x[i]
    line_chars <- strsplit(line, "")[[1]]
    line_width <- nchar(line, type = "width")

    if (line_width == 0) {
      result[i] <- line
      next
    }

    colored_chars <- character(length(line_chars))

    for (j in seq_along(line_chars)) {
      char <- line_chars[j]
      if (char == " ") {
        colored_chars[j] <- char
      } else {
        # Use column position for gradient color
        col_pos <- min(j, ncols)
        colored_chars[j] <- fmt(
          char,
          col = gradient[col_pos],
          output_type = output_type
        )
      }
    }

    result[i] <- paste0(colored_chars, collapse = "")
  }

  result
} # /rtemis::color_txt_columns


#' Color rows of text art
#'
#' This function accepts text input of 1 or more lines and two colors.
#' It will:
#' a) generate a color gradient between the two colors
#' b) apply the gradient to each row of the text, creating a top to bottom color gradient.
#'
#' @param x Character vector of text to colorize.
#' @param color_top Color for the top of the gradient.
#' @param color_bottom Color for the bottom of the gradient.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character vector with color formatting applied to each row.
#'
#' @author EDG
#' @keywords internal
#' @noRd
color_txt_rows <- function(
  x,
  color_top,
  color_bottom,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  # Number of rows
  nrows <- length(x)

  if (nrows == 0) {
    return(x)
  }

  # Create color gradient from color_top to color_bottom with nrows steps
  gradient <- grDevices::colorRampPalette(c(color_top, color_bottom))(nrows)

  # Apply the colors to each row of the text
  result <- character(nrows)

  for (i in seq_along(x)) {
    line <- x[i]
    line_chars <- strsplit(line, "")[[1]]
    line_width <- nchar(line, type = "width")

    if (line_width == 0) {
      result[i] <- line
      next
    }

    colored_chars <- character(length(line_chars))

    for (j in seq_along(line_chars)) {
      char <- line_chars[j]
      if (char == " ") {
        colored_chars[j] <- char
      } else {
        colored_chars[j] <- fmt(
          char,
          col = gradient[i],
          output_type = output_type
        )
      }
    }

    result[i] <- paste0(colored_chars, collapse = "")
  }

  result
} # /rtemis::color_txt_rows


#' pkglogo
#'
#' @param pkg Character: Package name.
#' @param filename Character: Filename of the logo file (without path).
#' @param fmt_fn Function: "color_txt_columns", "color_text_rows" Formatting function to apply to
#' the logo text.
#' @param args List: Arguments to pass to `fmt_fn`.
#' @param pad Integer: Left-pad output with this many spaces.
#'
#' @return Character: Formatted logo text.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pkglogo <- function(
  pkg = .packageName,
  filename = paste0(pkg, ".utf8"),
  fmt_fn = color_txt_columns,
  args = list(
    color_left = kaimana_red,
    color_right = coastside_orange,
    output_type = "ansi"
  ),
  pad = 2L
) {
  logo_file <- system.file(
    package = .packageName,
    "resources",
    filename
  )
  logo_txt <- readLines(logo_file)
  paste0(
    strrep(" ", pad),
    do.call(fmt_fn, c(list(x = logo_txt), args)),
    collapse = "\n"
  )
} # /rtemis::pkglogo


#' Show colors
#'
#' Display color previews with ANSI color blocks
#'
#' @param x Named vector or list of colors to preview.
#' @param pad Integer: Pad output with this many spaces.
#' @param center_title Logical: If TRUE, autopad title for centering, if present.
#' @param title Character: Optional title to display.
#' @param title_newline Logical: If TRUE, add newline after title.
#' @param limit Integer: Maximum number of colors to show. Set to -1L for no limit.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted string that can be printed with cat()
#'
#' @author EDG
#' @keywords internal
#' @noRd
show_col <- function(
  x,
  pad = 2L,
  center_title = TRUE,
  title = NULL,
  title_newline = TRUE,
  limit = 12L,
  output_type = c("ansi", "html", "plain")
) {
  output_type <- match.arg(output_type)

  # Helper function to build padded string equivalent of padcat
  build_padcat <- function(
    text,
    pad = 2L,
    newline_pre = FALSE,
    newline = FALSE
  ) {
    result <- ""
    if (newline_pre) {
      result <- paste0(result, "\n")
    }
    result <- paste0(result, strrep(" ", pad))
    result <- paste0(result, text)
    if (newline) {
      result <- paste0(result, "\n")
    }
    result
  }

  # Initialize output string
  result <- ""

  if (is.null(x)) {
    if (!is.null(title)) {
      result <- paste0(
        result,
        highlight(build_padcat(title, pad = pad, newline = title_newline))
      )
    }
    result <- paste0(result, strrep(" ", pad), "NULL")
    return(result)
  }

  if (length(x) == 0) {
    result <- paste0(result, class(x), " of length 0.\n")
    return(result)
  }

  # Convert to named list if needed
  if (is.null(names(x)) && !is.list(x)) {
    names(x) <- as.character(x)
  }

  x <- as.list(x)
  xnames <- names(x)
  if (is.null(xnames)) {
    xnames <- paste0("color_", seq_along(x))
  }

  # Calculate left-hand side width
  lhs <- max(nchar(xnames)) + pad

  # Add title if provided
  if (!is.null(title)) {
    title_pad <- if (center_title) {
      max(0, lhs - round((.5 * nchar(title))) - 3)
    } else {
      0
    }
    result <- paste0(
      result,
      highlight(build_padcat(title, pad = title_pad, newline = title_newline)),
      "\n"
    )
  }

  # Show limit message if needed
  counter <- 0L
  if (limit != -1L && length(x) > limit) {
    limit_text <- paste0(
      italic(
        gray(
          paste0(
            "Showing first ",
            limit,
            " of ",
            length(x),
            " colors.\n"
          ),
          output_type = output_type
        ),
        output_type = output_type
      )
    )
    result <- paste0(result, build_padcat(limit_text, pad = pad))
  }

  # Display each color
  for (i in seq_along(x)) {
    counter <- counter + 1L
    if (limit != -1L && counter > limit) {
      more_text <- paste0(
        italic(
          gray(
            paste0(
              "...",
              length(x) - limit,
              " more colors not shown.\n"
            )
          ),
          output_type = output_type
        )
      )
      result <- paste0(result, build_padcat(more_text, pad = pad))
      break
    }

    # Get color value
    color_val <- x[[i]]

    # Create color blocks: 2 solid, 2 medium, 2 light
    if (output_type == "ansi") {
      # Use the color directly
      color_display <- tryCatch(
        {
          # Create blocks with varying intensities
          solid_block <- fmt(
            "\u2588",
            col = color_val,
            output_type = output_type
          )
          medium_block <- fmt(
            "\u2593",
            col = color_val,
            output_type = output_type
          )
          light_block <- fmt(
            "\u2591",
            col = color_val,
            output_type = output_type
          )

          paste0(
            solid_block,
            solid_block,
            medium_block,
            medium_block,
            light_block,
            light_block
          )
        },
        error = function(e) {
          # Fallback if color conversion fails
          paste0(
            "\u2588\u2588\u2593\u2593\u2591\u2591 (",
            color_val,
            ")"
          )
        }
      )
    } else {
      # For non-ANSI output, just show the color value
      color_display <- paste0(
        "\u2588\u2588\u2593\u2593\u2591\u2591 (",
        color_val,
        ")"
      )
    }

    # Format and add the line
    item_text <- paste0(
      bold(
        format(
          xnames[i],
          width = lhs,
          justify = "right"
        ),
        output_type = output_type
      ),
      ": ",
      color_display,
      "\n"
    )
    result <- paste0(result, item_text)
  }

  result
} # /rtemis::show_col
