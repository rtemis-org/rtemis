# utils_plotly
# ::rtemis::
# 2021- EDG rtemis.org

# plotly_vline calls plotly_vline1 to create a list for one or more vertical
# lines, to be passed to plotly::layout's shapes argument
plotly_vline1 <- function(x, color = "#F48024", width = 1, dash = "dot") {
  list(
    type = "line",
    x0 = x,
    x1 = x,
    y0 = 0,
    y1 = 1,
    yref = "paper",
    line = list(
      color = color,
      width = width,
      dash = dash
    )
  )
}

# Calls plotly_vline1 for each x value
plotly_vline <- function(x, color = "#F48024", width = 1, dash = "dot") {
  color <- recycle(color, x)
  width <- recycle(width, x)
  dash <- recycle(dash, x)
  mapply(
    plotly_vline1,
    x,
    color = color,
    width = width,
    dash = dash,
    SIMPLIFY = FALSE
  )
}


plotly_hline1 <- function(y, color = "#F48024", width = 1, dash = "dot") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    y0 = y,
    y1 = y,
    xref = "paper",
    line = list(
      color = color,
      width = width,
      dash = dash
    )
  )
}


plotly_hline <- function(y, color = "#F48024", width = 1, dash = "dot") {
  color <- recycle(color, y)
  width <- recycle(width, y)
  dash <- recycle(dash, y)
  mapply(
    plotly_hline1,
    y,
    color = color,
    width = width,
    dash = dash,
    SIMPLIFY = FALSE
  )
}


# bracket y values for boxplot htest
bracket_y <- function(x, pad = c(.04, .05)) {
  l <- max(x) + (diff(range(x)) * pad)
  c(l, rev(l))
}


starbracket_y <- function(x, pad = c(.04, .05, .09)) {
  l <- max(x) + (diff(range(x)) * pad)
  list(star = l[3], bracket = c(l[1:2], rev(l[1:2])))
}


# plotly shade
plotly_shade <- function(
  plt,
  x,
  ypos,
  yneg,
  col,
  alpha = 1,
  legendgroup = NA,
  showlegend = FALSE
) {
  plt <- plotly::add_trace(
    plt,
    x = x,
    y = ypos,
    # type = scatter.type,
    mode = "lines",
    line = list(color = "transparent"),
    legendgroup = legendgroup,
    showlegend = showlegend,
    hoverinfo = "none",
    inherit = FALSE
  )
  plt <- plotly::add_trace(
    plt,
    x = x,
    y = yneg,
    # type = scatter.type,
    mode = "lines",
    fill = "tonexty",
    fillcolor = plotly::toRGB(col, alpha = alpha),
    line = list(color = "transparent"),
    legendgroup = legendgroup,
    showlegend = showlegend,
    hoverinfo = "none",
    inherit = FALSE
  )
}


#' Export plotly plot to file
#'
#' @param x plotly object.
#' @param filename Character: Filename to save the plot to.
#' @param width Numeric: Width of the exported image in pixels.
#' @param height Numeric: Height of the exported image in pixels.
#' @param scale Numeric: Scale factor for the exported image.
#' @param import_kaleido Logical: If TRUE, attempts to import kaleido for exporting plotly plots.
#' @param verbosity Integer: Verbosity level.
#'
#' @return NULL
#'
#' @author EDG
#' @keywords internal
#' @noRd
export_plotly <- function(
  x,
  filename,
  width = 600,
  height = 600,
  scale = 1,
  import_kaleido = TRUE,
  verbosity = 1L
) {
  # Import kaleido
  if (import_kaleido) {
    tryCatch(
      {
        reticulate::py_run_string("import kaleido")
        cat("Kaleido is available for plotly exports.\n")
      },
      error = function(e) {
        cat("Installing kaleido for plotly exports...\n")
        reticulate::py_install("kaleido")
        reticulate::py_run_string("import kaleido")
        cat("Kaleido installed successfully.\n")
      }
    )
  }

  # Intro
  if (verbosity > 0L) {
    msgstart("Exporting plotly plot to ", filename, "...")
  }

  # Export to file ----
  filename <- normalizePath(filename, mustWork = FALSE)
  # Create parent directory if it doesn't exist
  parent_dir <- dirname(filename)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE)
    if (!dir.exists(parent_dir)) {
      cli::cli_abort(
        "Failed to create directory {.file {parent_dir}}. Check path & permissions."
      )
    }
  }
  plotly::save_image(
    x,
    file = filename,
    width = width,
    height = height,
    scale = scale
  )

  # Check if the file was created
  if (!file.exists(filename)) {
    cli::cli_abort(
      "Failed to save plotly plot to {.file {filename}}. Check if the file path is correct and writable."
    )
  } else {
    if (verbosity > 0L) {
      msgdone()
    }
  }
} # /rtemis::export_plotly
