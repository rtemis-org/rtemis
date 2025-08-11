# draw_scatter.R
# ::rtemis::
# 2019- EDG rtemis.org

#' Interactive Scatter Plots
#'
#' Draw interactive scatter plots using `plotly`.
#'
#' @param x Numeric, vector/data.frame/list: x-axis data. If y is NULL and `NCOL(x) > 1`, first two columns used as `x` and `y`, respectively.
#' @param y Numeric, vector/data.frame/list: y-axis data.
#' @param fit Character: Fit method.
#' @param se_fit Logical: If TRUE, include standard error of the fit.
#' @param se_times Numeric: Multiplier for standard error.
#' @param include_fit_name Logical: If TRUE, include fit name in legend.
#' @param cluster Character: Clustering method.
#' @param cluster_params List: Parameters for clustering.
#' @param group Factor: Grouping variable.
# @param formula Formula: Formula for non-linear least squares fit.
#' @param rsq Logical: If TRUE, print R-squared values in legend if `fit` is set.
#' @param mode Character, vector: "markers", "lines", "markers+lines".
#' @param order_on_x Logical: If TRUE, order `x` and `y` on `x`.
#' @param main Character: Main title.
#' @param subtitle Character: Subtitle.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param col Color for markers.
#' @param alpha Numeric: Alpha for markers.
#' @param theme Theme object.
#' @param palette Character: Color palette.
#' @param axes_square Logical: If TRUE, draw a square plot.
#' @param group_names Character: Names for groups.
#' @param font_size Numeric: Font size.
#' @param marker_col Color for markers.
#' @param marker_size Numeric: Marker size.
#' @param symbol Character: Marker symbol.
#' @param fit_col Color for fit line.
#' @param fit_alpha Numeric: Alpha for fit line.
#' @param fit_lwd Numeric: Line width for fit line.
#' @param line_shape Character: Line shape for line plots. Options: "linear", "hv", "vh", "hvh", "vhv".
#' @param se_col Color for standard error band.
#' @param se_alpha Numeric: Alpha for standard error band.
#' @param scatter_type Character: Scatter plot type.
#' @param show_marginal_x Logical: If TRUE, add marginal distribution line markers on x-axis.
#' @param show_marginal_y Logical: If TRUE, add marginal distribution line markers on y-axis.
#' @param marginal_x Numeric: Data for marginal distribution on x-axis.
#' @param marginal_y Numeric: Data for marginal distribution on y-axis.
#' @param marginal_x_y Numeric: Y position of marginal markers on x-axis.
#' @param marginal_y_x Numeric: X position of marginal markers on y-axis.
#' @param marginal_col Color for marginal markers.
#' @param marginal_alpha Numeric: Alpha for marginal markers.
#' @param marginal_size Numeric: Size of marginal markers.
#' @param legend Logical: If TRUE, draw legend.
#' @param legend_title Character: Title for legend.
#' @param legend_trace Logical: If TRUE, draw legend trace. (For when you have `fit` and don't want a trace for the markers.)
#' @param legend_xy Numeric: Position of legend.
#' @param legend_xanchor Character: X anchor for legend.
#' @param legend_yanchor Character: Y anchor for legend.
#' @param legend_orientation Character: Orientation of legend.
#' @param legend_col Color for legend text.
#' @param legend_bg Color for legend background.
#' @param legend_border_col Color for legend border.
#' @param legend_borderwidth Numeric: Border width for legend.
#' @param legend_group_gap Numeric: Gap between legend groups.
#' @param x_showspikes Logical: If TRUE, show spikes on x-axis.
#' @param y_showspikes Logical: If TRUE, show spikes on y-axis.
#' @param spikedash Character: Dash type for spikes.
#' @param spikemode Character: Spike mode.
#' @param spikesnap Character: Spike snap mode.
#' @param spikecolor Color for spikes.
#' @param spikethickness Numeric: Thickness of spikes.
#' @param margin List: Plot margins.
#' @param main_y Numeric: Y position of main title.
#' @param main_yanchor Character: Y anchor for main title.
#' @param subtitle_x Numeric: X position of subtitle.
#' @param subtitle_y Numeric: Y position of subtitle.
#' @param subtitle_xref Character: X reference for subtitle.
#' @param subtitle_yref Character: Y reference for subtitle.
#' @param subtitle_xanchor Character: X anchor for subtitle.
#' @param subtitle_yanchor Character: Y anchor for subtitle.
#' @param automargin_x Logical: If TRUE, automatically adjust x-axis margins.
#' @param automargin_y Logical: If TRUE, automatically adjust y-axis margins.
#' @param xlim Numeric: Limits for x-axis.
#' @param ylim Numeric: Limits for y-axis.
#' @param axes_equal Logical: If TRUE, set equal scaling for axes.
#' @param diagonal Logical: If TRUE, add diagonal line.
#' @param diagonal_col Color for diagonal line.
#' @param diagonal_dash Character: "solid", "dash", "dot", "dashdot", "longdash", "longdashdot". Dash type for diagonal line.
#' @param diagonal_alpha Numeric: Alpha for diagonal line.
#' @param fit_params Hyperparameters: Parameters for fit.
#' @param vline Numeric: X position for vertical line.
#' @param vline_col Color for vertical line.
#' @param vline_width Numeric: Width for vertical line.
#' @param vline_dash Character: Dash type for vertical line.
#' @param hline Numeric: Y position for horizontal line.
#' @param hline_col Color for horizontal line.
#' @param hline_width Numeric: Width for horizontal line.
#' @param hline_dash Character: Dash type for horizontal line.
#' @param hovertext List: Hover text for markers.
#' @param width Numeric: Width of plot.
#' @param height Numeric: Height of plot.
#' @param displayModeBar Logical: If TRUE, display mode bar.
#' @param modeBar_file_format Character: File format for mode bar.
#' @param scrollZoom Logical: If TRUE, enable scroll zoom.
#' @param filename Character: Filename to save plot.
#' @param file_width Numeric: Width of saved file.
#' @param file_height Numeric: Height of saved file.
#' @param file_scale Numeric: Scale of saved file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' draw_scatter(iris$Sepal.Length, iris$Petal.Length,
#'   fit = "gam", se_fit = TRUE, group = iris$Species
#' )
#' }
draw_scatter <- function(
  x,
  y = NULL,
  fit = NULL,
  se_fit = FALSE,
  se_times = 1.96,
  include_fit_name = TRUE,
  cluster = NULL,
  cluster_params = list(k = 2),
  group = NULL,
  # formula = NULL,
  rsq = TRUE,
  mode = "markers",
  order_on_x = NULL,
  main = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  col = NULL,
  alpha = NULL,
  theme = choose_theme(),
  palette = rtemis_palette,
  axes_square = FALSE,
  group_names = NULL,
  font_size = 16,
  marker_col = NULL,
  marker_size = 8,
  symbol = "circle",
  fit_col = NULL,
  fit_alpha = .8,
  fit_lwd = 2.5,
  line_shape = "linear",
  se_col = NULL,
  se_alpha = .4,
  scatter_type = "scatter",
  show_marginal_x = FALSE,
  show_marginal_y = FALSE,
  marginal_x = x,
  marginal_y = y,
  marginal_x_y = NULL,
  marginal_y_x = NULL,
  marginal_col = NULL,
  marginal_alpha = .333,
  marginal_size = 10,
  legend = NULL,
  legend_title = NULL,
  legend_trace = TRUE,
  legend_xy = c(0, .98),
  legend_xanchor = "left",
  legend_yanchor = "auto",
  legend_orientation = "v",
  legend_col = NULL,
  legend_bg = "#FFFFFF00",
  legend_border_col = "#FFFFFF00",
  legend_borderwidth = 0,
  legend_group_gap = 0,
  x_showspikes = FALSE,
  y_showspikes = FALSE,
  spikedash = "solid",
  spikemode = "across",
  spikesnap = "hovered data",
  spikecolor = NULL,
  spikethickness = 1,
  margin = list(b = 65, l = 65, t = 50, r = 10, pad = 0),
  main_y = 1,
  main_yanchor = "bottom",
  subtitle_x = 0.02,
  subtitle_y = 0.99,
  subtitle_xref = "paper",
  subtitle_yref = "paper",
  subtitle_xanchor = "left",
  subtitle_yanchor = "top",
  automargin_x = TRUE,
  automargin_y = TRUE,
  xlim = NULL,
  ylim = NULL,
  axes_equal = FALSE,
  diagonal = FALSE,
  diagonal_col = NULL,
  diagonal_dash = "dot",
  diagonal_alpha = .66,
  fit_params = NULL,
  vline = NULL,
  vline_col = theme[["fg"]],
  vline_width = 1,
  vline_dash = "dot",
  hline = NULL,
  hline_col = theme[["fg"]],
  hline_width = 1,
  hline_dash = "dot",
  hovertext = NULL,
  width = NULL,
  height = NULL,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  scrollZoom = TRUE,
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  verbosity = 0L
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Arguments ----
  xname <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
  yname <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
  if (is.null(y) && NCOL(x) > 1) {
    if (is.null(xlab)) {
      xlab <- labelify(colnames(x)[1])
    }
    if (is.null(ylab)) {
      ylab <- labelify(colnames(x)[2])
    }
    y <- x[, 2]
    x <- x[, 1]
  }
  if (!is.null(fit)) {
    if (fit == "none") fit <- NULL
  } # easier to work with shiny
  if (is.logical(fit)) {
    if (fit) fit <- "GAM"
  }
  if (is.null(fit)) {
    se_fit <- FALSE
  }
  if (!is.null(fit)) {
    fit <- toupper(fit)
  }
  if (!is.null(main)) {
    main <- paste0("<b>", main, "</b>")
  }
  .mode <- mode
  .names <- group_names

  # fit & formula
  # if (!is.null(formula)) fit <- "NLS"

  if (se_fit) {
    if (!fit %in% c("GLM", "LM", "LOESS", "GAM", "NW")) {
      warning(paste(
        "Standard error of the fit not available for",
        fit,
        "- try LM, LOESS, GAM, or NW"
      ))
      se_fit <- FALSE
    }
  }

  # order_on_x ----
  if (is.null(order_on_x)) {
    order_on_x <- if (!is.null(fit) || any(grepl("lines", mode))) {
      TRUE
    } else {
      FALSE
    }
  }

  # Cluster ----
  if (!is.null(cluster)) {
    group <- suppressWarnings(
      cluster(
        x = data.frame(x, y),
        algorithm = cluster,
        parameters = do_call(
          get_clust_setup_fn(cluster),
          cluster_params
        )
      )@clusters
    )
    group <- paste("Cluster", group)
  }

  # Data ----
  # xlab, ylab ----
  # The gsubs remove all text up to and including a "$" symbol if present
  if (is.null(xlab)) {
    if (is.list(x)) xlab <- "x" else xlab <- xname
  }
  if (!is.null(y) && is.null(ylab)) {
    if (is.list(y)) ylab <- "y" else ylab <- yname
  }

  # Group ----
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group, drop = TRUE)
    y <- split(y, group, drop = TRUE)
    if (is.null(group_names)) {
      group_names <- levels(droplevels(group))
    }
    names(x) <- names(y) <- .names <- group_names
    if (!is.null(hovertext)) hovertext <- split(hovertext, group, drop = TRUE)
  }

  # Try to get names from list or data frame inputs
  if (is.list(y) || NCOL(y) > 1) {
    if (is.null(.names) && !is.null(names(y))) .names <- names(y)
  }
  if (is.list(x) || NCOL(x) > 1) {
    if (is.null(.names) && !is.null(names(x))) .names <- names(x)
  }

  # Data to lists ----
  x <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  y <- if (!is.null(y) && !is.list(y)) as.list(as.data.frame(y)) else y
  hovertext <- if (!is.null(hovertext) && !is.list(hovertext)) {
    as.list(as.data.frame(hovertext))
  } else {
    hovertext
  }
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    .names <- names(y)
  }
  if (length(y) == 1 && length(x) > 1) {
    y <- rep(y, length(x))
    .names <- names(x)
  }
  if (!is.null(hovertext) && length(hovertext) == 1 && length(x) > 1) {
    hovertext <- rep(hovertext, length(x))
  }
  n_groups <- length(x)

  if (is.null(legend)) {
    legend <- if (n_groups == 1 && is.null(fit)) FALSE else TRUE
  }

  if (length(.mode) < n_groups) {
    .mode <- c(.mode, rep(tail(.mode)[1], n_groups - length(.mode)))
  }

  # if (is.null(legend)) legend <- n_groups > 1
  if (is.null(.names)) {
    if (n_groups > 1) {
      .names <- paste("Group", seq_len(n_groups))
    } else {
      # .names <- if (!is.null(fit)) fit else NULL
      .names <- xname
    }
  }

  # Marginal data ----
  if (show_marginal_x && is.null(marginal_x)) {
    marginal_x <- x
  }
  if (show_marginal_y && is.null(marginal_y)) {
    marginal_y <- y
  }

  # Reorder ----
  if (order_on_x) {
    index <- lapply(x, order)
    x <- lapply(seq(x), \(i) x[[i]][index[[i]]])
    y <- lapply(seq(x), \(i) y[[i]][index[[i]]])
    if (!is.null(hovertext)) {
      hovertext <- lapply(seq(x), \(i) hovertext[[i]][index[[i]]])
    }
  }

  if (!is.null(fit) && fit == "LOESS") {
    id <- !is.na(x)
  }

  # Colors ----
  if (is.character(palette)) {
    palette <- rtpalette(palette)
  }
  if (is.null(col)) {
    col <- palette[seq_len(n_groups)]
  }
  if (length(col) < n_groups) {
    col <- rep(col, n_groups / length(col))
  }
  if (is.null(alpha)) {
    alpha <- if (mode == "markers") {
      autoalpha(max(lengths(x)))
    } else {
      1
    }
  }

  # Theme ----
  check_is_S7(theme, Theme)

  if (diagonal) {
    if (is.null(diagonal_col)) {
      diagonal_col <- theme[["fg"]]
    }
    diagonal_col <- adjustcolor(diagonal_col, diagonal_alpha)
  }

  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]], theme[["grid_alpha"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])
  if (!theme[["axes_visible"]]) {
    tick_col <- labs_col <- "transparent"
  }

  # marker_col, se_col ===
  if (is.null(marker_col)) {
    marker_col <- if (!is.null(fit) && n_groups == 1) {
      as.list(rep(theme[["fg"]], n_groups))
    } else {
      col
    }
  }

  if (!is.null(fit)) {
    if (is.null(fit_col)) fit_col <- col
  }

  if (se_fit && is.null(se_col)) {
    se_col <- col
  }

  if (is.null(legend_col)) {
    legend_col <- labs_col
  }
  if (is.null(spikecolor)) {
    spikecolor <- theme[["fg"]]
  }

  # Size ----
  # if (axes_square) {
  # width <- height <- min(dev.size("px"))
  # }
  # fitted & se_fit ----
  # If plotting se bands, need to include (fitted +/- se_times * se) in the axis limits
  if (se_fit) {
    se <- list()
  } else {
    se <- NULL
  }
  if (rsq) {
    .rsq <- list()
  } else {
    .rsq <- NULL
  }
  # if (rsq_pval) rsqp <- list() else rsqp <- NULL
  if (!is.null(fit)) {
    # algorithm <- get_alg_name(fit)
    fitted <- list()
    fitted_text <- character()
    for (i in seq_len(n_groups)) {
      mod <- train(
        x = data.frame(x = x[[i]], y = y[[i]]),
        algorithm = fit,
        hyperparameters = fit_params,
        verbosity = verbosity - 1L
      )
      fitted[[i]] <- fitted(mod)
      if (se_fit) {
        se[[i]] <- se(mod)
      }
      if (include_fit_name) {
        # fitted_text[i] <- switch(fit,
        #   NLS = mod$extra$model,
        #   NLA = mod$mod$formula,
        #   fit
        # )
        fitted_text[i] <- fit
      } else {
        fitted_text[i] <- ""
      }
      if (rsq) {
        fitted_text[i] <- paste0(
          fitted_text[i],
          if (n_groups == 1) " (" else " ",
          "R<sup>2</sup> = ",
          ddSci(mod@metrics_training[["Rsq"]]),
          if (n_groups == 1) ")"
        )
      }
      # if (rsq_pval) {
      #   if (fit  %in% c("LM", "GLM")) {
      #     rsqp[[i]] <- paste0(ddSci(mod@metrics_training$Rsq), " (",
      #                          ddSci(summary(mod$mod)$coefficients[2, 4]), ")")
      #   } else if (fit == "GAM") {
      #     rsqp[[i]] <- paste0(ddSci(mod@metrics_training$Rsq), " (",
      #                          ddSci(summary(mod$mod)$s.pv), ")")
      #   }
      # }
    }
  }

  # Axes Limits ----
  if (axes_equal) {
    if (is.null(xlim)) {
      xlim <- getlim(unlist(x), "r", .06)
    }
    if (is.null(ylim)) {
      ylim <- getlim(unlist(y), "r", .06)
      if (is.list(fitted) && !is.list(se)) {
        ylim_hi <- max(unlist(fitted))
        ylim_lo <- min(unlist(fitted))
        ylim <- range(ylim_lo, ylim_hi, y)
      }
      if (is.list(se)) {
        ylim_hi <- max(unlist(lapply(
          seq_along(fitted),
          function(i) {
            as.data.frame(fitted[[i]]) +
              se_times * as.data.frame(se[[i]])
          }
        )))
        ylim_lo <- min(unlist(lapply(
          seq_along(fitted),
          function(i) {
            as.data.frame(fitted[[i]]) -
              se_times * as.data.frame(se[[i]])
          }
        )))
        ylim <- range(ylim_lo, ylim_hi, y)
      }
      # if (is.list(error_y)) {
      #   error_y_hi <- lapply(seq(y), function(i) yl[[i]] + error_y[[i]])
      #   error_y_lo <- lapply(seq(y), function(i) yl[[i]] - error_y[[i]])
      #   ylim <- range(error_y_lo, error_y_hi, ylim)
      # }
    }

    xlim <- ylim <- range(xlim, ylim)
  } # /axes_equal

  # unlist will coerce Dates to numeric, also don't want padding
  if (is.null(xlim) && !inherits(x[[1]], "Date")) {
    xlim <- getlim(unlist(x), "r", .06)
  }
  if (is.null(ylim) && !inherits(y[[1]], "Date")) {
    ylim <- getlim(unlist(y), "r", .06)
  }

  # plotly ----
  if (!is.null(fit) && rsq) {
    if (!include_fit_name) {
      fitted_text <- gsub("^ ", "", fitted_text)
    }
    if (n_groups > 1) {
      .names <- paste0(.names, " (", fitted_text, ")")
    }
  }

  plt <- plotly::plot_ly(
    width = width,
    height = height
  )

  if (diagonal) {
    lo <- min(xlim[1], ylim[1])
    hi <- max(xlim[2], ylim[2])
    plt <- plotly::layout(
      plt,
      shapes = list(
        type = "line",
        x0 = lo,
        x1 = hi,
        y0 = lo,
        y1 = hi,
        line = list(
          color = diagonal_col,
          dash = diagonal_dash
        )
      )
    )
  }

  for (i in seq_len(n_groups)) {
    ## { Scatter } ----
    marker <- if (grepl("markers", .mode[i])) {
      list(
        color = plotly::toRGB(marker_col[[i]], alpha = alpha),
        size = marker_size,
        symbol = symbol
      )
    } else {
      NULL
    }
    plt <- plotly::add_trace(
      plt,
      x = x[[i]],
      y = y[[i]],
      type = scatter_type,
      mode = .mode[i],
      # fillcolor = plotly::toRGB(col[[i]], alpha),
      name = .names[i],
      # text = .text[[i]],
      # hoverinfo = "text",
      text = hovertext[[i]],
      marker = marker,
      line = if (grepl("lines", .mode[i])) {
        list(
          color = plotly::toRGB(marker_col[[i]], alpha = alpha),
          shape = line_shape
        )
      } else {
        NULL
      },
      legendgroup = if (legend_trace) {
        .names[i]
      } else {
        paste0(.names[i], "_marker")
      },
      showlegend = legend && legend_trace
    )
    # Marginal plots ----
    # Add marginal plots by plotting short vertical markers on the x and y axes
    if (show_marginal_x) {
      if (is.null(marginal_col)) {
        marginal_col <- plotly::toRGB(marker_col, alpha = marginal_alpha)
      }
      if (is.null(marginal_x_y)) {
        marginal_x_y <- ylim[1]
      }
      # Extend ylim to include marginal markers
      ylim[1] <- ylim[1] - 0.02 * diff(ylim)
      for (i in seq_len(n_groups)) {
        plt <- plotly::add_trace(
          plt,
          x = marginal_x[[i]],
          y = rep(marginal_x_y, length(marginal_x[[i]])),
          type = "scatter",
          mode = "markers",
          marker = list(
            color = marginal_col[[i]],
            size = marginal_size,
            symbol = "line-ns-open"
          ),
          showlegend = FALSE,
          hoverinfo = "x"
        )
      }
    } # /show_marginal_x

    if (show_marginal_y) {
      if (is.null(marginal_col)) {
        marginal_col <- plotly::toRGB(marker_col, alpha = marginal_alpha)
      }
      if (is.null(marginal_y_x)) {
        marginal_y_x <- xlim[1]
      }
      # Extend xlim to include marginal markers
      xlim[1] <- xlim[1] - 0.02 * diff(xlim)
      for (i in seq_len(n_groups)) {
        plt <- plotly::add_trace(
          plt,
          x = rep(marginal_y_x, length(marginal_y[[i]])),
          y = marginal_y[[i]],
          type = "scatter",
          mode = "markers",
          marker = list(
            color = marginal_col[[i]],
            size = marginal_size,
            symbol = "line-ew-open"
          ),
          showlegend = FALSE,
          hoverinfo = "y"
          # legendgroup = .names[i]
        )
      }
    } # /show_marginal_y

    ## { SE band } ----
    if (se_fit) {
      plt <- plotly::add_trace(
        plt,
        x = x[[i]],
        y = fitted[[i]] + se_times * se[[i]],
        type = scatter_type,
        mode = "lines",
        line = list(color = "transparent"),
        legendgroup = .names[i],
        showlegend = FALSE,
        hoverinfo = "none",
        inherit = FALSE
      )
      plt <- plotly::add_trace(
        plt,
        x = x[[i]],
        y = fitted[[i]] - se_times * se[[i]],
        type = scatter_type,
        mode = "lines",
        fill = "tonexty",
        fillcolor = plotly::toRGB(se_col[[i]], alpha = se_alpha),
        line = list(color = "transparent"),
        # name = shade_name,
        legendgroup = .names[i],
        showlegend = FALSE,
        hoverinfo = "none",
        inherit = FALSE
      )
    }
    if (!is.null(fit)) {
      ##  { Fitted line } ----
      lfit <- list(
        color = plotly::toRGB(fit_col[[i]], alpha = fit_alpha),
        width = fit_lwd
      )
      plt <- plotly::add_trace(
        plt,
        x = x[[i]],
        y = fitted[[i]],
        type = scatter_type,
        mode = "lines",
        line = lfit,
        name = fitted_text[i],
        legendgroup = .names[i],
        showlegend = if (legend & n_groups == 1) TRUE else FALSE,
        inherit = FALSE
      )
    }
  }

  # Layout ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )
  tickfont <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = theme[["tick_labels_col"]]
  )
  .legend <- list(
    title = list(
      text = legend_title,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = legend_col
      )
    ),
    x = legend_xy[1],
    xanchor = legend_xanchor,
    y = legend_xy[2],
    yanchor = legend_yanchor,
    font = list(
      family = theme[["font_family"]],
      size = font_size,
      color = legend_col
    ),
    orientation = legend_orientation,
    bgcolor = plotly::toRGB(legend_bg),
    bordercolor = plotly::toRGB(legend_border_col),
    borderwidth = legend_borderwidth,
    tracegroupgap = legend_group_gap
  )

  zerocol <- adjustcolor(theme[["zerolines_col"]], theme[["zerolines_alpha"]])
  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = ylab,
      showline = FALSE,
      showspikes = y_showspikes,
      spikecolor = spikecolor,
      spikedash = spikedash,
      spikemode = spikemode,
      spikesnap = spikesnap,
      spikethickness = spikethickness,
      # mirror = axes_mirrored,
      titlefont = f,
      showgrid = theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      zeroline = theme[["zerolines"]],
      zerolinecolor = zerocol,
      zerolinewidth = theme[["zerolines_lwd"]],
      range = ylim,
      automargin = automargin_y
    ),
    xaxis = list(
      title = list(text = xlab),
      showline = FALSE,
      showspikes = x_showspikes,
      spikecolor = spikecolor,
      spikedash = spikedash,
      spikemode = spikemode,
      spikesnap = spikesnap,
      spikethickness = spikethickness,
      # mirror = axes_mirrored,
      titlefont = f,
      showgrid = theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = tick_col,
      tickfont = tickfont,
      zeroline = theme[["zerolines"]],
      zerolinecolor = zerocol,
      zerolinewidth = theme[["zerolines_lwd"]],
      range = xlim,
      automargin = automargin_x
    ),
    title = list(
      text = main,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = main_col
      ),
      xref = "paper",
      x = theme[["main_adj"]],
      yref = "paper",
      y = main_y,
      yanchor = main_yanchor
    ),
    # titlefont = list(),
    paper_bgcolor = bg,
    plot_bgcolor = plot_bg,
    margin = margin,
    showlegend = legend,
    legend = .legend
  ) # /layout

  ## vline ----
  if (!is.null(vline)) {
    plt <- plotly::layout(
      plt,
      shapes = plotly_vline(
        vline,
        color = vline_col,
        width = vline_width,
        dash = vline_dash
      )
    )
  }

  ## hline ----
  if (!is.null(hline)) {
    plt <- plotly::layout(
      plt,
      shapes = plotly_hline(
        hline,
        color = hline_col,
        width = hline_width,
        dash = hline_dash
      )
    )
  }

  ## square ----
  if (axes_square) {
    plt <- plt |>
      plotly::layout(
        yaxis = list(
          scaleanchor = "x",
          scaleratio = 1
        )
      )
  }

  # Subtitle ----
  # add annotation at top left with same font as main title
  if (!is.null(subtitle)) {
    plt <- plt |>
      plotly::add_annotations(
        x = subtitle_x,
        y = subtitle_y,
        xref = subtitle_xref,
        yref = subtitle_yref,
        xanchor = subtitle_xanchor,
        yanchor = subtitle_yanchor,
        text = subtitle,
        showarrow = FALSE,
        font = list(
          family = theme[["font_family"]],
          size = font_size,
          color = main_col
        )
      )
  }

  # Config
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar_file_format,
      width = file_width,
      height = file_height
    ),
    scrollZoom = scrollZoom
  )

  # Write to file ----
  if (!is.null(filename)) {
    export_plotly(
      plt,
      filename = filename,
      width = file_width,
      height = file_height,
      scale = file_scale
    )
  } # /export_plotly

  plt
} # /rtemis::draw_scatter


#' True vs. Predicted Plot
#'
#' A `draw_scatter` wrapper for plotting true vs. predicted values
#'
#' @inheritParams draw_scatter
#' @param x Numeric, vector/data.frame/list: True values. If y is NULL and
#' `NCOL(x) > 1`, first two columns used as `x` and `y`, respectively
#' @param y Numeric, vector/data.frame/list: Predicted values
#' @param ... Additional arguments passed to [draw_scatter]
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(500)
#' y <- x + rnorm(500)
#' draw_fit(x, y)
#' }
draw_fit <- function(
  x,
  y,
  xlab = "True",
  ylab = "Predicted",
  fit = "glm",
  se_fit = TRUE,
  axes_square = TRUE,
  axes_equal = TRUE,
  diagonal = TRUE,
  ...
) {
  draw_scatter(
    x,
    y,
    xlab = xlab,
    ylab = ylab,
    fit = fit,
    se_fit = se_fit,
    axes_square = axes_square,
    axes_equal = axes_equal,
    diagonal = diagonal,
    ...
  )
} # /rtemis::draw_fit
