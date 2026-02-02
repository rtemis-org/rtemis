# draw_3Dscatter.R
# ::rtemis::
# 2019- EDG rtemis.org

#' Interactive 3D Scatter Plots
#'
#' Draw interactive 3D scatter plots using `plotly`.
#'
#' @details
#' See [rdocs.rtemis.org/draw](https://rdocs.rtemis.org/draw) for detailed documentation.
#'
#' Note that `draw_3Dscatter` uses the theme's `plot_bg` as `grid_col`.
#'
#' @param x Numeric, vector/data.frame/list: x-axis data.
#' @param y Numeric, vector/data.frame/list: y-axis data.
#' @param z Numeric, vector/data.frame/list: z-axis data.
#' @param fit Character: Fit method.
#' @param cluster Character: Clustering method.
#' @param cluster_config List: Config for clustering.
#' @param group Factor: Grouping variable.
#' @param formula Formula: Formula for non-linear least squares fit.
#' @param rsq Logical: If TRUE, print R-squared values in legend if `fit` is set.
#' @param mode Character, vector: "markers", "lines", "markers+lines".
#' @param order_on_x Logical: If TRUE, order `x` and `y` on `x`.
#' @param main Character: Main title.
#' @param xlab Character: x-axis label.
#' @param ylab Character: y-axis label.
#' @param zlab Character: z-axis label.
#' @param col Color for markers.
#' @param alpha Numeric: Alpha for markers.
#' @param bg Background color.
#' @param plot_bg Plot background color.
#' @param theme `Theme` object.
#' @param palette Character: Color palette.
#' @param axes_square Logical: If TRUE, draw a square plot.
#' @param group_names Character: Names for groups.
#' @param font_size Numeric: Font size.
#' @param marker_col Color for markers.
#' @param marker_size Numeric: Marker size.
#' @param fit_col Color for fit line.
#' @param fit_alpha Numeric: Alpha for fit line.
#' @param fit_lwd Numeric: Line width for fit line.
#' @param tick_font_size Numeric: Tick font size.
#' @param spike_col Spike lines color.
#' @param legend Logical: If TRUE, draw legend.
#' @param legend_xy Numeric: Position of legend.
#' @param legend_xanchor Character: X anchor for legend.
#' @param legend_yanchor Character: Y anchor for legend.
#' @param legend_orientation Character: Orientation of legend.
#' @param legend_col Color for legend text.
#' @param legend_bg Color for legend background.
#' @param legend_border_col Color for legend border.
#' @param legend_borderwidth Numeric: Border width for legend.
#' @param legend_group_gap Numeric: Gap between legend groups.
#' @param margin Numeric, named list: Margins for top, bottom, left, right.
#' @param fit_params `Hyperparameters` for fit.
#' @param width Numeric: Width of plot.
#' @param height Numeric: Height of plot.
#' @param padding Numeric: Graph padding.
#' @param displayModeBar Logical: If TRUE, display mode bar.
#' @param modeBar_file_format Character: File format for mode bar.
#' @param verbosity Integer: Verbosity level.
#' @param filename Character: Filename to save plot.
#' @param file_width Numeric: Width of saved file.
#' @param file_height Numeric: Height of saved file.
#' @param file_scale Numeric: Scale of saved file.
#'
#' @return A `plotly` object.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' draw_3Dscatter(iris, group = iris$Species, theme = theme_darkgraygrid())
#' }
draw_3Dscatter <- function(
  x,
  y = NULL,
  z = NULL,
  fit = NULL,
  cluster = NULL,
  cluster_config = NULL,
  group = NULL,
  formula = NULL,
  rsq = TRUE,
  mode = "markers",
  order_on_x = NULL,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  zlab = NULL,
  col = NULL,
  alpha = .8,
  bg = NULL,
  plot_bg = NULL,
  theme = choose_theme(),
  palette = rtemis_palette,
  axes_square = FALSE,
  group_names = NULL,
  font_size = 16,
  marker_col = NULL,
  marker_size = 8,
  fit_col = NULL,
  fit_alpha = .7,
  fit_lwd = 2.5,
  tick_font_size = 12,
  spike_col = NULL,
  legend = NULL,
  legend_xy = c(0, 1),
  legend_xanchor = "left",
  legend_yanchor = "auto",
  legend_orientation = "v",
  legend_col = NULL,
  legend_bg = "#FFFFFF00",
  legend_border_col = "#FFFFFF00",
  legend_borderwidth = 0,
  legend_group_gap = 0,
  margin = list(t = 30, b = 0, l = 0, r = 0),
  fit_params = NULL,
  width = NULL,
  height = NULL,
  padding = 0,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  verbosity = 0L,
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Arguments ----
  if (is.null(y) && is.null(z) && NCOL(x) > 2) {
    .colnames <- labelify(colnames(x))
    y <- x[, 2]
    z <- x[, 3]
    x <- x[, 1]
    if (is.null(xlab)) {
      xlab <- .colnames[1]
    }
    if (is.null(ylab)) {
      ylab <- .colnames[2]
    }
    if (is.null(zlab)) zlab <- .colnames[3]
  }
  if (!is.null(main)) {
    main <- paste0("<b>", main, "</b>")
  }
  if (!is.null(fit)) {
    if (fit == "none") fit <- NULL
  } # easier to work with shiny
  if (!is.null(fit)) {
    fit <- toupper(fit)
  }
  .mode <- mode
  .names <- group_names

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
        config = do_call(
          get_clust_setup_fn(cluster),
          cluster_config
        )
      )@clusters
    )
    group <- paste("Cluster", group)
  }

  # Data ----
  # xlab, ylab ----
  # The gsubs remove all text up to and including a "$" symbol if present
  if (is.null(xlab)) {
    if (is.list(x)) {
      xlab <- "x"
    } else {
      xlab <- labelify(gsub(".*\\$", "", deparse(substitute(x))))
    }
  }
  if (!is.null(y) && is.null(ylab)) {
    if (is.list(y)) {
      ylab <- "y"
    } else {
      ylab <- labelify(gsub(".*\\$", "", deparse(substitute(y))))
    }
  }
  if (!is.null(z) && is.null(zlab)) {
    if (is.list(z)) {
      zlab <- "z"
    } else {
      zlab <- labelify(gsub(".*\\$", "", deparse(substitute(z))))
    }
  }

  # '- Group ----
  if (!is.null(group)) {
    group <- as.factor(group)
    x <- split(x, group, drop = TRUE)
    y <- split(y, group, drop = TRUE)
    z <- split(z, group, drop = TRUE)
    if (is.null(group_names)) {
      group_names <- levels(droplevels(group))
    }
    names(x) <- names(y) <- names(z) <- .names <- group_names
  }

  # Try to get names from list or data frame inputs
  if (is.list(y) || NCOL(y) > 1) {
    if (is.null(.names) && !is.null(names(y))) .names <- names(y)
  }
  if (is.list(x) || NCOL(x) > 1) {
    if (is.null(.names) && !is.null(names(x))) .names <- names(x)
  }
  if (is.list(z) || NCOL(z) > 1) {
    if (is.null(.names) && !is.null(names(z))) .names <- names(z)
  }

  # Convert to lists ----
  x <- if (!is.list(x)) as.list(as.data.frame(x)) else x
  y <- if (!is.null(y) && !is.list(y)) as.list(as.data.frame(y)) else y
  z <- if (!is.null(z) && !is.list(z)) as.list(as.data.frame(z)) else z
  if (length(x) == 1 && length(y) > 1) {
    x <- rep(x, length(y))
    .names <- names(y)
  }
  if (length(y) == 1 && length(x) > 1) {
    y <- rep(y, length(x))
    .names <- names(x)
  }
  if (length(z) == 1 && length(x) > 1) {
    z <- rep(z, length(x))
    .names <- names(x)
  }
  n_groups <- length(x)

  # legend <- if (is.null(legend) & n_groups == 1 & is.null(fit)) FALSE else TRUE
  legend <- if (is.null(legend) && n_groups == 1) FALSE else TRUE

  if (length(.mode) < n_groups) {
    .mode <- c(.mode, rep(tail(.mode)[1], n_groups - length(.mode)))
  }

  # if (is.null(legend)) legend <- n_groups > 1
  if (is.null(.names)) {
    if (n_groups > 1) {
      .names <- paste("Group", seq_len(n_groups))
    } else {
      .names <- if (!is.null(fit)) fit else NULL
      .names <- NULL
    }
  }

  # Reorder ----
  if (order_on_x) {
    index <- lapply(x, order)
    x <- lapply(seq(x), function(i) x[[i]][index[[i]]])
    y <- lapply(seq(x), function(i) y[[i]][index[[i]]])
    z <- lapply(seq(x), function(i) z[[i]][index[[i]]])
  }

  # s.e. fit ----
  se_fit <- FALSE
  # if (se_fit) {
  #   if (!fit %in% c("GLM", "LM", "LOESS", "GAM", "NW")) {
  #     warning(paste("Standard error of the fit not available for", fit, "- try LM, LOESS, GAM, or NW"))
  #     se_fit <- FALSE
  #   }
  # }

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

  # Convert inputs to RGB
  spike_col <- plotly::toRGB(spike_col)

  # Theme ----
  axes_visible <- FALSE
  axes_mirrored <- FALSE
  check_is_S7(theme, Theme)

  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]], theme[["grid_alpha"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])
  if (!theme[["axes_visible"]]) {
    tick_col <- labs_col <- "transparent"
  }

  # marker_col, se_col ----
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

  # Derived
  if (is.null(legend_col)) {
    legend_col <- labs_col
  }

  # Size ----
  if (axes_square) {
    width <- height <- min(dev.size("px")) - 10
  }

  # fitted & se_fit ----
  # If plotting se bands, need to include (fitted +/- se.times * se) in the axis limits
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
  if (!is.null(fit)) {
    # learner <- get_train_fn(fit)
    fitted <- list()
    fitted_text <- character()
    for (i in seq_len(n_groups)) {
      df1 <- data.frame(x[[i]], y[[i]], z[[i]])
      mod <- train(
        df1,
        algorithm = fit,
        hyperparameters = fit_params,
        verbosity = verbosity
      )
      fitted[[i]] <- fitted(mod)
      if (se_fit) {
        se[[i]] <- se(mod)
      }
      fitted_text[i] <- fit
      if (rsq) {
        fitted_text[i] <- paste0(
          fitted_text[i],
          if (n_groups == 1) " (" else " ",
          "R<sup>2</sup> = ",
          ddSci(mod@metrics_training[["Rsq"]]),
          if (n_groups == 1) ")"
        )
      }
    }
  }

  # plotly ----
  plt <- plotly::plot_ly(
    width = width,
    height = height
  )
  for (i in seq_len(n_groups)) {
    # '- { Scatter } ----
    marker <- if (grepl("markers", .mode[i])) {
      list(
        color = plotly::toRGB(marker_col[[i]], alpha = alpha),
        size = marker_size
      )
    } else {
      NULL
    }
    plt <- plotly::add_trace(
      plt,
      x = x[[i]],
      y = y[[i]],
      z = z[[i]],
      type = "scatter3d",
      mode = .mode[i],
      # fillcolor = plotly::toRGB(col[[i]], alpha),
      # name = if (n_groups > 1) .names[i] else "Raw",
      name = .names[i],
      # text = .text[[i]],
      # hoverinfo = "text",
      # marker = if (grepl("markers", .mode[i])) list(color = plotly::toRGB(marker_col[[i]], alpha = alpha)) else NULL,
      marker = marker,
      line = if (grepl("lines", .mode[i])) {
        list(color = plotly::toRGB(marker_col[[i]], alpha = alpha))
      } else {
        NULL
      },
      legendgroup = if (n_groups > 1) .names[i] else "Raw",
      showlegend = legend
    )
    # if (se_fit) {
    #   # '- { SE band } ----
    #   plt <- plotly::add_trace(plt,
    #                            x = x[[i]],
    #                            y = fitted[[i]] + se.times * se[[i]],
    #                            type = "scatter",
    #                            mode = "lines",
    #                            line = list(color = "transparent"),
    #                            legendgroup = .names[i],
    #                            showlegend = FALSE,
    #                            hoverinfo = "none",
    #                            inherit = FALSE)
    #   plt <- plotly::add_trace(plt, x = x[[i]],
    #                            y = fitted[[i]] - se.times * se[[i]],
    #                            type = "scatter",
    #                            mode = "lines",
    #                            fill = "tonexty",
    #                            fillcolor = plotly::toRGB(se_col[[i]], alpha = se.alpha),
    #                            line = list(color = "transparent"),
    #                            # name = shade.name,
    #                            legendgroup = .names[i],
    #                            showlegend = FALSE,
    #                            hoverinfo = "none",
    #                            inherit = FALSE)
    # }

    if (!is.null(fit)) {
      # '- { Fitted mesh } ----
      plt <- plotly::add_trace(
        plt,
        x = x[[i]],
        y = y[[i]],
        z = fitted[[i]],
        type = "mesh3d",
        opacity = fit_alpha,
        name = fitted_text[i],
        # legendgroup = .names[i],
        # showlegend = if (legend & n_groups == 1) TRUE else FALSE,
        inherit = FALSE,
        showscale = FALSE,
        intensity = 1,
        colorscale = list(
          c(0, plotly::toRGB(fit_col[[i]])),
          c(1, plotly::toRGB(fit_col[[i]]))
        )
      )
    }
  }
  # Layout ----
  # '- layout ----
  f <- list(
    family = theme[["font_family"]],
    size = font_size,
    color = labs_col
  )
  tickfont <- list(
    family = theme[["font_family"]],
    size = tick_font_size,
    color = theme[["tick_labels_col"]]
  )
  .legend <- list(
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

  plt <- plotly::layout(
    plt,
    scene = list(
      yaxis = list(
        title = ylab,
        showline = axes_visible,
        mirror = axes_mirrored,
        titlefont = f,
        showgrid = theme[["grid"]],
        gridcolor = grid_col,
        gridwidth = theme[["grid_lwd"]],
        tickcolor = tick_col,
        tickfont = tickfont,
        zeroline = FALSE,
        spikecolor = spike_col
      ),
      xaxis = list(
        title = xlab,
        showline = axes_visible,
        mirror = axes_mirrored,
        titlefont = f,
        showgrid = theme[["grid"]],
        gridcolor = grid_col,
        gridwidth = theme[["grid_lwd"]],
        tickcolor = tick_col,
        tickfont = tickfont,
        zeroline = FALSE,
        spikecolor = spike_col
      ),
      zaxis = list(
        title = zlab,
        showline = axes_visible,
        mirror = axes_mirrored,
        titlefont = f,
        showgrid = theme[["grid"]],
        gridcolor = grid_col,
        gridwidth = theme[["grid_lwd"]],
        tickcolor = tick_col,
        tickfont = tickfont,
        zeroline = FALSE,
        spikecolor = spike_col
      )
    ),
    title = list(
      text = main,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = main_col
      )
    ),
    # titlefont = list(),
    paper_bgcolor = bg,
    plot_bgcolor = plot_bg,
    margin = margin,
    showlegend = legend,
    legend = .legend
  )

  # Padding
  plt[["sizingPolicy"]][["padding"]] <- padding
  # Config
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar_file_format,
      width = file_width,
      height = file_height
    )
  )

  # Write to file ----
  if (!is.null(filename)) {
    plotly::save_image(
      plt,
      file = file.path(filename),
      width = file_width,
      height = file_height,
      scale = file_scale
    )
  }

  plt
} # /rtemis::draw_3Dscatter
