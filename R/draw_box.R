# draw_box.R
# ::rtemis::
# EDG rtemis.org

#' Interactive Boxplots & Violin plots
#'
#' Draw interactive boxplots or violin plots using \pkg{plotly}
#'
#' For multiple box plots, the recommendation is:
#' - `x=dat[, columnindex]` for multiple variables of a data.frame
#' - `x=list(a=..., b=..., etc.)` for multiple variables of potentially
#' different length
#' - `x=split(var, group)` for one variable with multiple groups: group names
#' appear below boxplots
#' - `x=dat[, columnindex], group = factor` for grouping multiple variables:
#' group names appear in legend
#'
#' If `orientation == "h"`, `xlab` is applied to y-axis and vice versa.
#' Similarly, `x.axist.type` applies to y-axis - this defaults to
#' "category" and would not normally need changing.
#'
#' @param x Vector or List of vectors: Input
#' @param time Date or date-time vector
#' @param time_bin Character: "year", "quarter", "month", or "day". Period to
#' bin by
#' @param type Character: "box" or "violin"
#' @param group Factor to group by
#' @param x_transform Character: "none", "scale", or "minmax" to use raw values,
#' scaled and centered values or min-max normalized to 0-1, respectively.
#' Transform is applied to each variable before grouping, so that groups are
#' comparable
#' @param main Character: Plot title.
#' @param xlab Character: x-axis label.
#' @param ylab  Character: y-axis label.
#' @param col Color, vector: Color for boxes. If NULL, which will draw
#' colors from `palette`
#' @param alpha Float (0, 1]: Transparency for box colors.
#' @param bg Color: Background color.
#' @param plot_bg Color: Background color for plot area.
#' @param theme Theme object.
#' @param palette Character: Name of \pkg{rtemis} palette to use. Only used if `col = NULL`.
#' @param quartilemethod Character: "linear", "exclusive", "inclusive"
#' @param xlim Numeric vector: x-axis limits
#' @param ylim Numeric vector: y-axis limits
#' @param boxpoints Character or FALSE: "all", "suspectedoutliers", "outliers"
#' See <https://plotly.com/r/box-plots/#choosing-the-algorithm-for-computing-quartiles>
#' @param xnames Character, vector, length = NROW(x): x-axis names. Default = NULL, which
#' tries to set names automatically.
#' @param group_lines Logical: If TRUE, add separating lines between groups of
#' boxplots
#' @param group_lines_dash Character: "solid", "dot", "dash", "longdash",
#' "dashdot", or "longdashdot"
#' @param group_lines_col Color for `group_lines`
#' @param group_lines_alpha Numeric: transparency for `group_lines_col`
#' @param order_by_fn Function: If defined, order boxes by increasing value of
#' this function (e.g. median).
#' @param font_size  Float: Font size for all labels.
#' @param ylab_standoff Numeric: Standoff for y-axis label
#' @param legend Logical: If TRUE, draw legend.
#' @param legend_col Color: Legend text color. Default = NULL, determined by
#' the theme.
#' @param legend_xy Float, vector, length 2: Relative x, y position for legend.
#' @param xaxis_type Character: "linear", "log", "date", "category",
#' "multicategory"
#' @param cataxis_tickangle Numeric: Angle for categorical axis tick labels
#' @param margin Named list: plot margins.
#' @param violin_box Logical: If TRUE and type is "violin" show box within
#' violin plot
#' @param orientation Character: "v" or "h" for vertical, horizontal
#' @param annotate_n Logical: If TRUE, annotate with N in each box
#' @param annotate_n_y Numeric: y position for `annotate_n`
#' @param annotate_mean Logical: If TRUE, annotate with mean of each box
#' @param annotate_meansd Logical: If TRUE, annotate with mean (SD) of each box
#' @param annotate_meansd_y Numeric: y position for `annotate_meansd`
#' @param annotate_col Color for annotations
#' @param labelify Logical: If TRUE, [labelify] x names
#' @param legend_orientation "v" or "h" for vertical, horizontal
#' @param legend_xanchor Character: Legend's x anchor: "left", "center",
#' "right", "auto"
#' @param legend_yanchor Character: Legend's y anchor: "top", "middle",
#' "bottom", "auto"
#' @param automargin_x Logical: If TRUE, automatically set x-axis margins
#' @param automargin_y Logical: If TRUE, automatically set y-axis margins
#' @param boxgroupgap Numeric: Sets the gap (in plot fraction) between boxes
#' of the same location coordinate
#' @param hovertext Character vector: Text to show on hover for each data point
#' @param show_n Logical: If TRUE, show N in each box
#' @param pvals Numeric vector: Precomputed p-values. Should correspond to each box.
#' Bypasses `htest` and `htest_compare`. Requires `group` to be set
#' @param htest Character: e.g. "t.test", "wilcox.test" to compare each box to
#' the *first* box. If grouped, compare within each group to the first box.
#' If p-value of test is less than `htest.thresh`, add asterisk above/
#' to the side of each box
#' @param htest_compare Integer: 0: Compare all distributions against the first one;
#' 2: Compare every second box to the one before it. Requires `group` to
#' be set
#' @param htest_y Numeric: y coordinate for `htest` annotation
#' @param htest_annotate Logical: if TRUE, include htest annotation
#' @param htest_annotate_x Numeric: x-axis paper coordinate for htest annotation
#' @param htest_annotate_y Numeric: y-axis paper coordinate for htest annotation
#' @param htest_star_col Color for htest annotation stars
#' @param htest_bracket_col Color for htest annotation brackets
#' @param starbracket_pad Numeric: Padding for htest annotation brackets
#' @param use_plotly_group If TRUE, use plotly's `group` arg to group
#' boxes.
#' @param width Numeric: Force plot size to this width. Default = NULL, i.e. fill
#' available space
#' @param height Numeric: Force plot size to this height. Default = NULL, i.e. fill
#' available space
#' @param displayModeBar Logical: If TRUE, show plotly's modebar
#' @param filename Character: Path to file to save static plot.
#' @param modeBar_file_format Character: "svg", "png", "jpeg", "pdf"
#' @param file_width Integer: File width in pixels for when `filename` is
#' set.
#' @param file_height Integer: File height in pixels for when `filename`
#' is set.
#' @param file_scale Numeric: If saving to file, scale plot by this number
#' @param mathjax Character: "local", "cdn", or NULL
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' # A.1 Box plot of 4 variables
#' draw_box(iris[, 1:4])
#' # A.2 Grouped Box plot
#' draw_box(iris[, 1:4], group = iris[["Species"]])
#' draw_box(iris[, 1:4], group = iris[["Species"]], annotate_n = TRUE)
#' # B. Boxplot binned by time periods
#' # Synthetic data with an instantenous shift in distributions
#' set.seed(2021)
#' dat1 <- data.frame(alpha = rnorm(200, 0), beta = rnorm(200, 2), gamma = rnorm(200, 3))
#' dat2 <- data.frame(alpha = rnorm(200, 5), beta = rnorm(200, 8), gamma = rnorm(200, -3))
#' x <- rbind(dat1, dat2)
#' startDate <- as.Date("2019-12-04")
#' endDate <- as.Date("2021-03-31")
#' time <- seq(startDate, endDate, length.out = 400)
#' draw_box(x[, 1], time, "year", ylab = "alpha")
#' draw_box(x, time, "year", legend.xy = c(0, 1))
#' draw_box(x, time, "quarter", legend.xy = c(0, 1))
#' draw_box(x, time, "month",
#'   legend.orientation = "h",
#'   legend.xy = c(0, 1),
#'   legend.yanchor = "bottom"
#' )
#' # (Note how the boxplots widen when the period includes data from both dat1 and dat2)
#' }
#'
draw_box <- function(
  x,
  time = NULL,
  time_bin = c("year", "quarter", "month", "day"),
  type = c("box", "violin"),
  group = NULL,
  x_transform = c("none", "scale", "minmax"),
  main = NULL,
  xlab = "",
  ylab = NULL,
  col = NULL,
  alpha = .6,
  bg = NULL,
  plot_bg = NULL,
  theme = choose_theme(),
  palette = rtemis_palette,
  boxpoints = "outliers",
  quartilemethod = "linear",
  xlim = NULL,
  ylim = NULL,
  violin_box = TRUE,
  orientation = "v",
  annotate_n = FALSE,
  annotate_n_y = 1,
  annotate_mean = FALSE, # forr A.2.b.
  annotate_meansd = FALSE,
  annotate_meansd_y = 1,
  annotate_col = theme[["labs_col"]],
  xnames = NULL,
  group_lines = FALSE,
  group_lines_dash = "dot",
  group_lines_col = NULL,
  group_lines_alpha = .5,
  labelify = TRUE,
  order_by_fn = NULL,
  font_size = 16,
  # Axes
  ylab_standoff = 18,
  legend = NULL,
  legend_col = NULL,
  legend_xy = NULL,
  legend_orientation = "v",
  legend_xanchor = "auto",
  legend_yanchor = "auto",
  xaxis_type = "category",
  cataxis_tickangle = "auto",
  # margin = list(t = 35, pad = 0),
  margin = list(b = 65, l = 65, t = 50, r = 12, pad = 0),
  automargin_x = TRUE,
  automargin_y = TRUE,
  # boxgap = 0, #1/nvars, #.12,
  boxgroupgap = NULL,
  hovertext = NULL,
  show_n = FALSE,
  # boxmode = NULL,
  pvals = NULL,
  htest = "none",
  htest_compare = 0,
  #    htest.thresh = .05,
  htest_y = NULL,
  htest_annotate = TRUE,
  htest_annotate_x = 0,
  htest_annotate_y = -.065,
  htest_star_col = theme[["labs_col"]],
  htest_bracket_col = theme[["labs_col"]],
  starbracket_pad = c(.04, .05, .09),
  use_plotly_group = FALSE,
  width = NULL,
  height = NULL,
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  filename = NULL,
  file_width = 500,
  file_height = 500,
  file_scale = 1,
  mathjax = NULL
) {
  # Dependencies ----
  check_dependencies("plotly")

  # Arguments ----
  type <- match.arg(type)
  x_transform <- match.arg(x_transform)

  # Convert vector or data.frame/data.table/matrix to list
  if (!is.list(x)) {
    # x is vector
    if (is.numeric(x)) {
      .names <- deparse(substitute(x))
      x <- list(x)
      names(x) <- .names
    } else {
      # x is data.frame or matrix
      .names <- colnames(x)
      x <- lapply(seq_len(NCOL(x)), function(i) x[, i])
      names(x) <- .names
    }
  }
  nvars <- length(x)
  if (nvars > 1 && !is.null(group) && !is.null(time)) {
    cli::cli_abort("Better use subplot for each variable")
  }
  horizontal <- orientation == "h"

  if (x_transform != "none") {
    if (x_transform == "scale") {
      x <- lapply(x, \(z) as.numeric(scale(z)))
    } else if (x_transform == "minmax") {
      x <- lapply(x, drange)
    } else {
      cli::cli_abort("Unsupported x_transform specified")
    }
  }

  # Order by fn ----
  if (!is.null(order_by_fn)) {
    if (is.null(time)) {
      if (is.list(x)) {
        .order <- order(sapply(x, order_by_fn, na.rm = TRUE))
        if (is.data.frame(x)) {
          x <- x[, .order]
        } else {
          x <- x[names(x)[.order]]
        }
      }
      if (!is.null(xnames)) {
        xnames <- xnames[.order]
      }
    } else {
      cli::cli_abort("Cannot use `order_by_fn` with `time`")
    }
  }

  # Remove non-numeric vectors
  # which.nonnum <- which(sapply(x, function(i) !is.numeric(i)))
  # if (length(which.nonnum) > 0) x[[which.nonnum]] <- NULL

  if (!is.null(group)) {
    group <- factor(group)
  }
  n.groups <- if (is.null(group)) {
    length(x)
  } else {
    nlevels(group)
  }
  if (n.groups == 1) {
    htest <- "none"
  }
  .xnames <- xnames
  if (is.null(.xnames)) {
    .xnames <- names(x)
    if (is.null(.xnames)) {
      .xnames <- paste0("Feature", seq(n.groups))
    }
    if (labelify) .xnames <- labelify(.xnames)
  }

  # Colors ----
  if (is.character(palette)) {
    palette <- rtpalette(palette)
  }
  if (is.null(col)) {
    col <- recycle(palette, seq(n.groups))[seq(n.groups)]
  }
  if (!is.null(order_by_fn)) {
    col <- col[.order]
  }

  # Theme ----
  check_is_S7(theme, Theme)

  if (theme[["main_font"]] == 2) {
    main <- paste0("<b>", main, "</b>")
  }
  bg <- plotly::toRGB(theme[["bg"]])
  plot_bg <- plotly::toRGB(theme[["plot_bg"]])
  grid_col <- plotly::toRGB(theme[["grid_col"]])
  tick_col <- plotly::toRGB(theme[["tick_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  main_col <- plotly::toRGB(theme[["main_col"]])
  # axes_col <- plotly::toRGB(theme[["axes_col"]])

  # Derived
  if (is.null(legend_col)) {
    legend_col <- labs_col
  }

  # Plot ----
  if (is.null(time)) {
    if (is.null(group)) {
      # A.1 Single and multiple boxplots ----
      if (is.null(legend)) {
        legend <- FALSE
      }
      # Args for first trace
      .args <- if (horizontal) {
        list(x = x[[1]], y = NULL)
      } else {
        list(x = NULL, y = x[[1]])
      }
      .args <- c(
        .args,
        list(
          type = type,
          # name = .xnames[1],
          name = if (show_n) {
            paste0(.xnames[1], " (N=", length(x[[1]]), ")")
          } else {
            .xnames[1]
          },
          line = list(color = plotly::toRGB(col[1])),
          fillcolor = plotly::toRGB(col[1], alpha),
          marker = list(color = plotly::toRGB(col[1], alpha)),
          showlegend = legend
          # width = width
        )
      )
      if (!is.null(hovertext) && n.groups == 1) {
        hovertext <- list(hovertext)
      }
      if (type == "box") {
        .args <- c(
          .args,
          list(
            quartilemethod = quartilemethod,
            boxpoints = boxpoints
          )
        )
        if (!is.null(hovertext)) .args[["text"]] <- hovertext[[1]]
      }
      if (type == "violin") {
        .args[["box"]] <- list(visible = violin_box)
      }
      plt <- do.call(plotly::plot_ly, .args)
      if (n.groups > 1) {
        for (i in seq_len(n.groups)[-1]) {
          plt <- plotly::add_trace(
            plt,
            x = if (horizontal) x[[i]] else NULL,
            y = if (horizontal) NULL else x[[i]],
            # name = .xnames[i],
            name = if (show_n) {
              paste0(.xnames[i], " (N=", length(x[[i]]), ")")
            } else {
              .xnames[i]
            },
            line = list(color = plotly::toRGB(col[i])), # box borders
            fillcolor = plotly::toRGB(col[i], alpha), # box fill
            marker = list(color = plotly::toRGB(col[i], alpha)), # points
            text = if (!is.null(hovertext)) hovertext[[i]] else NULL
          )
        }
      }

      # '-Annotate N ----
      if (annotate_n) {
        Nperbox <- Filter(
          function(i) i > 0,
          sapply(x, function(j) length(na.exclude(j)))
        )
        plt <- plt |>
          plotly::add_annotations(
            xref = "paper",
            yref = "paper",
            xanchor = "right",
            yanchor = "bottom",
            x = 0,
            y = annotate_n_y,
            text = "N =",
            font = list(
              family = theme[["font_family"]],
              size = font_size,
              color = annotate_col
            ),
            showarrow = FALSE
          ) |>
          plotly::add_annotations(
            xref = "x",
            yref = "paper",
            yanchor = "bottom",
            # x = seq_len(nvars) - 1,
            x = seq_along(Nperbox) - 1,
            y = 1,
            text = as.character(Nperbox),
            font = list(
              family = theme[["font_family"]],
              size = font_size,
              color = annotate_col
            ),
            showarrow = FALSE
          )
      } # /annotate_n

      # '-Annotate Mean SD ----
      if (annotate_meansd) {
        Meanperbox <- Filter(
          function(i) i > 0,
          sapply(x, function(j) mean(na.exclude(j)))
        ) |>
          round(digits = 2) |>
          format(nsmall = 2)
        SDperbox <- Filter(
          function(i) i > 0,
          sapply(x, function(j) sd(na.exclude(j)))
        ) |>
          round(digits = 2) |>
          format(nsmall = 2)
        plt <- plt |>
          # plotly::add_annotations(
          #   xref = "paper", yref = "paper",
          #   xanchor = "right",
          #   yanchor = "bottom",
          #   x = 0, y = annotate_meansd_y,
          #   text = "Mean (SD)",
          #   font = list(
          #     family = theme[["font_family"]],
          #     size = font.size,
          #     color = annotate.col
          #   ),
          #   showarrow = FALSE
          # ) |>
          plotly::add_annotations(
            xref = "x",
            yref = "paper",
            yanchor = "bottom",
            # x = seq_len(nvars) - 1,
            x = seq_along(Meanperbox) - 1,
            y = 1,
            # text = as.character(Nperbox),
            text = paste0(Meanperbox, " (", SDperbox, ")"),
            font = list(
              family = theme[["font_family"]],
              size = font_size,
              color = annotate_col
            ),
            showarrow = FALSE
          )
      } # /annotate_meansd

      # '-htest ----
      if (htest != "none") {
        if (htest_compare == 0) {
          pvals <- sapply(x[-1], \(v) {
            suppressWarnings(
              do.call(htest, list(x = x[[1]], y = v))[["p.value"]]
            )
          })
        }
        y_sb <- starbracket_y(unlist(x), pad = starbracket_pad)
        if (is.null(htest_y)) {
          htest_y <- y_sb[["star"]]
        }
        plt <- plt |>
          plotly::add_annotations(
            xref = if (horizontal) "paper" else "x",
            # yref = if (horizontal) "x" else "paper",
            yref = if (horizontal) "x" else "y",
            yanchor = if (horizontal) "auto" else "top",
            xanchor = if (horizontal) "center" else "auto",
            x = if (horizontal) htest_y else seq_along(pvals), # exclude first
            y = if (horizontal) seq_along(pvals) else htest_y,
            # text = unname(ifelse(pvals < htest.thresh, "*", "")),
            text = pval_stars(pvals),
            font = list(
              family = theme[["font_family"]],
              size = font_size,
              color = annotate_col
            ),
            showarrow = FALSE
          )

        if (htest_annotate) {
          test <- switch(
            htest,
            `wilcox.test` = "Wilcoxon",
            `t.test` = "T-test",
            htest
          )
          plt <- plt |>
            plotly::add_annotations(
              xref = "paper",
              yref = "paper",
              yanchor = "top",
              xanchor = "left",
              x = htest_annotate_x,
              y = htest_annotate_y,
              # text = paste0("<sup>*</sup>", test, " p-val < ", htest.thresh),
              # text = paste0("* ", test, " p-val < ", htest.thresh),
              # text = paste0(
              #     '<span style="color:',
              #     htest.star.col, '">* </span>',
              #     test, " p-val < ", htest.thresh),
              text = paste0(
                test,
                " p-val:",
                '<span style="color:',
                htest_star_col,
                '"> * </span>',
                "< .05",
                '<span style="color:',
                htest_star_col,
                '"> ** </span>',
                "< .01",
                '<span style="color:',
                htest_star_col,
                '"> *** </span>',
                "< .001"
              ),
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = annotate_col
              ),
              showarrow = FALSE
            )
        }
      } # / htest!="none"
    } else {
      if (use_plotly_group) {
        # A.2.a Grouped boxplots with [group] ----
        # Best to use this for multiple variables x group.
        # For single variables x group, preferred way it to use
        # split(var, group) => A1
        if (is.null(legend)) {
          legend <- TRUE
        }
        dt <- cbind(data.table::as.data.table(x), group = group)
        dtlong <- data.table::melt(
          dt[, ID := seq_len(nrow(dt))],
          id.vars = c("ID", "group")
        )
        if (is.null(ylab)) {
          ylab <- ""
        }
        .args <- list(
          data = dtlong,
          type = type,
          x = if (horizontal) ~value else ~variable,
          y = if (horizontal) ~variable else ~value,
          color = ~group,
          colors = col2hex(col),
          showlegend = legend
        )
        if (type == "box") {
          .args <- c(
            .args,
            list(
              quartilemethod = quartilemethod,
              boxpoints = boxpoints,
              alpha = alpha
            )
          )
          if (!is.null(hovertext)) {
            dtlong <- merge(dtlong, cbind(dt[, list(ID)], hovertext))
            .args[["text"]] <- dtlong[["hovertext"]]
          }
        }
        if (type == "violin") {
          .args[["box"]] <- list(visible = violin_box)
        }
        cataxis <- list(
          tickvals = 0:(NCOL(dt) - 2),
          ticktext = .xnames
        )
        .args <- c(list(width = width, height = height), .args)
        plt <- do.call(plotly::plot_ly, .args) |>
          plotly::layout(
            boxmode = "group",
            xaxis = if (horizontal) NULL else cataxis,
            yaxis = if (horizontal) cataxis else NULL
          )
      } else {
        # A.2.b Grouped boxplots with split and loop ----
        # Replaces A.2.a to allow annotation positioning
        if (is.null(legend)) {
          legend <- TRUE
        }
        dts <- split(data.table::as.data.table(x), group, drop = TRUE)

        if (is.null(ylab)) {
          ylab <- ""
        }
        if (type == "box") {
          .args <- list(
            type = "box",
            quartilemethod = quartilemethod,
            boxpoints = boxpoints,
            alpha = alpha
          )
        } else {
          .args <- list(
            type = "violin",
            box = list(visible = violin_box)
          )
        }

        varnames <- names(x)
        nvars <- length(varnames)
        ngroups <- length(dts)
        groupnames <- names(dts)
        xval <- do.call(paste, expand.grid(groupnames, varnames))
        # text = xval[i],
        xval <- factor(xval, levels = xval)

        boxindex <- 0

        # plt <- plotly::plot_ly(type = type) # box or violin
        .args <- c(list(width = width, height = height), .args)
        plt <- do.call(plotly::plot_ly, .args)
        for (i in seq_along(varnames)) {
          # loop vars
          for (j in seq_along(dts)) {
            # loop groups
            boxindex <- boxindex + 1
            plt <- plt |>
              plotly::add_trace(
                x = if (horizontal) dts[[j]][[i]] else xval[boxindex],
                y = if (horizontal) xval[boxindex] else dts[[j]][[i]],
                name = groupnames[j],
                meta = xval[boxindex],
                line = list(color = plotly::toRGB(col[j])),
                fillcolor = plotly::toRGB(col[j], alpha),
                marker = list(color = plotly::toRGB(col[j], alpha)),
                showlegend = legend & (i == nvars),
                hoverinfo = "all",
                legendgroup = groupnames[j]
              )
          }
        }

        cataxis <- list(
          type = "category",
          tickmode = "array",
          tickvals = (mean(seq_len(ngroups)) + 0:(nvars - 1) * ngroups) - 1, # need -1 if type = "category"
          ticktext = .xnames,
          tickangle = cataxis_tickangle,
          automargin = TRUE
        )

        plt <- plt |>
          plotly::layout(
            xaxis = if (horizontal) NULL else cataxis,
            yaxis = if (horizontal) cataxis else NULL
          )

        # '- Group lines ----
        if (nvars > 1 && group_lines) {
          if (is.null(group_lines_col)) {
            group_lines_col <- theme[["fg"]]
          }
          group_lines_col <- adjustcolor(
            group_lines_col,
            group_lines_alpha
          )
          at <- seq((ngroups - .5), (ngroups * (nvars - 1) - .5), by = ngroups)
          if (horizontal) {
            plt <- plt |>
              plotly::layout(
                shapes = plotly_hline(
                  at,
                  color = group_lines_col,
                  dash = group_lines_dash
                )
              )
          } else {
            plt <- plt |>
              plotly::layout(
                shapes = plotly_vline(
                  at,
                  color = group_lines_col,
                  dash = group_lines_dash
                )
              )
          }
        }

        # '-Annotate N ----
        if (annotate_n) {
          Nperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) length(na.exclude(j)))
            })))
          )
          plt <- plt |>
            plotly::add_annotations(
              xref = "paper",
              yref = "paper",
              xanchor = "right",
              yanchor = "bottom",
              x = 0,
              y = annotate_n_y,
              text = "N =",
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = annotate_col
              ),
              showarrow = FALSE
            ) |>
            plotly::add_annotations(
              xref = "x",
              yref = "paper",
              yanchor = "bottom",
              x = seq_len(nvars * ngroups) - 1,
              y = 1,
              text = as.character(Nperbox),
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = annotate_col
              ),
              showarrow = FALSE
            )
        } # /annotate_n

        # '-Annotate Mean SD ----
        if (annotate_meansd) {
          Meanperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) mean(na.exclude(j)))
            })))
          ) |>
            round(digits = 2) |>
            format(nsmall = 2)
          SDperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) sd(na.exclude(j)))
            })))
          ) |>
            round(digits = 2) |>
            format(nsmall = 2)
          plt <- plt |>
            # plotly::add_annotations(
            #   xref = "paper", yref = "paper",
            #   xanchor = "right",
            #   yanchor = "bottom",
            #   x = 0, y = annotate_meansd_y,
            #   text = "N =",
            #   font = list(
            #     family = theme[["font_family"]],
            #     size = font_size,
            #     color = annotate_col
            #   ),
            #   showarrow = FALSE
            # ) |>
            plotly::add_annotations(
              xref = "x",
              yref = "paper",
              yanchor = "bottom",
              x = seq_len(nvars * ngroups) - 1,
              y = 1,
              text = paste0(Meanperbox, " (", SDperbox, ")"),
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = annotate_col
              ),
              showarrow = FALSE
            )
        } # /annotate_meansd

        # '-Annotate Mean ----
        if (annotate_mean) {
          Meanperbox <- Filter(
            function(i) i > 0,
            c(t(sapply(dts, function(i) {
              sapply(i, function(j) mean(na.exclude(j)))
            })))
          ) |>
            round(digits = 1) |>
            format(nsmall = 1)
          plt <- plt |>
            plotly::add_annotations(
              xref = "x",
              yref = "paper",
              yanchor = "bottom",
              x = seq_len(nvars * ngroups) - 1,
              y = 1,
              text = Meanperbox,
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = annotate_col
              ),
              showarrow = FALSE
            )
        } # /annotate_mean

        # '- htest ----
        if (htest != "none" || !is.null(pvals)) {
          # dts list elements are groups; columns are variables
          # pvals is N groups -1 x N vars
          if (is.null(pvals)) {
            if (htest_compare == 0) {
              pvals <- sapply(seq_len(nvars), \(cid) {
                sapply(2:ngroups, \(gid) {
                  suppressWarnings(
                    do.call(
                      htest,
                      list(
                        x = dts[[1]][[cid]],
                        y = dts[[gid]][[cid]]
                      )
                    )[["p.value"]]
                  )
                })
              })
              pvals <- c(rbind(1, pvals))
            } else if (htest_compare == 2) {
              pvals <- rep(1, nvars * ngroups)
              pvals[seq(2, ngroups * nvars, 2)] <- lapply(
                seq_len(nvars),
                \(cid) {
                  lapply(seq(htest_compare, ngroups, htest_compare), \(gid) {
                    suppressWarnings(
                      do.call(
                        htest,
                        list(
                          x = dts[[gid - 1]][[cid]],
                          y = dts[[gid]][[cid]]
                        )
                      )[["p.value"]]
                    )
                  })
                }
              ) |>
                unlist()
            }
          }
          # if brackets are drawn, center stars above them, otherwise
          # center stars above boxes
          axshift <- if (htest_compare == 2) 1.5 else 1
          y_sb <- starbracket_y(unlist(x), pad = starbracket_pad)
          if (is.null(htest_y)) {
            htest_y <- y_sb[["star"]]
          }
          plt <- plt |>
            plotly::add_annotations(
              xref = if (horizontal) "paper" else "x",
              # yref = if (horizontal) "x" else "paper",
              yref = if (horizontal) "x" else "y",
              yanchor = if (horizontal) "auto" else "top",
              xanchor = if (horizontal) "center" else "auto",
              x = if (horizontal) {
                htest_y
              } else {
                seq_len(nvars * ngroups) - axshift
              },
              y = if (horizontal) {
                seq_len(nvars * ngroups) - axshift
              } else {
                htest_y
              },
              # text = unname(ifelse(pvals < htest.thresh, "*", "")),
              text = pval_stars(pvals),
              font = list(
                family = theme[["font_family"]],
                size = font_size,
                color = htest_star_col
              ),
              showarrow = FALSE
            )
          if (htest_annotate) {
            test <- switch(
              htest,
              `wilcox.test` = "Wilcoxon",
              `t.test` = "T-test",
              htest
            )
            plt <- plt |>
              plotly::add_annotations(
                xref = "paper",
                yref = "paper",
                yanchor = "top",
                xanchor = "left",
                x = htest_annotate_x,
                y = htest_annotate_y,
                # text = paste0("<sup>*</sup>", test, " p-val < ", htest.thresh),
                # text = paste0("* ", test, " p-val < ", htest.thresh),
                # text = paste0(
                #     '<span style="color:',
                #     htest.star.col, '">* </span>',
                #     test, " p-val < ", htest.thresh
                # ),
                text = paste0(
                  test,
                  " p-val:",
                  '<span style="color:',
                  htest_star_col,
                  '"> * </span>',
                  "< .05",
                  '<span style="color:',
                  htest_star_col,
                  '"> ** </span>',
                  "< .01",
                  '<span style="color:',
                  htest_star_col,
                  '"> *** </span>',
                  "< .001"
                ),
                font = list(
                  family = theme[["font_family"]],
                  size = font_size,
                  color = annotate_col
                ),
                showarrow = FALSE
              )
          } # /htest.annotate

          # '- htest brackets for htest.compare == 2 ----
          if (htest_compare == 2) {
            for (i in seq(2, ngroups * nvars, 2)) {
              if (pvals[i] < .05) {
                # y_bracket <- bracket_y(unlist(x))
                plt <- plt |>
                  plotly::add_trace(
                    x = c(rep(xval[i - 1], 2), rep(xval[i], 2)),
                    y = y_sb[["bracket"]],
                    type = "scatter",
                    mode = "lines",
                    inherit = FALSE,
                    line = list(color = htest_bracket_col, width = 1),
                    showlegend = FALSE
                  )
              }
            }
          }
        } # /htest grouped
      }
    }
  } else {
    # B. Time-binned boxplots ----
    time_bin <- match.arg(time_bin)
    if (is.null(xlab)) {
      xlab <- ""
    }
    if (is.null(ylab)) {
      ylab <- ""
    }
    if (is.null(legend)) {
      legend <- TRUE
    }

    dt <- data.table::as.data.table(x)
    if (!is.null(group)) {
      dt[, group := group]
    }
    if (!is.null(hovertext)) {
      dt[, hovertext := hovertext]
    }

    dt[, timeperiod := date2factor(time, time_bin)] |>
      setkey(timeperiod)

    Npertimeperiod <- dt[levels(timeperiod)][,
      lapply(.SD, \(i) length(na.exclude(i))),
      by = timeperiod
    ] |>
      setorder()

    ## Long data
    # appease R CMD check
    ID <- timeperiod <- NULL
    dtlong <- data.table::melt(
      dt[, ID := .I],
      id.vars = c(
        "ID",
        "timeperiod",
        mgetnames(dt, "group", "hovertext")
      )
    )

    if (is.null(group)) {
      .args <- list(
        data = dtlong,
        type = type,
        x = if (horizontal) ~value else ~timeperiod,
        y = if (horizontal) ~timeperiod else ~value,
        color = ~variable,
        colors = col2hex(col),
        showlegend = legend
      )
    } else {
      .args <- list(
        data = dtlong,
        type = type,
        x = if (horizontal) ~value else ~timeperiod,
        y = if (horizontal) ~timeperiod else ~value,
        color = ~group,
        colors = col2hex(col),
        showlegend = legend
      )
    }

    if (!is.null(hovertext)) {
      .args[["text"]] <- dtlong[["hovertext"]]
    }

    if (type == "box") {
      .args <- c(
        .args,
        list(
          quartilemethod = quartilemethod,
          boxpoints = boxpoints
        )
      )
    }
    if (type == "violin") {
      .args[["box"]] <- list(visible = violin_box)
    }

    .args <- c(list(width = width, height = height), .args)
    plt <- do.call(plotly::plot_ly, .args)
    if (!is.null(group) || nvars > 1) {
      plt <- plt |> plotly::layout(boxmode = "group")
    }

    # '-Annotate N ----
    if (is.null(group) && annotate_n) {
      Nperbox <- Npertimeperiod[[2]] # include zeros
      plt <- plt |>
        plotly::add_annotations(
          xref = "paper",
          yref = "paper",
          xanchor = "right",
          yanchor = "bottom",
          x = 0,
          y = annotate_n_y,
          text = "N =",
          font = list(
            family = theme[["font_family"]],
            size = font_size,
            color = annotate_col
          ),
          showarrow = FALSE
        ) |>
        plotly::add_annotations(
          xref = "x",
          yref = "paper",
          yanchor = "bottom",
          x = seq_along(Nperbox) - 1,
          y = 1,
          text = paste(Nperbox),
          font = list(
            family = theme[["font_family"]],
            size = font_size,
            color = annotate_col
          ),
          showarrow = FALSE
        )
    }
  } # /time-binned boxplots

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
    x = legend_xy[1],
    y = legend_xy[2],
    xanchor = legend_xanchor,
    yanchor = legend_yanchor,
    bgcolor = "#ffffff00",
    font = list(
      family = theme[["font_family"]],
      size = font_size,
      color = legend_col
    ),
    orientation = legend_orientation
  )

  yaxis_title <- if (horizontal) xlab else ylab
  plt <- plotly::layout(
    plt,
    yaxis = list(
      title = list(text = yaxis_title, standoff = ylab_standoff),
      type = if (horizontal) xaxis_type else NULL,
      titlefont = f,
      showgrid = if (horizontal) FALSE else theme[["grid"]],
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = if (horizontal) NA else tick_col,
      tickfont = tickfont,
      zeroline = FALSE,
      automargin = automargin_y,
      range = ylim
    ),
    xaxis = list(
      title = if (horizontal) ylab else xlab,
      type = if (horizontal) NULL else xaxis_type,
      titlefont = f,
      showgrid = if (horizontal) theme[["grid"]] else FALSE,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      tickcolor = if (horizontal) tick_col else NA,
      tickfont = tickfont,
      automargin = automargin_x,
      range = xlim
    ),
    title = list(
      text = main,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = main_col
      ),
      xref = "paper",
      x = theme[["main_adj"]]
    ),
    paper_bgcolor = bg,
    plot_bgcolor = plot_bg,
    margin = margin,
    legend = .legend,
    # boxgap = boxgap,
    boxgroupgap = boxgroupgap
  )

  # Config ----
  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    displayModeBar = displayModeBar,
    toImageButtonOptions = list(
      format = modeBar_file_format,
      width = file_width,
      height = file_height
    ),
    mathjax = mathjax
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
} # /rtemis::draw_box.R

# todo: htest.compare = n
