# draw_protein
# ::rtemis::
# 2022- EDG rtemis.org

#' Plot an amino acid sequence with annotations
#'
#' Plot an amino acid sequence with multiple site and/or region annotations.
#'
#' @param x Character vector: amino acid sequence (1-letter abbreviations) OR
#' `a3` object OR Character: path to JSON file OR Character: UniProt accession number.
#' @param site Named list of lists with indices of sites. These will be
#' highlighted by coloring the border of markers.
#' @param region Named list of lists with indices of regions. These will be
#' highlighted by coloring the markers and lines of regions using the
#' `palette` colors.
#' @param ptm List of post-translational modifications.
#' @param cleavage_site List of cleavage sites.
#' @param variant List of variant information.
#' @param disease_variants List of disease variant information.
#' @param n_per_row Integer: Number of amino acids to show per row.
#' @param main Character: Main title.
#' @param main_xy Numeric vector, length 2: x and y coordinates for title.
#' e.g. if `main_xref` and `main_yref` are `"paper"`:
#' `c(0.055, .975)` is top left, `c(.5, .975)` is top and
#' middle.
#' @param main_xref Character: xref for title.
#' @param main_yref Character: yref for title.
#' @param main_xanchor Character: xanchor for title.
#' @param main_yanchor Character: yanchor for title.
#' @param layout Character: "1curve", "grid": type of layout to use.
#' @param show_markers Logical: If TRUE, show amino acid markers.
#' @param show_labels Logical: If TRUE, annotate amino acids with elements.
#' @param font_size Integer: Font size for labels.
#' @param label_col Color for labels.
#' @param scatter_mode Character: Mode for scatter plot.
#' @param marker_size Integer: Size of markers.
#' @param marker_col Color for markers.
#' @param marker_alpha Numeric: Alpha for markers.
#' @param marker_symbol Character: Symbol for markers.
#' @param line_col Color for lines.
#' @param line_alpha Numeric: Alpha for lines.
#' @param line_width Numeric: Width for lines.
#' @param show_full_names Logical: If TRUE, show full names of amino acids.
#' @param region_scatter_mode Character: Mode for scatter plot.
#' @param region_style Integer: Style for regions.
#' @param region_marker_size Integer: Size of region markers.
#' @param region_marker_alpha Numeric: Alpha for region markers.
#' @param region_marker_symbol Character: Symbol for region markers.
#' @param region_line_dash Character: Dash for region lines.
#' @param region_line_shape Character: Shape for region lines.
#' @param region_line_smoothing Numeric: Smoothing for region lines.
#' @param region_line_width Numeric: Width for region lines.
#' @param region_line_alpha Numeric: Alpha for region lines.
#' @param theme Theme object.
#' @param region_palette Named list of colors for regions.
#' @param region_outline_only Logical: If TRUE, only show outline of regions.
#' @param region_outline_pad Numeric: Padding for region outline.
#' @param region_pad Numeric: Padding for region.
#' @param region_fill_alpha Numeric: Alpha for region fill.
#' @param region_fill_shape Character: Shape for region fill.
#' @param region_fill_smoothing Numeric: Smoothing for region fill.
#' @param bpadcx Numeric: Padding for region border.
#' @param bpadcy Numeric: Padding for region border.
#' @param site_marker_size Integer: Size of site markers.
#' @param site_marker_symbol Character: Symbol for site markers.
#' @param site_marker_alpha Numeric: Alpha for site markers.
#' @param site_border_width Numeric: Width for site borders.
#' @param site_palette Named list of colors for sites.
#' @param variant_col Color for variants.
#' @param disease_variant_col Color for disease variants.
#' @param showlegend_ptm Logical: If TRUE, show legend for PTMs.
#' @param ptm_col Named list of colors for PTMs.
#' @param ptm_symbol Character: Symbol for PTMs.
#' @param ptm_offset Numeric: Offset for PTMs.
#' @param ptm_pad Numeric: Padding for PTMs.
#' @param ptm_marker_size Integer: Size of PTM markers.
#' @param clv_col Color for cleavage site annotations.
#' @param clv_symbol Character: Symbol for cleavage site annotations.
#' @param clv_offset Numeric: Offset for cleavage site annotations.
#' @param clv_pad Numeric: Padding for cleavage site annotations.
#' @param clv_marker_size Integer: Size of cleavage site annotation markers.
#' @param annotate_position_every Integer: Annotate every nth position.
#' @param annotate_position_alpha Numeric: Alpha for position annotations.
#' @param annotate_position_ay Numeric: Y offset for position annotations.
#' @param position_font_size Integer: Font size for position annotations.
#' @param legend_xy Numeric vector, length 2: x and y coordinates for legend.
#' @param legend_xanchor Character: xanchor for legend.
#' @param legend_yanchor Character: yanchor for legend.
#' @param legend_orientation Character: Orientation for legend.
#' @param legend_col Color for legend.
#' @param legend_bg Color for legend background.
#' @param legend_border_col Color for legend border.
#' @param legend_borderwidth Numeric: Width for legend border.
#' @param legend_group_gap Numeric: Gap between legend groups.
#' @param margin List: Margin settings.
#' @param showgrid_x Logical: If TRUE, show x grid.
#' @param showgrid_y Logical: If TRUE, show y grid.
#' @param automargin_x Logical: If TRUE, use automatic margin for x axis.
#' @param automargin_y Logical: If TRUE, use automatic margin for y axis.
#' @param xaxis_autorange Logical: If TRUE, use automatic range for x axis.
#' @param yaxis_autorange Character: If TRUE, use automatic range for y axis.
#' @param scaleanchor_y Character: Scale anchor for y axis.
#' @param scaleratio_y Numeric: Scale ratio for y axis.
#' @param hoverlabel_align Character: Alignment for hover label.
#' @param displayModeBar Logical: If TRUE, display mode bar.
#' @param modeBar_file_format Character: File format for mode bar.
#' @param scrollZoom Logical: If TRUE, enable scroll zoom.
#' @param filename Character: File name to save plot.
#' @param file_width Integer: Width for saved file.
#' @param file_height Integer: Height for saved file.
#' @param file_scale Numeric: Scale for saved file.
#' @param width Integer: Width for plot.
#' @param height Integer: Height for plot.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#' @examples
#' \dontrun{
#' tau <- seqinr::read.fasta("https://rest.uniprot.org/uniprotkb/P10636.fasta",
#'   seqtype = "AA"
#' )
#' draw_protein(as.character(tau[[1]]))
#'
#' # or directly using the UniProt accession number:
#' draw_protein("P10636")
#' }
draw_protein <- function(
  x,
  site = NULL,
  region = NULL,
  ptm = NULL,
  cleavage_site = NULL,
  variant = NULL,
  disease_variants = NULL,
  # label_group = NULL,
  n_per_row = NULL,
  main = NULL,
  main_xy = c(0.055, .975),
  main_xref = "paper",
  main_yref = "paper",
  main_xanchor = "middle",
  main_yanchor = "top",
  layout = c("simple", "grid", "1curve", "2curve"),
  show_markers = TRUE,
  show_labels = TRUE,
  font_size = 18,
  label_col = NULL,
  scatter_mode = "markers+lines",
  # AA marker
  marker_size = 28,
  marker_col = NULL, # "gray18",
  marker_alpha = 1,
  marker_symbol = "circle",
  # AA line
  line_col = NULL, # "gray18",
  line_alpha = 1,
  line_width = 2,
  # Hover names
  show_full_names = TRUE,
  # regions
  region_scatter_mode = "markers+lines",
  region_style = 3,
  region_marker_size = marker_size,
  region_marker_alpha = .6,
  region_marker_symbol = "circle",
  region_line_dash = "solid",
  region_line_shape = "line",
  region_line_smoothing = 1,
  region_line_width = 1,
  region_line_alpha = .6,
  theme = choose_theme(),
  region_palette = rtemis_palette,
  region_outline_only = FALSE,
  region_outline_pad = 2, # for fake polys
  region_pad = .35, # for real polys
  region_fill_alpha = .1666666,
  region_fill_shape = "line",
  region_fill_smoothing = 1,
  bpadcx = .5,
  bpadcy = .5,
  # Sites - colored marker border
  site_marker_size = marker_size,
  site_marker_symbol = marker_symbol,
  site_marker_alpha = 1,
  site_border_width = 1.5,
  site_palette = rtemis_palette,
  # Variants
  variant_col = "#FA6E1E",
  # Text groups
  disease_variant_col = "#E266AE", # "#c982d7"
  # PTMs
  showlegend_ptm = TRUE,
  ptm_col = NULL,
  ptm_symbol = "circle",
  ptm_offset = .12,
  ptm_pad = .35,
  ptm_marker_size = marker_size / 4.5,
  # Cleavage sites
  clv_col = NULL,
  clv_symbol = "triangle-down",
  clv_offset = .12,
  clv_pad = .35,
  clv_marker_size = marker_size / 4,
  # Position annotations
  annotate_position_every = 10,
  annotate_position_alpha = .5,
  annotate_position_ay = -.4 * marker_size,
  position_font_size = font_size - 6,
  # Legend
  legend_xy = c(.97, .954),
  legend_xanchor = "left",
  legend_yanchor = "top",
  legend_orientation = "v",
  legend_col = NULL,
  legend_bg = "#FFFFFF00",
  legend_border_col = "#FFFFFF00",
  legend_borderwidth = 0,
  legend_group_gap = 0,
  margin = list(b = 0, l = 0, t = 0, r = 0, pad = 0),
  # Axes
  showgrid_x = FALSE,
  showgrid_y = FALSE,
  automargin_x = TRUE,
  automargin_y = TRUE,
  xaxis_autorange = TRUE,
  yaxis_autorange = "reversed",
  scaleanchor_y = "x",
  scaleratio_y = 1,
  # Layout
  hoverlabel_align = "left",
  # config
  displayModeBar = TRUE,
  modeBar_file_format = "svg",
  scrollZoom = TRUE,
  # file out
  filename = NULL,
  file_width = 1320,
  file_height = 990,
  file_scale = 1,
  width = NULL,
  height = NULL,
  verbosity = 1L
) {
  # Data ----
  if (inherits(x, "A3")) {
    dat <- x
    x <- dat[["sequence"]]
    site <- iflengthy(dat[["annotations"]][["site"]])
    region <- iflengthy(dat[["annotations"]][["region"]])
    ptm <- iflengthy(dat[["annotations"]][["ptm"]])
    cleavage_site <- iflengthy(dat[["annotations"]][["cleavage_site"]])
    variant <- iflengthy(dat[["annotations"]][["variant"]])
    disease_variants <- iflengthy(dat[["annotations"]][["site"]][[
      "Disease_associated_variant"
    ]])
  }
  if (length(x) == 1) {
    if (grepl(".json$", x)) {
      dat <- jsonlite::read_json(
        x,
        simplifyVector = TRUE,
        simplifyMatrix = FALSE
      )
      x <- dat[["Sequence"]]
      disease_variants <- dat[["annotations"]][["site"]][[
        "Disease_associated_variant"
      ]]
      site <- dat[["annotations"]][["site"]]
      region <- dat[["annotations"]][["region"]]
      ptm <- dat[["annotations"]][["ptm"]]
      cleavage_site <- dat[["annotations"]][["cleavage_site"]]
    } else {
      dat <- uniprot_get(x, verbosity = verbosity)
      x <- dat[["sequence"]]
      # if (is.null(main)) main <- dat[["identifier"]]
    }
  }
  x <- toupper(x)
  position <- seq_along(x)
  n <- length(x)
  if (is.null(n_per_row)) {
    n_per_row <- ceiling(sqrt(n))
  }

  # Arguments ----
  layout <- match.arg(layout)

  # Coordinates ----
  if (layout == "grid") {
    # '- grid ----
    # 1:n_per_row, n_per_row:1, till n
    xs <- rep(c(1:n_per_row, n_per_row:1), length.out = n)
    nrows <- ceiling(n / n_per_row)
    ys <- rep(1:nrows, each = n_per_row, length = n)
  } else if (layout == "1curve") {
    # '- 1curve ----
    xs <- rep(c(1:n_per_row, (n_per_row - 1):2), length.out = n)
    nrows <- ceiling(1 + (n / n_per_row - 1))
    ys <- c(
      1,
      rep(seq(1, nrows * 4, 3), each = n_per_row - 1, length = n - 1)
    )
    # drop the n_per_row, then n_per_row - 1
    ys[seq(n_per_row, n, n_per_row - 1)] <-
      ys[seq(n_per_row, n, n_per_row - 1)] + 1.5
  } else if (layout == "simple") {
    # '- simple ----
    # if each point is 1 unit apart, border points must be sqrt(3)/2 away
    xs <- rep(c(1:n_per_row, (n_per_row - 1):2), length.out = n)
    nrows <- ceiling(1 + (n / n_per_row))
    ys <- c(
      1,
      rep(seq(1, nrows), each = n_per_row - 1, length = n - 1)
    )
    # every n_per_row, move to .5 up and sqrt(3)/2 right, left from previous
    # Right border
    ys[seq(n_per_row, n, (2 * n_per_row - 2))] <-
      ys[seq(n_per_row, n, (2 * n_per_row - 2))] + .5
    xs[seq(n_per_row, n, (2 * n_per_row - 2))] <-
      xs[seq(n_per_row, n, 2 * n_per_row - 2)] - 1 + sqrt(3) / 2
    # Left border
    ys[seq((2 * n_per_row) - 1, n, (2 * n_per_row - 2))] <-
      ys[seq((2 * n_per_row) - 1, n, (2 * n_per_row - 2))] + .5
    xs[seq((2 * n_per_row) - 1, n, (2 * n_per_row - 2))] <-
      xs[seq((2 * n_per_row) - 1, n, (2 * n_per_row - 2))] + 1 - sqrt(3) / 2
  } else if (layout == "2curve") {
    # '- 2curve ----
    xs <- rep(c(1:n_per_row, n_per_row:1), length.out = n)
    nrows <- ceiling(n / n_per_row)
    ys <- rep(1:nrows * 3 - 2, each = n_per_row, length = n)
    ys[seq(n_per_row, n, n_per_row)] <-
      ys[seq(n_per_row, n, n_per_row)] + 1
    ys[seq(n_per_row, n, n_per_row) + 1] <-
      ys[seq(n_per_row, n, n_per_row)] + 1
  }
  # Theme ----
  check_is_S7(theme, Theme)

  if (is.null(label_col)) {
    label_col <- theme[["fg"]]
  }
  label_col <- recycle(label_col, x)
  if (is.null(marker_col)) {
    marker_col <- color_fade(theme[["fg"]], theme[["bg"]], .9)
  }
  marker_col <- plotly::toRGB(marker_col, alpha = marker_alpha)
  if (is.null(line_col)) {
    line_col <- color_fade(theme[["fg"]], theme[["bg"]], .9)
  }
  line_col <- plotly::toRGB(line_col, alpha = marker_alpha)

  main_col <- plotly::toRGB(theme[["main_col"]])
  labs_col <- plotly::toRGB(theme[["labs_col"]])
  if (is.null(legend_col)) {
    legend_col <- labs_col
  }
  grid_col <- plotly::toRGB(theme[["grid_col"]], theme[["grid_alpha"]])

  # Palette ----
  if (is.character(region_palette)) {
    region_palette <- rtpalette(region_palette)
  }
  if (is.character(site_palette)) {
    site_palette <- rtpalette(site_palette)
  }

  # Match abbreviations to full names ----
  if (show_full_names) {
    input <- switch(max(nchar(x)), "1" = "1", "3" = "3", "full")

    if (input == "full") {
      xnames <- x
    } else {
      if (input == "1") {
        xnames <- factor(
          x,
          levels = aa[["Abbreviation1"]],
          labels = aa[["Name"]]
        ) |>
          as.character()
      } else {
        xnames <- factor(
          x,
          levels = toupper(aa[["Abbreviation3"]]),
          labels = aa[["Name"]]
        ) |>
          as.character()
      }
    }
  } else {
    xnames <- x
  }

  # Variants: overwrite xnames with tooltip info
  if (!is.null(variant)) {
    for (i in seq_along(variant)) {
      varidi <- variant[[i]][["position"]]
      xnames[varidi] <- paste0(
        xnames[varidi],
        "\n\n",
        list2html(variant[[i]], col = variant_col)
      )
    }
  }

  # plotly ----
  plt <- plotly::plot_ly(
    width = width,
    height = height
  )

  # AA markers and lines ----
  aaname <- if (is.null(disease_variants)) {
    "1&#176; structure"
  } else {
    paste0(
      "1&#176; structure (",
      "<span style='color:",
      disease_variant_col,
      "'>Disease variants</span>)"
    )
  }
  if (show_markers) {
    clvtext <- if (!is.null(cleavage_site)) {
      # Get cleavage sites for each amino acid
      sapply(position, \(i) {
        if (i %in% unlist(cleavage_site)) {
          paste0(
            "\n<b><em>Cleavage site for:</em></b>\n",
            paste0(
              names(cleavage_site)[sapply(cleavage_site, \(x) i %in% x)],
              collapse = "\n"
            )
          )
        } else {
          ""
        }
      })
    } else {
      NULL
    }
    plt <- plt |>
      plotly::add_trace(
        x = xs,
        y = ys,
        type = "scatter",
        mode = scatter_mode,
        marker = list(
          color = plotly::toRGB(marker_col, alpha = marker_alpha),
          size = marker_size,
          symbol = marker_symbol
        ),
        line = list(
          color = plotly::toRGB(line_col, alpha = line_alpha),
          width = line_width
        ),
        text = paste0(position, ": ", xnames, clvtext),
        name = aaname,
        # hoverinfo = marker.hoverinfo
        hoverinfo = "text"
      )
  }
  # regions ----
  if (!is.null(region)) {
    region_names <- names(region)
    if (is.null(region_names)) {
      region_names <- paste("region", seq_along(region))
    }

    if (region_style == 1) {
      # '- region style 1 ----
      # for overlapping sets within each region
      for (i in seq_along(region)) {
        for (j in seq_along(region[[i]])) {
          plt <- plt |>
            plotly::add_trace(
              x = xs[region[[i]][[j]]],
              y = ys[region[[i]][[j]]],
              type = "scatter",
              mode = region_scatter_mode,
              marker = list(
                color = plotly::toRGB(
                  region_palette[[i]],
                  alpha = region_marker_alpha
                ),
                size = region_marker_size,
                symbol = region_marker_symbol
              ),
              line = list(
                color = plotly::toRGB(
                  region_palette[[i]],
                  alpha = region_line_alpha
                ),
                dash = region_line_dash,
                shape = region_line_shape,
                smoothing = region_line_smoothing,
                width = region_line_width
              ),
              name = region_names[i],
              legendgroup = region_names[i],
              showlegend = j == 1
            )
          if (region_outline_only) {
            # simulate rounded selection around AAs
            # need region_marker_size & line_width > marker_size
            plt <- plt |>
              plotly::add_trace(
                x = xs[region[[i]][[j]]],
                y = ys[region[[i]][[j]]],
                type = "scatter",
                mode = region_scatter_mode,
                marker = list(
                  color = plotly::toRGB(
                    # marker_col,
                    theme[["bg"]],
                    alpha = marker_alpha
                  ),
                  size = region_marker_size - region_outline_pad,
                  symbol = region_marker_symbol
                ),
                line = list(
                  color = plotly::toRGB(
                    # line_col,
                    theme[["bg"]],
                    alpha = line_alpha
                  ),
                  shape = region_line_shape,
                  smoothing = region_line_smoothing,
                  width = region_line_width - region_outline_pad
                ),
                name = NULL,
                legendgroup = region_names[i],
                showlegend = FALSE
              )
            plt <- plt |>
              plotly::add_trace(
                x = xs[region[[i]][[j]]],
                y = ys[region[[i]][[j]]],
                type = "scatter",
                mode = scatter_mode,
                marker = list(
                  color = plotly::toRGB(marker_col, alpha = marker_alpha),
                  size = marker_size,
                  symbol = marker_symbol
                ),
                line = list(
                  color = plotly::toRGB(line_col, alpha = line_alpha),
                  width = line_width
                ),
                name = NULL,
                legendgroup = region_names[i],
                showlegend = FALSE
              )
          }
        }
      }
    } else if (region_style == 2) {
      # '- region style 2 ----
      # for non-overlapping sets within each region
      for (i in seq_along(region)) {
        plt <- plt |>
          plotly::add_trace(
            x = xs[unlist(region[[i]])],
            y = ys[unlist(region[[i]])],
            type = "scatter",
            mode = "markers",
            marker = list(
              color = plotly::toRGB(
                region_palette[[i]],
                alpha = region_marker_alpha
              ),
              size = region_marker_size,
              symbol = region_marker_symbol
            ),
            name = region_names[i]
          )
      }
    } else {
      # '- region style 3 ----
      # for 1curve only
      # region polys: get marker direction and location:
      # left, leftborder, right, rightborder
      dl <- c(
        "r",
        rep(c("r", "l"), each = n_per_row - 1, length = n - 1)
      )
      dl[seq(n_per_row, n, n_per_row - 1)] <-
        paste0(dl[seq(n_per_row, n, n_per_row - 1)], "b")
      # i: IDI of region group
      for (i in seq_along(region)) {
        # each region's directions
        region_dl <- lapply(seq_along(region[[i]]), \(j) {
          dl[region[[i]][[j]]]
        })

        region_poly_xy <- lapply(seq_along(region[[i]]), \(j) {
          poly_xys(
            xs = xs[region[[i]][[j]]],
            ys = ys[region[[i]][[j]]],
            d = region_dl[[j]],
            pad = region_pad,
            bpadcx = bpadcx,
            bpadcy = bpadcy
          )
        })

        for (j in seq_along(region[[i]])) {
          plt <- plt |>
            plotly::add_polygons(
              x = region_poly_xy[[j]][["px"]],
              y = region_poly_xy[[j]][["py"]],
              line = list(
                color = region_palette[[i]],
                width = region_line_width,
                shape = region_fill_shape,
                smoothing = region_fill_smoothing
              ),
              fillcolor = plotly::toRGB(
                region_palette[[i]],
                alpha = region_fill_alpha
              ),
              name = region_names[i],
              legendgroup = region_names[i],
              showlegend = j == 1
            )
        }
      } # each region's individual regions' coords
    }
  } # /regions

  # Sites ----
  if (!is.null(site)) {
    site_names <- names(site)
    if (is.null(site_names)) {
      site_names <- paste("Site", seq_along(site))
    }
    # for overlapping sets within each region
    for (i in seq_along(site)) {
      for (j in seq_along(site[[i]])) {
        plt <- plt |>
          plotly::add_trace(
            x = xs[site[[i]][[j]]],
            y = ys[site[[i]][[j]]],
            type = "scatter",
            mode = "markers",
            marker = list(
              color = plotly::toRGB(
                "#000000",
                alpha = 0
              ),
              size = site_marker_size,
              symbol = site_marker_symbol,
              line = list(
                color = plotly::toRGB(
                  site_palette[[i]],
                  alpha = site_marker_alpha
                ),
                width = site_border_width
              )
            ),
            name = site_names[i],
            legendgroup = site_names[i],
            showlegend = j == 1
          )
      }
    }
  } # /sites

  # PTMs ----
  # Note: Do not show both PTMs and cleavage sites using the same padding
  if (!is.null(ptm)) {
    if (verbosity > 1L) {
      msg2("Adding PTM markers...")
    }
    if (is.null(ptm_col)) {
      ptm_col <- 1 + seq_along(ptm)
    }
    ptm_symbol <- recycle(ptm_symbol, ptm)
    ptm_names <- names(ptm)
    for (i in seq_along(ptm)) {
      polyoffset <- npad(i, n = length(ptm), pad = ptm_pad)
      plt <- plt |>
        plotly::add_trace(
          x = xs[ptm[[i]]] + polyoffset[1],
          y = ys[ptm[[i]]] + polyoffset[2],
          type = "scatter",
          mode = "markers",
          marker = list(
            color = plotly::toRGB(ptm_col[[i]]),
            size = ptm_marker_size,
            symbol = ptm_symbol[i]
          ),
          name = ptm_names[i],
          showlegend = showlegend_ptm
        )
    }
  }
  # Cleavage sites ----
  # Note: Do not show both PTMs and cleavage sites using the same padding
  if (!is.null(cleavage_site)) {
    if (verbosity > 1L) {
      msg2("Adding cleavage site markers...")
    }
    if (is.null(clv_col)) {
      clv_col <- c(
        colorspace::qualitative_hcl(
          (length(cleavage_site)),
          h = c(40, 360),
          c = 120,
          l = 50
        )
      )
    }
    clv_symbol <- recycle(clv_symbol, cleavage_site)
    clv_names <- names(cleavage_site)
    for (i in seq_along(cleavage_site)) {
      polyoffset <- npad(i, n = length(cleavage_site), pad = clv_pad)
      plt <- plt |>
        plotly::add_trace(
          x = xs[cleavage_site[[i]]] + polyoffset[1],
          y = ys[cleavage_site[[i]]] + polyoffset[2],
          type = "scatter",
          mode = "markers",
          marker = list(
            color = plotly::toRGB(clv_col[[i]]),
            size = clv_marker_size,
            symbol = clv_symbol[i]
          ),
          name = clv_names[i],
          showlegend = showlegend_ptm
        )
    }
  }

  # AA labels ----
  if (show_labels) {
    # Variants
    if (!is.null(variant)) {
      variant_idi <- sapply(variant, \(v) v[["position"]])
      label_col[variant_idi] <- variant_col
    }
    # Disease variants
    if (!is.null(disease_variants)) {
      label_col[disease_variants] <- disease_variant_col
    }
    label_group <- factor(label_col)
    label_group_col <- levels(label_group)
    for (i in seq_along(label_group_col)) {
      idx <- label_group == label_group_col[i]
      plt <- plt |>
        plotly::add_annotations(
          xref = "x",
          yref = "y",
          x = xs[idx],
          y = ys[idx],
          text = x[idx],
          font = list(
            family = theme[["font_family"]],
            size = font_size,
            color = label_group_col[[i]]
          ),
          showarrow = FALSE
          # name = label_group.levels[[i]],
          # showlegend = nchar(label_group.levels[[i]]) > 0
        )
    }
    # }
  }

  # Position annotations ----
  if (
    !is.null(annotate_position_every) && length(x) > annotate_position_every
  ) {
    idxpos <- seq(annotate_position_every, n, annotate_position_every)
    plt <- plt |>
      plotly::add_annotations(
        x = xs[idxpos],
        y = ys[idxpos],
        xref = "x",
        yref = "y",
        xanchor = "middle",
        yanchor = "bottom",
        ax = 0,
        ay = annotate_position_ay,
        text = idxpos,
        showarrow = TRUE,
        arrowcolor = "#ffffff00",
        font = list(
          size = position_font_size,
          family = theme[["font_family"]],
          color = plotly::toRGB(theme[["fg"]], alpha = annotate_position_alpha)
        )
      )
  }

  # Layout ----
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
    xaxis = list(
      autorange = xaxis_autorange,
      showgrid = showgrid_x,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      zeroline = FALSE,
      showticklabels = FALSE,
      automargin = automargin_x
    ),
    yaxis = list(
      autorange = yaxis_autorange,
      showgrid = showgrid_y,
      gridcolor = grid_col,
      gridwidth = theme[["grid_lwd"]],
      zeroline = FALSE,
      showticklabels = FALSE,
      automargin = automargin_y,
      scaleanchor = scaleanchor_y,
      scaleratio = scaleratio_y
    ),
    title = list(
      text = main,
      font = list(
        family = theme[["font_family"]],
        size = font_size,
        color = main_col
      ),
      xref = main_xref,
      yref = main_yref,
      xanchor = main_xanchor,
      yanchor = main_yanchor,
      x = main_xy[1],
      y = main_xy[2]
    ),
    paper_bgcolor = theme[["bg"]],
    plot_bgcolor = theme[["plot_bg"]],
    margin = margin,
    legend = .legend,
    hoverlabel = list(
      align = "hoverlabel_align"
    )
  )

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
    scrollZoom = TRUE
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

  return(plt)
} # rtemis::draw_protein

aa <- data.frame(
  Abbreviation1 = c(
    "A",
    "R",
    "N",
    "D",
    "C",
    "Q",
    "E",
    "G",
    "H",
    "I",
    "L",
    "K",
    "M",
    "F",
    "P",
    "S",
    "T",
    "W",
    "Y",
    "V",
    "B",
    "Z",
    "X",
    ""
  ),
  Abbreviation3 = c(
    "Ala",
    "Arg",
    "Asn",
    "Asp",
    "Cys",
    "Gln",
    "Glu",
    "Gly",
    "His",
    "Ile",
    "Leu",
    "Lys",
    "Met",
    "Phe",
    "Pro",
    "Ser",
    "Thr",
    "Trp",
    "Tyr",
    "Val",
    "Asx",
    "Glx",
    "Xaa",
    "TERM"
  ),
  Name = c(
    "Alanine",
    "Arginine",
    "Asparagine",
    "Aspartate",
    "Cysteine",
    "Glutamine",
    "Glutamate",
    "Glycine",
    "Histidine",
    "Isoleucine",
    "Leucine",
    "Lysine",
    "Methionine",
    "Phenylalanine",
    "Proline",
    "Serine",
    "Threonine",
    "Tryptophan",
    "Tyrosine",
    "Valine",
    "Aspartic acid or Asparagine",
    "Glutamine or Glutamic acid",
    "(Any)",
    "Termination codon"
  )
)

poly_xys <- function(xs, ys, d, pad = 1, bpadcx = .5, bpadcy = .5) {
  n <- length(xs)
  dk <- rep(1, n)
  kinks <- which("rb" == d | "lb" == d)
  for (i in kinks) {
    if ((i + 1) <= n) {
      dk[(i + 1):n] <- -dk[(i + 1):n]
    }
  }

  # première ----
  px_1 <- switch(
    d[1],
    "r" = xs[1] - pad,
    "l" = xs[1] + pad,
    "rb" = c(xs[1] - pad, xs[1]),
    "lb" = c(xs[1] + pad, xs[1])
  )
  py_1 <- switch(
    d[1],
    "rb" = rep(ys[1] - pad, 2),
    "lb" = rep(ys[1] - pad, 2),
    ys[1] - pad
  )

  # aller ----
  # k: IDI of individual amino acid within individual region
  px_aller <-
    sapply(seq_along(d), \(k) {
      if (d[k] == "rb") {
        # rep(xs[k] + sqrt(.5 * pad^2), 2)
        rep(xs[k] + pad, 2)
      } else if (d[k] == "lb") {
        # rep(xs[k] - sqrt(.5 * pad^2), 2)
        rep(xs[k] - pad, 2)
      } else {
        xs[k]
      }
    }) |>
    unlist()

  py_aller <-
    sapply(seq_along(d), \(k) {
      if (d[[k]] %in% c("l", "r")) {
        if (dk[k] == -1) {
          ys[k] + pad
        } else {
          ys[k] - pad
        }
      } else {
        if (k == 1) {
          c(ys[k] - pad, ys[k] + sqrt(.5 * pad^2))
        } else if (k == length(d)) {
          c(ys[k] - sqrt(.5 * pad^2), ys[k] + pad)
        } else {
          c(ys[k] - sqrt(.5 * pad^2), ys[k] + sqrt(.5 * pad^2))
        }
      }
    }) |>
    unlist()

  # centre ----
  dr <- rev(d)
  dkr <- rev(dk)
  xsr <- rev(xs)
  ysr <- rev(ys)
  px_centre <-
    switch(
      dr[1],
      "r" = rep(xsr[1] + pad, 2),
      "l" = rep(xsr[1] - pad, 2),
      # "rb" = c(xsr[1], xsr[1] - sqrt(.5 * pad^2)),
      # "lb" = c(xsr[1], xsr[1] + sqrt(.5 * pad^2))
      "rb" = c(xsr[1], xsr[1] - pad),
      "lb" = c(xsr[1], xsr[1] + pad)
    )

  py_centre <-
    if (dr[1] %in% c("r", "l")) {
      if (length(kinks) > 0) {
        c(ysr[1] + pad, ysr[1] - pad)
      } else {
        c(ysr[1] - pad, ysr[1] + pad)
      }
    } else {
      rep(ysr[1] + pad, 2)
    }

  # retour ----
  px_retour <-
    sapply(seq_along(dr), \(k) {
      if (dr[k] == "rb") {
        if (k == 1 | k == length(dr)) {
          rep(xsr[k] - pad, 2)
        } else {
          rep(xsr[k] - 1.5 * sqrt(.5 * pad^2), 2)
        }
      } else if (dr[k] == "lb") {
        if (k == 1 | k == length(dr)) {
          rep(xsr[k] + pad, 2)
        } else {
          rep(xsr[k] + 1.5 * sqrt(.5 * pad^2), 2)
        }
      } else {
        xsr[k]
      }
    }) |>
    unlist()

  py_retour <-
    sapply(seq_along(dr), \(k) {
      if (dr[[k]] %in% c("l", "r")) {
        if (dkr[k] == -1) {
          ysr[k] - pad
        } else {
          ysr[k] + pad
        }
      } else {
        rep(ysr[k], 2)
      }
    }) |>
    unlist()

  # find point before and after rb/lb
  idirb <- which(d == "rb")
  if (length(idirb) > 0) {
    if (idirb > 1) {
      px_aller[idirb - 1] <- px_aller[idirb - 1] + sqrt(.5 * pad^2)
    }
    if ((idirb + 1) <= length(d)) {
      px_aller[idirb + 2] <- px_aller[idirb + 2] + sqrt(.5 * pad^2)
    }
  }

  idilb <- which(d == "lb")
  if (length(idilb) > 0) {
    if (idilb > 1) {
      px_aller[idilb - 1] <- px_aller[idilb - 1] - sqrt(.5 * pad^2)
    }
    if ((idilb + 1) <= length(d)) {
      px_aller[idilb + 2] <- px_aller[idilb + 2] - sqrt(.5 * pad^2)
    }
  }

  # pénultième ----
  py_pen <- if (d[1] %in% c("rb", "lb")) {
    ys[1] - sqrt(.5 * pad^2)
  } else {
    ys[1] + pad
  }

  # out ----
  list(
    px = c(px_1, px_aller, px_centre, px_retour, px_1[1], px_1[1]),
    py = c(py_1, py_aller, py_centre, py_retour, py_pen, py_1[1])
  )
}


qrtpad <- function(i, pad = .3) {
  qrt <- sqrt(.5 * pad^2)
  switch(
    i,
    `1` = c(qrt, -qrt),
    `2` = c(pad, 0),
    `3` = c(qrt, qrt),
    `4` = c(0, pad),
    `5` = c(-qrt, qrt),
    `6` = c(-pad, 0),
    `7` = c(-qrt, -qrt)
  )
}

# plot(x = 0:2, y = 0:2, pch = 19)
# for (i in 1:7) {
#   pd <- qrtpad(i)
#   points(x = 1 + pd[1], y = 1 + pd[2], pch = 2, col = "red")
# }
# sapply(1:7, qrtpad)

# npad: function to calculate circular offset of a point from the center of a region
# by dividing circle into n equal parts, beginning from the top
npad <- function(i, n = 12, pad = .3) {
  angle <- 2 * pi / n
  x <- sin(angle * i) * pad
  y <- cos(angle * i) * pad
  c(x, y)
}
