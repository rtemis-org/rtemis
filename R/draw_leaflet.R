# draw_leaflet.R
# ::rtemis::
# 2020 EDG rtemis.org

#' Plot interactive choropleth map using \pkg{leaflet}
#'
#' @param fips Character vector: FIPS codes. (If numeric, it will be appropriately zero-padded).
#' @param values Values to map to `fips`.
#' @param names Character vector: Optional county names to appear on hover along `values`.
#' @param fillOpacity Float: Opacity for fill colors.
#' @param palette Character: Color palette to use.
#' @param color_mapping Character: "Numeric" or "Bin".
#' @param col_lo Overlay color mapped to lowest value.
#' @param col_hi Overlay color mapped to highest value.
#' @param col_na Color mapped to NA values.
#' @param col_highlight Hover border color.
#' @param col_interpolate Character: "linear" or "spline".
#' @param col_bins Integer: Number of color bins to create if `color_mapping = "Bin"`.
#' @param domain Limits for mapping colors to values. Default = NULL and set to range.
#' @param weight Float: Weight of county border lines.
#' @param color Color of county border lines.
#' @param alpha Float: Overlay transparency.
#' @param bg_tile_provider Background tile (below overlay colors), one of `leaflet::providers`.
#' @param bg_tile_alpha Float: Background tile transparency.
#' @param fg_tile_provider Foreground tile (above overlay colors), one of `leaflet::providers`.
#' @param legend_position Character: One of: "topright", "bottomright", "bottomleft", "topleft".
#' @param legend_alpha Float: Legend box transparency.
#' @param legend_title Character: Defaults to name of `values` variable.
#' @param init_lng Float: Center map around this longitude (in decimal form). Default = -98.54180833333334 (US geographic center).
#' @param init_lat Float: Center map around this latitude (in decimal form). Default = 39.207413888888894 (US geographic center).
#' @param init_zoom Integer: Initial zoom level (depends on device, i.e. window, size).
#' @param stroke Logical: If TRUE, draw polygon borders.
#'
#' @return `leaflet` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' fips <- c(06075, 42101)
#' population <- c(874961, 1579000)
#' names <- c("SF", "Philly")
#' draw_leaflet(fips, population, names)
#' }
# NA in legend issue: https://github.com/rstudio/leaflet/issues/615
draw_leaflet <- function(
  fips,
  values,
  names = NULL,
  fillOpacity = 1,
  palette = NULL,
  color_mapping = c("Numeric", "Bin"),
  col_lo = "#0290EE",
  col_hi = "#FE4AA3",
  col_na = "#303030",
  col_highlight = "#FE8A4F",
  col_interpolate = c("linear", "spline"),
  col_bins = 21, # for color_mapping Bin
  domain = NULL,
  weight = .5,
  color = "black",
  alpha = 1,
  bg_tile_provider = leaflet::providers[["CartoDB.Positron"]],
  bg_tile_alpha = .67,
  fg_tile_provider = leaflet::providers[["CartoDB.PositronOnlyLabels"]],
  legend_position = c(
    "topright",
    "bottomright",
    "bottomleft",
    "topleft"
  ),
  legend_alpha = .8,
  legend_title = NULL,
  init_lng = -98.54180833333334,
  init_lat = 39.207413888888894,
  init_zoom = 3,
  stroke = TRUE
) {
  # Dependencies ----
  check_dependencies("leaflet", "geojsonio", "htmltools", "htmlwidgets", "sf")

  # Arguments ----
  vals_name <- deparse(substitute(values))
  color_mapping <- match.arg(color_mapping)
  col_interpolate <- match.arg(col_interpolate)
  if (is.null(palette)) {
    palette <- colorRamp(
      colors = c(col_lo, col_hi),
      interpolate = col_interpolate
    )
  }
  legend_position <- match.arg(legend_position)
  if (is.null(legend_title)) {
    legend_title <- labelify(vals_name)
  }

  # State vs. County data ----
  if (max(nchar(fips)) < 3) {
    geo <- readRDS(
      system.file(
        "extdata",
        "us-states.rds",
        package = "rtemis"
      )
    )
    fips <- if (is.character(fips)) {
      fips
    } else {
      sprintf("%02d", fips)
    }
  } else {
    geo <- readRDS(
      system.file(
        "extdata",
        "us-counties.rds",
        package = "rtemis"
      )
    )
    fips <- if (is.character(fips)) {
      fips
    } else {
      sprintf("%05d", fips)
    }
  }

  # Match input county-level data
  index <- match(geo[["id"]], fips)
  geo[["val"]] <- values[index]

  # Colorscale ----
  if (color_mapping == "Numeric") {
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = domain,
      na.color = col_na,
      alpha = TRUE
    )
  } else {
    pal <- leaflet::colorBin(
      palette = palette,
      domain = domain,
      na.color = col_na,
      bins = col_bins
    )
  }

  # Hover labels ----
  .labs <- values[index]
  if (!is.null(names)) {
    .names <- names[index]
    labels <- lapply(seq_len(NROW(geo)), function(i) {
      if (is.na(.labs[i])) {
        '<div style="color:#7f7f7f;">N/A</div>'
      } else {
        sprintf("<strong>%s</strong><br/>%g", .names[i], .labs[i])
      }
    }) |>
      lapply(htmltools::HTML)
  } else {
    labels <- lapply(seq_len(NROW(geo)), function(i) {
      if (is.na(.labs[i])) {
        '<div style="color:#7f7f7f;">N/A</div>'
      } else {
        sprintf("%g", .labs[i])
      }
    }) |>
      lapply(htmltools::HTML)
  }
  geo[["labels"]] <- labels[index]

  # leaflet map ----
  map <- leaflet::leaflet(geo) |>
    leaflet::addProviderTiles(
      provider = bg_tile_provider,
      options = leaflet::providerTileOptions(opacity = bg_tile_alpha)
    ) |>
    leaflet::addMapPane("polygons", zIndex = 410) |>
    leaflet::addMapPane("tiles", zIndex = 420) |>
    leaflet::addPolygons(
      fillColor = ~ pal(val),
      fillOpacity = fillOpacity,
      opacity = alpha,
      weight = weight,
      color = color,
      stroke = stroke,
      group = legend_title,
      options = leaflet::pathOptions(pane = "polygons"),
      highlight = leaflet::highlightOptions(
        weight = 2,
        color = col_highlight,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-weight" = "normal", padding = "2px 2px"),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    leaflet::addProviderTiles(
      provider = fg_tile_provider,
      options = leaflet::pathOptions(pane = "tiles")
    ) |>
    leaflet::addLegend(
      position = legend_position,
      pal = pal,
      values = geo[["val"]],
      opacity = legend_alpha,
      title = legend_title
    ) |>
    leaflet::addLayersControl(overlayGroups = c(legend_title)) |>
    leaflet::setView(lng = init_lng, lat = init_lat, zoom = init_zoom)

  insert <- htmltools::tags[["style"]](
    type = "text/css",
    "div.info.legend.leaflet-control br {clear: both;}"
  )
  map <- htmlwidgets::prependContent(map, insert)
  map
} # /rtemis:: draw_leaflet
