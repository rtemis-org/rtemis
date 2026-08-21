# test_draw.R
# ::rtemis::
# 2025- EDG rtemis.org

# draw_3Dscatter ----
test_that("draw_3Dscatter creates a plotly object and saves file", {
  # Check whether plotly and kaleido are available in reticulate
  temp_dir <- withr::local_tempdir()
  if (
    !requireNamespace("reticulate", quietly = TRUE) ||
      !reticulate::py_module_available("plotly") ||
      !reticulate::py_module_available("kaleido")
  ) {
    temp_file <- NULL
  } else {
    temp_file <- file.path(temp_dir, "draw_3Dscatter.pdf")
  }

  # Create the plot with file output
  p <- draw_3Dscatter(
    iris,
    group = iris$Species,
    theme = theme_darkgraygrid(),
    filename = temp_file
  )

  # Test that plotly object is created
  expect_s3_class(p, "plotly")

  # Test that file was successfully created by plotly/kaleido (only if temp_file is not NULL)
  if (!is.null(temp_file)) {
    expect_true(file.exists(temp_file))

    # Test that the file has content (not empty)
    file_info <- file.info(temp_file)
    expect_true(file_info$size > 0)

    # Test that it's a valid PDF file (starts with PDF header)
    file_content <- readBin(temp_file, "raw", n = 4)
    expect_equal(rawToChar(file_content), "%PDF")
  }
})

# draw_bar ----
test_that("draw_bar creates a plotly object", {
  p <- draw_bar(VADeaths, legend_xy = c(0, 1))
  expect_s3_class(p, "plotly")
})

# draw_box ----
test_that("draw_box creates a plotly object", {
  p <- draw_box(iris[, 1:4], group = iris[["Species"]], annotate_n = TRUE)
  expect_s3_class(p, "plotly")
})

# draw_calibration ----
test_that("draw_calibration creates a plotly object", {
  # Create a simple binary classification example
  set.seed(123)
  true_labels <- factor(sample(c("A", "B"), size = 100, replace = TRUE))
  predicted_prob <- runif(100)
  p <- draw_calibration(true_labels, predicted_prob)
  expect_s3_class(p, "plotly")
})

# draw_confusion ----
test_that("draw_confusion creates a plotly object", {
  true_labels <- factor(c("a", "a", "a", "b", "b", "b", "b", "b", "b", "b"))
  predicted_labels <- factor(c(
    "a",
    "b",
    "a",
    "b",
    "b",
    "a",
    "b",
    "b",
    "b",
    "a"
  ))
  predicted_prob <- c(0.3, 0.55, 0.45, 0.75, 0.57, 0.3, 0.8, 0.63, 0.62, 0.39)
  metrics <- classification_metrics(
    true_labels,
    predicted_labels,
    predicted_prob
  )
  p <- draw_confusion(metrics)
  expect_s3_class(p, "plotly")
})

# draw_dist ----
test_that("draw_dist creates a plotly object", {
  p <- draw_dist(iris[["Sepal.Length"]], group = iris[["Species"]])
  expect_s3_class(p, "plotly")
})

# draw_heatmap ----
test_that("draw_heatmap creates a plotly object", {
  x <- rnormmat(200, 20)
  xcor <- cor(x)
  p <- draw_heatmap(xcor)
  expect_s3_class(p, "plotly")
})

# draw_leaflet ----
test_that("draw_leaflet creates a leaflet object", {
  fips <- c(06075, 42101)
  population <- c(874961, 1579000)
  names <- c("SF", "Philly")
  p <- draw_leaflet(fips, population, names)
  expect_s3_class(p, "leaflet")
})

# draw_pie ----
test_that("draw_pie creates a plotly object", {
  p <- draw_pie(VADeaths[, 1, drop = FALSE])
  expect_s3_class(p, "plotly")
})

# draw_protein ----
test_that("draw_protein creates a plotly object", {
  tau <- c(
    "M",
    "A",
    "E",
    "P",
    "R",
    "Q",
    "E",
    "F",
    "E",
    "V",
    "M",
    "E",
    "D",
    "H",
    "A",
    "G",
    "T",
    "Y",
    "G",
    "L"
  )
  p <- draw_protein(tau)
  expect_s3_class(p, "plotly")
})

# draw_pvals ----
test_that("draw_pvals creates a plotly object", {
  p <- draw_pvals(
    c(0.01, 0.02, 0.03),
    xnames = c("Feature1", "Feature2", "Feature3")
  )
  expect_s3_class(p, "plotly")
})

# draw_scatter ----
test_that("draw_scatter creates a plotly object", {
  p <- draw_scatter(
    iris[["Sepal.Length"]],
    iris[["Petal.Length"]],
    group = iris[["Species"]],
    fit = "gam",
    se_fit = TRUE
  )
  expect_s3_class(p, "plotly")
})

# draw_spectrogram ----
test_that("draw_spectrogram creates a plotly object", {
  time <- seq(0, 1, length.out = 100)
  freq <- seq(1, 100, length.out = 100)
  power <- outer(time, freq, function(t, f) sin(t) * cos(f))
  p <- draw_spectrogram(
    x = time,
    y = freq,
    z = power
  )
  expect_s3_class(p, "plotly")
})

# draw_survfit ----
test_that("draw_survfit creates a plotly object", {
  data(cancer, package = "survival")
  sf2 <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung)
  p <- draw_survfit(sf2)
  expect_s3_class(p, "plotly")
})

# draw_table ----
test_that("draw_table creates a plotly object", {
  df <- data.frame(
    Name = c("Alice", "Bob", "Charlie"),
    Age = c(25, 30, 35),
    Score = c(90.5, 85.0, 88.0)
  )
  p <- draw_table(
    df,
    main = "Sample Table",
    main_col = "#00b2b2"
  )
  expect_s3_class(p, "plotly")
})

# draw_ts ----
test_that("draw_ts creates a plotly object", {
  time1 <- sample(seq(
    as.Date("2020-03-01"),
    as.Date("2020-07-23"),
    length.out = 100
  ))
  time2 <- sample(seq(
    as.Date("2020-05-01"),
    as.Date("2020-09-23"),
    length.out = 140
  ))
  time <- c(time1, time2)
  x <- c(rnorm(100), rnorm(140, 1, 1.5))
  group <- c(rep("Alpha", 100), rep("Beta", 140))
  p <- draw_ts(x, time, 7, group)
  expect_s3_class(p, "plotly")
})

# draw_varimp ----
test_that("draw_varimp creates a plotly object", {
  x <- rnorm(10)
  names(x) <- paste0("Feature_", seq(x))
  p <- draw_varimp(x)
  expect_s3_class(p, "plotly")
  p_h <- draw_varimp(x, orientation = "h")
  expect_s3_class(p_h, "plotly")
})

# draw_volcano ----
test_that("draw_volcano creates a plotly object", {
  set.seed(2019)
  x <- rnorm(100, mean = 0.5, sd = 2)
  pvals <- runif(100, min = 0, max = 0.1)
  p <- draw_volcano(x, pvals)
  expect_s3_class(p, "plotly")
})

# draw_xt ----
test_that("draw_xt creates a plotly object", {
  datetime <- seq(
    as.POSIXct("2020-01-01 00:00"),
    as.POSIXct("2020-01-02 00:00"),
    by = "hour"
  )
  df <- data.frame(
    datetime = datetime,
    value1 = rnorm(length(datetime)),
    value2 = rnorm(length(datetime))
  )
  p <- draw_xt(df, x = df[, 1], y = df[, 2:3])
  expect_s3_class(p, "plotly")
})


# draw_linad ----
# The coefficient tables are the reason this plot exists: a LINAD leaf's
# coefficients are the accumulated sum along its path, so a feature can carry a
# different slope -- and a different sign -- in each region. These check that
# the tables are present, are tables, and are colored on a scale that makes a
# sign change visible.

.linad_regression <- function(n = 300L) {
  set.seed(9)
  x <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    g = factor(sample(c("p", "q"), n, replace = TRUE))
  )
  # Opposite-signed slopes either side of the b split.
  x[["y"]] <- ifelse(x[["b"]] < 0, 3 * x[["a"]], -3 * x[["a"]]) +
    ifelse(x[["g"]] == "p", 2, -2) +
    20 +
    rnorm(n, sd = 0.5)
  x
}

test_that("draw_linad creates a visNetwork object", {
  skip_if_not_installed("visNetwork")
  dat <- .linad_regression()
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = 5L, force_max_leaves = TRUE),
    verbosity = 0L
  )
  p <- draw_linad(mod, verbosity = 0L)
  expect_s3_class(p, "visNetwork")
  nodes <- p[["x"]][["nodes"]]
  edges <- p[["x"]][["edges"]]
  # A tree has one edge per node bar the root.
  expect_identical(nrow(edges), nrow(nodes) - 1L)
  # Every node carries a table, not a line of text.
  expect_true(all(grepl("<table", nodes[["title"]], fixed = TRUE)))
  # And the node's own value, which is what the tree alone predicts there.
  expect_true(all(grepl("Node value", nodes[["title"]], fixed = TRUE)))
  # It is the table's headline number, not a line of running text: label and
  # value are separate elements, and both sit outside the scrolling element so
  # they stay visible while a wide model's rows are scrolled.
  head <- sub("<div style=\"max-height.*$", "", nodes[["title"]])
  expect_true(all(grepl("Node value", head, fixed = TRUE)))
  values <- ddSci(
    mod@model@frame[["node_value"]][as.integer(nodes[["id"]])],
    4L
  )
  expect_true(all(mapply(grepl, values, head, MoreArgs = list(fixed = TRUE))))
})


test_that("draw_linad draws a classification, and a smaller tree on request", {
  skip_if_not_installed("visNetwork")
  dat <- .linad_regression()
  dat[["y"]] <- factor(
    ifelse(dat[["y"]] > stats::median(dat[["y"]]), "hi", "lo"),
    levels = c("lo", "hi")
  )
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = 5L, force_max_leaves = TRUE),
    verbosity = 0L
  )
  expect_s3_class(draw_linad(mod, verbosity = 0L), "visNetwork")
  # `is_leaf` describes the size selected at training, so a smaller size has to
  # be read from `steps` instead; drawing 2 leaves must give 3 nodes, not 9.
  small <- draw_linad(mod, n_leaves = 2L, verbosity = 0L)
  expect_identical(nrow(small[["x"]][["nodes"]]), 3L)
  expect_error(draw_linad(mod, n_leaves = 99L), class = "rtemis_range_error")
})


test_that("draw_linad's color scale is centered on zero", {
  # The property the whole design rests on: a coefficient and its negation must
  # land at opposite ends of the scale, so a sign flip between nodes reads as a
  # color flip rather than as a number to be parsed.
  colors <- rtemis:::linad_diverging(
    c(-2, -1, 0, 1, 2),
    lo_col = "#0290EE",
    mid_col = "#1A1A1A",
    hi_col = "#FE4AA3"
  )
  expect_length(colors, 5L)
  expect_identical(colors[[1L]], "#0290EE")
  expect_identical(colors[[5L]], "#FE4AA3")
  # Zero sits at the midpoint, and equal magnitudes sit symmetrically about it.
  expect_identical(colors[[3L]], "#1A1A1A")
  expect_false(identical(colors[[2L]], colors[[4L]]))
  # A shifted set of values does not move where zero sits.
  shifted <- rtemis:::linad_diverging(
    c(0, 1, 2, 3, 4),
    lo_col = "#0290EE",
    mid_col = "#1A1A1A",
    hi_col = "#FE4AA3"
  )
  expect_identical(shifted[[1L]], "#1A1A1A")
  # All-zero input cannot divide by zero; it returns the midpoint throughout.
  expect_identical(
    unique(rtemis:::linad_diverging(c(0, 0), "#0290EE", "#1A1A1A", "#FE4AA3")),
    "#1A1A1A"
  )
})


test_that("draw_linad's top and sort_coefs shorten and order the tables", {
  skip_if_not_installed("visNetwork")
  dat <- .linad_regression()
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(
      max_leaves = 4L,
      node_model = "ridge",
      force_max_leaves = TRUE
    ),
    verbosity = 0L
  )
  full <- draw_linad(mod, verbosity = 0L)[["x"]][["nodes"]][["title"]]
  topped <- draw_linad(mod, top = 1L, verbosity = 0L)[["x"]][["nodes"]][[
    "title"
  ]]
  rows <- function(html) lengths(regmatches(html, gregexpr("<tr>", html)))
  # The column header plus `top` slopes. The caption is not a row: it sits
  # outside the table so it survives scrolling.
  expect_true(all(rows(topped) < rows(full)))
  expect_true(all(rows(topped) == 2L))
})


test_that("draw_linad's tables carry selected slopes, not the intercept", {
  skip_if_not_installed("visNetwork")
  dat <- .linad_regression()
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(
      max_leaves = 4L,
      node_model = "forward",
      nvmax = 1L,
      force_max_leaves = TRUE
    ),
    verbosity = 0L
  )
  tips <- draw_linad(mod, verbosity = 0L)[["x"]][["nodes"]][["title"]]
  # Column 1 of `coefficients` is the model's value at x = 0, not a fitted
  # intercept -- the node models are fitted without one. It is a long
  # extrapolation and does not belong in a table meant to be interpreted.
  expect_false(any(grepl("(Int)", tips, fixed = TRUE)))
  # The level is carried by the caption instead.
  expect_true(all(grepl("Node value", tips, fixed = TRUE)))
  # Forward selection zeroes most coefficients; a zero says the node's model did
  # not select the feature, so padding the table with them says nothing.
  expect_false(any(grepl(">0.000<", tips, fixed = TRUE)))
  # And the table scrolls rather than growing past the screen on a wide model.
  expect_true(all(grepl("overflow-y: auto", tips, fixed = TRUE)))
})


test_that("draw_linad says so when a node has no linear terms", {
  skip_if_not_installed("visNetwork")
  dat <- .linad_regression()
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(
      max_leaves = 3L,
      node_model = "constant",
      force_max_leaves = TRUE
    ),
    verbosity = 0L
  )
  tips <- draw_linad(mod, verbosity = 0L)[["x"]][["nodes"]][["title"]]
  # With constant nodes the node values are the whole model, so an empty table
  # is correct -- but it has to say that rather than render as a blank box.
  expect_true(all(grepl("no linear terms", tips, fixed = TRUE)))
  expect_true(all(grepl("Node value", tips, fixed = TRUE)))
})


test_that("draw_linad places nodes so that none can overlap", {
  # vis.js's own hierarchical layout does not guarantee this: its `nodeSpacing`
  # applies to a nominal node size rather than the rendered box, so a node made
  # wide by its label overlaps its siblings anyway. Measured in a browser at 12
  # overlapping pairs on a 23-node tree, unchanged by disabling the layout's
  # optimizations and barely moved by raising nodeSpacing to 500. The
  # coordinates are therefore computed here, and this is the guarantee.
  skip_if_not_installed("visNetwork")
  set.seed(3)
  n <- 200L
  # Long feature names are what make the boxes wide enough to collide.
  features <- c(
    "GMD_Temporal_Sup_L",
    "GMD_Temporal_Pole_Mid_L",
    "GMD_Rolandic_Oper_L",
    "GMD_Occipital_Sup_L"
  )
  dat <- as.data.frame(stats::setNames(
    lapply(features, function(i) runif(n)),
    features
  ))
  dat[["y"]] <- 63 + 0.3 * dat[[1L]] - 0.2 * dat[[2L]] + rnorm(n, sd = 0.1)
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = 10L, force_max_leaves = TRUE),
    verbosity = 0L
  )
  node_width <- 180L
  node_gap <- 24L
  font_size <- 14L
  p <- draw_linad(
    mod,
    node_width = node_width,
    node_gap = node_gap,
    font_size = font_size,
    verbosity = 0L
  )
  nodes <- p[["x"]][["nodes"]]
  expect_true(all(c("x", "y") %in% names(nodes)))
  # Separation is per pair, so the check is per pair: two neighbors must be at
  # least their two half-widths plus the gap apart.
  widths <- rtemis:::linad_label_width(nodes[["label"]], font_size, node_width)
  for (level in unique(nodes[["y"]])) {
    at_level <- which(nodes[["y"]] == level)
    ordered <- at_level[order(nodes[["x"]][at_level])]
    for (i in seq_along(ordered)[-1L]) {
      left <- ordered[[i - 1L]]
      right <- ordered[[i]]
      expect_gte(
        nodes[["x"]][[right]] - nodes[["x"]][[left]],
        (widths[[left]] + widths[[right]]) / 2 + node_gap - 1e-8
      )
    }
  }
})


test_that("linad_layout centers parents and separates every level", {
  frame <- data.table::data.table(
    node = 1:5,
    parent = c(NA, 1L, 1L, 2L, 2L),
    left = c(2L, 4L, NA, NA, NA),
    right = c(3L, 5L, NA, NA, NA),
    depth = c(0L, 1L, 1L, 2L, 2L)
  )
  out <- rtemis:::linad_layout(
    frame,
    1:5,
    widths = rep(80, 5L),
    gap = 20,
    level_separation = 50
  )
  expect_length(out[["x"]], 5L)
  # Depth drives the other axis.
  expect_identical(out[["y"]], c(0, 50, 50, 100, 100))
  # No two nodes at a depth are closer than their widths plus the gap.
  for (level in unique(out[["y"]])) {
    at_level <- sort(out[["x"]][out[["y"]] == level])
    if (length(at_level) > 1L) {
      expect_true(all(diff(at_level) >= 80 + 20 - 1e-8))
    }
  }
  # Node 2 has two children and sits centered between them.
  expect_equal(out[["x"]][[2L]], mean(out[["x"]][c(4L, 5L)]))
  # Direction swaps the axes rather than recomputing anything.
  sideways <- rtemis:::linad_layout(
    frame,
    1:5,
    widths = rep(80, 5L),
    gap = 20,
    level_separation = 50,
    direction = "LR"
  )
  expect_identical(sideways[["x"]], out[["y"]])
  expect_identical(sideways[["y"]], out[["x"]])
})


test_that("draw_linad marks the node whose table is showing", {
  skip_if_not_installed("visNetwork")
  dat <- .linad_regression()
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = 5L, force_max_leaves = TRUE),
    verbosity = 0L
  )
  nodes <- draw_linad(mod, verbosity = 0L)[["x"]][["nodes"]]
  # Colors go in per node as an object, not as one string: given a string,
  # vis.js derives every state from it and the hover border could then never
  # differ from the fill.
  expect_true(all(
    c("color.background", "color.border", "color.hover.border") %in%
      names(nodes)
  ))
  # At rest the border matches the fill, so it reads as no border at all.
  expect_identical(nodes[["color.border"]], nodes[["color.background"]])
  # On hover it flips to one color, on every node, and that color is not a
  # fill anywhere in the tree -- otherwise the marker could not be told from
  # an ordinary node.
  hover <- unique(nodes[["color.hover.border"]])
  expect_length(hover, 1L)
  expect_false(hover %in% nodes[["color.background"]])
  # The fill itself does not change on hover; only the border does.
  expect_identical(
    nodes[["color.hover.background"]],
    nodes[["color.background"]]
  )
  # An explicit hover color is honored.
  marked <- draw_linad(mod, hover_col = "#FF00FF", verbosity = 0L)
  expect_identical(
    unique(marked[["x"]][["nodes"]][["color.hover.border"]]),
    "#FF00FF"
  )

  # And the table is bordered, or a mid-gray panel over mid-gray nodes has no
  # edge at all. It follows the theme, since it has to read against both the
  # canvas behind it and the table inside it.
  for (name in c("darkgray", "white")) {
    theme <- choose_theme(name)
    tips <- draw_linad(mod, theme = theme, verbosity = 0L)[["x"]][["nodes"]][[
      "title"
    ]]
    expect_true(all(grepl(
      paste0("border: 1px solid ", theme[["fg"]]),
      tips,
      fixed = TRUE
    )))
  }
})


test_that("draw_linad colors coefficients by the package-wide sign scale", {
  skip_if_not_installed("visNetwork")
  # The same two colors mean the same two signs here as in draw_volcano() and
  # plot_manhattan(), so the palette is read from one place rather than
  # restated per plot.
  signs <- rtemis:::SIGN_COLORS
  defaults <- formals(draw_linad)
  expect_identical(eval(defaults[["lo_col"]]), signs[["negative"]])
  expect_identical(eval(defaults[["hi_col"]]), signs[["positive"]])
  # Node fills come from the theme, and no theme may put a node on either end
  # of the sign scale: a hue would then mean a coefficient's sign in one place
  # and a node's role in another.
  ends <- c(signs[["negative"]], signs[["positive"]])
  for (name in c("darkgray", "white", "black", "lightgraygrid")) {
    fills <- rtemis:::linad_palette(choose_theme(name))
    expect_false(any(
      unlist(fills[c("root", "node", "leaf")]) %in% ends
    ))
  }
})


test_that("twocol2html sets every label in one treatment", {
  html <- rtemis:::twocol2html(
    data.frame(Term = "a", Coefficient = 1),
    caption = "Node value",
    caption_value = "63.0242"
  )
  # The caption's label and the two column headers name the values beneath
  # them, so all three carry the same style, from one string.
  labels <- regmatches(
    html,
    gregexpr("text-transform: uppercase[^\"]*", html)
  )[[1L]]
  expect_length(labels, 3L)
  expect_length(unique(labels), 1L)
  # The opacity has to sit on an inner element: on the `th`, which carries the
  # header background, it would fade that background too and let the table
  # show through.
  headers <- regmatches(html, gregexpr("<th[^>]*>", html))[[1L]]
  expect_true(all(grepl("background-color", headers, fixed = TRUE)))
  expect_false(any(grepl("opacity", headers, fixed = TRUE)))
  # A caption with no value under it is content, not a label for something.
  plain <- rtemis:::twocol2html(
    data.frame(Term = "a", Coefficient = 1),
    caption = "Just a caption"
  )
  expect_equal(
    lengths(regmatches(
      plain,
      gregexpr("text-transform: uppercase", plain)
    )),
    2L,
    ignore_attr = TRUE
  )
})


test_that("draw_linad labels a node in two tiers", {
  skip_if_not_installed("visNetwork")
  dat <- .linad_regression()
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = 5L, force_max_leaves = TRUE),
    verbosity = 0L
  )
  nodes <- draw_linad(mod, verbosity = 0L)[["x"]][["nodes"]]
  labels <- nodes[["label"]]
  # An eyebrow carrying the node value and the case count, over the rule. The
  # markup selects vis.js font slots; it is not decoration.
  expect_true(all(grepl("^<b>[^<]+</b><i>[^<]*n=[0-9]+</i>\n", labels)))
  # The relational characters, not their ASCII digraphs.
  expect_false(any(grepl(">=", labels, fixed = TRUE)))
  expect_true(any(grepl("\u2265", labels, fixed = TRUE)))
  # Negative numbers carry a minus sign, as the coefficient table does, not a
  # hyphen. Checked on a value known to be negative rather than on whatever
  # this fit produced.
  expect_identical(rtemis:::linad_minus("-1.5"), "\u{2212}1.5")
  expect_identical(rtemis:::linad_minus("1.5"), "1.5")
  # Turning both off leaves the rule alone, with no stray separator.
  bare <- draw_linad(
    mod,
    show_node_value = FALSE,
    show_ncases = FALSE,
    verbosity = 0L
  )[["x"]][["nodes"]][["label"]]
  expect_false(any(grepl("<b>|<i>|\u00B7", bare)))
  # And either one alone is well formed.
  only_n <- draw_linad(mod, show_node_value = FALSE, verbosity = 0L)
  expect_true(all(grepl(
    "^<i>n=[0-9]+</i>\n",
    only_n[["x"]][["nodes"]][["label"]]
  )))
})


test_that("draw_linad refuses a model that is not a LINAD", {
  skip_if_not_installed("visNetwork")
  mod <- train(
    iris[51:150, ],
    hyperparameters = setup_CART(),
    verbosity = 0L
  )
  expect_error(draw_linad(mod), class = "rtemis_type_error")
})
