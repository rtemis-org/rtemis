# test_draw.R
# ::rtemis::
# 2025 EDG rtemis.org

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
