# draw_calibration.R
# ::rtemis::
# 2023 EDG rtemis.org

#' Draw calibration plot
#'
#' @param true_labels Factor or list of factors with true class labels
#' @param predicted_prob Numeric vector or list of numeric vectors with predicted probabilities
#' @param bin_method Character: "quantile" or "equidistant": Method to bin the estimated
#' probabilities.
#' @param n_bins Integer: Number of windows to split the data into
#' @param binclasspos Integer: Index of the positive class. The convention used in the package is
#' the second level is the positive class.
#' @param main Character: Main title
#' @param subtitle Character: Subtitle, placed bottom right of plot
#' @param xlab Character: x-axis label
#' @param ylab Character: y-axis label
#' @param show_marginal_x Logical: Add marginal plot of distribution of estimated probabilities
#' @param marginal_x_y Numeric: y position of marginal plot
#' @param marginal_col Character: Color of marginal plot
#' @param marginal_size Numeric: Size of marginal plot
#' @param mode Character: "lines", "markers", "lines+markers": How to plot.
#' @param show_brier Logical: If TRUE, add Brier scores to trace names.
#' @param theme `Theme` object.
#' @param filename Character: Path to save output.
#' @param ... Additional arguments passed to [draw_scatter]
#'
#' @return `plotly` object.
#'
#' @author EDG
#' @export
#'
#' @examplesIf interactive()
#' # Synthetic data with n cases
#' n <- 500L
#' true_labels <- factor(sample(c("A", "B"), n, replace = TRUE))
#' # Synthetic probabilities where A has mean 0.25 and B has mean 0.75
#' predicted_prob <- ifelse(true_labels == "A",
#'   rbeta(n, 2, 6),
#'   rbeta(n, 6, 2)
#' )
#' draw_calibration(true_labels, predicted_prob)
draw_calibration <- function(
  true_labels,
  predicted_prob,
  n_bins = 10L,
  bin_method = c("quantile", "equidistant"),
  binclasspos = 2L,
  main = NULL,
  subtitle = NULL,
  xlab = "Mean predicted probability",
  ylab = "Empirical risk",
  show_marginal_x = TRUE,
  marginal_x_y = -.02,
  marginal_col = NULL,
  marginal_size = 10,
  mode = "markers+lines",
  show_brier = TRUE,
  theme = choose_theme(getOption("rtemis_theme")),
  filename = NULL,
  ...
) {
  # Arguments ----
  bin_method <- match.arg(bin_method)
  if (!is.list(true_labels)) {
    true_labels <- list(true_labels = true_labels)
  }
  if (!is.list(predicted_prob)) {
    predicted_prob <- list(estimated_prob = predicted_prob)
  }
  # Ensure same number of inputs
  stopifnot(length(true_labels) == length(predicted_prob))

  # Theme ----
  check_is_S7(theme, Theme)

  pos_class <- lapply(true_labels, \(x) {
    levels(x)[binclasspos]
  })

  # Ensure same positive class
  stopifnot(length(unique(unlist(pos_class))) == 1)

  # Create windows
  if (bin_method == "equidistant") {
    breaks <- lapply(seq_along(predicted_prob), \(x) {
      seq(0, 1, length.out = n_bins + 1)
    })
  } else if (bin_method == "quantile") {
    breaks <- lapply(predicted_prob, \(x) {
      quantile(x, probs = seq(0, 1, length.out = n_bins + 1))
    })
  }

  # Calculate the mean probability in each window
  mean_bin_prob <- lapply(seq_along(predicted_prob), \(i) {
    sapply(seq_len(n_bins), \(j) {
      mean(predicted_prob[[i]][
        predicted_prob[[i]] >= breaks[[i]][j] &
          predicted_prob[[i]] < breaks[[i]][j + 1]
      ])
    })
  })
  names(mean_bin_prob) <- names(predicted_prob)

  # Calculate the proportion of condition positive cases in each window
  window_empirical_risk <- lapply(seq_along(predicted_prob), \(i) {
    sapply(seq_len(n_bins), \(j) {
      idl <- predicted_prob[[i]] >= breaks[[i]][j] &
        predicted_prob[[i]] < breaks[[i]][j + 1]
      sum(true_labels[[i]][idl] == pos_class[[i]]) / sum(idl)
    })
  })
  names(window_empirical_risk) <- names(predicted_prob)

  # Add Brier score
  if (show_brier) {
    .brier_score <- sapply(seq_along(predicted_prob), \(i) {
      brier_score(
        true_int = labels2int(true_labels[[i]], binclasspos),
        predicted_prob = predicted_prob[[i]]
      )
    })
    names(window_empirical_risk) <- paste0(
      names(window_empirical_risk),
      " (Brier=",
      round(.brier_score, 3),
      ")"
    )
  }

  # Calculate confidence intervals
  # confint <- sapply(seq_len(n_bins), \(i) {
  #     events <- length(true_labels[true_labels == pos_class & predicted_prob >= breaks[i] & predicted_prob < breaks[i + 1]])
  #     total <- length(predicted_prob >= breaks[i] & predicted_prob < breaks[i + 1])
  #     suppressWarnings(pt <- prop.test(
  #         events, total,
  #         conf.level = conf_level
  #     ))
  #     pt$conf.int
  # })

  # Plot
  if (is.null(subtitle)) {
    subtitle <- paste(
      "using",
      n_bins,
      if (bin_method == "quantile") "quantiles" else "equidistant bins"
    )
  }
  # if (is.null(subtitle) && !is.na(subtitle)) .subtitle <- paste0(subtitle, "\n", .subtitle)
  plt <- draw_scatter(
    x = mean_bin_prob,
    y = window_empirical_risk,
    main = main,
    # subtitle = paste("<i>", .subtitle, "</i>"),
    subtitle = subtitle,
    subtitle_x = 1,
    subtitle_y = 0,
    subtitle_yref = "y",
    subtitle_xanchor = "right",
    subtitle_yanchor = "bottom",
    xlab = xlab,
    ylab = ylab,
    show_marginal_x = show_marginal_x,
    marginal_x = predicted_prob,
    marginal_x_y = marginal_x_y,
    marginal_size = marginal_size,
    axes_square = TRUE,
    diagonal = TRUE,
    xlim = c(0, 1),
    ylim = c(0, 1),
    mode = mode,
    theme = theme,
    filename = filename,
    ...
  )

  # Add marginal.x ----
  # Using estimated probabilities
  # if (marginal.x) {
  #   if (is.null(marginal.col)) marginal.col <- plotly::toRGB(theme[["fg"]], alpha = .5)
  #   for (i in seq_along(mean_bin_prob)) {
  #     plt <- plotly::add_trace(
  #       plt,
  #       x = predicted_prob[[i]],
  #       y = rep(-.02, length(predicted_prob[[i]])),
  #       type = "scatter",
  #       mode = "markers",
  #       marker = list(
  #         color = marginal.col,
  #         size = marginal.size,
  #         symbol = "line-ns-open"
  #       ),
  #       showlegend = FALSE,
  #       hoverinfo = "x"
  #     )
  #   }
  # } # /marginal.x

  plt
} # /rtemis::draw_calibration
