# Integration check against a real producer, with independent count/rank
# calculations rather than comparisons between two draw extraction paths.
test_that("real resampled classifiers retain fold metrics and pooled prediction meaning", {
  flowers <- iris[c("Sepal.Length", "Sepal.Width")]
  flowers[["Species"]] <- factor(
    ifelse(iris[["Species"]] == "virginica", "Virginica", "Other"),
    levels = c("Other", "Virginica")
  )
  model <- rtemis::train(
    flowers,
    hyperparameters = rtemis::setup_GLM(),
    outer_resampling_config = rtemis::setup_KFold(n_resamples = 5L, seed = 31L),
    execution_config = rtemis::setup_SerialExecution(),
    verbosity = 0L
  )
  held_out <- lapply(model@outer_resampler@resamples, function(training) {
    setdiff(seq_len(nrow(flowers)), training)
  })
  expect_identical(
    sort(unlist(held_out, use.names = FALSE)),
    seq_len(nrow(flowers))
  )
  for (sample in c("training", "test")) {
    labels <- S7::prop(model, paste0("y_", sample))
    predictions <- S7::prop(model, paste0("predicted_", sample))
    probabilities <- S7::prop(model, paste0("predicted_prob_", sample))
    counts <- classification_plot_data(model, what = sample)
    actual <- stats::xtabs(n ~ reference + predicted, counts)
    expected <- table(
      reference = unlist(labels, use.names = FALSE),
      predicted = unlist(predictions, use.names = FALSE)
    )
    expect_equal(as.vector(actual), as.vector(expected))
    expect_equal(dimnames(actual), dimnames(expected))
    expect_equal(sum(counts[["n"]]), if (sample == "test") 150 else 600)

    balanced <- vapply(
      seq_along(labels),
      function(i) {
        tab <- table(labels[[i]], predictions[[i]])
        mean(diag(tab) / rowSums(tab))
      },
      numeric(1)
    )
    metrics <- metric_plot_data(model, what = sample)
    expect_identical(metrics[["fold"]], model@resample_ids)
    expect_equal(metrics[["value"]], unname(balanced))

    # Empirical AUC: every positive/negative score pair, half credit for ties.
    pair_auc <- function(y, p) {
      delta <- outer(p[y == "Virginica"], p[y == "Other"], "-")
      mean((delta > 0) + 0.5 * (delta == 0))
    }
    expected_auc <- vapply(
      seq_along(labels),
      function(i) {
        pair_auc(labels[[i]], probabilities[[i]][, "Virginica"])
      },
      numeric(1)
    )
    folds <- roc_plot_data(
      model,
      sample,
      "per_resample",
      positive = "Virginica"
    )
    aucs <- vapply(
      model@resample_ids,
      function(id) {
        unique(folds[["auc"]][folds[["fold"]] == id])
      },
      numeric(1)
    )
    expect_equal(unname(aucs), unname(expected_auc))
    aggregate <- roc_plot_data(model, sample, positive = "Virginica")
    expect_equal(
      unique(aggregate[["auc"]]),
      pair_auc(
        unlist(labels, use.names = FALSE),
        do.call(rbind, probabilities)[, "Virginica"]
      )
    )
    expect_false(isTRUE(all.equal(unique(aggregate[["auc"]]), mean(aucs))))
    for (variant in c("aggregate", "per_resample")) {
      widget <- plot_roc(model, what = sample, variant = variant)
      expect_s3_class(widget, "htmlwidget")
      expect_length(
        widget[["x"]][["option"]][["series"]],
        if (variant == "aggregate") 2L else 6L
      )
    }
  }
  expect_s3_class(plot_true_pred(model, what = "test"), "htmlwidget")
  expect_s3_class(plot_metric(model), "htmlwidget")
})
