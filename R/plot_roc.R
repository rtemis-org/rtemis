# plot_roc.R
# ::rtemis::
# 2026- EDG rtemis.org

#' Plot Classification ROC Curves
#'
#' S7 generic for ordinary and resampled rtemis classification
#' objects using ECharts.
#' All available training, validation, and test probability samples are selected
#' by default. Resampled results use training and test. An explicit unavailable
#' sample is an error; absence of training probabilities does not hide test data.
#'
#' `variant = "aggregate"` pools aligned prediction rows, retaining repeated
#' observations. `"per_resample"` draws individual curves with mean and sample
#' SD of fold AUCs. Missing probability folds remain undefined curves and are
#' disclosed; they are not zero AUCs. All curves use the same named classes and
#' binary positive class, defaulting to the first fold's second outcome level.
#'
#' @param x rtemis classification object: Ordinary or resampled result.
#' @param ... Method arguments, including `what` (Character: `"all"` or unique
#'   sample names), `variant` (Character: `"aggregate"` or `"per_resample"`),
#'   `positive` (Optional Character: binary positive class), and `labelify`
#'   (Logical: capitalize sample labels), followed by additional named
#'   arguments to [rtemis.draw::draw_roc()].
#' @return An ECharts htmlwidget.
#' @export
#' @examplesIf interactive() && requireNamespace("rpart", quietly = TRUE)
#' model <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
#' plot_roc(model)
plot_roc <- new_generic("plot_roc", "x")

#' Align a resampled field with the model's resample identifiers
#' @param x Optional List: Per-resample field.
#' @param ids Character: Resample identifiers in model order.
#' @return List aligned with ids, preserving null entries.
#' @keywords internal
#' @noRd
roc_align_folds <- new_generic("roc_align_folds", "x")
method(roc_align_folds, class_any) <- function(x, ids) {
  if (is.null(x)) {
    return(rep(list(NULL), length(ids)))
  }
  if (!is.list(x) || is.data.frame(x) || length(x) != length(ids)) {
    abort(
      "Supply one label/probability entry per resample, preserving NULL entries.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  if (!is.null(names(x))) {
    if (
      anyNA(names(x)) || anyDuplicated(names(x)) || !setequal(names(x), ids)
    ) {
      abort(
        "Per-resample names must match resample_ids exactly.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    x <- x[ids]
  }
  x
}

#' Extract portable ROC records from a model
#' @inheritParams plot_roc
#' @return Data frame of class, split, fold, FPR, TPR, AUC, and omitted counts.
#' @keywords internal
#' @noRd
roc_plot_data <- new_generic("roc_plot_data", "x")

#' Normalize classification prediction samples before computing ROC curves
#' @inheritParams plot_roc
#' @return Data frame of selected ROC records.
#' @keywords internal
#' @noRd
extract_roc_plot_data <- function(
  x,
  what = "all",
  variant = "aggregate",
  positive = NULL,
  labelify = TRUE
) {
  check_logical_scalar(labelify)
  variant <- match_arg(variant, c("aggregate", "per_resample"))
  properties <- names(S7_class(x)@properties)
  resampled <- "models" %in% properties
  if (!resampled && variant != "aggregate") {
    abort(
      "Use aggregate curves for an ordinary classification result.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  allowed <- c("training", "validation", "test")
  allowed <- allowed[paste0("predicted_prob_", allowed) %in% properties]
  all_samples <- identical(what, "all")
  if (
    !is.character(what) ||
      !length(what) ||
      anyNA(what) ||
      anyDuplicated(what) ||
      (!all_samples && any(!what %in% allowed))
  ) {
    abort(
      "Select 'all' or unique available training, validation, and test sample names.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (all_samples) {
    what <- allowed
  }
  ids <- if (resampled) x@resample_ids else "aggregate"
  # Prefer the training class convention even when only another sample is
  # selected. Named probability columns still identify their own classes.
  base_labels <- x@y_training
  if (resampled) {
    retained <- Filter(Negate(is.null), base_labels)
    base_labels <- if (length(retained)) retained[[1L]] else NULL
  }
  if (
    is.null(positive) && is.factor(base_labels) && nlevels(base_labels) == 2L
  ) {
    positive <- levels(base_labels)[[2L]]
  }
  if (any(ids == "aggregate") && resampled) {
    abort(
      "Reserve the fold identifier 'aggregate' for pooled ROC curves; rename that resample.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  pieces <- list()
  for (sample in what) {
    y <- prop(x, paste0("y_", sample))
    p <- prop(x, paste0("predicted_prob_", sample))
    unavailable <- is.null(p) ||
      (is.list(p) && all(vapply(p, is.null, logical(1))))
    if (unavailable) {
      if (all_samples) {
        next
      }
      abort(
        "Sample '",
        sample,
        "' has no probabilities; select a sample with predicted probabilities.",
        class = c("rtemis_null_input", "rtemis_input_error")
      )
    }
    if (!resampled) {
      if (
        is.factor(y) &&
          nlevels(y) == 2L &&
          is.matrix(p) &&
          ncol(p) == 1L &&
          is.null(colnames(p))
      ) {
        colnames(p) <- levels(y)[[2L]]
      }
      normalized <- list(rtemis.draw::roc_probabilities(y, p, positive))
    } else {
      labels <- roc_align_folds(y, ids)
      probabilities <- roc_align_folds(p, ids)
      first <- which(!vapply(labels, is.null, logical(1)))[1L]
      if (is.na(first) || !is.factor(labels[[first]])) {
        abort(
          "Keep factor class levels on resampled reference labels.",
          class = c("rtemis_value_error", "rtemis_input_error")
        )
      }
      classes <- levels(labels[[first]])
      selected <- if (length(classes) == 2L) {
        positive %||% classes[[2L]]
      } else {
        classes
      }
      normalized <- lapply(seq_along(ids), function(i) {
        yy <- labels[[i]]
        pp <- probabilities[[i]]
        if (is.null(yy) && is.null(pp)) {
          return(list(
            y = character(),
            levels = classes,
            classes = selected,
            prob = matrix(
              numeric(),
              0L,
              length(classes),
              dimnames = list(NULL, classes)
            )
          ))
        }
        if (is.null(yy) || !is.factor(yy) || !setequal(levels(yy), classes)) {
          abort(
            "Keep matching factor class identities and paired labels in every resample.",
            class = c("rtemis_value_error", "rtemis_input_error")
          )
        }
        # Missing fold scores remain missing observations in the pooled input,
        # and an undefined placeholder in the per-resample view.
        if (is.null(pp)) {
          pp <- matrix(
            NA_real_,
            length(yy),
            length(classes),
            dimnames = list(NULL, classes)
          )
        }
        if (
          is.matrix(pp) &&
            ncol(pp) == 1L &&
            is.null(colnames(pp)) &&
            nlevels(yy) == 2L
        ) {
          colnames(pp) <- levels(yy)[[2L]]
        }
        out <- rtemis.draw::roc_probabilities(
          yy,
          pp,
          if (length(classes) == 2L) selected else positive
        )
        out[["prob"]] <- out[["prob"]][, classes, drop = FALSE]
        out[["classes"]] <- selected
        out
      })
    }
    if (variant == "aggregate") {
      pooled <- normalized[[1L]]
      pooled[["y"]] <- unlist(lapply(normalized, `[[`, "y"), use.names = FALSE)
      pooled[["prob"]] <- do.call(rbind, lapply(normalized, `[[`, "prob"))
      out <- rtemis.draw::roc_vertices(pooled)
      out[["fold"]] <- "aggregate"
    } else {
      out <- do.call(
        rbind,
        lapply(seq_along(normalized), function(i) {
          curve <- rtemis.draw::roc_vertices(normalized[[i]])
          curve[["fold"]] <- ids[[i]]
          curve
        })
      )
    }
    out[["split"]] <- if (labelify) {
      paste0(toupper(substr(sample, 1L, 1L)), substring(sample, 2L))
    } else {
      sample
    }
    pieces[[length(pieces) + 1L]] <- out
  }
  if (!length(pieces)) {
    abort(
      "No predicted probabilities are available; select a classification result with probabilities.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

#' Draw ROC records extracted from a classification result
#' @inheritParams plot_roc
#' @return An ECharts htmlwidget.
#' @keywords internal
#' @noRd
draw_model_roc <- function(
  x,
  what = "all",
  variant = "aggregate",
  positive = NULL,
  labelify = TRUE,
  ...
) {
  data <- roc_plot_data(x, what, variant, positive, labelify)
  rtemis.draw::draw_roc(data, variant = variant, ...)
}
