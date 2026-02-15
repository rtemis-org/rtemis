# S7_Supervised.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7
# https://rconsortium.github.io/S7/articles/classes-objects.html?q=computed#computed-properties
# https://utf8-icons.com/

# Plot methods
# Supervised: plot_varimp
# SupervisedRes: plot_varimp, plot_metric
# Regression: plot_true_pred,
# Classification: plot_true_pred, plot_roc
# RegressionRes: plot_metric, plot_true_pred,
# ClassificationRes: plot_metric, plot_true_pred, plot_roc

# Supervised ----
#' @title Supervised
#'
#' @description
#' Superclass for supervised learning models.
#'
#' @author EDG
#' @noRd
Supervised <- new_class(
  name = "Supervised",
  properties = list(
    algorithm = class_character,
    model = class_any,
    type = class_character,
    preprocessor = Preprocessor | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner = Tuner | NULL,
    execution_config = ExecutionConfig,
    y_training = class_any,
    y_validation = class_any,
    y_test = class_any,
    predicted_training = class_any,
    predicted_validation = class_any,
    predicted_test = class_any,
    metrics_training = Metrics,
    metrics_validation = Metrics | NULL,
    metrics_test = Metrics | NULL,
    xnames = class_character,
    varimp = class_any,
    question = class_character | NULL,
    extra = class_any,
    session_info = class_any
  ),
  constructor = function(
    algorithm,
    model,
    type,
    preprocessor,
    hyperparameters,
    tuner,
    execution_config,
    y_training,
    y_validation,
    y_test,
    predicted_training,
    predicted_validation,
    predicted_test,
    metrics_training,
    metrics_validation,
    metrics_test,
    xnames,
    varimp,
    question,
    extra
  ) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      model = model,
      type = type,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      execution_config = execution_config,
      y_training = y_training,
      y_validation = y_validation,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      metrics_training = metrics_training,
      metrics_validation = metrics_validation,
      metrics_test = metrics_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      session_info = sessionInfo()
    )
  }
) # /rtemis::Supervised


# Predict Supervised ----
#' Predict `Supervised`
#'
#' Predict Method for `Supervised` objects
#'
#' @param object `Supervised` object.
#' @param newdata data.frame or similar: New data to predict.
#'
#' @noRd
method(predict, Supervised) <- function(object, newdata, ...) {
  check_inherits(newdata, "data.frame")
  do_call(
    predict_super,
    list(model = object@model, newdata = newdata, type = object@type)
  )
} # /rtemis::predict.Supervised


# Fitted Supervised ----
#' Fitted `Supervised`
#'
#' Fitted Method for `Supervised` objects
#'
#' @param object `Supervised` object.
#'
#' @keywords internal
#' @noRd
method(fitted, Supervised) <- function(object, ...) {
  object@predicted_training
} # /rtemis::fitted.Supervised


# Standard Error Supervised ----
#' Standard Error `Supervised`
#'
#' Standard Error Method for `Supervised` objects
#'
#' @param object `Supervised` object.
#'
#' @keywords internal
#' @noRd
method(se, Supervised) <- function(x, ...) {
  x@se_training
}

# Make Supervised props `$`- accessible ----
method(`$`, Supervised) <- function(x, name) {
  prop(x, name)
}


# `$`-autocomplete Supervised props ----
method(`.DollarNames`, Supervised) <- function(x, pattern = "") {
  all_names <- names(props(x))
  grep(pattern, all_names, value = TRUE)
}

# Make Supervised props `[[`- accessible ----
method(`[[`, Supervised) <- function(x, name) {
  prop(x, name)
}


# repr Supervised ----
#' repr `Supervised`
#'
#' @param x `Supervised` object.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
method(repr, Supervised) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  filename = NULL
) {
  output_type <- get_output_type(output_type, filename)
  # Class name
  out <- paste0(
    repr_S7name(x@type, pad = pad, output_type = output_type),
    highlight(x@algorithm, output_type = output_type),
    " (",
    desc_alg(x@algorithm),
    ")\n"
  )

  # Tuning, if available
  if (!is.null(x@tuner)) {
    out <- paste0(
      out,
      fmt(
        "\U2699",
        col = col_tuner,
        bold = TRUE,
        pad = pad,
        output_type = output_type
      ),
      " Tuned using ",
      desc(x@tuner),
      ".\n"
    )
  }

  # Calibration, if available
  if (prop_exists(x, "calibration_model")) {
    out <- paste0(
      out,
      fmt(
        "\U27CB",
        col = col_calibrator,
        bold = TRUE,
        pad = pad,
        output_type = output_type
      ),
      " Calibrated using ",
      desc_alg(x@calibration_model@algorithm),
      ".\n"
    )
  }

  out <- paste0(out, "\n")

  # {Regression, Classification} vs. CalibratedClassification
  if (prop_exists(x, "calibration_model")) {
    # CalibratedClassification
    # Metrics, training
    out <- paste0(
      out,
      # repr(x@metrics_training, pad = 2L, output_type = output_type)
      repr_CalibratedClassificationMetrics(
        x@metrics_training,
        x@metrics_training_calibrated,
        pad = pad + 2L,
        output_type = output_type
      )
    )

    # Metrics, validation
    if (length(x@metrics_validation) > 0) {
      out <- paste0(
        out,
        repr_CalibratedClassificationMetrics(
          x@metrics_validation,
          x@metrics_validation_calibrated,
          pad = pad + 2L,
          output_type = output_type
        )
      )
    }

    # Metrics, test
    if (length(x@metrics_test) > 0) {
      out <- paste0(
        out,
        "\n",
        repr_CalibratedClassificationMetrics(
          x@metrics_test,
          x@metrics_test_calibrated,
          pad = pad + 2L,
          output_type = output_type
        )
      )
    }
  } else {
    # {Regression, Classification}

    # Metrics, training
    out <- paste0(
      out,
      repr(x@metrics_training, pad = pad + 2L, output_type = output_type)
    )

    # Metrics, validation
    if (length(x@metrics_validation) > 0) {
      out <- paste0(
        out,
        repr(x@metrics_validation, pad = pad + 2L, output_type = output_type)
      )
    }

    # Metrics, test
    if (length(x@metrics_test) > 0) {
      out <- paste0(
        out,
        "\n",
        repr(x@metrics_test, pad = pad + 2L, output_type = output_type)
      )
    }
  }

  out
} # /rtemis::repr.Supervised


# Print Supervised ----
method(print, Supervised) <- function(
  x,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.Supervised


# Describe Supervised ----
method(describe, Supervised) <- function(x) {
  type <- x@type
  algorithm <- desc_alg(x@algorithm)
  cat(algorithm, " was used for ", tolower(type), ".\n", sep = "")
  desc <- paste0(algorithm, " was used for ", tolower(type), ".")

  # Tuning ----
  if (length(x@tuner) > 0) {
    describe(x@tuner)
  }

  # Metrics ----
  if (type == "Classification") {
    cat(
      "Balanced accuracy was",
      ddSci(x@metrics_training[["Overall"]][["Balanced_Accuracy"]]),
      "on the training set"
    )
    desc <- paste(
      desc,
      "Balanced accuracy was",
      ddSci(x@metrics_training[["Overall"]][["Balanced_Accuracy"]]),
      "in the training set"
    )
    if (!is.null(x@metrics_test[["Overall"]][["Balanced_Accuracy"]])) {
      cat(
        " and",
        ddSci(x@metrics_test[["Overall"]][["Balanced_Accuracy"]]),
        "in the test set."
      )
      desc <- paste(
        desc,
        "and",
        ddSci(x@metrics_test[["Overall"]][["Balanced_Accuracy"]]),
        "in the test set."
      )
    } else {
      cat(".")
      desc <- paste0(desc, ".")
    }
  } else if (type == "Regression") {
    cat(
      "R-squared was",
      ddSci(x@metrics_training[["Rsq"]]),
      "in the training set"
    )
    desc <- paste(
      desc,
      "R-squared was",
      ddSci(x@metrics_training[["Rsq"]]),
      "on the training set"
    )
    if (!is.null(x@metrics_test[["Rsq"]])) {
      cat(
        " and",
        ddSci(x@metrics_test[["Rsq"]]),
        "in the test."
      )
      desc <- paste(
        desc,
        "and",
        ddSci(x@metrics_test[["Rsq"]]),
        "on the test set."
      )
    } else {
      cat(".")
      desc <- paste0(desc, ".")
    }
  }
  cat("\n")
  invisible(desc)
} # /rtemis::describe.Supervised


# Classification ----
#' @title Classification
#'
#' @description
#' Supervised subclass for classification models.
#'
#' @author EDG
#' @noRd
Classification <- new_class(
  name = "Classification",
  parent = Supervised,
  properties = list(
    predicted_prob_training = class_double | class_data.frame | NULL,
    predicted_prob_validation = class_double | class_data.frame | NULL,
    predicted_prob_test = class_double | class_data.frame | NULL,
    binclasspos = class_integer
  ),
  constructor = function(
    algorithm = NULL,
    model = NULL,
    preprocessor = NULL, # Preprocessor
    hyperparameters = NULL, # Hyperparameters
    tuner = NULL, # Tuner
    execution_config,
    y_training = NULL,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_validation = NULL,
    predicted_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL,
    predicted_prob_training = NULL,
    predicted_prob_validation = NULL,
    predicted_prob_test = NULL,
    binclasspos = 2L
  ) {
    metrics_training <- classification_metrics(
      true_labels = y_training,
      predicted_labels = predicted_training,
      predicted_prob = predicted_prob_training,
      sample = "Training"
    )
    metrics_validation <- if (!is.null(y_validation)) {
      classification_metrics(
        true_labels = y_validation,
        predicted_labels = predicted_validation,
        predicted_prob = predicted_prob_validation,
        sample = "Validation"
      )
    } else {
      NULL
    }
    metrics_test <- if (!is.null(y_test)) {
      classification_metrics(
        true_labels = y_test,
        predicted_labels = predicted_test,
        predicted_prob = predicted_prob_test,
        sample = "Test"
      )
    } else {
      NULL
    }
    new_object(
      Supervised(
        algorithm = algorithm,
        model = model,
        type = "Classification",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner = tuner,
        execution_config = execution_config,
        y_training = y_training,
        y_validation = y_validation,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_validation = predicted_validation,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_validation = metrics_validation,
        metrics_test = metrics_test,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_test = predicted_prob_test,
      binclasspos = binclasspos
    )
  }
) # /rtemis::Classification


# CalibratedClassification ----
#' @title CalibratedClassification
#'
#' @description
#' Classification subclass for calibrated classification models.
#' The classification_model can be trained on any data, ideally different from any data used by
#' the classification model.
#'
#' @author EDG
#' @noRd
CalibratedClassification <- new_class(
  name = "CalibratedClassification",
  parent = Classification,
  properties = list(
    calibration_model = Supervised,
    predicted_training_calibrated = class_vector,
    predicted_validation_calibrated = class_vector | NULL,
    predicted_test_calibrated = class_vector | NULL,
    predicted_prob_training_calibrated = class_double,
    predicted_prob_validation_calibrated = class_double | NULL,
    predicted_prob_test_calibrated = class_double | NULL,
    metrics_training_calibrated = Metrics,
    metrics_validation_calibrated = Metrics | NULL,
    metrics_test_calibrated = Metrics | NULL
  ),
  constructor = function(classification_model, calibration_model) {
    # Predict calibrated probabilities of classification model datasets
    predicted_prob_training_calibrated <- predict(
      calibration_model,
      data.frame(
        predicted_probabilities = classification_model@predicted_prob_training
      ),
    )
    predicted_prob_validation_calibrated <- if (
      !is.null(classification_model@predicted_prob_validation)
    ) {
      predict(
        calibration_model,
        data.frame(
          predicted_probabilities = classification_model@predicted_prob_validation
        )
      )
    } else {
      NULL
    }
    predicted_prob_test_calibrated <- if (
      !is.null(classification_model@predicted_prob_test)
    ) {
      predict(
        calibration_model,
        data.frame(
          predicted_probabilities = classification_model@predicted_prob_test
        )
      )
    } else {
      NULL
    }
    # Predict calibrated labels of classification model datasets
    predicted_training_calibrated <- prob2categorical(
      predicted_prob_training_calibrated,
      levels = levels(classification_model@y_training)
    )
    predicted_validation_calibrated <- if (
      !is.null(classification_model@predicted_prob_validation)
    ) {
      prob2categorical(
        predicted_prob_validation_calibrated,
        levels = levels(classification_model@y_validation)
      )
    } else {
      NULL
    }
    predicted_test_calibrated <- if (
      !is.null(classification_model@predicted_prob_test)
    ) {
      prob2categorical(
        predicted_prob_test_calibrated,
        levels = levels(classification_model@y_test)
      )
    } else {
      NULL
    }
    metrics_training_calibrated <- classification_metrics(
      true_labels = classification_model@y_training,
      predicted_labels = predicted_training_calibrated,
      predicted_prob = predicted_prob_training_calibrated,
      sample = "Calibrated Training"
    )
    metrics_validation_calibrated <- if (
      !is.null(classification_model@y_validation)
    ) {
      classification_metrics(
        true_labels = classification_model@y_validation,
        predicted_labels = predicted_validation_calibrated,
        predicted_prob = predicted_prob_validation_calibrated,
        sample = "Calibrated Validation"
      )
    } else {
      NULL
    }
    metrics_test_calibrated <- if (!is.null(classification_model@y_test)) {
      classification_metrics(
        true_labels = classification_model@y_test,
        predicted_labels = predicted_test_calibrated,
        predicted_prob = predicted_prob_test_calibrated,
        sample = "Calibrated Test"
      )
    } else {
      NULL
    }
    new_object(
      classification_model,
      calibration_model = calibration_model,
      predicted_training_calibrated = predicted_training_calibrated,
      predicted_validation_calibrated = predicted_validation_calibrated,
      predicted_test_calibrated = predicted_test_calibrated,
      predicted_prob_training_calibrated = predicted_prob_training_calibrated,
      predicted_prob_validation_calibrated = predicted_prob_validation_calibrated,
      predicted_prob_test_calibrated = predicted_prob_test_calibrated,
      metrics_training_calibrated = metrics_training_calibrated,
      metrics_validation_calibrated = metrics_validation_calibrated,
      metrics_test_calibrated = metrics_test_calibrated
    )
  }
) # /rtemis::CalibratedClassification

# Predict CalibratedClassification ----
method(predict, CalibratedClassification) <- function(object, newdata, ...) {
  check_inherits(newdata, "data.frame")
  # Get the classification model's predicted probabilities
  raw_prob <- do_call(
    predict_super,
    list(model = object@model, newdata = newdata)
  )
  # Get the calibration model's predicted probabilities
  predict(
    object@calibration_model,
    newdata = data.frame(predicted_probabilities = raw_prob)
  )
} # /rtemis::predict.CalibratedClassification

se_compat_algorithms <- c("GLM", "GAM")

# Regression ----
#' @title Regression
#' @description
#' Supervised subclass for regression models.
#'
#' @author EDG
#' @noRd
Regression <- new_class(
  name = "Regression",
  parent = Supervised,
  properties = list(
    se_training = class_double | NULL,
    se_validation = class_double | NULL,
    se_test = class_double | NULL
  ),
  constructor = function(
    algorithm = NULL,
    model = NULL,
    preprocessor = NULL, # Preprocessor
    hyperparameters = NULL, # Hyperparameters
    tuner = NULL, # Tuner
    execution_config, # ExecutionConfig
    y_training = NULL,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_validation = NULL,
    predicted_test = NULL,
    se_training = NULL,
    se_validation = NULL,
    se_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL
  ) {
    # Metrics ----
    metrics_training <- regression_metrics(
      y_training,
      predicted_training,
      sample = "Training"
    )
    metrics_validation <- if (!is.null(y_validation)) {
      regression_metrics(
        y_validation,
        predicted_validation,
        sample = "Validation"
      )
    } else {
      NULL
    }
    metrics_test <- if (!is.null(y_test)) {
      regression_metrics(
        y_test,
        predicted_test,
        sample = "Test"
      )
    } else {
      NULL
    }
    new_object(
      Supervised(
        algorithm = algorithm,
        model = model,
        type = "Regression",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner = tuner,
        execution_config = execution_config,
        y_training = y_training,
        y_validation = y_validation,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_validation = predicted_validation,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_validation = metrics_validation,
        metrics_test = metrics_test,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      se_training = se_training,
      se_validation = se_validation,
      se_test = se_test
    )
  }
) # /rtemis::Regression

# Plot True Pred Regression ----
#' Plot True vs. Predicted for Regression
#'
#' @param x `Regression` object.
#' @param what Character vector: What to plot. Can include "training", "validation", "test", or
#' "all", which will plot all available.
#' @param fit Character: Algorithm to use to draw fit line.
#' @param theme `Theme` object.
#' @param labelify Logical: If TRUE, labelify the axis labels.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(plot_true_pred, Regression) <- function(
  x,
  what = "all",
  fit = "glm",
  theme = choose_theme(getOption("rtemis_theme")),
  labelify = TRUE,
  ...
) {
  if (length(what) == 1 && what == "all") {
    what <- c("training", "validation", "test")
  }
  true <- paste0("y_", what)
  true_l <- Filter(
    Negate(is.null),
    sapply(true, function(z) prop(x, z))
  )
  predicted <- paste0("predicted_", what)
  predicted_l <- Filter(
    Negate(is.null),
    sapply(predicted, function(z) prop(x, z))
  )
  if (labelify) {
    names(predicted_l) <- labelify(names(predicted_l))
  }
  draw_fit(
    x = true_l,
    y = predicted_l,
    fit = fit,
    theme = theme,
    ...
  )
} # /rtemis::plot_true_pred.Regression


# Plot True Pred Classification ----
#' Plot True vs. Predicted for Classification
#'
#' @param x `Classification` object.
#' @param what Character vector: What to plot. "training", "validation", "test"
#' @param xlab Character: x axis label. If NULL, will be generated automatically.
#' @param theme `Theme` object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(plot_true_pred, Classification) <- function(
  x,
  what = NULL,
  xlab = NULL,
  theme = choose_theme(getOption("rtemis_theme")),
  ...
) {
  if (is.null(what)) {
    if (!is.null(x@metrics_test)) {
      what <- "test"
    } else if (!is.null(x@metrics_validation)) {
      what <- "validation"
    } else {
      what <- "training"
    }
  }
  .confmat <- if (what == "training") {
    x@metrics_training
  } else if (what == "validation") {
    x@metrics_validation
  } else if (what == "test") {
    x@metrics_test
  }
  if (is.null(xlab)) {
    xlab <- labelify(paste("Predicted", what))
  }
  draw_confusion(
    .confmat,
    theme = theme,
    xlab = xlab,
    ...
  )
} # /rtemis::plot_true_pred.Classification


# plot_ROC Classification ----
method(plot_roc, Classification) <- function(
  x,
  what = NULL,
  theme = choose_theme(getOption("rtemis_theme")),
  palette = get_palette(getOption("rtemis_palette")),
  filename = NULL,
  ...
) {
  if (is.null(x@predicted_prob_training)) {
    msg(highlight2("No predicted probabilities available."))
    return(invisible())
  }
  if (is.null(what)) {
    what <- if (!is.null(x@metrics_test)) {
      c("training", "test")
    } else {
      "training"
    }
  }
  labelsl <- probl <- list()

  if ("training" %in% what) {
    labelsl[["Training"]] <- x@y_training
    probl[["Training"]] <- x@predicted_prob_training
  }
  if ("test" %in% what && !is.null(x@predicted_prob_test)) {
    labelsl[["Test"]] <- x@y_test
    probl[["Test"]] <- x@predicted_prob_test
  }

  draw_roc(
    true_labels = labelsl,
    predicted_prob = probl,
    theme = theme,
    palette = palette,
    filename = filename,
    ...
  )
} # /rtemis::plot_ROC.Classification

# make_Supervised() ----
make_Supervised <- function(
  algorithm = NULL,
  model = NULL,
  preprocessor = NULL,
  hyperparameters = NULL,
  tuner = NULL,
  execution_config,
  y_training = NULL,
  y_validation = NULL,
  y_test = NULL,
  predicted_training = NULL,
  predicted_validation = NULL,
  predicted_test = NULL,
  predicted_prob_training = NULL,
  predicted_prob_validation = NULL,
  predicted_prob_test = NULL,
  se_training = NULL,
  se_validation = NULL,
  se_test = NULL,
  xnames = character(),
  varimp = NULL,
  question = character(),
  extra = NULL,
  binclasspos = 2L
) {
  # Supervised ----
  if (is.factor(y_training)) {
    Classification(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      execution_config = execution_config,
      y_training = y_training,
      y_validation = y_validation,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_validation = predicted_prob_validation,
      predicted_prob_test = predicted_prob_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      binclasspos = binclasspos
    )
  } else {
    Regression(
      algorithm = algorithm,
      model = model,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner = tuner,
      execution_config = execution_config,
      y_training = y_training,
      y_validation = y_validation,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_validation = predicted_validation,
      predicted_test = predicted_test,
      se_training = se_training,
      se_validation = se_validation,
      se_test = se_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  }
} # /rtemis::make_Supervised

# Write Supervised ----
write_Supervised <- function(
  object,
  outdir = NULL,
  save_mod = FALSE,
  theme = choose_theme(getOption("rtemis_theme")),
  verbosity = 1L
) {
  if (verbosity > 0L) {
    print(object)
  }

  if (save_mod) {
    rt_save(rt, outdir, verbosity = verbosity)
  }
} # /rtemis::write_Supervised


# Present Regression ----
# present method for Regression objects
# Plot training + test metrics, if available, side by side using `plotly::subplot()`
# & run `describe()` on the object
method(present, Regression) <- function(
  x,
  what = c("training", "test"),
  theme = choose_theme(getOption("rtemis_theme")),
  filename = NULL,
  ...
) {
  # Describe the model
  describe(x)
  # Plot True vs. Predicted
  plot_true_pred(
    x,
    what = what,
    theme = theme,
    filename = filename,
    ...
  )
} # /rtemis::present.Regression

# Present Classification ----
# present method for Classification objects
# Plot training + test metrics if available, side by side
method(present, Classification) <- function(
  x,
  what = c("training", "test"),
  type = c("ROC", "confusion"),
  theme = choose_theme(getOption("rtemis_theme")),
  palette = get_palette(getOption("rtemis_palette")),
  filename = NULL
) {
  type <- match.arg(type)

  # Describe the model
  describe(x)

  if (type == "ROC") {
    plot_roc(
      x,
      what = what,
      theme = theme,
      palette = palette,
      filename = filename
    )
  } else if (type == "confusion") {
    # Training set plot
    if ("training" %in% what) {
      plot_training <- plot_true_pred(
        x,
        what = "training",
        theme = theme,
        xlab = "Predicted Training"
      )
    } else {
      plot_training <- NULL
    }
    # Test set plot
    if ("test" %in% what && !is.null(x@y_test)) {
      plot_test <- plot_true_pred(
        x,
        what = "test",
        theme = theme,
        xlab = "Predicted Test"
      )
    } else {
      plot_test <- NULL
    }

    # Combined plot
    # classification: confusion matrices side by side
    plotly::subplot(
      plot_training,
      plot_test,
      nrows = 1L,
      shareX = FALSE,
      shareY = FALSE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.01
    )
  }
} # /rtemis::present.Classification

# SupervisedRes ----
# fields metrics_training/metrics_validation/metrics_test
# could be active bindings that are collected from @models
#' SupervisedRes
#'
#' @description
#' Superclass for Resampled supervised learning models.
#'
#' @author EDG
#' @noRd
SupervisedRes <- new_class(
  name = "SupervisedRes",
  properties = list(
    algorithm = class_character,
    models = class_list,
    type = class_character,
    preprocessor = Preprocessor | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner_config = TunerConfig | NULL,
    outer_resampler = Resampler,
    execution_config = ExecutionConfig,
    y_training = class_any,
    y_test = class_any,
    predicted_training = class_any,
    predicted_test = class_any,
    metrics_training = MetricsRes,
    metrics_test = MetricsRes,
    xnames = class_character,
    varimp = class_any,
    question = class_character | NULL,
    extra = class_any,
    session_info = class_any
  ),
  constructor = function(
    algorithm,
    models,
    type,
    preprocessor,
    hyperparameters,
    tuner_config,
    outer_resampler,
    execution_config,
    y_training,
    y_test,
    predicted_training,
    predicted_test,
    metrics_training,
    metrics_test,
    metrics_training_mean,
    metrics_test_mean,
    xnames,
    varimp,
    question,
    extra
  ) {
    new_object(
      S7::S7_object(),
      algorithm = algorithm,
      models = models,
      type = models[[1]]@type,
      hyperparameters = hyperparameters,
      tuner_config = tuner_config,
      outer_resampler = outer_resampler,
      execution_config = execution_config,
      y_training = y_training,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_test = predicted_test,
      metrics_training = metrics_training,
      metrics_test = metrics_test,
      # metrics_training_mean = metrics_training_mean,
      # metrics_test_mean = metrics_test_mean,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra,
      session_info = sessionInfo()
    )
  }
) # /rtemis::SupervisedRes


# repr SupervisedRes ----
#' repr `SupervisedRes`
#'
#' @param x `SupervisedRes` object.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#' @param filename Character: Filename to save output to (not used).
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
method(repr, SupervisedRes) <- function(
  x,
  output_type = NULL,
  filename = NULL
) {
  output_type <- get_output_type(output_type, filename)

  # Class name + Alg name (2 lines)
  out <- paste0(
    repr_S7name(paste("Resampled", x@type, "Model"), output_type = output_type),
    highlight(x@algorithm, output_type = output_type),
    " (",
    desc_alg(x@algorithm),
    ")\n"
  )

  # Tuning, if available (1 line)
  if (!is.null(x@tuner_config)) {
    out <- paste0(
      out,
      fmt("\U2699", col = col_tuner, bold = TRUE, output_type = output_type),
      " Tuned using ",
      desc(x@tuner_config),
      ".\n"
    )
  }

  # Outer resampling (1 line)
  out <- paste0(
    out,
    fmt("\U27F3", col = col_outer, bold = TRUE, output_type = output_type),
    " Tested using ",
    desc(x@outer_resampler),
    ".\n"
  )

  # Calibration, if available
  if (prop_exists(x, "calibration_models")) {
    out <- paste0(
      out,
      fmt(
        "\U27CB",
        col = col_calibrator,
        bold = TRUE,
        output_type = output_type
      ),
      " Calibrated using ",
      desc_alg(x@calibration_models[[1]]@algorithm),
      " with ",
      desc(x@calibration_models[[1]]@outer_resampler@config),
      ".\n"
    )
  }

  out <- paste0(out, "\n")

  # Metrics, training
  if (prop_exists(x, "calibration_models")) {
    out <- paste0(
      out,
      repr_CalibratedClassificationResMetrics(
        x@metrics_training,
        x@metrics_training_calibrated,
        pad = 2L,
        output_type = output_type
      )
    )
  } else {
    out <- paste0(
      out,
      repr(x@metrics_training, pad = 2L, output_type = output_type)
    )
  }

  # Metrics, test
  if (prop_exists(x, "calibration_models")) {
    out <- paste0(
      out,
      "\n",
      repr_CalibratedClassificationResMetrics(
        x@metrics_test,
        x@metrics_test_calibrated,
        pad = 2L,
        output_type = output_type
      )
    )
  } else {
    out <- paste0(
      out,
      "\n",
      repr(x@metrics_test, pad = 2L, output_type = output_type)
    )
  }

  out
} # /rtemis::repr.SupervisedRes


# Print SupervisedRes ----
method(print, SupervisedRes) <- function(
  x,
  output_type = NULL,
  ...
) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.SupervisedRes


# Predict SupervisedRes ----
#' Predict SupervisedRes
#'
#' Predict Method for SupervisedRes objects
#'
#' @param object `SupervisedRes` object.
#' @param newdata data.frame or similar: New data to predict.
#' @param type Character: Type of prediction to output: "avg" applies `avg_fn` (default "mean") to
#' the predictions of individual models, "all" returns the predictions of all models in a
#' data.frame. "metrics" returns a list of data.frames with a) predictions from each model, b)
#' the mean of the predictions, and c) the standard deviation of the predictions.
#' @param ... Not used.
#'
#' @keywords internal
#' @noRd
method(predict, SupervisedRes) <- function(
  object,
  newdata,
  type = c("avg", "all", "metrics"),
  avg_fn = "mean",
  ...
) {
  check_inherits(newdata, "data.frame")
  type <- match.arg(type)

  predicted <- sapply(
    object@models,
    function(mod) {
      do_call(
        predict_super,
        list(model = mod@model, newdata = newdata, type = object@type)
      )
    }
  ) # -> data.frame n cases x n resamples

  if (type == "all") {
    return(predicted)
  } else if (type == "avg") {
    return(apply(predicted, 1, avg_fn))
  } else if (type == "metrics") {
    mean_predictions <- apply(predicted, 2, mean)
    sd_predictions <- apply(predicted, 2, sd)
    return(list(
      predictions = predicted,
      mean = mean_predictions,
      sd = sd_predictions
    ))
  }
} # /rtemis::predict.SupervisedRes

# ClassificationRes ----
#' @title ClassificationRes
#'
#' @description
#' SupervisedRes subclass for Resampled classification models.
#'
#' @author EDG
#' @noRd
ClassificationRes <- new_class(
  name = "ClassificationRes",
  parent = SupervisedRes,
  properties = list(
    predicted_prob_training = class_any,
    predicted_prob_test = class_any
  ),
  constructor = function(
    algorithm,
    models,
    preprocessor,
    hyperparameters,
    tuner_config,
    outer_resampler,
    execution_config,
    y_training,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_test = NULL,
    predicted_prob_training = NULL,
    predicted_prob_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL
  ) {
    metrics_training <- ClassificationMetricsRes(
      sample = "Training",
      res_metrics = lapply(models, function(mod) mod@metrics_training)
    )
    metrics_test <- ClassificationMetricsRes(
      sample = "Test",
      res_metrics = lapply(models, function(mod) mod@metrics_test)
    )
    new_object(
      SupervisedRes(
        algorithm = algorithm,
        models = models,
        type = "Classification",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner_config = tuner_config,
        outer_resampler = outer_resampler,
        execution_config = execution_config,
        y_training = y_training,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_test = metrics_test,
        # metrics_training_mean = metrics_training_mean,
        # metrics_test_mean = metrics_test_mean,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      predicted_prob_training = predicted_prob_training,
      predicted_prob_test = predicted_prob_test
    )
  }
) # /rtemis::ClassificationRes


# CalibratedClassificationRes ----
#' @title CalibratedClassificationRes
#'
#' @description
#' ClassificationRes subclass for calibrated classification models.
#' The calibration models are trained on resamples of the `ClassificationRes`'s test data.
#'
#' @author EDG
#' @noRd
# We use getter functions to avoid duplicating data
CalibratedClassificationRes <- new_class(
  name = "CalibratedClassificationRes",
  parent = ClassificationRes,
  properties = list(
    calibration_models = class_list,
    predicted_training_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_training
        })
      }
    ),
    predicted_test_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_test
        })
      }
    ),
    predicted_prob_training_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_prob_training
        })
      }
    ),
    predicted_prob_test_calibrated = new_property(
      getter = function(self) {
        lapply(self@calibration_models, function(mod) {
          mod@predicted_prob_test
        })
      }
    ),
    metrics_training_calibrated = ClassificationMetricsRes,
    metrics_test_calibrated = ClassificationMetricsRes
  ),
  constructor = function(ClassificationRes_model, calibrations_models) {
    # Aggregate calibrated metrics from individual models within each calibration resample
    # calibrations_models is a list of *Res objects, each containing multiple models
    # We need to extract all individual model metrics and aggregate them
    all_training_metrics <- unlist(
      lapply(calibrations_models, function(calmod) {
        calmod@metrics_training@res_metrics
      }),
      recursive = FALSE
    )
    all_test_metrics <- unlist(
      lapply(calibrations_models, function(calmod) {
        calmod@metrics_test@res_metrics
      }),
      recursive = FALSE
    )

    metrics_training_calibrated <- ClassificationMetricsRes(
      sample = "Training",
      res_metrics = all_training_metrics
    )
    metrics_test_calibrated <- ClassificationMetricsRes(
      sample = "Test",
      res_metrics = all_test_metrics
    )

    new_object(
      ClassificationRes_model,
      calibration_models = calibrations_models,
      metrics_training_calibrated = metrics_training_calibrated,
      metrics_test_calibrated = metrics_test_calibrated
    )
  }
) # /rtemis::CalibratedClassificationRes


# Predict CalibratedClassificationRes ----
method(predict, CalibratedClassificationRes) <- function(
  object,
  newdata,
  type = c("avg", "all", "metrics"),
  avg_fn = "mean",
  ...
) {
  check_inherits(newdata, "data.frame")
  type <- match.arg(type)

  # Check lengths match
  if (length(object@models) != length(object@calibration_models)) {
    cli::cli_abort("Number of base models and calibration models must match.")
  }

  predicted <- mapply(
    function(base_mod, cal_mod) {
      # 1. Predict with base model
      raw_prob <- predict(
        base_mod,
        newdata = newdata,
        type = base_mod@type
      )

      # 2. Predict with calibration model
      predict(
        cal_mod,
        newdata = data.frame(predicted_probabilities = raw_prob)
      )
    },
    object@models,
    object@calibration_models,
    SIMPLIFY = TRUE
  ) # -> matrix n cases x n resamples

  if (type == "all") {
    return(predicted)
  } else if (type == "avg") {
    return(apply(predicted, 1, avg_fn))
  } else if (type == "metrics") {
    mean_predictions <- apply(predicted, 2, mean)
    sd_predictions <- apply(predicted, 2, sd)
    # Return both aggregated prediction metrics (per resample)
    # AND optionally aggregated predictions per case?
    # Keeping consistent with SupervisedRes
    return(list(
      predictions = predicted,
      mean = mean_predictions,
      sd = sd_predictions
    ))
  }
} # /rtemis::predict.CalibratedClassificationRes


# RegressionRes ----
#' @title RegressionRes
#'
#' @description
#' SupervisedRes subclass for Resampled regression models.
#'
#' @author EDG
#' @noRd
RegressionRes <- new_class(
  name = "RegressionRes",
  parent = SupervisedRes,
  properties = list(
    se_training = class_any,
    se_validation = class_any,
    se_test = class_any
  ),
  constructor = function(
    algorithm,
    models,
    preprocessor,
    hyperparameters,
    tuner_config,
    outer_resampler,
    execution_config,
    y_training,
    y_validation = NULL,
    y_test = NULL,
    predicted_training = NULL,
    predicted_test = NULL,
    se_training = NULL,
    se_test = NULL,
    xnames = NULL,
    varimp = NULL,
    question = NULL,
    extra = NULL
  ) {
    metrics_training <- lapply(
      models,
      function(mod) mod@metrics_training@metrics
    )
    metrics_test <- lapply(models, function(mod) mod@metrics_test@metrics)
    metrics_training <- RegressionMetricsRes(
      sample = "Training",
      res_metrics = lapply(models, function(mod) mod@metrics_training)
    )
    metrics_test <- RegressionMetricsRes(
      sample = "Test",
      res_metrics = lapply(models, function(mod) mod@metrics_test)
    )
    new_object(
      SupervisedRes(
        algorithm = algorithm,
        models = models,
        type = "Regression",
        preprocessor = preprocessor,
        hyperparameters = hyperparameters,
        tuner_config = tuner_config,
        outer_resampler = outer_resampler,
        execution_config = execution_config,
        y_training = y_training,
        y_test = y_test,
        predicted_training = predicted_training,
        predicted_test = predicted_test,
        metrics_training = metrics_training,
        metrics_test = metrics_test,
        # metrics_training_mean = metrics_training_mean,
        # metrics_test_mean = metrics_test_mean,
        xnames = xnames,
        varimp = varimp,
        question = question,
        extra = extra
      ),
      se_training = se_training,
      se_test = se_test
    )
  }
) # /rtemis::RegressionRes


# desc SupervisedRes ----
method(desc, SupervisedRes) <- function(x, metric = NULL) {
  type <- x@type
  algorithm <- desc_alg(x@algorithm)
  # cat(algorithm, " was used for ", tolower(type), ".\n", sep = "")
  out <- paste0(algorithm, " was used for ", tolower(type), ".")

  # Tuning ----
  if (length(x@tuner_config) > 0) {
    out <- paste0(
      out,
      " Hyperparameter tuning was performed using ",
      desc(x@tuner_config),
      "."
    )
  }

  # Metrics ----
  if (type == "Classification") {
    if (is.null(metric)) {
      metric <- "Balanced_Accuracy"
    }
    out <- paste(
      out,
      "Mean",
      labelify(metric, toLower = TRUE),
      "was",
      ddSci(x@metrics_training@mean_metrics[["Balanced_Accuracy"]]),
      "in the training set and",
      ddSci(x@metrics_test@mean_metrics[["Balanced_Accuracy"]]),
      "in the test set across "
    )
  } else if (type == "Regression") {
    out <- paste(
      out,
      "Mean R-squared was",
      ddSci(x@metrics_training@mean_metrics[["Rsq"]]),
      "on the training set and",
      ddSci(x@metrics_test@mean_metrics[["Rsq"]]),
      "on the test set across "
    )
  }
  out <- paste0(out, desc(x@outer_resampler), ".")
  invisible(out)
} # /rtemis::desc.SupervisedRes

# Describe SupervisedRes ----
method(describe, SupervisedRes) <- function(x, ...) {
  cat(desc(x), "\n")
}

# Present SupervisedRes ----
method(present, SupervisedRes) <- function(
  x,
  theme = choose_theme(getOption("rtemis_theme")),
  ...
) {
  # Describe the model
  describe(x)
  # Plot the performance metrics
  plot_metric(x, what = c("training", "test"), theme = theme, ...)
} # /rtemis::present.SupervisedRes


# Plot True Pred RegressionRes ----
# Plot true vs. predicted aggregated across resamples for either training, test, or both.
#' Plot True vs. Predicted for RegressionRes
#'
#' @param x `RegressionRes` object.
#' @param what Character vector: "all", "training", "test". Which set(s) to plot.
#' @param fit Character: Algorithm to use to draw fit line.
#' @param theme `Theme` object.
#' @param labelify Logical: If TRUE, labelify the axis labels.
#' @param ... Additional arguments passed to [draw_fit].
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(plot_true_pred, RegressionRes) <- function(
  x,
  what = "all",
  fit = "glm",
  theme = choose_theme(getOption("rtemis_theme")),
  labelify = TRUE,
  ...
) {
  if (length(what) == 1 && what == "all") {
    what <- c("training", "test")
  }
  true <- paste0("y_", what)
  true_l <- sapply(true, function(z) {
    unlist(prop(x, z), use.names = FALSE)
  })

  predicted <- paste0("predicted_", what)
  predicted_l <- sapply(predicted, function(z) {
    unlist(prop(x, z), use.names = FALSE)
  })
  if (labelify) {
    names(predicted_l) <- labelify(names(predicted_l))
  }
  draw_fit(
    x = true_l,
    y = predicted_l,
    fit = fit,
    theme = theme,
    ...
  )
} # /rtemis::plot_true_pred.RegressionRes


# Plot True Pred ClassificationRes ----
# Cannot be combined with plot_true_pred.RegressionRes
# because scatter can overplot train & test, but confusion matrices must be subplots.
#' Plot True vs. Predicted for ClassificationRes
#'
#' @param x `ClassificationRes` object.
#' @param what Character vector: "all", "training", "test". Which set(s) to plot.
#' @param theme `Theme` object.
#' @param ... Additional arguments passed to [draw_confusion].
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(plot_true_pred, ClassificationRes) <- function(
  x,
  what = "all",
  theme = choose_theme(getOption("rtemis_theme")),
  ...
) {
  if (length(what) == 1 && what == "all") {
    what <- c("training", "test")
  }
  true <- paste0("y_", what)
  true_l <- sapply(true, function(z) {
    unlist(prop(x, z), use.names = FALSE)
  })

  predicted <- paste0("predicted_", what)
  predicted_l <- sapply(predicted, function(z) {
    unlist(prop(x, z), use.names = FALSE)
  })
  # if (labelify) {
  #   names(predicted_l) <- labelify(names(predicted_l))
  # }
  # => Do not pass filename to both training & testing, latter will overwrite; pass to subplot if
  # plotting both
  # Training
  if ("training" %in% what) {
    plt_training <- draw_confusion(
      table(true_l[["y_training"]], predicted_l[["predicted_training"]]),
      xlab = "Predicted Training",
      theme = theme,
      ...
    )
  }
  if ("test" %in% what) {
    plt_test <- draw_confusion(
      table(true_l[["y_test"]], predicted_l[["predicted_test"]]),
      xlab = "Predicted Test",
      theme = theme,
      ...
    )
  }

  if (length(what) == 1) {
    if (what == "training") {
      return(plt_training)
    } else {
      return(plt_test)
    }
  } else {
    return(plotly::subplot(
      plt_training,
      plt_test,
      nrows = 1L,
      shareX = FALSE,
      shareY = FALSE
    ))
  }
} # /rtemis::plot_true_pred.ClassificationRes


# Plot ROC ClassificationRes ----
#' Plot ROC for ClassificationRes
#'
#' @param x `ClassificationRes` object.
#' @param what Character vector: "all", "training", "test". Which set(s) to plot.
#' @param theme `Theme` object.
#' @param col Character vector: Colors to use for the ROC curves.
#' @param filename Character: Filename to save the plot to.
#' @param ... Additional arguments passed to [draw_roc].
#'
#' @return plotly object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(plot_roc, ClassificationRes) <- function(
  x,
  what = "all",
  theme = choose_theme(getOption("rtemis_theme")),
  palette = get_palette(getOption("rtemis_palette")),
  filename = NULL,
  ...
) {
  if (length(what) == 1 && what == "all") {
    what <- c("training", "test")
  }
  labelsl <- probl <- list()

  if ("training" %in% what) {
    labelsl[["Training"]] <- unlist(x@y_training, use.names = FALSE)
    probl[["Training"]] <- unlist(x@predicted_prob_training, use.names = FALSE)
  }
  if ("test" %in% what && !is.null(x@predicted_prob_test)) {
    labelsl[["Test"]] <- unlist(x@y_test, use.names = FALSE)
    probl[["Test"]] <- unlist(x@predicted_prob_test, use.names = FALSE)
  }

  draw_roc(
    true_labels = labelsl,
    predicted_prob = probl,
    theme = theme,
    palette = palette,
    filename = filename,
    ...
  )
} # /rtemis::plot_roc.ClassificationRes


# Plot Metric SupervisedRes ----
#' Plot Metric SupervisedRes
#'
#' Plot boxplot of performance metrics across resamples.
#'
#' @param x `SupervisedRes` object.
#' @param what Character vector: "training", "test". What to print. Default is to print both.
#' @param metric Character: Metric to plot.
#' @param ylab Character: Label for the y-axis.
#' @param boxpoints Character:"all", "outliers" - How to display points in the boxplot.
#' @param theme `Theme` object.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(plot_metric, SupervisedRes) <- function(
  x,
  what = c("training", "test"),
  metric = NULL,
  ylab = labelify(metric),
  boxpoints = "all",
  theme = choose_theme(getOption("rtemis_theme")),
  ...
) {
  what <- match.arg(what, several.ok = TRUE)
  .class <- x@type == "Classification"

  # Metric
  if (is.null(metric)) {
    if (.class) {
      metric <- "Balanced_Accuracy"
    } else {
      metric <- "Rsq"
    }
  }

  xl <- list()
  if ("training" %in% what) {
    if (.class) {
      xl[["Training"]] <- sapply(
        x@metrics_training@res_metrics,
        function(fold) {
          fold[["Overall"]][[metric]]
        }
      )
    } else {
      xl[["Training"]] <- sapply(
        x@metrics_training@res_metrics,
        function(fold) {
          fold[[metric]]
        }
      )
    }
  }
  if ("test" %in% what) {
    if (.class) {
      xl[["Test"]] <- sapply(x@metrics_test@res_metrics, function(fold) {
        fold[["Overall"]][[metric]]
      })
    } else {
      xl[["Test"]] <- sapply(x@metrics_test@res_metrics, function(fold) {
        fold[[metric]]
      })
    }
  }

  # Boxplot ----
  draw_box(xl, theme = theme, ylab = ylab, boxpoints = boxpoints, ...)
} # /rtemis::plot_metric.SupervisedRes


# Plot Variable Importance Supervised ----
method(plot_varimp, Supervised) <- function(
  x,
  theme = choose_theme(getOption("rtemis_theme")),
  filename = NULL,
  ...
) {
  if (is.null(x@varimp)) {
    msg(highlight2("No variable importance available."))
    return(invisible())
  }
  draw_varimp(x@varimp, theme = theme, filename = filename, ...)
} # /rtemis::plot_varimp.Supervised


# Plot Variable Importance SupervisedRes ----
method(plot_varimp, SupervisedRes) <- function(
  x,
  ylab = NULL,
  summarize_fn = "mean",
  show_top = 20L,
  theme = choose_theme(getOption("rtemis_theme")),
  filename = NULL,
  ...
) {
  if (is.null(x@varimp)) {
    msg(highlight2("No variable importance available."))
    return(invisible())
  }
  check_inherits(summarize_fn, "character")

  # ! Variable importance may be returned in different order in each resample !
  # Order varimp vectors by variable names
  # First, check each varimp vector is named
  if (!all(sapply(x@varimp, function(z) !is.null(names(z))))) {
    cli::cli_abort("Variable importance elements must be named vectors.")
  }
  # Not every variable gets a variable importance score necessarily
  # Each varimp vector as a one row data.table in order to rbindlist them, filling in NAs as needed.
  # x@varimp[[i]] may be named vector or data.frame
  varimp_dt <- lapply(x@varimp, function(z) {
    as.data.table(as.list(z), keep.rownames = TRUE)
  })

  varimp <- rbindlist(varimp_dt, use.names = TRUE, fill = TRUE)
  # Convert NA values to 0
  setDF(varimp)
  varimp[is.na(varimp)] <- 0
  # Summarize variable importance
  varimp_summary <- apply(varimp, 2, summarize_fn)
  # Sort columns by descending variable importance
  varimp_sorted <- varimp_summary[order(-varimp_summary)]
  if (length(varimp_sorted) > show_top) {
    varimp_sorted <- varimp_sorted[seq_len(show_top)]
  }
  # ylab
  if (is.null(ylab)) {
    ylab <- paste0(
      labelify(paste(summarize_fn, "Variable Importance")),
      "\n(across ",
      desc(x@outer_resampler),
      ")"
    )
  }
  draw_varimp(
    varimp_sorted,
    theme = theme,
    ylab = ylab,
    filename = filename,
    ...
  )
} # /rtemis::plot_varimp.SupervisedRes


# Make SupervisedRes ----
#' Make SupervisedRes
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_SupervisedRes <- function(
  algorithm,
  type,
  models,
  preprocessor,
  hyperparameters,
  tuner_config,
  outer_resampler,
  execution_config,
  y_training,
  y_test,
  predicted_training,
  predicted_test,
  predicted_prob_training,
  predicted_prob_test,
  se_training = NULL,
  se_test = NULL,
  xnames = character(),
  varimp = NULL,
  question = character(),
  extra = NULL
) {
  if (type == "Classification") {
    ClassificationRes(
      algorithm = algorithm,
      models = models,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner_config = tuner_config,
      outer_resampler = outer_resampler,
      execution_config = execution_config,
      y_training = y_training,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_test = predicted_test,
      predicted_prob_training = predicted_prob_training,
      predicted_prob_test = predicted_prob_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  } else {
    RegressionRes(
      algorithm = algorithm,
      models = models,
      preprocessor = preprocessor,
      hyperparameters = hyperparameters,
      tuner_config = tuner_config,
      outer_resampler = outer_resampler,
      execution_config = execution_config,
      y_training = y_training,
      y_test = y_test,
      predicted_training = predicted_training,
      predicted_test = predicted_test,
      se_training = se_training,
      se_test = se_test,
      xnames = xnames,
      varimp = varimp,
      question = question,
      extra = extra
    )
  }
} # /rtemis::make_SupervisedRes

early_stopping_algs <- c("LightGBM", "LightRF", "LightRuleFit")


# LightRuleFit ----
#' @title LightRuleFit
#'
#' @description
#' Class for LightRuleFit models.
#'
#' @author EDG
#' @noRd
LightRuleFit <- new_class(
  name = "LightRuleFit",
  properties = list(
    model_lightgbm = Supervised,
    model_glmnet = Supervised,
    rules = class_character,
    rules_coefs = class_data.frame,
    rules_index = class_integer,
    rules_selected = class_character,
    rules_selected_formatted = class_character,
    rules_selected_formatted_coefs = class_data.frame,
    y_levels = class_character | NULL,
    xnames = class_character,
    complexity_metrics = class_data.frame
  )
) # /rtemis::LightRuleFit

# Print LightRuleFit ----
method(print, LightRuleFit) <- function(x, ...) {
  objcat("rtemis LightRuleFit Model")
  cat(
    "Trained using ",
    highlight(x@model_lightgbm@algorithm),
    " and ",
    highlight(x@model_glmnet@algorithm),
    ".\n",
    sep = ""
  )
  cat("Selected", highlight(length(x@rules_selected)), "rules.\n")
  invisible(x)
} # /rtemis::print.LightRuleFit


# get_metric Regression ----
method(get_metric, Regression) <- function(x, set, metric) {
  prop(prop(x, paste0("metrics_", set)), "metrics")[[metric]]
} # /get_metric.Regression

# get_metric Classification ----
method(get_metric, Classification) <- function(x, set, metric) {
  prop(prop(x, paste0("metrics_", set)), "metrics")[["Overall"]][[metric]]
} # /get_metric.Classification

# get_metric RegressionRes ----
method(get_metric, RegressionRes) <- function(x, set, metric) {
  sapply(
    prop(prop(x, paste0("metrics_", set)), "res_metrics"),
    function(r) {
      r[[metric]]
    }
  )
} # /rtemis::get_metric.RegressionRes

# get_metric ClassificationRes ----
method(get_metric, ClassificationRes) <- function(x, set, metric) {
  sapply(
    prop(prop(x, paste0("metrics_", set)), "res_metrics"),
    function(r) {
      r[["Overall"]][[metric]]
    }
  )
} # /rtemis::get_metric.ClassificationRes


# Describe list of Supervised/Res ----
#' Describe multiple Supervised or SupervisedRes objects
#'
#' @param x List of `Supervised` or `SupervisedRes` objects.
#' @param metric Character: Metric to use for description. Default is NULL, which uses "Balanced_Accuracy" for Classification and "Rsq" for Regression.
#' @param decimal_places Integer: Number of decimal places to round metrics to.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
method(desc, class_list) <- function(
  x,
  metric = NULL,
  decimal_places = 3L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  # Check all elements are Supervised or all are SupervisedRes objects
  if (
    !all(sapply(x, S7_inherits, Supervised)) &&
      !all(sapply(x, S7_inherits, SupervisedRes))
  ) {
    cli::cli_abort(
      "All elements must be either Supervised or SupervisedRes objects"
    )
  }
  type <- if (S7_inherits(x[[1]], SupervisedRes)) {
    "SupervisedRes"
  } else {
    "Supervised"
  }

  # Check that all models are of the same type
  if (!all(sapply(x, function(m) m@type == x[[1]]@type))) {
    cli::cli_abort(
      "All objects must be of the same supervised learning type (Classification or Regression)."
    )
  }

  # Name list using algorithm names
  if (is.null(names(x))) {
    names(x) <- sapply(x, function(m) m@algorithm)
  }
  suptype <- x[[1]]@type

  # SupervisedRes ----
  if (type == "SupervisedRes") {
    # Check that the same resampling method was used - ideally same seed, but do not enforce that, but report it
    # Get resampling config from each
    res_params <- lapply(x, function(m) m@outer_resampler@config)
    # Check all resamplers of same type
    if (!all(sapply(res_params, function(p) p@type == res_params[[1]]@type))) {
      cli::cli_warn(
        "All SupervisedRes objects must use the same resampling method."
      )
    }
    # ?replace with loop that checks all resampler params
    # Check all resamplers use same n
    if (!all(sapply(res_params, function(p) p@n == res_params[[1]]@n))) {
      cli::cli_warn(
        "All SupervisedRes objects must use the same number of resamples."
      )
    }

    # Describe SupervisedRes objects
    # 1. Report names of algorithms used.
    out <- paste0(
      oxfordcomma(sapply(x, function(m) desc_abb_alg(m@algorithm))),
      " were used for ",
      suptype,
      ".\n"
    )
    # 2. Get metric
    if (is.null(metric)) {
      metric <- if (suptype == "Classification") {
        "Balanced_Accuracy"
      } else {
        "Rsq"
      }
    }
    metricv <- sapply(x, function(m) m@metrics_test@mean_metrics[[metric]])
  } # /SupervisedRes
  # => Supervised ----
  if (type == "Supervised") {
    # Describe SupervisedRes objects
    # 1. Report names of algorithms used.
    out <- paste0(
      oxfordcomma(sapply(x, function(m) desc_abb_alg(m@algorithm))),
      " were used for ",
      suptype,
      ".\n"
    )
    # 2. Get metric
    if (is.null(metric)) {
      metric <- if (suptype == "Classification") {
        "Balanced_Accuracy"
      } else {
        "Rsq"
      }
    }
    if (suptype == "Classification") {
      # Classification
      metricv <- sapply(x, function(m) {
        m@metrics_test@metrics[["Overall"]][[metric]]
      })
    } else {
      # Regression
      metricv <- sapply(x, function(m) m@metrics_test@metrics[[metric]])
    }
  } # /rtemis::Supervised

  # 3. Report mean metric across all models, sorting by performance
  metric_sorted <- sort(metricv, decreasing = TRUE)
  # => Get ties at specified decimal_places
  out <- paste0(
    out,
    "The top-performing model was ",
    bold(names(metric_sorted)[1], output_type = output_type),
    " with a test-set ",
    bold(labelify(metric), output_type = output_type),
    " of ",
    bold(
      ddSci(metric_sorted[1], decimal_places = decimal_places),
      output_type = output_type
    ),
    ", followed by ",
    oxfordcomma(names(metric_sorted[-1])),
    " with ",
    metric,
    " of ",
    oxfordcomma(ddSci(metric_sorted[-1], decimal_places = decimal_places)),
    " respectively."
  )
  out
} # /rtemis::desc.list


#' Print description of a list of Supervised or SupervisedRes objects
#'
#' @param x List of `Supervised` or `SupervisedRes` objects.
#'
#' @return Character of description invisibly. Prints description to output.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
method(describe, class_list) <- function(
  x,
  metric = NULL,
  decimal_places = 3L,
  output_type = NULL,
  ...
) {
  out <- desc(
    x,
    metric = metric,
    decimal_places = decimal_places,
    output_type = output_type
  )
  cat(out, "\n")
  invisible(out)
} # /rtemis::describe.list(Supervised/Res)
