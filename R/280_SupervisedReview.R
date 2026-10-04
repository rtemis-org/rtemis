# 280_SupervisedReview.R
# ::rtemis::
# 2026- EDG rtemis.org

# What `review()` reports about a trained supervised model.
#
# A review holds every value it rests on, in typed tables -- the sample, one
# row per performance metric, one row per baseline comparison, one row per
# tuned hyperparameter checked -- so a reader who knows the algorithm and the
# field can judge for themselves. Its findings then state what the review
# concludes, each message restating the values it rests on. `repr` prints a
# summary: one row per metric, the baseline comparisons, the findings; the
# full tables stay on the object.
#
# Findings follow the `Diagnostic` conventions: `code` is a stable identifier,
# permanent once published; `plain` is authored once per code in
# `REVIEW_PLAIN` and looked up by code; `message` is the technical account.
#
# A finding fires on an exact condition, an interval, or a published rule of
# thumb (cases per predictor). Where no defensible threshold exists -- how wide
# an interval is "too wide", how large a class imbalance matters -- the value
# is reported in the tables and no finding is made from it.
#
# spec: rtemis/review-method

# %% REVIEW_CODES ----
# The finding vocabulary, in the order `review()` reports it: what decides
# whether the evaluation can be trusted first, then what it shows.
REVIEW_CODES <- c(
  "NO_TEST_SET",
  "SINGLE_SPLIT",
  "TEST_PRECISION",
  "ABSENT_TEST_CLASSES",
  "FOLD_VARIATION",
  "DIM_P_GT_N",
  "FEW_CASES_PER_PREDICTOR",
  "PRESELECTION_RISK",
  "CONSTANT_PREDICTIONS",
  "CLASS_NEVER_PREDICTED",
  "BASELINE_ACCURACY",
  "BASELINE_BALANCED_ACCURACY",
  "BASELINE_AUC",
  "BASELINE_BRIER",
  "BASELINE_MSE",
  "GENERALIZATION_GAP",
  "TUNING_GRID_EDGE"
)

# %% REVIEW_SEVERITIES ----
# - "warning" the evaluation is unreliable, or shows a problem with the model
# - "note"    information worth knowing
REVIEW_SEVERITIES <- c("warning", "note")

# %% REVIEW_BASELINE_OUTCOMES ----
# How a model compares with the baseline on one metric, read from an interval:
# its lower end above the baseline is "better", its upper end below is
# "worse", and an interval containing the baseline is "indistinguishable".
REVIEW_BASELINE_OUTCOMES <- c("better", "worse", "indistinguishable")

# %% REVIEW_BASELINE_REFERENCES ----
# What a baseline comparison is against: the most common training class, the
# chance level of a metric (1/K for balanced accuracy, 0.5 for AUC), the
# training proportion of the positive class as a constant probability, or the
# training mean.
REVIEW_BASELINE_REFERENCES <- c(
  "majority_class",
  "chance",
  "training_prevalence",
  "training_mean"
)

# %% REVIEW_BASELINE_METHODS ----
# How a comparison was made: the exact McNemar test of paired correctness, the
# metric's confidence interval against a fixed reference, a paired t interval
# of the per-case loss reduction, or a description without inference.
REVIEW_BASELINE_METHODS <- c(
  "exact_mcnemar",
  "interval_vs_reference",
  "paired_t",
  "descriptive"
)

# %% REVIEW_PLAIN ----
# The plain-language text for each code, written by hand for a reader with no
# statistics background. It says what the check is about and why it matters;
# the values are in the review's tables and the technical account in the message.
REVIEW_PLAIN <- c(
  NO_TEST_SET = paste0(
    "The model was evaluated only on the cases it was trained on. ",
    "Performance on training cases usually overstates performance on new ",
    "cases, so it says little about how the model will generalize."
  ),
  SINGLE_SPLIT = paste0(
    "The data was split once into training and test cases. A different split ",
    "could give a different result, especially with few cases. Resampling ",
    "repeats the split, which gives a more stable estimate and shows how much ",
    "it varies."
  ),
  TEST_PRECISION = paste0(
    "Performance measured on a limited number of test cases is an estimate. ",
    "The confidence interval shows the range of performance consistent with ",
    "the test results; other things equal, fewer test cases give a wider ",
    "interval."
  ),
  ABSENT_TEST_CLASSES = paste0(
    "Some outcome classes have no test cases, so performance on those ",
    "classes, and metrics that average over every class, cannot be assessed."
  ),
  FOLD_VARIATION = paste0(
    "The data was split several times, each time training on one part and ",
    "testing on the rest. The variation in performance between splits shows ",
    "how much the estimate depends on which cases were used for training and ",
    "testing. The splits share training cases, so their results are not ",
    "independent and are described rather than tested."
  ),
  DIM_P_GT_N = paste0(
    "The model received more predictors than there are training cases. In ",
    "this situation a model can fit the training cases almost perfectly even ",
    "when the predictors carry no information about the outcome, so only ",
    "performance on held-out cases is informative."
  ),
  FEW_CASES_PER_PREDICTOR = paste0(
    "There are few training cases for each predictor. With little data per ",
    "predictor, a model can fit patterns that occur by chance and do not hold ",
    "in new cases. The threshold is a rule of thumb, not a requirement."
  ),
  PRESELECTION_RISK = paste0(
    "With many predictors and few cases, predictors are often selected before ",
    "modeling. Selection done by rtemis as part of training uses the training ",
    "cases only; selection done on all the data before it was passed to ",
    "rtemis biases evaluation, and the review cannot determine whether that ",
    "happened."
  ),
  CONSTANT_PREDICTIONS = paste0(
    "The model made the same prediction for every test case."
  ),
  CLASS_NEVER_PREDICTED = paste0(
    "Some outcome classes occur among the test cases but were never ",
    "predicted. This often happens with rare classes, where predicting only ",
    "the common classes maximizes accuracy."
  ),
  BASELINE_ACCURACY = paste0(
    "Compares the proportion of correct predictions with that of always ",
    "predicting the most common class in the training data, case by case. ",
    "When one class is much larger than the others, that rule alone is ",
    "correct for most cases."
  ),
  BASELINE_BALANCED_ACCURACY = paste0(
    "Balanced accuracy is the average, over classes, of the proportion of ",
    "each class predicted correctly, so a rare class counts as much as a ",
    "common one. Its chance level, reached by any constant prediction, is one ",
    "divided by the number of classes."
  ),
  BASELINE_AUC = paste0(
    "AUC measures how well the predicted probabilities rank cases of the ",
    "positive class above cases of the other class. Its chance level, ",
    "reached by random scores, is 0.5."
  ),
  BASELINE_BRIER = paste0(
    "Compares the predicted probabilities with a constant forecast: the ",
    "proportion of the positive class in the training data, given to every ",
    "case. The Brier score is the mean squared difference between predicted ",
    "probability and outcome; lower is better."
  ),
  BASELINE_MSE = paste0(
    "Compares the model's prediction errors with those of predicting the ",
    "training mean for every case."
  ),
  GENERALIZATION_GAP = paste0(
    "The model performs better on its training cases than on held-out cases, ",
    "by more than the uncertainty of the test estimate explains. This may ",
    "indicate overfitting -- fitting patterns specific to the training cases ",
    "-- and differences between the training and test cases may also ",
    "contribute. Constraining model complexity usually reduces overfitting."
  ),
  TUNING_GRID_EDGE = paste0(
    "The selected value of a tuned hyperparameter was the smallest or largest ",
    "value tried, so a better value may lie outside the range searched."
  )
)

# Every code carries its text, and no text is orphaned. Checked at load.
stopifnot(setequal(names(REVIEW_PLAIN), REVIEW_CODES))

# %% REVIEW_LIMITATIONS ----
# What no review of metrics can establish, stated in every review.
REVIEW_LIMITATIONS <- c(
  paste0(
    "Whether the model is useful depends on the question it addresses, the ",
    "costs of different errors, and how its predictions will be used; ",
    "performance metrics cannot establish this."
  ),
  paste0(
    "Preprocessing, decomposition and tuning done by rtemis are fitted on the ",
    "training cases of each split and only applied to its test cases. Steps ",
    "taken before the data was passed to rtemis -- selecting or filtering ",
    "predictors, transforming or removing cases using all the data, or ",
    "choosing among models by their test performance -- can bias evaluation; ",
    "this review cannot determine whether that happened."
  ),
  paste0(
    "Performance estimates apply to cases from the population the data was ",
    "drawn from; performance on a different population or setting may differ."
  )
)


# %% ReviewFinding ----
#' ReviewFinding Class
#'
#' @description
#' One finding from `review()`: a stable code, how much it matters, the
#' technical and plain-language accounts, and a suggestion where one exists.
#'
#' @field code Character \{"NO_TEST_SET", "SINGLE_SPLIT", "TEST_PRECISION", "ABSENT_TEST_CLASSES", "FOLD_VARIATION", "DIM_P_GT_N", "FEW_CASES_PER_PREDICTOR", "PRESELECTION_RISK", "CONSTANT_PREDICTIONS", "CLASS_NEVER_PREDICTED", "BASELINE_ACCURACY", "BASELINE_BALANCED_ACCURACY", "BASELINE_AUC", "BASELINE_BRIER", "BASELINE_MSE", "GENERALIZATION_GAP", "TUNING_GRID_EDGE"\}:
#'   Stable identifier for the kind of finding. Permanent once published.
#' @field severity Character \{"warning", "note"\}: How much the finding
#'   matters.
#' @field message Character: Technical account, restating the values it rests
#'   on.
#' @field plain Character: Plain-language account, authored per code.
#' @field suggestion Optional Character: What to do about it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ReviewFinding <- schema_class(
  name = "ReviewFinding",
  package = "rtemis",
  properties = list(
    code = prop_string(
      REVIEW_CODES[[1L]],
      enum = REVIEW_CODES,
      description = "Stable identifier for the kind of finding."
    ),
    severity = prop_string(
      REVIEW_SEVERITIES[[1L]],
      enum = REVIEW_SEVERITIES,
      description = "How much the finding matters: 'warning' the evaluation is unreliable or shows a problem with the model, 'note' worth knowing."
    ),
    message = prop_string(
      "",
      description = "Technical account of the finding, restating the values it rests on."
    ),
    plain = prop_string(
      "",
      description = "Plain-language account of the finding, written for a reader with no statistics background."
    ),
    suggestion = prop_string(
      NULL,
      nullable = TRUE,
      description = "What to do about the finding. Unset where nothing follows from it."
    )
  ),
  publication = SchemaPublication(
    role = "document",
    slug = "reviewfinding",
    title = "rtemis ReviewFinding",
    description = "One finding from reviewing a trained supervised model: a stable code, how much it matters, the technical and plain-language accounts of it, and a suggestion where one exists.",
    order = 28L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::ReviewFinding


# %% new_review_finding ----
#' Build a `ReviewFinding`, taking its plain text from the code
#'
#' @param code Character: One of `REVIEW_CODES`.
#' @param severity Character \{"warning", "note"\}: Severity.
#' @param message Character: Technical account.
#' @param suggestion Optional Character: What to do about it.
#'
#' @return `ReviewFinding` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_review_finding <- function(code, severity, message, suggestion = NULL) {
  ReviewFinding(
    code = code,
    severity = severity,
    message = message,
    plain = unname(REVIEW_PLAIN[[code]]),
    suggestion = suggestion
  )
} # /rtemis::new_review_finding


# %% prop_review_count ----
prop_review_count <- function(description, nullable = TRUE) {
  if (nullable) {
    prop_integer(NULL, min = 0L, nullable = TRUE, description = description)
  } else {
    prop_integer(min = 0L, description = description)
  }
}


# %% prop_review_value ----
prop_review_value <- function(description) {
  prop_float(NULL, nullable = TRUE, description = description)
}


# %% SupervisedReview ----
#' SupervisedReview Class
#'
#' @description
#' What `review()` returns for a trained supervised model: the sample, its
#' performance and how it compares with a baseline, the tuning checked, the
#' findings drawn from them, and what a review of metrics cannot establish.
#'
#' @field algorithm Character: Algorithm of the reviewed model.
#' @field type Character \{"Regression", "Classification"\}: Kind of supervised
#'   learning.
#' @field description Character: Methods-style description of the model, as
#'   `describe()` gives it.
#' @field confidence_level Numeric (0, 1): Confidence level of every interval.
#' @field min_cases_per_predictor Numeric (0, Inf): Cases per predictor below
#'   which the review warns.
#' @field sample List: Sample sizes and the number of predictors.
#' @field class_counts Optional data.frame: Cases per class (classification).
#' @field performance data.frame: One row per metric.
#' @field baseline Optional data.frame: One row per baseline comparison; unset
#'   without a test set.
#' @field tuning Optional data.frame: One row per tuned hyperparameter checked.
#' @field findings List of `ReviewFinding` objects.
#' @field limitations Character vector: What a review of metrics cannot
#'   establish.
#'
#' @author EDG
#' @noRd
SupervisedReview <- schema_class(
  name = "SupervisedReview",
  package = "rtemis",
  properties = list(
    algorithm = prop_string(
      description = "Algorithm identifier of the reviewed model."
    ),
    type = prop_string(
      SUPERVISED_TYPES[[1L]],
      enum = SUPERVISED_TYPES,
      description = "Kind of supervised learning the reviewed model performs."
    ),
    description = prop_string(
      "",
      description = "Methods-style description of the reviewed model."
    ),
    confidence_level = prop_float(
      0.95,
      exclusive_min = 0,
      exclusive_max = 1,
      description = "Confidence level of every interval in the review."
    ),
    min_cases_per_predictor = prop_float(
      10,
      exclusive_min = 0,
      description = "Training cases per learner column (minority-class cases for classification) below which the review notes a rule-of-thumb shortfall."
    ),
    sample = prop_state(prop_struct(
      members = list(
        n_training = prop_review_count(
          "Training cases, each counted once; for resampled models, the smallest training set over resamples.",
          nullable = FALSE
        ),
        n_test = prop_review_count(
          "Test predictions; for resampled models, all out-of-sample predictions over resamples. Unset without a test set."
        ),
        n_test_cases = prop_review_count(
          "Distinct cases predicted out of sample; fewer than n_test when test sets overlap. Unset without a test set."
        ),
        n_resamples = prop_review_count(
          "Successful outer resamples. Unset for a single split."
        ),
        n_resamples_requested = prop_review_count(
          "Requested outer resamples. Unset for a single split."
        ),
        n_predictors = prop_review_count(
          "Input predictors: the training data's columns other than the outcome. Unset when the training data has no fingerprint."
        ),
        n_learner_columns = prop_review_count(
          "Columns the learner received after preprocessing and decomposition: retained predictors plus components, before any encoding inside the algorithm; for resampled models, at the resample with the fewest cases per column.",
          nullable = FALSE
        ),
        n_components = prop_review_count(
          "Components the fitted decomposition produced. Unset without a decomposition."
        ),
        decomposition = prop_string(
          NULL,
          nullable = TRUE,
          description = "Decomposition algorithm preceding the learner. Unset without one."
        ),
        cases_per_predictor = prop_float(
          0,
          min = 0,
          description = "Training cases (minority-class cases for classification) per learner column, at the resample with the fewest."
        )
      ),
      required = c(
        "n_training",
        "n_test",
        "n_test_cases",
        "n_resamples",
        "n_resamples_requested",
        "n_predictors",
        "n_learner_columns",
        "n_components",
        "decomposition",
        "cases_per_predictor"
      ),
      description = "Sample sizes and the number of predictors."
    )),
    class_counts = prop_state(prop_table(
      columns = list(
        level = prop_string(description = "Outcome level."),
        training = prop_review_count(
          "Training cases of the level; for resampled models, the smallest over resamples.",
          nullable = FALSE
        ),
        test = prop_review_count(
          "Out-of-sample predictions of cases of the level. Unset without a test set."
        )
      ),
      nullable = TRUE,
      description = "Cases per outcome level, one row each. Unset for regression."
    )),
    performance = prop_state(prop_table(
      columns = list(
        metric = prop_string(description = "Metric name."),
        training = prop_review_value(
          "Training value; for resampled models, the mean over resamples."
        ),
        training_sd = prop_review_value(
          "Standard deviation of the training value over resamples. Unset for a single split."
        ),
        test = prop_review_value(
          "Test value; for resampled models, the mean over resamples. Unset without a test set."
        ),
        test_sd = prop_review_value(
          "Standard deviation of the test value over resamples. Unset for a single split."
        ),
        difference = prop_review_value(
          "Training minus test value (of the means, for resampled models)."
        ),
        pooled = prop_review_value(
          "Value over all out-of-sample predictions pooled across resamples, descriptive. Set for resampled models whose test sets do not overlap, for metrics that average over cases; unset for AUC, which would rank scores from different fitted models together."
        ),
        lower = prop_review_value(
          "Lower end of the confidence interval of the test value. Set for a single split only: resamples share training cases, so their results are not independent."
        ),
        upper = prop_review_value(
          "Upper end of the confidence interval of the test value."
        )
      ),
      min_items = 1L,
      description = "One row per performance metric the model reports."
    )),
    baseline = prop_state(prop_table(
      columns = list(
        metric = prop_string(description = "Metric compared."),
        reference = prop_string(
          enum = REVIEW_BASELINE_REFERENCES,
          description = "What the model is compared with: 'majority_class' always predicts the most common training class; 'chance' is the metric's chance level (1/K for balanced accuracy, 0.5 for AUC); 'training_prevalence' gives every case the training proportion of the positive class as its probability; 'training_mean' predicts the training mean. For resampled models each resample's reference is fit to its own training data."
        ),
        method = prop_string(
          enum = REVIEW_BASELINE_METHODS,
          description = "How the comparison was made: 'exact_mcnemar' tests paired correctness; 'interval_vs_reference' compares the metric's confidence interval with the reference; 'paired_t' is a t interval of the per-case loss reduction; 'descriptive' reports values without inference, as for resampled models."
        ),
        model = prop_review_value(
          "Model value: the test value for a single split, the mean over resamples otherwise."
        ),
        model_lower = prop_review_value(
          "Lower end of the confidence interval of the model value. Single split only."
        ),
        model_upper = prop_review_value(
          "Upper end of the confidence interval of the model value."
        ),
        baseline = prop_review_value(
          "Reference value: on the test cases for a single split, the mean over resamples otherwise."
        ),
        difference = prop_review_value(
          "Improvement of the model over the reference, positive when it favors the model: model minus reference for accuracy-type metrics, reference loss minus model loss for loss metrics; the mean over resamples for resampled models."
        ),
        difference_lower = prop_review_value(
          "Lower end of the confidence interval of the difference. Set for 'interval_vs_reference' and 'paired_t' comparisons."
        ),
        difference_upper = prop_review_value(
          "Upper end of the confidence interval of the difference."
        ),
        skill = prop_review_value(
          "Skill score, one minus the model's mean loss over the reference's, as a point estimate. Loss metrics only."
        ),
        p_value = prop_review_value(
          "Two-sided p-value of the exact McNemar test. 'exact_mcnemar' comparisons only."
        ),
        outcome = prop_string(
          NULL,
          enum = REVIEW_BASELINE_OUTCOMES,
          nullable = TRUE,
          description = "Two-sided verdict at the review's confidence level. Unset when no inference is made or the interval is unavailable."
        ),
        resamples_better = prop_review_count(
          "Resamples whose model value beats their own reference. Resampled models only."
        )
      ),
      nullable = TRUE,
      description = "One row per comparison with a reference that ignores the predictors. Unset without a test set."
    )),
    tuning = prop_state(prop_table(
      columns = list(
        hyperparameter = prop_string(description = "Tuned hyperparameter."),
        min = prop_review_value("Smallest value searched."),
        max = prop_review_value("Largest value searched."),
        n_values = prop_review_count(
          "Number of distinct values searched.",
          nullable = FALSE
        ),
        selected = prop_review_value(
          "Selected value. Unset for resampled models, which select one per resample."
        ),
        n_at_edge = prop_review_count(
          "Resamples (1 for a single split) selecting a smallest or largest value that could be extended."
        )
      ),
      nullable = TRUE,
      description = "One row per numeric hyperparameter searched over at least three values. Unset for an untuned model."
    )),
    findings = prop_state(prop_collection(
      ReviewFinding,
      description = "Findings, in reporting order."
    )),
    limitations = prop_string(
      "",
      vector = TRUE,
      description = "What a review of performance metrics cannot establish."
    )
  ),
  constructor = function(
    algorithm,
    type,
    description,
    confidence_level,
    min_cases_per_predictor,
    sample,
    performance,
    class_counts = NULL,
    baseline = NULL,
    tuning = NULL,
    findings = list(),
    limitations = REVIEW_LIMITATIONS
  ) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      type = type,
      description = description,
      confidence_level = confidence_level,
      min_cases_per_predictor = min_cases_per_predictor,
      sample = sample,
      class_counts = class_counts,
      performance = performance,
      baseline = baseline,
      tuning = tuning,
      findings = findings,
      limitations = limitations
    )
  },
  publication = SchemaPublication(
    role = "document",
    slug = "supervisedreview",
    title = "rtemis SupervisedReview",
    description = "A review of a trained supervised model: the sample, one row per performance metric with intervals, comparisons with a baseline predictor, tuned hyperparameters checked against their search range, findings drawn from them, and what a review of metrics cannot establish.",
    order = 29L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::SupervisedReview


# %% review_codes ----
#' Codes present in a `SupervisedReview`
#'
#' @param x `SupervisedReview` object.
#'
#' @return Character vector, one entry per finding, in reporting order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
review_codes <- function(x) {
  vapply(x@findings, function(f) f@code, character(1L))
} # /rtemis::review_codes


# %% fmt_review_cell ----
# A value to three decimal places, or empty when unset. Fixed notation, so a
# column of values lines up and a small difference reads 0.003, not 3e-03.
fmt_review_cell <- function(value) {
  if (is.null(value) || is.na(value)) "" else sprintf("%.3f", value)
}


# %% fmt_review_setting ----
# A hyperparameter value: whole numbers as integers.
fmt_review_setting <- function(value) {
  if (is.na(value)) {
    ""
  } else if (value == round(value)) {
    format(value, scientific = FALSE)
  } else {
    format(signif(value, 4L))
  }
}


# %% review_sample_line ----
# One sentence on the sample: cases, resamples and predictors.
review_sample_line <- function(x) {
  s <- x@sample
  resampled <- !is.null(s[["n_resamples"]])
  cases <- if (resampled) {
    paste0(
      s[["n_resamples"]],
      " resamples",
      if (s[["n_resamples"]] != s[["n_resamples_requested"]]) {
        paste0(" of ", s[["n_resamples_requested"]], " requested")
      },
      ": ",
      s[["n_test"]],
      " out-of-sample predictions of ",
      s[["n_test_cases"]],
      " cases, at least ",
      s[["n_training"]],
      " training cases per resample"
    )
  } else if (!is.null(s[["n_test"]])) {
    paste0(
      s[["n_training"]],
      " training and ",
      s[["n_test"]],
      ngettext(s[["n_test"]], " test case", " test cases")
    )
  } else {
    paste0(s[["n_training"]], " training cases, no test set")
  }
  columns <- s[["n_learner_columns"]]
  # The learner's columns are worth naming only when they differ from the
  # input predictors: after a decomposition, or when the input is unknown.
  same <- identical(s[["n_predictors"]], columns) &&
    is.null(s[["n_components"]])
  unit <- if (same) "predictor" else "learner column"
  predictors <- paste0(
    if (!is.null(s[["n_predictors"]])) {
      paste0(
        s[["n_predictors"]],
        ngettext(s[["n_predictors"]], " predictor", " predictors"),
        if (!same) ", "
      )
    },
    if (!same) {
      paste0(
        columns,
        ngettext(columns, " learner column", " learner columns"),
        if (!is.null(s[["n_components"]])) {
          paste0(
            " including ",
            s[["n_components"]],
            ngettext(s[["n_components"]], " component", " components"),
            " from ",
            s[["decomposition"]]
          )
        }
      )
    },
    " (",
    ddSci(s[["cases_per_predictor"]], decimal_places = 1L),
    if (x@type == "Classification") " minority-class" else "",
    " training cases per ",
    unit,
    ")"
  )
  paste0(cases, "; ", predictors, ".")
} # /rtemis::review_sample_line


# %% review_text_table ----
# Left-align the first column, right-align the rest, two spaces apart.
review_text_table <- function(table, indent) {
  widths <- apply(table, 2L, function(col) max(nchar(col)))
  apply(table, 1L, function(row) {
    cells <- vapply(
      seq_along(row)[-1L],
      function(k) formatC(row[[k]], width = widths[[k]]),
      character(1L)
    )
    paste0(
      indent,
      "  ",
      formatC(row[[1L]], width = -widths[[1L]]),
      paste0("  ", cells, collapse = "")
    )
  })
} # /rtemis::review_text_table


# %% review_performance_lines ----
# One row per metric: training, test, training minus test; "mean (SD)" when
# resampled.
review_performance_lines <- function(x, indent) {
  p <- x@performance
  resampled <- !is.null(x@sample[["n_resamples"]])
  cell <- function(value, sd) {
    if (resampled && !is.na(value) && !is.na(sd)) {
      paste0(fmt_review_cell(value), " (", fmt_review_cell(sd), ")")
    } else {
      fmt_review_cell(value)
    }
  }
  has_test <- !all(is.na(p[["test"]]))
  rows <- lapply(seq_len(NROW(p)), function(i) {
    c(
      label_metrics(p[["metric"]][[i]]),
      cell(p[["training"]][[i]], p[["training_sd"]][[i]]),
      if (has_test) {
        c(
          cell(p[["test"]][[i]], p[["test_sd"]][[i]]),
          fmt_review_cell(p[["difference"]][[i]])
        )
      }
    )
  })
  header <- if (has_test) {
    c("", "Training", "Test", "Training - test")
  } else {
    c("", "Training")
  }
  review_text_table(do.call(rbind, c(list(header), rows)), indent)
} # /rtemis::review_performance_lines


# %% REVIEW_REFERENCE_TEXT ----
# How each baseline reference reads in a printed summary.
REVIEW_REFERENCE_TEXT <- c(
  majority_class = "most common training class",
  chance = "chance level",
  training_prevalence = "training proportion as probability",
  training_mean = "training mean"
)


# %% review_baseline_lines ----
# One line per comparison: model against its reference, the interval, the
# verdict or, for resampled models, the resamples where the model did better.
review_baseline_lines <- function(x, indent) {
  b <- x@baseline
  level <- fmt_review_level(x@confidence_level)
  interval <- function(lower, upper) {
    if (is.na(lower) || is.na(upper)) {
      ""
    } else {
      paste0(
        " (",
        level,
        " CI ",
        fmt_review_cell(lower),
        " to ",
        fmt_review_cell(upper),
        ")"
      )
    }
  }
  # A comparison with neither value defined has nothing to print.
  shown <- which(!(is.na(b[["model"]]) & is.na(b[["baseline"]])))
  vapply(
    shown,
    function(i) {
      paste0(
        indent,
        "  ",
        label_metrics(b[["metric"]][[i]]),
        " ",
        fmt_review_cell(b[["model"]][[i]]),
        interval(b[["model_lower"]][[i]], b[["model_upper"]][[i]]),
        " vs ",
        fmt_review_cell(b[["baseline"]][[i]]),
        " (",
        REVIEW_REFERENCE_TEXT[[b[["reference"]][[i]]]],
        ")",
        if (b[["method"]][[i]] == "paired_t") {
          paste0(
            "; loss reduction ",
            fmt_review_cell(b[["difference"]][[i]]),
            interval(b[["difference_lower"]][[i]], b[["difference_upper"]][[i]])
          )
        },
        if (!is.na(b[["p_value"]][[i]])) {
          paste0(
            "; McNemar p = ",
            ddSci(b[["p_value"]][[i]], decimal_places = 3L)
          )
        },
        if (!is.na(b[["outcome"]][[i]])) {
          paste0(": ", b[["outcome"]][[i]])
        },
        if (!is.na(b[["resamples_better"]][[i]])) {
          paste0(
            "; better in ",
            b[["resamples_better"]][[i]],
            " of ",
            x@sample[["n_resamples"]],
            " resamples"
          )
        }
      )
    },
    character(1L)
  )
} # /rtemis::review_baseline_lines


# %% repr.SupervisedReview ----
#' repr SupervisedReview
#'
#' A summary: the sample in one sentence, one row per metric, one line per
#' baseline comparison, the tuned hyperparameters, the findings and the
#' limitations. Every value behind them is on the object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, SupervisedReview) <- function(x, pad = 0L, output_type = NULL) {
  indent <- strrep(" ", pad + 2L)
  heading <- function(text) {
    paste0(
      "\n",
      indent,
      fmt(text, bold = TRUE, output_type = output_type),
      "\n"
    )
  }
  resampled <- !is.null(x@sample[["n_resamples"]])
  out <- paste0(
    repr_S7name("SupervisedReview", pad = pad, output_type = output_type),
    indent,
    x@description,
    "\n",
    indent,
    review_sample_line(x),
    "\n"
  )

  # Performance ----
  out <- paste0(
    out,
    heading(
      if (resampled) {
        paste0(
          "Performance, mean (SD) over ",
          x@sample[["n_resamples"]],
          " resamples"
        )
      } else {
        "Performance"
      }
    ),
    paste(review_performance_lines(x, indent), collapse = "\n"),
    "\n"
  )

  # Baseline ----
  if (!is.null(x@baseline)) {
    out <- paste0(
      out,
      heading(
        if (resampled) {
          "Baseline comparisons, mean over resamples"
        } else {
          "Baseline comparisons"
        }
      ),
      paste(review_baseline_lines(x, indent), collapse = "\n"),
      "\n"
    )
  }

  # Tuning ----
  if (!is.null(x@tuning)) {
    t <- x@tuning
    lines <- vapply(
      seq_len(NROW(t)),
      function(i) {
        paste0(
          indent,
          "  ",
          t[["hyperparameter"]][[i]],
          ": searched ",
          fmt_review_setting(t[["min"]][[i]]),
          " to ",
          fmt_review_setting(t[["max"]][[i]]),
          " (",
          t[["n_values"]][[i]],
          " values), ",
          if (resampled) {
            paste0(
              t[["n_at_edge"]][[i]],
              " of ",
              x@sample[["n_resamples"]],
              " resamples at an extendable edge"
            )
          } else {
            paste0("selected ", fmt_review_setting(t[["selected"]][[i]]))
          }
        )
      },
      character(1L)
    )
    out <- paste0(out, heading("Tuning"), paste(lines, collapse = "\n"), "\n")
  }

  # Findings ----
  out <- paste0(out, heading("Findings"))
  if (length(x@findings) == 0L) {
    out <- paste0(out, indent, "  No findings.\n")
  }
  for (f in x@findings) {
    col <- if (f@severity == "warning") {
      rtemis_colors[["orange"]]
    } else {
      rtemis_colors[["blue"]]
    }
    out <- paste0(
      out,
      indent,
      "  * ",
      fmt(f@code, col = col, bold = TRUE, output_type = output_type),
      " (",
      f@severity,
      "): ",
      f@message,
      "\n"
    )
    if (!is.null(f@suggestion)) {
      out <- paste0(
        out,
        indent,
        "    ",
        fmt("Suggestion: ", italic = TRUE, output_type = output_type),
        f@suggestion,
        "\n"
      )
    }
  }

  # Limitations ----
  out <- paste0(out, heading("Limitations"))
  paste0(out, paste0(indent, "  - ", x@limitations, collapse = "\n"), "\n")
} # /rtemis::repr.SupervisedReview


# %% print.SupervisedReview ----
#' Print `SupervisedReview`
#'
#' @param x `SupervisedReview` object.
#' @param pad Integer: Left padding.
#' @param output_type Optional Character: Output format.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, SupervisedReview) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.SupervisedReview
