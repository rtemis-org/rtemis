# 280_SupervisedReview.R
# ::rtemis::
# 2026- EDG rtemis.org

# What `review()` reports about a trained supervised model.
#
# A review has two halves, and the order is the point. `basis` states every
# value the review rests on -- sample sizes, every training and test metric,
# intervals, baseline performance, the settings applied -- before any verdict,
# so a reader who knows the algorithm and the field can judge for themselves.
# `findings` then state what the review concludes, each naming the basis rows
# it rests on rather than carrying numbers of its own, so every value has one
# place in the document. A finding's `message` restates the values for a
# reader; the data is the basis.
#
# Findings follow the `Diagnostic` conventions: `code` is a stable identifier,
# permanent once published; `plain` is authored once per code in
# `REVIEW_PLAIN` and looked up, never composed at runtime or produced by a
# model; `message` is the technical account.
#
# A finding fires on an exact condition, an interval, or a published rule of
# thumb (cases per predictor). Where no defensible threshold exists -- how wide
# an interval is "too wide", how large a class imbalance matters -- the value
# is reported in the basis and no finding is made from it.
#
# spec: rtemis/review-method

# %% REVIEW_CODES ----
# The finding vocabulary, in the order `review()` reports it: what decides
# whether the evaluation can be trusted first, then what it shows.
REVIEW_CODES <- c(
  "NO_TEST_SET",
  "SINGLE_SPLIT",
  "TEST_PRECISION",
  "OVERLAPPING_TEST_SETS",
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
# - "note"    worth knowing; nothing is wrong
REVIEW_SEVERITIES <- c("warning", "note")

# %% REVIEW_BASIS_SECTIONS ----
REVIEW_BASIS_SECTIONS <- c(
  "sample",
  "performance",
  "baseline",
  "tuning",
  "setting"
)

# %% REVIEW_PLAIN ----
# The plain-language text for each code, written by hand for a reader with no
# statistics background. It says what the check is about and why it matters;
# the numbers are in the basis and the technical account in the message.
REVIEW_PLAIN <- c(
  NO_TEST_SET = paste0(
    "The model was only scored on the cases it learned from. A model can ",
    "memorize those cases, so this score says little about how it will do on ",
    "new ones. Keep some cases aside for testing, or let rtemis rotate which ",
    "cases are held out."
  ),
  SINGLE_SPLIT = paste0(
    "The data was split once into a part to learn from and a part to test ",
    "on. A different split could give a noticeably different score, ",
    "especially with few cases. Repeating the split several times and ",
    "averaging gives a steadier estimate and shows how much it varies."
  ),
  TEST_PRECISION = paste0(
    "A score measured on a limited number of test cases is an estimate. The ",
    "range shown is where the model's true performance plausibly lies; the ",
    "fewer the test cases, the wider it is."
  ),
  OVERLAPPING_TEST_SETS = paste0(
    "Some cases were held out and predicted more than once, as happens when ",
    "the data is resampled repeatedly or with replacement. Counting each of ",
    "those predictions as separate evidence would make the model look more ",
    "certain than it is, so the review describes how much the scores vary ",
    "between rounds instead of drawing a range around them."
  ),
  FOLD_VARIATION = paste0(
    "The data was split several times, each time training on one part and ",
    "testing on the rest. How much the score changes from one split to the ",
    "next shows how much any single estimate can be trusted."
  ),
  DIM_P_GT_N = paste0(
    "The model was given more measurements per case than there are cases to ",
    "learn from. In this situation a model can fit the training cases almost ",
    "perfectly even when the measurements carry no real information, so only ",
    "the score on held-out cases means anything."
  ),
  FEW_CASES_PER_PREDICTOR = paste0(
    "There are few cases for each measurement the model can use. With so ",
    "little data per measurement, a model easily picks up chance patterns ",
    "that will not hold in new cases."
  ),
  PRESELECTION_RISK = paste0(
    "When there are many measurements and few cases, it is common to pick the ",
    "most promising measurements first, looking at all the cases. If that ",
    "happened before training, the test cases helped choose the ",
    "measurements, and every score here is too optimistic. Any such choice ",
    "has to be made using the training cases only."
  ),
  CONSTANT_PREDICTIONS = paste0(
    "The model gave the same answer for every test case. It has not learned ",
    "to tell cases apart."
  ),
  CLASS_NEVER_PREDICTED = paste0(
    "Some groups occur among the test cases but the model never predicted ",
    "them. This often happens when a group is rare: the model does better on ",
    "paper by ignoring it, which is rarely what is wanted."
  ),
  BASELINE_ACCURACY = paste0(
    "Compares the share of correct predictions with what you would get by ",
    "always guessing the most common group. When one group is much larger ",
    "than the others, that simple guess is already right most of the time."
  ),
  BASELINE_BALANCED_ACCURACY = paste0(
    "Balanced accuracy averages how often each group is recognized, so a ",
    "rare group counts as much as a common one. Always guessing the same ",
    "group scores one divided by the number of groups."
  ),
  BASELINE_AUC = paste0(
    "AUC measures how well the model ranks cases of one group above the ",
    "other. A score of 0.5 means the ranking is no better than random."
  ),
  BASELINE_BRIER = paste0(
    "Compares the model's predicted probabilities with the simplest ",
    "forecast: giving every case the same probability, the share of that ",
    "group in the training data. A model worth using should forecast better ",
    "than that."
  ),
  BASELINE_MSE = paste0(
    "Compares the model's prediction errors with the errors you would make ",
    "by predicting the training average for every case. A model worth using ",
    "should make smaller errors than that."
  ),
  GENERALIZATION_GAP = paste0(
    "The model does clearly better on the cases it learned from than on new ",
    "ones, by more than chance in the test sample explains. It has learned ",
    "some patterns specific to its training cases, which is called ",
    "overfitting. Settings that make the model simpler usually reduce it."
  ),
  TUNING_GRID_EDGE = paste0(
    "The best setting found was at the edge of the range that was tried. ",
    "The best setting may lie further out, so trying a wider range could ",
    "give a better model."
  )
)

# Every code carries its text, and no text is orphaned. Checked at load.
stopifnot(setequal(names(REVIEW_PLAIN), REVIEW_CODES))

# %% REVIEW_LIMITATIONS ----
# What no review of metrics can establish, stated in every review.
REVIEW_LIMITATIONS <- c(
  paste0(
    "Whether the model is useful depends on the question, the cost of each ",
    "kind of error, and how the predictions will be used; none of these can ",
    "be judged from performance metrics."
  ),
  paste0(
    "Leakage before training -- selecting predictors, tuning preprocessing ",
    "or removing cases using all the data -- cannot be detected from the ",
    "fitted model and makes every estimate optimistic."
  ),
  paste0(
    "The test cases are assumed to come from the population the model will ",
    "be applied to; performance on a different population can differ."
  )
)


# %% ReviewFinding ----
#' ReviewFinding Class
#'
#' @description
#' One finding from `review()`: a stable code, how much it matters, the
#' technical and plain-language accounts, the basis rows it rests on, and a
#' suggestion where one exists.
#'
#' @field code Character \{"NO_TEST_SET", "SINGLE_SPLIT", "TEST_PRECISION", "OVERLAPPING_TEST_SETS", "FOLD_VARIATION", "DIM_P_GT_N", "FEW_CASES_PER_PREDICTOR", "PRESELECTION_RISK", "CONSTANT_PREDICTIONS", "CLASS_NEVER_PREDICTED", "BASELINE_ACCURACY", "BASELINE_BALANCED_ACCURACY", "BASELINE_AUC", "BASELINE_BRIER", "BASELINE_MSE", "GENERALIZATION_GAP", "TUNING_GRID_EDGE"\}:
#'   Stable identifier for the kind of finding. Permanent once published.
#' @field severity Character \{"warning", "note"\}: How much the finding
#'   matters.
#' @field message Character: Technical account, restating the values it rests
#'   on.
#' @field plain Character: Plain-language account, authored per code.
#' @field basis Character vector: Keys of the `SupervisedReview` basis rows the
#'   finding rests on.
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
      description = "Technical account of the finding, restating the basis values it rests on."
    ),
    plain = prop_string(
      "",
      description = "Plain-language account of the finding, written for a reader with no statistics background."
    ),
    basis = prop_string(
      "",
      vector = TRUE,
      unique_items = TRUE,
      description = "Keys of the basis rows the finding rests on."
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
    description = "One finding from reviewing a trained supervised model: a stable code, how much it matters, the technical and plain-language accounts of it, the keys of the basis rows it rests on, and a suggestion where one exists.",
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
#' @param basis Character vector: Basis keys the finding rests on.
#' @param suggestion Optional Character: What to do about it.
#'
#' @return `ReviewFinding` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_review_finding <- function(
  code,
  severity,
  message,
  basis,
  suggestion = NULL
) {
  ReviewFinding(
    code = code,
    severity = severity,
    message = message,
    plain = unname(REVIEW_PLAIN[[code]]),
    basis = basis,
    suggestion = suggestion
  )
} # /rtemis::new_review_finding


# %% SupervisedReview ----
#' SupervisedReview Class
#'
#' @description
#' What `review()` returns for a trained supervised model: the values the
#' review rests on, then the findings drawn from them, then what a review of
#' metrics cannot establish.
#'
#' @field algorithm Character: Algorithm of the reviewed model.
#' @field type Character \{"Regression", "Classification"\}: Kind of supervised
#'   learning.
#' @field description Character: Methods-style description of the model, as
#'   `describe()` gives it.
#' @field basis data.frame: One row per value, with columns `key`, `section`,
#'   `label` and `value`.
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
    basis = prop_state(prop_table(
      columns = list(
        key = prop_string(
          description = "Stable identifier of the value, referenced by findings."
        ),
        section = prop_string(
          enum = REVIEW_BASIS_SECTIONS,
          description = "What the value describes: the sample, model performance, baseline performance, hyperparameter tuning, or a review setting."
        ),
        label = prop_string(description = "Human-readable name of the value."),
        value = prop_float(
          NULL,
          nullable = TRUE,
          description = "The value; null where it is undefined for this model."
        )
      ),
      min_items = 1L,
      description = "Every value the review rests on, one row each, stated before any finding."
    )),
    findings = prop_state(prop_collection(
      ReviewFinding,
      description = "Findings drawn from the basis, in reporting order."
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
    basis,
    findings = list(),
    limitations = REVIEW_LIMITATIONS
  ) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      type = type,
      description = description,
      basis = basis,
      findings = findings,
      limitations = limitations
    )
  },
  publication = SchemaPublication(
    role = "document",
    slug = "supervisedreview",
    title = "rtemis SupervisedReview",
    description = "A review of a trained supervised model: every value the review rests on (sample sizes, training and test metrics with intervals, baseline performance, settings), then findings that each cite the basis rows they rest on, then what a review of metrics cannot establish.",
    order = 29L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::SupervisedReview


# %% new_supervised_review ----
#' Build a `SupervisedReview`, checking that its findings cite existing rows
#'
#' The single constructor `review()` calls. The class itself carries no native
#' validator: a published class's constraints are declared, and the rule
#' vocabulary has no form relating an array inside a collection to a table
#' column. Until it does, the producer guarantees the relation here.
#'
#' @param algorithm Character: Algorithm of the reviewed model.
#' @param type Character \{"Regression", "Classification"\}: Kind of learning.
#' @param description Character: Methods-style description of the model.
#' @param basis data.frame: Basis rows, with columns `key`, `section`, `label`
#'   and `value`.
#' @param findings List of `ReviewFinding` objects.
#'
#' @return `SupervisedReview` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_supervised_review <- function(
  algorithm,
  type,
  description,
  basis,
  findings
) {
  keys <- basis[["key"]]
  if (anyDuplicated(keys) > 0L) {
    rtemis.core::abort(
      "Review basis keys must be unique; duplicated: ",
      paste(unique(keys[duplicated(keys)]), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_schema_error")
    )
  }
  cited <- unlist(lapply(findings, function(f) f@basis))
  missing_keys <- setdiff(cited, keys)
  if (length(missing_keys) > 0L) {
    rtemis.core::abort(
      "Review findings cite basis keys that do not exist: ",
      paste(missing_keys, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_schema_error")
    )
  }
  SupervisedReview(
    algorithm = algorithm,
    type = type,
    description = description,
    basis = basis,
    findings = findings
  )
} # /rtemis::new_supervised_review


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


# %% format_review_value ----
# Counts print as integers; every other value prints to three decimal places,
# so a perfect score reads 1.000 beside its neighbors. Counts are the whole
# numbers of the sample, tuning and settings sections, and the resample count
# of the baseline section.
format_review_value <- function(value, section = "setting", key = "") {
  if (is.na(value)) {
    return("NA")
  }
  count <- section %in%
    c("sample", "tuning", "setting") ||
    endsWith(key, "resamples_better")
  if (count && value == round(value) && abs(value) < 1e9) {
    return(format(value, big.mark = ",", scientific = FALSE))
  }
  ddSci(value, decimal_places = 3L)
} # /rtemis::format_review_value


# %% repr.SupervisedReview ----
#' repr SupervisedReview
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, SupervisedReview) <- function(x, pad = 0L, output_type = NULL) {
  indent <- strrep(" ", pad + 2L)
  heading <- function(text) {
    paste0(
      "\n",
      fmt(text, bold = TRUE, pad = pad, output_type = output_type),
      "\n"
    )
  }
  out <- paste0(
    repr_S7name("SupervisedReview", pad = pad, output_type = output_type),
    indent,
    x@description,
    "\n"
  )

  # Basis ----
  out <- paste0(out, heading("Basis"))
  section_titles <- c(
    sample = "Sample",
    performance = "Performance",
    baseline = "Baseline",
    tuning = "Tuning",
    setting = "Settings"
  )
  basis <- x@basis
  label_width <- max(nchar(basis[["label"]]))
  for (section in REVIEW_BASIS_SECTIONS) {
    rows <- basis[basis[["section"]] == section, , drop = FALSE]
    if (NROW(rows) == 0L) {
      next
    }
    out <- paste0(
      out,
      indent,
      fmt(
        section_titles[[section]],
        col = highlight_col,
        output_type = output_type
      ),
      "\n"
    )
    lines <- paste0(
      indent,
      "  ",
      formatC(rows[["label"]], width = -label_width),
      "  ",
      mapply(
        format_review_value,
        rows[["value"]],
        key = rows[["key"]],
        MoreArgs = list(section = section),
        USE.NAMES = FALSE
      )
    )
    out <- paste0(out, paste(lines, collapse = "\n"), "\n")
  }

  # Findings ----
  out <- paste0(out, heading("Findings"))
  if (length(x@findings) == 0L) {
    out <- paste0(out, indent, "No findings.\n")
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
      "* ",
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
        "  ",
        fmt("Suggestion: ", italic = TRUE, output_type = output_type),
        f@suggestion,
        "\n"
      )
    }
  }

  # Limitations ----
  out <- paste0(out, heading("Limitations"))
  paste0(out, paste0(indent, "- ", x@limitations, collapse = "\n"), "\n")
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
