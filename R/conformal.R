# conformal.R
# ::rtemis::
# 2026- EDG rtemis.org

# Description
# `conformal()` methods and the arithmetic behind them. The classes they build
# live in `R/260_PredictionRegion.R`; see `plan/conformal.md` for the design.
#
# Every construction here reduces to the same two steps: score the calibration
# cases, then admit the outcomes whose score would not stand out among them.
# What differs is which model scores which case, and that is what decides the
# guarantee each construction carries.

# %% conformal_order ----
#' The order statistic a conformal quantile is
#'
#' `ceiling((n + 1) * (1 - alpha))`, the rank whose calibration score bounds a
#' fresh case's with probability at least `1 - alpha`. The `+ 1` is what makes
#' the guarantee finite-sample rather than asymptotic: the fresh case is counted
#' as if it were already in the calibration set.
#'
#' @param n Integer: Number of calibration cases.
#' @param alpha Numeric (0, 1): Miscoverage rate.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_order <- function(n, alpha) {
  as.integer(ceiling((n + 1) * (1 - alpha)))
} # /rtemis::conformal_order


# %% check_conformal_n ----
#' Refuse a calibration set too small for the requested `alpha`
#'
#' Below `ceiling(1 / alpha) - 1` cases the order statistic falls off the end of
#' the calibration scores and the region is the whole outcome space. That is a
#' valid answer and a useless one, so it is reported as the input problem it is
#' rather than returned as an infinite interval a reader might plot.
#'
#' @param n Integer: Number of calibration cases.
#' @param alpha Numeric (0, 1): Miscoverage rate.
#'
#' @return `n`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_conformal_n <- function(n, alpha) {
  if (conformal_order(n, alpha) > n) {
    rtemis.core::abort(
      "alpha = ",
      alpha,
      " needs at least ",
      as.integer(ceiling(1 / alpha) - 1),
      " calibration cases for a finite region; got ",
      n,
      ".\nUse a larger alpha, or calibrate on more data.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(n)
} # /rtemis::check_conformal_n


# %% conformal_quantile ----
#' The calibrated threshold for a split construction
#'
#' @param scores Numeric: Calibration nonconformity scores.
#' @param alpha Numeric (0, 1): Miscoverage rate.
#'
#' @return Numeric scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_quantile <- function(scores, alpha) {
  n <- length(scores)
  check_conformal_n(n, alpha)
  k <- conformal_order(n, alpha)
  # Only the k-th order statistic is wanted, so the rest of the vector is left
  # unsorted: partial sorting is linear where a full sort is not, and a
  # calibration set can be the whole training data under CV+.
  sort(scores, partial = k)[k]
} # /rtemis::conformal_quantile


# %% conformal_score_for ----
#' Resolve which nonconformity score runs
#'
#' A regression has one, so NULL and "absolute" mean the same thing there and a
#' set-valued score is a category error rather than a preference.
#'
#' @param requested Optional Character: `ConformalConfig@score`.
#' @param type Character: "Regression" or "Classification".
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_score_for <- function(requested, type) {
  if (identical(type, "Regression")) {
    if (!is.null(requested) && !identical(requested, "absolute")) {
      rtemis.core::abort(
        "score = \"",
        requested,
        "\" builds a set of labels and this is a regression. ",
        "Leave `score` NULL, which resolves to \"absolute\".",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    return("absolute")
  }
  if (is.null(requested)) {
    return("APS")
  }
  if (identical(requested, "absolute")) {
    rtemis.core::abort(
      "score = \"absolute\" is a residual and this is a classification. ",
      "Use \"APS\" or \"LAC\", or leave `score` NULL for \"APS\".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  requested
} # /rtemis::conformal_score_for


# %% full_prob_matrix ----
#' Every class's probability, including the binary case
#'
#' `Classification` stores one column for a binary outcome -- the positive
#' class's probability, the other being its complement -- and a conformal score
#' needs the probability of whichever class it is asked about. Widened here so
#' that one score function serves both the binary and the multiclass case, which
#' is the rule `prob_matrix()` already applies in the other direction.
#'
#' @param prob Numeric matrix: Predicted probabilities as `Classification`
#' stores them.
#' @param classes Character: Class labels, in outcome order.
#' @param binclasspos Integer: Which of two classes is the positive one.
#'
#' @return Numeric matrix, one column per class, named and in outcome order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
full_prob_matrix <- function(prob, classes, binclasspos = 2L) {
  prob <- as.matrix(prob)
  if (ncol(prob) == length(classes)) {
    colnames(prob) <- classes
    return(prob)
  }
  if (ncol(prob) != 1L || length(classes) != 2L) {
    rtemis.core::abort(
      "Predicted probabilities hold ",
      ncol(prob),
      " columns for ",
      length(classes),
      " classes.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  positive <- as.numeric(prob)
  out <- if (binclasspos == 2L) {
    cbind(1 - positive, positive)
  } else {
    cbind(positive, 1 - positive)
  }
  colnames(out) <- classes
  out
} # /rtemis::full_prob_matrix


# %% class_index ----
#' Position of each case's true label among the classes
#'
#' @param y Factor: True labels.
#' @param classes Character: Class labels, in outcome order.
#'
#' @return Integer vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
class_index <- function(y, classes) {
  idx <- match(as.character(y), classes)
  if (anyNA(idx)) {
    rtemis.core::abort(
      "Calibration outcomes hold labels the model was not trained on: ",
      paste(setdiff(unique(as.character(y)), classes), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  idx
} # /rtemis::class_index


# %% lac_scores ----
#' Least-ambiguous-set scores for the true labels
#'
#' @param prob Numeric matrix: One row per case, one column per class.
#' @param y_idx Integer: Position of each case's true label.
#'
#' @return Numeric vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lac_scores <- function(prob, y_idx) {
  1 - prob[cbind(seq_along(y_idx), y_idx)]
} # /rtemis::lac_scores


# %% aps_matrix ----
#' Randomized adaptive-prediction-set score for every class
#'
#' For each case and each class, the probability of the classes *strictly* more
#' probable than it, plus `u` times its own. The APS score of a label is its
#' entry here, and the APS set is every label whose entry does not exceed the
#' calibrated threshold -- so one matrix serves both scoring and set building.
#'
#' **`u` is what makes APS usable.** At `u = 1` the score is the inclusive
#' cumulative probability, so every case whose true label ranks last scores
#' exactly 1; a model erring on more than `alpha` of its cases then puts the
#' threshold at 1 and every set holds every label. Drawing `u` uniformly, one
#' value per case, spreads that mass over `[cumulative-above, cumulative-total]`
#' and recovers sets that discriminate (Romano, Sesia and Candes, 2020). The
#' draw is seeded from the config, so a region is still reproducible from what
#' it records.
#'
#' The score increases with rank whatever `u` is -- moving down one rank adds
#' `p_above * (1 - u) + u * p_own >= 0` -- so the sets stay nested in
#' probability order and never omit a class more probable than one they hold.
#'
#' Ties are broken by column order, which makes the result deterministic across
#' platforms; two exactly equal probabilities enter or leave together at any
#' threshold that separates the pair from the rest.
#'
#' @param prob Numeric matrix: One row per case, one column per class.
#' @param u Numeric \[0, 1\]: One draw per case.
#'
#' @return Numeric matrix of the same shape.
#'
#' @author EDG
#' @keywords internal
#' @noRd
aps_matrix <- function(prob, u) {
  out <- matrix(
    NA_real_,
    nrow = nrow(prob),
    ncol = ncol(prob),
    dimnames = dimnames(prob)
  )
  for (i in seq_len(nrow(prob))) {
    ord <- order(prob[i, ], decreasing = TRUE)
    sorted <- prob[i, ord]
    above <- cumsum(sorted) - sorted
    out[i, ord] <- above + u[[i]] * sorted
  }
  out
} # /rtemis::aps_matrix


# %% conformal_uniforms ----
#' The APS draws for one call, from one seeded stream
#'
#' Every uniform a call needs comes out of a single seeded block, so the
#' calibration draws and the test draws are distinct values from one stream
#' rather than two identical vectors from one seed. LAC draws nothing.
#'
#' @param sizes Integer: How many draws each consumer needs, in order.
#' @param score Character: Resolved score.
#' @param seed Integer: Seed from the config.
#'
#' @return List of numeric vectors parallel to `sizes`, or a list of NULLs when
#' the score does not draw.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_uniforms <- function(sizes, score, seed) {
  if (!identical(score, "APS")) {
    return(rep(list(NULL), length(sizes)))
  }
  drawn <- with_seed(seed, stats::runif(sum(sizes)))
  ends <- cumsum(sizes)
  starts <- ends - sizes + 1L
  lapply(seq_along(sizes), function(i) {
    if (sizes[[i]] == 0L) numeric(0L) else drawn[starts[[i]]:ends[[i]]]
  })
} # /rtemis::conformal_uniforms


# %% classification_scores ----
#' Nonconformity scores of the true labels
#'
#' @param prob Numeric matrix: One row per case, one column per class.
#' @param y_idx Integer: Position of each case's true label.
#' @param score Character \{"LAC", "APS"\}: Resolved score.
#' @param u Optional Numeric: APS draws, one per case.
#'
#' @return Numeric vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
classification_scores <- function(prob, y_idx, score, u = NULL) {
  if (identical(score, "LAC")) {
    lac_scores(prob, y_idx)
  } else {
    aps_matrix(prob, u)[cbind(seq_along(y_idx), y_idx)]
  }
} # /rtemis::classification_scores


# %% candidate_scores ----
#' Nonconformity score of every candidate label
#'
#' The same quantity `classification_scores()` reads for the true label, for all
#' of them: what a set is cut from.
#'
#' @param prob Numeric matrix: One row per case, one column per class.
#' @param score Character \{"LAC", "APS"\}: Resolved score.
#' @param u Optional Numeric: APS draws, one per case.
#'
#' @return Numeric matrix of the same shape.
#'
#' @author EDG
#' @keywords internal
#' @noRd
candidate_scores <- function(prob, score, u = NULL) {
  if (identical(score, "LAC")) {
    1 - prob
  } else {
    aps_matrix(prob, u)
  }
} # /rtemis::candidate_scores


# %% sets_from_scores ----
#' Cut label sets from a candidate-score matrix
#'
#' A set may come out empty -- no label scores below the threshold -- which is
#' the honest answer at this level and is returned as such rather than widened
#' to the most probable label.
#'
#' @param scores Numeric matrix: One row per case, one column per class.
#' @param q Numeric: Threshold a label must not exceed.
#' @param classes Character: Class labels.
#'
#' @return List of character vectors, one per case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
sets_from_scores <- function(scores, q, classes) {
  lapply(seq_len(nrow(scores)), function(i) {
    classes[scores[i, ] <= q]
  })
} # /rtemis::sets_from_scores


# %% conformal_config_for ----
#' Resolve the requested configuration
#'
#' NULL means the caller expressed no preference and gets the construction the
#' object supports, which is the only one it could have meant.
#'
#' @param config Optional `ConformalConfig` object.
#' @param default `ConformalConfig` object: What NULL resolves to.
#'
#' @return `ConformalConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_config_for <- function(config, default) {
  if (is.null(config)) {
    return(default)
  }
  check_is_S7(config, ConformalConfig)
  config
} # /rtemis::conformal_config_for


# %% conformal_calibration_predictions ----
#' Predictions and outcomes for the calibration cases
#'
#' Resolves the two questions a split construction turns on: which data
#' calibrates, and whether it is entitled to.
#'
#' Supplied `calibration` is predicted through the same pipeline as `newdata`.
#' Left NULL, the model's stored test split is used -- `train()` preprocesses,
#' predicts and scores `dat_test` and reads it for nothing else, so its
#' residuals are both already computed and untouched by fitting, tuning and
#' early stopping. What that cannot see is a user who trained several models and
#' kept the one with the best test metric; that is selection on the split, and
#' nothing in the object records it.
#'
#' A validation split is never taken by default. It is the early-stopping target
#' for four algorithms and, for the rest, the split users most often select on
#' by hand -- pass it as `calibration` to use it deliberately.
#'
#' @param x `Supervised` object.
#' @param calibration Optional tabular data: Calibration cases with the outcome.
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `y`, `predicted` (numeric for a regression, a full
#' probability matrix for a classification), `n` and `fingerprint`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_calibration_predictions <- function(
  x,
  calibration = NULL,
  verbosity = 1L
) {
  classification <- identical(x@type, "Classification")
  if (!is.null(calibration)) {
    check_inherits(calibration, "data.frame")
    y <- outcome(calibration)
    predicted <- predict(
      x,
      newdata = as.data.frame(features(calibration)),
      verbosity = 0L
    )
    if (classification) {
      predicted <- full_prob_matrix(
        predicted,
        levels(x@y_training),
        x@binclasspos
      )
    }
    msg(
      "Calibrating on ",
      length(y),
      " supplied cases...",
      verbosity = verbosity
    )
    return(list(
      y = y,
      predicted = predicted,
      n = length(y),
      fingerprint = data_fingerprint(calibration)
    ))
  }

  check_conformal_stored_split(x)
  stored <- if (classification) x@predicted_prob_test else x@predicted_test
  if (is.null(x@y_test) || is.null(stored)) {
    rtemis.core::abort(
      "This model has no calibration data.\n",
      "Pass `calibration`: a dataset holding the predictors and the outcome, ",
      "in the shape `train()` was given, whose cases the model has not seen.\n",
      "A model trained with `dat_test` calibrates on that split ",
      "automatically.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  if (classification) {
    stored <- full_prob_matrix(stored, levels(x@y_training), x@binclasspos)
  }
  msg(
    "Calibrating on the stored test split (",
    length(x@y_test),
    " cases)...",
    verbosity = verbosity
  )
  list(
    y = x@y_test,
    predicted = stored,
    n = length(x@y_test),
    fingerprint = NULL
  )
} # /rtemis::conformal_calibration_predictions


# %% check_conformal_stored_split ----
#' Refuse a stored test split that already calibrated something else
#'
#' `calibrate()` takes the probabilities to fit the calibration map on as an
#' argument, and `@predicted_prob_test` is a natural thing to hand it. Where it
#' was handed exactly that, the test split fitted the probability calibrator and
#' is no longer exchangeable with a fresh case, so conformalizing on it would
#' return an interval whose guarantee does not hold and whose numbers show no
#' symptom.
#'
#' Compared on the outcomes rather than the probabilities: the calibration map's
#' training frame holds transformed scores, but its outcome column is the test
#' split's labels unchanged.
#'
#' @param x `Supervised` object.
#'
#' @return `x`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_conformal_stored_split <- function(x) {
  if (!S7_inherits(x, CalibratedClassification)) {
    return(invisible(x))
  }
  fitted_on <- x@calibration_model@y_training
  if (
    !is.null(x@y_test) &&
      length(fitted_on) == length(x@y_test) &&
      identical(as.character(fitted_on), as.character(x@y_test))
  ) {
    rtemis.core::abort(
      "The probability calibrator was fitted on this model's test split, so ",
      "that split cannot also calibrate a conformal region: using it twice ",
      "breaks exchangeability and voids the coverage guarantee silently.\n",
      "Pass `calibration` naming data neither the model nor the calibrator ",
      "has seen.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(x)
} # /rtemis::check_conformal_stored_split


# %% conformal.Supervised ----
#' Conformal prediction region for a `Supervised`
#'
#' Split conformal, or CQR where the backend can answer a quantile query.
#' Computed on demand rather than stored, for the reason `se()` gives: a
#' quantity that depends on `newdata` and on a calibration set, and that most
#' users never ask for, is computed when asked.
#'
#' @param x `Supervised` object.
#' @param newdata tabular data: Cases to bound, predictors only.
#' @param calibration Optional tabular data: Calibration cases with the outcome.
#' @param config Optional `ConformalConfig` object: Defaults to
#' `setup_SplitConformal()`.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `PredictionInterval` or `PredictionSet` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(conformal, Supervised) <- function(
  x,
  newdata,
  calibration = NULL,
  config = NULL,
  verbosity = 1L,
  ...
) {
  check_inherits(newdata, "data.frame")
  config <- conformal_config_for(config, setup_SplitConformal())

  if (identical(config@type, "CVPlus")) {
    rtemis.core::abort(
      "CV+ needs a model trained with outer resampling: it reads the per-fold ",
      "models and their out-of-fold predictions off a `SupervisedRes`.\n",
      "Train with `outer_resampling_config = setup_Resampler()`, or use ",
      "`setup_SplitConformal()` on this model.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  if (identical(config@type, "CQR")) {
    return(conformal_cqr(
      x,
      newdata = newdata,
      calibration = calibration,
      config = config,
      verbosity = verbosity
    ))
  }

  calib <- conformal_calibration_predictions(
    x,
    calibration = calibration,
    verbosity = verbosity
  )
  score <- conformal_score_for(config@score, x@type)
  predicted <- predict(x, newdata = newdata, verbosity = 0L)

  region_args <- list(
    algorithm = x@algorithm,
    config = config,
    method = "Split",
    score = score,
    n_calibration = as.integer(calib[["n"]]),
    data_fingerprint = data_fingerprint(newdata),
    calibration_fingerprint = calib[["fingerprint"]]
  )

  if (identical(x@type, "Regression")) {
    q <- conformal_quantile(
      abs(as.numeric(calib[["y"]]) - as.numeric(calib[["predicted"]])),
      config@alpha
    )
    predicted <- as.numeric(predicted)
    return(do.call(
      PredictionInterval,
      c(
        region_args,
        list(
          q = q,
          predicted = predicted,
          lower = predicted - q,
          upper = predicted + q
        )
      )
    ))
  }

  classes <- levels(x@y_training)
  # Both sets of draws from one stream, so a calibration case and a test case
  # never share a draw by construction.
  draws <- conformal_uniforms(
    c(calib[["n"]], nrow(newdata)),
    score,
    config@seed
  )
  q <- conformal_quantile(
    classification_scores(
      calib[["predicted"]],
      class_index(calib[["y"]], classes),
      score,
      draws[[1L]]
    ),
    config@alpha
  )
  prob <- full_prob_matrix(predicted, classes, x@binclasspos)
  do.call(
    PredictionSet,
    c(
      region_args,
      list(
        q = q,
        sets = sets_from_scores(
          candidate_scores(prob, score, draws[[2L]]),
          q,
          classes
        ),
        predicted_prob = prob,
        classes = classes
      )
    )
  )
} # /rtemis::conformal.Supervised


# %% conformal_cqr ----
#' Conformalized quantile regression over a fitted model
#'
#' Starts from the model's own `alpha/2` and `1 - alpha/2` quantiles and moves
#' both ends by one calibrated amount, so a case the model is unsure about gets
#' a wider interval than one it is confident about -- which split conformal, one
#' number added to and subtracted from every prediction, cannot do.
#'
#' `calibration` is required rather than defaulted: the score needs the model's
#' quantiles *at the calibration cases*, which means querying the model, which
#' means having their features -- and a `Supervised` stores its test predictions
#' but not its test features.
#'
#' @param x `Supervised` object.
#' @param newdata tabular data: Cases to bound, predictors only.
#' @param calibration Optional tabular data: Calibration cases with the outcome.
#' @param config `CQRConfig` object.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `PredictionInterval` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_cqr <- function(x, newdata, calibration, config, verbosity = 1L) {
  if (!identical(x@type, "Regression")) {
    rtemis.core::abort(
      "CQR conformalizes a pair of predicted quantiles and this is a ",
      "classification, whose outcome has none.\n",
      "Use `setup_SplitConformal()`, which builds a set of labels.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  if (is.null(calibration)) {
    rtemis.core::abort(
      "CQR needs `calibration` data: it queries the model for quantiles at ",
      "the calibration cases, and a fitted model stores its test predictions ",
      "but not the features they came from.\n",
      "Pass a dataset holding the predictors and the outcome, in the shape ",
      "`train()` was given.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  check_inherits(calibration, "data.frame")

  quantiles <- c(config@alpha / 2, 1 - config@alpha / 2)
  y <- as.numeric(outcome(calibration))
  calibration_q <- supervised_quantiles(
    x,
    as.data.frame(features(calibration)),
    quantiles
  )
  msg(
    "Calibrating on ",
    length(y),
    " supplied cases...",
    verbosity = verbosity
  )
  # The CQR score: how far outside the predicted band the outcome fell, and
  # negative by how much it fell inside. One threshold therefore both widens a
  # band that under-covers and narrows one that over-covers.
  scores <- pmax(calibration_q[, 1L] - y, y - calibration_q[, 2L])
  q <- conformal_quantile(scores, config@alpha)

  newdata_q <- supervised_quantiles(x, newdata, quantiles)
  lower <- newdata_q[, 1L] - q
  upper <- newdata_q[, 2L] + q
  # `q` is negative when the model's band already over-covers, and CQR narrows
  # it by that much. A case whose own band is narrower than `2 * |q|` would come
  # back inverted, which is not an interval. It means the quantile model
  # over-covers so severely that the correction swamps the band, so it is
  # reported as that rather than left to the class validator, whose message
  # describes the symptom and not the cause.
  if (any(upper < lower)) {
    rtemis.core::abort(
      "The quantile model over-covers by more than the width of its own band ",
      "on ",
      sum(upper < lower),
      " of ",
      length(lower),
      " cases, so conformalizing it inverts them.\n",
      "Its ",
      format(100 * (1 - config@alpha), trim = TRUE),
      "% band is wider than the data warrants; refit the quantile model, or ",
      "use `setup_SplitConformal()`, whose correction only widens.",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  PredictionInterval(
    algorithm = x@algorithm,
    config = config,
    method = "CQR",
    score = "absolute",
    q = q,
    n_calibration = length(y),
    data_fingerprint = data_fingerprint(newdata),
    calibration_fingerprint = data_fingerprint(calibration),
    # The band's own midpoint, not `predict()`. A quantile forest's median band
    # and its mean prediction are different estimators, and an interval labeled
    # with a point it was not built around would invite the two to be read as
    # one quantity.
    predicted = rowMeans(newdata_q),
    lower = lower,
    upper = upper
  )
} # /rtemis::conformal_cqr


# %% supervised_quantiles ----
#' Predicted quantiles for `newdata`
#'
#' Routed through the same pipeline as `predict()`, so a stored preprocessor,
#' decomposition and algorithm-internal preprocessor are re-applied before the
#' backend is queried -- quantiles computed from differently transformed
#' features than the predictions they bound would be silently mismatched.
#'
#' @param x `Supervised` object.
#' @param newdata tabular data: Cases to predict.
#' @param quantiles Numeric (0, 1): Levels, in increasing order.
#'
#' @return Numeric matrix, one row per case and one column per level.
#'
#' @author EDG
#' @keywords internal
#' @noRd
supervised_quantiles <- function(x, newdata, quantiles) {
  check_inherits(newdata, "data.frame")
  features <- supervised_features(x, newdata, verbosity = 0L)
  # A missing method means this algorithm cannot answer a quantile query from
  # the model it fitted, which is a fact about the algorithm rather than a
  # failure. S7's dispatch error names a backend class the user never chose, so
  # it is translated into the algorithm they did.
  quantile_matrix <- tryCatch(
    quantile_super(
      model = x@model,
      newdata = features,
      quantiles = quantiles
    ),
    S7_error_method_not_found = function(e) {
      rtemis.core::abort(
        x@algorithm,
        " cannot predict quantiles from a fitted model, so CQR does not apply ",
        "to it.\n",
        "Train a quantile regression forest -- ",
        "`setup_Ranger(quantreg = TRUE)` -- or use `setup_SplitConformal()`, ",
        "which applies to every algorithm.",
        class = c("rtemis_unsupported_error", "rtemis_input_error")
      )
    }
  )
  if (
    !is.matrix(quantile_matrix) ||
      ncol(quantile_matrix) != length(quantiles) ||
      nrow(quantile_matrix) != nrow(features)
  ) {
    rtemis.core::abort(
      "Quantile backend returned a ",
      paste(dim(as.matrix(quantile_matrix)), collapse = " x "),
      " result for ",
      nrow(features),
      " cases at ",
      length(quantiles),
      " levels.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  quantile_matrix
} # /rtemis::supervised_quantiles


# %% conformal_fold_structure ----
#' Read and check the out-of-fold structure CV+ needs
#'
#' CV+ assumes every case is held out exactly once, so that the `n` residuals it
#' pools are the residuals of `n` distinct cases against models that did not see
#' them. `Resampler@resamples` holds each fold's *training* indices, so the
#' out-of-fold sets are their complements and the assumption is a property of
#' the indices rather than of the resampler's name -- which is why it is
#' checked rather than whitelisted: a `Custom` resampler that partitions is
#' entitled to CV+, and a stratified subsample that does not is not.
#'
#' @param x `SupervisedRes` object.
#'
#' @return Integer: The number of training cases.
#'
#' @author EDG
#' @keywords internal
#' @noRd
conformal_fold_structure <- function(x) {
  if (S7_inherits(x, CalibratedClassificationRes)) {
    rtemis.core::abort(
      "`calibrate()` fitted this object's probability calibrators on exactly ",
      "the out-of-fold predictions CV+ would calibrate on, so the two cannot ",
      "share them: using them twice breaks exchangeability and voids the ",
      "coverage guarantee silently.\n",
      "Conformalize the uncalibrated `ClassificationRes` instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  resamples <- x@outer_resampler@resamples
  n_folds <- length(resamples)
  if (n_folds < 2L) {
    rtemis.core::abort(
      "CV+ needs at least 2 resamples; this model has ",
      n_folds,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # The case count is not stored, but each fold accounts for all of it: its
  # training indices plus the outcomes held out from it.
  n <- length(resamples[[1L]]) + length(x@y_test[[1L]])
  # `use.names = FALSE` throughout: the resamples are a named list, so both
  # `unlist()` and `vapply()` would otherwise carry fold names into vectors that
  # are then compared for identity against bare indices.
  held_out <- unlist(
    lapply(resamples, function(idx) setdiff(seq_len(n), idx)),
    use.names = FALSE
  )
  partitions <- length(held_out) == n &&
    identical(sort(held_out), seq_len(n)) &&
    identical(
      vapply(x@y_test, length, integer(1L), USE.NAMES = FALSE),
      vapply(
        resamples,
        function(idx) n - length(idx),
        integer(1L),
        USE.NAMES = FALSE
      )
    )
  if (!partitions) {
    rtemis.core::abort(
      "CV+ needs every case held out exactly once, and ",
      desc(x@outer_resampler),
      " do not partition the data.\n",
      "Train with `setup_Resampler(type = \"KFold\")` or ",
      "`setup_Resampler(type = \"LOOCV\")`, or use ",
      "`setup_SplitConformal()` on a model with a held-out test split.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  n
} # /rtemis::conformal_fold_structure


# %% conformal.SupervisedRes ----
#' Conformal prediction region for a `SupervisedRes`
#'
#' CV+ for a regression, cross-conformal for a classification, and jackknife+
#' for either where the folds are leave-one-out.
#'
#' No data is spent on calibration: every case is out-of-fold exactly once, so
#' every case both trains and calibrates. The price is the guarantee, which is
#' `1 - 2 * alpha` in the worst case rather than `1 - alpha`.
#'
#' @param x `SupervisedRes` object.
#' @param newdata tabular data: Cases to bound, predictors only.
#' @param calibration Not used: the out-of-fold predictions calibrate, and
#' supplying anything else would be a different construction.
#' @param config Optional `ConformalConfig` object: Defaults to
#' `setup_CVPlus()`.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `PredictionInterval` or `PredictionSet` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(conformal, SupervisedRes) <- function(
  x,
  newdata,
  calibration = NULL,
  config = NULL,
  verbosity = 1L,
  ...
) {
  check_inherits(newdata, "data.frame")
  config <- conformal_config_for(config, setup_CVPlus())
  if (!identical(config@type, "CVPlus")) {
    rtemis.core::abort(
      desc(config),
      " applies to a single fitted model, not to a resampled one.\n",
      "Use `setup_CVPlus()` here, which reads the per-fold models and their ",
      "out-of-fold predictions, or conformalize one of `@models`.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  if (!is.null(calibration)) {
    rtemis.core::abort(
      "CV+ calibrates on the out-of-fold predictions this object already ",
      "holds, so it takes no `calibration` data.\n",
      "To calibrate on data of your own, conformalize a single model with ",
      "`setup_SplitConformal()`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  n <- conformal_fold_structure(x)
  check_conformal_n(n, config@alpha)
  score <- conformal_score_for(config@score, x@type)
  # Leave-one-out folds make this jackknife+; a set-valued outcome makes it
  # cross-conformal, CV+ being defined for intervals. Recorded, so a reader is
  # never left to infer which construction produced the numbers.
  method_name <- if (identical(x@type, "Classification")) {
    "CrossConformal"
  } else if (S7_inherits(x@outer_resampler@config, LOOCVConfig)) {
    "JackknifePlus"
  } else {
    "CVPlus"
  }

  msg(
    "Predicting ",
    nrow(newdata),
    " cases under ",
    length(x@models),
    " fold models...",
    verbosity = verbosity
  )
  region_args <- list(
    algorithm = x@algorithm,
    config = config,
    method = method_name,
    score = score,
    # No single threshold: a fold construction ranks a candidate against every
    # out-of-fold score rather than against one quantile of them.
    q = NULL,
    n_calibration = as.integer(n),
    data_fingerprint = data_fingerprint(newdata),
    calibration_fingerprint = x@data_fingerprint
  )

  if (identical(x@type, "Regression")) {
    return(do.call(
      PredictionInterval,
      c(region_args, cvplus_interval(x, newdata, config@alpha))
    ))
  }
  classes <- levels(x@models[[1L]]@y_training)
  do.call(
    PredictionSet,
    c(
      region_args,
      cross_conformal_set(
        x,
        newdata,
        config@alpha,
        score,
        classes,
        config@seed
      )
    )
  )
} # /rtemis::conformal.SupervisedRes


# %% cvplus_interval ----
#' The CV+ interval for each case of `newdata`
#'
#' For every training case `i`, CV+ pairs the prediction of the model that did
#' *not* see `i` with `i`'s own out-of-fold residual, and reads the interval off
#' the order statistics of those `n` pairs (Barber, Candes, Ramdas and
#' Tibshirani, 2021):
#'
#' \deqn{upper(x) = m\text{-th smallest of } f_{-k(i)}(x) + R_i}
#' \deqn{lower(x) = m\text{-th largest of } f_{-k(i)}(x) - R_i}
#'
#' with `m = ceiling((1 - alpha) * (n + 1))`. The prediction is constant within
#' a fold and the residual varies within it, which is why both terms are
#' expanded to length `n` rather than aggregated per fold.
#'
#' @param x `SupervisedRes` object.
#' @param newdata tabular data: Cases to bound.
#' @param alpha Numeric (0, 1): Miscoverage rate.
#'
#' @return List with `predicted`, `lower` and `upper`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
cvplus_interval <- function(x, newdata, alpha) {
  residuals <- unlist(
    mapply(
      function(y, predicted) abs(as.numeric(y) - as.numeric(predicted)),
      x@y_test,
      x@predicted_test,
      SIMPLIFY = FALSE
    ),
    use.names = FALSE
  )
  # Which fold each residual came from, so a fold's prediction can be paired
  # with its own residuals without re-deriving the indices.
  fold_of <- rep(
    seq_along(x@models),
    vapply(x@y_test, length, integer(1L), USE.NAMES = FALSE)
  )
  # One column per fold: `predict()` on the resampled object averages them,
  # which is the wrong operation here -- CV+ needs each fold's own prediction.
  fold_predictions <- vapply(
    x@models,
    function(model) {
      as.numeric(predict(model, newdata = newdata, verbosity = 0L))
    },
    numeric(nrow(newdata))
  )
  fold_predictions <- matrix(
    fold_predictions,
    nrow = nrow(newdata),
    ncol = length(x@models)
  )

  n <- length(residuals)
  m <- conformal_order(n, alpha)
  bounds <- vapply(
    seq_len(nrow(newdata)),
    function(i) {
      centers <- fold_predictions[i, fold_of]
      hi <- centers + residuals
      lo <- centers - residuals
      c(
        sort(lo, partial = n - m + 1L)[n - m + 1L],
        sort(hi, partial = m)[m]
      )
    },
    numeric(2L)
  )
  list(
    predicted = rowMeans(fold_predictions),
    lower = bounds[1L, ],
    upper = bounds[2L, ]
  )
} # /rtemis::cvplus_interval


# %% cross_conformal_set ----
#' The cross-conformal label set for each case of `newdata`
#'
#' The set-valued counterpart of CV+ (Vovk, 2015). A label enters the set when
#' fewer than `m = ceiling((1 - alpha) * (n + 1))` of the `n` out-of-fold
#' calibration scores fall strictly below the label's own score -- each
#' comparison made under the model of the fold the calibration case belongs to,
#' which is what makes every score in the pool one that its model did not see.
#'
#' One draw per test case is shared across the folds, so a label's score moves
#' with the fold model that produced it and not with which draw it happened to
#' get -- see `aps_matrix()` for what the draw is for.
#'
#' @param x `SupervisedRes` object.
#' @param newdata tabular data: Cases to bound.
#' @param alpha Numeric (0, 1): Miscoverage rate.
#' @param score Character \{"LAC", "APS"\}: Resolved score.
#' @param classes Character: Class labels.
#' @param seed Integer: Seed for the APS draws.
#'
#' @return List with `sets`, `predicted_prob` and `classes`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
cross_conformal_set <- function(x, newdata, alpha, score, classes, seed) {
  binclasspos <- x@models[[1L]]@binclasspos
  fold_sizes <- vapply(x@y_test, length, integer(1L), USE.NAMES = FALSE)
  fold_of <- rep(seq_along(x@models), fold_sizes)
  n <- sum(fold_sizes)
  draws <- conformal_uniforms(c(n, nrow(newdata)), score, seed)
  calibration_draws <- if (is.null(draws[[1L]])) {
    rep(list(NULL), length(fold_sizes))
  } else {
    # Split on a factor with explicit levels: `split()` on a bare integer
    # coerces it to a factor whose levels sort as strings, which puts fold 10
    # before fold 2 and hands each fold another fold's draws.
    split(draws[[1L]], factor(fold_of, levels = seq_along(fold_sizes)))
  }

  calibration <- unlist(
    mapply(
      function(y, prob, u) {
        prob <- full_prob_matrix(prob, classes, binclasspos)
        classification_scores(prob, class_index(y, classes), score, u)
      },
      x@y_test,
      x@predicted_prob_test,
      calibration_draws,
      SIMPLIFY = FALSE
    ),
    use.names = FALSE
  )

  # One pass per fold: the probabilities this fold's model gives `newdata`, used
  # both for the candidate scores each calibration score is compared against and
  # for the ensemble average reported alongside the sets.
  fold_prob <- lapply(x@models, function(model) {
    full_prob_matrix(
      predict(model, newdata = newdata, verbosity = 0L),
      classes,
      binclasspos
    )
  })
  fold_candidates <- lapply(fold_prob, function(prob) {
    candidate_scores(prob, score, draws[[2L]])
  })

  m <- conformal_order(n, alpha)
  sets <- lapply(seq_len(nrow(newdata)), function(i) {
    # For each label, how many calibration cases score strictly lower than the
    # test case does under that calibration case's own fold model.
    below <- vapply(
      seq_along(classes),
      function(j) {
        test_scores <- vapply(
          fold_candidates,
          function(candidates) candidates[i, j],
          numeric(1L)
        )
        sum(calibration < test_scores[fold_of])
      },
      numeric(1L)
    )
    classes[below < m]
  })
  # Averaged over folds as `predict(type = "avg")` does, so the reported
  # probabilities and the reported sets describe one model rather than two.
  predicted_prob <- Reduce(`+`, fold_prob) / length(fold_prob)
  list(sets = sets, predicted_prob = predicted_prob, classes = classes)
} # /rtemis::cross_conformal_set


# %% conformal_metrics ----
#' Score a Conformal Prediction Region
#'
#' @description
#' Empirical coverage and region size for a `PredictionRegion`, against the
#' outcomes of the cases it bounds.
#'
#' @details
#' **Coverage on the calibration data is not a check.** The threshold is chosen
#' as the `1 - alpha` empirical quantile of the calibration scores, so coverage
#' measured back on those same cases is `1 - alpha` by construction. Score
#' against outcomes nothing in the pipeline has seen, or the number means
#' nothing.
#'
#' **Read coverage and size together.** A model with no signal attains valid
#' coverage by returning intervals wide enough to be useless, so coverage alone
#' cannot separate a good model from a wide one. Width -- or set size, and the
#' rate of singleton and empty sets -- is the part that carries information
#' about the model.
#'
#' Coverage is a proportion over the cases supplied, so it carries the Monte
#' Carlo error of that sample: 100 cases at a nominal 90% have a standard error
#' near 3 points, and a single run landing at 87% is not evidence of a broken
#' guarantee.
#'
#' @param region `PredictionRegion` object, from [conformal].
#' @param true_outcome Numeric or Factor: The outcomes of the cases `region`
#' bounds, in the same order.
#'
#' @return One-row data.frame. `coverage` and `n` for either shape, plus
#' `mean_width` and `median_width` for an interval, or `mean_set_size`,
#' `singleton_rate` and `empty_rate` for a set.
#'
#' @author EDG
#' @export
#' @examples
#' x <- data.frame(age = rnorm(300), bmi = rnorm(300))
#' x[["y"]] <- x[["age"]] * 2 + rnorm(300, sd = 0.3)
#' mod <- train(
#'   x[1:200, ],
#'   dat_test = x[201:250, ],
#'   hyperparameters = setup_GLM(),
#'   verbosity = 0L
#' )
#' held_out <- x[251:300, ]
#' region <- conformal(mod, held_out[, c("age", "bmi")], verbosity = 0L)
#' conformal_metrics(region, held_out[["y"]])
conformal_metrics <- function(region, true_outcome) {
  check_is_S7(region, PredictionRegion)
  n <- region_n_cases(region)
  if (length(true_outcome) != n) {
    rtemis.core::abort(
      "`true_outcome` holds ",
      length(true_outcome),
      " values for ",
      n,
      " bounded cases.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }
  if (S7_inherits(region, PredictionInterval)) {
    true_outcome <- as.numeric(true_outcome)
    covered <- true_outcome >= region@lower & true_outcome <= region@upper
    return(data.frame(
      coverage = mean(covered),
      mean_width = mean(region@width),
      median_width = stats::median(region@width),
      n = n
    ))
  }
  labels <- as.character(true_outcome)
  covered <- mapply(
    function(label, set) label %in% set,
    labels,
    region@sets,
    USE.NAMES = FALSE
  )
  sizes <- region@set_size
  data.frame(
    coverage = mean(covered),
    mean_set_size = mean(sizes),
    singleton_rate = mean(sizes == 1L),
    empty_rate = mean(sizes == 0L),
    n = n
  )
} # /rtemis::conformal_metrics
