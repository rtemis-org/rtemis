# test_ValidateConfigFixtures.R
# ::rtemis::
# 2026- EDG rtemis.org

# The fixture corpus: one bad `(config, data)` pair per diagnostic code,
# asserting the exact code and severity, plus a clean pair that must come back
# empty.
#
# This corpus is permanent and grows. A code added to `DIAGNOSTIC_CODES` without
# a fixture fails "every code has a fixture" below, so the coverage claim is
# checked rather than asserted.
#
# Fixtures are small and synthetic, and built here rather than read from disk:
# a check is about the *shape* of a dataset -- a rare class, a constant column,
# a gap in the outcome -- and a constructed frame says which shape it is
# testing, where a data file only shows it.

.supervised_schema <- "https://schema.rtemis.org/supervised/v1/schema.json"

# The smallest config that reconstructs: a schema and an algorithm.
.config <- function(algorithm = "LightRF", ...) {
  c(
    list(
      `$schema` = .supervised_schema,
      hyperparameters = list(algorithm = algorithm, hyperparameters = list())
    ),
    list(...)
  )
}

.kfold <- function(n_resamples = 10L, ...) {
  list(type = "KFold", n_resamples = n_resamples, ...)
}

# `n` rows of two numeric predictors and a factor outcome with `minority` cases
# in its rarer class.
.balanced_data <- function(n = 40L, minority = NULL) {
  minority <- minority %||% (n %/% 2L)
  set.seed(2026L)
  data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    y = factor(rep(c("no", "yes"), times = c(n - minority, minority)))
  )
}

# The one finding a fixture is about. Fails loudly when a fixture stops
# isolating its code, which is the property that keeps the corpus honest.
.only <- function(diags, code) {
  expect_identical(
    diagnostic_codes(diags),
    code,
    info = paste0("expected exactly one ", code, " finding")
  )
  diags[[1L]]
}

# For the few shapes that genuinely trip two checks -- a column that is
# entirely missing is both un-imputable and non-varying -- pull out the one
# under test. Reach for `.only()` first; a fixture that needs this should say
# in a comment why both findings are right.
.finding <- function(diags, code) {
  codes <- diagnostic_codes(diags)
  expect_true(
    code %in% codes,
    info = paste0(code, " not among: ", paste(codes, collapse = ", "))
  )
  diags[[which(codes == code)[[1L]]]]
}

# `n` rows with one numeric predictor and a numeric outcome. Used where the
# fixture is about row counts rather than classes: with no factor outcome the
# class-balance check does not apply, so the finding under test stands alone.
.regression_data <- function(n) {
  set.seed(2026L)
  data.frame(x1 = rnorm(n), y = rnorm(n))
}


# %% The clean pair ----
test_that("a config that fits its data returns an empty Diagnostics", {
  out <- validate_config(
    .config(outer_resampling_config = .kfold(5L)),
    data = .balanced_data(60L)
  )
  expect_s7_class(out, Diagnostics)
  expect_length(out, 0L)
})


test_that("a clean regression pair returns an empty Diagnostics", {
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(50L), x2 = rnorm(50L), y = rnorm(50L))
  out <- validate_config(
    .config("GLM", outer_resampling_config = .kfold(5L)),
    data = dat
  )
  expect_length(out, 0L)
})


# %% OUTCOME_MISSING ----
test_that("OUTCOME_MISSING: the named outcome column is not in the data", {
  d <- .only(
    validate_config(.config(), data = .balanced_data(), outcome = "readmit"),
    "OUTCOME_MISSING"
  )
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["outcome"]], "readmit")
  expect_identical(d@evidence[["columns"]], c("x1", "x2", "y"))
  expect_null(d@fix)
})


test_that("OUTCOME_MISSING ends the pass rather than cascading", {
  # The data would also trip FEATURE_CONSTANT; with no outcome resolved, every
  # check that reads one would be reporting on a column that is not there.
  dat <- .balanced_data()
  dat[["const"]] <- 1L
  out <- validate_config(.config(), data = dat, outcome = "nope")
  expect_identical(diagnostic_codes(out), "OUTCOME_MISSING")
})


# %% OUTCOME_TYPE_MISMATCH ----
test_that("OUTCOME_TYPE_MISMATCH: a character outcome rtemis cannot use", {
  dat <- data.frame(x1 = rnorm(20L), y = rep(c("no", "yes"), 10L))
  d <- .only(validate_config(.config(), data = dat), "OUTCOME_TYPE_MISMATCH")
  expect_identical(d@severity, "error")
  # The profile's vocabulary, not R's class name: "string" is what a polars
  # reader calls the same column, and the rule has to mean one thing in both.
  expect_identical(d@evidence[["outcome_dtype"]], "string")
})


test_that("OUTCOME_TYPE_MISMATCH: positive_class on a numeric outcome", {
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(30L), y = rnorm(30L))
  d <- .only(
    validate_config(.config(positive_class = "yes"), data = dat),
    "OUTCOME_TYPE_MISMATCH"
  )
  # A warning: `train()` ignores `positive_class` on a numeric outcome and
  # completes, so this must not stop a run that works.
  expect_identical(d@severity, "warning")
  expect_identical(d@evidence[["declared_task"]], "Classification")
  expect_identical(d@evidence[["data_task"]], "Regression")
})


test_that("a binary outcome held as 0/1 integers reads as regression", {
  # The scenario's own trap: readmission stored as 0/1 is numeric to rtemis, so
  # nothing declares classification and nothing is reported. Pinned so that a
  # future change to `declared_task()` is a deliberate one.
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(30L), readmit = rep(c(0L, 1L), 15L))
  expect_length(validate_config(.config(), data = dat), 0L)
})


# %% RESAMPLE_MIN_CLASS ----
test_that("RESAMPLE_MIN_CLASS: the rarer class cannot fill every fold", {
  d <- .only(
    validate_config(
      .config(outer_resampling_config = .kfold(10L)),
      data = .balanced_data(60L, minority = 4L)
    ),
    "RESAMPLE_MIN_CLASS"
  )
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["min_class"]], "yes")
  expect_identical(d@evidence[["min_class_n"]], 4L)
  expect_identical(d@evidence[["n_resamples"]], 10L)
  # The fix is a patch reducing the fold count to what the class supports.
  expect_identical(
    d@fix,
    list(list(
      op = "replace",
      path = "/outer_resampling_config/n_resamples",
      value = 4L
    ))
  )
})


test_that("RESAMPLE_MIN_CLASS offers no fix below two cases", {
  d <- .only(
    validate_config(
      .config(outer_resampling_config = .kfold(5L)),
      data = .balanced_data(40L, minority = 1L)
    ),
    "RESAMPLE_MIN_CLASS"
  )
  expect_null(d@fix)
})


test_that("RESAMPLE_MIN_CLASS covers the tuner's inner resampler too", {
  out <- validate_config(
    .config(
      tuner_config = list(
        type = "GridSearch",
        config = list(resampler_config = .kfold(10L))
      )
    ),
    data = .balanced_data(60L, minority = 4L)
  )
  d <- .only(out, "RESAMPLE_MIN_CLASS")
  expect_identical(
    d@evidence[["resampler"]],
    "/tuner_config/config/resampler_config"
  )
  expect_identical(
    d@fix[[1L]][["path"]],
    "/tuner_config/config/resampler_config/n_resamples"
  )
})


test_that("RESAMPLE_MIN_CLASS does not apply to an unstratified bootstrap", {
  out <- validate_config(
    .config(
      outer_resampling_config = list(type = "Bootstrap", n_resamples = 10L)
    ),
    data = .balanced_data(60L, minority = 4L)
  )
  expect_length(out, 0L)
})


# %% RESAMPLE_N_ROWS ----
test_that("RESAMPLE_N_ROWS: more folds than rows", {
  d <- .only(
    validate_config(
      .config("GLM", outer_resampling_config = .kfold(10L)),
      data = .regression_data(6L)
    ),
    "RESAMPLE_N_ROWS"
  )
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["n_rows"]], 6L)
  expect_identical(d@evidence[["n_resamples"]], 10L)
  # Three folds of two, not six of one: repairing to the row count would only
  # trade this error for the single-case warning below.
  expect_identical(
    d@fix,
    list(list(
      op = "replace",
      path = "/outer_resampling_config/n_resamples",
      value = 3L
    ))
  )
})


test_that("RESAMPLE_N_ROWS offers no fix when no fold count would work", {
  d <- .only(
    validate_config(
      .config("GLM", outer_resampling_config = .kfold(10L)),
      data = .regression_data(3L)
    ),
    "RESAMPLE_N_ROWS"
  )
  expect_null(d@fix)
})


test_that("RESAMPLE_N_ROWS: folds that leave a single test case", {
  d <- .only(
    validate_config(
      .config("GLM", outer_resampling_config = .kfold(10L)),
      data = .regression_data(11L)
    ),
    "RESAMPLE_N_ROWS"
  )
  expect_identical(d@severity, "warning")
  expect_identical(d@evidence[["n_test"]], 1L)
  expect_null(d@fix)
})


test_that("RESAMPLE_N_ROWS: a train_p split that leaves no test cases", {
  d <- .only(
    validate_config(
      .config(
        "GLM",
        outer_resampling_config = list(
          type = "StratSub",
          n_resamples = 3L,
          train_p = 0.95
        )
      ),
      data = .regression_data(10L)
    ),
    "RESAMPLE_N_ROWS"
  )
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["n_test"]], 0)
})


# %% FEATURE_CONSTANT ----
test_that("FEATURE_CONSTANT: predictors that never vary", {
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  dat[["fee"]] <- 99L
  dat <- dat[, c("x1", "x2", "site", "fee", "y")]
  d <- .only(
    validate_config(.config(outer_resampling_config = .kfold(5L)), data = dat),
    "FEATURE_CONSTANT"
  )
  expect_identical(d@severity, "warning")
  expect_identical(d@evidence[["features"]], c("site", "fee"))
  expect_identical(
    d@fix,
    list(list(
      op = "add",
      path = "/preprocessor_config",
      value = list(remove_features = list("site", "fee"))
    ))
  )
})


test_that("FEATURE_CONSTANT patches into an existing preprocessor block", {
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  dat <- dat[, c("x1", "x2", "site", "y")]
  d <- .only(
    validate_config(
      .config(
        preprocessor_config = list(remove_features = "x2"),
        outer_resampling_config = .kfold(5L)
      ),
      data = dat
    ),
    "FEATURE_CONSTANT"
  )
  expect_identical(
    d@fix,
    list(list(
      op = "add",
      path = "/preprocessor_config/remove_features",
      value = list("x2", "site")
    ))
  )
})


test_that("FEATURE_CONSTANT is silent when the config already removes them", {
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  dat <- dat[, c("x1", "x2", "site", "y")]
  base <- .config(outer_resampling_config = .kfold(5L))
  expect_length(
    validate_config(
      c(base, list(preprocessor_config = list(remove_constants = TRUE))),
      data = dat
    ),
    0L
  )
  expect_length(
    validate_config(
      c(base, list(preprocessor_config = list(remove_features = "site"))),
      data = dat
    ),
    0L
  )
})


test_that("FEATURE_CONSTANT ignores a constant outcome", {
  # A constant outcome is not a predictor, so it is not this check's business.
  dat <- data.frame(x1 = rnorm(20L), x2 = rnorm(20L), y = rnorm(20L))
  dat[["y"]] <- 1
  expect_false(
    "FEATURE_CONSTANT" %in%
      diagnostic_codes(validate_config(
        .config("GLM"),
        data = dat
      ))
  )
})


# %% DIM_P_GT_N ----
.wide_data <- function(n_rows = 8L, n_cols = 20L) {
  set.seed(2026L)
  dat <- as.data.frame(matrix(rnorm(n_rows * n_cols), nrow = n_rows))
  dat[["y"]] <- rnorm(n_rows)
  dat
}


test_that("DIM_P_GT_N warns only where the fit goes rank-deficient", {
  # Severity is the algorithm's answer, not a judgment about wide data: an
  # unregularized least squares is rank-deficient here, a penalized one is not.
  d <- .only(validate_config(.config("GLM"), data = .wide_data()), "DIM_P_GT_N")
  expect_identical(d@severity, "warning")
  expect_identical(d@evidence[["encoded_p"]], 20L)
  expect_identical(d@evidence[["effective_p"]], 20L)
  expect_identical(d@evidence[["n_rows"]], 8L)
  expect_false(d@evidence[["algorithm_handles_p_gt_n"]])
  expect_null(d@fix)
})


test_that("DIM_P_GT_N is a note for an algorithm built for this regime", {
  for (algorithm in c("GLMNET", "HAL", "SPLS", "Ranger", "LightGBM")) {
    d <- .only(
      validate_config(.config(algorithm), data = .wide_data()),
      "DIM_P_GT_N"
    )
    expect_identical(d@severity, "note", info = algorithm)
    expect_true(d@evidence[["algorithm_handles_p_gt_n"]], info = algorithm)
  }
})


test_that("DIM_P_GT_N is a note where the answer is not the algorithm's", {
  # A stacked ensemble fits in this regime if its base learners do, so the
  # trait is NA and the finding records the shape without asserting harm.
  d <- .only(
    validate_config(.config("SuperLearner"), data = .wide_data()),
    "DIM_P_GT_N"
  )
  expect_identical(d@severity, "note")
})


test_that("DIM_P_GT_N counts what the learner sees, not the raw width", {
  # `train()` applies `decomposition_config` before fitting, so a pipeline that
  # extracts 3 components hands the learner 3 features however wide the data is.
  # Reporting 20 here would state a number the model never sees.
  expect_length(
    validate_config(
      .config(
        "GLM",
        decomposition_config = list(
          algorithm = "PCA",
          config = list(k = 3L)
        )
      ),
      data = .wide_data()
    ),
    0L
  )
})


test_that("DIM_P_GT_N reports the component count when it still exceeds rows", {
  d <- .only(
    validate_config(
      .config(
        "GLM",
        decomposition_config = list(
          algorithm = "PCA",
          config = list(k = 20L)
        )
      ),
      data = .wide_data()
    ),
    "DIM_P_GT_N"
  )
  expect_identical(d@severity, "warning")
  expect_identical(d@evidence[["effective_p"]], 20L)
  expect_identical(d@evidence[["decomposition"]], "PCA")
  expect_identical(d@evidence[["decomposition_k"]], 20L)
  expect_match(d@message, "PCA extracts 20 components", fixed = TRUE)
})


test_that("DIM_P_GT_N counts a factor once per level", {
  # Six rows, one numeric predictor and one 6-level factor: 1 + 6 = 7 encoded
  # columns from two predictors, which is what a schema cannot see.
  set.seed(2026L)
  dat <- data.frame(
    x1 = rnorm(6L),
    site = factor(letters[1:6]),
    y = rnorm(6L)
  )
  d <- .only(validate_config(.config("GLM"), data = dat), "DIM_P_GT_N")
  expect_identical(d@evidence[["n_features"]], 2L)
  expect_identical(d@evidence[["encoded_p"]], 7L)
  expect_identical(d@evidence[["categorical_features"]], "site")
})


test_that("DIM_P_GT_N is silent when the encoded width fits", {
  set.seed(2026L)
  dat <- data.frame(
    x1 = rnorm(20L),
    site = factor(rep(letters[1:4], 5L)),
    y = rnorm(20L)
  )
  expect_length(validate_config(.config("GLM"), data = dat), 0L)
})


# %% MISSING_INCOMPATIBLE ----
test_that("MISSING_INCOMPATIBLE: gaps reach an algorithm that refuses them", {
  dat <- .balanced_data(40L)
  dat[["x1"]][1:3] <- NA
  d <- .only(
    validate_config(
      .config("GLM", outer_resampling_config = .kfold(5L)),
      data = dat
    ),
    "MISSING_INCOMPATIBLE"
  )
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["algorithm"]], "GLM")
  expect_false(d@evidence[["algorithm_allows_missing"]])
  expect_identical(d@evidence[["n_missing"]], 3L)
})


test_that("MISSING_INCOMPATIBLE is silent for an algorithm that takes gaps", {
  dat <- .balanced_data(40L)
  dat[["x1"]][1:3] <- NA
  expect_length(
    validate_config(
      .config("LightGBM", outer_resampling_config = .kfold(5L)),
      data = dat
    ),
    0L
  )
})


test_that("MISSING_INCOMPATIBLE warns where the algorithm cannot answer", {
  # A meta learner accepts whatever its base learners accept, so the trait is
  # NA and the finding says gaps survive without claiming they are fatal.
  dat <- .balanced_data(40L)
  dat[["x1"]][1:3] <- NA
  d <- .only(
    validate_config(
      .config("SuperLearner", outer_resampling_config = .kfold(5L)),
      data = dat
    ),
    "MISSING_INCOMPATIBLE"
  )
  expect_identical(d@severity, "warning")
})


test_that("MISSING_INCOMPATIBLE: a gap in the outcome, which nothing removes", {
  dat <- .balanced_data(40L)
  dat[["y"]][1L] <- NA
  out <- validate_config(
    .config("GLM", preprocessor_config = list(impute = TRUE)),
    data = dat
  )
  d <- .only(out, "MISSING_INCOMPATIBLE")
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["outcome"]], "y")
  expect_identical(d@evidence[["n_missing"]], 1L)
})


test_that("MISSING_INCOMPATIBLE: complete_cases leaves nothing to train on", {
  # A standalone preprocessor, because `train()` rejects `complete_cases`
  # outright -- see the PREPROCESSOR_UNSUPPORTED fixtures below.
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(10L), x2 = rnorm(10L), y = rnorm(10L))
  dat[["x1"]][1:5] <- NA
  dat[["x2"]][6:10] <- NA
  d <- .only(
    validate_config(
      list(
        `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
        complete_cases = TRUE
      ),
      data = dat
    ),
    "MISSING_INCOMPATIBLE"
  )
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["n_complete"]], 0L)
})


test_that("MISSING_INCOMPATIBLE: imputation has nothing to learn from", {
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(20L), x2 = NA_real_, y = rnorm(20L))
  out <- validate_config(
    .config("GLM", preprocessor_config = list(impute = TRUE)),
    data = dat
  )
  # An entirely missing column is two things at once, and both findings are
  # correct: nothing to impute from, and nothing that varies.
  expect_setequal(
    diagnostic_codes(out),
    c("FEATURE_CONSTANT", "MISSING_INCOMPATIBLE")
  )
  d <- .finding(out, "MISSING_INCOMPATIBLE")
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["features"]], "x2")
})


test_that("MISSING_INCOMPATIBLE is silent when imputation resolves the gaps", {
  dat <- .balanced_data(40L)
  dat[["x1"]][1:3] <- NA
  expect_length(
    validate_config(
      .config("GLM", preprocessor_config = list(impute = TRUE)),
      data = dat
    ),
    0L
  )
})


# %% Applying a fix ----
# The property that makes `fix` worth carrying: applying it clears the finding
# it came from. Checked through a JSON round trip, because that is the path a
# client takes -- the patch is serialized, applied to the config document, and
# the result read back -- and it is where a scalar that should have stayed an
# array would show up.

# Enough of RFC 6902 for the operations `validate_config()` emits: `add` and
# `replace`, at a top-level or a one-level-nested pointer. Not a general
# implementation; a client uses its language's own.
.apply_patch <- function(config, patch) {
  for (op in patch) {
    expect_true(op[["op"]] %in% c("add", "replace"))
    keys <- strsplit(sub("^/", "", op[["path"]]), "/", fixed = TRUE)[[1L]]
    expect_lte(length(keys), 2L)
    if (length(keys) == 1L) {
      config[[keys[[1L]]]] <- op[["value"]]
    } else {
      config[[keys[[1L]]]][[keys[[2L]]]] <- op[["value"]]
    }
  }
  config
}

# Serialize a config the way a client would, and read it back. `auto_unbox`
# leaves a one-element *list* as an array, which is why a patch's list-valued
# `value` is built with `as.list()`.
.round_trip <- function(config) {
  jsonlite::fromJSON(
    jsonlite::toJSON(config, auto_unbox = TRUE, null = "null"),
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

.apply_all_fixes <- function(config, diags) {
  patch <- unlist(
    lapply(seq_len(length(diags)), function(i) diags[[i]]@fix),
    recursive = FALSE
  )
  .round_trip(.apply_patch(config, patch))
}


test_that("the RESAMPLE_MIN_CLASS fix clears the finding", {
  dat <- .balanced_data(60L, minority = 4L)
  config <- .config(outer_resampling_config = .kfold(10L))
  out <- validate_config(config, data = dat)
  expect_identical(diagnostic_codes(out), "RESAMPLE_MIN_CLASS")
  expect_length(validate_config(.apply_all_fixes(config, out), data = dat), 0L)
})


test_that("the RESAMPLE_N_ROWS fix clears the finding", {
  dat <- .regression_data(6L)
  config <- .config("GLM", outer_resampling_config = .kfold(10L))
  out <- validate_config(config, data = dat)
  expect_identical(diagnostic_codes(out), "RESAMPLE_N_ROWS")
  expect_length(validate_config(.apply_all_fixes(config, out), data = dat), 0L)
})


test_that("the FEATURE_CONSTANT fix clears the finding, block or no block", {
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  dat <- dat[, c("x1", "x2", "site", "y")]

  # No preprocessor block: the patch creates one.
  config <- .config(outer_resampling_config = .kfold(5L))
  out <- validate_config(config, data = dat)
  expect_identical(diagnostic_codes(out), "FEATURE_CONSTANT")
  expect_length(validate_config(.apply_all_fixes(config, out), data = dat), 0L)

  # An existing block, with a list the patch must not discard.
  config2 <- .config(
    preprocessor_config = list(remove_features = "x2"),
    outer_resampling_config = .kfold(5L)
  )
  out2 <- validate_config(config2, data = dat)
  fixed2 <- .apply_all_fixes(config2, out2)
  expect_length(validate_config(fixed2, data = dat), 0L)
  expect_setequal(
    fixed2[["preprocessor_config"]][["remove_features"]],
    c("x2", "site")
  )
})


test_that("several fixes apply together", {
  dat <- .balanced_data(60L, minority = 4L)
  dat[["site"]] <- factor("A")
  dat <- dat[, c("x1", "x2", "site", "y")]
  config <- .config(outer_resampling_config = .kfold(10L))
  out <- validate_config(config, data = dat)
  expect_setequal(
    diagnostic_codes(out),
    c("RESAMPLE_MIN_CLASS", "FEATURE_CONSTANT")
  )
  expect_length(validate_config(.apply_all_fixes(config, out), data = dat), 0L)
})


# %% Corpus coverage ----
test_that("every code has a fixture", {
  # Read from the test file itself: a code added to the vocabulary without a
  # fixture fails here, so the corpus cannot silently fall behind.
  src <- readLines(test_path("test_ValidateConfigFixtures.R"))
  covered <- DIAGNOSTIC_CODES[vapply(
    DIAGNOSTIC_CODES,
    function(code) {
      any(grepl(paste0("# %% ", code, " ----"), src, fixed = TRUE))
    },
    logical(1L)
  )]
  # SCHEMA_INVALID's fixtures are the schema half, in test_ValidateConfig.R.
  expect_setequal(covered, setdiff(DIAGNOSTIC_CODES, "SCHEMA_INVALID"))
})


test_that("a config with no outcome treats every column as a feature", {
  # A decomposition models every column jointly, so calling the last one an
  # outcome would drop it from the feature checks. `site` is last and constant;
  # a supervised config would let it through as the outcome, this must not.
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  out <- validate_config(
    list(
      `$schema` = "https://schema.rtemis.org/decomposition/v1/schema.json",
      algorithm = "PCA",
      config = list(k = 2L)
    ),
    data = dat
  )
  d <- .only(out, "FEATURE_CONSTANT")
  expect_identical(d@evidence[["features"]], "site")
})


test_that("an explicit outcome applies whatever the config is for", {
  dat <- .balanced_data(40L)
  out <- validate_config(
    list(
      `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
      remove_constants = FALSE
    ),
    data = dat,
    outcome = "nope"
  )
  expect_identical(diagnostic_codes(out), "OUTCOME_MISSING")
})


test_that("the data checks run on a preprocessor config too", {
  # A plan step is not always a supervised config. A standalone preprocessor
  # carries the parts the missingness and constant checks read, and gets them.
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  out <- validate_config(
    list(
      `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
      remove_constants = FALSE
    ),
    data = dat[, c("x1", "x2", "site", "y")]
  )
  expect_true("FEATURE_CONSTANT" %in% diagnostic_codes(out))
})


test_that("validate_config() stamps the step onto every data finding", {
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  out <- validate_config(
    .config(outer_resampling_config = .kfold(5L)),
    data = dat[, c("x1", "x2", "site", "y")],
    step = 2L
  )
  expect_true(all(vapply(
    seq_len(length(out)),
    function(i) identical(out[[i]]@step, 2L),
    logical(1L)
  )))
})


# %% Preprocessing thresholds ----
# `remove_features_thres` / `remove_cases_thres` drop what is missing above a
# threshold. `train()` rejects both -- a learned column drop would give each
# resample a different feature set, and a case drop cannot be replayed at
# predict time -- so inside a supervised config they resolve nothing. They are
# still supported by `preprocess()`, and there they are simulated exactly.

.thin_column_data <- function(n = 60L, n_missing = 54L) {
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(n), x2 = rnorm(n), y = rnorm(n))
  dat[["x1"]][seq_len(n_missing)] <- NA
  dat
}


test_that("a threshold in a supervised config resolves nothing", {
  out <- validate_config(
    .config("GLM", preprocessor_config = list(remove_features_thres = 0.5)),
    data = .thin_column_data()
  )
  expect_setequal(
    diagnostic_codes(out),
    c("PREPROCESSOR_UNSUPPORTED", "MISSING_INCOMPATIBLE")
  )
  # The gaps are still counted, because the step that would remove them is one
  # `train()` will not run.
  expect_identical(
    .finding(out, "MISSING_INCOMPATIBLE")@evidence[["n_missing"]],
    54L
  )
})


test_that("a threshold that drops the gappy feature clears it for preprocess()", {
  # The same config as a standalone preprocessor: here the step does run, and
  # `missing_after_preprocessing()` simulates it exactly.
  expect_length(
    validate_config(
      list(
        `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
        remove_features_thres = 0.5
      ),
      data = .thin_column_data()
    ),
    0L
  )
})


test_that("a threshold too high to drop it does not clear the finding", {
  d <- .only(
    validate_config(
      list(
        `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
        remove_features_thres = 0.95
      ),
      data = .thin_column_data()
    ),
    "MISSING_INCOMPATIBLE"
  )
  # A warning, not an error: a preprocessor config names no algorithm, so
  # whether the surviving gaps are fatal is not knowable from it.
  expect_identical(d@severity, "warning")
  expect_identical(d@evidence[["n_missing"]], 54L)
})


test_that("remove_cases_thres is simulated at the column count", {
  # `preprocess()` drops a case when its missing fraction reaches the
  # threshold, and does it before the feature step. A standalone preprocessor
  # designates no outcome, so every column counts toward the fraction: each
  # incomplete case here is missing 1 of 3, or 0.33.
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(20L), x2 = rnorm(20L), y = rnorm(20L))
  dat[["x1"]][1:3] <- NA
  pp <- function(thres) {
    list(
      `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
      remove_cases_thres = thres
    )
  }
  expect_length(validate_config(pp(0.3), data = dat), 0L)
  expect_identical(
    diagnostic_codes(validate_config(pp(0.5), data = dat)),
    "MISSING_INCOMPATIBLE"
  )
})


# %% PREPROCESSOR_UNSUPPORTED ----
test_that("PREPROCESSOR_UNSUPPORTED: steps train() cannot run", {
  dat <- .balanced_data(40L)
  for (op in c("complete_cases", "remove_duplicates")) {
    d <- .only(
      validate_config(
        .config("GLM", preprocessor_config = stats::setNames(list(TRUE), op)),
        data = dat
      ),
      "PREPROCESSOR_UNSUPPORTED"
    )
    expect_identical(d@severity, "error", info = op)
    expect_identical(d@evidence[["case_ops"]], op, info = op)
    expect_null(d@fix)
  }
})


test_that("PREPROCESSOR_UNSUPPORTED: a learned column drop", {
  d <- .only(
    validate_config(
      .config("GLM", preprocessor_config = list(remove_features_thres = 0.9)),
      data = .balanced_data(40L)
    ),
    "PREPROCESSOR_UNSUPPORTED"
  )
  expect_identical(d@evidence[["learned_drop_ops"]], "remove_features_thres")
  expect_match(d@message, "different feature set", fixed = TRUE)
})


test_that("PREPROCESSOR_UNSUPPORTED does not apply to a standalone preprocessor", {
  # `preprocess()` supports every one of these; only `train()` rejects them.
  expect_length(
    validate_config(
      list(
        `$schema` = "https://schema.rtemis.org/preprocessor/v1/schema.json",
        remove_duplicates = TRUE
      ),
      data = .balanced_data(40L)
    ),
    0L
  )
})


test_that("named remove_features and remove_constants stay allowed", {
  dat <- .balanced_data(40L)
  dat[["site"]] <- factor("A")
  dat <- dat[, c("x1", "x2", "site", "y")]
  for (cfg in list(
    list(remove_features = "site"),
    list(remove_constants = TRUE)
  )) {
    expect_length(
      validate_config(.config("GLM", preprocessor_config = cfg), data = dat),
      0L
    )
  }
})


test_that("a gap only in the outcome is reported once, not twice", {
  # Both the outcome rule and the reaches-the-learner rule carry this code; the
  # second must not fire, since no *feature* gap reaches anything.
  dat <- .balanced_data(40L)
  dat[["y"]][1L] <- NA
  out <- validate_config(.config("GLM"), data = dat)
  expect_identical(diagnostic_codes(out), "MISSING_INCOMPATIBLE")
  expect_identical(out[[1L]]@evidence[["outcome"]], "y")
})


# %% FEATURE_TYPE_UNSUPPORTED ----
test_that("FEATURE_TYPE_UNSUPPORTED: predictors train() cannot read", {
  set.seed(2026L)
  n <- 40L
  dat <- data.frame(
    x1 = rnorm(n),
    site = sample(c("a", "b"), n, TRUE),
    when = Sys.Date() + seq_len(n),
    y = rnorm(n)
  )
  d <- .only(
    validate_config(.config("GLM"), data = dat),
    "FEATURE_TYPE_UNSUPPORTED"
  )
  expect_identical(d@severity, "error")
  expect_identical(d@evidence[["features"]], c("site", "when"))
  expect_identical(d@evidence[["dtypes"]], c("string", "temporal"))
  # Converting a column changes the data, not the config, so there is no patch.
  expect_null(d@fix)
})


test_that("FEATURE_TYPE_UNSUPPORTED fires despite character2factor", {
  # `check_supervised()` runs before `preprocess()` in `train()`, so the
  # setting that would convert this column never gets the chance. Pinned
  # because the check would otherwise look over-strict.
  set.seed(2026L)
  dat <- data.frame(
    x1 = rnorm(40L),
    site = sample(c("a", "b"), 40L, TRUE),
    y = rnorm(40L)
  )
  expect_identical(
    diagnostic_codes(validate_config(
      .config("GLM", preprocessor_config = list(character2factor = TRUE)),
      data = dat
    )),
    "FEATURE_TYPE_UNSUPPORTED"
  )
  expect_error(
    train(
      dat,
      hyperparameters = setup_GLM(),
      preprocessor_config = setup_Preprocessor(character2factor = TRUE),
      verbosity = 0L
    ),
    "not numeric or factor"
  )
})


test_that("FEATURE_TYPE_UNSUPPORTED is silent on numeric and factor predictors", {
  set.seed(2026L)
  dat <- data.frame(
    x1 = rnorm(40L),
    x2 = 1:40,
    site = factor(sample(c("a", "b"), 40L, TRUE)),
    y = rnorm(40L)
  )
  expect_length(validate_config(.config("GLM"), data = dat), 0L)
})


# %% Reading the profile, not the data ----
# Every check but one reads a `DataProfile`, which is what lets the same rule
# run outside R. These pin that boundary: the evidence speaks the profile's
# vocabulary, and the one exception is named.

test_that("evidence reports profile dtypes, not R class names", {
  set.seed(2026L)
  dat <- data.frame(
    x1 = rnorm(20L),
    when = Sys.Date() + seq_len(20L),
    y = rnorm(20L)
  )
  d <- .only(
    validate_config(.config("GLM"), data = dat),
    "FEATURE_TYPE_UNSUPPORTED"
  )
  # "temporal", not "Date": the token a polars reader produces for the same
  # column, so a rule written against it means one thing in both.
  expect_identical(d@evidence[["dtypes"]], "temporal")
  expect_true(all(d@evidence[["dtypes"]] %in% PROFILE_DTYPES))
})


test_that("an outcome with more levels than the profile carries is reported", {
  # `PROFILE_MAX_LEVELS` omits the counts, so the class-balance check cannot
  # run. It says so rather than passing: a silent skip is indistinguishable
  # from a clean result.
  n <- PROFILE_MAX_LEVELS + 1L
  set.seed(2026L)
  dat <- data.frame(x1 = rnorm(n), y = factor(seq_len(n)))
  d <- .only(
    validate_config(
      .config(outer_resampling_config = .kfold(5L)),
      data = dat
    ),
    "RESAMPLE_MIN_CLASS"
  )
  expect_identical(d@severity, "note")
  expect_identical(d@evidence[["n_levels"]], n)
  expect_identical(d@evidence[["max_levels"]], PROFILE_MAX_LEVELS)
  expect_match(d@message, "was not checked", fixed = TRUE)
})


test_that("FEATURE_CONSTANT over the profile reproduces is_constant()", {
  # The two spellings must agree for both settings of
  # `remove_constants_skip_missing`, since the profile has no values to compare.
  set.seed(2026L)
  dat <- data.frame(
    varies = rnorm(6L),
    repeated = rep(1, 6L),
    repeated_with_gap = c(1, 1, 1, 1, 1, NA),
    all_missing = rep(NA_real_, 6L),
    y = rnorm(6L)
  )
  features <- c("varies", "repeated", "repeated_with_gap", "all_missing")
  for (skip in c(TRUE, FALSE)) {
    expected <- features[vapply(
      features,
      function(nm) is_constant(dat[[nm]], skip_missing = skip),
      logical(1L)
    )]
    out <- validate_config(
      .config(
        "LightGBM",
        preprocessor_config = list(remove_constants_skip_missing = skip)
      ),
      data = dat
    )
    found <- .finding(out, "FEATURE_CONSTANT")@evidence[["features"]]
    expect_setequal(found, expected)
  }
})
