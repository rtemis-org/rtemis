# test_ExplainSupervised.R
# ::rtemis::
# 2026- EDG rtemis.org

# `explain()` end to end, per algorithm.
#
# The load-bearing test here is additivity: `sum(phi) + baseline == prediction`
# to numerical tolerance, on the scale the contributions were computed on. It is
# what catches link-function and preprocessing errors, which produce
# plausible-looking but wrong attributions rather than failures. It is asserted
# against the **stored margin**, never against `predict()`, which applies the
# link for classification -- comparing the two is the error being guarded.

set.seed(2026)
.n <- 200L
.explain_dat <- data.frame(
  age = rnorm(.n),
  bmi = rnorm(.n),
  sex = rnorm(.n)
)
.explain_feats <- c("age", "bmi", "sex")
.regression_dat <- within(
  .explain_dat,
  y <- age * 2 - bmi + rnorm(.n, sd = 0.3)
)
.binary_dat <- within(
  .explain_dat,
  y <- factor(ifelse(age + rnorm(.n, sd = 0.5) > 0, "pos", "neg"))
)
.multiclass_dat <- within(
  .explain_dat,
  y <- factor(c("a", "b", "c")[as.integer(cut(age, 3L))])
)


# Every entry of `phi` must reconstruct its own column of `predicted`. The
# baseline is per case, so it is indexed by column rather than by name -- for
# almost every estimator its rows are identical, but not for one that routes
# each case to a different sub-model.
expect_additive <- function(x, tolerance = 1e-10) {
  for (k in seq_along(x@phi)) {
    label <- names(x@phi)[[k]]
    expect_equal(
      as.numeric(rowSums(x@phi[[label]]) + x@baseline[, k]),
      as.numeric(x@predicted[, k]),
      tolerance = tolerance,
      info = label
    )
  }
}


# %% LightGBM: regression ----
test_that("explain() decomposes a LightGBM regression exactly", {
  mod <- train(
    .regression_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  x <- explain(mod, .regression_dat[1:10, .explain_feats], verbosity = 0L)
  expect_s7_class(x, SHAP)
  expect_identical(x@algorithm, "LightGBM")
  expect_identical(x@estimator, "TreeSHAP")
  expect_true(x@exact)
  expect_identical(x@space, "input")
  expect_identical(x@feature_names, .explain_feats)
  # Regression has one output and no class to name it after.
  expect_named(x@phi, "outcome")
  expect_additive(x)
  # For regression the margin *is* the prediction, so the two agree here and
  # only here.
  expect_equal(
    as.numeric(x@predicted[, 1L]),
    as.numeric(predict(mod, .regression_dat[1:10, .explain_feats])),
    tolerance = 1e-10
  )
})


# %% LightGBM: binary ----
test_that("a binary explanation is on the margin, not the probability", {
  # Correction 6, as a test: `predict.Supervised` normalizes classification to
  # probabilities via `prob_matrix()`, and contributions live on the margin.
  mod <- train(.binary_dat, hyperparameters = setup_LightGBM(), verbosity = 0L)
  x <- explain(mod, .binary_dat[1:10, .explain_feats], verbosity = 0L)
  expect_identical(x@scale, "margin")
  expect_additive(x)
  probability <- predict(mod, .binary_dat[1:10, .explain_feats], verbosity = 0L)
  reconstructed <- rowSums(x@phi[[1L]]) + x@baseline[1L, 1L]
  # The link relates them...
  expect_equal(
    as.numeric(stats::plogis(reconstructed)),
    as.numeric(probability[, "pos"]),
    tolerance = 1e-10
  )
  # ...and without it they are simply different numbers.
  expect_false(
    isTRUE(all.equal(
      as.numeric(reconstructed),
      as.numeric(probability[, "pos"])
    ))
  )
})


test_that("a binary explanation is named for the positive class", {
  # The same rule `prob_matrix()` uses, so a SHAP's classes and a predicted
  # probability matrix's columns agree. The negative class's contributions are
  # the exact negation, so storing them would store one thing twice.
  mod <- train(.binary_dat, hyperparameters = setup_LightGBM(), verbosity = 0L)
  x <- explain(mod, .binary_dat[1:5, .explain_feats], verbosity = 0L)
  expect_length(x@phi, 1L)
  expect_named(x@phi, levels(mod@y_training)[[mod@binclasspos]])
})


# %% LightGBM: multiclass ----
test_that("a multiclass explanation is one matrix per class, each additive", {
  mod <- train(
    .multiclass_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  x <- explain(mod, .multiclass_dat[1:10, .explain_feats], verbosity = 0L)
  expect_named(x@phi, levels(mod@y_training))
  expect_identical(dim(x@predicted), c(10L, 3L))
  expect_additive(x)
})


test_that("regression, binary and multiclass differ only in length", {
  # The structure promise: a consumer written for multiclass handles the others
  # without special-casing.
  models <- list(
    regression = train(
      .regression_dat,
      hyperparameters = setup_LightGBM(),
      verbosity = 0L
    ),
    binary = train(
      .binary_dat,
      hyperparameters = setup_LightGBM(),
      verbosity = 0L
    ),
    multiclass = train(
      .multiclass_dat,
      hyperparameters = setup_LightGBM(),
      verbosity = 0L
    )
  )
  for (name in names(models)) {
    x <- explain(
      models[[name]],
      .explain_dat[1:6, .explain_feats],
      verbosity = 0L
    )
    expect_true(is.list(x@phi), info = name)
    expect_identical(dim(x@phi[[1L]]), c(6L, 3L), info = name)
    expect_identical(ncol(x@predicted), length(x@phi), info = name)
    expect_identical(colnames(x@baseline), names(x@phi), info = name)
    expect_identical(dim(x@baseline), dim(x@predicted), info = name)
  }
})


# %% Factors ----
test_that("a factor predictor gets one contribution, not one per level", {
  set.seed(7L)
  dat <- data.frame(
    age = rnorm(.n),
    dx = factor(sample(c("A", "B", "C"), .n, TRUE))
  )
  dat[["y"]] <- dat[["age"]] * 2 + as.integer(dat[["dx"]]) + rnorm(.n, sd = 0.3)
  mod <- train(dat, hyperparameters = setup_LightGBM(), verbosity = 0L)
  x <- explain(mod, dat[1:5, c("age", "dx")], verbosity = 0L)
  expect_identical(x@feature_names, c("age", "dx"))
  expect_identical(x@space, "input")
  expect_additive(x)
  # LightGBM encodes factors as integers rather than expanding them, so nothing
  # was aggregated and the encoded values would be the same numbers.
  expect_null(x@phi_encoded)
})


# %% Resolution ----
test_that("estimator, value function and scale are resolved and recorded", {
  mod <- train(
    .regression_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  x <- explain(mod, .regression_dat[1:5, .explain_feats], verbosity = 0L)
  # Asked for "auto" and got the concrete estimator; the two vocabularies do
  # not overlap, so the pair is unambiguous.
  expect_identical(x@config@estimator, "auto")
  expect_identical(x@estimator, "TreeSHAP")
  # NULL meant "resolve me", and what it resolved to is on the object rather
  # than left to be inferred from the backend's habits.
  expect_null(x@config@perturbation)
  expect_identical(x@perturbation, "conditional")
  expect_identical(
    x@perturbation,
    explanation_methods("LightGBM")[["perturbation"]]
  )
})


test_that("explain() records what it explained and what it explained against", {
  mod <- train(
    .regression_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  newdata <- .regression_dat[1:5, .explain_feats]
  background <- .regression_dat[, .explain_feats]
  x <- explain(mod, newdata, background = background, verbosity = 0L)
  expect_identical(x@data_fingerprint@hash, data_fingerprint(newdata)@hash)
  expect_identical(
    x@background_fingerprint@hash,
    data_fingerprint(background)@hash
  )
  # No background is a legitimate answer for a path-dependent estimator, which
  # takes its baseline from the model itself.
  expect_null(explain(mod, newdata, verbosity = 0L)@background_fingerprint)
})


# %% Refusals ----
test_that("estimator = 'exact' is refused where there is no exact estimator", {
  # Silently falling back to the kernel estimator would answer a different
  # question than the one asked.
  mod <- train(
    .regression_dat,
    hyperparameters = setup_Ranger(),
    verbosity = 0L
  )
  expect_error(
    explain(
      mod,
      .regression_dat[1:5, .explain_feats],
      config = setup_SHAP(estimator = "exact"),
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


test_that("an interventional TreeSHAP is refused rather than relabeled", {
  # The booster's contributions are path-dependent. Returning them under an
  # interventional label is exactly the silent mislabeling the perturbation
  # decision exists to prevent.
  mod <- train(
    .regression_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  expect_error(
    explain(
      mod,
      .regression_dat[1:5, .explain_feats],
      config = setup_SHAP(perturbation = "interventional"),
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


test_that("the probability scale is refused, differently, per outcome type", {
  regression <- train(
    .regression_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  # Not a thing a regression has.
  expect_error(
    explain(
      regression,
      .regression_dat[1:5, .explain_feats],
      config = setup_SHAP(scale = "probability"),
      verbosity = 0L
    ),
    class = "rtemis_value_error"
  )
  classification <- train(
    .binary_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  # A thing a classification has, which is not implemented yet.
  expect_error(
    explain(
      classification,
      .binary_dat[1:5, .explain_feats],
      config = setup_SHAP(scale = "probability"),
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


test_that("an estimator that is not written yet says so, by algorithm", {
  # Every registered algorithm now has an implemented estimator, so this guards
  # the *next* one rather than a current gap: S7's dispatch error names a
  # backend class the user never chose, and it has to be translated into the
  # algorithm they did. Provoked by swapping in a model class no method covers,
  # which is the only way to reach the branch now.
  mod <- train(.regression_dat, hyperparameters = setup_GLM(), verbosity = 0L)
  mod@model <- structure(list(), class = "not_an_algorithm_rtemis_knows")
  message <- tryCatch(
    explain(
      mod,
      .regression_dat[1:5, .explain_feats],
      background = .regression_dat[, .explain_feats],
      verbosity = 0L
    ),
    error = conditionMessage
  )
  expect_match(message, "GLM")
  expect_match(message, "not implemented")
  # ...and it names the fallback that applies to every algorithm.
  expect_match(message, "kernel")
})


# %% LinearSHAP ----
set.seed(11L)
.linear_dat <- data.frame(
  age = rnorm(.n),
  bmi = rnorm(.n),
  dx = factor(sample(c("A", "B", "C"), .n, TRUE))
)
.linear_feats <- c("age", "bmi", "dx")
.linear_regression <- within(
  .linear_dat,
  y <- 2 * age - bmi + as.integer(dx) * 0.5 + rnorm(.n, sd = 0.3)
)
.linear_binary <- within(
  .linear_dat,
  y <- factor(ifelse(age + rnorm(.n, sd = 0.5) > 0, "pos", "neg"))
)
.linear_multiclass <- within(
  .linear_dat,
  y <- factor(c("a", "b", "c")[as.integer(cut(age, 3L))])
)


test_that("LinearSHAP decomposes a GLM exactly, on the link scale", {
  mod <- train(
    .linear_regression,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  x <- explain(
    mod,
    .linear_regression[1:8, .linear_feats],
    background = .linear_regression[, .linear_feats],
    verbosity = 0L
  )
  expect_identical(x@estimator, "LinearSHAP")
  expect_true(x@exact)
  expect_identical(x@perturbation, "interventional")
  expect_additive(x)
})


test_that("a factor gets one contribution, summed over its contrast columns", {
  # `glm()` expands a factor through `model.matrix()` rather than through a
  # Preprocessor, and treatment contrasts give k - 1 columns with the reference
  # level absorbed into the intercept. The `assign` attribute records which
  # column came from which term, so the sum is exact and needs no assumption
  # about the coding.
  mod <- train(
    .linear_regression,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  expect_true(all(c("dxB", "dxC") %in% names(stats::coef(mod@model))))
  x <- explain(
    mod,
    .linear_regression[1:5, .linear_feats],
    background = .linear_regression[, .linear_feats],
    verbosity = 0L
  )
  expect_identical(x@feature_names, .linear_feats)
  expect_identical(x@space, "input")
  expect_additive(x)
})


test_that("LinearSHAP decomposes a binary GLM on the logit, not the probability", {
  mod <- train(.linear_binary, hyperparameters = setup_GLM(), verbosity = 0L)
  x <- explain(
    mod,
    .linear_binary[1:8, .linear_feats],
    background = .linear_binary[, .linear_feats],
    verbosity = 0L
  )
  expect_additive(x)
  probability <- predict(
    mod,
    .linear_binary[1:8, .linear_feats],
    verbosity = 0L
  )
  expect_equal(
    as.numeric(stats::plogis(x@predicted[, 1L])),
    as.numeric(probability[, "pos"]),
    tolerance = 1e-8
  )
})


test_that("LinearSHAP decomposes a glmnet for every outcome type", {
  cases <- list(
    regression = .linear_regression,
    binary = .linear_binary,
    multiclass = .linear_multiclass
  )
  for (name in names(cases)) {
    dat <- cases[[name]]
    mod <- train(dat, hyperparameters = setup_GLMNET(), verbosity = 0L)
    x <- explain(
      mod,
      dat[1:8, .linear_feats],
      background = dat[, .linear_feats],
      verbosity = 0L
    )
    expect_identical(x@estimator, "LinearSHAP", info = name)
    expect_identical(x@feature_names, .linear_feats, info = name)
    expect_additive(x)
  }
})


test_that("LinearSHAP decomposes an NNLS, which has no intercept", {
  dat <- data.frame(age = rnorm(.n), bmi = rnorm(.n))
  dat[["y"]] <- 2 * dat[["age"]] + 0.5 * dat[["bmi"]] + rnorm(.n, sd = 0.3)
  feats <- c("age", "bmi")
  mod <- train(dat, hyperparameters = setup_NNLS(), verbosity = 0L)
  x <- explain(mod, dat[1:8, feats], background = dat[, feats], verbosity = 0L)
  expect_additive(x)
  # With no intercept the baseline is just the fit at the background's mean.
  expect_equal(
    x@baseline[1L, 1L],
    sum(mod@model@coefficients * colMeans(dat[, feats])),
    tolerance = 1e-10
  )
})


test_that("LinearSHAP needs a background, and says which argument", {
  # Unlike a path-dependent estimator, this one has no baseline without data:
  # `E[x_j]` is a property of the background, and a fitted model does not carry
  # the data it was trained on.
  mod <- train(
    .linear_regression,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  expect_error(
    explain(mod, .linear_regression[1:5, .linear_feats], verbosity = 0L),
    "background",
    class = "rtemis_missing_error"
  )
})


test_that("a conditional LinearSHAP is refused rather than mislabeled", {
  # `beta_j * (x_j - E[x_j])` uses the marginal mean and never the joint
  # distribution, so it is the interventional answer whatever it is called.
  mod <- train(
    .linear_regression,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  expect_error(
    explain(
      mod,
      .linear_regression[1:5, .linear_feats],
      background = .linear_regression[, .linear_feats],
      config = setup_SHAP(perturbation = "conditional"),
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


test_that("coefficients that do not describe the model are caught, not used", {
  # The check that makes reading coefficients safe: a regularization path read
  # at the wrong step, or coefficients in a different order than the design,
  # gives contributions that are internally consistent and describe a different
  # model -- which no additivity check downstream could detect.
  design <- matrix(rnorm(40L), ncol = 2L)
  background <- matrix(rnorm(80L), ncol = 2L)
  beta <- matrix(c(1, -2), ncol = 1L)
  honest <- linear_shap(design, background, beta, 0.5)
  expect_equal(
    as.numeric(honest[["predicted"]]),
    as.numeric(design %*% beta + 0.5),
    tolerance = 1e-12
  )
  expect_error(
    linear_shap(
      design,
      background,
      beta,
      0.5,
      margin = design %*% matrix(c(1, 2), ncol = 1L)
    ),
    "do not describe the fitted model"
  )
})


# %% Probed LinearSHAP ----
test_that("probing recovers an affine map exactly", {
  # A difference of an affine function, not a numerical derivative: there is no
  # step size and no approximation error.
  beta <- c(2, -1, 0.5)
  intercept <- 1.25
  margin_fn <- function(x) {
    matrix(as.numeric(as.matrix(x) %*% beta) + intercept, ncol = 1L)
  }
  map <- probe_linear_map(margin_fn, c(a = 10, b = -3, c = 0.1))
  expect_equal(as.numeric(map[["coefficients"]]), beta, tolerance = 1e-12)
  expect_equal(map[["intercept"]], intercept, tolerance = 1e-12)
})


test_that("probing is taken around the given point, not a transposed one", {
  # `base + diag(p)` recycles column-major and would probe `base[i] + e_i`,
  # which is a different point and wrong wherever `base` is not constant.
  beta <- c(3, -2)
  margin_fn <- function(x) {
    matrix(as.numeric(as.matrix(x) %*% beta), ncol = 1L)
  }
  for (base in list(c(a = 0, b = 0), c(a = 100, b = -50))) {
    map <- probe_linear_map(margin_fn, base)
    expect_equal(as.numeric(map[["coefficients"]]), beta, tolerance = 1e-12)
    expect_equal(map[["intercept"]], 0, tolerance = 1e-9)
  }
})


test_that("a model that is not affine is refused, not linearized", {
  # Probing returns the tangent plane at the base point for anything; the
  # reconstruction check is what stops that being reported as an explanation.
  margin_fn <- function(x) {
    matrix(as.numeric(as.matrix(x)[, 1L])^2, ncol = 1L)
  }
  design <- matrix(rnorm(30L), ncol = 2L, dimnames = list(NULL, c("a", "b")))
  expect_error(
    probed_linear_shap(design, design, margin_fn),
    "do not describe the fitted model"
  )
})


# %% SPLS and LinearSVM ----
set.seed(9L)
.probe_n <- 250L
.probe_dat <- data.frame(
  a = rnorm(.probe_n),
  b = rnorm(.probe_n),
  dx = factor(sample(c("A", "B", "C"), .probe_n, TRUE))
)
.probe_feats <- c("a", "b", "dx")
.probe_regression <- within(
  .probe_dat,
  y <- 2 * a - b + as.integer(dx) * 0.4 + rnorm(.probe_n, sd = 0.3)
)
.probe_binary <- within(
  .probe_dat,
  y <- factor(ifelse(a + rnorm(.probe_n, sd = 0.5) > 0, "pos", "neg"))
)
.probe_multiclass <- within(
  .probe_dat,
  y <- factor(c("a", "b", "c")[as.integer(cut(a, 3L))])
)


test_that("SPLS and LinearSVM decompose exactly, through their own one-hot", {
  # These build `setup_Preprocessor(one_hot = TRUE)` at train time, so this is
  # the aggregation path end to end: the backend sees `dx_A`/`dx_B`/`dx_C` and
  # the user gets one `dx`.
  cases <- list(
    SPLS_regression = list(.probe_regression, setup_SPLS(k = 2L)),
    SPLS_binary = list(.probe_binary, setup_SPLS(k = 2L)),
    LinearSVM_regression = list(.probe_regression, setup_LinearSVM()),
    LinearSVM_binary = list(.probe_binary, setup_LinearSVM())
  )
  for (name in names(cases)) {
    dat <- cases[[name]][[1L]]
    mod <- train(dat, hyperparameters = cases[[name]][[2L]], verbosity = 0L)
    x <- explain(
      mod,
      dat[1:8, .probe_feats],
      background = dat[, .probe_feats],
      verbosity = 0L
    )
    expect_identical(x@estimator, "LinearSHAP", info = name)
    expect_identical(x@feature_names, .probe_feats, info = name)
    expect_identical(x@space, "input", info = name)
    expect_additive(x)
  }
})


test_that("a shared backend class routes each algorithm to its own estimator", {
  # Both SVMs are `e1071::svm`. LinearSVM dispatches to the method; RadialSVM
  # never reaches it, because the kernel estimator is model-agnostic and is
  # handled before dispatch.
  skip_if_not_installed("shapr")
  linear <- train(
    .probe_regression,
    hyperparameters = setup_LinearSVM(),
    verbosity = 0L
  )
  radial <- train(
    .probe_regression,
    hyperparameters = setup_RadialSVM(),
    verbosity = 0L
  )
  expect_identical(
    explain(
      linear,
      .probe_regression[1:3, .probe_feats],
      background = .probe_regression[, .probe_feats],
      verbosity = 0L
    )@estimator,
    "LinearSHAP"
  )
  expect_identical(
    explain(
      radial,
      .probe_regression[1:3, .probe_feats],
      background = .probe_regression[, .probe_feats],
      verbosity = 0L
    )@estimator,
    "KernelSHAP"
  )
})


test_that("a binary SVM's margin points at the positive class", {
  # `e1071` names the decision-value column "A/B" with a positive value favoring
  # A, and which class that is follows from the training data's order, not the
  # level order. Taking it as given would invert every contribution for about
  # half of all fitted models.
  mod <- train(
    .probe_binary,
    hyperparameters = setup_LinearSVM(),
    verbosity = 0L
  )
  x <- explain(
    mod,
    .probe_binary[1:20, .probe_feats],
    background = .probe_binary[, .probe_feats],
    verbosity = 0L
  )
  expect_named(x@phi, levels(mod@y_training)[[mod@binclasspos]])
  probability <- predict(
    mod,
    .probe_binary[1:20, .probe_feats],
    verbosity = 0L
  )
  expect_gt(cor(as.numeric(x@predicted[, 1L]), probability[, "pos"]), 0)
})


test_that("SPLS's binary margin is the logit of the reported probability", {
  mod <- train(
    .probe_binary,
    hyperparameters = setup_SPLS(k = 2L),
    verbosity = 0L
  )
  x <- explain(
    mod,
    .probe_binary[1:8, .probe_feats],
    background = .probe_binary[, .probe_feats],
    verbosity = 0L
  )
  probability <- predict(mod, .probe_binary[1:8, .probe_feats], verbosity = 0L)
  expect_equal(
    as.numeric(stats::plogis(x@predicted[, 1L])),
    as.numeric(probability[, "pos"]),
    tolerance = 1e-6
  )
})


test_that("multiclass SPLS and LinearSVM are refused, naming the fallback", {
  # One linear map per model, and a multiclass fit of either has none: SPLS's
  # per-class scores are identified only up to a constant, and a multiclass SVM
  # is one-vs-one voting.
  for (hyperparameters in list(setup_SPLS(k = 2L), setup_LinearSVM())) {
    mod <- train(
      .probe_multiclass,
      hyperparameters = hyperparameters,
      verbosity = 0L
    )
    expect_error(
      explain(
        mod,
        .probe_multiclass[1:5, .probe_feats],
        background = .probe_multiclass[, .probe_feats],
        verbosity = 0L
      ),
      "kernel",
      class = "rtemis_unsupported_error"
    )
  }
})


# %% Additive terms: GAM and MARS ----
set.seed(4L)
.terms_n <- 250L
.terms_dat <- data.frame(
  a = rnorm(.terms_n),
  b = rnorm(.terms_n),
  dx = factor(sample(c("A", "B", "C"), .terms_n, TRUE))
)
.terms_feats <- c("a", "b", "dx")
.terms_regression <- within(
  .terms_dat,
  y <- sin(a) + b * 0.5 + as.integer(dx) * 0.5 + rnorm(.terms_n, sd = 0.2)
)
.terms_binary <- within(
  .terms_dat,
  y <- factor(ifelse(sin(a) + rnorm(.terms_n, sd = 0.5) > 0, "pos", "neg"))
)


test_that("a GAM's own terms are its Shapley values", {
  # `train_GAM()` builds one smooth or parametric term per feature, so the model
  # already is the additive decomposition; nothing needs enumerating.
  for (dat in list(.terms_regression, .terms_binary)) {
    mod <- train(dat, hyperparameters = setup_GAM(), verbosity = 0L)
    x <- explain(
      mod,
      dat[1:8, .terms_feats],
      background = dat[, .terms_feats],
      verbosity = 0L
    )
    expect_identical(x@estimator, "GAMTerms")
    expect_true(x@exact)
    expect_identical(x@feature_names, .terms_feats)
    expect_additive(x)
  }
})


test_that("a GAM baseline is the background's mean prediction", {
  # Re-centered on the supplied background rather than on whatever mgcv
  # centered its smooths on, so a baseline means the same thing here as for
  # every other estimator.
  mod <- train(.terms_regression, hyperparameters = setup_GAM(), verbosity = 0L)
  x <- explain(
    mod,
    .terms_regression[1:8, .terms_feats],
    background = .terms_regression[, .terms_feats],
    verbosity = 0L
  )
  expect_equal(
    x@baseline[1L, 1L],
    mean(predict(mod, .terms_regression[, .terms_feats], verbosity = 0L)),
    tolerance = 1e-8
  )
})


test_that("a GAM captures nonlinearity a linear estimator could not", {
  # The point of using the terms: `sin(a)` is not monotone in `a`, so a
  # coefficient could not express what the smooth does.
  mod <- train(.terms_regression, hyperparameters = setup_GAM(), verbosity = 0L)
  x <- explain(
    mod,
    .terms_regression[, .terms_feats],
    background = .terms_regression[, .terms_feats],
    verbosity = 0L
  )
  contribution <- x@phi[[1L]][, "a"]
  # A linear contribution would be a perfect line in `a`; a smooth of sin() is
  # not, while still being monotone over this range's rank order.
  expect_lt(
    abs(cor(contribution, .terms_regression[["a"]])),
    0.999
  )
})


test_that("an additive MARS decomposes exactly through its own one-hot", {
  mod <- train(
    .terms_regression,
    hyperparameters = setup_MARS(),
    verbosity = 0L
  )
  x <- explain(
    mod,
    .terms_regression[1:8, .terms_feats],
    background = .terms_regression[, .terms_feats],
    verbosity = 0L
  )
  expect_identical(x@estimator, "MARSBasis")
  expect_identical(x@feature_names, .terms_feats)
  expect_identical(x@space, "input")
  expect_additive(x)
  # `earth` drops a one-hot level it did not select, and a feature with no term
  # contributes exactly nothing -- which the encoded view still shows.
  expect_true(all(
    c("dx_A", "dx_B", "dx_C") %in% colnames(x@phi_encoded[[1L]])
  ))
})


test_that("MARS is refused when the fit is not additive, not when asked", {
  # The gate is the fitted model, not `degree`: a degree-2 search that selected
  # no interaction is additive and is explained.
  set.seed(11L)
  n <- 400L
  interacting <- data.frame(a = rnorm(n), b = rnorm(n))
  interacting[["y"]] <- 3 *
    interacting[["a"]] *
    (interacting[["b"]] > 0) -
    2 * interacting[["a"]] * (interacting[["b"]] <= 0) +
    rnorm(n, sd = 0.2)
  feats <- c("a", "b")
  selected <- train(
    interacting,
    hyperparameters = setup_MARS(degree = 2L),
    verbosity = 0L
  )
  dirs <- selected@model[["dirs"]][
    selected@model[["selected.terms"]],
    ,
    drop = FALSE
  ]
  expect_gt(sum(rowSums(dirs != 0) > 1L), 0L)
  expect_error(
    explain(
      selected,
      interacting[1:5, feats],
      background = interacting[, feats],
      verbosity = 0L
    ),
    "not additive",
    class = "rtemis_unsupported_error"
  )
  # The same data fitted additively is explained.
  additive <- train(
    interacting,
    hyperparameters = setup_MARS(degree = 1L),
    verbosity = 0L
  )
  expect_additive(
    explain(
      additive,
      interacting[1:5, feats],
      background = interacting[, feats],
      verbosity = 0L
    )
  )
})


test_that("MARS classification is refused: a GLM sits over the basis", {
  mod <- train(.terms_binary, hyperparameters = setup_MARS(), verbosity = 0L)
  expect_error(
    explain(
      mod,
      .terms_binary[1:5, .terms_feats],
      background = .terms_binary[, .terms_feats],
      verbosity = 0L
    ),
    "GLM over the basis",
    class = "rtemis_unsupported_error"
  )
})


test_that("terms_feature_map refuses a term that reads two features", {
  expect_identical(
    terms_feature_map(c("s(age)", "dx"), c("age", "dx")),
    c("age", "dx")
  )
  # `s(ab)` must not be matched to a feature called `a`.
  expect_error(
    terms_feature_map("s(ab)", c("a", "b")),
    "reads 0 features"
  )
  expect_error(
    terms_feature_map("te(age, bmi)", c("age", "bmi")),
    "reads 2 features"
  )
})


# %% CART TreeSHAP ----
set.seed(2L)
.cart_n <- 300L
.cart_dat <- data.frame(
  a = rnorm(.cart_n),
  b = rnorm(.cart_n),
  dx = factor(sample(c("A", "B", "C"), .cart_n, TRUE))
)
.cart_feats <- c("a", "b", "dx")
.cart_dat[["y"]] <- ifelse(.cart_dat[["a"]] > 0, 2, -1) +
  ifelse(.cart_dat[["b"]] > 0.5, 1, 0) +
  as.integer(.cart_dat[["dx"]]) * 0.4 +
  rnorm(.cart_n, sd = 0.2)


test_that("CART TreeSHAP is exact for every outcome type", {
  cases <- list(
    regression = .cart_dat,
    binary = within(
      .cart_dat,
      y <- factor(ifelse(a + rnorm(.cart_n, sd = 0.4) > 0, "pos", "neg"))
    ),
    multiclass = within(
      .cart_dat,
      y <- factor(c("p", "q", "r")[as.integer(cut(a, 3L))])
    )
  )
  for (name in names(cases)) {
    mod <- train(cases[[name]], hyperparameters = setup_CART(), verbosity = 0L)
    x <- explain(mod, cases[[name]][1:8, .cart_feats], verbosity = 0L)
    expect_identical(x@estimator, "TreeSHAP", info = name)
    expect_true(x@exact, info = name)
    expect_additive(x)
    # The traversal must reproduce the model's own predictions, which is what
    # makes reading `rpart`'s split table safe.
    expect_equal(
      unname(x@predicted),
      unname(as.matrix(predict(
        mod,
        cases[[name]][1:8, .cart_feats],
        verbosity = 0L
      ))[,
        if (name == "binary") "pos" else seq_len(ncol(x@predicted)),
        drop = FALSE
      ]),
      tolerance = 1e-10,
      info = name
    )
  }
})


test_that("a CART baseline is the tree's own expected prediction", {
  mod <- train(.cart_dat, hyperparameters = setup_CART(), verbosity = 0L)
  x <- explain(mod, .cart_dat[1:8, .cart_feats], verbosity = 0L)
  # The empty coalition marginalizes every split by training coverage, which is
  # the mean prediction over the training data.
  expect_equal(
    x@baseline[1L, 1L],
    mean(predict(mod, .cart_dat[, .cart_feats], verbosity = 0L)),
    tolerance = 1e-8
  )
})


test_that("a feature the tree never split on gets exactly zero", {
  # The dummy axiom, and the reason the enumeration runs over the tree's own
  # features rather than every column.
  set.seed(5L)
  dat <- data.frame(a = rnorm(200L), noise = rnorm(200L))
  dat[["y"]] <- ifelse(dat[["a"]] > 0, 3, -3) + rnorm(200L, sd = 0.1)
  mod <- train(dat, hyperparameters = setup_CART(), verbosity = 0L)
  x <- explain(mod, dat[1:5, c("a", "noise")], verbosity = 0L)
  expect_true(all(x@phi[[1L]][, "noise"] == 0))
  expect_false(any(x@phi[[1L]][, "a"] == 0))
})


test_that("a duplicate feature the tree ignored gets zero, not half", {
  # "True to the model": a perfect proxy the tree never split on is a dummy
  # player in the path-dependent value function, so symmetry does not apply to
  # it. This is the interventional/conditional distinction showing up in a
  # place people find surprising, and it is correct.
  set.seed(6L)
  dat <- data.frame(a = rnorm(300L))
  dat[["copy"]] <- dat[["a"]]
  dat[["y"]] <- ifelse(dat[["a"]] > 0, 4, -4) + rnorm(300L, sd = 0.05)
  mod <- train(dat, hyperparameters = setup_CART(), verbosity = 0L)
  x <- explain(mod, dat[1:5, c("a", "copy")], verbosity = 0L)
  split_on <- unique(as.character(mod@model[["frame"]][["var"]]))
  expect_length(intersect(c("a", "copy"), split_on), 1L)
  ignored <- setdiff(c("a", "copy"), split_on)
  expect_true(all(x@phi[[1L]][, ignored] == 0))
})


test_that("cart_tree reads split rules aligned to their own nodes", {
  # `primary` holds 0 at every leaf, and indexing `splits` with the whole vector
  # would drop those rather than return NA -- shortening the result so every
  # rule lands against the wrong node. Routing is checked against `rpart`'s own
  # leaf assignment, which is the only way to see that.
  mod <- train(.cart_dat, hyperparameters = setup_CART(), verbosity = 0L)
  tree <- cart_tree(mod@model)
  expect_length(tree[["ncat"]], nrow(mod@model[["frame"]]))
  expect_length(tree[["index"]], nrow(mod@model[["frame"]]))
  expect_true(all(is.na(tree[["ncat"]][tree[["is_leaf"]]])))
  expect_false(any(is.na(tree[["ncat"]][!tree[["is_leaf"]]])))
  # Routing every training case must land where rpart says it does.
  known <- stats::setNames(
    rep(TRUE, length(unique(tree[["feature"]][!tree[["is_leaf"]]]))),
    unique(tree[["feature"]][!tree[["is_leaf"]]])
  )
  routed <- cart_coalition_value(
    tree,
    1L,
    .cart_dat[, .cart_feats],
    known
  )
  expect_equal(
    as.numeric(routed),
    as.numeric(predict(mod, .cart_dat[, .cart_feats], verbosity = 0L)),
    tolerance = 1e-10
  )
})


test_that("a tree splitting on too many features is refused, not left to run", {
  # 2^p traversals: past the limit the kernel estimator is the right tool.
  expect_gt(CART_SHAP_MAX_FEATURES, 8L)
  set.seed(8L)
  wide <- as.data.frame(matrix(rnorm(300L * 20L), ncol = 20L))
  wide[["y"]] <- rowSums(wide[, 1:20]) + rnorm(300L, sd = 0.1)
  mod <- train(
    wide,
    hyperparameters = setup_CART(maxdepth = 30L, minsplit = 2L, cp = 0),
    verbosity = 0L
  )
  used <- length(unique(as.character(mod@model[["frame"]][["var"]]))) - 1L
  skip_if_not(
    used > CART_SHAP_MAX_FEATURES,
    "tree is small enough to enumerate"
  )
  expect_error(
    explain(mod, wide[1:2, paste0("V", 1:20)], verbosity = 0L),
    "coalitions",
    class = "rtemis_unsupported_error"
  )
})


# %% HAL basis walk ----
test_that("HAL's basis walk is exact when every selected basis reads one feature", {
  set.seed(7L)
  n <- 200L
  dat <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    dx = factor(sample(c("A", "B"), n, TRUE))
  )
  dat[["y"]] <- 2 *
    dat[["a"]] -
    dat[["b"]] +
    as.integer(dat[["dx"]]) * 0.5 +
    rnorm(n, sd = 0.3)
  feats <- c("a", "b", "dx")
  mod <- train(
    dat,
    hyperparameters = setup_HAL(max_degree = 1L),
    verbosity = 0L
  )
  x <- explain(mod, dat[1:6, feats], background = dat[, feats], verbosity = 0L)
  expect_identical(x@estimator, "HALBasis")
  expect_identical(x@feature_names, feats)
  expect_additive(x)
  expect_equal(
    as.numeric(x@predicted[, 1L]),
    as.numeric(predict(mod, dat[1:6, feats], verbosity = 0L)),
    tolerance = 1e-8
  )
})


test_that("HAL's default fit is refused, naming the setting that fixes it", {
  # `setup_HAL()` defaults to `max_degree = 2L`, so a basis reading two features
  # is the common case rather than an edge one, and the message has to be
  # actionable rather than merely correct.
  set.seed(7L)
  n <- 200L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- 2 * dat[["a"]] - dat[["b"]] + rnorm(n, sd = 0.3)
  feats <- c("a", "b")
  expect_identical(formals(setup_HAL)[["max_degree"]], quote(2L))
  mod <- train(dat, hyperparameters = setup_HAL(), verbosity = 0L)
  expect_error(
    explain(mod, dat[1:5, feats], background = dat[, feats], verbosity = 0L),
    "max_degree = 1L",
    class = "rtemis_unsupported_error"
  )
})


# %% KernelSHAP ----
set.seed(4L)
.kernel_n <- 250L
.kernel_dat <- data.frame(
  a = rnorm(.kernel_n),
  b = rnorm(.kernel_n),
  c = rnorm(.kernel_n)
)
.kernel_feats <- c("a", "b", "c")
.kernel_regression <- within(
  .kernel_dat,
  y <- 2 * a - b + 0.5 * c + rnorm(.kernel_n, sd = 0.3)
)


test_that("KernelSHAP agrees with the exact LinearSHAP on the same model", {
  # The check that validates the exact tier against an implementation we did
  # not write. For a linear model the interventional Shapley value has a closed
  # form, and shapr's `independence` approach computes the same quantity by an
  # entirely different route, so the two must agree.
  skip_if_not_installed("shapr")
  mod <- train(
    .kernel_regression,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  newdata <- .kernel_regression[1:5, .kernel_feats]
  background <- .kernel_regression[, .kernel_feats]
  exact <- explain(mod, newdata, background = background, verbosity = 0L)
  kernel <- explain(
    mod,
    newdata,
    background = background,
    config = setup_SHAP(estimator = "kernel"),
    verbosity = 0L
  )
  expect_identical(exact@estimator, "LinearSHAP")
  expect_identical(kernel@estimator, "KernelSHAP")
  expect_identical(exact@perturbation, kernel@perturbation)
  expect_equal(kernel@phi[[1L]], exact@phi[[1L]], tolerance = 1e-6)
  expect_equal(kernel@baseline, exact@baseline, tolerance = 1e-8)
})


test_that("KernelSHAP answers for the algorithms with no exact estimator", {
  skip_if_not_installed("shapr")
  for (algorithm in c("Ranger", "RadialSVM", "KNN")) {
    mod <- train(
      .kernel_regression,
      hyperparameters = do.call(paste0("setup_", algorithm), list()),
      verbosity = 0L
    )
    x <- explain(
      mod,
      .kernel_regression[1:4, .kernel_feats],
      background = .kernel_regression[, .kernel_feats],
      verbosity = 0L
    )
    expect_identical(x@estimator, "KernelSHAP", info = algorithm)
    # Sampled, so `exact` is FALSE and additivity holds to the sampling error
    # rather than to machine precision.
    expect_false(x@exact, info = algorithm)
    expect_additive(x, tolerance = 1e-6)
  }
})


test_that("KernelSHAP explains a classification on the probability scale", {
  # It sees only what `predict()` returns. Its contributions are additive *on
  # that*, exactly, because it decomposes the function it was handed rather
  # than transforming a margin decomposition -- so this is honest, and merely
  # not comparable with a margin-scale explanation of the same model.
  skip_if_not_installed("shapr")
  binary <- within(
    .kernel_dat,
    y <- factor(ifelse(a + rnorm(.kernel_n, sd = 0.4) > 0, "pos", "neg"))
  )
  mod <- train(binary, hyperparameters = setup_Ranger(), verbosity = 0L)
  x <- explain(
    mod,
    binary[1:4, .kernel_feats],
    background = binary[, .kernel_feats],
    verbosity = 0L
  )
  expect_identical(x@scale, "probability")
  expect_named(x@phi, "pos")
  expect_additive(x, tolerance = 1e-6)
  expect_equal(
    as.numeric(x@predicted[, 1L]),
    as.numeric(predict(mod, binary[1:4, .kernel_feats], verbosity = 0L)[,
      "pos"
    ]),
    tolerance = 1e-10
  )
})


test_that("KernelSHAP gives multiclass one block per class", {
  skip_if_not_installed("shapr")
  multiclass <- within(
    .kernel_dat,
    y <- factor(c("p", "q", "r")[as.integer(cut(a, 3L))])
  )
  mod <- train(multiclass, hyperparameters = setup_Ranger(), verbosity = 0L)
  x <- explain(
    mod,
    multiclass[1:4, .kernel_feats],
    background = multiclass[, .kernel_feats],
    verbosity = 0L
  )
  expect_named(x@phi, levels(mod@y_training))
  expect_additive(x, tolerance = 1e-6)
})


test_that("the value function is the perturbation, not an approach to guess", {
  # The reason this plan chose shapr: `independence` *is* the interventional
  # value function, so the choice decision 1 says must be explicit is a
  # first-class argument rather than something inferred from a default.
  skip_if_not_installed("shapr")
  mod <- train(
    .kernel_regression,
    hyperparameters = setup_Ranger(),
    verbosity = 0L
  )
  conditional <- explain(
    mod,
    .kernel_regression[1:4, .kernel_feats],
    background = .kernel_regression[, .kernel_feats],
    config = setup_SHAP(
      estimator = "kernel",
      perturbation = "conditional",
      # "gaussian" rather than "ctree": the latter warns its way to a fallback
      # when `party` is absent, and the conditional path is what is under test,
      # not which estimator of it.
      approach = "gaussian"
    ),
    verbosity = 0L
  )
  expect_identical(conditional@perturbation, "conditional")
  expect_additive(conditional, tolerance = 1e-6)
  # `independence` is absent from the approach enum because `perturbation`
  # already says it; the two must not be sayable separately.
  expect_false("independence" %in% SHAP_APPROACHES)
})


test_that("KernelSHAP needs a background and refuses the margin scale", {
  skip_if_not_installed("shapr")
  binary <- within(
    .kernel_dat,
    y <- factor(ifelse(a + rnorm(.kernel_n, sd = 0.4) > 0, "pos", "neg"))
  )
  mod <- train(binary, hyperparameters = setup_Ranger(), verbosity = 0L)
  expect_error(
    explain(mod, binary[1:3, .kernel_feats], verbosity = 0L),
    "background",
    class = "rtemis_missing_error"
  )
  expect_error(
    explain(
      mod,
      binary[1:3, .kernel_feats],
      background = binary[, .kernel_feats],
      config = setup_SHAP(estimator = "kernel", scale = "margin"),
      verbosity = 0L
    ),
    class = "rtemis_unsupported_error"
  )
})


test_that("every refusal's suggested escape hatch actually works", {
  # Several messages name `setup_SHAP(estimator = "kernel")` as the way out.
  # Pointing a user at a door that does not open would be worse than refusing
  # without advice.
  skip_if_not_installed("shapr")
  dat <- .kernel_regression
  feats <- .kernel_feats
  refused <- list(
    # An algorithm with no exact estimator at all.
    Ranger = list(setup_Ranger(), setup_SHAP(estimator = "exact")),
    # A fit whose terms are not additive in its features.
    HAL = list(setup_HAL(), NULL)
  )
  for (name in names(refused)) {
    mod <- train(
      dat,
      hyperparameters = refused[[name]][[1L]],
      verbosity = 0L
    )
    config <- refused[[name]][[2L]]
    if (!is.null(config)) {
      expect_error(
        explain(
          mod,
          dat[1:3, feats],
          background = dat[, feats],
          config = config,
          verbosity = 0L
        ),
        class = "rtemis_unsupported_error",
        info = name
      )
    }
    escape <- explain(
      mod,
      dat[1:3, feats],
      background = dat[, feats],
      config = setup_SHAP(estimator = "kernel"),
      verbosity = 0L
    )
    expect_identical(escape@estimator, "KernelSHAP", info = name)
    expect_additive(escape, tolerance = 1e-6)
  }
})


# %% SupervisedRes ----
.resampled <- function(dat, hyperparameters = setup_GLM()) {
  train(
    dat,
    hyperparameters = hyperparameters,
    outer_resampling_config = setup_Resampler(n_resamples = 4L, type = "KFold"),
    verbosity = 0L
  )
}


test_that("the fold average decomposes the fold-averaged prediction exactly", {
  # The claim that makes averaging principled rather than a convenience: since
  # sum(phi_k) + b_k = f_k(x) per fold, averaging both sides gives
  # sum(phi_bar) + b_bar = f_bar(x). It needs one background shared by every
  # fold, which `explain()` enforces by taking a single argument.
  res <- .resampled(.regression_dat)
  newdata <- .regression_dat[1:6, .explain_feats]
  x <- explain(
    res,
    newdata,
    background = .regression_dat[, .explain_feats],
    verbosity = 0L
  )
  expect_s7_class(x, SHAP)
  expect_additive(x)
  expect_equal(
    as.numeric(x@predicted[, 1L]),
    as.numeric(predict(res, newdata, type = "avg")),
    tolerance = 1e-10
  )
})


test_that("type = 'all' returns one explanation per resample", {
  res <- .resampled(.regression_dat)
  newdata <- .regression_dat[1:5, .explain_feats]
  background <- .regression_dat[, .explain_feats]
  each <- explain(
    res,
    newdata,
    background = background,
    type = "all",
    verbosity = 0L
  )
  expect_length(each, length(res@models))
  expect_named(each, names(res@models))
  for (one in each) {
    expect_s7_class(one, SHAP)
    expect_additive(one)
  }
  # ...and the average is exactly their mean, not a re-estimate.
  averaged <- explain(res, newdata, background = background, verbosity = 0L)
  expect_equal(
    averaged@phi[[1L]],
    Reduce(`+`, lapply(each, function(one) one@phi[[1L]])) / length(each),
    tolerance = 1e-12
  )
})


test_that("averaging is refused when the folds do not agree", {
  # A mean of contributions on different scales, or over different features,
  # describes nothing. Models fitted to resamples of one dataset always agree;
  # the check exists because the failure would otherwise be silent.
  res <- .resampled(.regression_dat)
  newdata <- .regression_dat[1:4, .explain_feats]
  background <- .regression_dat[, .explain_feats]
  each <- explain(
    res,
    newdata,
    background = background,
    type = "all",
    verbosity = 0L
  )
  each[[2L]]@scale <- "probability"
  expect_error(average_shap(each), "@scale", class = "rtemis_value_error")
})


test_that("a resampled classification averages margins, not probabilities", {
  # The same link distinction as for a single model: `predict(type = "avg")`
  # averages probabilities, and the contributions decompose the mean margin.
  res <- .resampled(.binary_dat)
  newdata <- .binary_dat[1:6, .explain_feats]
  x <- explain(
    res,
    newdata,
    background = .binary_dat[, .explain_feats],
    verbosity = 0L
  )
  expect_identical(x@scale, "margin")
  expect_additive(x)
  probability <- predict(res, newdata, type = "avg")
  expect_false(
    isTRUE(all.equal(as.numeric(x@predicted[, 1L]), as.numeric(probability)))
  )
})


# %% Isotonic ----
test_that("a one-predictor model gives the whole deviation to its predictor", {
  # One player, so the Shapley value is the entire deviation from the baseline.
  # Additivity here is the definition rather than a check.
  set.seed(11L)
  n <- 200L
  dat <- data.frame(a = runif(n))
  dat[["y"]] <- 2 * dat[["a"]] + rnorm(n, sd = 0.1)
  feats <- "a"
  mod <- train(dat, hyperparameters = setup_Isotonic(), verbosity = 0L)
  x <- explain(
    mod,
    dat[1:6, feats, drop = FALSE],
    background = dat[, feats, drop = FALSE],
    verbosity = 0L
  )
  expect_identical(x@estimator, "Isotonic")
  expect_true(x@exact)
  expect_additive(x)
  expect_equal(
    as.numeric(x@phi[[1L]][, 1L]),
    as.numeric(x@predicted[, 1L] - x@baseline[, 1L]),
    tolerance = 1e-12
  )
})


# %% ConditionalSuperLearner ----
test_that("a routed case is explained by the expert that predicted it", {
  # The oracle sends each case to one expert, which predicts it alone, so the
  # expert's explanation *is* the ensemble's for that case -- exactly, and by
  # the expert's own estimator rather than a pass over the whole ensemble.
  set.seed(9L)
  n <- 300L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  # Two regimes an oracle can separate: the effect of `a` flips with `b`.
  dat[["y"]] <- ifelse(dat[["b"]] > 0, 3 * dat[["a"]], -3 * dat[["a"]]) +
    rnorm(n, sd = 0.3)
  feats <- c("a", "b")
  mod <- train(
    dat,
    hyperparameters = setup_ConditionalSuperLearner(
      base_learners = list(setup_GLM(), setup_GLM())
    ),
    verbosity = 0L
  )
  x <- explain(mod, dat[1:8, feats], background = dat[, feats], verbosity = 0L)
  expect_identical(x@estimator, "ExpertSHAP")
  # Both experts are GLMs, so both explanations are exact and so is the whole.
  expect_true(x@exact)
  expect_additive(x)
  expect_equal(
    as.numeric(x@predicted[, 1L]),
    as.numeric(predict(mod, dat[1:8, feats], verbosity = 0L)),
    tolerance = 1e-10
  )
})


test_that("a routed explanation's baseline is its own expert's", {
  # The reason `@baseline` is per case: cases routed to different experts are
  # each exact against a different expected prediction.
  set.seed(9L)
  n <- 300L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- ifelse(dat[["b"]] > 0, 3 * dat[["a"]], -3 * dat[["a"]]) +
    rnorm(n, sd = 0.3)
  feats <- c("a", "b")
  mod <- train(
    dat,
    hyperparameters = setup_ConditionalSuperLearner(
      base_learners = list(setup_GLM(), setup_GLM())
    ),
    verbosity = 0L
  )
  x <- explain(mod, dat[1:40, feats], background = dat[, feats], verbosity = 0L)
  expect_identical(dim(x@baseline), dim(x@predicted))
  # More than one distinct baseline, and no more than there are experts.
  distinct <- length(unique(round(x@baseline[, 1L], 10)))
  expect_gt(distinct, 1L)
  expect_lte(distinct, length(mod@model@experts))
})


test_that("a constant baseline still fills a row per case", {
  # One shape for both kinds of estimator, so no consumer branches on it.
  mod <- train(
    .regression_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  x <- explain(mod, .regression_dat[1:6, .explain_feats], verbosity = 0L)
  expect_identical(dim(x@baseline), c(6L, 1L))
  expect_length(unique(x@baseline[, 1L]), 1L)
})


# %% Categorical detail ----
set.seed(3L)
.dx_n <- 300L
.dx <- factor(
  sample(c("none", "diabetes", "asthma"), .dx_n, TRUE, prob = c(.6, .25, .15))
)
.dx_dat <- data.frame(age = rnorm(.dx_n), dx = .dx)
.dx_dat[["y"]] <- .dx_dat[["age"]] +
  3 * (.dx == "diabetes") +
  rnorm(.dx_n, sd = 0.3)
.dx_feats <- c("age", "dx")

.dx_explanation <- function(hyperparameters = setup_GLM()) {
  mod <- train(.dx_dat, hyperparameters = hyperparameters, verbosity = 0L)
  explain(
    mod,
    .dx_dat[, .dx_feats],
    background = .dx_dat[, .dx_feats],
    verbosity = 0L
  )
}


test_that("per-level contributions stay reachable, without being reported", {
  # `glm()` expands the factor itself, so the design-matrix values are the
  # per-level view. They are kept rather than discarded, but they are not the
  # answer to "which level matters" -- see below.
  x <- .dx_explanation()
  expect_identical(colnames(x@phi[["outcome"]]), .dx_feats)
  expect_false(is.null(x@phi_encoded))
  expect_true(all(
    c("dxdiabetes", "dxnone") %in% colnames(x@phi_encoded[[1L]])
  ))
  # The reference level has no column at all: a per-level view of a
  # contrast-coded model structurally cannot show it.
  expect_false("dxasthma" %in% colnames(x@phi_encoded[[1L]]))
  # Whatever the split, the total is the same.
  expect_equal(
    rowSums(x@phi_encoded[[1L]]),
    rowSums(x@phi[["outcome"]]),
    tolerance = 1e-10
  )
})


test_that("shap_by_level breaks a factor down by the level each case has", {
  # The question a per-feature average hides: "dx matters" is useless next to
  # "diabetes is what moves the prediction".
  x <- .dx_explanation()
  by_level <- shap_by_level(x, .dx_dat[, .dx_feats])
  expect_s3_class(by_level, "data.table")
  expect_identical(
    sort(by_level[["level"]]),
    sort(levels(.dx))
  )
  # Simulated so that only diabetes has an effect, and it is recovered.
  diabetes <- by_level[by_level[["level"]] == "diabetes", ][["mean"]]
  none <- by_level[by_level[["level"]] == "none", ][["mean"]]
  expect_gt(diabetes, 2)
  expect_lt(abs(none), 1)
  # ...and the reference level appears like any other, which is the thing the
  # per-level columns cannot do.
  expect_true("asthma" %in% by_level[["level"]])
  expect_identical(sum(by_level[["n"]]), .dx_n)
})


test_that("shap_by_level is per class, so multiclass is not a special case", {
  mod <- train(
    within(.dx_dat, y <- factor(c("a", "b", "c")[as.integer(cut(age, 3L))])),
    hyperparameters = setup_GLMNET(),
    verbosity = 0L
  )
  x <- explain(
    mod,
    .dx_dat[, .dx_feats],
    background = .dx_dat[, .dx_feats],
    verbosity = 0L
  )
  by_level <- shap_by_level(x, .dx_dat[, .dx_feats])
  expect_setequal(unique(by_level[["class"]]), names(x@phi))
  expect_identical(nrow(by_level), 3L * nlevels(.dx))
})


test_that("shap_by_level refuses data that is not what was explained", {
  # Contributions lined up against the wrong rows would produce a plausible
  # table describing nothing, so the fingerprint is checked rather than assumed.
  x <- .dx_explanation()
  expect_error(
    shap_by_level(x, .dx_dat[1:10, .dx_feats]),
    class = "rtemis_value_error"
  )
  expect_error(
    shap_by_level(x, .dx_dat[rev(seq_len(.dx_n)), .dx_feats]),
    class = "rtemis_value_error"
  )
})


test_that("shap_by_level says so when there is nothing categorical", {
  mod <- train(
    .regression_dat,
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  x <- explain(
    mod,
    .regression_dat[, .explain_feats],
    background = .regression_dat[, .explain_feats],
    verbosity = 0L
  )
  expect_error(
    shap_by_level(x, .regression_dat[, .explain_feats]),
    class = "rtemis_value_error"
  )
})


test_that("collapse_by_term sums design columns into their term", {
  phi <- matrix(
    1:12,
    nrow = 3L,
    dimnames = list(NULL, c("age", "dxB", "dxC", "bmi"))
  )
  out <- collapse_by_term(phi, c(1L, 3L, 3L, 2L), c("age", "bmi", "dx"))
  expect_identical(colnames(out), c("age", "bmi", "dx"))
  expect_equal(out[, "dx"], rowSums(phi[, c("dxB", "dxC")]))
  expect_equal(rowSums(out), rowSums(phi))
})


test_that("explain() inherits predict()'s strictness about columns", {
  mod <- train(
    .regression_dat,
    hyperparameters = setup_LightGBM(),
    verbosity = 0L
  )
  # Predictors only, in training order -- the same contract `predict()` states.
  expect_error(
    explain(mod, .regression_dat[1:5, ], verbosity = 0L),
    class = "rtemis_data_error"
  )
  expect_error(
    explain(mod, .regression_dat[1:5, c("bmi", "age", "sex")], verbosity = 0L),
    class = "rtemis_data_error"
  )
})
