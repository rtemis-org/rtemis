# test_Explanation.R
# ::rtemis::
# 2026- EDG rtemis.org

# Note: the estimators themselves, and the additivity checks that are this
# feature's most valuable test, live beside the algorithms they explain.
# Everything here is the shape both hierarchies promise.

# %% Fixtures ----
.shap_feats <- c("age", "bmi", "sex")

# One class's worth of contributions, with a baseline and the predictions they
# reconstruct: `predicted = rowSums(phi) + baseline` by construction, so any
# additivity assertion below is testing the object, not the fixture.
.shap_block <- function(n = 6L, seed = 2026L) {
  set.seed(seed)
  matrix(
    rnorm(n * length(.shap_feats)),
    nrow = n,
    dimnames = list(NULL, .shap_feats)
  )
}


# Construct without deriving anything, for the malformed inputs the validator
# has to reject. `predicted` is supplied plainly valid so that the branch under
# test is the one that fires: S7 checks a property's declared class before any
# validator sees the object, so a derived `predicted` would fail first and mask
# the message being asserted.
.make_shap_raw <- function(phi, baseline, predicted, ...) {
  # `baseline` is a matrix on the object -- one row per case, so an estimator
  # that routes each case to a different sub-model needs no special shape. The
  # fixtures name a value per class and it is expanded here.
  if (!is.matrix(baseline)) {
    baseline <- matrix(
      baseline,
      nrow = NROW(predicted),
      ncol = length(baseline),
      byrow = TRUE,
      dimnames = list(NULL, names(baseline))
    )
  }
  args <- list(
    algorithm = "LightGBM",
    config = setup_SHAP(),
    space = "input",
    feature_names = .shap_feats,
    phi = phi,
    baseline = baseline,
    predicted = predicted,
    scale = "margin",
    perturbation = "interventional",
    estimator = "TreeSHAP",
    exact = TRUE
  )
  do.call(SHAP, utils::modifyList(args, list(...)))
}


# Well-formed by construction: `predicted = rowSums(phi) + baseline`, so any
# additivity assertion below tests the object rather than the fixture.
.make_shap <- function(phi, baseline, ...) {
  predicted <- do.call(
    cbind,
    lapply(names(phi), function(nm) rowSums(phi[[nm]]) + baseline[[nm]])
  )
  colnames(predicted) <- names(phi)
  .make_shap_raw(phi = phi, baseline = baseline, predicted = predicted, ...)
}


.shap_regression <- function() {
  m <- .shap_block()
  .make_shap(list(medv = m), c(medv = 21.5))
}


.shap_multiclass <- function() {
  phi <- list(
    setosa = .shap_block(seed = 1L),
    versicolor = .shap_block(seed = 2L),
    virginica = .shap_block(seed = 3L)
  )
  .make_shap(phi, c(setosa = -1.1, versicolor = 0.3, virginica = 0.8))
}


# %% ExplanationConfig ----
test_that("ExplanationConfig is abstract; the kind is tagged on the subclass", {
  expect_error(ExplanationConfig())
  cfg <- setup_SHAP()
  expect_s7_class(cfg, ExplanationConfig)
  expect_identical(cfg@type, "SHAP")
})


test_that("the kind tag is not settable", {
  # `type` is a computed constant: a config cannot claim to be a kind it is not,
  # which is what lets a reader dispatch on the tag alone.
  expect_error(SHAPConfig(type = "ICE"))
})


# %% setup_SHAP() ----
test_that("setup_SHAP() defaults leave every resolved choice unset", {
  cfg <- setup_SHAP()
  expect_s7_class(cfg, SHAPConfig)
  expect_identical(cfg@estimator, "auto")
  # NULL is "resolve me", and each of these resolves from something the config
  # cannot see: the algorithm, and the outcome type.
  expect_null(cfg@perturbation)
  expect_null(cfg@scale)
  expect_null(cfg@approach)
  expect_null(cfg@n_coalitions)
})


test_that("setup_SHAP() rejects values outside the declared vocabularies", {
  expect_error(setup_SHAP(estimator = "treeshap"))
  expect_error(setup_SHAP(perturbation = "marginal"))
  expect_error(setup_SHAP(scale = "logit"))
  expect_error(setup_SHAP(approach = "independence"))
})


test_that("'independence' is not an approach, because it is a perturbation", {
  # shapr spells the interventional value function as an `approach`. Admitting
  # it here would give one decision two ways to be stated, and two configs that
  # disagree on `perturbation` could then mean the same thing.
  expect_false("independence" %in% SHAP_APPROACHES)
  expect_true("interventional" %in% SHAP_PERTURBATIONS)
})


# %% applies_when gates ----
test_that("kernel-only settings require the kernel estimator to be named", {
  # Not gated on "auto" resolving to kernel: a setting that a resolved exact
  # estimator would ignore has to be refused, not dropped.
  expect_error(setup_SHAP(n_coalitions = 64L), "estimator")
  expect_error(setup_SHAP(estimator = "exact", n_coalitions = 64L), "estimator")
  expect_s7_class(
    setup_SHAP(estimator = "kernel", n_coalitions = 64L),
    SHAPConfig
  )
})


test_that("approach requires both the kernel estimator and a conditional value function", {
  expect_error(setup_SHAP(approach = "ctree"), "estimator")
  expect_error(
    setup_SHAP(estimator = "kernel", approach = "ctree"),
    "perturbation"
  )
  expect_error(
    setup_SHAP(
      estimator = "kernel",
      perturbation = "interventional",
      approach = "ctree"
    ),
    "perturbation"
  )
  expect_s7_class(
    setup_SHAP(
      estimator = "kernel",
      perturbation = "conditional",
      approach = "ctree"
    ),
    SHAPConfig
  )
})


# %% Explanation / SHAP ----
test_that("Explanation is abstract; SHAP carries the kind tag", {
  expect_error(Explanation())
  x <- .shap_regression()
  expect_s7_class(x, Explanation)
  expect_identical(x@type, "SHAP")
})


test_that("regression, binary and multiclass all give one structure", {
  # The reason `phi` is a named list rather than a matrix or a 3-d array: a
  # consumer that handles multiclass handles the other two unchanged.
  reg <- .shap_regression()
  bin <- .make_shap(list(malignant = .shap_block()), c(malignant = 0.4))
  multi <- .shap_multiclass()
  for (x in list(reg, bin, multi)) {
    expect_true(is.list(x@phi))
    expect_true(all(vapply(x@phi, is.matrix, logical(1L))))
    expect_identical(colnames(x@baseline), names(x@phi))
    expect_identical(ncol(x@predicted), length(x@phi))
    # Per case, parallel to `predicted`, so nothing branches on whether an
    # estimator's baseline varies.
    expect_identical(dim(x@baseline), dim(x@predicted))
  }
  expect_length(reg@phi, 1L)
  expect_length(bin@phi, 1L)
  expect_length(multi@phi, 3L)
})


test_that("SHAP validates the shape its consumers rely on", {
  m <- .shap_block()
  one <- matrix(0, nrow = 6L, ncol = 1L)
  two <- matrix(0, nrow = 6L, ncol = 2L)
  expect_error(
    .make_shap_raw(list(), numeric(), matrix(numeric(0), nrow = 6L, ncol = 0L)),
    "at least one"
  )
  expect_error(.make_shap_raw(list(m), c(a = 0), one), "must be named")
  expect_error(
    .make_shap_raw(stats::setNames(list(m), ""), c(a = 0), one),
    "must be named"
  )
  expect_error(
    .make_shap_raw(list(a = as.data.frame(m)), c(a = 0), one),
    "matrices"
  )
  # A baseline that does not line up with `phi` makes every additivity check
  # silently wrong, so it is refused at construction.
  expect_error(
    .make_shap_raw(list(a = m, b = m), c(a = 0), two),
    "one column per entry"
  )
  expect_error(
    .make_shap_raw(list(a = m, b = m), c(a = 0, wrong = 0), two),
    "columns must match"
  )
})


test_that("SHAP refuses a feature_names / phi mismatch", {
  expect_error(
    .make_shap_raw(
      list(a = .shap_block()),
      c(a = 0),
      matrix(0, nrow = 6L, ncol = 1L),
      feature_names = c("age", "bmi")
    ),
    "columns"
  )
})


test_that("SHAP refuses ragged contribution matrices", {
  m <- .shap_block()
  expect_error(
    .make_shap_raw(
      list(a = m, b = m[, 1:2, drop = FALSE]),
      c(a = 0, b = 0),
      matrix(0, nrow = 6L, ncol = 2L)
    ),
    "same dimensions"
  )
})


test_that("SHAP refuses a predicted matrix that does not match phi", {
  m <- .shap_block()
  # One column per class and one row per case: without this, an additivity
  # check would compare against the wrong class's prediction.
  expect_error(
    .make_shap_raw(list(a = m), c(a = 0), matrix(0, nrow = 6L, ncol = 2L)),
    "one row per case"
  )
  expect_error(
    .make_shap_raw(list(a = m), c(a = 0), matrix(0, nrow = 3L, ncol = 1L)),
    "one row per case"
  )
})


test_that("the fixture is additive, which every estimator's test then asserts", {
  x <- .shap_multiclass()
  for (k in seq_along(x@phi)) {
    nm <- names(x@phi)[[k]]
    expect_equal(
      rowSums(x@phi[[nm]]) + x@baseline[, k],
      x@predicted[, k],
      tolerance = 1e-12
    )
  }
})


test_that("[[ indexes contributions by class", {
  x <- .shap_multiclass()
  expect_identical(x[["versicolor"]], x@phi[["versicolor"]])
  expect_identical(dim(x[["setosa"]]), c(6L, 3L))
})


# %% to_json ----
test_that("to_json publishes the explanation's identity, not its bulk", {
  x <- .shap_multiclass()
  j <- to_json(x)
  expect_identical(j[[".class"]], "SHAP")
  expect_identical(j[["type"]], "SHAP")
  expect_identical(j[["estimator"]], "TreeSHAP")
  expect_identical(j[["classes"]], names(x@phi))
  expect_identical(j[["n_cases"]], 6L)
  # The matrices are `n x p x k` bulk data and are deliberately absent, as the
  # prediction vectors are from `to_json(<Supervised>)`.
  expect_null(j[["phi"]])
  expect_null(j[["phi_encoded"]])
  expect_null(j[["predicted"]])
  # ...and the ranking summary a consumer would otherwise derive from them is
  # published instead.
  expect_named(j[["mean_abs_contribution"]], names(x@phi))
  expect_named(j[["mean_abs_contribution"]][["setosa"]], .shap_feats)
})


test_that("to_json carries the config and both fingerprints", {
  x <- .shap_multiclass()
  x@data_fingerprint <- data_fingerprint(iris)
  x@background_fingerprint <- data_fingerprint(iris[1:50, ])
  j <- to_json(x)
  expect_identical(j[["config"]][["type"]], "SHAP")
  # An attribution is relative to a background; two explanations against
  # different backgrounds are not comparable and nothing in the numbers says so.
  expect_false(
    identical(
      j[["data_fingerprint"]][["hash"]],
      j[["background_fingerprint"]][["hash"]]
    )
  )
})


# %% aggregate_one_hot() ----
.enc_phi <- function(cols, n = 3L, seed = 7L) {
  set.seed(seed)
  matrix(
    round(rnorm(n * length(cols)), 2),
    nrow = n,
    dimnames = list(NULL, cols)
  )
}


test_that("aggregating a one-hot expansion preserves the row sum exactly", {
  # The whole basis for reporting in the user's own columns: SHAP is additive,
  # so a factor's contribution is the sum of its encoded columns'. If this is
  # not exact, additivity does not survive the trip back.
  phi <- .enc_phi(c("age", "dx_A", "dx_B", "dx_C"))
  out <- aggregate_one_hot(phi, list(dx = c("A", "B", "C")))
  expect_identical(colnames(out), c("age", "dx"))
  expect_equal(rowSums(out), rowSums(phi), tolerance = 1e-12)
  expect_equal(out[, "dx"], rowSums(phi[, c("dx_A", "dx_B", "dx_C")]))
})


test_that("a group collapses where the factor was, keeping column order", {
  phi <- .enc_phi(c("dx_A", "dx_B", "age", "bmi"))
  out <- aggregate_one_hot(phi, list(dx = c("A", "B")))
  expect_identical(colnames(out), c("dx", "age", "bmi"))
})


test_that("a map entry with no matching column is skipped, not an error", {
  # `train()` learns the map on data that includes the outcome and applies it to
  # features alone, so entries with no column are expected.
  phi <- .enc_phi(c("age", "dx_A", "dx_B"))
  out <- aggregate_one_hot(
    phi,
    list(dx = c("A", "B"), y = c("yes", "no"))
  )
  expect_identical(colnames(out), c("age", "dx"))
})


test_that("no map leaves the matrix untouched", {
  phi <- .enc_phi(c("age", "bmi"))
  expect_identical(aggregate_one_hot(phi, NULL), phi)
  expect_identical(aggregate_one_hot(phi, list()), phi)
})


test_that("an ambiguous encoding is refused rather than double-counted", {
  # `x` with level "a_b" and `x_a` with level "b" both encode to `x_a_b`.
  # Attributing it to both would silently inflate each.
  phi <- .enc_phi("x_a_b")
  expect_error(
    aggregate_one_hot(phi, list(x = "a_b", x_a = "b")),
    "ambiguous"
  )
})


# %% shap_aggregate() ----
.agg_dat <- function(n = 60L, seed = 2026L) {
  set.seed(seed)
  data.frame(
    age = rnorm(n),
    bmi = rnorm(n),
    dx = factor(sample(c("A", "B", "C"), n, TRUE)),
    y = rnorm(n)
  )
}

.agg_feats <- c("age", "bmi", "dx")


test_that("an algorithm-internal one-hot is carried back to the user's columns", {
  # KNN builds `setup_Preprocessor(one_hot = TRUE)` at train time, so the
  # backend sees `dx_A`/`dx_B`/`dx_C` where the user wrote `dx`.
  dat <- .agg_dat()
  mod <- train(dat, hyperparameters = setup_KNN(), verbosity = 0L)
  encoded <- names(
    supervised_features(mod, dat[, .agg_feats], verbosity = 0L)
  )
  expect_true("dx_A" %in% encoded)
  phi <- .enc_phi(encoded)
  out <- shap_aggregate(phi, mod, .agg_feats)
  expect_identical(out[["space"]], "input")
  expect_identical(colnames(out[["phi"]]), .agg_feats)
  expect_equal(rowSums(out[["phi"]]), rowSums(phi), tolerance = 1e-12)
})


test_that("a user-level one-hot is carried back too, through both hops", {
  # The case `xnames` alone cannot express: the user's own preprocessor
  # expanded `dx` before `xnames` was recorded, so `xnames` is already encoded
  # and reporting in it would hand back three numbers for one factor.
  dat <- .agg_dat()
  mod <- train(
    dat,
    preprocessor_config = setup_Preprocessor(one_hot = TRUE),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  expect_true("dx_A" %in% mod@xnames)
  phi <- .enc_phi(mod@xnames)
  out <- shap_aggregate(phi, mod, .agg_feats)
  expect_identical(out[["space"]], "input")
  expect_identical(colnames(out[["phi"]]), .agg_feats)
  expect_equal(rowSums(out[["phi"]]), rowSums(phi), tolerance = 1e-12)
})


test_that("a decomposition stops the second hop at component space", {
  # Summing an expansion is exact; inverting a projection is a different
  # operation, and the manifold methods admit none at all.
  dat <- .agg_dat()
  mod <- train(
    dat,
    decomposition_config = setup_PCA(k = 2L),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  phi <- .enc_phi(mod@xnames)
  out <- shap_aggregate(phi, mod, .agg_feats)
  expect_identical(out[["space"]], "model")
  expect_true(any(grepl("^PC", colnames(out[["phi"]]))))
})


test_that("the input-space claim is verified, never assumed", {
  dat <- .agg_dat()
  mod <- train(dat, hyperparameters = setup_KNN(), verbosity = 0L)
  phi <- .enc_phi(names(
    supervised_features(mod, dat[, .agg_feats], verbosity = 0L)
  ))
  # Nothing to verify against: the hop is not taken.
  expect_identical(shap_aggregate(phi, mod, NULL)[["space"]], "model")
  # Verification fails: the hop is not taken, rather than claimed anyway.
  expect_identical(
    shap_aggregate(phi, mod, c(.agg_feats, "extra"))[["space"]],
    "model"
  )
})


# %% serializable_props ----
test_that("a SHAPConfig serializes its settings as siblings of the tag", {
  sp <- serializable_props(setup_SHAP(estimator = "kernel", n_coalitions = 64L))
  expect_identical(sp[["type"]], "SHAP")
  expect_identical(sp[["estimator"]], "kernel")
  expect_identical(sp[["n_coalitions"]], 64L)
  expect_false(is.list(sp[["config"]]))
})
