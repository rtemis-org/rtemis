# test_Hyperparameters.R
# ::rtemis::
# 2025- EDG rtemis.org

# Hyperparameters ----
# The superclass is abstract: only algorithm subclasses can be instantiated.
test_that("Hyperparameters superclass is abstract", {
  expect_error(Hyperparameters())
})

# A subclass with search values on a tunable hyperparameter needs tuning.
hpr <- setup_GLMNET(alpha = c(0, 1))
test_that("Hyperparameters succeeds", {
  expect_s7_class(hpr, Hyperparameters)
  # test that tuned is set correctly
  expect_identical(hpr@tuned, 0L)
})

# CARTHyperparameters ----
test_that("CARTHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(CARTHyperparameters(), CARTHyperparameters)
})

# get_hyperparams_need_tuning ----
test_that("get_hyperparams_need_tuning() succeeds", {
  expect_type(get_hyperparams_need_tuning(hpr), "list")
})

# Check printing of hp that need tuning ----
# CARTHyperparameters ----
# setup_CART ----
cart_hpr <- setup_CART(
  prune_cp = c(.001, .01, .1),
  minsplit = c(2L, 10L),
  minbucket = c(1L, 10L)
)
test_that("setup_CART() succeeds", {
  expect_s7_class(cart_hpr, CARTHyperparameters)
})

# needs_tuning ----
test_that("needs_tuning() succeeds", {
  expect_type(needs_tuning(cart_hpr), "logical")
})

# GLMNETHyperparameters ----
test_that("GLMNETHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(GLMNETHyperparameters(), GLMNETHyperparameters)
})

# setup_GLMNET ----
test_that("setup_GLMNET() succeeds", {
  expect_s7_class(setup_GLMNET(), GLMNETHyperparameters)
})

# LightCARTHyperparameters ----
test_that("LightCARTHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(LightCARTHyperparameters(), LightCARTHyperparameters)
})

# setup_LightCART ----
test_that("setup_LightCART() succeeds", {
  expect_s7_class(setup_LightCART(), LightCARTHyperparameters)
})

# LightRFHyperparameters ----
test_that("LightRFHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(LightRFHyperparameters(), LightRFHyperparameters)
})

# setup_LightRF ----
test_that("setup_LightRF() succeeds", {
  lrf_hpr <- setup_LightRF()
  lrf_hpr
  expect_s7_class(lrf_hpr, LightRFHyperparameters)
})

# %% Migrated classes: shared invariants ----
# Every algorithm class declares its hyperparameters with the prop_* factories.
.hp_classes <- list(
  GLM = GLMHyperparameters,
  GAM = GAMHyperparameters,
  CART = CARTHyperparameters,
  GLMNET = GLMNETHyperparameters,
  LightCART = LightCARTHyperparameters,
  LightRF = LightRFHyperparameters,
  LightGBM = LightGBMHyperparameters,
  LightRuleFit = LightRuleFitHyperparameters,
  Isotonic = IsotonicHyperparameters,
  LinearSVM = LinearSVMHyperparameters,
  RadialSVM = RadialSVMHyperparameters,
  TabNet = TabNetHyperparameters,
  Ranger = RangerHyperparameters
)

test_that("setup_* defaults do not drift from the property defaults", {
  # setup_* formals and the property specs both declare defaults (the former
  # for CRAN docs). This makes divergence impossible: constructing via
  # setup_*() must equal constructing the class with no arguments.
  for (nm in names(.hp_classes)) {
    setup_defaults <- get(paste0("setup_", nm))()@hyperparameters
    class_defaults <- .hp_classes[[nm]]()@hyperparameters
    expect_identical(setup_defaults, class_defaults, info = nm)
  }
})

test_that("every algorithm class exposes spec-derived views", {
  for (nm in names(.hp_classes)) {
    cls <- .hp_classes[[nm]]
    h <- get(paste0("setup_", nm))()
    expect_s7_class(h, Hyperparameters)
    expect_identical(h@algorithm, nm, info = nm)
    # tunable/fixed are derived from the specs, never hand-maintained.
    expect_identical(
      h@tunable_hyperparameters,
      tunable_spec_names(cls),
      info = nm
    )
    # Three disjoint categories that together account for every
    # hyperparameter: tunable (a vector is a search space), fixed (settable,
    # not tunable), constant (not settable at all).
    expect_length(
      intersect(h@tunable_hyperparameters, h@fixed_hyperparameters),
      0L
    )
    expect_length(
      intersect(h@fixed_hyperparameters, h@constant_hyperparameters),
      0L
    )
    expect_length(
      intersect(h@tunable_hyperparameters, h@constant_hyperparameters),
      0L
    )
    expect_setequal(
      c(
        h@tunable_hyperparameters,
        h@fixed_hyperparameters,
        h@constant_hyperparameters
      ),
      names(h@hyperparameters)
    )
  }
})

test_that("algorithm is a computed constant and cannot be set", {
  h <- setup_LightRF()
  expect_identical(h@algorithm, "LightRF")
  expect_error(h@algorithm <- "GLM")
})

test_that("unknown hyperparameters are rejected", {
  h <- setup_LightRF()
  expect_error(h@hyperparameters[["bogus"]] <- 1L, "Unknown")
  expect_error(update(h, list(bogus = 1L)), "Unknown")
})

# %% LightRF ----
test_that("LightRF back-compat access patterns work", {
  h <- setup_LightRF()
  # Individual props are validated and typed.
  expect_identical(h@nrounds, 500L)
  # `$` and `[[` route through the computed hyperparameters list.
  expect_identical(h$num_leaves, 4096L)
  expect_identical(h[["subsample"]], 0.623)
  # The computed list includes the unsettable RF constants.
  expect_identical(h@hyperparameters[["boosting_type"]], "rf")
  # Unset nullable prop reads as NULL from the list (zero-length -> NULL).
  expect_null(h@hyperparameters[["objective"]])
})

test_that("LightRF spec validators enforce bounds, enum, arity", {
  expect_error(setup_LightRF(nrounds = 0L))
  expect_error(setup_LightRF(feature_fraction = 0)) # exclusive min
  expect_error(setup_LightRF(feature_fraction = 1.1))
  expect_no_error(setup_LightRF(feature_fraction = 1)) # inclusive max
  expect_error(setup_LightRF(device_type = "tpu"))
  expect_no_error(setup_LightRF(device_type = "gpu"))
  # Tunable props accept search vectors; every value is checked.
  expect_no_error(setup_LightRF(num_leaves = c(1024L, 4096L)))
  expect_error(setup_LightRF(num_leaves = c(1024L, 0L)))
  # Fixed props reject vectors.
  expect_error(setup_LightRF(device_type = c("cpu", "gpu")))
})

test_that("LightRF tuned status derives from search values", {
  expect_identical(setup_LightRF()@tuned, -1L)
  expect_identical(setup_LightRF(nrounds = c(500L, 1000L))@tuned, 0L)
  # clean_posint upgrades doubles, including search vectors.
  expect_identical(
    setup_LightRF(nrounds = c(500, 1000))$nrounds,
    c(500L, 1000L)
  )
})

test_that("LightRF hyperparameters setter writes through to props", {
  h <- setup_LightRF()
  # Assignment into the list routes to the property (as train_LightRF does).
  h@hyperparameters[["objective"]] <- "binary"
  expect_identical(h@objective, "binary")
  # Routed assignments are validated.
  expect_error(h@hyperparameters[["nrounds"]] <- -5L)
  # update() (the Tuner path) works and locks the tuned status.
  h2 <- update(
    setup_LightRF(nrounds = c(500L, 1000L)),
    list(nrounds = 750L),
    tuned = 1L
  )
  expect_identical(h2$nrounds, 750L)
  expect_identical(h2@tuned, 1L)
  expect_true(is_tuned(h2))
})

test_that("LightRF constants cannot be changed", {
  h <- setup_LightRF()
  expect_error(
    h@hyperparameters[["boosting_type"]] <- "gbdt",
    "constant"
  )
  expect_error(h@hyperparameters[["subsample_freq"]] <- 2L)
  # Round-tripping the unchanged list (constants included) is fine.
  expect_no_error(h@hyperparameters <- h@hyperparameters)
  # Constants are NOT "fixed": fixed means settable but not tunable, which a
  # constant is not. They are their own category.
  expect_setequal(
    h@constant_hyperparameters,
    c(
      "boosting_type",
      "learning_rate",
      "subsample_freq",
      "early_stopping_rounds"
    )
  )
  expect_false(any(h@constant_hyperparameters %in% h@fixed_hyperparameters))
})

test_that("LightRFHyperparameters generates its JSON Schema", {
  schema <- S7_to_JSONSchema(
    LightRFHyperparameters,
    id = "https://schema.rtemis.org/hyperparameters/lightrf/v1/schema.json",
    base = Hyperparameters
  )
  expect_identical(
    sort(names(schema[["properties"]])),
    sort(spec_prop_names(LightRFHyperparameters))
  )
  expect_false("default" %in% names(schema[["properties"]][["nrounds"]]))
  # Tunable prop -> oneOf [scalar, array of search values].
  expect_length(schema[["properties"]][["linear_tree"]][["oneOf"]], 2L)
})

# %% tune_on_null ----
test_that("tune-on-null hyperparameters drive the tuned status", {
  # GLMNET: NULL lambda means "determine by cv.glmnet" -> needs tuning.
  h <- setup_GLMNET()
  expect_identical(h@tuned, 0L)
  expect_true("lambda" %in% names(get_hyperparams_need_tuning(h)))
  expect_identical(setup_GLMNET(lambda = 0.1)@tuned, -1L)
  # LightGBM: NULL nrounds means "determine by early stopping".
  expect_identical(setup_LightGBM()@tuned, 0L)
  expect_identical(setup_LightGBM(force_nrounds = 100L)@tuned, -1L)
  expect_identical(setup_LightGBM(force_nrounds = 100L)$nrounds, 100L)
})

test_that("update() converts the grid NULL sentinel back to NULL", {
  # expand_grid() encodes NULL search entries as the string "null".
  h <- update(setup_GLMNET(), list(lambda = "null"))
  expect_null(h$lambda)
  h <- update(setup_LightGBM(), list(nrounds = "null"))
  expect_null(h$nrounds)
})

# %% Constants ----
test_that("SVM kernels are constants, not settable hyperparameters", {
  expect_identical(setup_LinearSVM()[["kernel"]], "linear")
  expect_identical(setup_RadialSVM()[["kernel"]], "radial")
  h <- setup_LinearSVM()
  expect_error(h@hyperparameters[["kernel"]] <- "radial", "constant")
  expect_false("kernel" %in% h@tunable_hyperparameters)
})

# %% Cross-field validation ----
test_that("LightRuleFit rejects ifw combined with per-step ifw", {
  expect_error(setup_LightRuleFit(ifw = TRUE, ifw_lightgbm = TRUE))
  expect_error(setup_LightRuleFit(ifw = TRUE, ifw_glmnet = TRUE))
  expect_no_error(setup_LightRuleFit(ifw = TRUE))
  expect_no_error(setup_LightRuleFit(ifw_lightgbm = TRUE, ifw_glmnet = TRUE))
})

# %% Vector-valued hyperparameters ----
test_that("vector-valued hyperparameters accept vectors", {
  # penalty_factor is one value per feature, not a set of search values.
  h <- setup_GLMNET(penalty_factor = c(1, 1, 0.5))
  expect_length(h$penalty_factor, 3L)
  expect_false("penalty_factor" %in% h@tunable_hyperparameters)
  # Element-wise constraints still apply.
  expect_error(setup_GLMNET(penalty_factor = c(1, -1)))
  # CART cost is per-feature.
  expect_length(setup_CART(cost = c(1, 2))$cost, 2L)
})

# LightGBMHyperparameters ----
test_that("LightGBMHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(LightGBMHyperparameters(), LightGBMHyperparameters)
})

# setup_LightGBM ----
test_that("setup_LightGBM() succeeds", {
  lgbm_hpr <- setup_LightGBM(
    num_leaves = c(4, 8, 16),
    learning_rate = c(.001, .01, .1)
  )
  expect_s7_class(setup_LightGBM(), LightGBMHyperparameters)
})

# LightRuleFitHyperparameters ----
test_that("LightRuleFitHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(LightRuleFitHyperparameters(), LightRuleFitHyperparameters)
})

# setup_LightRuleFit ----
test_that("setup_LightRuleFit() succeeds", {
  expect_s7_class(setup_LightRuleFit(), LightRuleFitHyperparameters)
})

# IsotonicHyperparameters ----
test_that("IsotonicHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(IsotonicHyperparameters(), IsotonicHyperparameters)
})

# setup_Isotonic ----
test_that("setup_Isotonic() succeeds", {
  expect_s7_class(setup_Isotonic(), IsotonicHyperparameters)
})

# RadialSVMHyperparameters ----
test_that("RadialSVMHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(RadialSVMHyperparameters(), RadialSVMHyperparameters)
})

# setup_LinearSVM ----
test_that("setup_LinearSVM() succeeds", {
  expect_s7_class(setup_LinearSVM(), LinearSVMHyperparameters)
})

# setup_RadialSVM ----
test_that("setup_RadialSVM() succeeds", {
  expect_s7_class(setup_RadialSVM(), RadialSVMHyperparameters)
})

# TabNetHyperparameters ----
test_that("TabNetHyperparameters() constructs from its property defaults", {
  # Defaults come from the PropertySpecs, so no argument is required.
  expect_s7_class(TabNetHyperparameters(), TabNetHyperparameters)
})

# setup_TabNet ----
test_that("setup_TabNet() succeeds", {
  expect_s7_class(setup_TabNet(), TabNetHyperparameters)
})

# setup_Ranger ----
test_that("setup_Ranger() succeeds", {
  expect_s7_class(setup_Ranger(), RangerHyperparameters)
})


# %% Property roles ----------------------------------------------------------
# Run state is written onto the object during tuning but is not configuration:
# it must stay out of both the generated schema and the serialized config, and
# be re-derived on read. Regression guard for the boundary that `prop_state()`
# declares.

test_that("run state stays out of the serialized config", {
  h <- setup_LightGBM()
  expect_identical(
    role_prop_names(LightGBMHyperparameters, "state"),
    c("nrounds", "best_iter")
  )
  # The Tuner writes both.
  h@nrounds <- 137L
  h@best_iter <- 120
  serialized <- serializable_props(h)[["hyperparameters"]]
  expect_false(any(c("nrounds", "best_iter") %in% names(serialized)))
  # ...but the training backend still reads them off the runtime list.
  expect_identical(h@hyperparameters[["nrounds"]], 137L)

  g <- setup_GLMNET()
  g@`lambda.min` <- 0.01
  expect_false(
    "lambda.min" %in% names(serializable_props(g)[["hyperparameters"]])
  )
})


test_that("every Ranger property is generated from its own declaration", {
  # No property needs a hand-written schema fragment.
  expect_length(role_prop_names(RangerHyperparameters, "external"), 0L)
  schema <- S7_to_JSONSchema(
    RangerHyperparameters,
    id = "https://schema.rtemis.org/hyperparameters/ranger/v1/schema.json",
    base = Hyperparameters
  )
  props <- schema[["properties"]]
  # `respect_unordered_factors` is a plain enum.
  expect_identical(
    as.character(props[["respect_unordered_factors"]][["enum"]]),
    c("partition", "ignore", "order")
  )
  # `inbag`: one per-case count vector per tree.
  expect_identical(props[["inbag"]][["items"]][["type"]], "array")
  # `split_select_weights` broadcasts: one vector for all trees, or one per tree.
  branches <- props[["split_select_weights"]][["oneOf"]]
  expect_length(branches, 3L)
  expect_identical(branches[[2L]][["items"]][["type"]], "number")
  expect_identical(branches[[3L]][["items"]][["type"]], "array")
})


test_that("run state is in the schema as readOnly, but never serialized", {
  # Two independent axes. A reader needs the field to reconstruct the class,
  # and a run record carries its value; a portable config must not, because it
  # is re-derived on read.
  schema <- S7_to_JSONSchema(
    GLMNETHyperparameters,
    id = "https://schema.rtemis.org/hyperparameters/glmnet/v1/schema.json",
    base = Hyperparameters
  )
  for (nm in c("lambda.min", "lambda.1se")) {
    prop_schema <- schema[["properties"]][[nm]]
    expect_false(is.null(prop_schema), info = nm)
    expect_true(prop_schema[["readOnly"]], info = nm)
    expect_identical(prop_schema[["x-rtemis"]][["role"]], "state", info = nm)
    # Declared with a real type, not an opaque blob.
    expect_true("number" %in% as.character(prop_schema[["type"]]), info = nm)
  }
  written <- serializable_props(setup_GLMNET())[["hyperparameters"]]
  expect_false(any(c("lambda.min", "lambda.1se") %in% names(written)))
})


test_that("prop_state requires a factory-built property", {
  # Run state is declared with the same type and bounds as configuration, so
  # its schema is generated rather than hand-written.
  expect_error(prop_state(S7::class_double), class = "rtemis_type_error")
  expect_no_error(prop_state(prop_float(NULL, nullable = TRUE)))
})
