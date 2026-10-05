# test_ResolvedHyperparameters.R
# ::rtemis::
# 2026- EDG rtemis.org

# A fitted model records the value its backend used for an eligible unset
# hyperparameter, read from the fitted object or from the installed backend's
# resolution rule. A hyperparameter whose unset value means all, none or a
# count that follows the data stays unset.

.bin <- iris[51:150, ]
.bin[["Species"]] <- droplevels(.bin[["Species"]])
.hp <- function(model, name) model@hyperparameters@hyperparameters[[name]]


test_that("values are recorded only where unset and in effect", {
  hp <- setup_LightGBM(force_nrounds = 10L)
  recorded <- record_backend_values(
    hp,
    list(
      # Unset and in effect: recorded.
      sigmoid = 1,
      # Gated off under gbdt boosting: not recorded.
      drop_rate = 0.1,
      # Set by the user: kept.
      num_leaves = 99L,
      # Not a hyperparameter of the class: ignored.
      not_a_hyperparameter = 1
    )
  )
  expect_identical(recorded[["sigmoid"]], 1)
  expect_null(recorded[["drop_rate"]])
  expect_identical(recorded[["num_leaves"]], hp[["num_leaves"]])
  # Run state is observed by the run, not recorded as a backend choice.
  expect_null(
    record_backend_values(setup_LINAD(), list(best_n_leaves = 2L))[[
      "best_n_leaves"
    ]]
  )
})

test_that("ranger records mtry, node size, split rule and factor handling", {
  skip_if_not_installed("ranger")
  set.seed(1)
  model <- train(
    .bin,
    hyperparameters = setup_Ranger(num_trees = 20L),
    verbosity = 0L
  )
  expect_identical(.hp(model, "mtry"), as.integer(model@model[["mtry"]]))
  expect_identical(
    .hp(model, "min_node_size"),
    as.integer(model@model[["min.node.size"]])
  )
  expect_identical(.hp(model, "splitrule"), model@model[["splitrule"]])
  expect_identical(.hp(model, "respect_unordered_factors"), "ignore")
  expect_identical(.hp(model, "min_bucket"), 1L)
  # A value the user set is what the fit used.
  set_by_user <- train(
    .bin,
    hyperparameters = setup_Ranger(num_trees = 20L, mtry = 3L),
    verbosity = 0L
  )
  expect_identical(.hp(set_by_user, "mtry"), 3L)
  expect_identical(as.integer(set_by_user@model[["mtry"]]), 3L)
})

test_that("MARS records earth's penalty and term limit, and no pruning bound", {
  skip_if_not_installed("earth")
  model <- train(mtcars, hyperparameters = setup_MARS(), verbosity = 0L)
  expect_identical(.hp(model, "penalty"), as.numeric(model@model[["penalty"]]))
  expect_identical(.hp(model, "nk"), as.integer(model@model[["nk"]]))
  # Unset means no limit; the number of forward-pass terms belongs to the
  # sample.
  expect_null(.hp(model, "nprune"))
})

test_that("LightGBM records the parameters its model ran with, where its objective uses them", {
  skip_if_not_installed("lightgbm")
  regression <- train(
    mtcars,
    hyperparameters = setup_LightGBM(force_nrounds = 10L),
    verbosity = 0L
  )
  parameters <- lightgbm_model_parameters(regression@model)
  expect_identical(
    .hp(regression, "boost_from_average"),
    as.integer(parameters[["boost_from_average"]]) != 0L
  )
  expect_identical(.hp(regression, "reg_sqrt"), FALSE)
  # The sigmoid is a classification setting; regression does not use it.
  expect_null(.hp(regression, "sigmoid"))
  # DART settings have no effect under gbdt and stay unset.
  expect_null(.hp(regression, "drop_rate"))
  binary <- train(
    .bin,
    hyperparameters = setup_LightGBM(force_nrounds = 10L),
    verbosity = 0L
  )
  expect_identical(.hp(binary, "sigmoid"), 1)
  expect_null(.hp(binary, "reg_sqrt"))
  multiclass <- train(
    iris,
    hyperparameters = setup_LightGBM(force_nrounds = 10L),
    verbosity = 0L
  )
  expect_identical(.hp(multiclass, "boost_from_average"), TRUE)
  expect_null(.hp(multiclass, "sigmoid"))
  expect_null(.hp(multiclass, "reg_sqrt"))
  huber <- train(
    mtcars,
    hyperparameters = setup_LightGBM(force_nrounds = 10L, objective = "huber"),
    verbosity = 0L
  )
  expect_null(.hp(huber, "reg_sqrt"))
  expect_identical(.hp(huber, "boost_from_average"), TRUE)
  # Run state filled from `force_nrounds` is reported as derived.
  fold <- record(regression)[["folds"]][[1L]][["hyperparameters"]]
  expect_identical(fold[["origin"]][["nrounds"]], "derived")
})

test_that("each objective-specific LightGBM parameter changes the fit exactly under its listed objectives", {
  skip_if_not_installed("lightgbm")
  skip_on_cran()
  # A differential check of LIGHTGBM_OBJECTIVE_PARAMETERS against the
  # installed LightGBM: a parameter is in effect under an objective when
  # changing it changes the predictions. The L2 penalty keeps the sigmoid
  # slope from cancelling out of the Newton steps.
  set.seed(10)
  n <- 300L
  x <- matrix(stats::rnorm(n * 3L), n)
  y_positive <- exp(0.5 * x[, 1L] + stats::rnorm(n, sd = 0.3)) + 0.5
  y_class <- cut(
    x[, 1L] + stats::rnorm(n),
    c(-Inf, -1, 1.2, Inf),
    labels = FALSE
  ) -
    1L
  y_probability <- stats::plogis(x[, 1L] + stats::rnorm(n))
  outcome <- function(objective) {
    switch(
      objective,
      binary = as.integer(y_class == 2L),
      multiclass = ,
      multiclassova = y_class,
      cross_entropy = ,
      cross_entropy_lambda = y_probability,
      y_positive
    )
  }
  changed <- list(sigmoid = 2, reg_sqrt = TRUE, boost_from_average = FALSE)
  fit <- function(objective, extra = list()) {
    params <- c(
      list(
        objective = objective,
        verbose = -1L,
        num_threads = 1L,
        deterministic = TRUE,
        num_leaves = 4L,
        lambda_l2 = 5
      ),
      if (objective %in% c("multiclass", "multiclassova")) {
        list(num_class = 3L)
      },
      extra
    )
    model <- lightgbm::lgb.train(
      params,
      lightgbm::lgb.Dataset(x, label = outcome(objective)),
      nrounds = 5L,
      verbose = -1L
    )
    as.numeric(stats::predict(model, x))
  }
  for (objective in LIGHTGBM_OBJECTIVES) {
    reference <- fit(objective)
    for (nm in names(LIGHTGBM_OBJECTIVE_PARAMETERS)) {
      difference <- max(abs(
        fit(objective, changed[nm]) - reference
      ))
      expect_identical(
        difference > 1e-8,
        objective %in% LIGHTGBM_OBJECTIVE_PARAMETERS[[nm]],
        info = paste(nm, "under", objective)
      )
    }
  }
})

test_that("HAL records the knots its generator placed and its basis threshold", {
  skip_if_not_installed("hal9001")
  dat <- mtcars[, c("cyl", "disp", "hp", "mpg")]
  model <- train(dat, hyperparameters = setup_HAL(), verbosity = 0L)
  expected <- as.integer(hal9001:::num_knots_generator(
    max_degree = .hp(model, "max_degree"),
    smoothness_orders = .hp(model, "smoothness_orders"),
    base_num_knots_0 = 200,
    base_num_knots_1 = 50
  ))
  expect_identical(.hp(model, "num_knots"), expected)
  # The basis threshold applies to zero-order bases only.
  expect_null(.hp(model, "reduce_basis"))
  zero <- train(
    dat,
    hyperparameters = setup_HAL(smoothness_orders = 0L),
    verbosity = 0L
  )
  expect_identical(.hp(zero, "reduce_basis"), zero@model[["reduce_basis"]])
})

test_that("MonotonicHAL records its basis threshold at order 0 only", {
  skip_if_not_installed("hal9001")
  set.seed(11)
  dat <- data.frame(x = stats::rnorm(120L))
  dat[["y"]] <- dat[["x"]] + stats::rnorm(120L)
  zero <- train(
    dat,
    hyperparameters = setup_MonotonicHAL(smoothness_orders = 0L, seed = 1L),
    verbosity = 0L
  )
  expect_identical(.hp(zero, "reduce_basis"), zero@model[["reduce_basis"]])
  expect_false(is.null(.hp(zero, "reduce_basis")))
  first <- train(
    dat,
    hyperparameters = setup_MonotonicHAL(smoothness_orders = 1L, seed = 1L),
    verbosity = 0L
  )
  expect_null(.hp(first, "reduce_basis"))
})

test_that("BART leaves the feature subsample unset, meaning every covariate", {
  skip_if_not_installed("stochtree")
  model <- train(
    mtcars,
    hyperparameters = setup_BART(num_gfr = 2L, num_mcmc = 5L),
    verbosity = 0L
  )
  expect_null(.hp(model, "num_features_subsample"))
})

test_that("a recorded backend value is reported with a resolved origin", {
  skip_if_not_installed("ranger")
  set.seed(2)
  rec <- record(train(
    .bin,
    hyperparameters = setup_Ranger(num_trees = 20L),
    verbosity = 0L
  ))
  # The fit's hyperparameters, in the fold record, state the value and that
  # the run determined it.
  fitted <- rec[["folds"]][[1L]][["hyperparameters"]]
  expect_identical(fitted[["mtry"]], 2L)
  expect_identical(fitted[["origin"]][["mtry"]], "derived")
})


test_that("MLP records its loss, generated shape and optimizer settings", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  set.seed(4)
  model <- train(
    mtcars,
    hyperparameters = setup_MLP(max_epochs = 2L, optimizer = "adamw"),
    verbosity = 0L
  )
  expect_identical(.hp(model, "loss"), "mse")
  # `loss` is declared default_on_null, so the record reports a default.
  fold <- record(model)[["folds"]][[1L]][["hyperparameters"]]
  expect_identical(fold[["origin"]][["loss"]], "default")
  expect_identical(.hp(model, "shape"), "funnel")
  expect_identical(.hp(model, "shape_layers"), 3L)
  expect_identical(.hp(model, "beta1"), 0.9)
  expect_identical(.hp(model, "beta2"), 0.999)
  expect_identical(
    .hp(model, "eps"),
    eval(formals(torch::optim_adamw)[["eps"]])
  )
  # Momentum has no effect under AdamW and stays unset.
  expect_null(.hp(model, "momentum"))
})

test_that("LINAD records the engine settings it resolved, leaving gated ones unset", {
  set.seed(5)
  model <- train(mtcars, hyperparameters = setup_LINAD(), verbosity = 0L)
  settings <- linad_settings(setup_LINAD())
  expect_identical(.hp(model, "split_criterion"), settings[["split_criterion"]])
  expect_identical(
    .hp(model, "min_cases_node_model"),
    settings[["min_cases_node_model"]]
  )
  ridge <- train(
    mtcars,
    hyperparameters = setup_LINAD(node_model = "ridge"),
    verbosity = 0L
  )
  expect_null(.hp(ridge, "nvmax"))
  # Root settings follow the root model, and smoothing needs validation cases.
  constant <- train(
    mtcars,
    hyperparameters = setup_LINAD(node_model = "constant"),
    verbosity = 0L
  )
  expect_null(.hp(constant, "root_nvmax"))
  expect_null(.hp(constant, "root_lambda"))
  expect_null(.hp(constant, "root_alpha"))
  expect_null(.hp(model, "root_alpha"))
  expect_identical(.hp(model, "root_nvmax"), settings[["root_nvmax"]])
  expect_null(.hp(model, "smooth_validation_curve"))
  # A penalized root under constant nodes uses the penalty and not the mixing.
  ridge_root <- train(
    mtcars,
    hyperparameters = setup_LINAD(
      node_model = "constant",
      root_model = "ridge"
    ),
    verbosity = 0L
  )
  expect_identical(.hp(ridge_root, "root_lambda"), settings[["root_lambda"]])
  expect_null(.hp(ridge_root, "root_alpha"))
  expect_null(.hp(ridge_root, "root_nvmax"))
  # A forward root under ridge nodes uses its own stopping rule.
  forward_root <- train(
    mtcars,
    hyperparameters = setup_LINAD(node_model = "ridge", root_model = "forward"),
    verbosity = 0L
  )
  expect_identical(.hp(forward_root, "root_forward_stop"), "bic")
  expect_null(.hp(forward_root, "forward_stop"))
  expect_null(.hp(forward_root, "root_node_test"))
  expect_identical(.hp(forward_root, "node_test"), "none")
  # A ridge root inherits the nodes' slopes test.
  tested <- train(
    mtcars,
    hyperparameters = setup_LINAD(node_model = "ridge", node_test = "bic"),
    verbosity = 0L
  )
  expect_identical(.hp(tested, "root_node_test"), "bic")
  expect_null(.hp(tested, "root_forward_stop"))
  # A root learning rate of 0 fits no root model.
  no_root <- train(
    mtcars,
    hyperparameters = setup_LINAD(root_learning_rate = 0),
    verbosity = 0L
  )
  expect_null(.hp(no_root, "root_model"))
  expect_null(.hp(no_root, "root_nvmax"))
  expect_null(.hp(no_root, "root_lambda"))
  # Leaves selected on validation cases record the smoothing setting.
  validated <- train(
    mtcars[1:24, ],
    dat_validation = mtcars[25:32, ],
    hyperparameters = setup_LINAD(),
    verbosity = 0L
  )
  expect_false(is.null(validated@model@leaf_curve))
  expect_identical(.hp(validated, "smooth_validation_curve"), FALSE)
  # A validation set with a single leaf to keep selects nothing.
  one_leaf <- train(
    mtcars[1:24, ],
    dat_validation = mtcars[25:32, ],
    hyperparameters = setup_LINAD(max_leaves = 1L),
    verbosity = 0L
  )
  expect_null(one_leaf@model@leaf_curve)
  expect_null(.hp(one_leaf, "smooth_validation_curve"))
})

test_that("LINAD's root stopping rule and slopes test reach the root fit", {
  # An outcome unrelated to the features: a cost per term stops forward
  # selection early, and the slopes test drops the root's slopes.
  set.seed(13)
  dat <- as.data.frame(matrix(stats::rnorm(600L), 100L))
  dat[["y"]] <- stats::rnorm(100L)
  fit <- function(...) {
    model <- train(
      dat,
      hyperparameters = setup_LINAD(force_max_leaves = TRUE, ...),
      verbosity = 0L
    )
    model@predicted_training
  }
  # Unset inherits the node-level value.
  expect_identical(
    fit(forward_stop = "aic"),
    fit(forward_stop = "aic", root_forward_stop = "aic")
  )
  expect_identical(
    fit(node_model = "ridge", node_test = "bic"),
    fit(node_model = "ridge", node_test = "bic", root_node_test = "bic")
  )
  # The root's own value changes the fit.
  expect_false(identical(
    fit(
      node_model = "ridge",
      root_model = "forward",
      root_forward_stop = "none"
    ),
    fit(node_model = "ridge", root_model = "forward", root_forward_stop = "bic")
  ))
  expect_false(identical(
    fit(node_model = "constant", root_model = "ridge", root_node_test = "none"),
    fit(node_model = "constant", root_model = "ridge", root_node_test = "bic")
  ))
  # The forest's trees fit their roots the same way.
  forest <- function(...) {
    train(
      dat,
      hyperparameters = setup_LINADForest(
        n_trees = 2L,
        force_max_leaves = TRUE,
        ...
      ),
      execution_config = setup_SerialExecution(seed = 1L),
      verbosity = 0L
    )@predicted_training
  }
  expect_false(identical(
    forest(
      node_model = "ridge",
      root_model = "forward",
      root_forward_stop = "none"
    ),
    forest(
      node_model = "ridge",
      root_model = "forward",
      root_forward_stop = "bic"
    )
  ))
  expect_false(identical(
    forest(
      node_model = "constant",
      root_model = "ridge",
      root_node_test = "none"
    ),
    forest(
      node_model = "constant",
      root_model = "ridge",
      root_node_test = "bic"
    )
  ))
})

test_that("the root applies the slopes test only to a ridge or elastic-net root", {
  # The root is the first `linad_solve()` call of a fit.
  root_node_test <- function(hyperparameters) {
    calls <- character()
    solve <- linad_solve
    local_mocked_bindings(
      linad_solve = function(...) {
        calls[[length(calls) + 1L]] <<- list(...)[["node_test"]]
        solve(...)
      }
    )
    train(
      mtcars,
      hyperparameters = hyperparameters,
      verbosity = 0L
    )
    calls[[1L]]
  }
  expect_identical(
    root_node_test(setup_LINAD(
      max_leaves = 2L,
      node_model = "ridge",
      node_test = "bic",
      root_model = "forward"
    )),
    "none"
  )
  expect_identical(
    root_node_test(setup_LINAD(
      max_leaves = 2L,
      node_model = "ridge",
      root_model = "forward",
      root_node_test = "bic"
    )),
    "none"
  )
  expect_identical(
    root_node_test(setup_LINAD(
      max_leaves = 2L,
      node_model = "ridge",
      node_test = "bic"
    )),
    "bic"
  )
})

test_that("positional setup calls bind the arguments that precede the root controls", {
  linad <- setup_LINAD(
    20L,
    2L,
    1L,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    0.1,
    NULL,
    NULL,
    NULL,
    0.5
  )
  expect_identical(linad[["root_learning_rate"]], 0.5)
  expect_identical(
    utils::tail(names(formals(setup_LINAD)), 2L),
    c("root_forward_stop", "root_node_test")
  )
  expect_identical(
    utils::tail(names(formals(setup_LINADForest)), 2L),
    c("root_forward_stop", "root_node_test")
  )
})

test_that("LINADForest keeps its all-feature samples unset and records smoothing only where trees select", {
  set.seed(12)
  model <- train(
    mtcars,
    hyperparameters = setup_LINADForest(n_trees = 3L, max_leaves = 4L),
    verbosity = 0L
  )
  # Unset means every feature, on any design the recipe is reused on.
  expect_null(.hp(model, "mtry_tree"))
  expect_null(.hp(model, "mtry_split"))
  selected <- any(vapply(
    model@model@trees,
    function(tree) !is.null(tree@leaf_curve),
    logical(1L)
  ))
  expect_identical(
    is.null(.hp(model, "smooth_validation_curve")),
    !selected
  )
  # Too few out-of-bag cases for any tree to select its size.
  small <- train(
    mtcars[1:8, ],
    hyperparameters = setup_LINADForest(n_trees = 2L, max_leaves = 4L),
    verbosity = 0L
  )
  expect_null(.hp(small, "smooth_validation_curve"))
})

test_that("GLMTree records partykit's minimum node size", {
  skip_if_not_installed("partykit")
  set.seed(6)
  dat <- data.frame(x1 = rnorm(120), x2 = rnorm(120))
  dat[["y"]] <- dat[["x1"]] + rnorm(120)
  model <- train(dat, hyperparameters = setup_GLMTree(), verbosity = 0L)
  expect_identical(
    .hp(model, "minsize"),
    as.integer(10L * length(stats::coef(model@model, node = 1L)))
  )
})

test_that("TabNet records the widths of the fitted network", {
  skip_if_not_installed("tabnet")
  skip_if_not(torch::torch_is_installed())
  set.seed(7)
  model <- train(
    mtcars,
    hyperparameters = setup_TabNet(epochs = 2L),
    verbosity = 0L
  )
  config <- model@model[["fit"]][["config"]]
  expect_identical(.hp(model, "decision_width"), as.integer(config[["n_d"]]))
  expect_identical(.hp(model, "attention_width"), as.integer(config[["n_a"]]))
  # The importance sample follows the number of training cases and stays
  # unset.
  expect_null(.hp(model, "importance_sample_size"))
})

test_that("TabNet accepts an importance sample size only with importance computed", {
  expect_identical(
    setup_TabNet(importance_sample_size = 12L)[["importance_sample_size"]],
    12L
  )
  expect_error(
    setup_TabNet(skip_importance = TRUE, importance_sample_size = 12L),
    "applies only when @skip_importance"
  )
  hp <- setup_TabNet(importance_sample_size = 12L)
  expect_error(
    hp@skip_importance <- TRUE,
    "applies only when @skip_importance"
  )
  expect_identical(
    get_spec_fields(
      TabNetHyperparameters@properties[["importance_sample_size"]]
    )[["applies_when"]],
    list(skip_importance = FALSE)
  )
})

test_that("LightRuleFit records what its boosting and lasso stages resolved", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("glmnet")
  set.seed(8)
  model <- train(
    .bin,
    hyperparameters = setup_LightRuleFit(),
    verbosity = 0L
  )
  expect_identical(
    .hp(model, "objective"),
    model@model@model_lightgbm@hyperparameters[["objective"]]
  )
  expect_identical(
    .hp(model, "lambda_glmnet"),
    model@model@model_glmnet@hyperparameters[["lambda"]]
  )
})


test_that("LightRuleFit stops with a corrective error when boosting makes no split", {
  skip_if_not_installed("lightgbm")
  expect_error(
    train(mtcars, hyperparameters = setup_LightRuleFit(), verbosity = 0L),
    "no split",
    class = "rtemis_value_error"
  )
})


test_that("the conditional SuperLearner records its oracle loss", {
  set.seed(9)
  model <- train(
    mtcars,
    hyperparameters = setup_ConditionalSuperLearner(
      base_learners = list(glm = setup_GLM(), cart = setup_CART()),
      meta_learner = setup_CART()
    ),
    verbosity = 0L
  )
  expect_identical(.hp(model, "loss"), "squared_error")
})
