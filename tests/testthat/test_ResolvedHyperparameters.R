# test_ResolvedHyperparameters.R
# ::rtemis::
# 2026- EDG rtemis.org

# A hyperparameter left unset is chosen by the backend; the fitted model
# records the value the backend used, read from the fitted object.

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
  # A value the user set is what the fit used.
  set_by_user <- train(
    .bin,
    hyperparameters = setup_Ranger(num_trees = 20L, mtry = 3L),
    verbosity = 0L
  )
  expect_identical(.hp(set_by_user, "mtry"), 3L)
  expect_identical(as.integer(set_by_user@model[["mtry"]]), 3L)
})

test_that("MARS records earth's penalty, term limit and pruning bound", {
  skip_if_not_installed("earth")
  model <- train(mtcars, hyperparameters = setup_MARS(), verbosity = 0L)
  expect_identical(.hp(model, "penalty"), as.numeric(model@model[["penalty"]]))
  expect_identical(.hp(model, "nk"), as.integer(model@model[["nk"]]))
  expect_identical(.hp(model, "nprune"), NROW(model@model[["dirs"]]))
})

test_that("LightGBM records the parameters its model ran with", {
  skip_if_not_installed("lightgbm")
  model <- train(
    mtcars,
    hyperparameters = setup_LightGBM(force_nrounds = 10L),
    verbosity = 0L
  )
  parameters <- lightgbm_model_parameters(model@model)
  expect_identical(
    .hp(model, "boost_from_average"),
    as.integer(parameters[["boost_from_average"]]) != 0L
  )
  expect_identical(.hp(model, "sigmoid"), as.numeric(parameters[["sigmoid"]]))
  # DART settings have no effect under gbdt and stay unset.
  expect_null(.hp(model, "drop_rate"))
})

test_that("HAL records the knots its generator placed", {
  skip_if_not_installed("hal9001")
  model <- train(
    mtcars[, c("cyl", "disp", "hp", "mpg")],
    hyperparameters = setup_HAL(),
    verbosity = 0L
  )
  expected <- as.integer(hal9001:::num_knots_generator(
    max_degree = .hp(model, "max_degree"),
    smoothness_orders = .hp(model, "smoothness_orders"),
    base_num_knots_0 = 200,
    base_num_knots_1 = 50
  ))
  expect_identical(.hp(model, "num_knots"), expected)
})

test_that("BART records the feature subsample size", {
  skip_if_not_installed("stochtree")
  model <- train(
    mtcars,
    hyperparameters = setup_BART(num_gfr = 2L, num_mcmc = 5L),
    verbosity = 0L
  )
  expect_identical(
    .hp(model, "num_features_subsample"),
    as.integer(model@model[["model_params"]][["num_covariates"]])
  )
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
  expect_identical(.hp(model, "importance_sample_size"), NROW(mtcars))
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
