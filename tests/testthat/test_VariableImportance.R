# test_VariableImportance.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% helpers ----
.measure <- function(value = c(0.75, NA_real_), kind = "split_gain", ...) {
  importance_measure(
    c("age", "weight"),
    value,
    kind = kind,
    description = "A measure.",
    ...
  )
}


test_that("variable importance holds named measures, each with its descriptor", {
  result <- VariableImportance(
    measures = list(
      Gain = .measure(),
      Coefficient = .measure(
        c(-2, 0),
        kind = "coefficient",
        signed = TRUE,
        scale_dependent = TRUE,
        direction = "absolute"
      ),
      `another measure` = .measure(c(Inf, 1))
    )
  )
  expect_identical(
    names(result@measures),
    c("Gain", "Coefficient", "another measure")
  )
  expect_identical(result@measures[["Coefficient"]]@kind, "coefficient")
  # A value that is not finite is unavailable.
  expect_identical(
    result@measures[["another measure"]]@values,
    c(age = NA_real_, weight = 1)
  )
  table <- varimp_table(result)
  expect_identical(
    names(table),
    c("variable", "Gain", "Coefficient", "another measure")
  )
  expect_identical(table[["Coefficient"]], c(-2, 0))
  expect_identical(
    schema_publication_annotation(VariableImportance)[["scope"]],
    "shared"
  )
  expect_error(VariableImportance(measures = list()), "at least 1")
  expect_error(.measure(kind = "gain"))
  expect_error(.measure(computed_on = "test"))
  expect_error(
    importance_measure(
      character(),
      numeric(),
      kind = "split_gain",
      description = ""
    ),
    "empty"
  )
  # One value per name: a repeated predictor is rejected.
  expect_error(
    importance_measure(
      c("x", "x"),
      c(1, 2),
      kind = "split_gain",
      description = ""
    ),
    "unique"
  )
})

test_that("varimp_table aligns measures that cover different predictors", {
  result <- VariableImportance(
    measures = list(
      a = importance_measure(
        c("x", "y"),
        c(1, 2),
        kind = "split_gain",
        description = ""
      ),
      b = importance_measure(
        c("y", "z"),
        c(3, 4),
        kind = "split_gain",
        description = ""
      )
    )
  )
  table <- varimp_table(result)
  expect_identical(table[["variable"]], c("x", "y", "z"))
  expect_identical(table[["a"]], c(1, 2, NA))
  expect_identical(table[["b"]], c(NA, 3, 4))
})

test_that("variable importance roundtrips through its wire form", {
  result <- VariableImportance(
    measures = list(
      Gain = .measure(),
      Coefficient = .measure(c(-2, 0), kind = "coefficient", signed = TRUE)
    )
  )
  wire <- jsonlite::fromJSON(
    jsonlite::toJSON(
      S7_to_list(result),
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ),
    simplifyVector = FALSE
  )
  expect_identical(names(wire[["measures"]]), c("Gain", "Coefficient"))
  decoded <- do.call(VariableImportance, from_wire(wire, VariableImportance))
  expect_identical(
    varimp_table(decoded)[["Gain"]],
    c(0.75, NA_real_)
  )
  expect_identical(decoded@measures[["Coefficient"]]@signed, TRUE)
})

test_that("additional table columns roundtrip without source class defaults", {
  Measures <- schema_class(
    name = "AdditionalColumns",
    package = "rtemis",
    properties = list(
      data = prop_table(
        columns = list(variable = prop_string(description = "Name.")),
        additional = prop_float(NULL, nullable = TRUE, description = "Value."),
        min_columns = 2L,
        min_items = 1L,
        description = "Rows."
      )
    ),
    publication = SchemaPublication(
      kind = "report",
      scope = "shared",
      description = "Rows with additional numeric columns."
    )
  )
  schema <- S7_to_JSONSchema(
    Measures,
    id = "https://example.test/additional",
    asserted = TRUE
  )
  row <- schema[["properties"]][["data"]][["items"]]
  expect_identical(row[["minProperties"]], 2L)
  spec <- get_spec(Measures@properties[["data"]])
  declarations <- default_declarations(
    spec,
    schema[["properties"]][["data"]],
    "/properties/data"
  )
  expect_true(
    "/properties/data/items/additionalProperties" %in% names(declarations)
  )
  artifacts <- list(
    format_version = 1L,
    declarations = stats::setNames(list(declarations), schema[["$id"]])
  )
  graph <- default_artifact_graph(
    stats::setNames(list(schema), schema[["$id"]]),
    artifacts
  )
  Restored <- graph[["class"]](schema[["$id"]])
  expect_identical(
    spec_fields(get_spec(Restored@properties[["data"]])),
    spec_fields(spec)
  )
  expect_error(
    Restored(data = data.frame(variable = "x", Gain = "invalid")),
    "number"
  )
  wire <- list(
    data = list(
      list(variable = "age", Gain = 0.75, `another measure` = NULL),
      list(variable = "weight", Gain = NULL, `another measure` = 1)
    )
  )
  decoded <- do.call(Restored, from_wire(wire, Restored))
  expect_identical(
    names(decoded@data),
    c("variable", "Gain", "another measure")
  )
  expect_identical(decoded@data[["Gain"]], c(0.75, NA_real_))
})


test_that("typed additional members retain validation and defaults recursively", {
  property <- prop_struct(
    list(label = prop_string("value")),
    nullable = TRUE,
    additional = prop_array(prop_integer(2L, min = 1L), nullable = TRUE),
    min_members = 2L
  )
  spec <- get_spec(property)
  expect_identical(
    spec_fields(spec_object(spec_fields(spec))),
    spec_fields(spec)
  )
  Test <- S7::new_class(
    "AdditionalMembers",
    properties = list(payload = property)
  )
  expect_no_error(Test(payload = list(label = "value", counts = c(1L, 2L))))
  expect_error(Test(payload = list(label = "value")), "at least 2")
  expect_error(
    Test(payload = list(label = "value", counts = c(1, 2))),
    "integer"
  )
  expect_error(
    Test(payload = list(label = "value", counts = c(0L, 2L))),
    ">= 1"
  )
  expect_error(
    prop_table(
      list(x = prop_string()),
      additional = prop_array(prop_integer(1L), nullable = TRUE)
    ),
    "scalar"
  )
  expect_error(
    prop_struct(list(x = prop_string()), nullable = TRUE, min_members = 2L),
    "closed shape"
  )
  expect_error(
    prop_struct(list(x = prop_string()), nullable = TRUE, min_members = -1L),
    "non-negative"
  )
})


test_that("a LightGBM model without splits has unset variable importance", {
  skip_if_not_installed("lightgbm")
  dataset <- lightgbm::lgb.Dataset(
    matrix(1, nrow = 10L, ncol = 2L),
    label = seq_len(10L)
  )
  model <- lightgbm::lgb.train(
    params = list(objective = "regression", verbosity = -1L, num_threads = 1L),
    data = dataset,
    nrounds = 1L
  )
  expect_null(varimp_super(model))
})


test_that("each algorithm describes its importance measures as it computed them", {
  set.seed(11)
  bin <- iris[51:150, ]
  bin[["Species"]] <- droplevels(bin[["Species"]])
  reg <- mtcars[, c("wt", "hp", "qsec", "mpg")]
  measure <- function(model, name) get_varimp(model)@measures[[name]]

  cart <- measure(
    train(bin, hyperparameters = setup_CART(), verbosity = 0L),
    "importance"
  )
  expect_identical(cart@kind, "split_gain")
  expect_match(cart@description, "Gini index", fixed = TRUE)

  glm <- measure(
    train(reg, hyperparameters = setup_GLM(), verbosity = 0L),
    "Coefficient"
  )
  expect_identical(c(glm@kind, glm@direction), c("coefficient", "absolute"))
  expect_true(glm@signed && glm@scale_dependent)

  skip_if_not_installed("ranger")
  for (mode in c("impurity", "impurity_corrected", "permutation")) {
    m <- measure(
      train(
        bin,
        hyperparameters = setup_Ranger(num_trees = 20L, importance = mode),
        verbosity = 0L
      ),
      "importance"
    )
    expected <- switch(
      mode,
      impurity = c("split_gain", "training"),
      impurity_corrected = c("corrected_split_gain", "training"),
      permutation = c("permutation", "out_of_bag")
    )
    expect_identical(c(m@kind, m@computed_on), expected, info = mode)
  }
  expect_null(get_varimp(train(
    bin,
    hyperparameters = setup_Ranger(num_trees = 20L, importance = "none"),
    verbosity = 0L
  )))

  skip_if_not_installed("e1071")
  svm <- measure(
    train(bin, hyperparameters = setup_LinearSVM(), verbosity = 0L),
    "Coefficient"
  )
  expect_false(svm@scale_dependent)
  expect_match(svm@description, "per standard deviation", fixed = TRUE)

  skip_if_not_installed("lightgbm")
  lgb <- get_varimp(train(
    bin,
    hyperparameters = setup_LightGBM(force_nrounds = 20L),
    verbosity = 0L
  ))
  expect_identical(names(lgb@measures), c("Gain", "Cover", "Frequency"))
  expect_identical(
    vapply(lgb@measures, function(m) m@kind, character(1L)),
    c(Gain = "split_gain", Cover = "split_cover", Frequency = "split_frequency")
  )
})


test_that("measure names that are table columns or repeat an aggregate are kept apart", {
  result <- VariableImportance(
    measures = list(
      variable = importance_measure(
        c("x", "y"),
        c(1, 2),
        kind = "contribution",
        description = ""
      ),
      fold = importance_measure(
        c("x", "y"),
        c(3, 4),
        kind = "contribution",
        description = ""
      )
    )
  )
  table <- varimp_table(result)
  expect_identical(
    names(table),
    c("variable", "variable (measure)", "fold (measure)")
  )
  expect_identical(table[["variable"]], c("x", "y"))
  expect_identical(table[["variable (measure)"]], c(1, 2))
})

test_that("plots rank bars as each measure's direction declares", {
  skip_if_not_installed("rtemis.draw")
  mod <- Regression(
    algorithm = "GLM",
    hyperparameters = setup_GLM(),
    execution_config = setup_SerialExecution(),
    xnames = c("good", "bad"),
    y_training = 1:4,
    predicted_training = 1:4
  )
  mod@varimp <- VariableImportance(
    measures = list(
      cv_risk = importance_measure(
        c("good", "bad"),
        c(0.1, 2),
        kind = "cross_validated_risk",
        computed_on = "cross_validation",
        direction = "smaller",
        description = ""
      ),
      permutation = importance_measure(
        c("good", "bad"),
        c(0.5, -3),
        kind = "permutation",
        computed_on = "out_of_bag",
        signed = TRUE,
        description = ""
      )
    )
  )
  bars <- function(...) {
    option <- plot_varimp(mod, top_n = 1L, ...)[["x"]][["option"]]
    unlist(option[["yAxis"]][["data"]] %||% option[["xAxis"]][["data"]])
  }
  expect_identical(bars(measure = "cv_risk"), "good")
  expect_identical(bars(measure = "permutation"), "good")
  # An explicit choice overrides the declared direction.
  expect_identical(
    bars(measure = "permutation", rank_by = "magnitude"),
    "bad"
  )
})

test_that("descriptors follow the settings the model was fitted with", {
  set.seed(13)
  bin <- iris[51:150, ]
  bin[["Species"]] <- droplevels(bin[["Species"]])
  reg <- mtcars[, c("wt", "hp", "qsec", "mpg")]
  measure <- function(model, name = names(get_varimp(model)@measures)[[1L]]) {
    get_varimp(model)@measures[[name]]
  }

  # CART with the information split.
  info <- rpart::rpart(Species ~ ., bin, parms = list(split = "information"))
  expect_match(
    varimp_super(info)@measures[["importance"]]@description,
    "entropy",
    fixed = TRUE
  )

  # MARS with one predictor reports evimp's unnormalized sums.
  skip_if_not_installed("earth")
  one <- measure(train(
    mtcars[, c("wt", "mpg")],
    hyperparameters = setup_MARS(),
    verbosity = 0L
  ))
  expect_false(grepl("square root", one@description, fixed = TRUE))
  several <- measure(train(reg, hyperparameters = setup_MARS(), verbosity = 0L))
  expect_match(several@description, "square root", fixed = TRUE)

  # NNLS reports normalization.
  skip_if_not_installed("nnls")
  normalized <- measure(train(
    reg,
    hyperparameters = setup_NNLS(normalize = TRUE),
    verbosity = 0L
  ))
  expect_match(normalized@description, "sum to 1", fixed = TRUE)
  raw <- measure(train(
    reg,
    hyperparameters = setup_NNLS(normalize = FALSE),
    verbosity = 0L
  ))
  expect_false(grepl("sum to 1", raw@description, fixed = TRUE))

  # BART needs two draws with a split for a standard deviation.
  sparse <- bart_varimp(
    c("x", "y"),
    c(1, 0),
    c(NA_real_, NA_real_),
    n_draws = 1L
  )
  expect_identical(
    sparse@measures[["inclusion_sd"]]@values,
    c(x = NA_real_, y = NA_real_)
  )

  skip_if_not_installed("ranger")
  ranger_measure <- function(...) {
    measure(train(
      bin,
      hyperparameters = setup_Ranger(num_trees = 20L, ...),
      verbosity = 0L
    ))
  }
  scaled <- ranger_measure(
    importance = "permutation",
    scale_permutation_importance = TRUE
  )
  expect_match(scaled@description, "standard error", fixed = TRUE)
  local <- ranger_measure(
    importance = "permutation",
    scale_permutation_importance = TRUE,
    local_importance = TRUE
  )
  expect_false(grepl("standard error", local@description, fixed = TRUE))
  hellinger <- ranger_measure(importance = "impurity", splitrule = "hellinger")
  expect_match(hellinger@description, "Hellinger distance", fixed = TRUE)
  expect_match(hellinger@description, "averaged over trees", fixed = TRUE)

  skip_if_not_installed("spls")
  splsda <- measure(train(bin, hyperparameters = setup_SPLS(), verbosity = 0L))
  expect_false(splsda@scale_dependent)
  expect_match(splsda@description, "per standard deviation", fixed = TRUE)

  skip_if_not_installed("e1071")
  svm_reg <- measure(train(
    reg,
    hyperparameters = setup_LinearSVM(),
    verbosity = 0L
  ))
  expect_match(svm_reg@description, "regression function", fixed = TRUE)
  svm_bin <- train(bin, hyperparameters = setup_LinearSVM(), verbosity = 0L)
  m <- measure(svm_bin)
  expect_match(m@description, "toward class virginica", fixed = TRUE)
  # The weights agree in sign with the margin toward the positive class.
  margin <- svm_margin(svm_bin@model, bin[1:5, 1:4])
  x_scaled <- scale(
    as.matrix(bin[1:5, 1:4]),
    svm_bin@model[["x.scale"]][["scaled:center"]],
    svm_bin@model[["x.scale"]][["scaled:scale"]]
  )
  linear <- drop(x_scaled %*% m@values[colnames(x_scaled)])
  expect_equal(cor(linear, drop(margin)), 1, tolerance = 1e-6)
})

test_that("a LightRuleFit class named like the aggregate keeps both measures", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("glmnet")
  dat <- iris
  levels(dat[["Species"]]) <- c("Coefficient", "class_b", "class_c")
  set.seed(3)
  vi <- get_varimp(train(
    dat,
    hyperparameters = setup_LightRuleFit(nrounds = 20L),
    verbosity = 0L
  ))
  expect_true(all(
    c("Coefficient", "Coefficient (class)", "class_b", "class_c") %in%
      names(vi@measures)
  ))
  expect_identical(vi@measures[["Coefficient"]]@kind, "coefficient_magnitude")
  expect_identical(vi@measures[["Coefficient (class)"]]@kind, "coefficient")
})


test_that("ranger holdout importance is labeled as computed on held-out cases", {
  skip_if_not_installed("ranger")
  set.seed(17)
  bin <- iris[51:150, ]
  bin[["Species"]] <- droplevels(bin[["Species"]])
  weights <- rep(c(1, 0), times = c(70L, 30L))[sample(100L)]
  model <- train(
    bin,
    hyperparameters = setup_Ranger(
      num_trees = 20L,
      importance = "permutation",
      holdout = TRUE
    ),
    weights = weights,
    verbosity = 0L
  )
  m <- get_varimp(model)@measures[["importance"]]
  expect_identical(m@computed_on, "held_out")
  expect_match(m@description, "held-out cases", fixed = TRUE)
})

test_that("a discrete stacked ensemble reports selection weights", {
  set.seed(19)
  dat <- mtcars[, c("wt", "hp", "qsec", "mpg")]
  model <- train(
    dat,
    hyperparameters = setup_SuperLearner(
      base_learners = list(glm = setup_GLM(), cart = setup_CART()),
      discrete = TRUE
    ),
    verbosity = 0L
  )
  vi <- get_varimp(model)
  expect_match(
    vi@measures[["weight"]]@description,
    "Selection weight",
    fixed = TRUE
  )
  expect_identical(sort(unname(vi@measures[["weight"]]@values)), c(0, 1))
  expect_identical(vi@measures[["cv_risk"]]@direction, "smaller")
  expect_match(
    vi@measures[["cv_risk"]]@description,
    "Mean squared error",
    fixed = TRUE
  )
})
