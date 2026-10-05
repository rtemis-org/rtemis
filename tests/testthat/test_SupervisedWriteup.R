# test_SupervisedWriteup.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% Models ----
set.seed(2026)
.bin <- iris[51:150, ]
.bin[["Species"]] <- droplevels(.bin[["Species"]])
.idx <- sample(100, 70)
.mod_bin <- train(
  .bin[.idx, ],
  dat_test = .bin[-.idx, ],
  hyperparameters = setup_CART(maxdepth = tune_over(1L, 2L, 3L)),
  verbosity = 0L
)
.mod_reg <- train(
  mtcars[1:22, ],
  dat_test = mtcars[23:32, ],
  hyperparameters = setup_GLM(),
  verbosity = 0L
)
.mod_res <- train(
  iris,
  hyperparameters = setup_CART(),
  outer_resampling_config = setup_KFold(n_resamples = 5L),
  verbosity = 0L
)
.mods <- list(bin = .mod_bin, reg = .mod_reg, res = .mod_res)
.wus <- lapply(.mods, writeup)

# Paragraph templates of a writeup.
.templates <- function(w) unlist(lapply(w@sections, function(s) s@paragraphs))

# All templates of a writeup as one string.
.text <- function(w) paste(.templates(w), collapse = " ")

# The value of a token.
.value <- function(w, key) w@values[["value"]][match(key, w@values[["key"]])]
.value_text <- function(w, key) {
  w@values[["text"]][match(key, w@values[["key"]])]
}

# A section's templates, by heading.
.section <- function(w, heading) {
  s <- Find(function(s) s@heading == heading, w@sections)
  if (is.null(s)) NULL else paste(s@paragraphs, collapse = " ")
}

# Text that may carry digits without being a number: tokens, code spans, and
# the metric name F1.
.strip_names <- function(text) {
  text <- gsub(WRITEUP_REF_TOKEN, "", text)
  text <- gsub(WRITEUP_VALUE_TOKEN, "", text)
  text <- gsub("`[^`]*`", "", text)
  gsub("\\bF1\\b", "", text)
}


# %% Numbers come from the values table ----
test_that("templates hold no digit outside a token or a name", {
  for (w in .wus) {
    expect_false(
      any(grepl("[0-9]", .strip_names(.templates(w)))),
      info = w@algorithm
    )
  }
})

test_that("every token names a row, and every row is rendered", {
  for (w in .wus) {
    tokens <- regmatches(.text(w), gregexpr(WRITEUP_VALUE_TOKEN, .text(w)))[[
      1L
    ]]
    keys <- unique(substr(tokens, 2L, nchar(tokens) - 1L))
    expect_true(all(keys %in% w@values[["key"]]))
    # Rows not in a paragraph are cells of the performance table or of the
    # hyperparameter tables.
    table_only <- setdiff(w@values[["key"]], keys)
    expect_true(all(grepl(
      "^table_(training|training_sd|test|test_sd|lower|upper|hp)_",
      table_only
    )))
    cells <- c(w@hyperparameters[["value"]], w@hyperparameters[["tried"]])
    cell_tokens <- unlist(regmatches(
      cells,
      gregexpr(WRITEUP_VALUE_TOKEN, cells)
    ))
    expect_true(all(
      substr(cell_tokens, 2L, nchar(cell_tokens) - 1L) %in% w@values[["key"]]
    ))
    expect_false(anyDuplicated(w@values[["key"]]) > 0L)
  }
})

test_that("the performance table renders only values-table cells", {
  w <- .wus[["bin"]]
  rows <- writeup_table_rows(w)
  cells <- as.vector(rows[-1L, -1L])
  cells <- unlist(strsplit(cells[nzchar(cells)], "\u2013", fixed = TRUE))
  expect_true(all(cells %in% w@values[["text"]]))
  expect_identical(
    rows[1L, ],
    c(
      "Metric",
      "Training",
      "Test",
      paste0(.value_text(w, "confidence_percent"), "% CI")
    )
  )
  wr <- .wus[["res"]]
  expect_match(
    writeup_table_caption(wr),
    "mean (SD) over 5 resamples",
    fixed = TRUE
  )
})

test_that("values equal the quantities they name, computed independently", {
  w <- .wus[["bin"]]
  y <- .mod_bin@y_test
  predicted <- .mod_bin@predicted_test
  expect_identical(.value(w, "n_training"), 70)
  expect_identical(.value(w, "n_test"), 30)
  expect_identical(.value(w, "n_predictors"), 4)
  # Sensitivity of the positive class (the second level) and its
  # Clopper-Pearson interval.
  positive <- levels(y)[[2L]]
  hits <- sum(predicted == positive & y == positive)
  n_positive <- sum(y == positive)
  expect_equal(.value(w, "table_test_sensitivity"), hits / n_positive)
  ci <- stats::binom.test(hits, n_positive)[["conf.int"]]
  expect_equal(.value(w, "table_lower_sensitivity"), ci[[1L]])
  expect_equal(.value(w, "table_upper_sensitivity"), ci[[2L]])
  expect_identical(.value_text(w, "positive_class"), positive)
  counts <- as.vector(table(.mod_bin@y_training))
  expect_identical(
    c(.value(w, "count_training_1"), .value(w, "count_training_2")),
    as.numeric(counts)
  )
  expect_identical(
    .value(w, "selected_maxdepth"),
    as.numeric(.mod_bin@tuner@best_hyperparameters[["maxdepth"]])
  )
  # Candidates come from the space the tuner searched.
  expect_identical(.value(w, "grid_maxdepth_3"), 3)
  expect_identical(
    w@values[["source"]][match("grid_maxdepth_3", w@values[["key"]])],
    "tuner.hyperparameters.maxdepth[3]"
  )

  wr <- .wus[["res"]]
  expect_identical(.value(wr, "n_cases"), 150)
  expect_identical(.value(wr, "outer_resamples"), 5)
  fold_ba <- vapply(
    .mod_res@models,
    function(m) m@metrics_test@metrics[["overall"]][["balanced_accuracy"]],
    numeric(1L)
  )
  expect_equal(.value(wr, "table_test_balanced_accuracy"), mean(fold_ba))
  expect_equal(
    .value(wr, "table_test_sd_balanced_accuracy"),
    stats::sd(fold_ba)
  )
})

test_that("versions are those recorded when the model was trained", {
  w <- .wus[["bin"]]
  si <- .mod_bin@session_info
  expect_identical(
    .value_text(w, "version_r"),
    paste(si[["R.version"]][["major"]], si[["R.version"]][["minor"]], sep = ".")
  )
  rpart_version <- (si[["otherPkgs"]][["rpart"]] %||%
    si[["loadedOnly"]][["rpart"]])[["Version"]]
  expect_identical(.value_text(w, "version_rpart"), rpart_version)
})

test_that("formatting follows the value kind", {
  expect_identical(fmt_writeup_value(1234L, "count"), "1,234")
  expect_identical(fmt_writeup_value(1234L, "integer"), "1234")
  expect_identical(fmt_writeup_value(0.91666, "metric"), "0.917")
  expect_identical(fmt_writeup_value(0.0304885, "quantity"), "0.0305")
  expect_identical(fmt_writeup_value(0.1, "quantity"), "0.1")
  expect_identical(fmt_writeup_value(0.0004, "p_value"), "p < 0.001")
  expect_identical(fmt_writeup_value(0.0123, "p_value"), "p = 0.012")
  expect_identical(fmt_writeup_value(0.95, "percent"), "95")
  expect_identical(fmt_writeup_value(0.975, "percent"), "97.5")
  # A setting is written in full.
  expect_identical(
    fmt_writeup_value(0.123456789012345678, "exact"),
    "0.123456789012346"
  )
  expect_identical(fmt_writeup_value(1e-08, "exact"), "1e-08")
  expect_identical(fmt_writeup_value(0.05, "exact"), "0.05")
})


# %% Rendering ----
test_that("rendering replaces tokens and numbers references by first citation", {
  values <- data.frame(
    key = "n",
    value = 3,
    kind = "count",
    text = "3",
    source = "here"
  )
  refs <- c(a = 1L, b = 2L)
  expect_identical(
    render_writeup_template("{n} cases {ref:b,a}.", values, refs),
    "3 cases [1, 2]."
  )
  expect_error(
    render_writeup_template("{m} cases.", values, refs),
    "names no row",
    class = "rtemis_value_error"
  )
  expect_error(
    render_writeup_template("x {ref:c}.", values, refs),
    "no reference",
    class = "rtemis_value_error"
  )
  for (w in .wus) {
    tokens <- regmatches(.text(w), gregexpr(WRITEUP_REF_TOKEN, .text(w)))[[1L]]
    first <- unique(trimws(unlist(strsplit(
      substr(tokens, 6L, nchar(tokens) - 1L),
      ","
    ))))
    expect_identical(w@references[["key"]], first)
  }
})

test_that("the rendered text states the numbers of the values table", {
  text <- repr(.wus[["bin"]], output_type = "plain")
  expect_match(
    text,
    "comprised 70 training cases and 30 test cases, with 4 predictors.",
    fixed = TRUE
  )
  expect_false(grepl("{", text, fixed = TRUE))
  expect_match(text, "Clopper-Pearson method [", fixed = TRUE)
})


# %% Content ----
test_that("a single split states its statistics, a resampled model does not", {
  bin <- .text(.wus[["bin"]])
  expect_match(bin, "exact McNemar test", fixed = TRUE)
  expect_match(bin, "Clopper-Pearson", fixed = TRUE)
  res <- .text(.wus[["res"]])
  expect_match(
    res,
    "no confidence intervals or tests were computed",
    fixed = TRUE
  )
  expect_false(grepl("McNemar", res, fixed = TRUE))
  expect_match(res, "{outer_resamples}-fold cross-validation", fixed = TRUE)
  expect_match(res, "stratified by outcome class", fixed = TRUE)
})

test_that("the statistics paragraph names intervals it could not obtain", {
  rv <- review(.mod_bin)
  rv@performance[["lower"]][rv@performance[["metric"]] == "auc"] <- NA_real_
  ctx <- writeup_context(.mod_bin, rv)
  text <- writeup_statistics_single(ctx, writeup_collector(), "{p}")
  expect_false(grepl("DeLong", text, fixed = TRUE))
  expect_match(
    text,
    "No confidence interval could be computed for AUC",
    fixed = TRUE
  )
})

test_that("tuning is described from the tuner", {
  bin <- .section(.wus[["bin"]], "Hyperparameter tuning")
  expect_match(bin, "grid search over `maxdepth`", fixed = TRUE)
  expect_match(bin, "highest mean balanced accuracy", fixed = TRUE)
  expect_null(.section(.wus[["res"]], "Hyperparameter tuning"))
})

test_that("a randomized search reports the combinations evaluated", {
  set.seed(5)
  w <- writeup(train(
    iris,
    hyperparameters = setup_CART(maxdepth = tune_over(1L, 2L, 3L)),
    tuner_config = setup_GridSearch(
      search_type = "randomized",
      randomize_p = 0.5
    ),
    verbosity = 0L
  ))
  expect_match(
    .section(w, "Hyperparameter tuning"),
    "A random {grid_evaluated} of the {grid_eligible} combinations were evaluated.",
    fixed = TRUE
  )
  expect_identical(.value(w, "grid_evaluated"), 2)
  expect_identical(.value(w, "grid_eligible"), 3)
})

test_that("a hyperparameter set is described by its configurations", {
  set.seed(7)
  single <- writeup(train(
    iris,
    hyperparameters = list(
      a = setup_CART(cp = 0.001, maxdepth = 2L),
      b = setup_CART(cp = 0.1, maxdepth = tune_over(4L, 6L))
    ),
    verbosity = 0L
  ))
  text <- .section(single, "Hyperparameter tuning")
  expect_match(
    text,
    "`{variant_a}` (`cp` = {variant_a_cp_1}, `maxdepth` = {variant_a_maxdepth_1})",
    fixed = TRUE
  )
  expect_match(
    text,
    "`{variant_b}` (`cp` = {variant_b_cp_1}, `maxdepth` {variant_b_maxdepth_1} or {variant_b_maxdepth_2})",
    fixed = TRUE
  )
  expect_identical(.value(single, "variant_b_cp_1"), 0.1)
  resampled <- writeup(train(
    iris,
    hyperparameters = list(
      shallow = setup_CART(maxdepth = 2L),
      deep = setup_CART(maxdepth = 6L)
    ),
    outer_resampling_config = setup_KFold(n_resamples = 3L),
    verbosity = 0L
  ))
  expect_match(
    .text(resampled),
    "`{variant_shallow}` (`maxdepth` = {variant_shallow_maxdepth_1}) and `{variant_deep}` (`maxdepth` = {variant_deep_maxdepth_1})",
    fixed = TRUE
  )
  expect_identical(.value(resampled, "variant_deep_maxdepth_1"), 6)
})

test_that("decomposition reports the components the fit kept", {
  model <- train(
    iris,
    hyperparameters = setup_CART(),
    decomposition_config = setup_PCA(k = 3L, tol = 0.9),
    verbosity = 0L
  )
  w <- writeup(model)
  expect_identical(
    .value(w, "decomposition_components"),
    as.numeric(NCOL(model@decomposition@transformed))
  )
  # Without a test set, nothing is applied to test cases.
  text <- .section(w, "Preprocessing")
  expect_match(
    text,
    "Decomposition was fitted on the training cases.",
    fixed = TRUE
  )
  expect_false(grepl("test cases", text, fixed = TRUE))
})

test_that("a model without a test set is described as evaluated on training cases", {
  w <- writeup(train(iris, hyperparameters = setup_CART(), verbosity = 0L))
  expect_match(.text(w), "evaluated on the training cases only", fixed = TRUE)
  expect_match(
    .text(w),
    "Performance on the training cases is shown",
    fixed = TRUE
  )
  expect_null(.section(w, "Comparison with a baseline"))
  expect_false("confidence_percent" %in% w@values[["key"]])
})

test_that("leave-one-out and user-defined resampling are described", {
  loocv <- writeup(train(
    mtcars[1:12, ],
    hyperparameters = setup_GLM(),
    outer_resampling_config = setup_LOOCV(),
    verbosity = 0L
  ))
  expect_match(
    .section(loocv, "Evaluation"),
    "leave-one-out cross-validation",
    fixed = TRUE
  )
  custom <- writeup(train(
    iris,
    hyperparameters = setup_CART(),
    outer_resampling_config = setup_Custom(list(
      c(1:40, 51:90, 101:140),
      c(11:50, 61:100, 111:150)
    )),
    verbosity = 0L
  ))
  expect_match(
    .section(custom, "Evaluation"),
    "{outer_resamples} user-defined resamples",
    fixed = TRUE
  )
})

test_that("resampled baseline counts use the resamples in which both were defined", {
  text <- .section(.wus[["res"]], "Comparison with a baseline")
  expect_match(
    text,
    "of the {baseline_accuracy_resamples_compared}",
    fixed = TRUE
  )
  b <- .wus[["res"]]@review@baseline
  expect_identical(
    b[["resamples_compared"]],
    rep(5L, NROW(b))
  )
})

test_that("unrecorded information is listed", {
  expect_true("How the test set was selected." %in% .wus[["bin"]]@not_reported)
  expect_false("How the test set was selected." %in% .wus[["res"]]@not_reported)
  mars <- writeup(train(mtcars, hyperparameters = setup_MARS(), verbosity = 0L))
  expect_true(any(grepl(
    "How MARS selects values inside each fit",
    mars@not_reported
  )))
})

test_that("internal selection is described for GLMNET, LightGBM, HAL and LINAD", {
  ctx <- list(
    tuner = .mod_bin@tuner,
    space_root = "tuner.hyperparameters",
    grid = data.frame(param_combo_id = 1L),
    fits = list(.mod_bin)
  )
  w <- writeup_collector()
  out <- writeup_selection(setup_GLMNET(), ctx, w)
  expect_match(out[["tuning"]], "`cv.glmnet`", fixed = TRUE)
  expect_match(out[["tuning"]], "within one standard error", fixed = TRUE)
  expect_identical(out[["collected"]], "lambda")
  out <- writeup_selection(setup_GLMNET(which_lambda_cv = "lambda.min"), ctx, w)
  expect_match(
    out[["tuning"]],
    "minimizing the cross-validated error",
    fixed = TRUE
  )
  expect_identical(
    writeup_selection(setup_GLMNET(lambda = 0.1), ctx, w),
    list()
  )
  out <- writeup_selection(setup_LightGBM(), ctx, w)
  expect_match(
    out[["tuning"]],
    "early stopping on the inner validation cases",
    fixed = TRUE
  )
  out <- writeup_selection(setup_LightGBM(boosting = "dart"), ctx, w)
  expect_match(
    out[["tuning"]],
    "without early stopping, which DART boosting does not support",
    fixed = TRUE
  )
  expect_match(
    writeup_selection(setup_HAL(), ctx, w)[["model"]],
    "`hal9001`",
    fixed = TRUE
  )
  expect_match(
    writeup_selection(setup_LINAD(), ctx, w)[["model"]],
    "keeps every leaf",
    fixed = TRUE
  )
  ctx[["grid"]] <- data.frame(param_combo_id = 1L, best_n_leaves = 3L)
  expect_identical(
    writeup_selection(setup_LINAD(), ctx, w)[["collected"]],
    "best_n_leaves"
  )
  expect_null(writeup_selection(setup_CART(), ctx, w))
})

test_that("each algorithm's declared backends are the packages its train_ method checks", {
  for (algorithm in names(SUPERVISED_BACKENDS)) {
    packages <- setdiff(SUPERVISED_BACKENDS[[algorithm]], "stats")
    if (length(packages) == 0L) {
      next
    }
    cls <- get(paste0(algorithm, "Hyperparameters"))
    body <- paste(deparse(S7::method(train_, cls)), collapse = "\n")
    for (p in packages) {
      expect_match(
        body,
        paste0("\"", p, "\""),
        fixed = TRUE,
        info = paste(algorithm, p)
      )
    }
  }
})


# %% Stratification ----
test_that("stratification groups a categorical variable by its observed levels", {
  y <- factor(rep(letters[1:6], each = 10L))
  folds <- kfold(y, k = 5L, seed = 1L, verbosity = 0L)
  for (f in folds) {
    expect_identical(as.vector(table(y[-f])), rep(2L, 6L))
  }
  subsamples <- strat_sub(y, n_resamples = 3L, train_p = 0.5, seed = 1L)
  for (s in subsamples) {
    expect_identical(as.vector(table(y[s])), rep(5L, 6L))
  }
  # Unused levels and character values form one stratum per observed value.
  sparse <- factor(
    rep(c("a", "b", "c", "j"), each = 10L),
    levels = letters[1:10]
  )
  for (f in kfold(sparse, k = 5L, seed = 1L, verbosity = 0L)) {
    expect_identical(as.vector(table(droplevels(sparse[-f]))), rep(2L, 4L))
  }
  chr <- rep(c("x", "y", "z"), each = 10L)
  for (f in kfold(chr, k = 5L, seed = 1L, verbosity = 0L)) {
    expect_identical(as.vector(table(chr[-f])), rep(2L, 3L))
  }
})

test_that("k-fold records the intervals a numeric outcome was cut into", {
  set.seed(9)
  dat <- data.frame(x = rnorm(30), y = rep(c(1, 2), 15))
  w <- writeup(train(
    dat,
    hyperparameters = setup_GLM(),
    outer_resampling_config = setup_KFold(n_resamples = 3L),
    verbosity = 0L
  ))
  expect_identical(.value(w, "outer_strat_bins"), 2)
})


# %% Output ----
test_that("write_text writes a writeup as Markdown and refuses to overwrite", {
  path <- tempfile(fileext = ".md")
  write_text(.wus[["bin"]], path, verbosity = 0L)
  md <- readLines(path)
  expect_identical(md[[1L]], "## Methods")
  expect_true("## Results" %in% md)
  expect_true("## References" %in% md)
  expect_true(any(grepl("^\\| Metric \\| Training \\| Test \\|", md)))
  expect_false(any(grepl("{", md, fixed = TRUE)))
  expect_error(
    write_text(.wus[["bin"]], path, verbosity = 0L),
    "exists",
    class = "rtemis_value_error"
  )
  write_text(.wus[["bin"]], path, overwrite = TRUE, verbosity = 0L)
})


# %% Contract ----
test_that("a writeup record validates against its published schema", {
  testthat::skip_if_not_installed("jsonvalidate")
  doc <- record_object(.wus[["bin"]])
  schema_of <- function(cls) {
    schema <- S7_to_JSONSchema(
      cls,
      id = paste0("https://schema.rtemis.org/test/", tolower(cls@name), ".json")
    )
    schema[["$id"]] <- NULL
    schema[["properties"]][["$schema"]] <- NULL
    schema
  }
  targets <- list(
    reviewfinding = function() schema_of(ReviewFinding),
    writeupsection = function() schema_of(WriteupSection),
    supervisedreview = function() inline(schema_of(SupervisedReview))
  )
  inline <- function(node) {
    if (!is.list(node)) {
      return(node)
    }
    ref <- node[["$ref"]]
    if (is.character(ref) && length(ref) == 1L && startsWith(ref, "https://")) {
      for (slug in names(targets)) {
        if (grepl(paste0("/", slug, "/"), ref, fixed = TRUE)) {
          return(targets[[slug]]())
        }
      }
      stop("unexpected reference: ", ref)
    }
    lapply(node, inline)
  }
  as_json <- function(d) {
    jsonlite::toJSON(
      d,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    )
  }
  validate <- jsonvalidate::json_validator(
    as_json(inline(schema_of(SupervisedWriteup))),
    engine = "ajv"
  )
  expect_true(validate(as_json(doc), verbose = TRUE))
  # Negative cases: a value kind and an algorithm outside their vocabularies.
  bad <- doc
  bad[["values"]][["kind"]][[1L]] <- "number"
  expect_false(validate(as_json(bad)))
  bad <- doc
  bad[["algorithm"]] <- "NotAnAlgorithm"
  expect_false(validate(as_json(bad)))
  bad <- doc
  bad[["hyperparameters"]][["source"]][[1L]] <- "guessed"
  expect_false(validate(as_json(bad)))
})


# %% Hyperparameter tables ----
.hp_row <- function(w, name) {
  w@hyperparameters[w@hyperparameters[["name"]] == name, , drop = FALSE]
}
.render_cell <- function(w, cell) {
  render_writeup_template(cell, w@values, writeup_ref_numbers(w))
}
# Sources of the tokens of a cell.
.cell_sources <- function(w, cell) {
  tokens <- regmatches(cell, gregexpr(WRITEUP_VALUE_TOKEN, cell))[[1L]]
  keys <- substr(tokens, 2L, nchar(tokens) - 1L)
  w@values[["source"]][match(keys, w@values[["key"]])]
}
# One fit's entry for writeup_hp_cell().
.fit_value <- function(value, source = "default", applies = TRUE) {
  list(
    value = value,
    identity = as.character(jsonlite::toJSON(value, null = "null")),
    source = source,
    applies = applies
  )
}

test_that("table cells hold no number outside a token", {
  for (w in .wus) {
    cells <- c(w@hyperparameters[["value"]], w@hyperparameters[["tried"]])
    stripped <- gsub(WRITEUP_VALUE_TOKEN, "", cells)
    expect_false(any(grepl("[0-9]", stripped)), info = w@algorithm)
  }
})

test_that("the main table lists primary, tuned and specified hyperparameters", {
  w <- .wus[["bin"]]
  maxdepth <- .hp_row(w, "maxdepth")
  expect_true(maxdepth[["main"]])
  expect_identical(maxdepth[["source"]], "tuned")
  expect_identical(
    .render_cell(w, maxdepth[["value"]]),
    as.character(.mod_bin@hyperparameters[["maxdepth"]])
  )
  expect_identical(.render_cell(w, maxdepth[["tried"]]), "1; 2; 3")
  expect_identical(
    unique(.cell_sources(w, maxdepth[["tried"]])),
    "derived: distinct values of tuner.tuning_results.param_grid.maxdepth"
  )
  expect_identical(
    .cell_sources(w, maxdepth[["value"]]),
    "hyperparameters.maxdepth"
  )
  # Primary for CART, at its default.
  cp <- .hp_row(w, "cp")
  expect_true(cp[["main"]])
  expect_identical(cp[["source"]], "default")
  # Neither primary, tuned nor specified.
  expect_false(.hp_row(w, "maxcompete")[["main"]])
  # GLM declares no primary hyperparameter and was given none.
  expect_false(any(.wus[["reg"]]@hyperparameters[["main"]]))
  model <- .section(.wus[["bin"]], "Model")
  expect_match(model, "Table {table_hyperparameters} lists", fixed = TRUE)
  expect_false(grepl("default", model))
})

test_that("a value the backend chose is the fitted backend's", {
  skip_if_not_installed("ranger")
  set.seed(3)
  mod <- train(
    iris,
    hyperparameters = setup_Ranger(num_trees = 20L),
    verbosity = 0L
  )
  w <- writeup(mod)
  mtry <- .hp_row(w, "mtry")
  expect_identical(mtry[["source"]], "resolved")
  expect_identical(.cell_sources(w, mtry[["value"]]), "hyperparameters.mtry")
  # A value the user supplied.
  expect_identical(.hp_row(w, "num_trees")[["source"]], "specified")
  expect_identical(
    .render_cell(w, mtry[["value"]]),
    as.character(mod@model[["mtry"]])
  )
})

test_that("hyperparameters that do not apply are omitted and named", {
  skip_if_not_installed("lightgbm")
  mod <- train(
    mtcars,
    hyperparameters = setup_LightGBM(force_nrounds = 10L),
    verbosity = 0L
  )
  w <- writeup(mod)
  drop_rate <- .hp_row(w, "drop_rate")
  expect_false(drop_rate[["applies"]])
  expect_false("drop_rate" %in% writeup_hp_table_rows(w, main = FALSE)[, 1L])
  expect_match(
    writeup_hp_table_caption(w, main = FALSE),
    "Not applicable under this configuration:.*drop_rate"
  )
})

test_that("an unset hyperparameter reads unset, with its declared meaning", {
  w <- .wus[["bin"]]
  prune_cp <- .hp_row(w, "prune_cp")
  expect_identical(prune_cp[["source"]], "unset")
  rows <- writeup_hp_table_rows(w, main = FALSE)
  expect_identical(unname(rows[rows[, 1L] == "prune_cp", 2L]), "unset")
  expect_true(
    paste0("prune_cp: ", unset_meaning(CARTHyperparameters, "prune_cp")) %in%
      attr(rows, "notes")
  )
})

test_that("a value that differs between resamples is listed with its count", {
  render <- function(fit_values, ...) {
    w <- writeup_collector()
    cell <- writeup_hp_cell(
      w,
      "table_hp_x",
      fit_values,
      "models[].hyperparameters.x",
      ...
    )
    render_writeup_template(cell, writeup_values_table(w), integer())
  }
  expect_identical(
    render(list(.fit_value(2L), .fit_value(3L), .fit_value(2L))),
    "2 (2); 3 (1)"
  )
  # One value chosen two ways, and a resample in which it had no effect.
  expect_identical(
    render(list(
      .fit_value(20L, "default"),
      .fit_value(1L, "specified"),
      .fit_value(20L, "default"),
      .fit_value(NULL, "unset", applies = FALSE)
    )),
    "20 (2, default); 1 (1, specified); not applicable (1)"
  )
  # Configurations that share a summary are told apart by their settings.
  kfold <- function(n) {
    v <- setup_KFold(n_resamples = n)
    list(
      value = v,
      identity = as.character(jsonlite::toJSON(S7_to_list(v), null = "null")),
      source = "specified",
      applies = TRUE
    )
  }
  configs <- render(list(kfold(2L), kfold(3L)), object_valued = TRUE)
  parts <- strsplit(configs, "; ", fixed = TRUE)[[1L]]
  expect_length(parts, 2L)
  expect_false(identical(parts[[1L]], parts[[2L]]))
  expect_match(parts[[1L]], "\\(1\\)$")
})

test_that("values tried are those the evaluated configurations gave", {
  set.seed(5)
  mod <- train(
    iris,
    hyperparameters = setup_CART(maxdepth = tune_over(1L, 2L, 3L)),
    tuner_config = setup_GridSearch(
      search_type = "randomized",
      randomize_p = 0.5
    ),
    verbosity = 0L
  )
  w <- writeup(mod)
  tried <- strsplit(
    .render_cell(w, .hp_row(w, "maxdepth")[["tried"]]),
    "; ",
    fixed = TRUE
  )[[1L]]
  expect_setequal(
    tried,
    as.character(unique(mod@tuner@tuning_results[["param_grid"]][["maxdepth"]]))
  )
  # A hyperparameter set: the value each evaluated member gives, unset
  # included.
  set_mod <- train(
    iris,
    hyperparameters = list(
      a = setup_CART(prune_cp = NULL),
      b = setup_CART(prune_cp = 0.1)
    ),
    verbosity = 0L
  )
  ws <- writeup(set_mod)
  prune_cp <- .hp_row(ws, "prune_cp")
  expect_setequal(
    strsplit(.render_cell(ws, prune_cp[["tried"]]), "; ", fixed = TRUE)[[1L]],
    c("unset", "0.1")
  )
  expect_true(
    "config.hyperparameters.variants.b.prune_cp" %in%
      .cell_sources(ws, prune_cp[["tried"]])
  )
  # The value is that of the member the fit came from.
  winner <- set_mod@hyperparameters@variant
  expect_identical(
    prune_cp[["source"]],
    if (identical(winner, "a")) "unset" else "specified"
  )
})

test_that("a resampled model's cell counts every resample's value", {
  set.seed(6)
  mod <- train(
    iris,
    hyperparameters = setup_CART(maxdepth = tune_over(1L, 2L, 3L)),
    outer_resampling_config = setup_KFold(n_resamples = 3L),
    verbosity = 0L
  )
  w <- writeup(mod)
  selected <- vapply(
    mod@models,
    function(m) m@hyperparameters[["maxdepth"]],
    integer(1L)
  )
  counts <- table(selected)
  expected <- if (length(counts) == 1L) {
    names(counts)
  } else {
    paste0(names(counts), " (", as.integer(counts), ")")
  }
  rendered <- .render_cell(w, .hp_row(w, "maxdepth")[["value"]])
  expect_setequal(strsplit(rendered, "; ", fixed = TRUE)[[1L]], expected)
})

# Hyperparameter rows of a resampled model whose first fit is altered, and a
# writeup holding them, for the aggregation and rendering cases a single
# search does not produce.
.altered_rows <- function(alter) {
  x <- .mod_res
  ctx <- writeup_context(x, review(x))
  ctx[["fits"]][[1L]]@hyperparameters <- alter(
    ctx[["fits"]][[1L]]@hyperparameters
  )
  w <- writeup_collector()
  rows <- writeup_hyperparameters(ctx, w)
  wu <- .wus[["res"]]
  wu@values <- writeup_values_table(w)
  wu@hyperparameters <- rows
  list(rows = rows, writeup = wu)
}

test_that("a value chosen differently in one resample makes the row varied", {
  out <- .altered_rows(function(hp) {
    hp@cp <- 0.5
    hp
  })
  cp <- out[["rows"]][out[["rows"]][["name"]] == "cp", ]
  expect_identical(cp[["source"]], "varied")
  expect_setequal(
    strsplit(.render_cell(out[["writeup"]], cp[["value"]]), "; ")[[1L]],
    c("0.5 (1, resolved during fitting)", "0.01 (4, default)")
  )
  # An unset value beside a set one: each reads as itself, and the meaning is
  # listed under the table.
  out <- .altered_rows(function(hp) {
    hp@prune_cp <- 0.1
    hp
  })
  rows <- writeup_hp_table_rows(out[["writeup"]], main = FALSE)
  cell <- unname(rows[rows[, 1L] == "prune_cp", 2L])
  expect_setequal(
    strsplit(cell, "; ")[[1L]],
    c("0.1 (1, resolved during fitting)", "unset (4, unset)")
  )
  expect_true(
    paste0("prune_cp: ", unset_meaning(CARTHyperparameters, "prune_cp")) %in%
      attr(rows, "notes")
  )
})

test_that("an unset alternative tuning evaluated has its meaning listed", {
  set.seed(5)
  mod <- train(
    iris,
    hyperparameters = list(
      a = setup_CART(prune_cp = 0.01),
      b = setup_CART(prune_cp = NULL)
    ),
    tuner_config = setup_GridSearch(resampler_config = setup_KFold(2L)),
    execution_config = setup_SerialExecution(seed = 5L),
    verbosity = 0L
  )
  # The selected member sets prune_cp, so only the tried cell holds unset.
  expect_identical(mod@hyperparameters@variant, "a")
  w <- writeup(mod)
  prune_cp <- .hp_row(w, "prune_cp")
  expect_match(.render_cell(w, prune_cp[["tried"]]), "unset", fixed = TRUE)
  expect_true(
    paste0("prune_cp: ", unset_meaning(CARTHyperparameters, "prune_cp")) %in%
      attr(writeup_hp_table_rows(w, main = FALSE), "notes")
  )
})

test_that("one value chosen two ways is listed once per way", {
  w <- writeup_collector()
  cell <- writeup_hp_cell(
    w,
    "table_hp_x",
    list(.fit_value(2L, "default"), .fit_value(2L, "specified")),
    "models[].hyperparameters.x"
  )
  expect_identical(
    render_writeup_template(cell, writeup_values_table(w), integer()),
    "2 (1, default); 2 (1, specified)"
  )
})

test_that("a set's pairing uses the member the fit came from", {
  set.seed(5)
  mod <- train(
    iris,
    hyperparameters = list(a = setup_CART(cp = 1), b = setup_CART(cp = 0.01)),
    tuner_config = setup_GridSearch(resampler_config = setup_KFold(2L)),
    verbosity = 0L
  )
  # cp = 1 grows no split, so b wins.
  expect_identical(mod@hyperparameters@variant, "b")
  w <- writeup(mod)
  # 0.01 is CART's default; compared with member a it would read resolved.
  expect_identical(.hp_row(w, "cp")[["source"]], "default")
  # Without a config, the set is read from the tuner.
  bare <- mod
  bare@config <- NULL
  wb <- writeup(bare)
  expect_true(all(startsWith(
    .cell_sources(wb, .hp_row(wb, "cp")[["tried"]]),
    "tuner.searched_set.variants."
  )))
})

test_that("a set of config-valued hyperparameters is written up", {
  sl <- function(n) {
    setup_SuperLearner(
      base_learners = list(cart = setup_CART(), glm = setup_GLM()),
      meta_learner = setup_NNLS(),
      inner_resampling_config = setup_KFold(n_resamples = n)
    )
  }
  set.seed(8)
  # Few predictors, so the GLM is full rank on the inner folds' cases.
  mod <- train(
    mtcars[, c("wt", "hp", "mpg")],
    hyperparameters = list(two = sl(2L), three = sl(3L)),
    tuner_config = setup_GridSearch(resampler_config = setup_KFold(2L)),
    verbosity = 0L
  )
  w <- writeup(mod)
  tried <- strsplit(
    .render_cell(w, .hp_row(w, "inner_resampling_config")[["tried"]]),
    "; ",
    fixed = TRUE
  )[[1L]]
  # Two configurations that share the summary KFold, told apart by settings.
  expect_length(tried, 2L)
  expect_false(identical(tried[[1L]], tried[[2L]]))
  # The same learners in both members: no values tried.
  expect_identical(.hp_row(w, "base_learners")[["tried"]], "")
  # Grouping in the value cell is by configuration, not by summary: a second
  # fit with the other inner resampler.
  ctx <- writeup_context(mod, review(mod))
  other <- ctx[["fits"]][[1L]]
  n <- other@hyperparameters@inner_resampling_config@n_resamples
  other@hyperparameters@inner_resampling_config <- setup_KFold(
    n_resamples = if (n == 2L) 3L else 2L
  )
  ctx[["fits"]] <- c(ctx[["fits"]], list(other))
  cw <- writeup_collector()
  rows <- writeup_hyperparameters(ctx, cw)
  cell <- render_writeup_template(
    rows[["value"]][rows[["name"]] == "inner_resampling_config"],
    writeup_values_table(cw),
    integer()
  )
  parts <- strsplit(cell, "; ", fixed = TRUE)[[1L]]
  expect_length(parts, 2L)
  expect_false(identical(parts[[1L]], parts[[2L]]))
})

test_that("a tuned unset hidden_units makes the width settings apply", {
  skip_if_not_installed("torch")
  skip_if_not(torch::torch_is_installed())
  mod <- train(
    iris,
    hyperparameters = setup_MLP(
      hidden_units = tune_over(NULL, c(8L, 4L)),
      max_epochs = 1L,
      batch_size = 32L
    ),
    tuner_config = setup_GridSearch(resampler_config = setup_KFold(2L)),
    execution_config = setup_SerialExecution(seed = 5L),
    verbosity = 0L
  )
  w <- writeup(mod)
  widths_generated <- is.null(mod@tuner@best_hyperparameters[["hidden_units"]])
  for (nm in c("shape", "shape_layers", "shape_max_units")) {
    expect_identical(.hp_row(w, nm)[["applies"]], widths_generated, info = nm)
  }
})

test_that("applicability follows the backend's rules as well as the gates", {
  # MLP widths: generated only when hidden_units is unset.
  given <- setup_MLP(hidden_units = c(8L, 4L))
  expect_false(hyperparameter_applies(given, "shape", given@hyperparameters))
  generated <- setup_MLP()
  expect_true(
    hyperparameter_applies(generated, "shape", generated@hyperparameters)
  )
  skip_if_not_installed("lightgbm")
  regression <- train(
    mtcars,
    hyperparameters = setup_LightGBM(force_nrounds = 5L),
    verbosity = 0L
  )
  expect_false(.hp_row(writeup(regression), "sigmoid")[["applies"]])
  expect_true(.hp_row(writeup(regression), "reg_sqrt")[["applies"]])
})

test_that("include_hyperparameters replaces the primary list", {
  w <- writeup(.mod_bin, include_hyperparameters = "minsplit")
  main <- w@hyperparameters[["name"]][w@hyperparameters[["main"]]]
  # The tuned hyperparameter stays; cp, primary by declaration, leaves.
  expect_setequal(main, c("minsplit", "maxdepth"))
  none <- writeup(.mod_bin, include_hyperparameters = character())
  expect_identical(
    none@hyperparameters[["name"]][none@hyperparameters[["main"]]],
    "maxdepth"
  )
  expect_false(none@primary_listed)
  expect_null(none@include_hyperparameters)
  expect_match(
    writeup_hp_table_caption(none),
    "^Table 1\\. Every hyperparameter that was tuned or specified"
  )
  expect_match(
    writeup_hp_table_caption(.wus[["bin"]]),
    "^Table 1\\. The primary hyperparameters of the algorithm"
  )
  expect_error(
    writeup(.mod_bin, include_hyperparameters = "not_a_hyperparameter"),
    "names no hyperparameter",
    class = "rtemis_value_error"
  )
})

test_that("the Markdown carries both hyperparameter tables", {
  path <- tempfile(fileext = ".md")
  write_text(.wus[["bin"]], path, verbosity = 0L)
  md <- readLines(path)
  expect_true(any(grepl("^Table 1\\. The primary hyperparameters", md)))
  expect_true(any(grepl("^Table 2\\. Performance", md)))
  expect_true("## Supplementary material" %in% md)
  expect_true(any(grepl("^Table S1\\.", md)))
  expect_true(any(grepl("^- prune_cp: Unset prunes nothing", md)))
  # The unrecorded items, as print shows them.
  expect_true("## Not recorded by the model" %in% md)
  expect_true(all(
    paste0("- ", .wus[["bin"]]@not_reported) %in% md
  ))
})
