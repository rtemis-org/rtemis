# result-rule-cases.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% result_rule_cases ----
#' Build independent boundary cases for portable supervised result relations
#' @return List of native baselines, mutations, and expected rule identifiers.
#' @keywords internal
#' @noRd
result_rule_cases <- function() {
  execution <- setup_SerialExecution(seed = 21L)
  regression <- Regression(
    algorithm = "GLM",
    execution_config = execution,
    hyperparameters = setup_GLM(),
    y_training = c(1, 2, 3, 4),
    predicted_training = c(1, 2, 3, 4),
    xnames = "x"
  )
  classify <- function(lev) {
    y <- factor(rep(lev, length.out = 4L), levels = lev)
    Classification(
      algorithm = "KNN",
      execution_config = execution,
      hyperparameters = setup_KNN(),
      y_training = y,
      predicted_training = y,
      xnames = "x",
      predicted_prob_training = matrix(
        1 / length(lev),
        4L,
        if (length(lev) == 2L) 1L else length(lev)
      )
    )
  }
  bases <- list(
    regression = regression,
    multiclass = classify(c("a", "b", "c")),
    binary = classify(c("b", "a"))
  )
  for (kind in names(bases)) {
    for (sample in c("validation", "test")) {
      changes <- list(
        bases[[kind]]@y_training,
        bases[[kind]]@predicted_training
      )
      names(changes) <- paste0(c("y_", "predicted_"), sample)
      if (kind != "regression") {
        changes[[paste0("predicted_prob_", sample)]] <- bases[[
          kind
        ]]@predicted_prob_training
      }
      props(bases[[kind]]) <- changes
    }
  }
  cases <- list()
  add <- function(id, kind, changes = list(), ids = character()) {
    cases[[length(cases) + 1L]] <<- list(
      id = id,
      base = bases[[kind]],
      changes = changes,
      ids = sort(ids)
    )
  }
  ref <- function(layout, columns, lev = NULL, rows = 4L) {
    DataRef(
      path = "unopened.parquet",
      hash = "not-read-by-semantic-validation",
      bytes = 0,
      n_rows = rows,
      n_cols = 7L,
      layout = layout,
      columns = columns,
      levels = lev
    )
  }
  for (kind in names(bases)) {
    add(paste0(kind, ".valid"), kind)
    for (sample in c("training", "validation", "test")) {
      predicted <- paste0("predicted_", sample)
      observed <- paste0("y_", sample)
      probability <- paste0("predicted_prob_", sample)
      ids <- paste0("supervised.rows.", sample)
      if (kind != "regression") {
        ids <- c(
          ids,
          paste0("classification.rows.", probability, ".", predicted)
        )
      }
      add(
        paste(kind, sample, "prediction-rows", sep = "."),
        kind,
        setNames(list(prop(bases[[kind]], predicted)[1:3]), predicted),
        ids
      )
      if (kind == "regression") {
        next
      }
      add(
        paste(kind, sample, "probability-rows", sep = "."),
        kind,
        setNames(
          list(prop(bases[[kind]], probability)[1:3, , drop = FALSE]),
          probability
        ),
        paste0("classification.rows.", probability, ".", c(observed, predicted))
      )
      add(
        paste(kind, sample, "probability-width", sep = "."),
        kind,
        setNames(list(matrix(0.5, 4L, 2L)), probability),
        paste0("classification.columns.", sample)
      )
      add(
        paste(kind, sample, "null-probability", sep = "."),
        kind,
        setNames(list(NULL), probability)
      )
      add(
        paste(kind, sample, "prediction-level-order", sep = "."),
        kind,
        setNames(
          list(factor(
            prop(bases[[kind]], predicted),
            levels = rev(levels(bases[[kind]]@y_training))
          )),
          predicted
        ),
        paste0("supervised.levels.", predicted)
      )
      if (sample != "training") {
        add(
          paste(kind, sample, "outcome-level-order", sep = "."),
          kind,
          setNames(
            list(factor(
              prop(bases[[kind]], observed),
              levels = rev(levels(bases[[kind]]@y_training))
            )),
            observed
          ),
          paste0("supervised.levels.", observed)
        )
      }
    }
  }
  add(
    "regression.missing-cells",
    "regression",
    list(predicted_training = c(1, NA_real_, 3, 4))
  )
  add(
    "classification.missing-cells",
    "multiclass",
    list(
      predicted_training = factor(
        c("a", NA, "c", "a"),
        levels = c("a", "b", "c")
      ),
      predicted_prob_training = matrix(NA_real_, 4L, 3L)
    )
  )
  add(
    "classification.unused-level",
    "multiclass",
    list(
      y_training = factor(rep("a", 4), levels = c("a", "b", "c")),
      predicted_training = factor(rep("a", 4), levels = c("a", "b", "c"))
    )
  )
  add(
    "classification.changed-dictionary",
    "multiclass",
    list(predicted_training = factor(rep("d", 4), levels = c("a", "b", "d"))),
    "supervised.levels.predicted_training"
  )
  add(
    "classification.absent-outcome-keeps-prediction-check",
    "multiclass",
    list(y_test = NULL, predicted_prob_test = matrix(1 / 3, 3L, 3L)),
    "classification.rows.predicted_prob_test.predicted_test"
  )
  add(
    "regression.external",
    "regression",
    list(predicted_training = ref("array", "v0"))
  )
  add(
    "regression.external-rows",
    "regression",
    list(predicted_training = ref("array", "v0", rows = 3L)),
    "supervised.rows.training"
  )
  add(
    "classification.external-selected-columns",
    "multiclass",
    list(predicted_prob_training = ref("matrix", c("v2", "v0", "v6")))
  )
  add(
    "classification.external-width",
    "multiclass",
    list(predicted_prob_training = ref("matrix", c("v2", "v0"))),
    "classification.columns.training"
  )
  add(
    "classification.external-rows",
    "multiclass",
    list(
      predicted_prob_training = ref("matrix", c("v2", "v0", "v6"), rows = 3L)
    ),
    paste0(
      "classification.rows.predicted_prob_training.",
      c("y_training", "predicted_training")
    )
  )
  add(
    "classification.external-factor",
    "multiclass",
    list(y_training = ref("factor", "v0", c("a", "b", "c")))
  )
  add(
    "classification.external-levels",
    "multiclass",
    list(predicted_training = ref("factor", "v0", c("b", "a", "c"))),
    "supervised.levels.predicted_training"
  )
  add(
    "classification.both-external",
    "multiclass",
    list(
      y_training = ref("factor", "v0", c("a", "b", "c")),
      predicted_training = ref("factor", "v0", c("a", "b", "c")),
      predicted_prob_training = ref("matrix", c("v2", "v0", "v6"))
    )
  )
  cases
}


# %% result_rule_probe ----
#' Construct a property-valid value without the class's relational validators
#' @param case Named list: A result-rule fixture.
#' @return S7 object with the source property's exact declarations.
#' @keywords internal
#' @noRd
result_rule_probe <- function(case) {
  cls <- S7_class(case[["base"]])
  probe <- new_class(paste0("Probe", cls@name), properties = cls@properties)
  do.call(
    probe,
    utils::modifyList(
      props(case[["base"]]),
      case[["changes"]],
      keep.null = TRUE
    )
  )
}
