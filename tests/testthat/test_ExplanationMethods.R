# test_ExplanationMethods.R
# ::rtemis::
# 2026- EDG rtemis.org

# The applicability table is what `explain()` dispatches on: the estimator
# cannot be read off the fitted object's class, because one backend class can
# back two algorithms warranting different estimators (LinearSVM and RadialSVM
# are both `e1071::svm`). A row that disagrees with the code therefore explains
# a model by the wrong method rather than failing, so the table's agreement with
# the rest of the package is asserted here in both directions.

methods_table <- explanation_methods()


# %% Table shape ----
test_that("explanation_algorithms is typed and well-formed", {
  expect_s3_class(methods_table, "data.frame")
  expect_identical(anyDuplicated(methods_table[["name"]]), 0L)
  expect_type(methods_table[["name"]], "character")
  expect_type(methods_table[["estimator"]], "character")
  expect_type(methods_table[["rationale"]], "character")
  # NA means exactness is a property of the *fitted* model rather than of the
  # algorithm: two with a single linear map for a regression or binary outcome
  # but none for a multiclass one, three additive only when the fit selected no
  # term reading two features, and one that is exact when the experts it routed
  # to are. Listed so a new NA has to be added here on purpose rather than
  # standing in for an unauthored row.
  expect_type(methods_table[["exact"]], "logical")
  expect_setequal(
    methods_table[["name"]][is.na(methods_table[["exact"]])],
    c(
      "SPLS",
      "LinearSVM",
      "MARS",
      "HAL",
      "MonotonicHAL",
      # Exact when every expert it routed to is; the experts are the fit.
      "ConditionalSuperLearner"
    )
  )
  expect_true(all(nzchar(methods_table[["rationale"]])))
})


test_that("every estimator and value function is one the package declares", {
  expect_true(all(methods_table[["estimator"]] %in% SHAP_RESOLVED_ESTIMATORS))
  expect_true(all(methods_table[["perturbation"]] %in% SHAP_PERTURBATIONS))
  expect_false(anyNA(methods_table[["perturbation"]]))
})


test_that("perturbation is derived from the estimator, not restated", {
  # Two columns that could disagree would be two sources of truth; this one is
  # computed from `SHAP_ESTIMATOR_PERTURBATION` so it cannot.
  expect_identical(
    methods_table[["perturbation"]],
    unname(SHAP_ESTIMATOR_PERTURBATION[methods_table[["estimator"]]])
  )
})


test_that("one estimator name means one value function", {
  # `TreeSHAP` covers CART and the LightGBM family. If those defaulted to
  # different value functions, two results reading `estimator = "TreeSHAP"`
  # would not be answering the same question -- the silent incomparability the
  # perturbation decision exists to prevent.
  by_estimator <- split(
    methods_table[["perturbation"]],
    methods_table[["estimator"]]
  )
  for (estimator in names(by_estimator)) {
    expect_length(unique(by_estimator[[estimator]]), 1L)
  }
})


# %% Registration ----
test_that("every registered algorithm has exactly one row, and no others do", {
  # `explain()` promises an answer for every algorithm rtemis can train, so the
  # table and the roster are the same set. A new algorithm that forgets its row
  # fails here rather than erroring in a user's hands.
  expect_setequal(methods_table[["name"]], supervised_algorithms[["name"]])
  expect_identical(nrow(methods_table), nrow(supervised_algorithms))
})


test_that("explanation_methods() returns the whole table or one row", {
  expect_identical(nrow(explanation_methods("Ranger")), 1L)
  # Matched case-insensitively, like every other algorithm name in the API.
  expect_identical(
    explanation_methods("ranger"),
    explanation_methods("Ranger")
  )
  expect_error(
    explanation_methods("NoSuchAlgorithm"),
    class = "rtemis_value_error"
  )
})


# %% Contents ----
test_that("the exact tier is the algorithms with additive structure to read", {
  exact <- methods_table[["name"]][
    !is.na(methods_table[["exact"]]) & methods_table[["exact"]]
  ]
  # Linear models, additive models, and trees: each carries a decomposition the
  # estimator reads rather than samples, for every fit of it.
  expect_true(all(
    c("GLM", "GLMNET", "NNLS", "LightGBM", "CART", "GAM") %in% exact
  ))
  # A network, a kernel machine and a neighborhood method carry none, ever.
  expect_false(any(c("MLP", "RadialSVM", "KNN", "TabNet") %in% exact))
  # MARS and HAL carry one for some fits and not others, which is what NA says:
  # a term reading two features cannot be split by summing, and both default to
  # a search that allows one.
  expect_true(all(is.na(
    explanation_methods()[["exact"]][
      explanation_methods()[["name"]] %in% c("MARS", "HAL", "MonotonicHAL")
    ]
  )))
  # Unconditional exactness is the minority, which is the honest shape of this.
  expect_lt(length(exact), nrow(methods_table) / 2)
})


test_that("the two SVMs get different estimators despite one backend class", {
  # The reason the table dispatches rather than the fitted object's class:
  # both are `e1071::svm`, and `explain_super.class_svm` is one method.
  expect_identical(
    explanation_methods("LinearSVM")[["estimator"]],
    "LinearSHAP"
  )
  expect_identical(
    explanation_methods("RadialSVM")[["estimator"]],
    "KernelSHAP"
  )
})


test_that("KernelSHAP is the fallback, so no algorithm is left unexplained", {
  # `exact = FALSE` says `estimator = "exact"` has nothing to offer, not that
  # the algorithm cannot be explained.
  inexact <- which(!is.na(methods_table[["exact"]]) & !methods_table[["exact"]])
  expect_gt(length(inexact), 0L)
  expect_identical(
    unique(methods_table[["estimator"]][inexact]),
    "KernelSHAP"
  )
})
