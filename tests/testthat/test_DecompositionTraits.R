# test_DecompositionTraits.R
# ::rtemis::
# 2026- EDG rtemis.org

# The traits table states facts about each decomposition algorithm that the
# metrics layer reads to decide which metrics apply. A trait that disagrees
# with the code silently gives an algorithm the wrong metrics, so the two
# columns that correspond to a dispatched method are asserted equal to the
# existence of that method, in both directions.

traits <- decomposition_traits()

config_class <- function(algorithm) {
  get(paste0(algorithm, "Config"), envir = asNamespace("rtemis"))
}

has_method <- function(generic, algorithm) {
  tryCatch(
    {
      S7::method(generic, config_class(algorithm))
      TRUE
    },
    error = function(e) FALSE
  )
}

# Every S7 class in the package that descends from `DecompositionConfig`, so a
# new algorithm cannot be added without a traits row.
decomposition_config_classes <- function() {
  ns <- asNamespace("rtemis")
  Filter(
    function(nm) {
      obj <- get(nm, envir = ns)
      if (!inherits(obj, "S7_class")) {
        return(FALSE)
      }
      parent <- obj@parent
      while (inherits(parent, "S7_class")) {
        if (identical(parent@name, "DecompositionConfig")) {
          return(TRUE)
        }
        parent <- parent@parent
      }
      FALSE
    },
    ls(ns, all.names = TRUE)
  )
}


# Table shape ----
test_that("decom_algorithms is typed and well-formed", {
  expect_s3_class(traits, "data.frame")
  expect_identical(anyDuplicated(traits[["name"]]), 0L)
  for (column in c(
    "linear",
    "can_apply",
    "invertible",
    "orthogonal",
    "ordered",
    "deterministic",
    "nonneg"
  )) {
    expect_type(traits[[column]], "logical")
    expect_false(anyNA(traits[[column]]))
  }
  for (column in c("name", "description", "preserves", "package")) {
    expect_type(traits[[column]], "character")
  }
  expect_true(all(
    traits[["preserves"]] %in%
      c("variance", "global", "local", "reconstruction")
  ))
})


test_that("decomposition_traits() returns the whole table or one row", {
  expect_identical(nrow(decomposition_traits("PCA")), 1L)
  # Matched case-insensitively, like every other algorithm name in the API.
  expect_identical(decomposition_traits("pca"), decomposition_traits("PCA"))
  expect_error(
    decomposition_traits("NoSuchAlgorithm"),
    class = "rtemis_value_error"
  )
})


# Registration ----
test_that("every registered algorithm has a setup_ function and decomp_ method", {
  for (algorithm in traits[["name"]]) {
    expect_true(
      exists(paste0("setup_", algorithm), envir = asNamespace("rtemis")),
      info = algorithm
    )
    expect_true(has_method(decomp_, algorithm), info = algorithm)
  }
})


test_that("every DecompositionConfig subclass has a traits row", {
  registered <- paste0(traits[["name"]], "Config")
  expect_setequal(decomposition_config_classes(), registered)
})


# Drift guards ----
test_that("can_apply is TRUE exactly when an apply_decomp_ method exists", {
  for (i in seq_len(nrow(traits))) {
    algorithm <- traits[["name"]][i]
    expect_identical(
      has_method(apply_decomp_, algorithm),
      traits[["can_apply"]][i],
      info = algorithm
    )
  }
})


test_that("invertible is TRUE exactly when a reconstruct_ method exists", {
  for (i in seq_len(nrow(traits))) {
    algorithm <- traits[["name"]][i]
    expect_identical(
      has_method(reconstruct_, algorithm),
      traits[["invertible"]][i],
      info = algorithm
    )
  }
})


test_that("decom_algorithms_invertible is derived from invertible", {
  expect_identical(
    decom_algorithms_invertible,
    traits[["name"]][traits[["invertible"]]]
  )
})


test_that("decom_algorithms_applicable is derived from can_apply", {
  expect_identical(
    decom_algorithms_applicable,
    traits[["name"]][traits[["can_apply"]]]
  )
  for (algorithm in traits[["name"]]) {
    expect_identical(
      decom_can_apply(algorithm),
      traits[["can_apply"]][traits[["name"]] == algorithm],
      info = algorithm
    )
  }
})
