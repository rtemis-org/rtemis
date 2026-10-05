# test_BackendArguments.R
# ::rtemis::
# 2026- EDG rtemis.org

# A hyperparameter a class declares reaches its backend: each case compares a
# fit with the hyperparameter set against one without it.

.bin <- iris[51:150, ]
.bin[["Species"]] <- droplevels(.bin[["Species"]])


test_that("CART passes feature costs to rpart", {
  skip_if_not_installed("rpart")
  fit <- function(...) {
    train(
      .bin,
      hyperparameters = setup_CART(maxdepth = 1L, ...),
      verbosity = 0L
    )
  }
  # The root split's feature changes when its cost rises.
  root_split <- function(model) {
    as.character(model@model[["frame"]][["var"]][[1L]])
  }
  unit <- fit()
  costly <- stats::setNames(rep(1, 4L), names(.bin)[1:4])
  costly[[root_split(unit)]] <- 100
  expect_false(identical(
    root_split(unit),
    root_split(fit(cost = unname(costly)))
  ))
  expect_identical(
    unit@model[["frame"]],
    fit(cost = rep(1, 4L))@model[["frame"]]
  )
})


test_that("Ranger passes class weights to ranger in the outcome's level order", {
  skip_if_not_installed("ranger")
  # Overlapping, imbalanced classes, where weighting a class moves the splits.
  set.seed(2)
  dat <- data.frame(a = stats::rnorm(300L), b = stats::rnorm(300L))
  dat[["y"]] <- factor(ifelse(
    dat[["a"]] + stats::rnorm(300L, sd = 2) > 1.5,
    "pos",
    "neg"
  ))
  fit <- function(...) {
    train(
      dat,
      hyperparameters = setup_Ranger(num_trees = 50L, seed = 1L, ...),
      verbosity = 0L
    )@predicted_prob_training
  }
  levels <- levels(dat[["y"]])
  equal <- fit()
  weighted <- fit(class_weights = c(1, 20))
  expect_false(identical(equal, weighted))
  # Names place each weight on its class, whatever their order.
  expect_identical(
    fit(class_weights = stats::setNames(c(20, 1), rev(levels))),
    weighted
  )
  # The weights ranger receives, in level order.
  received <- NULL
  ranger_fit <- ranger::ranger
  local_mocked_bindings(
    ranger = function(...) {
      received <<- list(...)[["class.weights"]]
      ranger_fit(...)
    },
    .package = "ranger"
  )
  fit(class_weights = stats::setNames(c(20, 1), rev(levels)))
  expect_identical(received, stats::setNames(c(1, 20), levels))
  expect_error(
    fit(class_weights = c(a = 1, b = 20)),
    "names must be the outcome's levels",
    class = "rtemis_value_error"
  )
})
