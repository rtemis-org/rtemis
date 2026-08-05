# test_GenericArgForcing.R
# ::rtemis::
# 2026- EDG rtemis.org

# An S7 generic declared with explicit formals must force each supplied
# argument before dispatching.
#
# S7 inlines every named formal into the method call as a promise. An argument
# whose first force happens inside the method, and which raises there, leaves
# that promise flagged under evaluation -- and anything that later walks the
# stack and touches it reports "promise already under evaluation" instead of
# the real error. `rlang::trace_back()` does exactly that, via
# `call_zap_inline()`, so every testthat failure would hit it.
#
# The check is generic over the namespace rather than a maintained list, so a
# generic written later is covered the day it is added.

# %% .explicit_generics ----
# Every S7 generic in the package declaring formals beyond its dispatch
# argument(s) and `...`, with those extra formals. A generic taking only `...`
# cannot fail this way: its sole named formal is the dispatch argument, which
# S7 has already forced to select the method.
.explicit_generics <- function() {
  ns <- asNamespace("rtemis")
  out <- list()
  for (nm in ls(ns, all.names = TRUE)) {
    obj <- get(nm, envir = ns)
    if (!inherits(obj, "S7_generic")) {
      next
    }
    extra <- setdiff(names(formals(obj)), c("...", attr(obj, "dispatch_args")))
    if (length(extra) > 0L) {
      out[[nm]] <- extra
    }
  }
  out
}


# %% .forced_names ----
# Names appearing as `force(<name>)` anywhere in a function body.
.forced_names <- function(fn) {
  found <- character()
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (
      identical(e[[1L]], quote(force)) &&
        length(e) >= 2L &&
        is.symbol(e[[2L]])
    ) {
      found <<- c(found, as.character(e[[2L]]))
    }
    lst <- as.list(e)
    for (i in seq_along(lst)) {
      if (identical(lst[[i]], quote(expr = ))) {
        next
      }
      if (is.call(lst[[i]])) {
        walk(lst[[i]])
      }
    }
  }
  walk(body(fn))
  unique(found)
}


test_that("every explicit-formals generic forces its supplied arguments", {
  # Read from the generic's body rather than by raising an error through each
  # one: that would need a dispatchable fixture per generic, and what has to be
  # true is a property of the body.
  generics <- .explicit_generics()
  expect_gt(length(generics), 0L)
  for (nm in names(generics)) {
    unforced <- setdiff(
      generics[[nm]],
      .forced_names(get(nm, envir = asNamespace("rtemis")))
    )
    expect_identical(
      unforced,
      character(),
      info = paste0(
        nm,
        "(): add `if (!missing(x)) force(x)` before S7_dispatch() for: ",
        paste(unforced, collapse = ", ")
      )
    )
  }
})


test_that("a failing argument reports its own error, not a promise error", {
  # The behavior the forcing exists for, end to end on the case that exposed
  # it: `newdata` still carries the outcome column, so it has one column too
  # many, and that rejection must be what surfaces.
  x <- data.table::data.table(a = rnorm(60L), b = rnorm(60L))
  x[, y := a + b + rnorm(60L)]
  mod <- train(x = x, hyperparameters = setup_GLM(), verbosity = 0L)
  expect_error(
    predict(mod, x),
    "Predictor names and order in newdata must exactly match"
  )
})


test_that("the stack stays walkable while such an error propagates", {
  # The direct statement of the invariant. `rlang::trace_back()` runs on every
  # testthat failure and forces any promise inlined into a dispatch frame, so a
  # half-forced argument shows up here.
  skip_if_not_installed("rlang")
  x <- data.table::data.table(a = rnorm(60L), b = rnorm(60L))
  x[, y := a + b + rnorm(60L)]
  mod <- train(x = x, hyperparameters = setup_GLM(), verbosity = 0L)
  walk_error <- NULL
  tryCatch(
    withCallingHandlers(
      predict(mod, x),
      error = function(e) {
        walk_error <<- tryCatch(
          {
            rlang::trace_back()
            NULL
          },
          error = function(inner) conditionMessage(inner)
        )
      }
    ),
    error = function(e) NULL
  )
  expect_null(walk_error)
})
