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
# the real error. Capturing a backtrace does exactly that, so every testthat
# failure would hit it.
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


# %% .calls_force_supplied ----
# Whether a function body contains a `force_supplied()` call.
.calls_force_supplied <- function(fn) {
  found <- FALSE
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    if (identical(e[[1L]], quote(force_supplied))) {
      found <<- TRUE
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
  found
}


test_that("every explicit-formals generic forces its supplied arguments", {
  # Read from the generic's body rather than by raising an error through each
  # one: that would need a dispatchable fixture per generic, and what has to be
  # true is a property of the body.
  generics <- .explicit_generics()
  expect_gt(length(generics), 0L)
  for (nm in names(generics)) {
    expect_true(
      .calls_force_supplied(get(nm, envir = asNamespace("rtemis"))),
      info = paste0(
        nm,
        "(): declares formals beyond its dispatch argument(s) (",
        paste(generics[[nm]], collapse = ", "),
        ") and must call force_supplied() before S7_dispatch()."
      )
    )
  }
})


test_that("force_supplied forces exactly what the caller named", {
  # An unsupplied default must stay unevaluated: forcing it would do work
  # nothing asked for, and would turn an omitted required formal into
  # "argument is missing, with no default" before a method can say better.
  built <- 0L
  f <- function(
    a,
    b = {
      built <<- built + 1L
      "default"
    },
    c
  ) {
    force_supplied()
    "returned"
  }
  expect_identical(f(a = stop_on_force <- 1L), "returned")
  expect_identical(built, 0L)
  # A supplied argument is forced, so its failure surfaces here.
  expect_error(f(a = stop("supplied and fallible")), "supplied and fallible")
  expect_identical(built, 0L)
})


test_that("a failing argument reports its own error, not a promise error", {
  # The behavior the forcing exists for, end to end on the case that exposed
  # it: `newdata` still carries the outcome column, so it has one column too
  # many, and that rejection must be what surfaces.
  #
  # `expect_error()` is the whole apparatus this needs. Catching a condition
  # makes testthat capture a backtrace for it, and capturing a backtrace forces
  # any promise inlined into a dispatch frame -- so reaching the message at all
  # requires the stack to be walkable. With the forcing removed, this line does
  # not fail on the regexp; it errors with "promise already under evaluation".
  # Asserting that separately, by walking the stack directly, would only be
  # re-testing the backtrace library.
  x <- data.table::data.table(a = rnorm(60L), b = rnorm(60L))
  x[, y := a + b + rnorm(60L)]
  mod <- train(x = x, hyperparameters = setup_GLM(), verbosity = 0L)
  expect_error(
    predict(mod, x),
    "Predictor names and order in newdata must exactly match"
  )
})
