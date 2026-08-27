# test_ChecksArtifact.R
# ::rtemis::
# 2026- EDG rtemis.org

# The published rule set, checked as what shipped rather than as what the
# generator would produce.
#
# `data-raw/` is in `.Rbuildignore`, so the generator and its contract are not
# here to re-run. What is here is `inst/checks/v1/checks.json` itself, which is
# what a second implementation reads -- so a hand edit to it, which is the one
# way the artifact can detach from its source, fails these rather than passing
# silently until a port disagrees.
#
# The artifact is the contract, not the implementation. `R/validate_data.R`
# remains the reference implementation, and rtemis does not evaluate the rules;
# what these assert is that the two describe the same vocabulary and that the
# fixture corpus covers every rule the artifact declares.

.checks_path <- system.file("checks", "v1", "checks.json", package = "rtemis")
.traits_path <- system.file("traits", "v1", "traits.json", package = "rtemis")

skip_if_no_artifact <- function() {
  skip_if(!nzchar(.checks_path), "checks/v1 artifact is not installed")
}


# %% .logic_nodes ----
# Every node of a compiled expression, itself included. A second, deliberately
# minimal copy of what `data-raw/checks_contract.R` walks at generation time:
# that one gates what is written, this one gates what shipped.
.logic_nodes <- function(node) {
  if (!is.list(node)) {
    return(list())
  }
  c(list(node), unlist(lapply(node, .logic_nodes), recursive = FALSE))
}


# %% Structure ----
test_that("the checks artifact declares the sections an evaluator reads", {
  skip_if_no_artifact()
  doc <- jsonlite::fromJSON(.checks_path, simplifyVector = FALSE)
  expect_identical(
    doc[["$id"]],
    "https://schema.rtemis.org/checks/v1/checks.json"
  )
  expect_identical(doc[["expression_language"]], "jsonlogic")
  expect_true(all(
    c("inputs", "evaluation", "let", "rules", "unevaluable") %in% names(doc)
  ))
  expect_gt(length(doc[["let"]]), 0L)
  expect_gt(length(doc[["rules"]]), 0L)
  # Bindings are evaluated in order and a scan is the only array producer, so
  # every entry must say which it is.
  kinds <- vapply(doc[["let"]], `[[`, character(1L), "kind")
  expect_setequal(unique(kinds), c("expr", "scan"))
})


test_that("every rule speaks the published diagnostic vocabulary", {
  skip_if_no_artifact()
  rules <- jsonlite::fromJSON(.checks_path, simplifyVector = FALSE)[["rules"]]
  codes <- vapply(rules, `[[`, character(1L), "code")
  severities <- vapply(rules, `[[`, character(1L), "severity")
  ids <- vapply(rules, `[[`, character(1L), "id")
  expect_true(all(codes %in% DIAGNOSTIC_CODES))
  expect_true(all(severities %in% DIAGNOSTIC_SEVERITIES))
  expect_false(anyDuplicated(ids) > 0L)
  # A rule's id names its code, so a finding can be traced back to the rule
  # that made it without a lookup.
  expect_true(all(startsWith(ids, paste0(codes, "/"))))
  # `plain` is authored per code in `DIAGNOSTIC_PLAIN` and looked up by the
  # renderer. A copy in the artifact would be one per rule, and would drift.
  expect_false(any(vapply(
    rules,
    function(r) "plain" %in% names(r),
    logical(1L)
  )))
})


test_that("no rule uses a JSONLogic semantic that varies between ports", {
  skip_if_no_artifact()
  doc <- jsonlite::fromJSON(.checks_path, simplifyVector = FALSE)
  nodes <- .logic_nodes(doc[["let"]])
  nodes <- c(
    nodes,
    .logic_nodes(doc[["rules"]]),
    .logic_nodes(doc[["unevaluable"]])
  )

  # Loose equality, and the truthiness cast under another name.
  operators <- unlist(lapply(nodes, names))
  expect_false(any(c("==", "!=", "!!") %in% operators))

  # Truthiness itself: a conditional position never holds a bare reference, so
  # nothing is decided by JSONLogic's own notion of falsy.
  bare <- unlist(lapply(nodes, function(node) {
    unlist(lapply(
      intersect(names(node), c("if", "and", "or", "!")),
      function(operator) {
        args <- node[[operator]]
        if (!is.list(args)) {
          return(NULL)
        }
        indices <- seq_along(args)
        positions <- if (identical(operator, "if")) {
          indices[indices %% 2L == 1L & indices < length(args)]
        } else {
          indices
        }
        vapply(
          positions,
          function(i) identical(names(args[[i]]), "var"),
          logical(1L)
        )
      }
    ))
  }))
  expect_false(any(bare))

  # Scope: JSONLogic replaces the data with the current element inside these,
  # so a lambda reaching for a binding or an input resolves to null rather than
  # failing. Iteration that needs outer scope belongs in a `scan`.
  outer <- c("bindings", "profile", "config", "outcome", "traits", "item")
  open <- unlist(lapply(nodes, function(node) {
    scoped <- intersect(
      names(node),
      c("map", "reduce", "filter", "all", "some", "none")
    )
    unlist(lapply(scoped, function(operator) {
      args <- node[[operator]]
      if (!is.list(args) || length(args) < 2L) {
        return(NULL)
      }
      vapply(
        .logic_nodes(args[[2L]]),
        function(sub) {
          path <- sub[["var"]]
          is.character(path) &&
            length(path) == 1L &&
            strsplit(path, ".", fixed = TRUE)[[1L]][[1L]] %in% outer
        },
        logical(1L)
      )
    }))
  }))
  expect_false(any(open))
})


test_that("every message slot names a value the rule carries", {
  skip_if_no_artifact()
  rules <- jsonlite::fromJSON(.checks_path, simplifyVector = FALSE)[["rules"]]
  pattern <- "\\{([A-Za-z_][A-Za-z0-9_]*)(\\|[^{}|]*\\|[^{}|]*)?\\}"
  for (r in rules) {
    templates <- c(
      r[["message"]],
      vapply(r[["fix"]][["patch"]], `[[`, character(1L), "path")
    )
    found <- unlist(regmatches(templates, gregexpr(pattern, templates)))
    slots <- sub("^\\{([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", found)
    expect_setequal(
      setdiff(slots, c(names(r[["evidence"]]), names(r[["slots"]]))),
      character()
    )
  }
})


# %% Traits ----
test_that("the traits artifact answers for every supervised algorithm", {
  skip_if_no_artifact()
  traits <- jsonlite::fromJSON(.traits_path, simplifyVector = FALSE)[["traits"]]
  expect_setequal(
    vapply(traits, `[[`, character(1L), "name"),
    supervised_algorithms[["name"]]
  )
  # Three-valued, and null is not "unknown": it is the answer belonging to the
  # algorithm's base learners rather than to the algorithm. A rule tests it
  # with `=== false`, so the distinction has to survive serialization.
  for (t in traits) {
    for (trait in c("class", "reg", "surv", "missing", "p_gt_n")) {
      expect_true(
        is.null(t[[trait]]) || is.logical(t[[trait]]),
        info = paste(t[["name"]], trait)
      )
    }
  }
})


# %% Corpus coverage ----
test_that("every rule in the artifact has a fixture", {
  skip_if_no_artifact()
  rules <- jsonlite::fromJSON(.checks_path, simplifyVector = FALSE)[["rules"]]
  ids <- vapply(rules, `[[`, character(1L), "id")

  # Read from the corpus itself, as "every code has a fixture" already does. A
  # rule added without a fixture fails here, so the corpus cannot fall behind
  # the rule set.
  src <- readLines(test_path("test_ValidateConfigFixtures.R"))
  marked <- trimws(sub("^.*# rule:", "", grep("# rule:", src, value = TRUE)))

  # Two rules have no reachable fixture, and the reason is checked rather than
  # asserted below: `declared_task()` reads an algorithm that performs one task
  # only, and no algorithm in the table currently does.
  unreachable <- c(
    "OUTCOME_TYPE_MISMATCH/algorithm-classifies-only",
    "OUTCOME_TYPE_MISMATCH/algorithm-regresses-only"
  )
  expect_true(all(unreachable %in% ids))
  expect_setequal(marked, setdiff(ids, unreachable))
  expect_false(anyDuplicated(setdiff(marked, marked[duplicated(marked)])) > 0L)
})


test_that("the unreachable rules are unreachable for the stated reason", {
  # The exemption above holds only while every algorithm performs both tasks.
  # Adding a classification-only or regression-only algorithm makes those two
  # rules reachable, and this fails until they have fixtures.
  one_sided <- xor(
    as.logical(supervised_algorithms[["class"]]),
    as.logical(supervised_algorithms[["reg"]])
  )
  expect_false(
    any(one_sided, na.rm = TRUE),
    info = paste(
      "one-sided algorithms now exist:",
      paste(supervised_algorithms[["name"]][which(one_sided)], collapse = ", ")
    )
  )
})


# %% The compiler's bans ----
# `data-raw/` is not shipped, so these run from a source checkout and skip
# everywhere else. They are worth having there: the whole port-safety argument
# rests on two things being *unwritable* rather than merely discouraged, and a
# claim like that is only true while something tries to write them.
.compiler_path <- testthat::test_path(
  "..",
  "..",
  "data-raw",
  "checks_compile.R"
)

skip_if_no_compiler <- function() {
  skip_if(!file.exists(.compiler_path), "data-raw/ is not in this build")
  sys.source(.compiler_path, envir = parent.frame())
}


test_that("loose equality cannot be written", {
  skip_if_no_compiler()
  env <- list(flag = "boolean", n = "number", .item = FALSE)
  # Not rejected after parsing: absent from the grammar, so the tokenizer names
  # the replacement rather than the parser reporting a syntax error.
  expect_error(checks_compile("n == 1", env, "t"), "loose equality")
  expect_error(checks_compile("n != 1", env, "t"), "loose equality")
  expect_no_error(checks_compile("n === 1", env, "t"))
  expect_no_error(checks_compile("n !== 1", env, "t"))
})


test_that("a conditional position must be provably boolean", {
  skip_if_no_compiler()
  env <- list(flag = "boolean", n = "number", .item = FALSE)
  # A reference into an input document infers as `any`, and `any` is not
  # boolean. That is the whole mechanism by which truthiness is unreachable.
  expect_error(
    checks_compile("if config.impute then 1 else 2", env, "t"),
    "truthiness"
  )
  expect_error(checks_compile("config.impute and flag", env, "t"), "truthiness")
  expect_error(checks_compile("not config.impute", env, "t"), "truthiness")
  expect_no_error(checks_compile("config.impute === true and flag", env, "t"))
  # A guard that is a bare reference is only accepted where it is already
  # boolean; comparing a number to `true` would hide a mistake rather than fix
  # one.
  expect_error(checks_compile_boolean("n", env, "a guard"), "boolean")
  expect_no_error(checks_compile_boolean("flag", env, "a guard"))
})


test_that("a reference must name a binding or an input", {
  skip_if_no_compiler()
  expect_error(
    checks_compile("profil.n_rows > 1", list(flag = "boolean"), "t"),
    "neither"
  )
  # `item` exists only where an iteration puts it there.
  expect_error(
    checks_compile("item.n > 1", list(flag = "boolean"), "t"),
    "only in scope"
  )
  expect_no_error(
    checks_compile("item.n > 1", list(flag = "boolean", .item = TRUE), "t")
  )
})


test_that("round_half_even reproduces R's rounding, tie rule included", {
  skip_if_no_compiler()
  # The emitted expansion, evaluated in R. `%%` is a remainder and so exact at
  # these magnitudes, which is what makes the `=== 0.5` test true precisely at
  # a tie -- and the tie rule decides a real finding: at 5 rows and train_p 0.9
  # it is the difference between a warning and an error.
  emulate <- function(x) {
    frac <- x %% 1
    floor_x <- x - frac
    floor_x + if (frac > 0.5 || (frac == 0.5 && (floor_x %% 2) == 1)) 1 else 0
  }
  grid <- expand.grid(
    n = 0:120,
    p = c(0.05, 0.25, 1 / 3, 0.5, 0.6, 2 / 3, 0.7, 0.75, 0.9, 0.95, 0.99)
  )
  values <- grid[["n"]] * grid[["p"]]
  expect_identical(
    vapply(values, emulate, numeric(1L)),
    as.numeric(round(values))
  )
})
