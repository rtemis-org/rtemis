# checks_contract.R
# ::rtemis::
# 2026- EDG rtemis.org

# Enforces the authoring rules at the point the rule set is produced. Sourced
# by `generate_checks.R`, which calls `assert_checks_contract()` on the built
# document before writing it, so a violating rule set cannot be generated.
#
# The same arrangement `schema_contract.R` has with the schemas, and for the
# same reason: a rule that is checked only by review is checked once.
#
# Most of the contract is upstream of here -- `==` is not in the grammar, and
# the type check refuses a non-boolean guard -- so these are the assertions
# that need the *finished* document to make:
#
# 1. No loose equality and no truthiness cast survive into the artifact. The
#    grammar makes them unwritable; this makes them unemittable, so a bug in
#    the emitter cannot reintroduce what the grammar excludes.
# 2. Every stock array lambda is closed over its element. JSONLogic replaces
#    the data inside `map` / `reduce` / `filter` / `all` / `some` / `none`, so a
#    lambda referencing an input or a binding does not fail -- it silently
#    resolves to null. Iteration that needs outer scope belongs in a `scan`.
# 3. Every `{slot}` in a message or a patch path names an evidence key the same
#    rule declares. A slot that names nothing renders as itself.
# 4. Codes and severities come from the published vocabulary, and no rule
#    carries `plain`: that text is authored per code in `DIAGNOSTIC_PLAIN` and
#    looked up by the renderer, never generated and never duplicated here.

# %% Banned operators ----
# The two JSONLogic semantics that vary between ports, plus the truthiness cast
# that is the second one under another name.
CHECKS_BANNED_OPS <- c("==", "!=", "!!")

# Where an operand is decided as a condition. JSONLogic reads a non-boolean
# there by truthiness -- empty arrays falsy, the string "0" truthy -- which is
# its own spec but not every port's.
CHECKS_CONDITION_OPS <- c("if", "and", "or", "!")

# Where a lambda's scope is the element rather than the data.
CHECKS_SCOPED_OPS <- c("map", "reduce", "filter", "all", "some", "none")

# Roots that exist only in the outer data. A `var` on one of these inside a
# scoped lambda resolves to null at evaluation time.
CHECKS_OUTER_ROOTS <- c(
  "bindings",
  "profile",
  "config",
  "outcome",
  "traits",
  "item"
)


# %% .logic_nodes ----
# Every node of a compiled expression, itself included, as `list(path, node)`.
.logic_nodes <- function(node, path = character()) {
  if (!is.list(node)) {
    return(list())
  }
  out <- list(list(path = path, node = node))
  keys <- names(node)
  for (i in seq_along(node)) {
    key <- if (is.null(keys)) as.character(i) else keys[[i]]
    out <- c(out, .logic_nodes(node[[i]], c(path, key)))
  }
  out
} # /.logic_nodes


# %% .banned_operators ----
.banned_operators <- function(logic) {
  found <- vapply(
    .logic_nodes(logic),
    function(sub) {
      keys <- names(sub[["node"]])
      if (is.null(keys)) {
        ""
      } else {
        paste(intersect(keys, CHECKS_BANNED_OPS), collapse = ",")
      }
    },
    character(1L)
  )
  unique(unlist(strsplit(found[nzchar(found)], ",", fixed = TRUE)))
} # /.banned_operators


# %% .bare_conditions ----
# Conditional positions holding a bare reference. The compiler wraps these as
# `x === true`; asserting it here is what makes that structural rather than a
# convention, so reading the artifact is enough to see that no truthiness
# decides anything.
#
# `if` takes its conditions at the odd positions of a 3+ chain; `and`, `or` and
# `!` take every operand as one.
.bare_conditions <- function(logic) {
  out <- character()
  for (sub in .logic_nodes(logic)) {
    node <- sub[["node"]]
    for (operator in intersect(names(node), CHECKS_CONDITION_OPS)) {
      args <- node[[operator]]
      if (!is.list(args)) {
        next
      }
      indices <- seq_along(args)
      positions <- if (identical(operator, "if")) {
        indices[indices %% 2L == 1L & indices < length(args)]
      } else {
        indices
      }
      for (i in positions) {
        path <- args[[i]][["var"]]
        if (is.character(path) && length(path) == 1L) {
          out <- c(out, paste0("`", operator, "` decides `", path, "`"))
        }
      }
    }
  }
  unique(out)
} # /.bare_conditions


# %% .open_lambdas ----
# Paths of every `var` inside a scoped lambda that reaches for outer data.
#
# The lambda of a scoped operator is its *second* argument; the first is the
# array expression, which is evaluated against the outer data and so may
# reference anything.
.open_lambdas <- function(logic) {
  out <- character()
  for (sub in .logic_nodes(logic)) {
    node <- sub[["node"]]
    keys <- names(node)
    scoped <- intersect(keys, CHECKS_SCOPED_OPS)
    if (length(scoped) == 0L) {
      next
    }
    for (operator in scoped) {
      args <- node[[operator]]
      if (!is.list(args) || length(args) < 2L) {
        next
      }
      for (var_node in .logic_nodes(args[[2L]])) {
        path <- var_node[["node"]][["var"]]
        if (!is.character(path) || length(path) != 1L) {
          next
        }
        root <- strsplit(path, ".", fixed = TRUE)[[1L]][[1L]]
        if (root %in% CHECKS_OUTER_ROOTS) {
          out <- c(out, paste0(operator, " lambda reads `", path, "`"))
        }
      }
    }
  }
  unique(out)
} # /.open_lambdas


# %% .rule_logic ----
# Every compiled expression a rule carries, so the operator and lambda checks
# see all of them rather than the condition alone.
.rule_logic <- function(r) {
  c(
    unname(r[["let"]]),
    list(r[["applies_when"]], r[["condition"]]),
    unname(r[["evidence"]]),
    unname(r[["slots"]]),
    list(r[["fix"]][["when"]]),
    lapply(r[["fix"]][["patch"]], `[[`, "value")
  )
} # /.rule_logic


# %% .reads_binding ----
# Whether a compiled expression reads a named binding.
.reads_binding <- function(logic, name) {
  target <- paste0("bindings.", name)
  any(vapply(
    .logic_nodes(logic),
    function(sub) {
      path <- sub[["node"]][["var"]]
      is.character(path) && length(path) == 1L && identical(path, target)
    },
    logical(1L)
  ))
} # /.reads_binding


# %% assert_checks_contract ----
# Abort unless the built rule set satisfies the authoring rules.
#
# `doc` Named list: the document `checks_build_let()` and
#       `checks_build_rules()` produced, before serialization.
#
# Returns `doc` invisibly, so it can wrap a write call.
assert_checks_contract <- function(doc) {
  problems <- character()
  note <- function(...) {
    problems <<- c(problems, paste0(...))
  }

  all_logic <- c(
    lapply(doc[["let"]], function(b) {
      c(
        list(b[["logic"]], b[["source"]][["logic"]], b[["where"]]),
        unname(b[["select"]])
      )
    }),
    lapply(doc[["rules"]], .rule_logic),
    lapply(doc[["unevaluable"]], function(u) list(u[["when"]]))
  )
  for (logic in unlist(all_logic, recursive = FALSE)) {
    banned <- .banned_operators(logic)
    if (length(banned) > 0L) {
      note(
        "uses ",
        paste0("`", banned, "`", collapse = ", "),
        ". Loose equality and the truthiness cast are the only JSONLogic ",
        "semantics that vary between ports, and a divergence there produces a ",
        "wrong severity rather than an error. Use `===` / `!==`, and compare ",
        "explicitly instead of casting."
      )
    }
    bare <- .bare_conditions(logic)
    if (length(bare) > 0L) {
      note(
        paste(bare, collapse = "; "),
        ". A conditional position holding a bare reference is decided by ",
        "truthiness, whose spec is JSONLogic's own and not every port's. ",
        "Compare explicitly: `x === true`."
      )
    }
    open <- .open_lambdas(logic)
    if (length(open) > 0L) {
      note(
        paste(open, collapse = "; "),
        ". JSONLogic replaces the data with the current element inside these ",
        "operators, so an outer reference resolves to null rather than ",
        "failing. Iteration that needs outer scope belongs in a `scan`."
      )
    }
  }

  for (r in doc[["rules"]]) {
    if (!r[["code"]] %in% DIAGNOSTIC_CODES) {
      note(
        "rule `",
        r[["id"]],
        "` uses code `",
        r[["code"]],
        "`, which is not in DIAGNOSTIC_CODES"
      )
    }
    if (!r[["severity"]] %in% DIAGNOSTIC_SEVERITIES) {
      note("rule `", r[["id"]], "` uses severity `", r[["severity"]], "`")
    }
    if ("plain" %in% names(r)) {
      note(
        "rule `",
        r[["id"]],
        "` carries `plain`. That text is authored once per code in ",
        "DIAGNOSTIC_PLAIN and looked up by the renderer; a copy here would be ",
        "one per rule and would drift."
      )
    }
    # A named outcome that is not a column ends the pass: every check below it
    # would report on a column that is not there. Flattened into rules that is
    # not statement order but a guard, and one every rule but the finding
    # itself has to carry -- so it is asserted rather than remembered.
    if (
      !identical(r[["code"]], "OUTCOME_MISSING") &&
        !.reads_binding(r[["applies_when"]], "outcome_resolvable")
    ) {
      note(
        "rule `",
        r[["id"]],
        "` does not guard on `outcome_resolvable`. A named outcome that is ",
        "not a column ends the pass in the reference implementation, so every ",
        "rule but OUTCOME_MISSING must be suppressed by it."
      )
    }
    slots <- c(
      message_slots(r[["message"]]),
      unlist(lapply(r[["fix"]][["patch"]], function(o) {
        message_slots(o[["path"]])
      }))
    )
    unknown <- setdiff(slots, c(names(r[["evidence"]]), names(r[["slots"]])))
    if (length(unknown) > 0L) {
      note(
        "rule `",
        r[["id"]],
        "` reads slot",
        if (length(unknown) == 1L) " " else "s ",
        paste0("`{", unknown, "}`", collapse = ", "),
        ", which its evidence does not declare"
      )
    }
    # Evidence is read by clients whether or not the message names it, so an
    # unread evidence key is fine. An unread *slot* is not: a slot exists only
    # to be rendered.
    stray <- setdiff(names(r[["slots"]]), slots)
    if (length(stray) > 0L) {
      note(
        "rule `",
        r[["id"]],
        "` declares slot",
        if (length(stray) == 1L) " " else "s ",
        paste0("`", stray, "`", collapse = ", "),
        " that its message does not read. A slot is presentation only; a value ",
        "worth carrying unread is evidence."
      )
    }
  }

  if (length(problems) > 0L) {
    stop(
      "Rule-set contract violated:\n  - ",
      paste(problems, collapse = "\n  - "),
      "\nSee plan/validation-rules.md.",
      call. = FALSE
    )
  }
  invisible(doc)
} # /assert_checks_contract
