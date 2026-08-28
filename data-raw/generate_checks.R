# generate_checks.R
# ::rtemis::
# 2026- EDG rtemis.org

# Compiles `data-raw/checks.R` to `checks/v1/checks.json` and publishes the
# algorithm traits the rules read as `traits/v1/traits.json`. Run with:
# Rscript data-raw/generate_checks.R [SCHEMA_REPO]
#
# Also writes both documents to `inst/`, so that the package carries the
# artifact it publishes: `data-raw/` is in `.Rbuildignore`, and the corpus test
# that pairs every rule with a fixture has to read the rule set on CRAN.

suppressMessages(devtools::load_all(quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"

source(file.path("data-raw", "write_json.R"))
source(file.path("data-raw", "checks_compile.R"))
source(file.path("data-raw", "checks_contract.R"))
source(file.path("data-raw", "checks.R"))


# Traits ---------------------------------------------------------------------
# The columns the rules read, as an array of records. `supervised_algorithms`
# holds them as strings ("TRUE" / "FALSE" / NA) because it is built from a
# character matrix; they are logical here, so that `=== true` and `=== false`
# mean what they say and the third value is JSON null.
trait_columns <- c("class", "reg", "surv", "missing", "p_gt_n")
traits <- lapply(seq_len(nrow(supervised_algorithms)), function(i) {
  row <- c(
    list(name = supervised_algorithms[["name"]][[i]]),
    lapply(trait_columns, function(column) {
      as.logical(supervised_algorithms[[column]][[i]])
    })
  )
  names(row) <- c("name", trait_columns)
  row
})

traits_document <- list(
  `$id` = paste0(base_url, "/traits/v1/traits.json"),
  title = "rtemis supervised algorithm traits",
  description = paste0(
    "What each supervised algorithm can do, as the facts a validator needs. ",
    "Read by `checks/v1/checks.json`. Every trait is three-valued, and a null ",
    "is not \"unknown\" but *not the algorithm's to answer*: a stacked ensemble ",
    "accepts whatever its base learners accept, so the answer is a property of ",
    "the library it is given rather than of the algorithm. Test with an ",
    "explicit comparison against true, false and null; a trait read by ",
    "truthiness gives the wrong answer for a meta learner."
  ),
  # What each column means, because one of them does not mean what its
  # neighbours do and a reader has no way to tell from the values.
  trait_descriptions = list(
    class = paste0(
      "rtemis fits this algorithm to a categorical outcome. `checks/v1` reads ",
      "it to decide whether a config declares a task its outcome cannot serve."
    ),
    reg = paste0(
      "rtemis fits this algorithm to a numeric outcome. Read with `class` for ",
      "the same decision."
    ),
    surv = paste0(
      "The backend supports right-censored survival outcomes. **This is not a ",
      "statement about rtemis**, unlike `class` and `reg`: rtemis resolves an ",
      "outcome to Classification or Regression only, and dispatches no ",
      "survival run, so no `checks/v1` rule reads this. It is published ",
      "because survival support is planned and the column is what a future ",
      "rule will read; until then it describes what the backend could do, not ",
      "what rtemis will do with it."
    ),
    missing = paste0(
      "The algorithm accepts a training set containing missing values. Read by ",
      "MISSING_INCOMPATIBLE, where false is an error and null a warning."
    ),
    p_gt_n = paste0(
      "The algorithm gives a usable fit with more predictors than cases. False ",
      "only where the fit is an unregularized least squares and goes ",
      "rank-deficient. Read by DIM_P_GT_N, where it decides the severity."
    )
  ),
  traits = traits
)


# Checks ---------------------------------------------------------------------
compiled <- checks_build_let(CHECKS_LET)
rules <- checks_build_rules(CHECKS_RULES, compiled[["env"]])
unevaluable <- lapply(CHECKS_UNEVALUABLE, function(entry) {
  list(
    id = entry[["id"]],
    when = checks_compile_boolean(
      entry[["when"]],
      compiled[["env"]],
      paste0("unevaluable `", entry[["id"]], "`")
    ),
    affects = entry[["affects"]],
    reason = entry[["reason"]]
  )
})

checks_document <- list(
  `$id` = paste0(base_url, "/checks/v1/checks.json"),
  title = "rtemis data checks",
  description = paste0(
    "Whether a config is right for the data it is about to run on, as rules ",
    "rather than as code. Each rule reads a `profile/v1` description of the ",
    "dataset, a config document, the outcome column's name, and ",
    "`traits/v1`; none reads the data. Generated from rtemis's own checks, ",
    "which remain the reference implementation."
  ),
  expression_language = "jsonlogic",
  inputs = list(
    profile = paste0(base_url, "/profile/v1/schema.json"),
    config = "The config being validated, with defaults resolved.",
    outcome = "Name of the outcome column, or null to take rtemis's convention.",
    traits = paste0(base_url, "/traits/v1/traits.json")
  ),
  evaluation = list(
    data = paste0(
      "Evaluate against an object holding the four inputs under their own ",
      "names, plus `bindings` (filled by `let`) and, inside an iteration, ",
      "`item`."
    ),
    let = paste0(
      "Evaluate in order, writing each result to `bindings.<name>`. An entry ",
      "of kind `expr` evaluates its `logic`. An entry of kind `scan` iterates ",
      "its source, evaluating `where` and `select` with the element bound to ",
      "`item` and everything else still in scope -- which is what JSONLogic's ",
      "own `filter` cannot do, since it replaces the data with the element. ",
      "`select` is a name -> expression map and is the only way to build a ",
      "record."
    ),
    source = paste0(
      "A scan source is either `logic` (an expression yielding an array) or ",
      "`pointers` (a list of RFC 6901 JSON Pointers into the config). Each ",
      "pointer yields `{pointer, name, value}`: the pointer as written, its ",
      "last segment, and what it resolves to, or null. A `*` segment expands ",
      "to every member of the object or array at that point, in document ",
      "order, which is how a hyperparameter set's members are reached."
    ),
    rules = paste0(
      "For each rule: if `over` names a scan, run once per element with it ",
      "bound to `item`, otherwise once. Evaluate `let` into `bindings`, then ",
      "`applies_when`; where false, the rule does not apply and reports ",
      "nothing. Where `condition` is true, report a finding carrying the ",
      "rule's `code`, `severity`, evaluated `evidence`, and rendered ",
      "`message`. Attach `fix` where present and its `when` holds."
    ),
    message = paste0(
      "Three substitutions, no conditionals. `{key}` inserts the value of an ",
      "evidence key or a slot; an array value joins its elements with ', '. ",
      "`{key|singular|plural}` selects on the value, which is its own ",
      "magnitude when a number and its length when an array: singular at ",
      "exactly 1, plural otherwise."
    ),
    plain = paste0(
      "In `plain`, keyed by `code` rather than by rule: several rules share a ",
      "code, and one explanation per kind of problem is the point. Render it ",
      "beside the rule's own `message`, which states the particulars."
    ),
    unevaluable = paste0(
      "Where an entry's `when` holds, the rule set cannot fully answer for ",
      "the codes it `affects`. Report the validation as incomplete for those ",
      "codes rather than as clean."
    )
  ),
  severities = DIAGNOSTIC_SEVERITIES,
  # The plain-language account per code. Published rather than left to each
  # host to transcribe: it is authored once here, every implementation renders
  # it verbatim, and the conformance corpus already asserts the exact text, so
  # a copy in another language was a copy that had to agree without any way to
  # check it until a fixture happened to cover it.
  plain = as.list(DIAGNOSTIC_PLAIN),
  let = compiled[["let"]],
  rules = rules,
  unevaluable = unevaluable
)

assert_checks_contract(checks_document)


# Write ----------------------------------------------------------------------
for (target in c(schema_repo, "inst")) {
  write_json_document(
    traits_document,
    file.path(target, "traits", "v1", "traits.json")
  )
  write_json_document(
    checks_document,
    file.path(target, "checks", "v1", "checks.json")
  )
}

cat(sprintf(
  "%-16s %d bindings, %d rules, %d unevaluable -> %s\n",
  "checks",
  length(compiled[["let"]]),
  length(rules),
  length(unevaluable),
  file.path(schema_repo, "checks", "v1", "checks.json")
))
cat(sprintf(
  "%-16s %d algorithms -> %s\n",
  "traits",
  length(traits),
  file.path(schema_repo, "traits", "v1", "traits.json")
))
