# generate_checks_corpus.R
# ::rtemis::
# 2026- EDG rtemis.org

# Records the fixture corpus as portable conformance data:
# `checks/v1/corpus.json`. Run with:
# Rscript data-raw/generate_checks_corpus.R [SCHEMA_REPO]
#
# `test_ValidateConfigFixtures.R` is the oracle for every implementation of
# `checks/v1`, but it is R fixtures constructing frames: it proves rtemis
# agrees with itself and says nothing about anything else. This records what
# each fixture actually put in and got out --
#
#     (profile, config, outcome) -> findings
#
# -- so that a second implementation has something to reproduce. Without it the
# agreement between rtemis and the CLI is derived-by-construction, which is a
# claim about how the artifact was generated rather than about what the two
# report.
#
# Nothing here reimplements a check. `validate_config()` is rebound in the
# namespace and the suite is sourced, so every call the fixtures make is
# recorded on the way past: a fixture added upstream is recorded the next time
# this runs, and one that changes shape cannot silently keep its old triple.
#
# Not written to `inst/`, unlike `checks.json` and `traits.json`. Those ship
# because `test_ChecksArtifact.R` reads them on CRAN; nothing in R reads the
# corpus -- it is generated *from* the tests that are its oracle -- so shipping
# 160 KB of it in the package tarball would buy nothing.

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages({
  library(jsonlite)
  library(testthat)
})

args <- commandArgs(trailingOnly = TRUE)
schema_repo <- if (length(args) >= 1L) args[[1L]] else "~/Schemas/schema"
schema_repo <- path.expand(schema_repo)
base_url <- "https://schema.rtemis.org"
checks_path <- file.path(schema_repo, "checks", "v1", "checks.json")
if (!file.exists(checks_path)) {
  stop(
    "no rule set at ",
    checks_path,
    ": run data-raw/generate_checks.R first"
  )
}

source(file.path("data-raw", "write_json.R"))


# %% array_evidence_keys ----
# Which evidence keys hold arrays.
#
# `toJSON(auto_unbox = TRUE)` writes a one-element vector as a scalar, so a
# single-column `columns` would serialize as a string where the rule set says
# array -- a document neither implementation would accept. Rather than a
# hand-kept list, the answer is read off the artifact: an evidence expression
# is an array when it is a `map`, or a `var` naming a binding that is a `scan`
# or itself a `map`. A key added upstream is classified without editing this.
array_evidence_keys <- function(checks_file) {
  checks <- jsonlite::fromJSON(checks_file, simplifyVector = FALSE)
  is_array_binding <- vapply(
    checks[["let"]],
    function(b) {
      identical(b[["kind"]], "scan") ||
        (identical(b[["kind"]], "expr") &&
          identical(names(b[["logic"]])[[1L]], "map"))
    },
    logical(1L)
  )
  array_bindings <- vapply(checks[["let"]], `[[`, character(1L), "name")[
    is_array_binding
  ]
  keys <- character()
  for (rule in checks[["rules"]]) {
    for (key in names(rule[["evidence"]])) {
      expr <- rule[["evidence"]][[key]]
      if (!is.list(expr) || length(expr) != 1L) {
        next
      }
      op <- names(expr)[[1L]]
      is_array <- identical(op, "map") ||
        (identical(op, "var") &&
          startsWith(expr[["var"]], "bindings.") &&
          sub("^bindings\\.", "", expr[["var"]]) %in% array_bindings)
      if (is_array) {
        keys <- c(keys, key)
      }
    }
  }
  # `nearest` is the host's to supply -- edit distance is in no expression
  # language of this size -- so it is not in the artifact and is named here.
  unique(c(keys, "nearest"))
} # /array_evidence_keys


# %% array_config_keys ----
# The same problem one level over: `remove_features = "k1"` is a one-element
# array and `auto_unbox` would write it as a string. Read off the published
# tree -- every property whose declared type admits an array -- rather than
# kept by hand here.
array_config_keys <- function(dir) {
  keys <- character()
  walk <- function(node) {
    if (!is.list(node)) {
      return(invisible(NULL))
    }
    props <- node[["properties"]]
    if (is.list(props)) {
      for (name in names(props)) {
        type <- props[[name]][["type"]]
        if ("array" %in% unlist(type) || !is.null(props[[name]][["items"]])) {
          keys <<- c(keys, name)
        }
      }
    }
    lapply(node, walk)
    invisible(NULL)
  }
  files <- list.files(
    dir,
    pattern = "\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  for (file in files) {
    walk(jsonlite::fromJSON(file, simplifyVector = FALSE))
  }
  unique(keys)
} # /array_config_keys


ARRAY_EVIDENCE_KEYS <- array_evidence_keys(checks_path)
ARRAY_CONFIG_KEYS <- array_config_keys(schema_repo)


# %% as_document ----
# A config as the document a client would send: an empty block is `{}` and
# never `[]`, and a property the schemas declare as an array stays one.
as_document <- function(x, key = NULL) {
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) == 0L) {
      return(structure(list(), names = character()))
    }
    out <- lapply(seq_along(x), function(i) as_document(x[[i]], names(x)[[i]]))
    names(out) <- names(x)
    return(out)
  }
  if (!is.null(key) && key %in% ARRAY_CONFIG_KEYS) {
    return(I(x))
  }
  x
} # /as_document


# %% as_evidence ----
as_evidence <- function(evidence) {
  for (key in intersect(names(evidence), ARRAY_EVIDENCE_KEYS)) {
    if (!is.data.frame(evidence[[key]])) {
      evidence[[key]] <- I(evidence[[key]])
    }
  }
  evidence
} # /as_evidence


# %% as_finding ----
# A `Diagnostic` as `diagnostic/v1` carries it. `to_json()` decides which
# properties that is, so the recorded finding cannot fall behind the class the
# way a hand-written list does; what is added here is only the serialization
# jsonlite cannot infer -- which evidence values are arrays, and a patch op's
# object shape.
#
# The list this replaced dropped `step`, on the stated grounds that the corpus
# records one config at a time so the field is always NULL. It is not:
# `validate_config(step = )` stamps it, one fixture exists to prove that, and
# the corpus was discarding the value that fixture is about. No comparison reads
# `step`, which is why nothing noticed.
as_finding <- function(d) {
  out <- to_json(d)
  out[["evidence"]] <- as_evidence(out[["evidence"]])
  if (!is.null(out[["fix"]])) {
    out[["fix"]] <- lapply(out[["fix"]], as_document)
  }
  out
} # /as_finding


# %% as_profile ----
# `to_json()` and not a reassembly: it walks the class's published properties,
# so the recorded document is the one rtemis emits rather than a second
# description of `profile/v1` maintained here. The list this replaced dropped
# `fingerprint`, which went unnoticed for as long as the schema declared no
# `required` -- and a profile the corpus records but rtemis would never emit is
# not an oracle for anything.
as_profile <- function(p) {
  to_json(p)
} # /as_profile


# Capture --------------------------------------------------------------------
# `validate_config()` is rebound in the namespace so that every call the suite
# makes is recorded on the way past.
cases <- new.env(parent = emptyenv())
cases$rows <- list()
cases$label <- "<none>"

ns <- asNamespace("rtemis")
original <- get("validate_config", envir = ns)

recording <- function(config, data = NULL, outcome = NULL, step = NULL) {
  out <- original(config, data = data, outcome = outcome, step = step)
  if (!is.null(data)) {
    n <- length(cases$rows) + 1L
    cases$rows[[n]] <- list(
      id = paste0(cases$label, " #", n),
      # The profile a validation makes: the cheap one, with no duplicate scan.
      profile = as_profile(data_profile(data, n_duplicates = FALSE)),
      config = as_document(config),
      outcome = outcome,
      findings = lapply(seq_len(length(out)), function(i) as_finding(out[[i]]))
    )
  }
  out
} # /recording

for (env in list(ns, as.environment("package:rtemis"))) {
  if (exists("validate_config", envir = env, inherits = FALSE)) {
    unlockBinding("validate_config", env)
    assign("validate_config", recording, envir = env)
    lockBinding("validate_config", env)
  }
}

# The test name each triple came from, so a conformance failure names the
# fixture that produced it rather than an index.
labelling <- function(desc, code) {
  cases$label <- desc
  testthat::test_that(desc, code)
} # /labelling

fixtures <- file.path(
  "tests",
  "testthat",
  "test_ValidateConfigFixtures.R"
)
env <- new.env(parent = ns)
assign("test_that", labelling, envir = env)
# Sourced from the suite's own directory and under a silent reporter: what is
# wanted is the calls the fixtures make, not the assertions they make about
# them. `just test` is what checks those, and running them here too would only
# mean this file could disagree with it.
old_wd <- setwd(dirname(fixtures))
testthat::with_reporter(
  testthat::SilentReporter$new(),
  suppressMessages(sys.source(
    basename(fixtures),
    envir = env,
    keep.source = FALSE
  ))
)
setwd(old_wd)


# Write ----------------------------------------------------------------------
if (length(cases$rows) == 0L) {
  stop(
    "no fixture called validate_config() with data; the capture did not take"
  )
}
# A case is only an oracle if what it records is a document both implementations
# could have been handed, so every recorded profile and finding carries the full
# property set of the schema it claims to be. Asserted here, where a dropped
# field is one line from its cause: without it the failure surfaces two repos
# away, in the CLI's conformance suite, as a schema the corpus does not satisfy.
schema_props <- function(family) {
  names(jsonlite::fromJSON(
    file.path(schema_repo, family, "v1", "schema.json"),
    simplifyVector = FALSE
  )[["properties"]])
}
assert_complete <- function(x, props, what, id) {
  missing <- setdiff(props, names(x))
  if (length(missing) > 0L) {
    stop(
      what,
      " in '",
      id,
      "' is missing ",
      paste(missing, collapse = ", "),
      ". It has to be a document rtemis would emit, not a reassembly of one.",
      call. = FALSE
    )
  }
}
profile_props <- schema_props("profile")
diagnostic_props <- schema_props("diagnostic")
for (row in cases$rows) {
  assert_complete(row[["profile"]], profile_props, "The profile", row[["id"]])
  for (finding in row[["findings"]]) {
    assert_complete(finding, diagnostic_props, "A finding", row[["id"]])
  }
}

out_file <- file.path(schema_repo, "checks", "v1", "corpus.json")
write_json_document(
  list(
    `$id` = paste0(base_url, "/checks/v1/corpus.json"),
    title = "rtemis checks conformance corpus",
    description = paste0(
      "What rtemis's own fixture suite put into `validate_config()` and got ",
      "out: one case per `(profile, config, outcome)` with the findings ",
      "rtemis reported for it. The oracle every implementation of ",
      "`checks/v1` has to reproduce -- message, plain-language text, ",
      "evidence and fix, key for key. Generated from ",
      "`tests/testthat/test_ValidateConfigFixtures.R`, never hand-edited: a ",
      "finding corrected here would assert agreement with something rtemis ",
      "does not do."
    ),
    rtemis_version = as.character(utils::packageVersion("rtemis")),
    cases = cases$rows
  ),
  out_file
)

cat(sprintf(
  "%-16s %d cases -> %s\n",
  "checks corpus",
  length(cases$rows),
  out_file
))
