# utils_audit_props.R
# ::rtemis::
# 2026- EDG rtemis.org

# Development-time audit comparing each property's *documented* contract
# (the roxygen `@param` type declaration mandated by AGENTS.md) against its
# *enforced* contract (the `PropertySpec` built by the `prop_*` factories).
#
# The two are independent declarations of the same intent, written at different
# times by different hands, so disagreement between them is a reliable signal
# that one of the two is wrong. This catches a class of error the existing
# drift guards cannot: those compare `setup_*` defaults against spec defaults
# (same layer, both machine-readable), whereas this compares the human-stated
# contract against the machine-enforced one.
#
# Not a package gate: `audit_prop_docs()` returns findings for triage. See
# `data-raw/audit_props.R` for the runner.
#
# Documented grammar (AGENTS.md):
#   @param <name> [(Tunable) ][Optional ]<Type>[ <Constraint>][ vector]: <Description>
# with Constraint one of `[a, b]` (closed), `(a, b)` (open), `[a, b)` /
# `(a, b]` (half-open), or `\{"a", "b"\}` (enum).

# %% DOC_TYPE_MAP ----
# Documented type word -> `PropertySpec@type`. "Float" is not part of the
# documented vocabulary (AGENTS.md says Numeric); it is mapped so that the
# audit reports it as a vocabulary finding rather than an unparseable type.
DOC_TYPE_MAP <- c(
  Logical = "boolean",
  Integer = "integer",
  Numeric = "number",
  Float = "number",
  Character = "string"
)

# Type words that are legitimate in docs but have no `PropertySpec` equivalent,
# i.e. they describe a `prop_external()` / plain property. Not findings.
DOC_TYPE_NONSPEC <- c(
  "List",
  "Matrix",
  "Named vector",
  "Function",
  "S7 class",
  "tabular data"
)

# Property name -> a fallback `@param` name, for the case where a `setup_*`
# argument is deliberately spelled differently from the property it sets. The
# property's own name always takes precedence; the alias is consulted only when
# no `@param` of that name exists.
#
# Empty by design. Every entry here is a smell: the public argument name and
# the property/schema name diverge, so a user reading `?setup_X` and a user
# reading a config JSON see different names. The one entry this map was
# introduced for (`n` <- `n_resamples`) was removed by renaming the property to
# `n_resamples` instead (2026-07-26). Prefer that fix over adding an entry.
PROP_DOC_ALIASES <- character()

# Severity ranking, most serious first. A wrong type silently accepts wrong
# values; a missing description only degrades generated help text.
AUDIT_SEVERITY <- c(
  "type",
  "enum",
  "bounds",
  "nullable",
  "vector",
  "tunable",
  "vocabulary",
  "undocumented",
  "no_doc_source",
  "description"
)


# %% parse_roxygen_params ----
#' Extract `@param` entries from the roxygen blocks in a directory of R files
#'
#' Associates each roxygen block with the function defined immediately after
#' it. A single `@param` may document several parameters (`@param min,max`),
#' which are split. Continuation lines are appended.
#'
#' @param r_dir Character: Path to a directory of R source files.
#'
#' @return Named list keyed by function name; each element a named character
#'   vector of `@param` texts keyed by parameter name.
#'
#' @author EDG
#' @keywords internal
#' @noRd
parse_roxygen_params <- function(r_dir) {
  files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  out <- list()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    block <- character()
    for (line in lines) {
      if (grepl("^\\s*#'", line)) {
        block <- c(block, sub("^\\s*#'\\s?", "", line))
        next
      }
      # Functions document with `@param`, classes with `@field`; both use the
      # same `<name> <Type>: <Description>` grammar, and both are the
      # human-stated contract for whatever they are attached to. Keyed by the
      # assigned name either way, so a class is looked up by its own name.
      fn <- regmatches(
        line,
        regexec(
          "^\\s*([a-zA-Z._][a-zA-Z0-9._]*)\\s*<-\\s*(function|(S7::)?new_class)",
          line
        )
      )[[1L]]
      if (length(fn) >= 2L && length(block) > 0L) {
        params <- .roxygen_block_params(block)
        if (length(params) > 0L) {
          out[[fn[[2L]]]] <- params
        }
      }
      # Any non-roxygen line ends the current block.
      block <- character()
    }
  }
  out
} # /rtemis::parse_roxygen_params


# %% .roxygen_block_params ----
#' Collect `@param` entries from one roxygen block's lines
#'
#' @param block Character vector: Roxygen block with the `#'` prefix stripped.
#'
#' @return Named character vector of `@param` texts.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.roxygen_block_params <- function(block) {
  out <- character()
  current <- NULL
  for (line in block) {
    m <- regmatches(
      line,
      regexec("^@(?:param|field)\\s+(\\S+)\\s*(.*)$", line, perl = TRUE)
    )[[1L]]
    if (length(m) == 3L) {
      current <- m[[2L]]
      out[[current]] <- m[[3L]]
      next
    }
    if (grepl("^@", line)) {
      current <- NULL
      next
    }
    if (!is.null(current) && nzchar(trimws(line))) {
      out[[current]] <- paste(out[[current]], trimws(line))
    }
  }
  if (length(out) == 0L) {
    return(out)
  }
  # `@param min,max ...` documents several parameters with one entry.
  split_names <- strsplit(names(out), ",", fixed = TRUE)
  rep_texts <- rep(unname(out), lengths(split_names))
  stats::setNames(rep_texts, trimws(unlist(split_names)))
} # /rtemis::.roxygen_block_params


# %% resolve_doc_delegation ----
#' Follow a `See [setup_X]` `@param` entry to the declaration it defers to
#'
#' Several wrapper classes document shared parameters by cross-reference rather
#' than repeating the declaration (`SuperConfigLive` defers to
#' `setup_SuperConfig`) — the hand-written equivalent of roxygen's
#' `@inheritParams`. The audit follows the reference so that delegating is not
#' reported as a missing type declaration.
#'
#' @param text Character: The `@param` text, possibly a delegation.
#' @param prop_name Character: Parameter name being resolved.
#' @param docs Named list: Output of `parse_roxygen_params()`.
#'
#' @return Character: The resolved `@param` text, or `text` unchanged when it is
#'   not a delegation or the target does not document `prop_name`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_doc_delegation <- function(text, prop_name, docs) {
  m <- regmatches(
    text,
    regexec("[Ss]ee\\s+\\[?(setup_[A-Za-z0-9._]+)\\]?", text)
  )[[1L]]
  if (length(m) != 2L) {
    return(text)
  }
  target <- docs[[m[[2L]]]]
  if (is.null(target) || !prop_name %in% names(target)) {
    return(text)
  }
  target[[prop_name]]
} # /rtemis::resolve_doc_delegation


# %% parse_doc_type ----
#' Parse a documented type declaration into its components
#'
#' @param text Character: The text of an `@param` entry, i.e. everything after
#'   the parameter name.
#'
#' @return Named list with elements `type` (documented type word, or NA),
#'   `json_type` (mapped `PropertySpec@type`, or NA), `tunable`, `nullable`,
#'   `vector` (logical), `enum` (character or NULL), `minimum`, `maximum`,
#'   `exclusive_minimum`, `exclusive_maximum` (numeric or NULL), and
#'   `parsed` (logical: whether a type declaration was found at all).
#'
#' @author EDG
#' @keywords internal
#' @noRd
parse_doc_type <- function(text) {
  empty <- list(
    type = NA_character_,
    json_type = NA_character_,
    tunable = FALSE,
    nullable = FALSE,
    vector = FALSE,
    enum = NULL,
    minimum = NULL,
    maximum = NULL,
    exclusive_minimum = NULL,
    exclusive_maximum = NULL,
    parsed = FALSE
  )
  # The declaration is everything before the first ": " separator.
  head <- sub("(?s):\\s.*$", "", text, perl = TRUE)
  if (identical(head, text) && !grepl(":$", text)) {
    # No separator: not a type declaration.
    return(empty)
  }
  head <- trimws(sub(":$", "", head))
  out <- empty
  if (grepl("^\\(Tunable\\)", head)) {
    out[["tunable"]] <- TRUE
    head <- trimws(sub("^\\(Tunable\\)", "", head))
  }
  if (grepl("^Optional\\b", head)) {
    out[["nullable"]] <- TRUE
    head <- trimws(sub("^Optional\\b", "", head))
  }
  # "Integer or NULL" is a non-standard spelling of "Optional Integer".
  if (grepl("\\bor NULL$", head)) {
    out[["nullable"]] <- TRUE
    head <- trimws(sub("\\bor NULL$", "", head))
  }
  if (grepl("\\bvectors?$", head)) {
    out[["vector"]] <- TRUE
    head <- trimws(sub("\\bvectors?$", "", head))
  }
  # Trailing constraint: an interval or an enum, with roxygen escaping allowed.
  constraint <- regmatches(
    head,
    regexpr("(\\\\?[\\[(]|\\\\?\\{).*$", head, perl = TRUE)
  )
  if (length(constraint) == 1L) {
    head <- trimws(substr(head, 1L, nchar(head) - nchar(constraint)))
    out <- .parse_doc_constraint(constraint, out)
  }
  out[["type"]] <- if (nzchar(head)) head else NA_character_
  out[["json_type"]] <- unname(DOC_TYPE_MAP[out[["type"]]])
  if (is.na(out[["json_type"]])) {
    out[["json_type"]] <- NA_character_
  }
  out[["parsed"]] <- TRUE
  out
} # /rtemis::parse_doc_type


# %% .parse_doc_constraint ----
#' Parse a documented constraint (interval or enum) into spec fields
#'
#' @param constraint Character: The constraint text, e.g. `"[1, Inf)"` or
#'   `'\\{"a", "b"\\}'`.
#' @param out Named list: Accumulator from `parse_doc_type()`.
#'
#' @return `out`, with bounds or enum filled in.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.parse_doc_constraint <- function(constraint, out) {
  clean <- gsub("\\\\", "", constraint)
  if (grepl("^\\{", clean)) {
    values <- regmatches(clean, gregexpr('"[^"]*"', clean))[[1L]]
    if (length(values) > 0L) {
      out[["enum"]] <- gsub('"', "", values)
    }
    return(out)
  }
  # Extracted positionally rather than by regex: a bracket expression matching
  # `]` and `)` is a portability trap across R's regex engines.
  n <- nchar(clean)
  if (n < 4L) {
    return(out)
  }
  open <- substr(clean, 1L, 1L)
  close <- substr(clean, n, n)
  if (!open %in% c("[", "(") || !close %in% c("]", ")")) {
    return(out)
  }
  parts <- strsplit(substr(clean, 2L, n - 1L), ",", fixed = TRUE)[[1L]]
  if (length(parts) != 2L) {
    return(out)
  }
  lower <- suppressWarnings(as.numeric(trimws(parts[[1L]])))
  upper <- suppressWarnings(as.numeric(trimws(parts[[2L]])))
  if (!is.na(lower) && is.finite(lower)) {
    if (open == "[") {
      out[["minimum"]] <- lower
    } else {
      out[["exclusive_minimum"]] <- lower
    }
  }
  if (!is.na(upper) && is.finite(upper)) {
    if (close == "]") {
      out[["maximum"]] <- upper
    } else {
      out[["exclusive_maximum"]] <- upper
    }
  }
  out
} # /rtemis::.parse_doc_constraint


# %% spec_classes ----
#' All S7 classes in a namespace that declare `prop_*` properties
#'
#' @param ns Character: Namespace to search.
#'
#' @return Named list of S7 classes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
spec_classes <- function(ns = "rtemis") {
  namespace <- asNamespace(ns)
  nms <- ls(namespace, all.names = FALSE)
  out <- list()
  for (nm in nms) {
    obj <- tryCatch(get(nm, envir = namespace), error = function(e) NULL)
    if (!inherits(obj, "S7_class")) {
      next
    }
    if (length(spec_prop_names(obj)) > 0L) {
      out[[nm]] <- obj
    }
  }
  out
} # /rtemis::spec_classes


# %% doc_source_for_class ----
#' Candidate `setup_*` function names documenting a config class
#'
#' `LightRFHyperparameters` -> `setup_LightRF`, `PCAConfig` -> `setup_PCA`.
#' Families whose subclasses share one setup function (resamplers, tuners) fall
#' back to it.
#'
#' @param class_name Character: S7 class name.
#'
#' @return Character vector of candidate function names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
doc_source_for_class <- function(class_name) {
  stripped <- sub("(Hyperparameters|Config)$", "", class_name)
  # Both spellings occur: setup_LightRF (stripped) and setup_ExecutionConfig
  # (unstripped). Try the stripped form first, then the class name verbatim.
  fallback <- c(
    KFold = "setup_Resampler",
    StratSub = "setup_Resampler",
    StratBoot = "setup_Resampler",
    Bootstrap = "setup_Resampler",
    LOOCV = "setup_Resampler",
    Custom = "setup_Resampler",
    GridSearch = "setup_GridSearch",
    Preprocessor = "setup_Preprocessor"
  )
  out <- c(
    paste0("setup_", stripped),
    paste0("setup_", class_name),
    unname(fallback[stripped]),
    # Last resort: the class's own `@field` block. Ranked below `setup_*`
    # because the setup function is the user-facing contract; `@field` fills
    # gaps and covers classes built by something other than a `setup_*`
    # function (e.g. `DataFingerprint`, built by `data_fingerprint()`).
    class_name
  )
  # `fallback[stripped]` yields NA for classes with no family fallback.
  unique(out[!is.na(out)])
} # /rtemis::doc_source_for_class


# %% audit_prop_docs ----
#' Audit documented property contracts against their enforced PropertySpecs
#'
#' For every `prop_*`-declared property of every config class, compares the
#' roxygen `@param` type declaration against the property's `PropertySpec` and
#' reports disagreements. See the file header for rationale.
#'
#' @param r_dir Character: Path to the package's `R/` source directory.
#' @param classes Optional named list of S7 classes. Default: every class in
#'   the rtemis namespace declaring `prop_*` properties.
#' @param aliases Named character: Property name -> fallback `@param` name; see
#'   `PROP_DOC_ALIASES`, which is the default and is empty by design.
#'
#' @return `data.frame` with columns `class`, `property`, `check`, `severity`,
#'   `documented`, `declared`, ordered by severity.
#'
#' @author EDG
#' @keywords internal
#' @noRd
audit_prop_docs <- function(r_dir, classes = NULL, aliases = PROP_DOC_ALIASES) {
  if (!dir.exists(r_dir)) {
    rtemis.core::abort(
      "`r_dir` does not exist: ",
      r_dir,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (is.null(classes)) {
    classes <- spec_classes()
  }
  docs <- parse_roxygen_params(r_dir)
  findings <- list()
  add <- function(class_name, property, check, documented, declared) {
    findings[[length(findings) + 1L]] <<- data.frame(
      class = class_name,
      property = property,
      check = check,
      severity = match(check, AUDIT_SEVERITY),
      documented = documented,
      declared = declared,
      stringsAsFactors = FALSE
    )
  }
  for (class_name in names(classes)) {
    cls <- classes[[class_name]]
    sources <- doc_source_for_class(class_name)
    params <- character()
    for (src in sources) {
      if (!is.null(docs[[src]])) {
        # Earlier candidates win; a family fallback only fills gaps.
        params <- c(
          params,
          docs[[src]][setdiff(names(docs[[src]]), names(params))]
        )
      }
    }
    if (length(params) == 0L) {
      # No documentation source at all: one finding for the class, rather than
      # one per property, which would drown the per-property findings.
      add(
        class_name,
        "(class)",
        "no_doc_source",
        paste0("no ", paste(sources, collapse = " / ")),
        paste(length(spec_prop_names(cls)), "declared properties")
      )
      next
    }
    for (prop_name in spec_prop_names(cls)) {
      spec <- get_spec(cls@properties[[prop_name]])
      if (!nzchar(spec@description)) {
        add(class_name, prop_name, "description", "", "(empty)")
      }
      # The property's own name always wins; an alias only fills a gap, so a
      # class with its own `n` is unaffected by the resampler `n_resamples`
      # alias.
      doc_name <- prop_name
      if (!doc_name %in% names(params) && prop_name %in% names(aliases)) {
        alias <- unname(aliases[prop_name])
        if (!is.na(alias) && alias %in% names(params)) {
          doc_name <- alias
        }
      }
      text <- if (doc_name %in% names(params)) params[[doc_name]] else NULL
      if (!is.null(text)) {
        text <- resolve_doc_delegation(text, doc_name, docs)
      }
      if (is.null(text)) {
        add(class_name, prop_name, "undocumented", "(none)", spec@type)
        next
      }
      findings <- .compare_doc_spec(
        text,
        spec,
        class_name,
        prop_name,
        findings
      )
    }
  }
  if (length(findings) == 0L) {
    return(data.frame(
      class = character(),
      property = character(),
      check = character(),
      severity = integer(),
      documented = character(),
      declared = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, findings)
  out[order(out[["severity"]], out[["class"]], out[["property"]]), ]
} # /rtemis::audit_prop_docs


# %% .compare_doc_spec ----
#' Compare one documented declaration against its PropertySpec
#'
#' @param text Character: `@param` text.
#' @param spec `PropertySpec` object.
#' @param class_name,prop_name Character: Identifiers for the finding.
#' @param findings List: Accumulator.
#'
#' @return `findings`, extended.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.compare_doc_spec <- function(text, spec, class_name, prop_name, findings) {
  doc <- parse_doc_type(text)
  add <- function(check, documented, declared) {
    findings[[length(findings) + 1L]] <<- data.frame(
      class = class_name,
      property = prop_name,
      check = check,
      severity = match(check, AUDIT_SEVERITY),
      documented = documented,
      declared = declared,
      stringsAsFactors = FALSE
    )
  }
  if (!doc[["parsed"]]) {
    add("undocumented", "(no type declaration)", spec@type)
    return(findings)
  }
  doc_type <- doc[["type"]]
  if (!is.na(doc_type) && doc_type %in% DOC_TYPE_NONSPEC) {
    # Documented as a non-schema type but declared with a factory.
    add("type", doc_type, spec@type)
    return(findings)
  }
  if (is.na(doc[["json_type"]])) {
    add(
      "vocabulary",
      if (is.na(doc_type)) "(unparsed)" else doc_type,
      spec@type
    )
  } else {
    if (!identical(doc[["json_type"]], spec@type)) {
      add("type", doc_type, spec@type)
    } else if (identical(doc_type, "Float")) {
      add("vocabulary", "Float", "Numeric (AGENTS.md vocabulary)")
    }
  }
  if (!identical(doc[["nullable"]], spec@nullable)) {
    add(
      "nullable",
      if (doc[["nullable"]]) "Optional" else "not Optional",
      if (spec@nullable) "nullable = TRUE" else "nullable = FALSE"
    )
  }
  if (!identical(doc[["tunable"]], spec@tunable)) {
    add(
      "tunable",
      if (doc[["tunable"]]) "(Tunable)" else "not marked Tunable",
      if (spec@tunable) "tunable = TRUE" else "tunable = FALSE"
    )
  }
  spec_is_vector <- spec@container != "none"
  if (!identical(doc[["vector"]], spec_is_vector)) {
    add(
      "vector",
      if (doc[["vector"]]) "vector" else "scalar",
      if (spec_is_vector) "container != none" else "container = none"
    )
  }
  if (!is.null(doc[["enum"]]) || !is.null(spec@enum)) {
    if (!setequal(doc[["enum"]] %||% character(), spec@enum %||% character())) {
      add(
        "enum",
        .fmt_set(doc[["enum"]]),
        .fmt_set(spec@enum)
      )
    }
  }
  for (bound in c(
    "minimum",
    "maximum",
    "exclusive_minimum",
    "exclusive_maximum"
  )) {
    doc_value <- doc[[bound]]
    spec_value <- prop(spec, bound)
    if (!isTRUE(all.equal(doc_value %||% NA, spec_value %||% NA))) {
      add(
        "bounds",
        paste0(bound, " = ", .fmt_bound(doc_value)),
        paste0(bound, " = ", .fmt_bound(spec_value))
      )
    }
  }
  findings
} # /rtemis::.compare_doc_spec


# %% .fmt_set ----
#' Format a character set for an audit finding
#'
#' @param x Character or NULL.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fmt_set <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("(none)")
  }
  paste0("{", paste(x, collapse = ", "), "}")
} # /rtemis::.fmt_set


# %% .fmt_bound ----
#' Format a numeric bound for an audit finding
#'
#' @param x Numeric or NULL.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.fmt_bound <- function(x) {
  if (is.null(x) || length(x) == 0L) "(none)" else as.character(x)
} # /rtemis::.fmt_bound
