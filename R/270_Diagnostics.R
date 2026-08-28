# 270_Diagnostics.R
# ::rtemis::
# 2026- EDG rtemis.org

# What `validate_config()` reports: one finding per problem, in a container that
# is empty when there is nothing wrong.
#
# A finding carries two texts, and the split is the point. `message` is
# technical and names the fields involved, for a developer reading a log.
# `plain` is written for someone with no statistics background, and it is
# *stored data*, authored once per code and looked up -- never composed at
# runtime, never produced by a model. `DIAGNOSTIC_PLAIN` below is where it
# lives, and adding a code without adding its line fails at load.
#
# `code` is a stable identifier, permanent from the day it is written: a client
# branches on it, a test asserts it, and a report groups by it. Rewording a
# `message` is free; renaming a code is a breaking change.
#
# `fix` is an RFC 6902 JSON Patch against the config the finding is about, and
# is present only where a deterministic fix exists -- one an implementation can
# apply without choosing anything. Where the remedy is a judgment call (which
# fold count, which algorithm), the field is absent, because a patch that
# guesses is worse than none.

# %% DIAGNOSTIC_CODES ----
# The diagnostic vocabulary. One schema code plus the seven data checks.
#
# `SCHEMA_INVALID` is the whole schema half: `read_config()`'s reconstruction
# either succeeds or names what is wrong, so the finding is that condition
# wrapped rather than a second, parallel validator.
DIAGNOSTIC_CODES <- c(
  "SCHEMA_INVALID",
  "OUTCOME_MISSING",
  "OUTCOME_TYPE_MISMATCH",
  "RESAMPLE_MIN_CLASS",
  "RESAMPLE_N_ROWS",
  "FEATURE_CONSTANT",
  "FEATURE_TYPE_UNSUPPORTED",
  "DIM_P_GT_N",
  "MISSING_INCOMPATIBLE"
)

# %% DIAGNOSTIC_SEVERITIES ----
# What a finding means for running the config, weakest last:
# - "error"   the run will fail, or will answer a different question than asked
# - "warning" the run will complete, but the result is likely not what is wanted
# - "note"    worth knowing; nothing is wrong
DIAGNOSTIC_SEVERITIES <- c("error", "warning", "note")

# %% DIAGNOSTIC_PLAIN ----
# The plain-language text for each code, one entry, written by hand.
#
# Written for a reader with no statistics background: it says what is wrong and
# what it means for the answer, in the words that reader would use, and names no
# field, class or function. The technical account is `Diagnostic@message`, and
# the numbers are `Diagnostic@evidence`; neither belongs here.
#
# Stored rather than generated. A sentence assembled at runtime drifts with the
# code that assembles it, and one produced by a model is a claim nothing checked
# -- which is the whole reason this text is data.
DIAGNOSTIC_PLAIN <- c(
  SCHEMA_INVALID = paste0(
    "This setup could not be read. Something in it does not match what rtemis ",
    "expects -- a misspelled setting, or a value of the wrong kind. Nothing ",
    "was checked against your data, because the setup has to be readable ",
    "first."
  ),
  OUTCOME_MISSING = paste0(
    "The column you asked to predict is not in this dataset. Check the ",
    "spelling, or pick a column that is there."
  ),
  OUTCOME_TYPE_MISMATCH = paste0(
    "The column you asked to predict does not hold the kind of values this ",
    "setup expects. Predicting which group something falls into needs a ",
    "column of labels; predicting a quantity needs a column of numbers. ",
    "Right now the two do not match, so the run would answer a different ",
    "question than you asked."
  ),
  RESAMPLE_MIN_CLASS = paste0(
    "To check how well the model does, the data is split into several parts ",
    "and each part is held back in turn. One of the groups you are predicting ",
    "has fewer cases than there are parts, so at least one part would contain ",
    "none of that group and the score for it would be meaningless. Use fewer ",
    "parts, or gather more cases of the rarer group."
  ),
  RESAMPLE_N_ROWS = paste0(
    "To check how well the model does, the data is split into several parts ",
    "and each part is held back in turn. There are not enough rows for the ",
    "number of parts asked for, so some parts would be empty or hold a single ",
    "row. A score from one row says nothing. Use fewer parts, or more data."
  ),
  FEATURE_CONSTANT = paste0(
    "Some columns hold the same value in every row, or hold no value at all. ",
    "A column that never changes cannot explain anything that does, so it ",
    "adds nothing to the model and can be left out."
  ),
  FEATURE_TYPE_UNSUPPORTED = paste0(
    "Some of the columns being used to predict hold text, or dates, or ",
    "something else a model cannot read. A model needs numbers, or labels from ",
    "a fixed set of categories. Convert those columns before training: text ",
    "naming a category should become a category, and a date should become the ",
    "parts of it that matter, such as the month or the day of the week."
  ),
  DIM_P_GT_N = paste0(
    "There are more things being measured than there are rows to learn from. ",
    "A model in this position can fit the rows it was given almost perfectly ",
    "and still be wrong about new cases. Some methods are built for exactly ",
    "this and handle it; others cannot, and give an answer that only looks ",
    "like one. The finding says which of the two you have."
  ),
  MISSING_INCOMPATIBLE = paste0(
    "This dataset has gaps -- rows where a value was not recorded -- and the ",
    "way this run is set up does not deal with them. Either fill the gaps in, ",
    "drop the rows or columns that have them, or choose a method that can ",
    "work around them."
  )
)

# Every code carries its text, and no text is orphaned. Checked at load, so a
# code added without its line fails here rather than at the first finding.
stopifnot(setequal(names(DIAGNOSTIC_PLAIN), DIAGNOSTIC_CODES))


# %% Diagnostic ----
#' Diagnostic Class
#'
#' @description
#' One finding from [validate_config]: a stable code, how much it matters, the
#' technical and plain-language accounts of it, the numbers behind it, and --
#' where one exists -- a patch that fixes it.
#'
#' @field code Character \{"SCHEMA_INVALID", "OUTCOME_MISSING", "OUTCOME_TYPE_MISMATCH", "RESAMPLE_MIN_CLASS", "RESAMPLE_N_ROWS", "FEATURE_CONSTANT", "FEATURE_TYPE_UNSUPPORTED", "DIM_P_GT_N", "MISSING_INCOMPATIBLE"\}:
#'   Stable identifier for the kind of finding. Permanent once published; a
#'   client may branch on it.
#' @field severity Character \{"error", "warning", "note"\}: What the finding
#'   means for running the config.
#' @field step Optional Integer [1, Inf): Position of the config in the plan it
#'   came from. NULL when the config was validated on its own.
#' @field message Character: Technical account, naming the fields involved.
#' @field plain Character: Plain-language account, authored per code and stored
#'   in `DIAGNOSTIC_PLAIN`.
#' @field evidence List: The measured values behind the finding, keyed by name.
#' @field fix Optional List vector: RFC 6902 JSON Patch against the config this
#'   finding is about, one element per operation. NULL where no deterministic
#'   fix exists.
#'
#' @author EDG
#' @noRd
Diagnostic <- new_class(
  name = "Diagnostic",
  package = "rtemis",
  properties = list(
    code = prop_string(
      DIAGNOSTIC_CODES[[1L]],
      enum = DIAGNOSTIC_CODES,
      description = "Stable identifier for the kind of finding."
    ),
    severity = prop_string(
      DIAGNOSTIC_SEVERITIES[[1L]],
      enum = DIAGNOSTIC_SEVERITIES,
      description = "What the finding means for running the config: 'error' the run fails or answers a different question, 'warning' it completes but likely not as wanted, 'note' worth knowing."
    ),
    step = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Position of the config in the plan it came from. NULL when the config was validated on its own."
    ),
    message = prop_string(
      "",
      description = "Technical account of the finding, naming the fields involved."
    ),
    plain = prop_string(
      "",
      description = "Plain-language account of the finding, written for a reader with no statistics background."
    ),
    evidence = prop_bag(
      description = "The measured values behind the finding, keyed by name."
    ),
    # An RFC 6902 patch is an array of operation objects, and an operation's
    # `value` is any JSON value at all -- an integer for a fold count, an array
    # of names for dropped columns. No leaf type covers that, so each operation
    # is declared as an object and RFC 6902 remains the authority on what is
    # inside it.
    fix = prop_array(
      items = prop_bag(),
      nullable = TRUE,
      description = "RFC 6902 JSON Patch against the config this finding is about. NULL where no deterministic fix exists."
    )
  )
) # /rtemis::Diagnostic


# %% new_diagnostic ----
#' Build a `Diagnostic`, taking its plain text from the code
#'
#' The single constructor every check calls, so `plain` cannot be composed at a
#' call site: it is looked up from `DIAGNOSTIC_PLAIN` by code, which is what
#' makes it authored data rather than runtime output.
#'
#' @param code Character: One of `DIAGNOSTIC_CODES`.
#' @param severity Character \{"error", "warning", "note"\}: Severity.
#' @param message Character: Technical account of the finding.
#' @param evidence List: The measured values behind the finding.
#' @param fix Optional List: RFC 6902 JSON Patch.
#' @param step Optional Integer [1, Inf): Position in the plan.
#'
#' @return `Diagnostic` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
new_diagnostic <- function(
  code,
  severity,
  message,
  evidence = list(),
  fix = NULL,
  step = NULL
) {
  Diagnostic(
    code = code,
    severity = severity,
    step = step,
    message = message,
    plain = unname(DIAGNOSTIC_PLAIN[[code]]),
    evidence = evidence,
    fix = fix
  )
} # /rtemis::new_diagnostic


# %% repr.Diagnostic ----
#' repr Diagnostic
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Diagnostic) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name("Diagnostic", pad = pad, output_type = output_type),
    repr_ls(
      list(
        code = x@code,
        severity = x@severity,
        step = x@step,
        message = x@message,
        evidence = x@evidence,
        fix = if (is.null(x@fix)) NULL else paste(length(x@fix), "operations")
      ),
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.Diagnostic


# %% print.Diagnostic ----
#' Print `Diagnostic`
#'
#' @param x `Diagnostic` object.
#' @param pad Integer: Left padding.
#' @param output_type Optional Character: Output format.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, Diagnostic) <- function(x, pad = 0L, output_type = NULL, ...) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.Diagnostic


# %% Diagnostics ----
#' Diagnostics Class
#'
#' @description
#' What [validate_config] returns: the findings for one config, in the order
#' they were made. An empty `Diagnostics` means the config is clean -- there is
#' no separate "valid" flag, because an empty list of problems is the same
#' statement and cannot disagree with itself.
#'
#' @field diagnostics List of `Diagnostic` objects: The findings.
#'
#' @author EDG
#' @noRd
Diagnostics <- new_class(
  name = "Diagnostics",
  package = "rtemis",
  properties = list(
    # A list of S7 objects has no `prop_*` factory -- the same position
    # `RegressionMetricsRes@res_metrics` is in -- so it is declared plainly and
    # published through the registry's `array_refs`, which points its schema at
    # `diagnostic/v1`.
    diagnostics = new_property(class_list)
  ),
  constructor = function(diagnostics = list()) {
    for (d in diagnostics) {
      check_is_S7(d, Diagnostic)
    }
    new_object(S7_object(), diagnostics = diagnostics)
  }
) # /rtemis::Diagnostics


# %% length.Diagnostics ----
#' Number of findings
#'
#' @param x `Diagnostics` object.
#'
#' @return Integer.
#'
#' @author EDG
#' @noRd
method(length, Diagnostics) <- function(x) {
  length(x@diagnostics)
} # /rtemis::length.Diagnostics


# %% `[[`.Diagnostics ----
# Reach one finding by position.
method(`[[`, Diagnostics) <- function(x, i) {
  x@diagnostics[[i]]
} # /rtemis::`[[`.Diagnostics


# %% diagnostic_codes ----
#' Codes present in a `Diagnostics`
#'
#' @param x `Diagnostics` object.
#'
#' @return Character vector, one entry per finding, in order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
diagnostic_codes <- function(x) {
  vapply(x@diagnostics, function(d) d@code, character(1L))
} # /rtemis::diagnostic_codes


# %% has_errors ----
#' Does a `Diagnostics` carry any finding of severity "error"?
#'
#' The question a caller deciding whether to run asks. Warnings and notes do not
#' block a run, so they do not answer it.
#'
#' @param x `Diagnostics` object.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
has_errors <- function(x) {
  any(vapply(x@diagnostics, function(d) d@severity == "error", logical(1L)))
} # /rtemis::has_errors


# %% repr.Diagnostics ----
#' repr Diagnostics
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Diagnostics) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("Diagnostics", pad = pad, output_type = output_type)
  n <- length(x@diagnostics)
  if (n == 0L) {
    return(paste0(
      out,
      fmt(
        "  No problems found.\n",
        col = rtemis_colors[["green"]],
        bold = TRUE,
        pad = pad,
        output_type = output_type
      )
    ))
  }
  lines <- vapply(
    x@diagnostics,
    function(d) {
      col <- switch(
        d@severity,
        error = rtemis_colors[["red"]],
        warning = rtemis_colors[["orange"]],
        rtemis_colors[["blue"]]
      )
      paste0(
        "  * ",
        fmt(
          d@code,
          col = col,
          bold = TRUE,
          pad = pad,
          output_type = output_type
        ),
        " (",
        d@severity,
        "): ",
        d@message
      )
    },
    character(1L)
  )
  paste0(out, paste(lines, collapse = "\n"), "\n")
} # /rtemis::repr.Diagnostics


# %% print.Diagnostics ----
#' Print `Diagnostics`
#'
#' @param x `Diagnostics` object.
#' @param pad Integer: Left padding.
#' @param output_type Optional Character: Output format.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, Diagnostics) <- function(x, pad = 0L, output_type = NULL, ...) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.Diagnostics
