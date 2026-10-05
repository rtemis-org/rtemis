# 285_AISupervisedReview.R
# ::rtemis::
# 2026- EDG rtemis.org

# What `ai_review()` returns: a written assessment of a `SupervisedReview` by
# a language model, kept together with the review it was based on and a
# record of how it was produced.
#
# The deterministic review is the evidence; the written assessment is an
# interpretation of it. Every statement cites the codes of the findings it
# rests on, and a cited code that is not in the review is rejected before the
# object is built, so a reader can always trace a statement back to a finding
# whose values are in `review`. The provenance -- model, provider, settings,
# the full prompt, a hash of the review -- makes the interpretation
# reproducible as far as the model allows and auditable regardless.
#
# spec: rtemis/review-method

# %% AIReviewItem ----
#' AIReviewItem Class
#'
#' @description
#' One statement of an `AISupervisedReview`, with the codes of the review
#' findings it rests on.
#'
#' @field text Character: The statement.
#' @field codes Optional Character \{"NO_TEST_SET", "SINGLE_SPLIT", "TEST_PRECISION", "ABSENT_TEST_CLASSES", "FOLD_VARIATION", "DIM_P_GT_N", "FEW_CASES_PER_PREDICTOR", "PRESELECTION_RISK", "CONSTANT_PREDICTIONS", "CLASS_NEVER_PREDICTED", "BASELINE_ACCURACY", "BASELINE_BALANCED_ACCURACY", "BASELINE_AUC", "BASELINE_BRIER", "BASELINE_MSE", "GENERALIZATION_GAP", "TUNING_GRID_EDGE"\} vector:
#'   Codes of the findings it rests on; NULL for a statement about a reported
#'   value with no finding.
#'
#' @author EDG
#' @keywords internal
#' @noRd
AIReviewItem <- schema_class(
  name = "AIReviewItem",
  package = "rtemis",
  properties = list(
    text = prop_string("", description = "The statement."),
    codes = prop_string(
      NULL,
      enum = REVIEW_CODES,
      vector = TRUE,
      unique_items = TRUE,
      nullable = TRUE,
      description = "Codes of the review findings the statement rests on. Unset for a statement about a reported value with no finding."
    )
  ),
  publication = SchemaPublication(
    role = "document",
    slug = "aireviewitem",
    title = "rtemis AIReviewItem",
    description = "One statement of a language-model assessment of a supervised model review, with the codes of the review findings it rests on.",
    order = 30L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::AIReviewItem


# %% AISupervisedReview ----
#' AISupervisedReview Class
#'
#' @description
#' A language model's written assessment of a `SupervisedReview`: a summary,
#' an evaluation and next steps whose statements cite review findings, and
#' caveats, with the review itself and a record of how the text was produced.
#'
#' @field review `SupervisedReview`: The review the assessment is based on.
#' @field context Optional Character: Domain context supplied by the analyst.
#' @field summary Character: Short summary.
#' @field evaluation List of `AIReviewItem`: What the review shows.
#' @field next_steps List of `AIReviewItem`: Suggested actions.
#' @field caveats Optional Character vector: What the assessment cannot
#'   establish.
#' @field provenance List: Model, provider, temperature, package version,
#'   prompt, review hash and creation time.
#'
#' @author EDG
#' @noRd
AISupervisedReview <- schema_class(
  name = "AISupervisedReview",
  package = "rtemis",
  properties = list(
    review = prop_object(
      SupervisedReview,
      description = "The deterministic review the assessment is based on."
    ),
    context = prop_string(
      NULL,
      nullable = TRUE,
      description = "Domain context supplied by the analyst. Unset when none was given."
    ),
    summary = prop_string("", description = "Short summary of the assessment."),
    evaluation = prop_state(prop_collection(
      AIReviewItem,
      description = "What the review shows, one statement per item."
    )),
    next_steps = prop_state(prop_collection(
      AIReviewItem,
      description = "Suggested actions, most valuable first."
    )),
    caveats = prop_string(
      NULL,
      vector = TRUE,
      nullable = TRUE,
      description = "What the assessment cannot establish. Unset when the model stated none."
    ),
    provenance = prop_state(prop_struct(
      members = list(
        model = prop_string(description = "Model that wrote the assessment."),
        provider = prop_string(
          description = "Interface the model was called through."
        ),
        temperature = prop_float(
          NULL,
          min = 0,
          nullable = TRUE,
          description = "Sampling temperature. Unset when the provider default was used."
        ),
        package_version = prop_string(
          description = "Version of the package that called the model."
        ),
        prompt = prop_string(
          description = "The full prompt sent to the model."
        ),
        review_sha256 = prop_string(
          description = "SHA-256 hash of the serialized review sent to the model."
        ),
        created = prop_string(
          description = "Creation time, ISO 8601 in UTC."
        )
      ),
      required = c(
        "model",
        "provider",
        "temperature",
        "package_version",
        "prompt",
        "review_sha256",
        "created"
      ),
      description = "How the assessment was produced."
    ))
  ),
  constructor = function(
    review,
    summary,
    evaluation,
    next_steps,
    caveats,
    provenance,
    context = NULL
  ) {
    new_object(
      S7_object(),
      review = review,
      context = context,
      summary = summary,
      evaluation = evaluation,
      next_steps = next_steps,
      caveats = caveats,
      provenance = provenance
    )
  },
  publication = SchemaPublication(
    role = "document",
    slug = "aisupervisedreview",
    title = "rtemis AISupervisedReview",
    description = "A language model's written assessment of a supervised model review: a summary, an evaluation and next steps whose statements cite the review's finding codes, and caveats, with the review itself and a record of the model, settings, prompt and review hash that produced it.",
    order = 31L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::AISupervisedReview


# %% ai_review_byline ----
# Who wrote the assessment, from which review, and when.
ai_review_byline <- function(x) {
  p <- x@provenance
  paste0(
    "Written by ",
    p[["model"]],
    " (",
    p[["provider"]],
    ") from the review of ",
    x@review@algorithm,
    ", ",
    p[["created"]],
    ". An interpretation; the review holds the evidence."
  )
} # /rtemis::ai_review_byline


# %% repr_ai_review_items ----
# One bullet per item, its cited codes in brackets.
repr_ai_review_items <- function(items, indent, output_type) {
  vapply(
    items,
    function(item) {
      paste0(
        indent,
        "  * ",
        item@text,
        if (length(item@codes) > 0L) {
          fmt(
            paste0(" [", paste(item@codes, collapse = ", "), "]"),
            muted = TRUE,
            output_type = output_type
          )
        }
      )
    },
    character(1L)
  )
} # /rtemis::repr_ai_review_items


# %% repr.AISupervisedReview ----
#' repr AISupervisedReview
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, AISupervisedReview) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  indent <- strrep(" ", pad + 2L)
  heading <- function(text) {
    paste0(
      "\n",
      indent,
      fmt(text, bold = TRUE, output_type = output_type),
      "\n"
    )
  }
  out <- paste0(
    repr_S7name("AISupervisedReview", pad = pad, output_type = output_type),
    indent,
    fmt(ai_review_byline(x), muted = TRUE, output_type = output_type),
    "\n",
    heading("Summary"),
    indent,
    "  ",
    x@summary,
    "\n"
  )
  if (length(x@evaluation) > 0L) {
    out <- paste0(
      out,
      heading("Evaluation"),
      paste(
        repr_ai_review_items(x@evaluation, indent, output_type),
        collapse = "\n"
      ),
      "\n"
    )
  }
  if (length(x@next_steps) > 0L) {
    out <- paste0(
      out,
      heading("Next steps"),
      paste(
        repr_ai_review_items(x@next_steps, indent, output_type),
        collapse = "\n"
      ),
      "\n"
    )
  }
  if (length(x@caveats) > 0L) {
    out <- paste0(
      out,
      heading("Caveats"),
      paste0(indent, "  - ", x@caveats, collapse = "\n"),
      "\n"
    )
  }
  out
} # /rtemis::repr.AISupervisedReview


# %% print.AISupervisedReview ----
#' Print `AISupervisedReview`
#'
#' @param x `AISupervisedReview` object.
#' @param pad Integer: Left padding.
#' @param output_type Optional Character: Output format.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, AISupervisedReview) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.AISupervisedReview


# %% to_markdown.AISupervisedReview ----
#' Render an AI review as Markdown
#'
#' The byline, summary, evaluation, next steps and caveats, each item followed
#' by the finding codes it cites, then the review the assessment rests on,
#' whose sections are one heading level lower.
#'
#' @param x `AISupervisedReview` object.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_markdown, AISupervisedReview) <- function(x, ...) {
  section <- function(title, body) c("", md_heading(title, 2L), "", body)
  items <- function(items) {
    vapply(
      items,
      function(item) {
        paste0(
          "- ",
          item@text,
          if (length(item@codes) > 0L) {
            paste0(" [", paste0("`", item@codes, "`", collapse = ", "), "]")
          }
        )
      },
      character(1L)
    )
  }
  out <- c(
    paste0("*", ai_review_byline(x), "*"),
    section("Summary", x@summary)
  )
  if (length(x@evaluation) > 0L) {
    out <- c(out, section("Evaluation", items(x@evaluation)))
  }
  if (length(x@next_steps) > 0L) {
    out <- c(out, section("Next steps", items(x@next_steps)))
  }
  if (length(x@caveats) > 0L) {
    out <- c(out, section("Caveats", paste0("- ", x@caveats)))
  }
  out <- c(out, section("Review", review_markdown(x@review, level = 3L)))
  paste0(paste(out, collapse = "\n"), "\n")
} # /rtemis::to_markdown.AISupervisedReview
