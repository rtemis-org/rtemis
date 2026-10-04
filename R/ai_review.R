# ai_review.R
# ::rtemis::
# 2026- EDG rtemis.org

# `ai_review()`: a language model writes an assessment of a `SupervisedReview`.
# The model receives the review as JSON and the analyst's context, and answers
# in a declared structure whose statements cite finding codes. The answer is checked against the review before an
# `AISupervisedReview` is built. The classes live in `285_AISupervisedReview.R`.
#
# spec: rtemis/review-method

# %% AI_REVIEW_INSTRUCTIONS ----
# The fixed part of the prompt. The analyst's context and the review follow it.
AI_REVIEW_INSTRUCTIONS <- paste(
  "You are writing an assessment of a trained supervised learning model for",
  "the analyst who trained it. Your only sources are the review below,",
  "produced deterministically by rtemis, and the analyst's context, if given.",
  "",
  "Rules:",
  "- Use only values that appear in the review, rounded to at most three",
  "  decimal places. Do not compute, estimate or invent numbers.",
  "- Each item in `evaluation` and `next_steps` lists in `codes` the codes of",
  "  the review findings it rests on, using only codes that appear under",
  "  `findings`. An item about a reported value with no finding lists none.",
  "- For a single split, the review reports confidence intervals and",
  "  verdicts. For a resampled model it reports the variation between",
  "  resamples and makes no interval or test; do not describe resampled",
  "  results as statistically significant or not.",
  "- Distinguish what the review observed from possible explanations.",
  "- Whether the model is useful depends on the question it addresses, the",
  "  costs of different errors and how its predictions will be used.",
  "- Preprocessing, decomposition and tuning in rtemis are fitted on the",
  "  training cases of each split. Steps taken before the data was passed to",
  "  rtemis cannot be seen in the review.",
  "- Next steps are concrete actions, most valuable first, in rtemis terms",
  "  where possible: outer resampling, tuning hyperparameters, a",
  "  decomposition step, a different algorithm, more data. Do not suggest",
  "  what the evaluation design below says was already done.",
  "- Cite a code only for a statement that the finding supports, and only in",
  "  `codes`, never in the text.",
  "- `caveats` states what this assessment cannot establish for this model.",
  "- Write plainly and neutrally, for a reader who may not be a statistician.",
  sep = "\n"
)


# %% ai_review_output_schema ----
#' The structure the model answers in
#'
#' Codes are an enumeration of the codes present in the review, so a model
#' that honors the schema can cite no other; `ai_review()` checks anyway.
#'
#' @param codes Character: Finding codes present in the review.
#'
#' @return rtemis.llm `Schema`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ai_review_output_schema <- function(codes) {
  code_items <- if (length(codes) > 0L) {
    rtemis.llm::field(
      "code",
      description = "Code of a review finding.",
      type = "string",
      enum = codes
    )
  } else {
    "string"
  }
  item <- function(what) {
    rtemis.llm::schema(
      name = NULL,
      rtemis.llm::field("text", description = what, type = "string"),
      rtemis.llm::field(
        "codes",
        description = "Codes of the review findings this rests on; empty if none.",
        type = "array",
        items = code_items
      )
    )
  }
  rtemis.llm::schema(
    name = "ai_review",
    rtemis.llm::field(
      "summary",
      description = "Two to four sentences on what the review shows.",
      type = "string"
    ),
    rtemis.llm::field(
      "evaluation",
      description = "What the review shows about the evaluation and the model.",
      type = "array",
      items = item("One statement of the evaluation.")
    ),
    rtemis.llm::field(
      "next_steps",
      description = "Suggested actions, most valuable first.",
      type = "array",
      items = item("One suggested action and why.")
    ),
    rtemis.llm::field(
      "caveats",
      description = "What this assessment cannot establish.",
      type = "array",
      items = "string"
    )
  )
} # /rtemis::ai_review_output_schema


# %% ai_review_design ----
#' The evaluation design, stated for the model
#'
#' The design determines what the assessment may claim and which steps were
#' already taken, so it is stated explicitly in the prompt.
#'
#' @param x `SupervisedReview` object.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ai_review_design <- function(x) {
  s <- x@sample
  if (!is.null(s[["n_resamples"]])) {
    paste0(
      "outer resampling with ",
      s[["n_resamples"]],
      " resamples was already used. The review describes the variation ",
      "between resamples and reports no confidence interval, test or ",
      "verdict; do not use the words significant or significantly, and do ",
      "not suggest outer resampling."
    )
  } else if (!is.null(s[["n_test"]])) {
    paste0(
      "a single split into ",
      s[["n_training"]],
      " training and ",
      s[["n_test"]],
      " test cases. The review reports confidence intervals and verdicts at ",
      "the ",
      format(x@confidence_level * 100, digits = 15L),
      "% level for this split."
    )
  } else {
    paste0(
      "no test set: the model was evaluated on its ",
      s[["n_training"]],
      " training cases only, so performance on new cases is unknown."
    )
  }
} # /rtemis::ai_review_design


# %% ai_review_prompt ----
#' Assemble the prompt
#'
#' @param review_json Character: The review, serialized.
#' @param context Optional Character: Analyst's context.
#' @param design Character: The evaluation design, from `ai_review_design()`.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ai_review_prompt <- function(review_json, context, design) {
  paste0(
    AI_REVIEW_INSTRUCTIONS,
    "\n\n",
    "Evaluation design: ",
    design,
    "\n\n",
    if (is.null(context)) {
      paste0(
        "No context was given. State that usefulness cannot be assessed ",
        "without knowing the question and the costs of errors, and what ",
        "information would allow it.\n\n"
      )
    } else {
      paste0(
        "Context from the analyst. Relate the summary and the evaluation to ",
        "it, without going beyond what the review supports:\n",
        context,
        "\n\n"
      )
    },
    "Review (JSON):\n",
    review_json
  )
} # /rtemis::ai_review_prompt


# %% ai_review_items ----
#' Convert parsed items to `AIReviewItem` objects, checking cited codes
#'
#' @param items List: Parsed array of items.
#' @param codes Character: Codes present in the review.
#' @param where Character: Field name, for error messages.
#'
#' @return List of `AIReviewItem`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
ai_review_items <- function(items, codes, where) {
  lapply(items, function(item) {
    text <- item[["text"]]
    if (!is.character(text) || length(text) != 1L || !nzchar(text)) {
      rtemis.core::abort(
        "The model's `",
        where,
        "` contains an item without text.",
        class = c("rtemis_value_error", "rtemis_llm_output_error")
      )
    }
    cited <- unlist(item[["codes"]])
    unknown <- setdiff(cited, codes)
    if (length(unknown) > 0L) {
      rtemis.core::abort(
        "The model's `",
        where,
        "` cites codes that are not findings of this review: ",
        paste(unknown, collapse = ", "),
        ". The review's findings are: ",
        if (length(codes) > 0L) paste(codes, collapse = ", ") else "none",
        ".",
        class = c("rtemis_value_error", "rtemis_llm_output_error")
      )
    }
    AIReviewItem(
      text = text,
      codes = if (length(cited) > 0L) unique(as.character(cited))
    )
  })
} # /rtemis::ai_review_items


# %% ai_review_temperature ----
# The temperature the model will use: the per-call value, or the model's
# configured one where it has one.
ai_review_temperature <- function(llm, temperature) {
  if (!is.null(temperature)) {
    return(temperature)
  }
  config <- tryCatch(llm@config, error = function(e) NULL) %||%
    tryCatch(llm@llmconfig, error = function(e) NULL)
  tryCatch(config@temperature, error = function(e) NULL)
} # /rtemis::ai_review_temperature


# %% ai_review.SupervisedReview ----
method(ai_review, SupervisedReview) <- function(
  x,
  llm,
  context = NULL,
  temperature = NULL,
  verbosity = 1L,
  ...
) {
  check_dependencies("rtemis.llm", "jsonlite")
  if (!inherits(llm, c("rtemis.llm::LLM", "rtemis.llm::Agent"))) {
    rtemis.core::abort(
      "`llm` must be an rtemis.llm model or agent, for example ",
      "`rtemis.llm::create_Ollama()` or `rtemis.llm::create_Anthropic()`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.null(context)) {
    check_character_scalar(context)
  }
  if (!is.null(temperature)) {
    check_nonneg_double_scalar(temperature)
  }

  # Prompt ----
  # Four decimal places: enough for every reported value, and short enough
  # that the model does not copy long expansions into its text.
  review_json <- as.character(jsonlite::toJSON(
    record_object(x),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = 4L
  ))
  codes <- unique(review_codes(x))
  prompt <- ai_review_prompt(review_json, context, ai_review_design(x))

  # Generate ----
  response <- rtemis.llm::generate(
    llm,
    prompt = prompt,
    temperature = temperature,
    output_schema = ai_review_output_schema(codes),
    verbosity = verbosity,
    validate_output = TRUE,
    on_validation_failure = "abort"
  )
  parsed <- tryCatch(
    jsonlite::fromJSON(response@content, simplifyVector = FALSE),
    error = function(e) {
      rtemis.core::abort(
        "The model's answer is not valid JSON.",
        class = c("rtemis_value_error", "rtemis_llm_output_error")
      )
    }
  )
  summary <- parsed[["summary"]]
  if (!is.character(summary) || length(summary) != 1L || !nzchar(summary)) {
    rtemis.core::abort(
      "The model's answer has no summary.",
      class = c("rtemis_value_error", "rtemis_llm_output_error")
    )
  }
  caveats <- unlist(parsed[["caveats"]])

  AISupervisedReview(
    review = x,
    context = context,
    summary = summary,
    evaluation = ai_review_items(parsed[["evaluation"]], codes, "evaluation"),
    next_steps = ai_review_items(parsed[["next_steps"]], codes, "next_steps"),
    caveats = if (length(caveats) > 0L) as.character(caveats),
    provenance = list(
      model = response@model_name,
      provider = S7_class(llm)@name,
      temperature = ai_review_temperature(llm, temperature),
      package_version = paste0(
        "rtemis.llm ",
        as.character(utils::packageVersion("rtemis.llm"))
      ),
      prompt = prompt,
      review_sha256 = as.character(openssl::sha256(review_json)),
      created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  )
} # /rtemis::ai_review.SupervisedReview


# %% ai_review.Supervised ----
method(ai_review, Supervised) <- function(
  x,
  llm,
  context = NULL,
  temperature = NULL,
  verbosity = 1L,
  ...
) {
  ai_review(
    review(x),
    llm = llm,
    context = context,
    temperature = temperature,
    verbosity = verbosity
  )
} # /rtemis::ai_review.Supervised


# %% ai_review.SupervisedRes ----
method(ai_review, SupervisedRes) <- function(
  x,
  llm,
  context = NULL,
  temperature = NULL,
  verbosity = 1L,
  ...
) {
  ai_review(
    review(x),
    llm = llm,
    context = context,
    temperature = temperature,
    verbosity = verbosity
  )
} # /rtemis::ai_review.SupervisedRes
