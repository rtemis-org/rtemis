# test_AIReview.R
# ::rtemis::
# 2026- EDG rtemis.org

# `ai_review()` without a model server: rtemis.llm's `generate()` is replaced
# by a stub that records what it was sent and returns a fixed answer, so the
# tests check what rtemis sends, how it checks the answer, and what it keeps.

testthat::skip_if_not_installed("rtemis.llm")
testthat::skip_if_not_installed("jsonlite")

# %% helpers ----
.idx <- c(1:40, 51:90, 101:140)
.mod <- train(
  iris[.idx, ],
  dat_test = iris[-.idx, ],
  hyperparameters = setup_CART(),
  verbosity = 0L
)
.rev <- review(.mod)

# An Ollama model object built without a server.
.llm <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    ollama_check_model = function(...) invisible(TRUE),
    .package = "rtemis.llm",
    .env = env
  )
  rtemis.llm:::Ollama(
    config = rtemis.llm:::OllamaConfig(
      model_name = "test-model",
      temperature = 0.3,
      base_url = "http://localhost:11434"
    ),
    system_prompt = "You are helpful."
  )
}

# Replace generate() with a stub returning `answer`, a list serialized as the
# model's JSON content; what generate() received is kept in `sent`.
.stub_generate <- function(answer, env = parent.frame()) {
  sent <- new.env()
  testthat::local_mocked_bindings(
    generate = function(x, prompt, ...) {
      sent[["prompt"]] <- prompt
      sent[["args"]] <- list(...)
      content <- if (is.character(answer)) {
        answer
      } else {
        as.character(jsonlite::toJSON(answer, auto_unbox = TRUE))
      }
      rtemis.llm:::LLMMessage(content = content, model_name = "test-model")
    },
    .package = "rtemis.llm",
    .env = env
  )
  sent
}

.answer <- function(codes = c("SINGLE_SPLIT", "BASELINE_ACCURACY")) {
  list(
    summary = "The model separates the three species well on 30 test cases.",
    evaluation = list(
      list(
        text = "Accuracy is clearly above always predicting one class.",
        codes = list(codes[[2L]])
      ),
      list(text = "Training and test accuracy are close.", codes = list())
    ),
    next_steps = list(
      list(
        text = "Use outer resampling to estimate variation between splits.",
        codes = list(codes[[1L]])
      )
    ),
    caveats = list("Usefulness cannot be assessed without context.")
  )
}


# %% Success ----
test_that("ai_review() builds an AISupervisedReview from a checked answer", {
  llm <- .llm()
  sent <- .stub_generate(.answer())
  out <- ai_review(.rev, llm = llm, verbosity = 0L)
  expect_s7_class(out, AISupervisedReview)
  expect_identical(out@review, .rev)
  expect_null(out@context)
  expect_match(out@summary, "separates", fixed = TRUE)
  expect_length(out@evaluation, 2L)
  expect_identical(out@evaluation[[1L]]@codes, "BASELINE_ACCURACY")
  # An item about a reported value with no finding cites none.
  expect_null(out@evaluation[[2L]]@codes)
  expect_identical(out@next_steps[[1L]]@codes, "SINGLE_SPLIT")
  expect_identical(
    out@caveats,
    "Usefulness cannot be assessed without context."
  )
})

test_that("the provenance records model, provider, settings, prompt and review hash", {
  llm <- .llm()
  sent <- .stub_generate(.answer())
  out <- ai_review(.rev, llm = llm, verbosity = 0L)
  p <- out@provenance
  expect_identical(p[["model"]], "test-model")
  expect_identical(p[["provider"]], "Ollama")
  # The model's configured temperature, since none was given for the call.
  expect_identical(p[["temperature"]], 0.3)
  expect_match(p[["package_version"]], "^rtemis.llm ")
  expect_identical(p[["prompt"]], sent[["prompt"]])
  review_json <- as.character(jsonlite::toJSON(
    record_object(.rev),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = 4L
  ))
  expect_identical(
    p[["review_sha256"]],
    as.character(openssl::sha256(review_json))
  )
  expect_match(p[["created"]], "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  # A per-call temperature is recorded as given.
  out_t <- ai_review(.rev, llm = llm, temperature = 0, verbosity = 0L)
  expect_identical(out_t@provenance[["temperature"]], 0)
  expect_identical(sent[["args"]][["temperature"]], 0)
})

test_that("the prompt carries the review and the context, and no data", {
  llm <- .llm()
  sent <- .stub_generate(.answer())
  ai_review(.rev, llm = llm, verbosity = 0L)
  expect_match(sent[["prompt"]], "No context was given", fixed = TRUE)
  expect_match(sent[["prompt"]], "\"findings\"", fixed = TRUE)
  # The review holds aggregates only; case-level vectors are not sent.
  expect_false(grepl("y_test|predicted_test|Sepal", sent[["prompt"]]))
  context <- "Screening tool; missing a virginica case is costly."
  out <- ai_review(.rev, llm = llm, context = context, verbosity = 0L)
  expect_identical(out@context, context)
  expect_match(sent[["prompt"]], context, fixed = TRUE)
  expect_false(grepl("No context was given", sent[["prompt"]], fixed = TRUE))
})

test_that("the output schema restricts codes to the review's findings", {
  llm <- .llm()
  sent <- .stub_generate(.answer())
  ai_review(.rev, llm = llm, verbosity = 0L)
  schema <- rtemis.llm::as_list(sent[["args"]][["output_schema"]])
  code_enum <- schema[["properties"]][["evaluation"]][["items"]][[
    "properties"
  ]][["codes"]][["items"]][["enum"]]
  expect_setequal(unlist(code_enum), review_codes(.rev))
  expect_identical(sent[["args"]][["on_validation_failure"]], "abort")
})

test_that("ai_review() on a model reviews it first", {
  llm <- .llm()
  .stub_generate(.answer())
  out <- ai_review(.mod, llm = llm, verbosity = 0L)
  expect_identical(record_object(out@review), record_object(review(.mod)))
})


# %% Rejected answers ----
test_that("an answer citing a code that is not a finding is rejected", {
  llm <- .llm()
  .stub_generate(.answer(codes = c("SINGLE_SPLIT", "GENERALIZATION_GAP")))
  expect_error(
    ai_review(.rev, llm = llm, verbosity = 0L),
    "GENERALIZATION_GAP",
    class = "rtemis_llm_output_error"
  )
})

test_that("an answer that is not JSON or has no summary is rejected", {
  llm <- .llm()
  .stub_generate("not json")
  expect_error(
    ai_review(.rev, llm = llm, verbosity = 0L),
    class = "rtemis_llm_output_error"
  )
  no_summary <- .answer()
  no_summary[["summary"]] <- ""
  .stub_generate(no_summary)
  expect_error(
    ai_review(.rev, llm = llm, verbosity = 0L),
    "no summary",
    class = "rtemis_llm_output_error"
  )
})

test_that("ai_review() checks its inputs", {
  expect_error(
    ai_review(.rev, llm = "gpt", verbosity = 0L),
    class = "rtemis_type_error"
  )
  llm <- .llm()
  expect_error(ai_review(.rev, llm = llm, context = c("a", "b")))
  expect_error(ai_review(.rev, llm = llm, temperature = -1))
})


# %% Contract ----
test_that("repr shows the assessment with its cited codes", {
  llm <- .llm()
  .stub_generate(.answer())
  out <- repr(ai_review(.rev, llm = llm, verbosity = 0L), output_type = "plain")
  expect_lt(regexpr("Summary", out), regexpr("Evaluation", out))
  expect_lt(regexpr("Evaluation", out), regexpr("Next steps", out))
  expect_match(out, "[BASELINE_ACCURACY]", fixed = TRUE)
  expect_match(out, "test-model (Ollama)", fixed = TRUE)
})

test_that("an AI review record validates against its published schema", {
  testthat::skip_if_not_installed("jsonvalidate")
  llm <- .llm()
  .stub_generate(.answer())
  doc <- record_object(ai_review(.rev, llm = llm, verbosity = 0L))
  schema_of <- function(cls) {
    schema <- S7_to_JSONSchema(
      cls,
      id = paste0("https://schema.rtemis.org/test/", tolower(cls@name), ".json")
    )
    schema[["$id"]] <- NULL
    schema[["properties"]][["$schema"]] <- NULL
    schema
  }
  targets <- list(
    reviewfinding = schema_of(ReviewFinding),
    aireviewitem = schema_of(AIReviewItem)
  )
  inline <- function(node) {
    if (!is.list(node)) {
      return(node)
    }
    ref <- node[["$ref"]]
    if (is.character(ref) && length(ref) == 1L && startsWith(ref, "https://")) {
      for (slug in names(targets)) {
        if (grepl(paste0("/", slug, "/"), ref, fixed = TRUE)) {
          return(targets[[slug]])
        }
      }
      if (grepl("/supervisedreview/", ref, fixed = TRUE)) {
        return(inline(schema_of(SupervisedReview)))
      }
      stop("unexpected reference: ", ref)
    }
    lapply(node, inline)
  }
  validate <- jsonvalidate::json_validator(
    jsonlite::toJSON(
      inline(schema_of(AISupervisedReview)),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    ),
    engine = "ajv"
  )
  as_json <- function(d) {
    jsonlite::toJSON(
      d,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    )
  }
  expect_true(validate(as_json(doc), verbose = TRUE))
  # Negative case: a cited code outside the vocabulary.
  bad <- doc
  bad[["evaluation"]][[1L]][["codes"]] <- list("NOT_A_CODE")
  expect_false(validate(as_json(bad)))
})


# %% Markdown ----
test_that("Markdown shows the assessment, its codes, then the review", {
  llm <- .llm()
  .stub_generate(.answer())
  out <- ai_review(.rev, llm = llm, verbosity = 0L)
  md_text <- to_markdown(out)
  md <- strsplit(md_text, "\n", fixed = TRUE)[[1L]]
  expect_identical(md[[1L]], paste0("*", ai_review_byline(out), "*"))
  expect_identical(
    md[startsWith(md, "## ")],
    c("## Summary", "## Evaluation", "## Next steps", "## Caveats", "## Review")
  )
  expect_true(any(grepl("[`BASELINE_ACCURACY`]", md, fixed = TRUE)))
  expect_true(any(grepl("[`SINGLE_SPLIT`]", md, fixed = TRUE)))
  # The review follows, one heading level lower.
  expect_match(
    md_text,
    paste(review_markdown(.rev, level = 3L), collapse = "\n"),
    fixed = TRUE
  )
  expect_true("### Findings" %in% md)
})
