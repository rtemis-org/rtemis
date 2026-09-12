# defaults-conformance.R
# ::rtemis::
# 2026- EDG rtemis.org

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
cases <- list()
add <- function(name, slug, cls, input, context = NULL) {
  result <- resolve_config_defaults(cls, input, context)
  wire_input <- lapply(names(input), function(nm) {
    if (
      !S7_inherits(input[[nm]]) &&
        !is.null(get_spec_fields(cls@properties[[nm]])[["target_class"]])
    ) {
      input[[nm]]
    } else {
      default_wire_value(input[[nm]], get_spec_fields(cls@properties[[nm]]))
    }
  })
  names(wire_input) <- names(input)
  if (!length(wire_input)) {
    names(wire_input) <- character()
  }
  origins <- result[["origins"]]
  if (!length(origins)) {
    names(origins) <- character()
  }
  cases[[name]] <<- list(
    name = name,
    schema = paste0("https://schema.rtemis.org/", slug, "/v1/schema.json"),
    input = wire_input,
    context = context,
    expected = list(
      values = result[["values"]],
      origins = origins,
      pending = I(sort(names(result[["pending"]]) %||% character())),
      complete = result[["complete"]]
    )
  )
}
for (scale in c(FALSE, TRUE)) {
  add(
    paste0("preprocessor-scale-", scale),
    "preprocessor",
    PreprocessorConfig,
    list(scale = scale)
  )
  stopifnot(identical(setup_Preprocessor(scale = scale)@center, scale))
}
add(
  "preprocessor-override",
  "preprocessor",
  PreprocessorConfig,
  list(scale = TRUE, center = FALSE)
)
add(
  "preprocessor-explicit-null",
  "preprocessor",
  PreprocessorConfig,
  list(center = NULL)
)
for (replace in c(FALSE, TRUE)) {
  add(
    paste0("ranger-replace-", replace),
    "hyperparameters",
    RangerHyperparameters,
    list(algorithm = "Ranger", replace = replace)
  )
  stopifnot(identical(
    setup_Ranger(replace = replace)@sample_fraction,
    if (replace) 1 else 0.632
  ))
}
add(
  "ranger-override",
  "hyperparameters",
  RangerHyperparameters,
  list(algorithm = "Ranger", replace = FALSE, sample_fraction = 0.8)
)
add(
  "ranger-candidates",
  "hyperparameters",
  RangerHyperparameters,
  list(algorithm = "Ranger", replace = tune_over(FALSE, TRUE))
)
add(
  "tsne-null",
  "decomposition",
  tSNEConfig,
  list(algorithm = "tSNE", Y_init = NULL),
  list(verbose = FALSE)
)
add(
  "tsne-initialized",
  "decomposition",
  tSNEConfig,
  list(algorithm = "tSNE", Y_init = matrix(0, 2L, 2L)),
  list(verbose = FALSE)
)
add(
  "future-pending",
  "execution",
  FutureExecutionConfig,
  list(backend = "future")
)
add(
  "future-context",
  "execution",
  FutureExecutionConfig,
  list(backend = "future"),
  list(seed = 19L, n_workers = 2L, future_plan = "sequential")
)
add(
  "future-null-seed",
  "execution",
  FutureExecutionConfig,
  list(backend = "future", seed = NULL),
  list(seed = 19L, n_workers = 2L, future_plan = "sequential")
)
add("grid-inner-default", "tuner", GridSearchConfig, list(type = "GridSearch"))
add(
  "grid-inner-supplied",
  "tuner",
  GridSearchConfig,
  list(
    type = "GridSearch",
    resampler_config = list(type = "KFold", n_resamples = 7L)
  )
)
add(
  "superlearner-library",
  "hyperparameters",
  SuperLearnerHyperparameters,
  list(algorithm = "SuperLearner")
)
add(
  "superlearner-custom-membership",
  "hyperparameters",
  SuperLearnerHyperparameters,
  list(
    algorithm = "SuperLearner",
    base_learners = list(
      first = list(algorithm = "GLM"),
      second = list(algorithm = "GLM")
    )
  )
)
add("loocv-observed-count", "resampler", LOOCVConfig, list(type = "LOOCV"))
add("fingerprint-no-fill", "datafingerprint", DataFingerprint, list())
for (slug in c("delimited", "xlsx")) {
  family <- schema_catalog()[["families"]][["ingest"]]
  leaf <- Filter(
    function(x) {
      tolower(discriminator_value(x[["cls"]], family[["discriminator"]])) ==
        slug
    },
    family[["algorithms"]]
  )[[1L]][["cls"]]
  input <- stats::setNames(
    list(discriminator_value(leaf, family[["discriminator"]])),
    family[["discriminator"]]
  )
  add(paste0("ingest-", slug), "ingest", leaf, input)
}
jsonlite::write_json(
  list(format_version = 1L, cases = unname(cases)),
  args[[1L]],
  auto_unbox = TRUE,
  null = "null",
  pretty = TRUE,
  digits = NA
)
cat(length(cases), "R defaults conformance cases written to", args[[1L]], "\n")
