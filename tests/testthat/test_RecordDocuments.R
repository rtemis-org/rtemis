# test_RecordDocuments.R
# ::rtemis::
# 2026- EDG rtemis.org

# Does a record rtemis *writes* validate against the schema rtemis *publishes*?
#
# `test_SchemaContract.R` asks structural questions -- does the schema have the
# right shape -- and answers them from the class alone. It never builds a
# document, so it cannot catch one that is well-shaped and still invalid. That
# question had only ever been answered downstream, in Rust, after a publish:
# `family_discriminator()` covered five of the ten families, so a record for
# any of the other five went out without its discriminator, matched no
# dispatcher branch, and was rejected by `unevaluatedProperties`.
#
# A dispatched family's document is the dispatcher's object composed with one
# leaf's, so it is validated against the two together. `jsonvalidate` resolves
# no `https://` reference, so each leaf is inlined into the dispatcher's
# `$defs` and every `then.$ref` retargeted at the inlined copy.

testthat::skip_if_not_installed("jsonvalidate")

# %% .stub_external_refs ----
# A nested config block is a document in its own right, validated by its own
# family's case below, and `jsonvalidate` resolves no `https://` reference. Its
# `$ref` becomes a bare object so the block's presence and shape still count
# without pulling in a second schema.
.stub_external_refs <- function(node) {
  if (!is.list(node)) {
    return(node)
  }
  ref <- node[["$ref"]]
  if (is.character(ref) && length(ref) == 1L && startsWith(ref, "https://")) {
    return(list(type = "object"))
  }
  lapply(node, .stub_external_refs)
}


# %% .record_bundle ----
# The dispatcher plus the one leaf the document dispatches to -- the only arm
# whose properties `unevaluatedProperties` must see evaluated. Built in process
# the way `data-raw/generate_schemas.R` builds them.
.record_bundle <- function(base, leaf, discriminator, family) {
  slug <- tolower(discriminator_value(leaf, discriminator))
  leaf_id <- paste0(
    "https://schema.rtemis.org/",
    family,
    "/",
    slug,
    "/v1/record.json"
  )
  disp <- S7_dispatcher_JSONSchema(
    classes = list(leaf),
    id = paste0("https://schema.rtemis.org/", family, "/v1/record.json"),
    discriminator = discriminator,
    base = base,
    record = TRUE,
    title = family,
    description = family,
    discriminator_description = discriminator
  )
  d <- S7_to_JSONSchema(
    leaf,
    id = leaf_id,
    title = leaf@name,
    description = leaf@name,
    base = base,
    record = TRUE,
    closed = FALSE
  )
  d[["$id"]] <- NULL
  d[["properties"]][["$schema"]] <- NULL
  d <- .stub_external_refs(d)
  disp[["$defs"]] <- stats::setNames(list(d), slug)
  disp[["$id"]] <- NULL
  disp[["allOf"]] <- lapply(disp[["allOf"]], function(clause) {
    if (!is.null(clause[["then"]][["$ref"]])) {
      clause[["then"]][["$ref"]] <- paste0("#/$defs/", slug)
    }
    clause
  })
  jsonvalidate::json_validator(
    jsonlite::toJSON(disp, auto_unbox = TRUE, null = "null"),
    engine = "ajv"
  )
}


# %% .cases ----
# One built config per family. `nested_record()` is the document producer:
# `config_record()` is the leaf fragment it composes onto the discriminator and
# the base's shared fields.
.cases <- function() {
  list(
    execution = setup_SerialExecution(seed = 1L),
    execution_parallel = setup_MiraiExecution(n_workers = 2L, seed = 1L),
    clustering = setup_KMeans(k = 3L),
    decomposition = setup_PCA(k = 2L),
    resampler = setup_KFold(3L),
    tuner = setup_GridSearch(resampler_config = setup_KFold(3L)),
    ingest = setup_DelimitedIngest(),
    partition = setup_RandomPartition(),
    conformal = setup_SplitConformal(),
    explanation = setup_SHAP(),
    hyperparameters = setup_GLM()
  )
}


testthat::test_that("every family's record validates against its own schema", {
  for (nm in names(.cases())) {
    obj <- .cases()[[nm]]
    cls <- S7_class(obj)
    base <- family_base(cls)
    discriminator <- family_discriminator(obj)
    expect_false(
      is.null(discriminator),
      info = paste0(nm, ": family has no declared discriminator.")
    )
    if (is.null(discriminator)) {
      # Report every affected family rather than stopping at the first.
      next
    }
    validator <- .record_bundle(base, cls, discriminator, tolower(base@name))
    doc <- jsonlite::toJSON(
      nested_record(obj, obj),
      auto_unbox = TRUE,
      null = "null"
    )
    # Validating against the schema is not enough on its own: a writer and a
    # schema that are short in the *same* way agree with each other. This says
    # what a record is for -- every value it carries states where it came from
    # -- and is derived from the document, not from the helpers that built it.
    #
    # Two exemptions, both structural: the discriminator is implied by the
    # variant rather than chosen, and a nested config block carries an `origin`
    # of its own.
    record <- nested_record(obj, obj)
    fields <- setdiff(names(record), "origin")
    nested <- Filter(
      function(f) is.list(record[[f]]) && "origin" %in% names(record[[f]]),
      fields
    )
    expect_setequal(
      names(record[["origin"]]),
      setdiff(fields, c(discriminator, nested))
    )

    result <- validator(doc, verbose = TRUE)
    errors <- attr(result, "errors")
    expect_true(
      isTRUE(result),
      info = paste0(
        nm,
        " (",
        cls@name,
        "): the record rtemis writes does not validate against the record ",
        "schema rtemis publishes.\n",
        if (!is.null(errors)) {
          paste(utils::head(errors[["message"]], 8), collapse = "\n")
        }
      )
    )
  }
})


testthat::test_that("every dispatched family has a record document under test", {
  # Adding a family without a case above is the omission that let
  # `ExecutionConfig` ship records with no discriminator.
  covered <- vapply(
    .cases(),
    function(obj) family_base(S7_class(obj))@name,
    character(1L)
  )
  expect_setequal(
    unique(covered),
    unname(vapply(
      schema_catalog()$families,
      function(f) f$base_class@name,
      character(1L)
    ))
  )
})
