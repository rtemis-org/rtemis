# record.R
# ::rtemis::
# 2026- EDG rtemis.org

# Turning a finished run into a record: the values it used, where each one came
# from, and what produced it. See `plan/config-artifacts.md`.
#
# The hard part is `origin`, and what makes it answerable is that a model
# carries *both* configs -- the input it was given (`@config`) and the resolved
# one it ran (`@hyperparameters` and friends). Comparing them is observation:
# a field that changed was changed by the run, and whether it was tuned or
# derived is decided by the declarations (`tunable`, `tune_on_null`), not by a
# guess.
#
# One inference remains, and it is worth naming. `setup_*()` applies its
# defaults before `train()` ever sees the config, so "the user typed
# `nrounds = 500`" and "500 is the default" arrive identical. They are separated
# by comparing against the declared default, which means a user who explicitly
# supplies the default value is reported as `"default"`. The value is the same
# either way, so a replay is unaffected; only the attribution is.

# %% value_origin ----
#' Where one value came from
#'
#' @param input Value the run was given, or NULL if the field was unset.
#' @param resolved Value the run used.
#' @param spec `PropertySpec` for the field, or NULL.
#' @param state Logical: Whether the field is run state -- written by the run,
#'   never supplied. Such a field cannot be `"user"` or `"default"`: if it holds
#'   nothing, the run never got to it.
#'
#' @return Character: one of `VALUE_ORIGINS`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
value_origin <- function(input, resolved, spec, state = FALSE) {
  if (state && is.null(resolved)) {
    # Only a run writes it, and it holds nothing -- so the run never determined
    # it. `default` would claim a default exists for something no default can
    # supply.
    return("unset")
  }
  if (!identical(input, resolved)) {
    # NULL meaning "apply the default for this task type" is a restatement of
    # what was asked for, not something measured or searched -- so resolving it
    # is a `default`, however much the value changed.
    if (!is.null(spec) && spec@default_on_null && is.null(input)) {
      return("default")
    }
    # The run changed it. Which way is a declaration, not a guess: a search
    # space narrowing to one value, or a NULL that means "determine by tuning",
    # is tuning; anything else the run worked out from the data.
    # `input` is a wire value: `config_record()` runs it through `wire_value()`
    # before comparing, so a search space arrives tagged rather than as the R
    # object.
    searched <- !is.null(spec) &&
      (is_wire_candidates(input) ||
        (spec@tune_on_null && is.null(input)))
    return(if (searched) "tuned" else "derived")
  }
  # Unchanged: the run used what it was given. Whether that was chosen or
  # merely inherited is the one inference here -- see the file header.
  if (!is.null(spec) && identical(input, spec@default)) "default" else "user"
} # /rtemis::value_origin


# %% config_record ----
#' The record form of one config object
#'
#' Its serialized values, plus an `origin` map saying where each came from.
#' `origin` covers exactly the fields the record carries, which is what the
#' generated `record.json` requires.
#'
#' @param input S7 config object the run was given, or NULL when the run was
#'   given none (every value is then the run's own).
#' @param resolved S7 config object the run used.
#'
#' @return Named list: the values, plus `origin`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_record <- function(input, resolved) {
  cls <- S7_class(resolved)
  # The fields the *record schema* declares, which is what `origin` must cover:
  # a family leaf's inherited machinery is subtracted by `S7_to_JSONSchema()`'s
  # `base`, so it is subtracted here too. `serializable_props()` is the wrong
  # level -- for a family it returns the wire shape (`{algorithm, config}`),
  # while a leaf record declares the flat fields inside it.
  base <- family_base(cls)
  names_ <- record_names(cls, base)
  props <- cls@properties
  # A config-valued property is a record in its own right -- a
  # `GridSearchConfig` holds a `ResamplerConfig` -- so it is built recursively
  # and left out of this block's `origin`, which the schema does too: it
  # carries its own. A *list* of them (a meta learner's `base_learners`) is
  # published as an array of `$ref`s and gets one record per element, for the
  # same reason.
  # A `HyperparameterCandidates` is S7 too, and is none of that: it is the
  # *value* a tunable property holds, and it travels as the tag `wire_value()`
  # writes. Recording it as a nested config would publish its R-side field
  # (`from_vector`) and an `origin` of its own, which the schema's
  # `{"candidates": [...]}` branch does not admit; drop the property from this
  # block's `origin`, the one place that reports it was tuned; and defeat
  # `is_wire_candidates()`, whose exact-names test is what lets `from_wire()`
  # read the value back.
  nested <- Filter(
    function(nm) {
      value <- prop(resolved, nm)
      S7_inherits(value) && !is_candidates(value)
    },
    names_
  )
  nested_list <- Filter(
    function(nm) is_S7_list(prop(resolved, nm)),
    setdiff(names_, nested)
  )
  flat <- setdiff(names_, c(nested, nested_list))

  values <- lapply(flat, function(nm) {
    S7_to_list(wire_value(prop(resolved, nm), props[[nm]]))
  })
  names(values) <- flat
  input_values <- if (is.null(input)) {
    list()
  } else {
    stats::setNames(
      lapply(flat, function(nm) {
        S7_to_list(wire_value(prop(input, nm), props[[nm]]))
      }),
      flat
    )
  }
  origin <- lapply(flat, function(nm) {
    value_origin(
      input_values[[nm]],
      values[[nm]],
      get_spec(props[[nm]]),
      state = identical(prop_role(props[[nm]]), "state")
    )
  })
  names(origin) <- flat

  sub <- lapply(nested, function(nm) {
    nested_record(
      if (is.null(input)) NULL else prop(input, nm),
      prop(resolved, nm)
    )
  })
  names(sub) <- nested

  # Each element paired with the element the input held under the same name, so
  # a library entry the user supplied and one the run added read differently.
  # Positional pairing would misattribute after an insertion; names are what the
  # library is keyed by.
  sub_lists <- lapply(nested_list, function(nm) {
    given <- if (is.null(input)) NULL else prop(input, nm)
    resolved_list <- prop(resolved, nm)
    out <- lapply(names(resolved_list), function(entry) {
      nested_record(given[[entry]], resolved_list[[entry]])
    })
    names(out) <- names(resolved_list)
    out
  })
  names(sub_lists) <- nested_list

  # Declaration order, so a record reads like the class it describes.
  out <- c(values, sub, sub_lists)[names_]
  c(out, list(origin = origin))
} # /rtemis::config_record


# %% is_S7_list ----
#' Is this a non-empty list whose every element is an S7 object?
#'
#' The test for a property published as an array of `$ref`s. Requires *every*
#' element to be S7: a mixed list is not a shape any schema declares, and
#' treating it as one would silently drop the non-S7 elements from the record.
#'
#' @param x Any value.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_S7_list <- function(x) {
  is.list(x) && length(x) > 0L && all(vapply(x, S7_inherits, logical(1L)))
} # /rtemis::is_S7_list


# %% family_base ----
#' The base class of a config object's family
#'
#' The topmost ancestor below `S7_object`: `Hyperparameters` for
#' `SuperLearnerHyperparameters`, `ResamplerConfig` for `KFoldConfig`, NULL for a
#' flat config that has no family.
#'
#' Must be the *family* base, not `cls@parent`: `S7_to_JSONSchema()` subtracts
#' the family base from every leaf, so a class with an intermediate ancestor
#' (`SuperLearnerHyperparameters` sits under `StackedLearnerHyperparameters`
#' under `MetaLearnerHyperparameters`) would otherwise have the intermediate's
#' properties in its schema but not in its record, and the record would fail
#' validation against the schema generated from the same class.
#'
#' @param cls S7 class.
#'
#' @return S7 class, or NULL when `cls` has no parent but `S7_object`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
family_base <- function(cls) {
  base <- cls@parent
  if (is.null(base) || identical(base@name, "S7_object")) {
    return(NULL)
  }
  while (!is.null(base@parent) && !identical(base@parent@name, "S7_object")) {
    base <- base@parent
  }
  base
} # /rtemis::family_base


# %% record_values ----
#' The field names a record carries for one config class
#'
#' Unlike a written config, a record keeps every field: an unset one is stated
#' as `null` rather than omitted, so nothing in it falls back to a reader's
#' defaults.
#'
#' It also keeps **run state**, which a config drops. `lambda.min` and the
#' `nrounds` early stopping settled on are precisely what a record exists to
#' report -- a config omits them because they are re-derived on read, but a
#' record is the statement of what a run produced. Only constants are left out,
#' the algorithm implying them, and computed views, which are functions of
#' fields already present.
#'
#' @param cls S7 class.
#' @param base S7 class or NULL: the family base, whose properties a leaf
#'   record does not declare.
#'
#' @return Character vector, in declaration order.
#'
#' @author EDG
#' @keywords internal
#' @noRd
record_names <- function(cls, base) {
  names_ <- if (is.null(base)) {
    names(cls@properties)
  } else {
    own_prop_names(cls, base)
  }
  keep <- Filter(
    function(nm) {
      prop <- cls@properties[[nm]]
      role <- prop_role(prop)
      if (identical(role, "computed")) {
        return(FALSE)
      }
      if (identical(role, "state")) {
        return(TRUE)
      }
      spec <- get_spec(prop)
      is.null(spec) || !spec@constant
    },
    names_
  )
  # `S7_to_list()` because a value may itself be a config object -- a
  # `GridSearchConfig` holds a `ResamplerConfig` -- and a record is JSON, not
  # objects. Applied per value rather than to the whole list so NULLs survive.
  keep
} # /rtemis::record_names


# %% provenance_of ----
#' The provenance block for a fitted model
#'
#' Drawn from what the model already carries -- `@session` for timing,
#' `@session_info` for the environment, `@data_fingerprint` for data identity --
#' rather than recomputed, so a record cannot disagree with the object it came
#' from. Only what a record needs to be read on its own is promoted; the full
#' `sessionInfo()` stays where it is.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param outcome Character: How the run ended; see `RUN_OUTCOMES`.
#'
#' @return `Provenance` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
provenance_of <- function(x, outcome = "completed") {
  # A pipeline result carries no observability session, so timing and
  # environment come from the current one. A `Supervised` records its own.
  has <- function(nm) nm %in% names(S7_class(x)@properties)
  session <- if (has("session")) prop(x, "session")
  started <- if (is.null(session)) NULL else session@started
  finished <- if (is.null(session)) NULL else session@finished
  info <- if (has("session_info")) prop(x, "session_info") else list()
  Provenance(
    rtemis_version = as.character(utils::packageVersion("rtemis")),
    # From the recorded session rather than the current one: a model reloaded
    # from disk must report the R that trained it, not the R reading it.
    r_version = info[["R.version"]][["version.string"]] %||% R.version.string,
    platform = info[["platform"]] %||% R.version[["platform"]],
    started = iso8601(started),
    finished = iso8601(finished),
    elapsed_seconds = if (is.null(started) || is.null(finished)) {
      0
    } else {
      as.numeric(difftime(finished, started, units = "secs"))
    },
    outcome = outcome,
    data_training = if (has("data_fingerprint")) prop(x, "data_fingerprint")
  )
} # /rtemis::provenance_of


# %% iso8601 ----
#' A timestamp as ISO 8601, or "" when there is none
#'
#' @param x `POSIXct` or NULL.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
iso8601 <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("")
  }
  format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
} # /rtemis::iso8601


# %% record ----
#' Derive a run record from a fitted model
#'
#' The record of what ran: every config value resolved, an `origin` saying where
#' each came from, and a provenance block. Derived rather than stored -- the
#' object already holds both configs, and keeping a second representation on it
#' would let the two drift.
#'
#' @param x Fitted model object.
#' @param ... Passed to methods.
#'
#' @return Named list, conforming to the family's `record.json`.
#'
#' @author EDG
#' @export
#' @examples
#' mod <- train(iris, hyperparameters = setup_CART())
#' names(record(mod))
record <- new_generic("record", "x")


# %% record.Supervised ----
#' @author EDG
#' @noRd
method(record, Supervised) <- function(x, outcome = "completed") {
  supervised_record(x, folds = list(x), outcome = outcome)
} # /rtemis::record.Supervised


# %% record.SupervisedRes ----
#' @author EDG
#' @noRd
method(record, SupervisedRes) <- function(x, outcome = "completed") {
  supervised_record(x, folds = x@models, outcome = outcome)
} # /rtemis::record.SupervisedRes


# %% supervised_record ----
#' Assemble a supervised run record
#'
#' The top level is what was *asked for*; `folds` is what *ran*, once per model
#' fitted. They are separate because a resampled run resolves different values
#' in each fold -- early stopping picks a different `nrounds` every time -- so a
#' single resolved value at the top would be a claim the run never made. A
#' single fit is one fold rather than a second shape.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param folds List of `Supervised` objects: the models actually fitted.
#' @param outcome Character: How the run ended; see `RUN_OUTCOMES`.
#'
#' @return Named list conforming to `supervised/v1/record.json`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
supervised_record <- function(x, folds, outcome = "completed") {
  input <- prop(x, "config")
  if (is.null(input)) {
    rtemis.core::abort(
      "This model carries no input config, so a record cannot say what was asked for. Only a top-level `train()` call stores one.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  # Every block as the run was *given* it: comparing an input against itself
  # yields origins of `user` / `default`, which is what "asked for" means.
  blocks <- c(
    "preprocessor_config",
    "decomposition_config",
    "hyperparameters",
    "tuner_config",
    "outer_resampling_config",
    "execution_config"
  )
  asked <- lapply(blocks, function(nm) {
    block <- prop(input, nm)
    nested_record(block, block)
  })
  names(asked) <- blocks

  own <- config_record(input, input)
  own_names <- setdiff(names(own), c(blocks, "origin"))
  c(
    list(`$schema` = .RTEMIS_RECORD_SCHEMAS[["supervised"]]),
    own[own_names],
    asked,
    list(
      origin = own[["origin"]][own_names],
      folds = lapply(seq_along(folds), function(i) fold_record(folds[[i]], i)),
      # The headline scores, so "was this model any good?" is one lookup rather
      # than an average over `folds`. Each fold's full metrics are there too;
      # this block is the reason to open the file.
      metrics = sample_metrics(x, metric_row),
      metrics_sd = sample_metrics(x, metric_sd_row),
      provenance = S7_to_list(provenance_of(x, outcome = outcome))
    )
  )
} # /rtemis::supervised_record


# %% fold_record ----
#' What one fitted model resolved
#'
#' Paired against the run's input so each value carries its origin: a search
#' space narrowed to one value reads `tuned`, a NULL the data filled reads
#' `derived`.
#'
#' @param model `Supervised` object for one fold.
#' @param index Integer: 1-based outer resample.
#'
#' @return Named list: one entry of the record's `folds` array.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fold_record <- function(model, index) {
  input <- model@config
  out <- list(index = as.integer(index))
  if (!is.null(model@preprocessor)) {
    out[["preprocessor_config"]] <- nested_record(
      if (is.null(input)) NULL else input@preprocessor_config,
      fitted_config(model@preprocessor)
    )
  }
  if (!is.null(model@decomposition)) {
    out[["decomposition_config"]] <- nested_record(
      if (is.null(input)) NULL else input@decomposition_config,
      model@decomposition@config
    )
  }
  out[["hyperparameters"]] <- nested_record(
    if (is.null(input)) NULL else input@hyperparameters,
    model@hyperparameters
  )
  # The grid, the per-resample metrics and the winner, as the Tuner holds them:
  # a tuning decision must be re-examinable from the record alone.
  if (!is.null(model@tuner)) {
    out[["tuning"]] <- c(
      model@tuner@tuning_results,
      list(best = model@tuner@best_hyperparameters)
    )
  }
  # What this fold scored, in full: the confusion matrix and per-class rows as
  # well as the headline row. The record already said how every tuning
  # *candidate* scored; without this it was silent on the model that was kept.
  out[["metrics"]] <- sample_metrics(model, record_object)
  out
} # /rtemis::fold_record


# %% SUPERVISED_SAMPLES ----
# The samples a supervised run scores, in the order a reader expects them.
# `SupervisedRes` has no validation sample, so each lookup is guarded.
SUPERVISED_SAMPLES <- c("training", "validation", "test")


# %% record_object ----
#' An S7 object as a record block
#'
#' Every **published** property, run state included. `serializable_props()` is
#' the wrong filter here for the same reason it was wrong for hyperparameters:
#' it answers what a *config* carries, and a config drops state because it is
#' re-derived on read. A record exists to report exactly what the run wrote.
#'
#' @param x S7 object, or NULL.
#'
#' @return Named list, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
record_object <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  declared <- S7_class(x)@properties
  nms <- published_prop_names(S7_class(x))
  out <- lapply(nms, function(nm) {
    value <- wire_value(prop(x, nm), declared[[nm]])
    if (S7_inherits(value)) record_object(value) else value
  })
  names(out) <- nms
  out
} # /rtemis::record_object


# %% metric_row ----
#' One sample's headline scores, as a flat name-to-value map
#'
#' The row a reader compares runs on: a regression sample's four metrics, or a
#' classification sample's `overall` row. Per-class metrics and the confusion
#' matrix are deliberately not here -- they are in the fold's full metrics
#' block, which is the typed contract. This block exists to be read without
#' computing anything, which is what makes an `outdir` of records rankable.
#'
#' @param x `Metrics` or `MetricsRes` object, or NULL.
#'
#' @return Named list of scalars, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
metric_row <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  row <- if (S7_inherits(x, MetricsRes)) {
    # Resampled: the mean across folds, which is the comparable summary.
    x@mean_metrics
  } else if (S7_inherits(x, ClassificationMetrics)) {
    x@metrics[["overall"]]
  } else {
    x@metrics
  }
  if (is.null(row)) NULL else as.list(row)
} # /rtemis::metric_row


# %% metric_sd_row ----
#' One sample's dispersion across folds, or NULL for a single fit
#'
#' @param x `Metrics` or `MetricsRes` object, or NULL.
#'
#' @return Named list of scalars, or NULL when the run fitted one model and
#'   there is no dispersion to report.
#'
#' @author EDG
#' @keywords internal
#' @noRd
metric_sd_row <- function(x) {
  if (is.null(x) || !S7_inherits(x, MetricsRes)) {
    return(NULL)
  }
  if (is.null(x@sd_metrics)) NULL else as.list(x@sd_metrics)
} # /rtemis::metric_sd_row


# %% sample_metrics ----
#' Per-sample metrics of one model, under a given reader
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param reader Function applied to each sample's metrics object.
#'
#' @return Named list keyed by `SUPERVISED_SAMPLES`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
sample_metrics <- function(x, reader) {
  out <- lapply(SUPERVISED_SAMPLES, function(sample) {
    nm <- paste0("metrics_", sample)
    # `SupervisedRes` declares no validation sample.
    if (!prop_exists(x, nm)) {
      return(NULL)
    }
    reader(prop(x, nm))
  })
  names(out) <- SUPERVISED_SAMPLES
  out
} # /rtemis::sample_metrics


# %% nested_record ----
#' The record form of one nested config block, or NULL
#'
#' A family config serializes as `{algorithm, <payload>}`, so its record is that
#' shape with the payload replaced by the leaf's record. A flat config is its own
#' record. Either way the block carries its own `origin`, which is why the
#' parent's does not cover it.
#'
#' @param input S7 config the run was given, or NULL.
#' @param resolved S7 config the run used, or NULL.
#'
#' @return Named list, or NULL when the run used no such block.
#'
#' @author EDG
#' @keywords internal
#' @noRd
nested_record <- function(input, resolved) {
  if (is.null(resolved)) {
    return(NULL)
  }
  leaf <- config_record(input, resolved)
  shape <- family_shape(resolved)
  if (is.null(shape)) {
    return(leaf)
  }
  out <- list()
  # The discriminator always leads: without it a dispatcher matches no branch,
  # and its `unevaluatedProperties` then rejects the very fields the leaf
  # declares.
  out[[shape[["discriminator"]]]] <- prop(resolved, shape[["discriminator"]])
  if (is.null(shape[["payload"]])) {
    # Top-level mode: the leaf's fields are siblings of the discriminator.
    return(c(out, leaf))
  }
  out[[shape[["payload"]]]] <- leaf
  out
} # /rtemis::nested_record


# %% family_shape ----
#' How a discriminated config family serializes: its discriminator and payload
#'
#' Mirrors `data-raw/schema_registry.R`. The discriminator is what a dispatcher's
#' `if/then` keys on, so a record block missing it matches no branch -- and then
#' `unevaluatedProperties` rejects every field the leaf would have declared.
#' `payload` is NULL for a family that serializes its variant's fields as
#' siblings of the discriminator rather than nesting them (the resampler).
#'
#' @param x S7 config object.
#'
#' @return Named list with `discriminator` and `payload`, or NULL when `x` is
#'   not a discriminated family.
#'
#' @author EDG
#' @keywords internal
#' @noRd
family_shape <- function(x) {
  if (S7_inherits(x, Hyperparameters)) {
    list(discriminator = "algorithm", payload = "hyperparameters")
  } else if (
    S7_inherits(x, DecompositionConfig) || S7_inherits(x, ClusteringConfig)
  ) {
    list(discriminator = "algorithm", payload = "config")
  } else if (S7_inherits(x, TunerConfig)) {
    list(discriminator = "type", payload = "config")
  } else if (S7_inherits(x, ResamplerConfig)) {
    # Top-level mode: the variant's fields are siblings of `type`.
    list(discriminator = "type", payload = NULL)
  } else {
    NULL
  }
} # /rtemis::family_shape


# %% record.Decomposition ----
#' @author EDG
#' @noRd
method(record, Decomposition) <- function(x, outcome = "completed") {
  pipeline_record(
    x,
    "decompose",
    x@decompose_config,
    "decomposition_config",
    x@config,
    outcome
  )
} # /rtemis::record.Decomposition


# %% record.Clustering ----
#' @author EDG
#' @noRd
method(record, Clustering) <- function(x, outcome = "completed") {
  pipeline_record(
    x,
    "cluster",
    x@cluster_config,
    "clustering_config",
    x@config,
    outcome
  )
} # /rtemis::record.Clustering


# %% pipeline_record ----
#' Assemble a record for a single-run pipeline
#'
#' `decomp()` and `cluster()` fit one model, so there is no per-fold structure:
#' the algorithm config appears once, resolved, with its origins. The supervised
#' record's `folds` exists because outer resampling fits several models that
#' resolve *different* values; nothing here does.
#'
#' @param x `Decomposition` or `Clustering` object.
#' @param family Character: `"decompose"` or `"cluster"`.
#' @param input S7 config object the run was given.
#' @param block Character: the input's field holding the algorithm config.
#' @param resolved S7 config object the run used.
#' @param outcome Character: How the run ended; see `RUN_OUTCOMES`.
#'
#' @return Named list conforming to the family's `record.json`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pipeline_record <- function(x, family, input, block, resolved, outcome) {
  if (is.null(input)) {
    rtemis.core::abort(
      "This object carries no input config, so a record cannot say what was asked for.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  own <- config_record(input, input)
  own_names <- setdiff(names(own), c(block, "origin"))
  out <- c(
    list(`$schema` = .RTEMIS_RECORD_SCHEMAS[[family]]),
    own[own_names]
  )
  out[[block]] <- nested_record(prop(input, block), resolved)
  c(
    out,
    list(
      origin = own[["origin"]][own_names],
      provenance = S7_to_list(provenance_of(x, outcome = outcome))
    )
  )
} # /rtemis::pipeline_record
