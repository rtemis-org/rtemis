# record.R
# ::rtemis::
# 2026- EDG rtemis.org

# Turning a finished run into a record: the values it used, where each one came
# from, and what produced it. See `plan/config-artifacts.md`.
#
# The hard part is `origin`, and what makes it answerable is that a model
# carries *both* configs — the input it was given (`@config`) and the resolved
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
#'
#' @return Character: one of `VALUE_ORIGINS`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
value_origin <- function(input, resolved, spec) {
  if (!identical(input, resolved)) {
    # NULL meaning "apply the default for this task type" is a restatement of
    # what was asked for, not something measured or searched — so resolving it
    # is a `default`, however much the value changed.
    if (!is.null(spec) && spec@default_on_null && is.null(input)) {
      return("default")
    }
    # The run changed it. Which way is a declaration, not a guess: a search
    # space narrowing to one value, or a NULL that means "determine by tuning",
    # is tuning; anything else the run worked out from the data.
    searched <- !is.null(spec) &&
      ((spec@tunable && length(input) > 1L) ||
        (spec@tune_on_null && is.null(input)))
    return(if (searched) "tuned" else "derived")
  }
  # Unchanged: the run used what it was given. Whether that was chosen or
  # merely inherited is the one inference here — see the file header.
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
  # level — for a family it returns the wire shape (`{algorithm, config}`),
  # while a leaf record declares the flat fields inside it.
  base <- if (identical(cls@parent@name, "S7_object")) NULL else cls@parent
  values <- record_values(resolved, base)
  input_values <- if (is.null(input)) list() else record_values(input, base)
  props <- cls@properties
  origin <- lapply(names(values), function(nm) {
    value_origin(input_values[[nm]], values[[nm]], get_spec(props[[nm]]))
  })
  names(origin) <- names(values)
  c(values, list(origin = origin))
} # /rtemis::config_record


# %% record_values ----
#' The values a record carries for one config object
#'
#' Unlike a written config, a record keeps every field: an unset one is stated
#' as `null` rather than omitted, so nothing in it falls back to a reader's
#' defaults.
#'
#' It also keeps **run state**, which a config drops. `lambda.min` and the
#' `nrounds` early stopping settled on are precisely what a record exists to
#' report — a config omits them because they are re-derived on read, but a
#' record is the statement of what a run produced. Only constants are left out,
#' the algorithm implying them, and computed views, which are functions of
#' fields already present.
#'
#' @param x S7 config object.
#' @param base S7 class or NULL: the family base, whose properties a leaf
#'   record does not declare.
#'
#' @return Named list, in declaration order, with NULLs preserved.
#'
#' @author EDG
#' @keywords internal
#' @noRd
record_values <- function(x, base) {
  cls <- S7_class(x)
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
  values <- lapply(keep, function(nm) prop(x, nm))
  names(values) <- keep
  values
} # /rtemis::record_values


# %% provenance_of ----
#' The provenance block for a fitted model
#'
#' Drawn from what the model already carries — `@session` for timing,
#' `@session_info` for the environment, `@data_fingerprint` for data identity —
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
  session <- prop(x, "session")
  started <- if (is.null(session)) NULL else session@started
  finished <- if (is.null(session)) NULL else session@finished
  info <- prop(x, "session_info")
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
    data_training = prop(x, "data_fingerprint")
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
#' each came from, and a provenance block. Derived rather than stored — the
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
#' in each fold — early stopping picks a different `nrounds` every time — so a
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
  out
} # /rtemis::fold_record


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
  payload <- family_payload(resolved)
  if (is.null(payload)) {
    return(leaf)
  }
  discriminator <- if (payload == "config") "algorithm" else "algorithm"
  out <- list()
  out[[discriminator]] <- prop(resolved, discriminator)
  out[[payload]] <- leaf
  out
} # /rtemis::nested_record


# %% family_payload ----
#' The payload field name for a discriminated config family, or NULL
#'
#' Mirrors `data-raw/schema_registry.R`'s `payload`: a family serializes its
#' variant's parameters under one key, and a record nests the leaf record there.
#'
#' @param x S7 config object.
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
family_payload <- function(x) {
  if (S7_inherits(x, Hyperparameters)) {
    "hyperparameters"
  } else if (
    S7_inherits(x, DecompositionConfig) || S7_inherits(x, ClusteringConfig)
  ) {
    "config"
  } else {
    NULL
  }
} # /rtemis::family_payload
