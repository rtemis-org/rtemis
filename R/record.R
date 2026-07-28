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
#' defaults. `config_prop_values()` already drops what a record must not carry
#' (constants, run state the model holds), so what is added back here is only
#' the fields whose value is NULL.
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
    function(nm) prop_serialized(cls@properties[[nm]]),
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
  input <- x@config
  if (is.null(input)) {
    rtemis.core::abort(
      "This model carries no input config, so a record cannot say what was asked for. Only a top-level `train()` call stores one.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  # The blocks the run resolved, paired with what it was given. A block the run
  # did not use stays NULL on both sides and is written as null.
  resolved <- list(
    preprocessor_config = if (!is.null(x@preprocessor)) {
      fitted_config(x@preprocessor)
    },
    decomposition_config = if (!is.null(x@decomposition)) {
      x@decomposition@config
    },
    hyperparameters = x@hyperparameters,
    tuner_config = if (!is.null(x@tuner)) x@tuner@config,
    outer_resampling_config = NULL,
    execution_config = x@execution_config
  )
  nested <- lapply(names(resolved), function(nm) {
    nested_record(prop(input, nm), resolved[[nm]])
  })
  names(nested) <- names(resolved)

  # The recipe's own scalar fields, which no nested block covers.
  own <- config_record(input, input)
  own_names <- setdiff(names(own), c(names(resolved), "origin"))
  c(
    list(`$schema` = .RTEMIS_RECORD_SCHEMAS[["supervised"]]),
    own[own_names],
    nested,
    list(
      origin = own[["origin"]][own_names],
      provenance = S7_to_list(provenance_of(x, outcome = outcome))
    )
  )
} # /rtemis::record.Supervised


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
