# 070_Hyperparameters.R
# ::rtemis::
# 2025- EDG rtemis.org

# References ----
# S7
# - https://github.com/RConsortium/S7
# - https://rconsortium.github.io/S7/
# LightGBM parameters
# - https://lightgbm.readthedocs.io/en/latest/Parameters.html

# Architecture ----
# Every `*Hyperparameters` class declares its hyperparameters with the
# `prop_*` factories (010_Props.R): one declaration per hyperparameter carries
# the type, default, bounds, enum, tunability, and description, from which S7
# validators, the tunable/fixed name vectors, the `hyperparameters` list, and
# the JSON Schema (S7_to_JSONSchema) are all generated. Parameters whose R
# type cannot be expressed as a JSON type (e.g. character-or-function) are
# declared as plain S7 properties: still stored and validated by class, but
# non-tunable and excluded from schemas via `exclude`.
#
# The abstract `Hyperparameters` superclass provides computed properties:
# - `hyperparameters`: assembles the named list from the subclass's own
#   properties (getter), and routes assignments back to the properties, where
#   they are validated (setter). Changing a constant is an error.
# - `tunable_hyperparameters` / `fixed_hyperparameters`: derived from specs.
# - `tuned`: derived from current values via `get_tuned_status()` unless the
#   Tuner has set a status explicitly (backing store `.tuned`; NA = derive).
# Subclasses use S7's default constructors (defaults come from the specs);
# `setup_*` functions clean user input and construct.

# %% Constants ----
# `tuned` values ----
# -9: Set by Tuner: Actively being tuned (Values fixed by Tuner).
# -2: Not tunable (No tunable_hyperparameters).
# -1: Not tunable (tunable_hyperparameters exist, but none of them have more than one value).
#  0: Untuned but tunable (search values present, or a tune-on-NULL parameter is unset).
#  1: Set by Tuner: Tuned (Started as 0, set to 1 when tuned).
TUNED_STATUS_TUNING <- -9L
TUNED_STATUS_NOT_TUNABLE <- -2L
TUNED_STATUS_NO_SEARCH_VALUES <- -1L
TUNED_STATUS_UNTUNED <- 0L
TUNED_STATUS_TUNED <- 1L

# `resampled` values ----
# 0: Running on single training set.
# 1: Running on resampled training sets.

# %% hp_prop_names ----
#' Names of a Hyperparameters subclass's own (hyperparameter) properties
#'
#' All properties declared by the subclass itself -- factory-built or plain --
#' excluding the properties inherited from `Hyperparameters`.
#'
#' @param x S7 class (a `Hyperparameters` subclass).
#'
#' @return Character vector of property names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hp_prop_names <- function(x) {
  own_prop_names(x, Hyperparameters)
} # /rtemis::hp_prop_names


# %% hp_prop_values ----
#' Collect hyperparameter property values from an instance
#'
#' Named list of the subclass's own property values. Unset hyperparameters
#' read as NULL.
#'
#' @param self `Hyperparameters` object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hp_prop_values <- function(self) {
  own_prop_values(self, Hyperparameters)
} # /rtemis::hp_prop_values


# %% resolve_data_bounds ----
#' Resolve training-data dimensions referenced by `data_bound` declarations
#'
#' Every dimension is derived from `x` on each call; none is stored on an
#' object, since all of them are facts about the data rather than about the
#' config being checked.
#'
#' @param x tabular data: Training data.
#' @param needed Character: Which dimensions to resolve. Only these are
#'   computed, so a caller declaring nothing feature-related may pass a bare
#'   vector -- `features()` requires at least two columns and would abort on
#'   one. `resample()` relies on this: a resampler bounds only `n_cases`.
#' @param has_outcome Logical: If TRUE, `x` follows the supervised convention
#'   (the last column is the outcome, the rest are features). Pass FALSE for a
#'   frame that is already features-only, as the decomposition path uses:
#'   otherwise the last feature is silently treated as an outcome and excluded
#'   from every dimension.
#'
#' @return Named list with one element per requested `DATA_BOUNDS` entry.
#'   `n_classes` is NULL for regression, where the outcome has no levels, and
#'   also when `has_outcome` is FALSE.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_data_bounds <- function(x, needed = DATA_BOUNDS, has_outcome = TRUE) {
  out <- list()
  if ("n_cases" %in% needed) {
    out[["n_cases"]] <- NROW(x)
  }
  if ("n_classes" %in% needed) {
    y <- if (has_outcome) outcome(x) else NULL
    out[["n_classes"]] <- if (is.factor(y)) nlevels(y) else NULL
  }
  feature_bounds <- c("n_features", "feature_names", "numeric_feature_names")
  if (any(feature_bounds %in% needed)) {
    feats <- if (has_outcome) features(x) else x
    if ("n_features" %in% needed) {
      out[["n_features"]] <- NCOL(feats)
    }
    if ("feature_names" %in% needed) {
      out[["feature_names"]] <- names(feats)
    }
    if ("numeric_feature_names" %in% needed) {
      out[["numeric_feature_names"]] <- getnumericnames(feats)
    }
  }
  out
} # /rtemis::resolve_data_bounds


# %% check_data_bounds ----
#' Check all `data_bound` properties against the training data
#'
#' Engine behind the default `validate_hyperparameters()` method, and usable
#' with any config object whose properties carry `PropertySpec`s. Walks the
#' class's properties, and for each one declaring a `data_bound` (see
#' `DATA_BOUNDS` in 010_Props.R) checks the current value against the resolved
#' dimension:
#'
#' - scalar property: every value must be `<=` the dimension. Tunable
#'   hyperparameters hold their whole search space here, hence `any()`.
#' - `vector` property: `length(value)` must equal the dimension.
#' - a name bound (`NAME_BOUNDS`): values must be a subset of the named columns.
#'
#' Unset (NULL) values are skipped, as is `n_classes` in regression.
#'
#' @param config S7 object with `prop_*`-declared properties (a
#'   `Hyperparameters`, a `DecompositionConfig`, ...).
#' @param x tabular data: Training data.
#' @param has_outcome Logical: Whether `x` carries an outcome column; see
#'   `resolve_data_bounds()`.
#'
#' @return `config`, invisibly. Throws if any bound is violated.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_data_bounds <- function(config, x, has_outcome = TRUE) {
  bounds <- data_bound_props(S7_class(config))
  if (length(bounds) == 0L) {
    return(invisible(config))
  }
  dims <- resolve_data_bounds(x, unique(bounds), has_outcome = has_outcome)
  specs <- S7_class(config)@properties
  for (nm in names(bounds)) {
    # `prop()`, not `[[`: a config family's `[[` routes into its computed
    # payload list, which excludes properties declared on the family base.
    value <- prop(config, nm)
    if (is.null(value)) {
      next
    }
    bound <- bounds[[nm]]
    dim_value <- dims[[bound]]
    # n_classes is undefined for regression: the declaration simply does not
    # apply rather than being an error.
    if (is.null(dim_value)) {
      next
    }
    # A domain holds one value per candidate, and the tuner may fit any of
    # them, so every candidate has to be in bounds -- an out-of-range one
    # would otherwise surface as a failed grid cell partway through tuning.
    for (value in if (is_candidates(value)) value@candidates else list(value)) {
      if (bound %in% NAME_BOUNDS) {
        # `unlist()` because a name bound may sit on a container of *vectors* -- a
        # map from group name to feature names. `setdiff()` on the list would
        # compare each element's deparsed form against the column names and report
        # every group as unknown.
        unknown <- setdiff(unlist(value, use.names = FALSE), dim_value)
        if (length(unknown) > 0L) {
          rtemis.core::abort(
            "`",
            nm,
            "` must name ",
            if (bound == "numeric_feature_names") "numeric " else "",
            "training features; not found: ",
            paste(unknown, collapse = ", "),
            ".",
            class = c("rtemis_value_error", "rtemis_input_error")
          )
        }
        next
      }
      spec <- get_spec(specs[[nm]])
      is_vector <- !is.null(spec) && spec@container != "none"
      if (is_vector) {
        if (length(value) != dim_value) {
          rtemis.core::abort(
            "`",
            nm,
            "` must have one value per ",
            DATA_BOUND_NOUN[[bound]],
            ": expected length ",
            dim_value,
            ", got ",
            length(value),
            ".",
            class = c("rtemis_length_error", "rtemis_input_error")
          )
        }
      } else if (any(value > dim_value)) {
        rtemis.core::abort(
          "`",
          nm,
          "` cannot be greater than the number of ",
          DATA_BOUND_NOUN_PLURAL[[bound]],
          " (",
          dim_value,
          "); got ",
          paste(unique(value[value > dim_value]), collapse = ", "),
          ".",
          class = c("rtemis_range_error", "rtemis_input_error")
        )
      }
    }
  }
  invisible(config)
} # /rtemis::check_data_bounds


# %% Hyperparameters ----
#' Hyperparameters
#'
#' @description
#' Abstract superclass for algorithm hyperparameters. Subclasses declare
#' each hyperparameter as a property (see the Architecture note at the top
#' of this file); this class contributes the run state and the computed,
#' spec-derived views.
#'
#' @field algorithm Character: Algorithm name (computed constant, overridden
#'   per subclass).
#' @field .tuned Integer: Backing store for `tuned`; NA = derive from values.
#' @field tuned Integer: Tuning status (computed; see TUNED_STATUS constants).
#' @field resampled Integer: Outer resampling status.
#' @field n_workers Integer: Number of workers to use for tuning.
#' @field hyperparameters Named list of hyperparameter values (computed from
#'   the subclass's properties; assignment routes back to the properties and
#'   validates).
#' @field tunable_hyperparameters Character: Names of tunable hyperparameters
#'   -- a vector of values is a search space.
#' @field fixed_hyperparameters Character: Names of fixed hyperparameters --
#'   settable by the user but not tunable. Arity is a separate axis: a fixed
#'   hyperparameter may itself be vector-valued.
#' @field constant_hyperparameters Character: Names of constant
#'   hyperparameters -- determined by the class and not settable.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Hyperparameters <- new_class(
  name = "Hyperparameters",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    algorithm = class_character,
    .tuned = new_property(class_integer, default = NA_integer_),
    tuned = new_property(
      class_integer,
      getter = function(self) {
        if (is.na(self@.tuned)) {
          get_tuned_status(self)
        } else {
          self@.tuned
        }
      }
    ),
    resampled = new_property(class_integer, default = 0L),
    n_workers = new_property(class_integer, default = 1L),
    hyperparameters = new_property(
      class_list,
      getter = function(self) {
        hp_prop_values(self)
      },
      setter = function(self, value) {
        route_config_assignment(
          self,
          Hyperparameters,
          value,
          label = self@algorithm,
          noun = "hyperparameter"
        )
      }
    ),
    tunable_hyperparameters = new_property(
      class_character,
      getter = function(self) {
        tunable_spec_names(S7_class(self))
      }
    ),
    fixed_hyperparameters = new_property(
      class_character,
      getter = function(self) {
        cls <- S7_class(self)
        setdiff(
          hp_prop_names(cls),
          c(tunable_spec_names(cls), constant_spec_names(cls))
        )
      }
    ),
    constant_hyperparameters = new_property(
      class_character,
      getter = function(self) constant_spec_names(S7_class(self))
    )
  )
) # /rtemis::Hyperparameters


# %% validate_hyperparameters.Hyperparameters ----
#' Default hyperparameter validation against training data
#'
#' Checks every property declaring a `data_bound`. Algorithms whose constraints
#' the `data_bound` vocabulary covers need no method of their own.
#'
#' @param hyperparameters `Hyperparameters`: Hyperparameters to check.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(validate_hyperparameters, Hyperparameters) <- function(
  hyperparameters,
  x
) {
  check_data_bounds(hyperparameters, x)
} # /rtemis::validate_hyperparameters.Hyperparameters


# %% abbrev_names ----
#' Name the first few and count the rest
#'
#' For a roster too long to read: the leading names, then how many more there
#' are. A class declaring its backend's whole parameter surface has scores.
#'
#' @param x Character: Names.
#' @param n Integer: How many to name.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
abbrev_names <- function(x, n = 6L) {
  if (length(x) <= n) {
    return(x)
  }
  c(x[seq_len(n)], paste0("(+", length(x) - n, " more)"))
} # /rtemis::abbrev_names


# %% repr.Hyperparameters ----
#' Repr Hyperparameters
#'
#' repr method for Hyperparameters object.
#'
#' @param x `Hyperparameters` object.
#' @param pad Integer: Left padding for printed output.
#' @param maxlength Integer: Maximum length of items to show before truncating with ellipsis.
#'   `-1` means no limit.
#' @param limit Integer: Limit number of items to show. `-1` means no limit.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @author EDG
#' @noRd
method(repr, Hyperparameters) <- function(
  x,
  pad = 0L,
  maxlength = -1L,
  limit = -1L,
  output_type = NULL
) {
  out <- repr_S7name(
    paste0(x@algorithm, "Hyperparameters"),
    pad = pad,
    output_type = output_type
  )
  # Select the display props explicitly: subclasses declare each
  # hyperparameter as its own property, which would otherwise print twice
  # (individually and inside `hyperparameters`).
  #
  # Unset hyperparameters are named rather than listed one per line, and the
  # tunable/fixed rosters are counted rather than enumerated. A class declaring
  # its backend's whole surface has scores of each -- LightGBM prints 86 lines
  # otherwise, 31 of them `NULL` -- and a value the user did not set is the one
  # thing they did not need to read. Both remain reachable with `$`.
  hyperparameters <- x@hyperparameters
  is_unset <- vapply(hyperparameters, is.null, logical(1L))
  display <- list(
    hyperparameters = hyperparameters[!is_unset],
    unset = abbrev_names(names(hyperparameters)[is_unset]),
    tunable_hyperparameters = length(x@tunable_hyperparameters),
    fixed_hyperparameters = length(x@fixed_hyperparameters),
    tuned = x@tuned,
    resampled = x@resampled,
    n_workers = x@n_workers
  )
  if (length(display[["unset"]]) == 0L) {
    display[["unset"]] <- NULL
  }
  out <- paste0(
    out,
    repr_ls(
      display,
      pad = pad,
      maxlength = maxlength,
      limit = limit,
      output_type = output_type
    )
  )
  if (x@tuned == TUNED_STATUS_TUNING) {
    out <- paste0(
      out,
      fmt(
        "\n  Hyperparameters are being tuned.\n",
        col = col_tuner,
        bold = TRUE,
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_NOT_TUNABLE) {
    out <- paste0(
      out,
      fmt(
        "\n  No hyperparameters are tunable.\n",
        col = col_tuner,
        bold = TRUE,
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_UNTUNED) {
    need_tuning <- names(get_hyperparams_need_tuning(x))
    out <- paste0(
      out,
      fmt(
        paste0(
          "\n  ",
          ngettext(length(need_tuning), "Hyperparameter ", "Hyperparameters "),
          oxfordcomma(
            need_tuning
          ),
          ngettext(length(need_tuning), " needs ", " need "),
          "tuning.\n"
        ),
        col = col_tuner,
        bold = TRUE,
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_NO_SEARCH_VALUES) {
    out <- paste0(
      out,
      fmt(
        "\n  No search values defined for tunable hyperparameters.\n",
        col = col_tuner,
        bold = TRUE,
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_TUNED) {
    out <- paste0(
      out,
      fmt(
        "\n  Hyperparameters are tuned.\n",
        col = col_tuner,
        bold = TRUE,
        output_type = output_type
      )
    )
  }
  out
} # /rtemis::repr.Hyperparameters


# %% print.Hyperparameters ----
method(print, Hyperparameters) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.Hyperparameters


# %% serializable_props.Hyperparameters ----
# A serialized config is `{algorithm, hyperparameters}` -- exactly what
# `.list_to_Hyperparameters()` consumes and what the hyperparameters schema
# describes. Everything else on the object is either derived from the specs
# (`tunable_`/`fixed_hyperparameters`) or run state set during training
# (`tuned`, `resampled`, `n_workers`), so it is reconstructed on read rather
# than written.
method(serializable_props, Hyperparameters) <- function(x) {
  list(
    algorithm = x@algorithm,
    hyperparameters = config_prop_values(x, Hyperparameters)
  )
} # /rtemis::serializable_props.Hyperparameters


# %% is_tuned.Hyperparameters ----
method(is_tuned, Hyperparameters) <- function(x) {
  x@tuned == TUNED_STATUS_TUNED
} # /is_tuned.Hyperparameters


# %% get_tuned_status.Hyperparameters ----
#' Derive tuning status from current values
#'
#' Spec-driven: any tunable hyperparameter with more than one value (search
#' values), or any hyperparameter declared `tune_on_null` that is unset,
#' means "needs tuning".
#'
#' @keywords internal
#' @noRd
method(get_tuned_status, Hyperparameters) <- function(x) {
  tunable <- x@tunable_hyperparameters
  if (length(tunable) == 0L) {
    return(TUNED_STATUS_NOT_TUNABLE)
  }
  values <- x@hyperparameters
  # A search space is a `HyperparameterCandidates`, never a value that happens to
  # have several elements: `hidden_units = c(12L, 6L, 2L)` is one architecture.
  if (any(vapply(values[tunable], is_candidates, logical(1L)))) {
    return(TUNED_STATUS_UNTUNED)
  }
  null_tune <- vapply(
    tune_on_null_spec_names(S7_class(x)),
    function(nm) is.null(values[[nm]]),
    logical(1L)
  )
  if (any(null_tune)) {
    return(TUNED_STATUS_UNTUNED)
  }
  TUNED_STATUS_NO_SEARCH_VALUES
} # /rtemis::get_tuned_status.Hyperparameters


# %% .update_hyperparameters ----
#' Set hyperparameter values on a Hyperparameters object
#'
#' Shared engine for the `update()` methods. The literal string "null" is the
#' grid sentinel produced by `expand_grid()` for NULL search entries, and NA the
#' marker `gate_tuning_grid()` writes into a combination an `applies_when` gate
#' excludes a hyperparameter from; both are set as NULL.
#'
#' The values are applied as one transaction, so the class validator sees the
#' finished object. A combination from a conditional grid passes through
#' intermediate states that are invalid on their own: raising
#' `smoothness_orders` above 0 while `reduce_basis` still holds its search value
#' is one.
#'
#' @param object `Hyperparameters` object.
#' @param hyperparameters Named list of values to set.
#' @param tuned Integer or NULL: Tuning status to set; NULL re-derives from
#'   the updated values.
#'
#' @return Updated `Hyperparameters` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.update_hyperparameters <- function(object, hyperparameters, tuned) {
  settable <- hp_prop_names(S7_class(object))
  values <- list()
  for (hp in names(hyperparameters)) {
    if (!hp %in% settable) {
      rtemis.core::abort(
        "Unknown ",
        object@algorithm,
        " hyperparameter '",
        hp,
        "'.",
        class = "rtemis_input_error"
      )
    }
    value <- hyperparameters[[hp]]
    if (identical(value, "null") || (length(value) == 1L && is.na(value))) {
      value <- NULL
    }
    # Single-bracket assignment keeps a NULL as an element of the list, which
    # is the value a closed gate sets.
    values[hp] <- list(value)
  }
  if (length(values) > 0L) {
    props(object) <- values
  }
  object@.tuned <- if (is.null(tuned)) NA_integer_ else tuned
  object
} # /rtemis::.update_hyperparameters


# %% update.Hyperparameters ----
#' Update Hyperparameters
#'
#' @param object `Hyperparameters` object.
#' @param hyperparameters Named list of algorithm hyperparameter values.
#' @param tuned Integer or NULL: Tuning status; NULL re-derives it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(update, Hyperparameters) <- function(
  object,
  hyperparameters,
  tuned = NULL,
  ...
) {
  .update_hyperparameters(object, hyperparameters, tuned)
} # /rtemis::update.Hyperparameters


# %% freeze.Hyperparameters ----
method(freeze, Hyperparameters) <- function(x) {
  x@.tuned <- TUNED_STATUS_NO_SEARCH_VALUES
  x
} # /rtemis::freeze.Hyperparameters


# %% lock.Hyperparameters ----
method(lock, Hyperparameters) <- function(x) {
  x@.tuned <- TUNED_STATUS_TUNED
  x
} # /rtemis::lock.Hyperparameters


# %% `$`.Hyperparameters ----
# Make Hyperparameters@hyperparameters@name `$`-accessible
method(`$`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}


# %% `.DollarNames`.Hyperparameters ----
# `$`-autocomplete Hyperparameters@hyperparameters
method(`.DollarNames`, Hyperparameters) <- function(x, pattern = "") {
  all_names <- names(x@hyperparameters)
  grep(pattern, all_names, value = TRUE)
}


# %% `[[`.Hyperparameters ----
# Make Hyperparameters@hyperparameters@name `[[`-accessible
method(`[[`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}


# %% needs_tuning.Hyperparameters ----
method(needs_tuning, Hyperparameters) <- function(x) {
  x@tuned == TUNED_STATUS_UNTUNED
} # /rtemis::needs_tuning.Hyperparameters


# %% training_device.Hyperparameters ----
#' Most algorithms run on the CPU by definition and have nothing to report.
#'
#' @keywords internal
#' @noRd
method(training_device, Hyperparameters) <- function(x) {
  NULL
} # /rtemis::training_device.Hyperparameters


# %% get_hyperparams_need_tuning.Hyperparameters ----
#' Get hyperparameters that need tuning.
#'
#' Tunable hyperparameters with more than one value (search values), plus any
#' hyperparameter declared `tune_on_null` that is unset (as a NULL entry, which
#' `expand_grid()` converts to its "null" sentinel).
#'
#' @keywords internal
#' @noRd
method(get_hyperparams_need_tuning, Hyperparameters) <- function(x) {
  # -> list
  values <- x@hyperparameters
  tunable <- x@tunable_hyperparameters
  out <- values[tunable[vapply(values[tunable], is_candidates, logical(1L))]]
  for (nm in tune_on_null_spec_names(S7_class(x))) {
    if (is.null(values[[nm]])) {
      out <- c(out, stats::setNames(list(NULL), nm))
    }
  }
  out
} # /get_hyperparams_need_tuning.Hyperparameters


# %% gate_tuning_grid ----
#' Apply `applies_when` gates to an expanded tuning grid
#'
#' Sets a gated hyperparameter to NA -- which `.update_hyperparameters()` reads
#' back as NULL -- in the rows whose gating values do not put it in effect, then
#' collapses the rows that this makes identical.
#'
#' A gated hyperparameter held at a single value is absent from the expansion
#' and is added as a column here, so that it too can be dropped from the rows
#' that cannot use it. A gate whose gating hyperparameters are all held at
#' single values is constant across the grid and `check_applies_when()` has
#' already settled it at construction.
#'
#' NA is the marker, distinct from `expand_grid()`'s "null" sentinel: the two
#' mean different things -- "determine by tuning" against "does not apply here"
#' -- and NA keeps a numeric column numeric.
#'
#' @param grid data.frame: The expanded cross product of the search values.
#' @param object `Hyperparameters` object the grid was expanded from.
#'
#' @return data.frame: The gated, deduplicated grid.
#'
#' @author EDG
#' @keywords internal
#' @noRd
gate_tuning_grid <- function(grid, object) {
  cls <- S7_class(object)
  for (nm in applies_when_spec_names(cls)) {
    value <- prop(object, nm)
    if (is.null(value)) {
      next
    }
    gate <- get_spec_fields(cls@properties[[nm]])[["applies_when"]]
    if (!any(names(gate) %in% names(grid))) {
      next
    }
    if (!nm %in% names(grid)) {
      grid[[nm]] <- value
    }
    closed <- logical(NROW(grid))
    for (gate_name in names(gate)) {
      gate_values <- if (gate_name %in% names(grid)) {
        grid[[gate_name]]
      } else {
        prop(object, gate_name)
      }
      closed <- closed | !(gate_values %in% gate[[gate_name]])
    }
    grid[[nm]][closed] <- NA
  }
  grid <- unique(grid)
  rownames(grid) <- NULL
  grid
} # /rtemis::gate_tuning_grid


# %% tuning_grid.Hyperparameters ----
#' @author EDG
#' @keywords internal
#' @noRd
method(tuning_grid, Hyperparameters) <- function(x) {
  grid_params <- get_hyperparams_need_tuning(x)
  if (length(grid_params) == 0L) {
    return(NULL)
  }
  # A domain is unwrapped into the candidates themselves: a scalar
  # hyperparameter's back into a vector, which is the shape `expand.grid()` and
  # every downstream reader already handle, and a vector-valued one's into a
  # list, so that one grid cell holds one whole value rather than one element
  # of it. The declaration decides which, never the shape of the candidates.
  specs <- S7_class(x)@properties
  grid_params <- stats::setNames(
    lapply(names(grid_params), function(nm) {
      value <- grid_params[[nm]]
      if (!is_candidates(value)) {
        return(value)
      }
      spec <- get_spec(specs[[nm]])
      if (!is.null(spec) && spec@container != "none") {
        value@candidates
      } else {
        unlist(value@candidates, use.names = FALSE)
      }
    }),
    names(grid_params)
  )
  # expand_grid converts a NULL search entry to its "null" sentinel.
  gate_tuning_grid(expand_grid(grid_params, stringsAsFactors = FALSE), x)
} # /rtemis::tuning_grid.Hyperparameters


# %% grid_row_values ----
#' One tuning-grid row as hyperparameter values
#'
#' A container tunable's candidates are a **list column** -- one grid cell holds
#' one whole value, such as an entire architecture -- so `as.list()` over the row
#' leaves that value inside a one-element list. Every other column is atomic and
#' `as.list()` is already right. Unwrapping here rather than at each call site
#' keeps the two readers of a grid row (the cell that is fitted, and the winning
#' combination that is reported) reading it the same way.
#'
#' @param grid data.frame: The tuning grid.
#' @param index Integer: Row to read.
#' @param columns Columns to read, as any `[` subscript.
#'
#' @return Named list of hyperparameter values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
grid_row_values <- function(grid, index, columns = TRUE) {
  values <- as.list(grid[index, columns, drop = FALSE])
  lapply(values, function(value) {
    if (is.list(value) && length(value) == 1L) value[[1L]] else value
  })
} # /rtemis::grid_row_values


# %% get_hyperparams.(Hyperparameters, class_character) ----
method(get_hyperparams, list(Hyperparameters, class_character)) <- function(
  x,
  param_names
) {
  sapply(param_names, function(p) x@hyperparameters[p], USE.NAMES = FALSE)
} # /rtemis::get_hyperparams.Hyperparameters


# %% GLMHyperparameters ----
#' @title GLMHyperparameters
#'
#' @description
#' Hyperparameters subclass for GLM.
#'
#' @author EDG
#' @keywords internal
#' @noRd
GLMHyperparameters <- new_class(
  name = "GLMHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("GLM"),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::GLMHyperparameters


# %% setup_GLM ----
#' Setup GLM Hyperparameters
#'
#' Setup hyperparameters for GLM training.
#'
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return GLMHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' glm_hyperparams <- setup_GLM(ifw = TRUE)
#' glm_hyperparams
setup_GLM <- function(ifw = FALSE) {
  GLMHyperparameters(ifw = ifw)
} # /rtemis::setup_GLM


# %% GAMHyperparameters ----
#' @title GAMHyperparameters
#'
#' @description
#' Hyperparameters subclass for GAM.
#'
#' @author EDG
#' @keywords internal
#' @noRd
GAMHyperparameters <- new_class(
  name = "GAMHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("GAM"),
    k = prop_integer(
      5L,
      min = 1L,
      tunable = TRUE,
      description = "Number of knots."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::GAMHyperparameters


# %% setup_GAM ----
#' Setup GAM Hyperparameters
#'
#' Setup hyperparameters for GAM training.
#'
#' Get more information from [mgcv::gam].
#'
#' @param k (Tunable) Integer [1, Inf): Number of knots.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return GAMHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' gam_hyperparams <- setup_GAM(k = 5L, ifw = FALSE)
#' gam_hyperparams
setup_GAM <- function(k = 5L, ifw = FALSE) {
  k <- clean_posint(k)
  GAMHyperparameters(k = k, ifw = ifw)
} # /rtemis::setup_GAM


# %% MARSHyperparameters ----
#' @title MARSHyperparameters
#'
#' @description
#' Hyperparameters subclass for MARS.
#'
#' `degree` and `nprune` are the two parameters worth tuning first: the former
#' sets how many features may interact in a term, the latter how many terms
#' survive pruning.
#'
#' `earth` cross-validates internally when `nfold` is greater than 1, which is
#' what `pmethod = "cv"` selects the number of terms with. That is separate
#' from, and can be used instead of, tuning `nprune` through `train()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
MARSHyperparameters <- new_class(
  name = "MARSHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("MARS"),
    degree = prop_integer(
      1L,
      min = 1L,
      tunable = TRUE,
      description = "Maximum degree of interaction. 1 builds an additive model with no interaction terms."
    ),
    penalty = prop_float(
      NULL,
      min = -1,
      nullable = TRUE,
      tunable = TRUE,
      description = "Generalized Cross Validation penalty per knot. NULL uses 3 when @degree is greater than 1 and 2 otherwise. 0 penalizes terms but not knots, and -1 removes the penalty."
    ),
    nk = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Maximum number of terms, including the intercept, created by the forward pass. NULL lets earth derive it from the number of features."
    ),
    nprune = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Maximum number of terms, including the intercept, retained after pruning. NULL keeps every term the forward pass created."
    ),
    thresh = prop_float(
      0.001,
      min = 0,
      exclusive_max = 1,
      tunable = TRUE,
      description = "Forward pass stopping threshold: stop once adding a term changes R-squared by less than this."
    ),
    minspan = prop_integer(
      0L,
      tunable = TRUE,
      description = "Minimum number of observations between knots. 0 derives the value internally, and a negative value instead sets the maximum number of equally spaced knots per feature."
    ),
    endspan = prop_integer(
      0L,
      min = 0L,
      tunable = TRUE,
      description = "Minimum number of observations before the first and after the final knot. 0 derives the value internally."
    ),
    newvar_penalty = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "Penalty for adding a feature not already in the model during the forward pass. 0 applies no penalty; useful values typically range from 0.01 to 0.2."
    ),
    fast_k = prop_integer(
      20L,
      min = 0L,
      tunable = TRUE,
      description = "Maximum number of parent terms considered at each step of the forward pass. 0 disables Fast MARS, which is slower but builds a better model."
    ),
    pmethod = prop_string(
      "backward",
      enum = c("backward", "none", "exhaustive", "forward", "seqrep", "cv"),
      description = "Pruning method. \"cv\" selects the number of terms by cross-validation and requires @nfold. Multiclass classification allows only \"backward\" and \"none\"."
    ),
    nfold = prop_integer(
      0L,
      min = 0L,
      data_bound = "n_cases",
      description = "Number of cross-validation folds used to estimate out-of-fold R-squared. 0 disables cross-validation."
    ),
    ncross = prop_integer(
      1L,
      min = 1L,
      description = "Number of times the @nfold cross-validation is repeated. Applies only when @nfold is greater than 1."
    ),
    stratify = prop_boolean(
      TRUE,
      description = "Stratify the cross-validation folds on the outcome. Applies only when @nfold is greater than 1."
    ),
    fast_beta = prop_float(
      1,
      min = 0,
      max = 1,
      description = "Fast MARS aging coefficient. 0 sometimes gives better results."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  ),
  validator = function(self) {
    if (identical(self@pmethod, "cv") && self@nfold < 2L) {
      "@pmethod \"cv\" selects the number of terms by cross-validation, so @nfold must be at least 2."
    }
  }
) # /rtemis::MARSHyperparameters


# %% setup_MARS ----
#' Setup MARS Hyperparameters
#'
#' Setup hyperparameters for Multivariate Adaptive Regression Splines training.
#'
#' Get more information from [earth::earth].
#'
#' `get_varimp()` returns earth's three importance criteria, in this order:
#' `importance` (the GCV criterion), `rss` (the RSS criterion), and
#' `subset_proportion` (the fraction of pruning subsets that retain the
#' feature). See `varimp_super` in `train_MARS.R` for how each is derived.
#'
#' @param degree (Tunable) Integer [1, Inf): Maximum degree of interaction. 1 builds an additive model with no interaction terms.
#' @param penalty (Tunable) Optional Numeric [-1, Inf): Generalized Cross Validation penalty per knot. NULL uses 3 when `degree` is greater than 1 and 2 otherwise.
#' @param nk (Tunable) Optional Integer [1, Inf): Maximum number of terms, including the intercept, created by the forward pass. NULL lets earth derive it from the number of features.
#' @param nprune (Tunable) Optional Integer [1, Inf): Maximum number of terms, including the intercept, retained after pruning. NULL keeps every term the forward pass created.
#' @param thresh (Tunable) Numeric [0, 1): Forward pass stopping threshold: stop once adding a term changes R-squared by less than this.
#' @param minspan (Tunable) Integer (-Inf, Inf): Minimum number of observations between knots. 0 derives the value internally, and a negative value instead sets the maximum number of equally spaced knots per feature.
#' @param endspan (Tunable) Integer [0, Inf): Minimum number of observations before the first and after the final knot. 0 derives the value internally.
#' @param newvar_penalty (Tunable) Numeric [0, Inf): Penalty for adding a feature not already in the model during the forward pass.
#' @param fast_k (Tunable) Integer [0, Inf): Maximum number of parent terms considered at each step of the forward pass. 0 disables Fast MARS.
#' @param pmethod Character \{"backward", "none", "exhaustive", "forward", "seqrep", "cv"\}: Pruning method. "cv" requires `nfold`. Multiclass classification allows only "backward" and "none".
#' @param nfold Integer [0, Inf): Number of cross-validation folds used to estimate out-of-fold R-squared. 0 disables cross-validation.
#' @param ncross Integer [1, Inf): Number of times the `nfold` cross-validation is repeated.
#' @param stratify Logical: If TRUE, stratify the cross-validation folds on the outcome.
#' @param fast_beta Numeric \[0, 1\]: Fast MARS aging coefficient.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return MARSHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' mars_hyperparams <- setup_MARS(degree = 2L, nprune = 10L)
#' mars_hyperparams
setup_MARS <- function(
  # tunable
  degree = 1L,
  penalty = NULL,
  nk = NULL,
  nprune = NULL,
  thresh = 0.001,
  minspan = 0L,
  endspan = 0L,
  newvar_penalty = 0,
  fast_k = 20L,
  # fixed
  pmethod = "backward",
  nfold = 0L,
  ncross = 1L,
  stratify = TRUE,
  fast_beta = 1,
  ifw = FALSE
) {
  degree <- clean_posint(degree)
  nk <- clean_posint(nk)
  nprune <- clean_posint(nprune)
  minspan <- clean_int(minspan)
  endspan <- clean_int(endspan)
  fast_k <- clean_int(fast_k)
  nfold <- clean_int(nfold)
  ncross <- clean_posint(ncross)
  MARSHyperparameters(
    degree = degree,
    penalty = penalty,
    nk = nk,
    nprune = nprune,
    thresh = thresh,
    minspan = minspan,
    endspan = endspan,
    newvar_penalty = newvar_penalty,
    fast_k = fast_k,
    pmethod = pmethod,
    nfold = nfold,
    ncross = ncross,
    stratify = stratify,
    fast_beta = fast_beta,
    ifw = ifw
  )
} # /rtemis::setup_MARS


# %% CARTHyperparameters ----
#' @title CARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for CART.
#'
#' @author EDG
#' @keywords internal
#' @noRd
CARTHyperparameters <- new_class(
  name = "CARTHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("CART"),
    cp = prop_float(
      0.01,
      min = 0,
      tunable = TRUE,
      description = "Complexity parameter."
    ),
    maxdepth = prop_integer(
      20L,
      min = 1L,
      max = 30L,
      tunable = TRUE,
      description = "Maximum depth of tree (rpart limit: 30)."
    ),
    minsplit = prop_integer(
      2L,
      min = 1L,
      tunable = TRUE,
      description = "Minimum number of observations in a node to attempt a split."
    ),
    minbucket = prop_integer(
      1L,
      min = 1L,
      tunable = TRUE,
      description = "Minimum number of observations in a terminal node."
    ),
    prune_cp = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      tunable = TRUE,
      description = "Complexity for cost-complexity pruning after the tree is built."
    ),
    method = prop_string(
      "auto",
      enum = c("auto", "anova", "class", "poisson", "exp"),
      description = "Splitting method. auto = set from outcome type."
    ),
    model = prop_boolean(
      TRUE,
      description = "Keep a copy of the model frame in the result."
    ),
    maxcompete = prop_integer(
      4L,
      min = 0L,
      description = "Number of competitor splits retained in the output."
    ),
    maxsurrogate = prop_integer(
      5L,
      min = 0L,
      description = "Number of surrogate splits retained in the output."
    ),
    usesurrogate = prop_integer(
      2L,
      min = 0L,
      max = 2L,
      description = "How to use surrogates in the splitting process."
    ),
    surrogatestyle = prop_integer(
      0L,
      min = 0L,
      max = 1L,
      description = "Controls the selection of the best surrogate."
    ),
    xval = prop_integer(
      0L,
      min = 0L,
      description = "Number of rpart-internal cross-validation folds."
    ),
    cost = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_features",
      description = "Variable costs."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::CARTHyperparameters


# %% setup_CART ----
#' Setup CART Hyperparameters
#'
#' Setup hyperparameters for CART training.
#'
#' Get more information from [rpart::rpart] and [rpart::rpart.control].
#'
#' @param cp (Tunable) Numeric [0, Inf): Complexity parameter.
#' @param maxdepth (Tunable) Integer \[1, 30\]: Maximum depth of tree.
#' @param minsplit (Tunable) Integer [1, Inf): Minimum number of observations in a node to split.
#' @param minbucket (Tunable) Integer [1, Inf): Minimum number of observations in a terminal node.
#' @param prune_cp (Tunable) Optional Numeric [0, Inf): Complexity for cost-complexity pruning after tree is built.
#' @param method Character \{"auto", "anova", "class", "poisson", "exp"\}: Splitting method.
#' @param model Logical: If TRUE, keep a copy of the model frame.
#' @param maxcompete Integer [0, Inf): Maximum number of competitive splits.
#' @param maxsurrogate Integer [0, Inf): Maximum number of surrogate splits.
#' @param usesurrogate Integer \[0, 2\]: Number of surrogate splits to use.
#' @param surrogatestyle Integer \[0, 1\]: Type of surrogate splits.
#' @param xval Integer [0, Inf): Number of cross-validation folds.
#' @param cost Optional Numeric (0, Inf) vector: One for each feature.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return CARTHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' cart_hyperparams <- setup_CART(cp = 0.01, maxdepth = 10L, ifw = TRUE)
#' cart_hyperparams
setup_CART <- function(
  # tunable
  cp = 0.01,
  maxdepth = 20L,
  minsplit = 2L,
  minbucket = 1L, # round(minsplit / 3),
  prune_cp = NULL,
  # fixed
  method = "auto",
  model = TRUE,
  maxcompete = 4L,
  maxsurrogate = 5L,
  usesurrogate = 2L,
  surrogatestyle = 0L,
  xval = 0L,
  cost = NULL,
  ifw = FALSE
) {
  maxdepth <- clean_int(maxdepth)
  minsplit <- clean_int(minsplit)
  minbucket <- clean_int(minbucket)
  maxcompete <- clean_int(maxcompete)
  maxsurrogate <- clean_int(maxsurrogate)
  usesurrogate <- clean_int(usesurrogate)
  surrogatestyle <- clean_int(surrogatestyle)
  xval <- clean_int(xval)
  CARTHyperparameters(
    cp = cp,
    maxdepth = maxdepth,
    minsplit = minsplit,
    minbucket = minbucket,
    prune_cp = prune_cp,
    method = method,
    model = model,
    maxcompete = maxcompete,
    maxsurrogate = maxsurrogate,
    usesurrogate = usesurrogate,
    surrogatestyle = surrogatestyle,
    xval = xval,
    cost = cost,
    ifw = ifw
  )
} # /rtemis::setup_CART


# %% LINAD_LEAF_MODELS ----
# The leaf model is one flat choice of procedure *and* regularization rather
# than a model/penalty pair, because an `applies_when` gate may not name a
# property that is itself gated. "constant" fits an intercept only, which is
# what reduces LINAD to the Additive Tree.
LINAD_LEAF_MODELS <- c("forward", "ridge", "elasticnet", "constant")


# %% LINADHyperparameters ----
#' @title LINADHyperparameters
#'
#' @description
#' Hyperparameters subclass for LINAD, the Linear Additive Tree.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LINADHyperparameters <- new_class(
  name = "LINADHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("LINAD"),
    # Growth ----
    max_leaves = prop_integer(
      20L,
      min = 1L,
      tunable = TRUE,
      description = "Largest number of terminal nodes to grow. Plays the role the number of trees plays in gradient boosting."
    ),
    force_max_leaves = prop_boolean(
      FALSE,
      description = "Keep every leaf grown instead of selecting a tree size on the validation set."
    ),
    smooth_validation_curve = prop_boolean(
      NULL,
      nullable = TRUE,
      applies_when = list(force_max_leaves = FALSE),
      description = "Smooth the validation curve before reading its minimum. Steadier when the validation set is small."
    ),
    min_cases_split = prop_integer(
      2L,
      min = 2L,
      tunable = TRUE,
      description = "Fewest cases a node may hold and still be considered for splitting."
    ),
    min_cases_leaf = prop_integer(
      1L,
      min = 1L,
      tunable = TRUE,
      description = "Fewest cases a split must leave on each side."
    ),
    min_cases_node_model = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(
        node_model = c("forward", "ridge", "elasticnet")
      ),
      description = "Fewest cases needed to fit a linear model at a node. Below it the node inherits its parent unchanged."
    ),
    # Leaf model ----
    node_model = prop_string(
      "forward",
      enum = LINAD_LEAF_MODELS,
      description = "Model fitted at each node. constant is the intercept-only model every decision tree node carries; the others add a regularized linear model on top of it."
    ),
    nvmax = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(node_model = "forward"),
      description = "Number of terms forward selection adds beside the intercept. A term count, not a ceiling; capped at the width of the design."
    ),
    lambda = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(node_model = c("ridge", "elasticnet")),
      description = "L2 penalty on the leaf models, on a standardized design so one value means the same at every node."
    ),
    alpha = prop_float(
      NULL,
      min = 0,
      max = 1,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(node_model = "elasticnet"),
      description = "Elastic-net mixing: 0 is ridge, 1 is lasso."
    ),
    learning_rate = prop_float(
      0.1,
      exclusive_min = 0,
      max = 1,
      tunable = TRUE,
      description = "Shrinkage applied to every functional update."
    ),
    # Root model ----
    root_model = prop_string(
      NULL,
      enum = LINAD_LEAF_MODELS,
      nullable = TRUE,
      description = "Model fitted at the root. NULL uses node_model."
    ),
    root_nvmax = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "nvmax for the root model. NULL uses nvmax. Ignored unless the root model is forward selection."
    ),
    root_lambda = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      tunable = TRUE,
      description = "lambda for the root model. NULL uses lambda. Ignored unless the root model is penalized."
    ),
    root_alpha = prop_float(
      NULL,
      min = 0,
      max = 1,
      nullable = TRUE,
      tunable = TRUE,
      description = "alpha for the root model. NULL uses alpha. Ignored unless the root model is an elastic net."
    ),
    root_learning_rate = prop_float(
      1,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Shrinkage applied to the root model's slopes. 0 fits no root model, so the first step is a split; 1 fits it in full."
    ),
    # Splitting ----
    split_search = prop_string(
      "stump",
      enum = c("stump", "exhaustive"),
      description = "How a split is chosen. stump takes the best squared-error split of the gradient; exhaustive scores each candidate by the loss after fitting both child models."
    ),
    split_binning = prop_integer(
      NULL,
      min = 2L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Discretize each numeric feature into this many equal-frequency bins and consider only bin boundaries as splits. Applies to both split searches. NULL considers every distinct value."
    ),
    split_bin_type = prop_string(
      "frequency",
      enum = c("frequency", "width"),
      tunable = TRUE,
      description = "How split_binning places bin edges: frequency spaces them evenly through the cases, width evenly through the feature's range."
    ),
    n_cuts = prop_integer(
      NULL,
      min = 2L,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(split_search = "exhaustive"),
      description = "Number of cut points tried per feature by the exhaustive search. split_bin_type decides how they are spaced."
    ),
    # Soft weighting ----
    gamma = prop_float(
      0.1,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Weight retained by a case in the branch it does not belong to. 0 is a hard partition; larger values share influence and reduce variance."
    ),
    # Step size and node selection ----
    line_search = prop_string(
      "expansion",
      enum = c("expansion", "child", "none"),
      tunable = TRUE,
      description = "Scope of the Newton step for each update: one per expansion, one per child, or none."
    ),
    line_search_max = prop_float(
      1000,
      exclusive_min = 0,
      description = "Largest absolute step the line search may take."
    ),
    constant_rule = prop_string(
      "closed_form",
      enum = c("closed_form", "least_squares"),
      tunable = TRUE,
      description = "Rule for a node's constant: the manuscript's closed forms, or the weighted mean of the residual. Identical for regression; they differ only for classification."
    ),
    node_selection = prop_string(
      "local",
      enum = c("local", "global"),
      tunable = TRUE,
      description = "Which frontier node to split next: by the loss reduction over its own cases, or over every case with this node's model extrapolated."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  ),
  validator = function(self) {
    check_applies_when(self)
  }
) # /rtemis::LINADHyperparameters


# %% setup_LINAD ----
#' Setup LINAD Hyperparameters
#'
#' Setup hyperparameters for LINAD, the Linear Additive Tree. LINAD
#' **generalizes** the decision tree and the regularized linear model: CART, the
#' Additive Tree and a penalized linear fit are each a special case of it,
#' recovered exactly by the hyperparameter combinations given under Details.
#'
#' @details
#' Every node carries a constant, as any decision tree's nodes do. `node_model`
#' selects the model fitted at each node: `"constant"` for that constant alone,
#' or a regularized linear model on top of it. That, with `max_leaves` and
#' `gamma`, is what makes LINAD a generalization rather than a hybrid -- the
#' familiar algorithms are points in its hyperparameter space, and everything
#' between them is reachable too.
#'
#' @section What LINAD generalizes:
#' Each of the following is a special case of LINAD, recovered exactly rather
#' than approximated:
#'
#' \describe{
#'   \item{A pure linear model}{`max_leaves = 1`. Nothing is split, so the model
#'     is the root's alone -- ridge, elastic net or forward stepwise selection,
#'     whichever `node_model` names. Verified against a direct penalized solve.}
#'   \item{CART}{`node_model = "constant"`, `gamma = 0`, `learning_rate = 1`,
#'     `line_search = "none"`. Constants only, a hard partition and no
#'     shrinkage. Fitted values match `rpart`'s to machine precision.}
#'   \item{The Additive Tree}{The same, with `gamma > 0`. Soft weighting is
#'     precisely what separates the Additive Tree from CART (Luna and
#'     colleagues, 2019; see References).}
#'   \item{LINAD}{Any `node_model` other than `"constant"`, with
#'     `max_leaves > 1` -- a tree whose nodes each carry a linear model.}
#' }
#'
#' Every setting named above is tunable, so where a dataset belongs on that
#' space is a question the data can answer rather than one to settle in advance:
#' a linear generating process should select a full root fit and no splitting,
#' while terminal nodes needing opposite-signed coefficients should select a
#' split first. `root_learning_rate` is the dial for that first step -- at 0 the
#' root is the outcome's mean and the tree splits first, at 1 a full linear
#' model is fitted before any split.
#'
#' @section How it grows:
#' LINAD fits a regularized linear model on the whole sample, splits on the
#' gradient of the loss, fits a model on each side of the split, and repeats.
#' Every update is linear, so the coefficients along a root-to-leaf path sum:
#' the finished model is a tree with one linear model per leaf, optimized
#' stagewise rather than fitted leaf by leaf. Prediction routes a case to its
#' leaf and evaluates that leaf's coefficients.
#'
#' Splits are found on the features as given, so a factor splits on a set of its
#' levels; the node models use a reference-coded design matrix built internally.
#' LINAD therefore needs no encoding preprocessor and handles factors directly.
#'
#' `max_leaves` plays the role that the number of trees plays in gradient
#' boosting, and like it is chosen on held-out data: the tree is grown to
#' `max_leaves` and the size with the lowest validation loss is kept, unless
#' `force_max_leaves` is TRUE. Pass `dat_validation` to [train] to control the
#' set that choice is made on.
#'
#' `gamma` lets every case carry weight into both branches, decaying as
#' `gamma^depth`, so each node's model is pulled toward what the rest of the
#' data supports. It is the algorithm's main variance-reducing device.
#'
#' `split_search = "exhaustive"` scores each candidate split by the loss after
#' fitting both child models, rather than taking the best split of the gradient.
#' It costs more per split and can find structure the gradient stump cannot see
#' at all -- an interaction that changes a slope without changing either side's
#' mean is invisible to a stump by construction.
#'
#' `split_binning` discretizes numeric features before either search, so a
#' candidate split falls on a bin boundary rather than between any two distinct
#' values; `n_cuts` then thins what is left, for the exhaustive search only.
#' `split_bin_type` governs both: `"frequency"` spaces cuts evenly through the
#' cases, `"width"` evenly through the feature's range.
#'
#' @section Learning rate and the root model:
#' `learning_rate` shrinks the update made at each **node**. It does not shrink
#' the root, which `root_learning_rate` governs and which defaults to 1: the
#' root model is the initialization `F_0`, not a boosting step, and shrinking an
#' initialization is not meaningful.
#'
#' The consequence is worth stating, because it surprises: **however small
#' `learning_rate` is, the fit never falls below a single global linear model.**
#' On data whose signal is largely linear, that model is already good, so
#' shrinking the tree's correction moves the metrics very little and two very
#' different small learning rates look alike. On a signal the root cannot
#' represent -- a step with no linear component -- the same sweep moves test
#' R-squared from 0.58 to 0.97.
#'
#' To shrink everything, including the root, set `root_learning_rate` to the
#' same value. That recovers the familiar picture in which a small rate leaves
#' the model badly under-fitted: at `1e-6` the fit reduces to the constant that
#' alone minimizes the loss -- the weighted mean of the outcome, or half its log
#' odds for a classification -- which scores an R-squared of 0 rather than
#' anything worse. `root_learning_rate` interpolates between that constant and
#' the full linear model, never towards zero.
#'
#' The `line_search` step does **not** counteract this. It is scale-free -- for
#' squared error it is the exact minimizer along the update direction, which is
#' near 1 whatever the residual's size -- so the applied step stays proportional
#' to `learning_rate`. Measured node updates scale linearly with it, by a factor
#' of 50 across a 50-fold change in the rate.
#'
#' `line_search`, `node_selection` and `constant_rule` each select among
#' alternative update rules -- the scope of the Newton step, which frontier node
#' is split next, and how a node's constant is computed. There is no settled
#' best choice among them, so each is tunable.
#'
#' @param max_leaves (Tunable) Integer [1, Inf): Largest number of terminal nodes to grow.
#' @param force_max_leaves Logical: If TRUE, keep every leaf grown instead of selecting a size on the validation set.
#' @param smooth_validation_curve Optional Logical: If TRUE, smooth the validation curve before reading its minimum. Applies only when `force_max_leaves` is FALSE.
#' @param min_cases_split (Tunable) Integer [2, Inf): Fewest cases a node may hold and still be considered for splitting.
#' @param min_cases_leaf (Tunable) Integer [1, Inf): Fewest cases a split must leave on each side.
#' @param min_cases_node_model (Tunable) Optional Integer [1, Inf): Fewest cases needed to fit a linear model at a node. Applies only when `node_model` fits one.
#' @param node_model Character \{"forward", "ridge", "elasticnet", "constant"\}: Model fitted at each node. "constant" is the intercept-only model every decision tree node carries; the others add a regularized linear model on top of it.
#' @param nvmax (Tunable) Optional Integer [1, Inf): Terms forward selection adds beside the intercept. Applies only when `node_model` is "forward".
#' @param lambda (Tunable) Optional Numeric [0, Inf): L2 penalty on the leaf models. Applies only when `node_model` is "ridge" or "elasticnet".
#' @param alpha (Tunable) Optional Numeric \[0, 1\]: Elastic-net mixing, 0 ridge to 1 lasso. Applies only when `node_model` is "elasticnet".
#' @param learning_rate (Tunable) Numeric (0, 1\]: Shrinkage applied to every functional update.
#' @param root_model Optional Character \{"forward", "ridge", "elasticnet", "constant"\}: Model fitted at the root. NULL uses `node_model`.
#' @param root_nvmax (Tunable) Optional Integer [1, Inf): `nvmax` for the root model. NULL uses `nvmax`.
#' @param root_lambda (Tunable) Optional Numeric [0, Inf): `lambda` for the root model. NULL uses `lambda`.
#' @param root_alpha (Tunable) Optional Numeric \[0, 1\]: `alpha` for the root model. NULL uses `alpha`.
#' @param root_learning_rate (Tunable) Numeric \[0, 1\]: Shrinkage applied to the root model's slopes.
#' @param split_search Character \{"stump", "exhaustive"\}: How a split is chosen.
#' @param split_binning (Tunable) Optional Integer [2, Inf): Discretize each numeric feature into this many bins and consider only bin boundaries as splits. Applies to both split searches.
#' @param split_bin_type (Tunable) Character \{"frequency", "width"\}: How bin edges are placed.
#' @param n_cuts (Tunable) Optional Integer [2, Inf): Cut points tried per feature; `split_bin_type` decides their spacing. Applies only when `split_search` is "exhaustive".
#' @param gamma (Tunable) Numeric \[0, 1\]: Weight a case retains in the branch it does not belong to. 0 is a hard partition.
#' @param line_search (Tunable) Character \{"expansion", "child", "none"\}: Scope of the Newton step for each update.
#' @param line_search_max Numeric (0, Inf): Largest absolute step the line search may take.
#' @param constant_rule (Tunable) Character \{"closed_form", "least_squares"\}: How a node's constant is computed. The two coincide for regression and differ only for classification.
#' @param node_selection (Tunable) Character \{"local", "global"\}: Criterion for choosing which frontier node to split next.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return LINADHyperparameters object.
#'
#' @references
#' Luna JM, Gennatas ED, Ungar LH, Eaton E, Diffenderfer ES, Jensen ST,
#' Simone CB 2nd, Friedman JH, Solberg TD, Valdes G (2019).
#' Building more accurate decision trees with the additive tree.
#' \emph{Proceedings of the National Academy of Sciences}, 116(40), 19887-19893.
#' \doi{10.1073/pnas.1816748116}
#'
#' @author EDG
#' @export
#' @examples
#' linad_hyperparams <- setup_LINAD(max_leaves = 10L, learning_rate = 0.1)
#' linad_hyperparams
#' # The Additive Tree is LINAD without the linear models
#' addtree_hyperparams <- setup_LINAD(node_model = "constant", gamma = 0.8)
#' addtree_hyperparams
setup_LINAD <- function(
  # tunable
  max_leaves = 20L,
  min_cases_split = 2L,
  min_cases_leaf = 1L,
  min_cases_node_model = NULL,
  nvmax = NULL,
  lambda = NULL,
  alpha = NULL,
  learning_rate = 0.1,
  root_nvmax = NULL,
  root_lambda = NULL,
  root_alpha = NULL,
  root_learning_rate = 1,
  n_cuts = NULL,
  gamma = 0.1,
  split_binning = NULL,
  split_bin_type = "frequency",
  line_search = "expansion",
  constant_rule = "closed_form",
  node_selection = "local",
  ifw = FALSE,
  # fixed
  force_max_leaves = FALSE,
  smooth_validation_curve = NULL,
  node_model = "forward",
  root_model = NULL,
  split_search = "stump",
  line_search_max = 1000
) {
  max_leaves <- clean_int(max_leaves)
  min_cases_split <- clean_int(min_cases_split)
  min_cases_leaf <- clean_int(min_cases_leaf)
  min_cases_node_model <- clean_int(min_cases_node_model)
  nvmax <- clean_int(nvmax)
  root_nvmax <- clean_int(root_nvmax)
  n_cuts <- clean_int(n_cuts)
  split_binning <- clean_int(split_binning)
  LINADHyperparameters(
    max_leaves = max_leaves,
    force_max_leaves = force_max_leaves,
    smooth_validation_curve = smooth_validation_curve,
    min_cases_split = min_cases_split,
    min_cases_leaf = min_cases_leaf,
    min_cases_node_model = min_cases_node_model,
    node_model = node_model,
    nvmax = nvmax,
    lambda = lambda,
    alpha = alpha,
    learning_rate = learning_rate,
    root_model = root_model,
    root_nvmax = root_nvmax,
    root_lambda = root_lambda,
    root_alpha = root_alpha,
    root_learning_rate = root_learning_rate,
    split_search = split_search,
    split_binning = split_binning,
    split_bin_type = split_bin_type,
    n_cuts = n_cuts,
    gamma = gamma,
    line_search = line_search,
    line_search_max = line_search_max,
    constant_rule = constant_rule,
    node_selection = node_selection,
    ifw = ifw
  )
} # /rtemis::setup_LINAD


# %% GLMNETHyperparameters ----
#' @title GLMNETHyperparameters
#'
#' @description
#' Hyperparameters subclass for GLMNET. `lambda.min` and `lambda.1se` are
#' runtime state written by the Tuner (from cv.glmnet), not settable
#' hyperparameters -- exclude them from schema generation.
#'
#' @author EDG
#' @keywords internal
#' @noRd
GLMNETHyperparameters <- new_class(
  name = "GLMNETHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("GLMNET"),
    alpha = prop_float(
      1,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Elastic net mixing parameter (0 = ridge, 1 = lasso)."
    ),
    family = prop_string(
      NULL,
      enum = c(
        "gaussian",
        "binomial",
        "poisson",
        "multinomial",
        "cox",
        "mgaussian"
      ),
      nullable = TRUE,
      default_on_null = TRUE,
      description = "GLM family. NULL = set from outcome type."
    ),
    offset = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_cases",
      description = "Offset."
    ),
    which_lambda_cv = prop_string(
      "lambda.1se",
      enum = c("lambda.1se", "lambda.min"),
      description = "Which cross-validated lambda to use for prediction."
    ),
    nlambda = prop_integer(
      100L,
      min = 1L,
      description = "Number of lambda values."
    ),
    lambda = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      vector = TRUE,
      tune_on_null = TRUE,
      description = "Regularization strength. NULL = determined by cv.glmnet during tuning."
    ),
    penalty_factor = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      vector = TRUE,
      description = "Penalty factor, one per feature."
    ),
    standardize = prop_boolean(
      TRUE,
      description = "Standardize features."
    ),
    intercept = prop_boolean(
      TRUE,
      description = "Include intercept."
    ),
    ifw = prop_boolean(
      TRUE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    ),
    # Run state, written by the Tuner from cv.glmnet results.
    `lambda.min` = prop_state(prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      description = "Lambda minimizing cross-validated error."
    )),
    `lambda.1se` = prop_state(prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      description = "Largest lambda within one standard error of the minimum."
    ))
  )
) # /rtemis::GLMNETHyperparameters


# %% setup_GLMNET ----
#' Setup GLMNET Hyperparameters
#'
#' Setup hyperparameters for GLMNET training.
#'
#' Get more information from [glmnet::glmnet].
#'
#' @param alpha (Tunable) Numeric \[0, 1\]: Elastic net mixing parameter.
#' @param family Optional Character \{"gaussian", "binomial", "poisson", "multinomial", "cox", "mgaussian"\}: Family. NULL = set from outcome type.
#' @param offset Optional Numeric vector: Offset, one value per case.
#' @param which_lambda_cv Character \{"lambda.1se", "lambda.min"\}: Which lambda to use for prediction.
#' @param nlambda Integer [1, Inf): Number of lambda values.
#' @param lambda Optional Numeric [0, Inf) vector: Lambda values. NULL = determined by cv.glmnet during tuning.
#' @param penalty_factor Optional Numeric [0, Inf) vector: Penalty factor for each feature.
#' @param standardize Logical: If TRUE, standardize features.
#' @param intercept Logical: If TRUE, include intercept.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return GLMNETHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' glm_hyperparams <- setup_GLMNET(alpha = 1, ifw = TRUE)
#' glm_hyperparams
setup_GLMNET <- function(
  # tunable
  alpha = 1,
  # fixed
  family = NULL,
  offset = NULL,
  which_lambda_cv = "lambda.1se",
  nlambda = 100L,
  lambda = NULL,
  penalty_factor = NULL,
  standardize = TRUE,
  intercept = TRUE,
  ifw = TRUE
) {
  nlambda <- clean_posint(nlambda)
  GLMNETHyperparameters(
    alpha = alpha,
    family = family,
    offset = offset,
    which_lambda_cv = which_lambda_cv,
    nlambda = nlambda,
    lambda = lambda,
    penalty_factor = penalty_factor,
    standardize = standardize,
    intercept = intercept,
    ifw = ifw
  )
} # /rtemis::setup_GLMNET


# %% HALHyperparameters ----
#' @title HALHyperparameters
#'
#' @description
#' Hyperparameters subclass for the Highly Adaptive Lasso.
#'
#' `lambda` is not a hyperparameter here: `hal9001` selects it by
#' cross-validation inside the fit, so `cv_select` is a constant. Exposing it
#' as a search dimension would nest that cross-validation inside rtemis' own
#' resampling for no gain.
#'
#' `num_knots` is paired with `max_degree` in a way no property declaration
#' expresses, so the validator checks it. `reduce_basis` declares its dependence
#' on `smoothness_orders` as an `applies_when` gate, which the validator, the
#' tuning grid, and the published schema all read.
#'
#' @author EDG
#' @keywords internal
#' @noRd
HALHyperparameters <- new_class(
  name = "HALHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("HAL"),
    max_degree = prop_integer(
      2L,
      min = 1L,
      tunable = TRUE,
      data_bound = "n_features",
      description = "Highest order of interaction among features that a basis function may represent."
    ),
    smoothness_orders = prop_integer(
      1L,
      min = 0L,
      max = 9L,
      tunable = TRUE,
      description = "Smoothness of the basis functions: 0 fits zero-order indicators, 1 piecewise linear splines, higher values higher-order splines."
    ),
    num_knots = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      vector = TRUE,
      description = "Number of knots per interaction degree, one value per degree, non-increasing. NULL generates them from max_degree and smoothness_orders."
    ),
    reduce_basis = prop_float(
      NULL,
      exclusive_min = 0,
      max = 1,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(smoothness_orders = 0L),
      description = "Minimum proportion of cases a basis function must be non-zero in to be kept. NULL uses the backend default of 1/sqrt(n)."
    ),
    max_basis = prop_integer(
      5000000L,
      min = 1L,
      description = "Largest projected basis-function count that will be fit. Training aborts above it rather than enumerating a basis that will not finish."
    ),
    cv_select = prop_const(
      TRUE,
      description = "Select lambda by cross-validation inside the fit; determined by the class."
    ),
    use_min = prop_boolean(
      TRUE,
      description = "Select lambda.min from the internal cross-validation. FALSE selects the more heavily penalized lambda.1se."
    ),
    nfolds = prop_integer(
      10L,
      min = 3L,
      description = "Number of folds of the internal cross-validation that selects lambda."
    ),
    seed = prop_integer(
      NULL,
      nullable = TRUE,
      description = "Random seed for the internal cross-validation's fold assignment. NULL leaves it drawn from the ambient RNG."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  ),
  validator = function(self) {
    # The knot vector is indexed by interaction degree, so its length is tied
    # to `max_degree` -- which means `max_degree` must be a single value for
    # the pairing to be well defined.
    if (!is.null(self@num_knots)) {
      if (is_candidates(self@max_degree)) {
        return(
          "@num_knots cannot be combined with a search over @max_degree: it needs one value per degree, so leave it NULL while tuning @max_degree."
        )
      }
      if (length(self@num_knots) != self@max_degree) {
        return(paste0(
          "@num_knots must have one value per interaction degree: expected length ",
          self@max_degree,
          ", got ",
          length(self@num_knots),
          "."
        ))
      }
      if (is.unsorted(rev(self@num_knots))) {
        return(
          "@num_knots must be non-increasing across degrees: higher-order interactions cannot use more knots than lower-order ones."
        )
      }
    }
    # The backend applies the basis reduction only to the zero-order basis and
    # warns that it dropped the request otherwise, which @reduce_basis declares
    # as an `applies_when` gate.
    check_applies_when(self)
  }
) # /rtemis::HALHyperparameters


# %% setup_HAL ----
#' Setup HAL Hyperparameters
#'
#' Setup hyperparameters for Highly Adaptive Lasso training.
#'
#' Both outcome types are fit with [hal9001::fit_hal], which spans the outcome
#' with a basis of indicator or spline terms over every interaction of the
#' features up to `max_degree`, then fits a lasso over that basis.
#' Regression uses the gaussian family, binary classification the binomial one;
#' `hal9001` has no multinomial family, so multiclass classification is not
#' supported.
#'
#' `lambda` is selected by cross-validation inside the fit and is not a search
#' dimension. `seed` fixes that cross-validation's fold assignment; `nfolds`
#' and `use_min` control it.
#'
#' `hal9001` takes a numeric matrix, so factors are one-hot encoded first and
#' the encoder is re-applied at predict time.
#'
#' Scaling: the basis has one function per knot per feature subset, so its size
#' grows as `C(n_features, max_degree)` and, through the knots, with the number
#' of cases -- making the lasso that follows quadratic in the number of cases.
#' Training projects the basis size up front, reports it at `verbosity >= 1`,
#' warns past a million, and aborts past `max_basis`. `max_degree` is the
#' strongest lever on that count, `num_knots` the next.
#'
#' `get_varimp()` reports two measures, both aggregated over the basis
#' functions that involve each feature:
#' - `importance`: the sum of the absolute values of their non-zero
#'   coefficients.
#' - `max_coefficient`: the largest single such absolute coefficient,
#'   separating a feature carried by one strong term from one carried by many
#'   weak ones.
#'
#' `plot_varimp(mod, measure = "max_coefficient")` plots the second; the first
#' is the default. Both read coefficients on the scale of the basis functions.
#' At `smoothness_orders = 0` the basis is made of indicators, so the
#' coefficients are unit-free and directly comparable across features; at
#' higher orders each basis function carries the units of its feature, so scale
#' the features first if they are not already comparable.
#'
#' @param max_degree (Tunable) Integer [1, Inf): Highest order of interaction among features that a basis function may represent.
#' @param smoothness_orders (Tunable) Integer \[0, 9\]: Smoothness of the basis functions: 0 fits zero-order indicators, 1 piecewise linear splines, higher values higher-order splines.
#' @param reduce_basis (Tunable) Optional Numeric (0, 1]: Minimum proportion of cases a basis function must be non-zero in to be kept. Applies only when `smoothness_orders` is 0; a search that also covers higher orders drops it from those grid cells.
#' @param num_knots Optional Integer [1, Inf) vector: Number of knots per interaction degree, one value per degree, non-increasing. NULL generates them from max_degree and smoothness_orders.
#' @param use_min Logical: If TRUE, select `lambda.min` from the internal cross-validation; if FALSE, the more heavily penalized `lambda.1se`.
#' @param nfolds Integer [3, Inf): Number of folds of the internal cross-validation that selects lambda.
#' @param max_basis Integer [1, Inf): Largest projected basis-function count that will be fit.
#' @param seed Optional Integer: Random seed for the internal cross-validation's fold assignment. NULL leaves it drawn from the ambient RNG.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return HALHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' hal_hyperparams <- setup_HAL(max_degree = 1L)
#' hal_hyperparams
setup_HAL <- function(
  # tunable
  max_degree = 2L,
  smoothness_orders = 1L,
  reduce_basis = NULL,
  # fixed
  num_knots = NULL,
  use_min = TRUE,
  nfolds = 10L,
  max_basis = 5000000L,
  seed = NULL,
  ifw = FALSE
) {
  max_degree <- clean_posint(max_degree)
  smoothness_orders <- clean_int(smoothness_orders)
  num_knots <- clean_posint(num_knots)
  nfolds <- clean_posint(nfolds)
  max_basis <- clean_posint(max_basis)
  seed <- clean_int(seed)
  HALHyperparameters(
    max_degree = max_degree,
    smoothness_orders = smoothness_orders,
    num_knots = num_knots,
    reduce_basis = reduce_basis,
    max_basis = max_basis,
    use_min = use_min,
    nfolds = nfolds,
    seed = seed,
    ifw = ifw
  )
} # /rtemis::setup_HAL


# %% MonotonicHALHyperparameters ----
#' @title MonotonicHALHyperparameters
#'
#' @description
#' Hyperparameters subclass for the shape-constrained Highly Adaptive Lasso.
#'
#' This is a separate class from `HALHyperparameters` rather than a set of
#' defaults over it, because the three values that distinguish it are
#' invariants and not choices: the interaction degree is 1, the fit is
#' constrained monotonic non-decreasing in every feature, and no basis-size
#' guardrail is needed at degree 1. None of the three is representable as a
#' property, so no combination of this class's properties can produce a
#' non-monotonic or higher-degree fit.
#'
#' `lambda` is not a hyperparameter here: `hal9001` selects it by
#' cross-validation inside the fit, so `cv_select` is a constant.
#'
#' `reduce_basis` declares its dependence on `smoothness_orders` as an
#' `applies_when` gate, which the validator, the tuning grid, and the published
#' schema all read.
#'
#' @author EDG
#' @keywords internal
#' @noRd
MonotonicHALHyperparameters <- new_class(
  name = "MonotonicHALHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("MonotonicHAL"),
    smoothness_orders = prop_integer(
      1L,
      min = 0L,
      max = 1L,
      tunable = TRUE,
      description = "Smoothness of the basis functions: 0 fits zero-order indicators and yields a step function, 1 fits piecewise linear splines and yields a continuous one."
    ),
    num_knots = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Number of knots spanning each feature. NULL generates them from smoothness_orders."
    ),
    reduce_basis = prop_float(
      NULL,
      exclusive_min = 0,
      max = 1,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(smoothness_orders = 0L),
      description = "Minimum proportion of cases a basis function must be non-zero in to be kept. NULL uses the backend default of 1/sqrt(n)."
    ),
    penalized = prop_boolean(
      TRUE,
      description = "Apply the lasso penalty to the basis functions. FALSE removes it, making the fit the non-parametric maximum likelihood estimate over the monotonic class."
    ),
    cv_select = prop_const(
      TRUE,
      description = "Select lambda by cross-validation inside the fit; determined by the class."
    ),
    use_min = prop_boolean(
      TRUE,
      description = "Select lambda.min from the internal cross-validation. FALSE selects the more heavily penalized lambda.1se."
    ),
    nfolds = prop_integer(
      10L,
      min = 3L,
      description = "Number of folds of the internal cross-validation that selects lambda."
    ),
    seed = prop_integer(
      NULL,
      nullable = TRUE,
      description = "Random seed for the internal cross-validation's fold assignment. NULL leaves it drawn from the ambient RNG."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  ),
  validator = function(self) {
    # The backend applies the basis reduction only to the zero-order basis and
    # warns that it dropped the request otherwise, which @reduce_basis declares
    # as an `applies_when` gate.
    check_applies_when(self)
  }
) # /rtemis::MonotonicHALHyperparameters


# %% setup_MonotonicHAL ----
#' Setup MonotonicHAL Hyperparameters
#'
#' Setup hyperparameters for monotonic Highly Adaptive Lasso training.
#'
#' A Highly Adaptive Lasso restricted to additive, monotonic non-decreasing
#' fits. [hal9001::fit_hal] is given a formula that constrains every basis
#' function's coefficient to be non-negative, and the interaction degree is
#' fixed at 1. Regression uses the gaussian family, binary classification the
#' binomial one; `hal9001` has no multinomial family, so multiclass
#' classification is not supported.
#'
#' The intended use is probability calibration, where the single feature is a
#' classifier's score. Monotonicity is what makes that safe: a calibration map
#' that reorders scores changes the ranking and so changes AUC, and a
#' non-decreasing map cannot. Pass it to [calibrate] to use it instead of the
#' default calibrator, [setup_Isotonic].
#'
#' Relative to `Isotonic`, which is the other monotonic calibrator, this fits on
#' the logit scale, so it does not saturate at 0 and 1 the way the boundary
#' bins of isotonic regression do -- it can still round to an endpoint in
#' double precision
#' when the input scores are themselves extreme. At `smoothness_orders = 1` it
#' is strictly increasing rather than a step function, so it introduces no ties
#' and leaves AUC unchanged.
#'
#' `smoothness_orders` is the one substantive choice. 0 fits indicator basis
#' functions and recovers a step function; 1 fits piecewise linear splines and
#' yields a continuous map.
#'
#' `penalized = FALSE` removes the lasso penalty, giving the non-parametric
#' maximum likelihood estimate over the monotonic class. Combined with
#' `smoothness_orders = 0` that is isotonic regression, up to the logit-scale
#' parameterization.
#'
#' `lambda` is selected by cross-validation inside the fit and is not a search
#' dimension. `seed` fixes that cross-validation's fold assignment; `nfolds`
#' and `use_min` control it. Calibration sets are often small, so `nfolds` is
#' an upper bound: training backs off to as many folds as the data supports at
#' three cases each, and says so at `verbosity >= 1`.
#'
#' `get_varimp()` reports the same two measures as `setup_HAL`, `importance`
#' and `max_coefficient`. Both are of limited use at a single feature.
#'
#' @param smoothness_orders (Tunable) Integer \[0, 1\]: Smoothness of the basis functions: 0 fits zero-order indicators and yields a step function, 1 fits piecewise linear splines and yields a continuous one.
#' @param reduce_basis (Tunable) Optional Numeric (0, 1]: Minimum proportion of cases a basis function must be non-zero in to be kept. Applies only when `smoothness_orders` is 0; a search that also covers higher orders drops it from those grid cells.
#' @param num_knots Optional Integer [1, Inf): Number of knots spanning each feature. NULL generates them from smoothness_orders.
#' @param penalized Logical: If TRUE, apply the lasso penalty to the basis functions; if FALSE, remove it.
#' @param use_min Logical: If TRUE, select `lambda.min` from the internal cross-validation; if FALSE, the more heavily penalized `lambda.1se`.
#' @param nfolds Integer [3, Inf): Largest number of folds of the internal cross-validation that selects lambda.
#' @param seed Optional Integer: Random seed for the internal cross-validation's fold assignment. NULL leaves it drawn from the ambient RNG.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return MonotonicHALHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' monotonichal_hyperparams <- setup_MonotonicHAL(smoothness_orders = 0L)
#' monotonichal_hyperparams
setup_MonotonicHAL <- function(
  # tunable
  smoothness_orders = 1L,
  reduce_basis = NULL,
  # fixed
  num_knots = NULL,
  penalized = TRUE,
  use_min = TRUE,
  nfolds = 10L,
  seed = NULL,
  ifw = FALSE
) {
  smoothness_orders <- clean_int(smoothness_orders)
  num_knots <- clean_posint(num_knots)
  nfolds <- clean_posint(nfolds)
  seed <- clean_int(seed)
  MonotonicHALHyperparameters(
    smoothness_orders = smoothness_orders,
    num_knots = num_knots,
    reduce_basis = reduce_basis,
    penalized = penalized,
    use_min = use_min,
    nfolds = nfolds,
    seed = seed,
    ifw = ifw
  )
} # /rtemis::setup_MonotonicHAL


# %% lightgbm_objective_props ----
#' Objective-specific LightGBM hyperparameters
#'
#' @description
#' The properties that parameterize a LightGBM objective, shared by the four
#' algorithms `lightgbm::lgb.train()` backs.
#'
#' @details
#' One backend behind four wrappers, so a property meaningful to any LightGBM
#' fit is declared once and spliced into each class that can use it. A factory
#' rather than a shared parent class, because S7 constructs an inherited property
#' with the *parent's* default whatever a subclass redeclares -- so a base class
#' could only ever hold properties no algorithm wants to default differently.
#' `prop_metrics_sample()` and `prop_conformal_alpha()` share declarations the
#' same way.
#'
#' Every property here is nullable and unset by default, meaning "whatever
#' LightGBM's own default is". That is deliberate: pinning rtemis defaults onto
#' an objective almost nobody selects would publish values a backend upgrade
#' could silently diverge from, and the four `train_*` functions drop NULL
#' entries before the call, since LightGBM reads a NULL as an empty value and
#' range-checks it (`alpha = NULL` aborts with `Check failed: (alpha) > (0.0)`).
#'
#' The parameters tied to an objective a user must *choose* are gated on
#' `objective` with `applies_when`, so setting one without naming the objective
#' it belongs to is an error rather than a value LightGBM ignores. The three
#' that apply to the objectives rtemis resolves to on its own -- `sigmoid`,
#' `boost_from_average`, `reg_sqrt` -- are left ungated, since gating them would
#' make a user restate the objective rtemis had already chosen for them.
#'
#' @param alpha Logical: Include `alpha`. FALSE on `LightRuleFit`, where the
#' name is its GLMNET step's elastic-net mixing parameter.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_objective_props <- function(alpha = TRUE) {
  props <- list(
    # LightGBM spells the huber and quantile level `alpha` and admits no alias,
    # so this is the backend's own name rather than a choice.
    alpha = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(objective = c("huber", "quantile")),
      description = "Huber delta or quantile level. NULL = LightGBM's default of 0.9."
    ),
    tweedie_variance_power = prop_float(
      NULL,
      min = 1,
      exclusive_max = 2,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(objective = "tweedie"),
      description = "Tweedie power: 1 is Poisson-like, near 2 is gamma-like. NULL = LightGBM's default of 1.5."
    ),
    fair_c = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(objective = "fair"),
      description = "Fair loss scale. NULL = LightGBM's default of 1."
    ),
    poisson_max_delta_step = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(objective = "poisson"),
      description = "Step cap safeguarding Poisson optimization. NULL = LightGBM's default of 0.7."
    ),
    sigmoid = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      tunable = TRUE,
      description = "Sigmoid slope, for a binary or one-vs-all objective. NULL = LightGBM's default of 1."
    ),
    boost_from_average = prop_boolean(
      NULL,
      nullable = TRUE,
      description = "Start from the outcome's mean, for a regression, binary or cross-entropy objective. NULL = LightGBM's default of TRUE."
    ),
    reg_sqrt = prop_boolean(
      NULL,
      nullable = TRUE,
      description = "Fit the square root of the outcome and square the prediction back, for a regression objective. NULL = LightGBM's default of FALSE."
    )
  )
  if (!alpha) {
    props[["alpha"]] <- NULL
  }
  prop_group(props, "objective")
} # /rtemis::lightgbm_objective_props


# %% lightgbm_regularization_props ----
#' Tree-growth regularization hyperparameters
#'
#' @description
#' What LightGBM requires of a split before it will make one, and what it does
#' to the leaf values afterwards.
#'
#' @details
#' Always in effect, so each carries LightGBM's own default as a real value
#' rather than being left unset: a record saying `min_gain_to_split: 0` states
#' what ran, where `null` would send a reader to the backend's documentation.
#' The gated groups -- objective, DART, GOSS -- do the opposite, since a
#' parameter that applies only under a mode has nothing to state until that mode
#' is chosen.
#'
#' `linear_lambda` is offered where `linear_tree` is, being the penalty on the
#' linear models it fits at the leaves.
#'
#' @param min_data_in_leaf Integer: Default for the minimum cases per leaf.
#' @param linear_lambda Logical: Include `linear_lambda`, for the classes that
#' declare `linear_tree`.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_regularization_props <- function(
  min_data_in_leaf = 20L,
  linear_lambda = TRUE
) {
  props <- list(
    min_data_in_leaf = prop_integer(
      min_data_in_leaf,
      min = 0L,
      tunable = TRUE,
      description = "Minimum number of cases in a leaf."
    ),
    min_sum_hessian_in_leaf = prop_float(
      1e-3,
      min = 0,
      tunable = TRUE,
      description = "Minimum sum of hessians in a leaf: the smooth counterpart of a case count."
    ),
    min_gain_to_split = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "Minimum loss reduction a split must buy to be made."
    ),
    max_delta_step = prop_float(
      0,
      tunable = TRUE,
      description = "Cap on each leaf's output; 0 or less leaves it uncapped."
    ),
    path_smooth = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "Smooth each leaf towards its parent, more strongly the fewer cases it holds."
    ),
    extra_trees = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Choose one split threshold per feature at random rather than the best."
    ),
    extra_seed = prop_integer(
      6L,
      description = "Random seed for the thresholds `extra_trees` draws."
    ),
    # Gated, so nullable: a gate says the property does not apply, and NULL is
    # how that is said. The always-in-effect members above carry real defaults.
    linear_lambda = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(linear_tree = TRUE),
      description = "L2 penalty on the linear models fitted at the leaves. NULL = LightGBM's default of 0."
    )
  )
  if (!linear_lambda) {
    props[["linear_lambda"]] <- NULL
  }
  prop_group(props, "regularization")
} # /rtemis::lightgbm_regularization_props


# %% lightgbm_binning_props ----
#' Feature binning and missing-value hyperparameters
#'
#' @description
#' How LightGBM discretizes features before growing trees, and what it treats as
#' missing.
#'
#' @details
#' These are `lgb.Dataset` parameters rather than `lgb.train()` ones, and they
#' reach the backend all the same: `lgb.train()` calls `data$update_params()`
#' before `construct()`, and rtemis hands it an unconstructed dataset. Anything
#' that forced construction earlier in `prepare_lgb_data()` would silently stop
#' them taking effect.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_binning_props <- function() {
  prop_group(
    list(
      max_bin = prop_integer(
        255L,
        min = 2L,
        tunable = TRUE,
        description = "Most bins a feature is discretized into. Fewer is faster and more regularized."
      ),
      min_data_in_bin = prop_integer(
        3L,
        min = 1L,
        tunable = TRUE,
        description = "Minimum number of cases per bin."
      ),
      use_missing = prop_boolean(
        TRUE,
        description = "Handle missing values. FALSE ignores them."
      ),
      zero_as_missing = prop_boolean(
        FALSE,
        description = "Treat zeros as missing, rather than only NA."
      )
    ),
    "binning"
  )
} # /rtemis::lightgbm_binning_props


# %% lightgbm_categorical_props ----
#' Categorical-split hyperparameters
#'
#' @description
#' How LightGBM splits on a categorical feature, which it does natively rather
#' than by one-hot encoding.
#'
#' @details
#' rtemis passes `categorical_feature` when it builds the dataset, so every
#' wrapper can reach these. `max_cat_threshold` and `min_data_per_group` are
#' declared per class, their defaults differing by algorithm.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_categorical_props <- function() {
  prop_group(
    list(
      cat_l2 = prop_float(
        10,
        min = 0,
        tunable = TRUE,
        description = "L2 regularization on a categorical split."
      ),
      cat_smooth = prop_float(
        10,
        min = 0,
        tunable = TRUE,
        description = "Smoothing over categorical levels, which reduces the effect of rare ones."
      ),
      max_cat_to_onehot = prop_integer(
        4L,
        min = 1L,
        tunable = TRUE,
        description = "Below this many levels, split one-versus-rest rather than by partition."
      )
    ),
    "categorical"
  )
} # /rtemis::lightgbm_categorical_props


# %% lightgbm_early_stopping_props ----
#' Early-stopping hyperparameters beside the round count
#'
#' @description
#' How much better a round must be to count as an improvement.
#'
#' @details
#' For the two wrappers that stop early. A LightCART fit is one tree, and
#' LightRuleFit fixes its first stage's round count.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_early_stopping_props <- function() {
  prop_group(
    list(
      early_stopping_min_delta = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "Smallest improvement that resets the early-stopping counter."
      )
    ),
    "early_stopping"
  )
} # /rtemis::lightgbm_early_stopping_props


# %% lightgbm_determinism_props ----
#' Reproducibility hyperparameters
#'
#' @description
#' What makes two runs of one configuration agree.
#'
#' @details
#' `deterministic` costs speed and needs a row- or column-wise histogram forced,
#' which rtemis does by default (`force_col_wise = TRUE`). The seeds here are the
#' ones LightGBM derives per stage; the run's master seed is
#' `setup_ExecutionConfig(seed = )`.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_determinism_props <- function() {
  prop_group(
    list(
      deterministic = prop_boolean(
        FALSE,
        description = "Force reproducible results at some cost in speed. Requires a forced histogram direction."
      ),
      objective_seed = prop_integer(
        5L,
        description = "Random seed for the objective, where it draws."
      )
    ),
    "determinism"
  )
} # /rtemis::lightgbm_determinism_props


# %% lightgbm_execution_props ----
#' Execution hyperparameters
#'
#' @description
#' How LightGBM organizes the work, without changing what it fits -- except
#' `top_k`, which changes which splits the voting learner considers.
#'
#' @param top_k Logical: Include `top_k`, for the classes that declare
#' `tree_learner`.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_execution_props <- function(top_k = TRUE) {
  props <- list(
    force_row_wise = prop_boolean(
      FALSE,
      description = "Force row-wise histogram building. Cheaper in memory than column-wise, slower per iteration."
    ),
    histogram_pool_size = prop_float(
      -1,
      description = "Memory cap in MB for the histogram cache; -1 leaves it uncapped."
    ),
    top_k = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      applies_when = list(tree_learner = "voting"),
      description = "Features each machine votes for in the voting tree learner. NULL = LightGBM's default of 20."
    )
  )
  if (!top_k) {
    props[["top_k"]] <- NULL
  }
  prop_group(props, "execution")
} # /rtemis::lightgbm_execution_props


# %% lightgbm_bagging_props ----
#' Bagging hyperparameters beside the fraction and frequency
#'
#' @description
#' Per-class bagging rates and the bagging seed.
#'
#' @details
#' For the wrappers that bag: a LightCART fit is one tree on all of the data.
#' The per-class rates are classification-only in the backend, which no property
#' spec can gate on -- the outcome decides it, not a sibling property -- so they
#' are documented rather than gated.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_bagging_props <- function() {
  prop_group(
    list(
      pos_bagging_fraction = prop_float(
        1,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of positive cases sampled per tree, for a binary outcome."
      ),
      neg_bagging_fraction = prop_float(
        1,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of negative cases sampled per tree, for a binary outcome."
      ),
      bagging_seed = prop_integer(
        3L,
        description = "Random seed for bagging."
      )
    ),
    "bagging"
  )
} # /rtemis::lightgbm_bagging_props


# %% lightgbm_feature_sampling_props ----
#' Feature-sampling hyperparameters beside the fraction
#'
#' @description
#' Sampling features per node rather than per tree, and the seed that drives it.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_feature_sampling_props <- function() {
  prop_group(
    list(
      feature_fraction_bynode = prop_float(
        1,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of features sampled at each node, on top of the per-tree fraction."
      ),
      feature_fraction_seed = prop_integer(
        2L,
        description = "Random seed for feature sampling."
      )
    ),
    "feature_sampling"
  )
} # /rtemis::lightgbm_feature_sampling_props


# %% lightgbm_cegb_props ----
#' Cost-efficient gradient boosting hyperparameters
#'
#' @description
#' Trade split gain against a per-split and per-feature cost, so a model that
#' must be cheap to evaluate can be fitted directly rather than pruned after.
#'
#' @details
#' Works in every boosting mode and in a single tree -- both mechanisms live in
#' split finding, which each of them runs. **The penalty scale is not comparable
#' across modes.** Under `gbdt` the trees after the first fit shrunken residuals,
#' so split gains are small and a fixed penalty bites early; under `rf` every
#' tree fits the full-scale outcome and the same penalty does much less. A value
#' tuned on one does not transfer to the other.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_cegb_props <- function() {
  prop_group(
    list(
      cegb_tradeoff = prop_float(
        1,
        min = 0,
        tunable = TRUE,
        description = "Overall multiplier on every cost-efficiency penalty below."
      ),
      cegb_penalty_split = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "Cost charged per split, in units of loss reduction."
      ),
      cegb_penalty_feature_lazy = prop_array(
        prop_float(0, min = 0, vector = TRUE, data_bound = "n_features"),
        nullable = TRUE,
        broadcast = TRUE,
        description = "Per-feature cost charged the first time a case uses it, one value per feature."
      ),
      cegb_penalty_feature_coupled = prop_array(
        prop_float(0, min = 0, vector = TRUE, data_bound = "n_features"),
        nullable = TRUE,
        broadcast = TRUE,
        description = "Per-feature cost charged the first time any case uses it, one value per feature."
      )
    ),
    "cegb"
  )
} # /rtemis::lightgbm_cegb_props


# %% lightgbm_quantized_props ----
#' Quantized-gradient training hyperparameters
#'
#' @description
#' Train on gradients discretized into a few bins, which is faster and does
#' change the fit.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_quantized_props <- function() {
  quantized <- list(use_quantized_grad = TRUE)
  prop_group(
    list(
      use_quantized_grad = prop_boolean(
        FALSE,
        description = "Discretize gradients before finding splits. Faster, and changes the fit."
      ),
      num_grad_quant_bins = prop_integer(
        NULL,
        min = 2L,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = quantized,
        description = "Bins the gradients are discretized into. NULL = LightGBM's default of 4."
      ),
      quant_train_renew_leaf = prop_boolean(
        NULL,
        nullable = TRUE,
        applies_when = quantized,
        description = "Recompute leaf values from the exact gradients after each iteration. NULL = LightGBM's default of FALSE."
      ),
      stochastic_rounding = prop_boolean(
        NULL,
        nullable = TRUE,
        applies_when = quantized,
        description = "Round gradients stochastically rather than to nearest. NULL = LightGBM's default of TRUE."
      )
    ),
    "quantized"
  )
} # /rtemis::lightgbm_quantized_props


# %% lightgbm_constraint_props ----
#' Structural constraint hyperparameters
#'
#' @description
#' Constraints on what the trees may express: monotone relationships,
#' permissible feature interactions, and per-feature split weighting.
#'
#' @details
#' Three of these are per-feature and carry `data_bound = "n_features"`, so a
#' form builder knows their length is decided by the data rather than by the
#' config. `interaction_constraints` is LightGBM's own bracketed-groups string,
#' passed through as written -- the alternative would be a nested container whose
#' shape rtemis would have to keep in step with the backend's parser.
#'
#' rtemis ships MonotonicHAL as a whole algorithm for monotonicity; these are
#' LightGBM's own constraints, and the two are unrelated implementations of the
#' same idea.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_constraint_props <- function() {
  prop_group(
    list(
      monotone_constraints = prop_integer(
        NULL,
        min = -1L,
        max = 1L,
        nullable = TRUE,
        vector = TRUE,
        data_bound = "n_features",
        description = "Per-feature monotonicity: 1 increasing, -1 decreasing, 0 unconstrained."
      ),
      monotone_constraints_method = prop_string(
        NULL,
        enum = c("basic", "intermediate", "advanced"),
        nullable = TRUE,
        applies_when = list(monotone_constraints = c(-1L, 1L)),
        description = "How monotonicity is enforced: 'basic' is fastest, 'advanced' least restrictive."
      ),
      monotone_penalty = prop_float(
        NULL,
        min = 0,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = list(monotone_constraints = c(-1L, 1L)),
        description = "Depth penalty on splits that would break monotonicity. NULL = LightGBM's default of 0."
      ),
      interaction_constraints = prop_string(
        NULL,
        nullable = TRUE,
        description = "Feature groups permitted to interact, in LightGBM's own bracketed form, e.g. \"[0,1],[2,3]\"."
      ),
      feature_contri = prop_float(
        NULL,
        nullable = TRUE,
        vector = TRUE,
        data_bound = "n_features",
        description = "Per-feature multiplier on split gain, one value per feature."
      )
    ),
    "constraints"
  )
} # /rtemis::lightgbm_constraint_props


# %% lightgbm_boosting_props ----
#' Boosting mode and sampling strategy
#'
#' @description
#' Which algorithm LightGBM runs, and how it subsamples cases.
#'
#' @details
#' `boosting` selects gradient boosting, a random forest, or DART. GOSS was a
#' `boosting` value before LightGBM 4.0 and is now `data_sample_strategy`; only
#' the current spelling is declared, so one decision has one switch.
#'
#' Not for LightCART, which fits one tree, nor for LightRF, which pins
#' `boosting` to `"rf"` -- that constant is what makes it a random forest.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_boosting_props <- function() {
  prop_group(
    list(
      boosting = prop_string(
        "gbdt",
        enum = c("gbdt", "rf", "dart"),
        description = "Boosting algorithm: gradient boosting, random forest, or DART (dropout)."
      ),
      data_sample_strategy = prop_string(
        "bagging",
        enum = c("bagging", "goss"),
        description = "How cases are subsampled: at random, or by gradient magnitude (GOSS)."
      )
    ),
    "boosting"
  )
} # /rtemis::lightgbm_boosting_props


# %% lightgbm_dart_props ----
#' DART dropout hyperparameters
#'
#' @description
#' The dropout settings DART adds to gradient boosting.
#'
#' @details
#' Gated on `boosting = "dart"`, so setting one without selecting DART is an
#' error rather than a value LightGBM ignores. Every one is unset by default,
#' meaning LightGBM's own default.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_dart_props <- function() {
  dart <- list(boosting = "dart")
  prop_group(
    list(
      drop_rate = prop_float(
        NULL,
        min = 0,
        max = 1,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = dart,
        description = "Fraction of existing trees to drop per iteration. NULL = LightGBM's default of 0.1."
      ),
      max_drop = prop_integer(
        NULL,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = dart,
        description = "Most trees dropped in one iteration; 0 or less removes the cap. NULL = LightGBM's default of 50."
      ),
      skip_drop = prop_float(
        NULL,
        min = 0,
        max = 1,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = dart,
        description = "Probability of skipping dropout for an iteration. NULL = LightGBM's default of 0.5."
      ),
      uniform_drop = prop_boolean(
        NULL,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = dart,
        description = "Drop trees uniformly rather than weighted. NULL = LightGBM's default of FALSE."
      ),
      xgboost_dart_mode = prop_boolean(
        NULL,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = dart,
        description = "Use the XGBoost DART normalization. NULL = LightGBM's default of FALSE."
      ),
      drop_seed = prop_integer(
        NULL,
        nullable = TRUE,
        applies_when = dart,
        description = "Random seed for choosing which trees to drop. NULL = LightGBM's default of 4."
      )
    ),
    "dart"
  )
} # /rtemis::lightgbm_dart_props


# %% lightgbm_goss_props ----
#' GOSS sampling hyperparameters
#'
#' @description
#' The retain ratios Gradient-based One-Side Sampling keeps.
#'
#' @details
#' Gated on `data_sample_strategy = "goss"`. GOSS keeps every case with a large
#' gradient and samples the rest, so the two rates are what it retains from each
#' end; LightGBM requires their sum not to exceed 1, which the class validator
#' checks rather than leaving to the backend.
#'
#' @return Named list of S7 properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_goss_props <- function() {
  goss <- list(data_sample_strategy = "goss")
  prop_group(
    list(
      top_rate = prop_float(
        NULL,
        min = 0,
        max = 1,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = goss,
        description = "Share of large-gradient cases retained. NULL = LightGBM's default of 0.2."
      ),
      other_rate = prop_float(
        NULL,
        min = 0,
        max = 1,
        nullable = TRUE,
        tunable = TRUE,
        applies_when = goss,
        description = "Share of the remaining cases retained. NULL = LightGBM's default of 0.1."
      )
    ),
    "goss"
  )
} # /rtemis::lightgbm_goss_props


# %% check_lightgbm_sampling ----
#' Cross-parameter rules the property specs cannot carry
#'
#' Two rules LightGBM enforces mid-fit, checked here so they are reported
#' against the setting the user wrote rather than as a backend abort:
#'
#' - GOSS cannot be combined with bagging. `bagging_fraction = 1` is not bagging,
#'   which is why the rtemis default is compatible; anything below it is.
#' - GOSS's two retain ratios cannot sum above 1, since together they are a share
#'   of the training cases.
#'
#' @param self `Hyperparameters` object.
#'
#' @return Character message, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_lightgbm_sampling <- function(self) {
  # A tunable property may hold a *domain* rather than a value, and each grid
  # cell is validated on its way in -- so a cell that breaks either rule is
  # already refused with the message below. What a domain needs deciding here is
  # whether *any* cell could satisfy the rule at all: a search where none can is
  # a run that fails every cell, and it should say so now rather than as "all N
  # tuning grid cells failed".
  #
  # `check_applies_when()` reads a gated domain the same way -- the gate opens
  # when any candidate value satisfies it -- so a search space is rejected only
  # when it is hopeless, never when part of it is workable.
  if (!any(candidate_values(self@data_sample_strategy) == "goss")) {
    return(NULL)
  }
  fraction <- candidate_values(self@bagging_fraction)
  if (is.numeric(fraction) && !any(fraction >= 1)) {
    return(paste0(
      "@data_sample_strategy \"goss\" cannot be combined with bagging, and no ",
      "value of @bagging_fraction avoids it: ",
      paste(fraction, collapse = ", "),
      ". GOSS samples by gradient instead, so leave @bagging_fraction at 1."
    ))
  }
  # The smallest reachable sum: above 1 there is no combination of the two
  # domains that LightGBM would accept.
  top <- candidate_values(self@top_rate)
  other <- candidate_values(self@other_rate)
  if (
    is.numeric(top) &&
      is.numeric(other) &&
      length(top) > 0L &&
      length(other) > 0L &&
      min(top) + min(other) > 1
  ) {
    return(paste0(
      "@top_rate + @other_rate must not exceed 1: together they are a share of ",
      "the training cases, and the smallest they can sum to here is ",
      min(top) + min(other),
      "."
    ))
  }
  NULL
} # /rtemis::check_lightgbm_sampling


# %% LightCARTHyperparameters ----
#' @title LightCARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightCART.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightCARTHyperparameters <- new_class(
  name = "LightCARTHyperparameters",
  parent = Hyperparameters,
  properties = c(
    list(
      algorithm = prop_algorithm("LightCART"),
      device_type = prop_string(
        "cpu",
        enum = c("cpu", "gpu", "cuda"),
        description = "Compute device."
      ),
      force_col_wise = prop_boolean(
        TRUE,
        description = "Force column-wise histogram building (CPU only)."
      ),
      feature_fraction = prop_float(
        1.0,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of features sampled per tree."
      ),
      num_leaves = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of leaves in one tree."
      ),
      max_depth = prop_integer(
        -1L,
        tunable = TRUE,
        description = "Maximum tree depth. -1 = no limit."
      ),
      lambda_l1 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L1 regularization."
      ),
      lambda_l2 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L2 regularization."
      ),
      max_cat_threshold = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of split points for categorical features."
      ),
      min_data_per_group = prop_integer(
        100L,
        min = 1L,
        tunable = TRUE,
        description = "Minimum number of cases per categorical group."
      ),
      linear_tree = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Fit linear models at leaves."
      ),
      objective = prop_string(
        NULL,
        nullable = TRUE,
        default_on_null = TRUE,
        description = "LightGBM objective. NULL = set from outcome type."
      ),
      ifw = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Inverse Frequency Weighting in classification."
      )
    ),
    lightgbm_objective_props(),
    lightgbm_regularization_props(),
    lightgbm_binning_props(),
    lightgbm_categorical_props(),
    lightgbm_determinism_props(),
    lightgbm_execution_props(top_k = FALSE),
    lightgbm_feature_sampling_props(),
    lightgbm_cegb_props(),
    lightgbm_quantized_props(),
    lightgbm_constraint_props()
  ),
  # Reads each property's `applies_when` spec, which is what gates the
  # objective-specific parameters on `objective`. Spec-driven, so it declares
  # no rule the schema does not already publish.
  validator = function(self) {
    check_applies_when(self)
  }
) # /rtemis::LightCARTHyperparameters


# %% setup_LightCART ----
#' Setup LightCART Hyperparameters
#'
#' Setup hyperparameters for LightCART training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param num_leaves (Tunable) Integer [1, Inf): Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees. -1 = no limit.
#' @param lambda_l1 (Tunable) Numeric [0, Inf): L1 regularization.
#' @param lambda_l2 (Tunable) Numeric [0, Inf): L2 regularization.
#' @param max_cat_threshold (Tunable) Integer [1, Inf): Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Integer [1, Inf): Minimum number of observations per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
#' @param device_type Character \{"cpu", "gpu", "cuda"\}: Compute device.
#' @param force_col_wise Logical: Force column-wise histogram building (CPU only).
#' @param feature_fraction (Tunable) Numeric (0, 1]: Fraction of features sampled per tree.
#' @param feature_fraction_bynode (Tunable) Numeric (0, 1]: Fraction of features sampled
#' at each node, on top of the per-tree fraction.
#' @param feature_fraction_seed Integer: Random seed for feature sampling.
#' @param deterministic Logical: Force reproducible results at some cost in speed.
#' @param objective_seed Integer: Random seed for the objective, where it draws.
#' @param force_row_wise Logical: Force row-wise histogram building. Cheaper in memory
#' than column-wise, slower per iteration.
#' @param histogram_pool_size Numeric: Memory cap in MB for the histogram cache; -1
#' leaves it uncapped.
#' @param cegb_tradeoff (Tunable) Numeric [0, Inf): Overall multiplier on every
#' cost-efficiency penalty. Its scale is not comparable between boosting modes.
#' @param cegb_penalty_split (Tunable) Numeric [0, Inf): Cost charged per split, in units
#' of loss reduction.
#' @param cegb_penalty_feature_lazy Optional List: Per-feature cost charged the first
#' time a case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param cegb_penalty_feature_coupled Optional List: Per-feature cost charged the first
#' time any case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param use_quantized_grad Logical: Discretize gradients before finding splits. Faster,
#' and changes the fit.
#' @param num_grad_quant_bins (Tunable) Optional Integer [2, Inf): Bins the gradients are
#' discretized into. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of 4.
#' @param quant_train_renew_leaf Optional Logical: Recompute leaf values from the exact
#' gradients after each iteration. Requires `use_quantized_grad = TRUE`. NULL uses
#' LightGBM's default of FALSE.
#' @param stochastic_rounding Optional Logical: Round gradients stochastically rather than
#' to nearest. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of TRUE.
#' @param monotone_constraints Optional Integer \[-1, 1\] vector: Per-feature
#' monotonicity: 1 increasing, -1 decreasing, 0 unconstrained. One value per feature.
#' @param monotone_constraints_method Optional Character \{"basic", "intermediate",
#' "advanced"\}: How monotonicity is enforced. Requires `monotone_constraints`.
#' @param monotone_penalty (Tunable) Optional Numeric [0, Inf): Depth penalty on splits
#' that would break monotonicity. Requires `monotone_constraints`. NULL uses LightGBM's
#' default of 0.
#' @param interaction_constraints Optional Character: Feature groups permitted to interact,
#' in LightGBM's own bracketed form, e.g. "\[0,1\],\[2,3\]".
#' @param feature_contri Optional Numeric vector: Per-feature multiplier on split gain,
#' one value per feature.
#' @param min_data_in_leaf (Tunable) Integer [0, Inf): Minimum number of cases in a leaf.
#' @param min_sum_hessian_in_leaf (Tunable) Numeric [0, Inf): Minimum sum of hessians in a
#' leaf: the smooth counterpart of a case count.
#' @param min_gain_to_split (Tunable) Numeric [0, Inf): Minimum loss reduction a split must
#' buy to be made.
#' @param max_delta_step (Tunable) Numeric: Cap on each leaf's output; 0 or less leaves it
#' uncapped.
#' @param path_smooth (Tunable) Numeric [0, Inf): Smooth each leaf towards its parent, more
#' strongly the fewer cases it holds.
#' @param extra_trees (Tunable) Logical: Choose one split threshold per feature at random
#' rather than the best.
#' @param extra_seed Integer: Random seed for the thresholds `extra_trees` draws.
#' @param linear_lambda (Tunable) Optional Numeric [0, Inf): L2 penalty on the linear models
#' fitted at the leaves. Requires `linear_tree = TRUE`. NULL uses LightGBM's default of 0.
#' @param max_bin (Tunable) Integer [2, Inf): Most bins a feature is discretized into. Fewer
#' is faster and more regularized.
#' @param min_data_in_bin (Tunable) Integer [1, Inf): Minimum number of cases per bin.
#' @param use_missing Logical: Handle missing values. FALSE ignores them.
#' @param zero_as_missing Logical: Treat zeros as missing, rather than only NA.
#' @param cat_l2 (Tunable) Numeric [0, Inf): L2 regularization on a categorical split.
#' @param cat_smooth (Tunable) Numeric [0, Inf): Smoothing over categorical levels, which
#' reduces the effect of rare ones.
#' @param max_cat_to_onehot (Tunable) Integer [1, Inf): Below this many levels, split
#' one-versus-rest rather than by partition.
#' @param alpha (Tunable) Optional Numeric (0, Inf): Huber delta or quantile level.
#' Requires `objective` to be "huber" or "quantile". NULL uses LightGBM's default of
#' 0.9.
#' @param tweedie_variance_power (Tunable) Optional Numeric [1, 2): Tweedie power: 1 is
#' Poisson-like, near 2 is gamma-like. Requires `objective = "tweedie"`. NULL uses
#' LightGBM's default of 1.5.
#' @param fair_c (Tunable) Optional Numeric (0, Inf): Fair loss scale. Requires
#' `objective = "fair"`. NULL uses LightGBM's default of 1.
#' @param poisson_max_delta_step (Tunable) Optional Numeric (0, Inf): Step cap
#' safeguarding Poisson optimization. Requires `objective = "poisson"`. NULL uses
#' LightGBM's default of 0.7.
#' @param sigmoid (Tunable) Optional Numeric (0, Inf): Sigmoid slope, for a binary or
#' one-vs-all objective. NULL uses LightGBM's default of 1.
#' @param boost_from_average Optional Logical: Start from the outcome's mean, for a
#' regression, binary or cross-entropy objective. NULL uses LightGBM's default of TRUE.
#' @param reg_sqrt Optional Logical: Fit the square root of the outcome and square the
#' prediction back, for a regression objective. NULL uses LightGBM's default of FALSE.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return LightCARTHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' lightcart_hyperparams <- setup_LightCART(num_leaves = 32L, ifw = FALSE)
#' lightcart_hyperparams
setup_LightCART <- function(
  num_leaves = 32L,
  max_depth = -1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 100L,
  linear_tree = FALSE,
  objective = NULL,
  device_type = "cpu",
  force_col_wise = TRUE,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1,
  feature_fraction_seed = 2L,
  deterministic = FALSE,
  objective_seed = 5L,
  force_row_wise = FALSE,
  histogram_pool_size = -1,
  cegb_tradeoff = 1,
  cegb_penalty_split = 0,
  cegb_penalty_feature_lazy = NULL,
  cegb_penalty_feature_coupled = NULL,
  use_quantized_grad = FALSE,
  num_grad_quant_bins = NULL,
  quant_train_renew_leaf = NULL,
  stochastic_rounding = NULL,
  monotone_constraints = NULL,
  monotone_constraints_method = NULL,
  monotone_penalty = NULL,
  interaction_constraints = NULL,
  feature_contri = NULL,
  min_data_in_leaf = 20L,
  min_sum_hessian_in_leaf = 1e-3,
  min_gain_to_split = 0,
  max_delta_step = 0,
  path_smooth = 0,
  extra_trees = FALSE,
  extra_seed = 6L,
  linear_lambda = NULL,
  max_bin = 255L,
  min_data_in_bin = 3L,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  cat_l2 = 10,
  cat_smooth = 10,
  max_cat_to_onehot = 4L,
  alpha = NULL,
  tweedie_variance_power = NULL,
  fair_c = NULL,
  poisson_max_delta_step = NULL,
  sigmoid = NULL,
  boost_from_average = NULL,
  reg_sqrt = NULL,
  ifw = FALSE
) {
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  min_data_in_leaf <- clean_posint(min_data_in_leaf)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  LightCARTHyperparameters(
    num_leaves = num_leaves,
    max_depth = max_depth,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    objective = objective,
    device_type = device_type,
    force_col_wise = force_col_wise,
    feature_fraction = feature_fraction,
    feature_fraction_bynode = feature_fraction_bynode,
    feature_fraction_seed = feature_fraction_seed,
    deterministic = deterministic,
    objective_seed = objective_seed,
    force_row_wise = force_row_wise,
    histogram_pool_size = histogram_pool_size,
    cegb_tradeoff = cegb_tradeoff,
    cegb_penalty_split = cegb_penalty_split,
    cegb_penalty_feature_lazy = cegb_penalty_feature_lazy,
    cegb_penalty_feature_coupled = cegb_penalty_feature_coupled,
    use_quantized_grad = use_quantized_grad,
    num_grad_quant_bins = num_grad_quant_bins,
    quant_train_renew_leaf = quant_train_renew_leaf,
    stochastic_rounding = stochastic_rounding,
    monotone_constraints = monotone_constraints,
    monotone_constraints_method = monotone_constraints_method,
    monotone_penalty = monotone_penalty,
    interaction_constraints = interaction_constraints,
    feature_contri = feature_contri,
    min_data_in_leaf = min_data_in_leaf,
    min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
    min_gain_to_split = min_gain_to_split,
    max_delta_step = max_delta_step,
    path_smooth = path_smooth,
    extra_trees = extra_trees,
    extra_seed = extra_seed,
    linear_lambda = linear_lambda,
    max_bin = max_bin,
    min_data_in_bin = min_data_in_bin,
    use_missing = use_missing,
    zero_as_missing = zero_as_missing,
    cat_l2 = cat_l2,
    cat_smooth = cat_smooth,
    max_cat_to_onehot = max_cat_to_onehot,
    alpha = alpha,
    tweedie_variance_power = tweedie_variance_power,
    fair_c = fair_c,
    poisson_max_delta_step = poisson_max_delta_step,
    sigmoid = sigmoid,
    boost_from_average = boost_from_average,
    reg_sqrt = reg_sqrt,
    ifw = ifw
  )
} # /rtemis::setup_LightCART


# %% LightRFHyperparameters ----
#' @title LightRFHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightRF (LightGBM random forest mode).
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightRFHyperparameters <- new_class(
  name = "LightRFHyperparameters",
  parent = Hyperparameters,
  properties = c(
    list(
      algorithm = prop_algorithm("LightRF"),
      # Constants: these four are what make lightgbm train a random forest, so
      # they belong to the class rather than to the user.
      boosting = prop_const(
        "rf",
        description = "Boosting type. 'rf' is what makes LightGBM a random forest."
      ),
      learning_rate = prop_const(
        1,
        description = "Learning rate. No effect in 'rf' mode; set for clarity."
      ),
      bagging_freq = prop_const(
        1L,
        description = "Bagging frequency."
      ),
      early_stopping_rounds = prop_const(
        -1L,
        description = "Early stopping rounds. -1 disables early stopping."
      ),
      nrounds = prop_integer(
        500L,
        min = 1L,
        tunable = TRUE,
        description = "Number of boosting rounds (trees)."
      ),
      num_leaves = prop_integer(
        4096L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of leaves per tree."
      ),
      max_depth = prop_integer(
        -1L,
        tunable = TRUE,
        description = "Maximum tree depth. -1 = no limit."
      ),
      feature_fraction = prop_float(
        NULL,
        exclusive_min = 0,
        max = 1,
        nullable = TRUE,
        tunable = TRUE,
        description = "Fraction of features sampled per tree. NULL = sqrt(n_features)/n_features for classification, 0.33 for regression."
      ),
      bagging_fraction = prop_float(
        0.623,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of cases sampled per tree (bagging fraction)."
      ),
      lambda_l1 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L1 regularization."
      ),
      lambda_l2 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L2 regularization."
      ),
      max_cat_threshold = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of split points for categorical features."
      ),
      min_data_per_group = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Minimum number of cases per categorical group."
      ),
      linear_tree = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Fit linear models at leaves."
      ),
      ifw = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Inverse frequency weighting of outcome classes."
      ),
      objective = prop_string(
        NULL,
        nullable = TRUE,
        default_on_null = TRUE,
        description = "LightGBM objective. NULL = set from outcome type."
      ),
      device_type = prop_string(
        "cpu",
        enum = c("cpu", "gpu", "cuda"),
        description = "Compute device."
      ),
      tree_learner = prop_string(
        "serial",
        enum = c("serial", "feature", "data", "voting"),
        description = "Tree learner type."
      ),
      force_col_wise = prop_boolean(
        TRUE,
        description = "Force column-wise histogram building (CPU only)."
      )
    ),
    lightgbm_objective_props(),
    lightgbm_regularization_props(),
    lightgbm_binning_props(),
    lightgbm_categorical_props(),
    lightgbm_determinism_props(),
    lightgbm_execution_props(),
    lightgbm_bagging_props(),
    lightgbm_feature_sampling_props(),
    lightgbm_cegb_props(),
    lightgbm_quantized_props(),
    lightgbm_constraint_props(),
    lightgbm_early_stopping_props()
  ),
  # Reads each property's `applies_when` spec, which is what gates the
  # objective-specific parameters on `objective`. Spec-driven, so it declares
  # no rule the schema does not already publish.
  validator = function(self) {
    check_applies_when(self)
  }
) # /rtemis::LightRFHyperparameters

# %% setup_LightRF ----
#' Setup LightRF Hyperparameters
#'
#' Setup hyperparameters for LightRF training.
#'
#' Get more information from [lightgbm::lgb.train].
#' Note that `boosting`, `learning_rate`, `bagging_freq` and
#' `early_stopping_rounds` are *constants* here: they cannot be set, because
#' they are what makes `lightgbm` train a random forest. All of them are
#' settable when training gradient boosting with LightGBM.
#'
#' @param nrounds (Tunable) Integer [1, Inf): Number of boosting rounds.
#' @param num_leaves (Tunable) Integer [1, Inf): Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees. -1 = no limit.
#' @param feature_fraction (Tunable) Optional Numeric (0, 1]: Fraction of features to use. NULL derives it from the data: sqrt(n_features)/n_features for classification, 0.33 for regression.
#' @param bagging_fraction (Tunable) Numeric (0, 1]: Fraction of cases sampled per tree.
#' @param lambda_l1 (Tunable) Numeric [0, Inf): L1 regularization.
#' @param lambda_l2 (Tunable) Numeric [0, Inf): L2 regularization.
#' @param max_cat_threshold (Tunable) Integer [1, Inf): Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Integer [1, Inf): Minimum number of observations per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
#' @param early_stopping_min_delta (Tunable) Numeric [0, Inf): Smallest improvement that
#' resets the early-stopping counter.
#' @param deterministic Logical: Force reproducible results at some cost in speed.
#' @param objective_seed Integer: Random seed for the objective, where it draws.
#' @param force_row_wise Logical: Force row-wise histogram building. Cheaper in memory
#' than column-wise, slower per iteration.
#' @param histogram_pool_size Numeric: Memory cap in MB for the histogram cache; -1
#' leaves it uncapped.
#' @param top_k (Tunable) Optional Integer [1, Inf): Features each machine votes for in
#' the voting tree learner. Requires `tree_learner = "voting"`. NULL uses LightGBM's
#' default of 20.
#' @param pos_bagging_fraction (Tunable) Numeric (0, 1]: Fraction of positive cases
#' sampled per tree, for a binary outcome.
#' @param neg_bagging_fraction (Tunable) Numeric (0, 1]: Fraction of negative cases
#' sampled per tree, for a binary outcome.
#' @param bagging_seed Integer: Random seed for bagging.
#' @param feature_fraction_bynode (Tunable) Numeric (0, 1]: Fraction of features sampled
#' at each node, on top of the per-tree fraction.
#' @param feature_fraction_seed Integer: Random seed for feature sampling.
#' @param cegb_tradeoff (Tunable) Numeric [0, Inf): Overall multiplier on every
#' cost-efficiency penalty. Its scale is not comparable between boosting modes.
#' @param cegb_penalty_split (Tunable) Numeric [0, Inf): Cost charged per split, in units
#' of loss reduction.
#' @param cegb_penalty_feature_lazy Optional List: Per-feature cost charged the first
#' time a case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param cegb_penalty_feature_coupled Optional List: Per-feature cost charged the first
#' time any case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param use_quantized_grad Logical: Discretize gradients before finding splits. Faster,
#' and changes the fit.
#' @param num_grad_quant_bins (Tunable) Optional Integer [2, Inf): Bins the gradients are
#' discretized into. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of 4.
#' @param quant_train_renew_leaf Optional Logical: Recompute leaf values from the exact
#' gradients after each iteration. Requires `use_quantized_grad = TRUE`. NULL uses
#' LightGBM's default of FALSE.
#' @param stochastic_rounding Optional Logical: Round gradients stochastically rather than
#' to nearest. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of TRUE.
#' @param monotone_constraints Optional Integer \[-1, 1\] vector: Per-feature
#' monotonicity: 1 increasing, -1 decreasing, 0 unconstrained. One value per feature.
#' @param monotone_constraints_method Optional Character \{"basic", "intermediate",
#' "advanced"\}: How monotonicity is enforced. Requires `monotone_constraints`.
#' @param monotone_penalty (Tunable) Optional Numeric [0, Inf): Depth penalty on splits
#' that would break monotonicity. Requires `monotone_constraints`. NULL uses LightGBM's
#' default of 0.
#' @param interaction_constraints Optional Character: Feature groups permitted to interact,
#' in LightGBM's own bracketed form, e.g. "\[0,1\],\[2,3\]".
#' @param feature_contri Optional Numeric vector: Per-feature multiplier on split gain,
#' one value per feature.
#' @param min_data_in_leaf (Tunable) Integer [0, Inf): Minimum number of cases in a leaf.
#' @param min_sum_hessian_in_leaf (Tunable) Numeric [0, Inf): Minimum sum of hessians in a
#' leaf: the smooth counterpart of a case count.
#' @param min_gain_to_split (Tunable) Numeric [0, Inf): Minimum loss reduction a split must
#' buy to be made.
#' @param max_delta_step (Tunable) Numeric: Cap on each leaf's output; 0 or less leaves it
#' uncapped.
#' @param path_smooth (Tunable) Numeric [0, Inf): Smooth each leaf towards its parent, more
#' strongly the fewer cases it holds.
#' @param extra_trees (Tunable) Logical: Choose one split threshold per feature at random
#' rather than the best.
#' @param extra_seed Integer: Random seed for the thresholds `extra_trees` draws.
#' @param linear_lambda (Tunable) Optional Numeric [0, Inf): L2 penalty on the linear models
#' fitted at the leaves. Requires `linear_tree = TRUE`. NULL uses LightGBM's default of 0.
#' @param max_bin (Tunable) Integer [2, Inf): Most bins a feature is discretized into. Fewer
#' is faster and more regularized.
#' @param min_data_in_bin (Tunable) Integer [1, Inf): Minimum number of cases per bin.
#' @param use_missing Logical: Handle missing values. FALSE ignores them.
#' @param zero_as_missing Logical: Treat zeros as missing, rather than only NA.
#' @param cat_l2 (Tunable) Numeric [0, Inf): L2 regularization on a categorical split.
#' @param cat_smooth (Tunable) Numeric [0, Inf): Smoothing over categorical levels, which
#' reduces the effect of rare ones.
#' @param max_cat_to_onehot (Tunable) Integer [1, Inf): Below this many levels, split
#' one-versus-rest rather than by partition.
#' @param alpha (Tunable) Optional Numeric (0, Inf): Huber delta or quantile level.
#' Requires `objective` to be "huber" or "quantile". NULL uses LightGBM's default of
#' 0.9.
#' @param tweedie_variance_power (Tunable) Optional Numeric [1, 2): Tweedie power: 1 is
#' Poisson-like, near 2 is gamma-like. Requires `objective = "tweedie"`. NULL uses
#' LightGBM's default of 1.5.
#' @param fair_c (Tunable) Optional Numeric (0, Inf): Fair loss scale. Requires
#' `objective = "fair"`. NULL uses LightGBM's default of 1.
#' @param poisson_max_delta_step (Tunable) Optional Numeric (0, Inf): Step cap
#' safeguarding Poisson optimization. Requires `objective = "poisson"`. NULL uses
#' LightGBM's default of 0.7.
#' @param sigmoid (Tunable) Optional Numeric (0, Inf): Sigmoid slope, for a binary or
#' one-vs-all objective. NULL uses LightGBM's default of 1.
#' @param boost_from_average Optional Logical: Start from the outcome's mean, for a
#' regression, binary or cross-entropy objective. NULL uses LightGBM's default of TRUE.
#' @param reg_sqrt Optional Logical: Fit the square root of the outcome and square the
#' prediction back, for a regression objective. NULL uses LightGBM's default of FALSE.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#' @param device_type Character \{"cpu", "gpu", "cuda"\}: Compute device.
#' @param tree_learner Character \{"serial", "feature", "data", "voting"\}: Tree learner type.
#' @param force_col_wise Logical: Use only with CPU - If TRUE, force col-wise histogram building.
#'
#' @return LightRFHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' lightrf_hyperparams <- setup_LightRF(nrounds = 1000L, ifw = FALSE)
#' lightrf_hyperparams
setup_LightRF <- function(
  nrounds = 500L,
  num_leaves = 4096L,
  max_depth = -1L,
  feature_fraction = NULL,
  bagging_fraction = .623,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  ifw = FALSE,
  # fixed
  objective = NULL,
  early_stopping_min_delta = 0,
  deterministic = FALSE,
  objective_seed = 5L,
  force_row_wise = FALSE,
  histogram_pool_size = -1,
  top_k = NULL,
  pos_bagging_fraction = 1,
  neg_bagging_fraction = 1,
  bagging_seed = 3L,
  feature_fraction_bynode = 1,
  feature_fraction_seed = 2L,
  cegb_tradeoff = 1,
  cegb_penalty_split = 0,
  cegb_penalty_feature_lazy = NULL,
  cegb_penalty_feature_coupled = NULL,
  use_quantized_grad = FALSE,
  num_grad_quant_bins = NULL,
  quant_train_renew_leaf = NULL,
  stochastic_rounding = NULL,
  monotone_constraints = NULL,
  monotone_constraints_method = NULL,
  monotone_penalty = NULL,
  interaction_constraints = NULL,
  feature_contri = NULL,
  min_data_in_leaf = 20L,
  min_sum_hessian_in_leaf = 1e-3,
  min_gain_to_split = 0,
  max_delta_step = 0,
  path_smooth = 0,
  extra_trees = FALSE,
  extra_seed = 6L,
  linear_lambda = NULL,
  max_bin = 255L,
  min_data_in_bin = 3L,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  cat_l2 = 10,
  cat_smooth = 10,
  max_cat_to_onehot = 4L,
  alpha = NULL,
  tweedie_variance_power = NULL,
  fair_c = NULL,
  poisson_max_delta_step = NULL,
  sigmoid = NULL,
  boost_from_average = NULL,
  reg_sqrt = NULL,
  device_type = "cpu",
  tree_learner = "serial",
  force_col_wise = TRUE
) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  LightRFHyperparameters(
    nrounds = nrounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    feature_fraction = feature_fraction,
    bagging_fraction = bagging_fraction,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective,
    early_stopping_min_delta = early_stopping_min_delta,
    deterministic = deterministic,
    objective_seed = objective_seed,
    force_row_wise = force_row_wise,
    histogram_pool_size = histogram_pool_size,
    top_k = top_k,
    pos_bagging_fraction = pos_bagging_fraction,
    neg_bagging_fraction = neg_bagging_fraction,
    bagging_seed = bagging_seed,
    feature_fraction_bynode = feature_fraction_bynode,
    feature_fraction_seed = feature_fraction_seed,
    cegb_tradeoff = cegb_tradeoff,
    cegb_penalty_split = cegb_penalty_split,
    cegb_penalty_feature_lazy = cegb_penalty_feature_lazy,
    cegb_penalty_feature_coupled = cegb_penalty_feature_coupled,
    use_quantized_grad = use_quantized_grad,
    num_grad_quant_bins = num_grad_quant_bins,
    quant_train_renew_leaf = quant_train_renew_leaf,
    stochastic_rounding = stochastic_rounding,
    monotone_constraints = monotone_constraints,
    monotone_constraints_method = monotone_constraints_method,
    monotone_penalty = monotone_penalty,
    interaction_constraints = interaction_constraints,
    feature_contri = feature_contri,
    min_data_in_leaf = min_data_in_leaf,
    min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
    min_gain_to_split = min_gain_to_split,
    max_delta_step = max_delta_step,
    path_smooth = path_smooth,
    extra_trees = extra_trees,
    extra_seed = extra_seed,
    linear_lambda = linear_lambda,
    max_bin = max_bin,
    min_data_in_bin = min_data_in_bin,
    use_missing = use_missing,
    zero_as_missing = zero_as_missing,
    cat_l2 = cat_l2,
    cat_smooth = cat_smooth,
    max_cat_to_onehot = max_cat_to_onehot,
    alpha = alpha,
    tweedie_variance_power = tweedie_variance_power,
    fair_c = fair_c,
    poisson_max_delta_step = poisson_max_delta_step,
    sigmoid = sigmoid,
    boost_from_average = boost_from_average,
    reg_sqrt = reg_sqrt,
    device_type = device_type,
    tree_learner = tree_learner,
    force_col_wise = force_col_wise
  )
} # /rtemis::setup_LightRF


# %% LightGBMHyperparameters ----
#' @title LightGBMHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightGBM. `nrounds` is derived (from
#' `force_nrounds`, or by early-stopping during tuning) and `best_iter` is
#' runtime state written by the Tuner -- exclude both from schema generation.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightGBMHyperparameters <- new_class(
  name = "LightGBMHyperparameters",
  parent = Hyperparameters,
  properties = c(
    list(
      algorithm = prop_algorithm("LightGBM"),
      max_nrounds = prop_integer(
        1000L,
        min = 1L,
        description = "Maximum number of boosting rounds when tuning nrounds by early stopping."
      ),
      force_nrounds = prop_integer(
        NULL,
        min = 1L,
        nullable = TRUE,
        description = "Use this many boosting rounds; disables the search for nrounds."
      ),
      early_stopping_rounds = prop_integer(
        10L,
        min = 1L,
        description = "Number of rounds without improvement to stop training."
      ),
      num_leaves = prop_integer(
        8L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of leaves in one tree."
      ),
      max_depth = prop_integer(
        -1L,
        tunable = TRUE,
        description = "Maximum tree depth. -1 = no limit."
      ),
      learning_rate = prop_float(
        0.01,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Learning rate."
      ),
      feature_fraction = prop_float(
        1.0,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of features sampled per tree."
      ),
      bagging_fraction = prop_float(
        1.0,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of cases sampled per tree (bagging fraction)."
      ),
      bagging_freq = prop_integer(
        1L,
        min = 1L,
        tunable = TRUE,
        description = "Bagging frequency."
      ),
      lambda_l1 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L1 regularization."
      ),
      lambda_l2 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L2 regularization."
      ),
      max_cat_threshold = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of split points for categorical features."
      ),
      min_data_per_group = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Minimum number of cases per categorical group."
      ),
      linear_tree = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Fit linear models at leaves."
      ),
      ifw = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Inverse Frequency Weighting in classification."
      ),
      objective = prop_string(
        NULL,
        nullable = TRUE,
        default_on_null = TRUE,
        description = "LightGBM objective. NULL = set from outcome type."
      ),
      device_type = prop_string(
        "cpu",
        enum = c("cpu", "gpu", "cuda"),
        description = "Compute device."
      ),
      tree_learner = prop_string(
        "serial",
        enum = c("serial", "feature", "data", "voting"),
        description = "Tree learner type."
      ),
      force_col_wise = prop_boolean(
        TRUE,
        description = "Force column-wise histogram building (CPU only)."
      ),
      # Derived: force_nrounds if set, otherwise determined by early stopping
      # during tuning.
      nrounds = prop_state(prop_integer(
        NULL,
        min = 1L,
        nullable = TRUE,
        tune_on_null = TRUE,
        description = "Resolved number of boosting rounds. NULL = determined by early stopping during tuning."
      )),
      # Run state: best iteration, written by the Tuner.
      best_iter = prop_state(prop_float(
        NULL,
        min = 0,
        nullable = TRUE,
        description = "Best iteration found by early stopping."
      ))
    ),
    lightgbm_objective_props(),
    lightgbm_boosting_props(),
    lightgbm_dart_props(),
    lightgbm_goss_props(),
    lightgbm_regularization_props(),
    lightgbm_binning_props(),
    lightgbm_categorical_props(),
    lightgbm_determinism_props(),
    lightgbm_execution_props(),
    lightgbm_bagging_props(),
    lightgbm_feature_sampling_props(),
    lightgbm_cegb_props(),
    lightgbm_quantized_props(),
    lightgbm_constraint_props(),
    lightgbm_early_stopping_props()
  ),
  validator = function(self) {
    # Two rules LightGBM enforces mid-fit; reported here against the setting the
    # user wrote. Neither is expressible on a property spec: both compare one
    # property against another rather than against a constant.
    sampling <- check_lightgbm_sampling(self)
    if (!is.null(sampling)) {
      return(sampling)
    }
    # Gates the objective-specific, DART and GOSS parameters on the property that
    # selects them. Spec-driven, so it publishes no rule the schema lacks.
    check_applies_when(self)
  }
) # /rtemis::LightGBMHyperparameters

method(update, LightGBMHyperparameters) <- function(
  object,
  hyperparameters,
  tuned = NULL,
  ...
) {
  object <- .update_hyperparameters(object, hyperparameters, tuned)
  # Update nrounds (e.g. in LightRuleFit)
  if (is.null(object@nrounds) && !is.null(object@force_nrounds)) {
    object@nrounds <- object@force_nrounds
  }
  object
} # /update.LightGBMHyperparameters


# %% setup_LightGBM ----
#' Setup LightGBM Hyperparameters
#'
#' Setup hyperparameters for LightGBM training.
#'
#' Get more information from [lightgbm::lgb.train].
#' `nrounds` is auto-tuned using early stopping (up to `max_nrounds`) unless
#' `force_nrounds` is set.
#'
#' @param max_nrounds Integer [1, Inf): Maximum number of boosting rounds.
#' @param force_nrounds Optional Integer [1, Inf): Use this many boosting rounds. Disables search for nrounds.
#' @param early_stopping_rounds Integer [1, Inf): Number of rounds without improvement to stop training.
#' @param num_leaves (Tunable) Integer [1, Inf): Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees. -1 = no limit.
#' @param learning_rate (Tunable) Numeric (0, 1]: Learning rate.
#' @param feature_fraction (Tunable) Numeric (0, 1]: Fraction of features to use.
#' @param bagging_fraction (Tunable) Numeric (0, 1]: Fraction of cases sampled per tree.
#' @param bagging_freq (Tunable) Integer [1, Inf): Resample every this many iterations.
#' @param lambda_l1 (Tunable) Numeric [0, Inf): L1 regularization.
#' @param lambda_l2 (Tunable) Numeric [0, Inf): L2 regularization.
#' @param max_cat_threshold (Tunable) Integer [1, Inf): Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Integer [1, Inf): Minimum number of observations per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
#' @param early_stopping_min_delta (Tunable) Numeric [0, Inf): Smallest improvement that
#' resets the early-stopping counter.
#' @param deterministic Logical: Force reproducible results at some cost in speed.
#' @param objective_seed Integer: Random seed for the objective, where it draws.
#' @param force_row_wise Logical: Force row-wise histogram building. Cheaper in memory
#' than column-wise, slower per iteration.
#' @param histogram_pool_size Numeric: Memory cap in MB for the histogram cache; -1
#' leaves it uncapped.
#' @param top_k (Tunable) Optional Integer [1, Inf): Features each machine votes for in
#' the voting tree learner. Requires `tree_learner = "voting"`. NULL uses LightGBM's
#' default of 20.
#' @param pos_bagging_fraction (Tunable) Numeric (0, 1]: Fraction of positive cases
#' sampled per tree, for a binary outcome.
#' @param neg_bagging_fraction (Tunable) Numeric (0, 1]: Fraction of negative cases
#' sampled per tree, for a binary outcome.
#' @param bagging_seed Integer: Random seed for bagging.
#' @param feature_fraction_bynode (Tunable) Numeric (0, 1]: Fraction of features sampled
#' at each node, on top of the per-tree fraction.
#' @param feature_fraction_seed Integer: Random seed for feature sampling.
#' @param cegb_tradeoff (Tunable) Numeric [0, Inf): Overall multiplier on every
#' cost-efficiency penalty. Its scale is not comparable between boosting modes.
#' @param cegb_penalty_split (Tunable) Numeric [0, Inf): Cost charged per split, in units
#' of loss reduction.
#' @param cegb_penalty_feature_lazy Optional List: Per-feature cost charged the first
#' time a case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param cegb_penalty_feature_coupled Optional List: Per-feature cost charged the first
#' time any case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param use_quantized_grad Logical: Discretize gradients before finding splits. Faster,
#' and changes the fit.
#' @param num_grad_quant_bins (Tunable) Optional Integer [2, Inf): Bins the gradients are
#' discretized into. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of 4.
#' @param quant_train_renew_leaf Optional Logical: Recompute leaf values from the exact
#' gradients after each iteration. Requires `use_quantized_grad = TRUE`. NULL uses
#' LightGBM's default of FALSE.
#' @param stochastic_rounding Optional Logical: Round gradients stochastically rather than
#' to nearest. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of TRUE.
#' @param monotone_constraints Optional Integer \[-1, 1\] vector: Per-feature
#' monotonicity: 1 increasing, -1 decreasing, 0 unconstrained. One value per feature.
#' @param monotone_constraints_method Optional Character \{"basic", "intermediate",
#' "advanced"\}: How monotonicity is enforced. Requires `monotone_constraints`.
#' @param monotone_penalty (Tunable) Optional Numeric [0, Inf): Depth penalty on splits
#' that would break monotonicity. Requires `monotone_constraints`. NULL uses LightGBM's
#' default of 0.
#' @param interaction_constraints Optional Character: Feature groups permitted to interact,
#' in LightGBM's own bracketed form, e.g. "\[0,1\],\[2,3\]".
#' @param feature_contri Optional Numeric vector: Per-feature multiplier on split gain,
#' one value per feature.
#' @param min_data_in_leaf (Tunable) Integer [0, Inf): Minimum number of cases in a leaf.
#' @param min_sum_hessian_in_leaf (Tunable) Numeric [0, Inf): Minimum sum of hessians in a
#' leaf: the smooth counterpart of a case count.
#' @param min_gain_to_split (Tunable) Numeric [0, Inf): Minimum loss reduction a split must
#' buy to be made.
#' @param max_delta_step (Tunable) Numeric: Cap on each leaf's output; 0 or less leaves it
#' uncapped.
#' @param path_smooth (Tunable) Numeric [0, Inf): Smooth each leaf towards its parent, more
#' strongly the fewer cases it holds.
#' @param extra_trees (Tunable) Logical: Choose one split threshold per feature at random
#' rather than the best.
#' @param extra_seed Integer: Random seed for the thresholds `extra_trees` draws.
#' @param linear_lambda (Tunable) Optional Numeric [0, Inf): L2 penalty on the linear models
#' fitted at the leaves. Requires `linear_tree = TRUE`. NULL uses LightGBM's default of 0.
#' @param max_bin (Tunable) Integer [2, Inf): Most bins a feature is discretized into. Fewer
#' is faster and more regularized.
#' @param min_data_in_bin (Tunable) Integer [1, Inf): Minimum number of cases per bin.
#' @param use_missing Logical: Handle missing values. FALSE ignores them.
#' @param zero_as_missing Logical: Treat zeros as missing, rather than only NA.
#' @param cat_l2 (Tunable) Numeric [0, Inf): L2 regularization on a categorical split.
#' @param cat_smooth (Tunable) Numeric [0, Inf): Smoothing over categorical levels, which
#' reduces the effect of rare ones.
#' @param max_cat_to_onehot (Tunable) Integer [1, Inf): Below this many levels, split
#' one-versus-rest rather than by partition.
#' @param boosting Character \{"gbdt", "rf", "dart"\}: Boosting algorithm:
#' gradient boosting, random forest, or DART (dropout).
#' @param data_sample_strategy Character \{"bagging", "goss"\}: How cases are
#' subsampled: at random, or by gradient magnitude (GOSS). GOSS cannot be combined
#' with bagging, so it requires `bagging_fraction = 1`.
#' @param drop_rate (Tunable) Optional Numeric \[0, 1\]: Fraction of existing trees
#' to drop per iteration. Requires `boosting = "dart"`. NULL uses LightGBM's
#' default of 0.1.
#' @param max_drop (Tunable) Optional Integer: Most trees dropped in one iteration;
#' 0 or less removes the cap. Requires `boosting = "dart"`. NULL uses LightGBM's
#' default of 50.
#' @param skip_drop (Tunable) Optional Numeric \[0, 1\]: Probability of skipping
#' dropout for an iteration. Requires `boosting = "dart"`. NULL uses LightGBM's
#' default of 0.5.
#' @param uniform_drop (Tunable) Optional Logical: Drop trees uniformly rather than
#' weighted. Requires `boosting = "dart"`. NULL uses LightGBM's default of FALSE.
#' @param xgboost_dart_mode (Tunable) Optional Logical: Use the XGBoost DART
#' normalization. Requires `boosting = "dart"`. NULL uses LightGBM's default of
#' FALSE.
#' @param drop_seed Optional Integer: Random seed for choosing which trees to drop.
#' Requires `boosting = "dart"`. NULL uses LightGBM's default of 4.
#' @param top_rate (Tunable) Optional Numeric \[0, 1\]: Share of large-gradient cases
#' GOSS retains. Requires `data_sample_strategy = "goss"`. NULL uses LightGBM's
#' default of 0.2.
#' @param other_rate (Tunable) Optional Numeric \[0, 1\]: Share of the remaining cases
#' GOSS retains; with `top_rate` it must not exceed 1. Requires
#' `data_sample_strategy = "goss"`. NULL uses LightGBM's default of 0.1.
#' @param alpha (Tunable) Optional Numeric (0, Inf): Huber delta or quantile level.
#' Requires `objective` to be "huber" or "quantile". NULL uses LightGBM's default of
#' 0.9.
#' @param tweedie_variance_power (Tunable) Optional Numeric [1, 2): Tweedie power: 1 is
#' Poisson-like, near 2 is gamma-like. Requires `objective = "tweedie"`. NULL uses
#' LightGBM's default of 1.5.
#' @param fair_c (Tunable) Optional Numeric (0, Inf): Fair loss scale. Requires
#' `objective = "fair"`. NULL uses LightGBM's default of 1.
#' @param poisson_max_delta_step (Tunable) Optional Numeric (0, Inf): Step cap
#' safeguarding Poisson optimization. Requires `objective = "poisson"`. NULL uses
#' LightGBM's default of 0.7.
#' @param sigmoid (Tunable) Optional Numeric (0, Inf): Sigmoid slope, for a binary or
#' one-vs-all objective. NULL uses LightGBM's default of 1.
#' @param boost_from_average Optional Logical: Start from the outcome's mean, for a
#' regression, binary or cross-entropy objective. NULL uses LightGBM's default of TRUE.
#' @param reg_sqrt Optional Logical: Fit the square root of the outcome and square the
#' prediction back, for a regression objective. NULL uses LightGBM's default of FALSE.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#' @param device_type Character \{"cpu", "gpu", "cuda"\}: Compute device.
#' @param tree_learner Character \{"serial", "feature", "data", "voting"\}: Tree learner type.
#' @param force_col_wise Logical: Use only with CPU - If TRUE, force col-wise histogram building.
#'
#' @return LightGBMHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' lightgbm_hyperparams <- setup_LightGBM(
#'   max_nrounds = 500L,
#'   learning_rate = tune_over(0.001, 0.01, 0.05), ifw = TRUE
#' )
#' lightgbm_hyperparams
setup_LightGBM <- function(
  max_nrounds = 1000L,
  force_nrounds = NULL,
  early_stopping_rounds = 10L,
  # tunable
  num_leaves = 8L,
  max_depth = -1L,
  learning_rate = 0.01,
  feature_fraction = 1.0,
  bagging_fraction = 1.0,
  bagging_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  ifw = FALSE,
  objective = NULL,
  early_stopping_min_delta = 0,
  deterministic = FALSE,
  objective_seed = 5L,
  force_row_wise = FALSE,
  histogram_pool_size = -1,
  top_k = NULL,
  pos_bagging_fraction = 1,
  neg_bagging_fraction = 1,
  bagging_seed = 3L,
  feature_fraction_bynode = 1,
  feature_fraction_seed = 2L,
  cegb_tradeoff = 1,
  cegb_penalty_split = 0,
  cegb_penalty_feature_lazy = NULL,
  cegb_penalty_feature_coupled = NULL,
  use_quantized_grad = FALSE,
  num_grad_quant_bins = NULL,
  quant_train_renew_leaf = NULL,
  stochastic_rounding = NULL,
  monotone_constraints = NULL,
  monotone_constraints_method = NULL,
  monotone_penalty = NULL,
  interaction_constraints = NULL,
  feature_contri = NULL,
  min_data_in_leaf = 20L,
  min_sum_hessian_in_leaf = 1e-3,
  min_gain_to_split = 0,
  max_delta_step = 0,
  path_smooth = 0,
  extra_trees = FALSE,
  extra_seed = 6L,
  linear_lambda = NULL,
  max_bin = 255L,
  min_data_in_bin = 3L,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  cat_l2 = 10,
  cat_smooth = 10,
  max_cat_to_onehot = 4L,
  boosting = "gbdt",
  data_sample_strategy = "bagging",
  drop_rate = NULL,
  max_drop = NULL,
  skip_drop = NULL,
  uniform_drop = NULL,
  xgboost_dart_mode = NULL,
  drop_seed = NULL,
  top_rate = NULL,
  other_rate = NULL,
  alpha = NULL,
  tweedie_variance_power = NULL,
  fair_c = NULL,
  poisson_max_delta_step = NULL,
  sigmoid = NULL,
  boost_from_average = NULL,
  reg_sqrt = NULL,
  device_type = "cpu",
  tree_learner = "serial",
  force_col_wise = TRUE
) {
  max_nrounds <- clean_posint(max_nrounds)
  force_nrounds <- clean_posint(force_nrounds)
  early_stopping_rounds <- clean_posint(early_stopping_rounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  bagging_freq <- clean_posint(bagging_freq)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  LightGBMHyperparameters(
    max_nrounds = max_nrounds,
    force_nrounds = force_nrounds,
    early_stopping_rounds = early_stopping_rounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    feature_fraction = feature_fraction,
    bagging_fraction = bagging_fraction,
    bagging_freq = bagging_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective,
    early_stopping_min_delta = early_stopping_min_delta,
    deterministic = deterministic,
    objective_seed = objective_seed,
    force_row_wise = force_row_wise,
    histogram_pool_size = histogram_pool_size,
    top_k = top_k,
    pos_bagging_fraction = pos_bagging_fraction,
    neg_bagging_fraction = neg_bagging_fraction,
    bagging_seed = bagging_seed,
    feature_fraction_bynode = feature_fraction_bynode,
    feature_fraction_seed = feature_fraction_seed,
    cegb_tradeoff = cegb_tradeoff,
    cegb_penalty_split = cegb_penalty_split,
    cegb_penalty_feature_lazy = cegb_penalty_feature_lazy,
    cegb_penalty_feature_coupled = cegb_penalty_feature_coupled,
    use_quantized_grad = use_quantized_grad,
    num_grad_quant_bins = num_grad_quant_bins,
    quant_train_renew_leaf = quant_train_renew_leaf,
    stochastic_rounding = stochastic_rounding,
    monotone_constraints = monotone_constraints,
    monotone_constraints_method = monotone_constraints_method,
    monotone_penalty = monotone_penalty,
    interaction_constraints = interaction_constraints,
    feature_contri = feature_contri,
    min_data_in_leaf = min_data_in_leaf,
    min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
    min_gain_to_split = min_gain_to_split,
    max_delta_step = max_delta_step,
    path_smooth = path_smooth,
    extra_trees = extra_trees,
    extra_seed = extra_seed,
    linear_lambda = linear_lambda,
    max_bin = max_bin,
    min_data_in_bin = min_data_in_bin,
    use_missing = use_missing,
    zero_as_missing = zero_as_missing,
    cat_l2 = cat_l2,
    cat_smooth = cat_smooth,
    max_cat_to_onehot = max_cat_to_onehot,
    boosting = boosting,
    data_sample_strategy = data_sample_strategy,
    drop_rate = drop_rate,
    max_drop = max_drop,
    skip_drop = skip_drop,
    uniform_drop = uniform_drop,
    xgboost_dart_mode = xgboost_dart_mode,
    drop_seed = drop_seed,
    top_rate = top_rate,
    other_rate = other_rate,
    alpha = alpha,
    tweedie_variance_power = tweedie_variance_power,
    fair_c = fair_c,
    poisson_max_delta_step = poisson_max_delta_step,
    sigmoid = sigmoid,
    boost_from_average = boost_from_average,
    reg_sqrt = reg_sqrt,
    device_type = device_type,
    tree_learner = tree_learner,
    force_col_wise = force_col_wise,
    nrounds = force_nrounds
  )
} # /rtemis::setup_LightGBM


# %% LightRuleFitHyperparameters ----
# Which of LightRuleFit's hyperparameters go to which step. `@alpha` and
# `@lambda` are the GLMNET step's; everything else the class shares with
# `LightGBMHyperparameters` goes to the LightGBM step.
LightRuleFit_glmnet_params <- c("alpha_glmnet", "lambda_glmnet")


# %% LightRuleFit_lightgbm_params ----
#' Hyperparameters forwarded to LightRuleFit's LightGBM step
#'
#' Derived rather than listed. A hand-written list is a second place to record
#' which properties the two classes share, and it fell behind the moment the
#' classes grew: 38 properties were declared on LightRuleFit and silently not
#' forwarded, so setting one did nothing at all.
#'
#' The rule is what the list was always trying to say -- every property
#' LightRuleFit shares with `LightGBMHyperparameters`, less the two that belong
#' to the GLMNET step and the `ifw` switches, which `train_LightRuleFit()`
#' resolves per step itself.
#'
#' @return Character vector of hyperparameter names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightRuleFit_lightgbm_params <- function() {
  shared <- intersect(
    hp_prop_names(LightRuleFitHyperparameters),
    hp_prop_names(LightGBMHyperparameters)
  )
  setdiff(
    shared,
    c(LightRuleFit_glmnet_params, "ifw", "ifw_lightgbm", "ifw_glmnet")
  )
} # /rtemis::LightRuleFit_lightgbm_params

#' @title LightRuleFitHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightRuleFit. The class validator enforces
#' that `ifw` (which applies to both steps) is not combined with the
#' per-step `ifw_lightgbm` / `ifw_glmnet`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightRuleFitHyperparameters <- new_class(
  name = "LightRuleFitHyperparameters",
  parent = Hyperparameters,
  properties = c(
    list(
      algorithm = prop_algorithm("LightRuleFit"),
      device_type = prop_string(
        "cpu",
        enum = c("cpu", "gpu", "cuda"),
        description = "Compute device."
      ),
      force_col_wise = prop_boolean(
        TRUE,
        description = "Force column-wise histogram building (CPU only)."
      ),
      tree_learner = prop_string(
        "serial",
        enum = c("serial", "feature", "data", "voting"),
        description = "Tree learner type."
      ),
      feature_fraction = prop_float(
        1.0,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of features sampled per tree."
      ),
      linear_tree = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Fit linear models at leaves."
      ),
      max_cat_threshold = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of split points for categorical features."
      ),
      min_data_per_group = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Minimum number of cases per categorical group."
      ),
      nrounds = prop_integer(
        200L,
        min = 1L,
        tunable = TRUE,
        description = "Number of boosting rounds (LightGBM step)."
      ),
      num_leaves = prop_integer(
        32L,
        min = 1L,
        tunable = TRUE,
        description = "Maximum number of leaves in one tree (LightGBM step)."
      ),
      max_depth = prop_integer(
        4L,
        tunable = TRUE,
        description = "Maximum tree depth (LightGBM step). -1 = no limit."
      ),
      learning_rate = prop_float(
        0.1,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Learning rate (LightGBM step)."
      ),
      bagging_fraction = prop_float(
        0.666,
        exclusive_min = 0,
        max = 1,
        tunable = TRUE,
        description = "Fraction of cases sampled per tree (LightGBM step)."
      ),
      bagging_freq = prop_integer(
        1L,
        min = 1L,
        tunable = TRUE,
        description = "Bagging frequency (LightGBM step)."
      ),
      lambda_l1 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L1 regularization (LightGBM step)."
      ),
      lambda_l2 = prop_float(
        0,
        min = 0,
        tunable = TRUE,
        description = "L2 regularization (LightGBM step)."
      ),
      objective = prop_string(
        NULL,
        nullable = TRUE,
        default_on_null = TRUE,
        description = "LightGBM objective. NULL = set from outcome type."
      ),
      ifw_lightgbm = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Inverse Frequency Weighting in the LightGBM step."
      ),
      alpha_glmnet = prop_float(
        1,
        min = 0,
        max = 1,
        tunable = TRUE,
        description = "Elastic net mixing parameter (GLMNET step)."
      ),
      lambda_glmnet = prop_float(
        NULL,
        min = 0,
        nullable = TRUE,
        vector = TRUE,
        description = "Regularization strength (GLMNET step). NULL = determined by cv.glmnet."
      ),
      ifw_glmnet = prop_boolean(
        FALSE,
        tunable = TRUE,
        description = "Inverse Frequency Weighting in the GLMNET step."
      ),
      ifw = prop_boolean(
        FALSE,
        description = "Inverse Frequency Weighting in both steps. Cannot be combined with ifw_lightgbm or ifw_glmnet."
      )
    ),
    lightgbm_objective_props(),
    lightgbm_boosting_props(),
    lightgbm_dart_props(),
    lightgbm_goss_props(),
    lightgbm_regularization_props(),
    lightgbm_binning_props(),
    lightgbm_categorical_props(),
    lightgbm_determinism_props(),
    lightgbm_execution_props(),
    lightgbm_bagging_props(),
    lightgbm_feature_sampling_props(),
    lightgbm_cegb_props(),
    lightgbm_quantized_props(),
    lightgbm_constraint_props()
  ),
  validator = function(self) {
    if (any(self@ifw) && (any(self@ifw_lightgbm) || any(self@ifw_glmnet))) {
      return("@ifw cannot be combined with @ifw_lightgbm or @ifw_glmnet.")
    }
    sampling <- check_lightgbm_sampling(self)
    if (!is.null(sampling)) {
      return(sampling)
    }
    # Gates the objective-specific, DART and GOSS parameters on the property that
    # selects them, as on the other three classes.
    check_applies_when(self)
  }
) # /rtemis::LightRuleFitHyperparameters


# %% setup_LightRuleFit ----
#' Setup LightRuleFit Hyperparameters
#'
#' Setup hyperparameters for LightRuleFit training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param nrounds (Tunable) Integer [1, Inf): Number of boosting rounds.
#' @param num_leaves (Tunable) Integer [1, Inf): Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param learning_rate (Tunable) Numeric (0, 1]: Learning rate.
#' @param bagging_fraction (Tunable) Numeric (0, 1]: Fraction of cases sampled per tree.
#' @param bagging_freq (Tunable) Integer [1, Inf): Resample every this many iterations.
#' @param lambda_l1 (Tunable) Numeric [0, Inf): L1 regularization.
#' @param lambda_l2 (Tunable) Numeric [0, Inf): L2 regularization.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
#' @param device_type Character \{"cpu", "gpu", "cuda"\}: Compute device.
#' @param force_col_wise Logical: Force column-wise histogram building (CPU only).
#' @param tree_learner Character \{"serial", "feature", "data", "voting"\}: Tree learner
#' type.
#' @param top_k (Tunable) Optional Integer [1, Inf): Features each machine votes for in
#' the voting tree learner. Requires `tree_learner = "voting"`. NULL uses LightGBM's
#' default of 20.
#' @param feature_fraction (Tunable) Numeric (0, 1]: Fraction of features sampled per tree.
#' @param feature_fraction_bynode (Tunable) Numeric (0, 1]: Fraction of features sampled
#' at each node, on top of the per-tree fraction.
#' @param feature_fraction_seed Integer: Random seed for feature sampling.
#' @param linear_tree (Tunable) Logical: Fit linear models at leaves.
#' @param linear_lambda (Tunable) Optional Numeric [0, Inf): L2 penalty on the linear models
#' fitted at the leaves. Requires `linear_tree = TRUE`. NULL uses LightGBM's default of 0.
#' @param max_cat_threshold (Tunable) Integer [1, Inf): Maximum number of split points for
#' categorical features.
#' @param min_data_per_group (Tunable) Integer [1, Inf): Minimum number of cases per
#' categorical group.
#' @param alpha (Tunable) Optional Numeric (0, Inf): Huber delta or quantile level.
#' Requires `objective` to be "huber" or "quantile". NULL uses LightGBM's default of 0.9.
#' @param deterministic Logical: Force reproducible results at some cost in speed.
#' @param objective_seed Integer: Random seed for the objective, where it draws.
#' @param force_row_wise Logical: Force row-wise histogram building. Cheaper in memory
#' than column-wise, slower per iteration.
#' @param histogram_pool_size Numeric: Memory cap in MB for the histogram cache; -1
#' leaves it uncapped.
#' @param pos_bagging_fraction (Tunable) Numeric (0, 1]: Fraction of positive cases
#' sampled per tree, for a binary outcome.
#' @param neg_bagging_fraction (Tunable) Numeric (0, 1]: Fraction of negative cases
#' sampled per tree, for a binary outcome.
#' @param bagging_seed Integer: Random seed for bagging.
#' @param cegb_tradeoff (Tunable) Numeric [0, Inf): Overall multiplier on every
#' cost-efficiency penalty. Its scale is not comparable between boosting modes.
#' @param cegb_penalty_split (Tunable) Numeric [0, Inf): Cost charged per split, in units
#' of loss reduction.
#' @param cegb_penalty_feature_lazy Optional List: Per-feature cost charged the first
#' time a case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param cegb_penalty_feature_coupled Optional List: Per-feature cost charged the first
#' time any case uses it, in \[0, Inf). One vector applied to every tree, or one vector
#' per tree.
#' @param use_quantized_grad Logical: Discretize gradients before finding splits. Faster,
#' and changes the fit.
#' @param num_grad_quant_bins (Tunable) Optional Integer [2, Inf): Bins the gradients are
#' discretized into. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of 4.
#' @param quant_train_renew_leaf Optional Logical: Recompute leaf values from the exact
#' gradients after each iteration. Requires `use_quantized_grad = TRUE`. NULL uses
#' LightGBM's default of FALSE.
#' @param stochastic_rounding Optional Logical: Round gradients stochastically rather than
#' to nearest. Requires `use_quantized_grad = TRUE`. NULL uses LightGBM's default of TRUE.
#' @param monotone_constraints Optional Integer \[-1, 1\] vector: Per-feature
#' monotonicity: 1 increasing, -1 decreasing, 0 unconstrained. One value per feature.
#' @param monotone_constraints_method Optional Character \{"basic", "intermediate",
#' "advanced"\}: How monotonicity is enforced. Requires `monotone_constraints`.
#' @param monotone_penalty (Tunable) Optional Numeric [0, Inf): Depth penalty on splits
#' that would break monotonicity. Requires `monotone_constraints`. NULL uses LightGBM's
#' default of 0.
#' @param interaction_constraints Optional Character: Feature groups permitted to interact,
#' in LightGBM's own bracketed form, e.g. "\[0,1\],\[2,3\]".
#' @param feature_contri Optional Numeric vector: Per-feature multiplier on split gain,
#' one value per feature.
#' @param min_data_in_leaf (Tunable) Integer [0, Inf): Minimum number of cases in a leaf.
#' @param min_sum_hessian_in_leaf (Tunable) Numeric [0, Inf): Minimum sum of hessians in a
#' leaf: the smooth counterpart of a case count.
#' @param min_gain_to_split (Tunable) Numeric [0, Inf): Minimum loss reduction a split must
#' buy to be made.
#' @param max_delta_step (Tunable) Numeric: Cap on each leaf's output; 0 or less leaves it
#' uncapped.
#' @param path_smooth (Tunable) Numeric [0, Inf): Smooth each leaf towards its parent, more
#' strongly the fewer cases it holds.
#' @param extra_trees (Tunable) Logical: Choose one split threshold per feature at random
#' rather than the best.
#' @param extra_seed Integer: Random seed for the thresholds `extra_trees` draws.
#' @param max_bin (Tunable) Integer [2, Inf): Most bins a feature is discretized into. Fewer
#' is faster and more regularized.
#' @param min_data_in_bin (Tunable) Integer [1, Inf): Minimum number of cases per bin.
#' @param use_missing Logical: Handle missing values. FALSE ignores them.
#' @param zero_as_missing Logical: Treat zeros as missing, rather than only NA.
#' @param cat_l2 (Tunable) Numeric [0, Inf): L2 regularization on a categorical split.
#' @param cat_smooth (Tunable) Numeric [0, Inf): Smoothing over categorical levels, which
#' reduces the effect of rare ones.
#' @param max_cat_to_onehot (Tunable) Integer [1, Inf): Below this many levels, split
#' one-versus-rest rather than by partition.
#' @param boosting Character \{"gbdt", "rf", "dart"\}: Boosting algorithm:
#' gradient boosting, random forest, or DART (dropout).
#' @param data_sample_strategy Character \{"bagging", "goss"\}: How cases are
#' subsampled: at random, or by gradient magnitude (GOSS). GOSS cannot be combined
#' with bagging, so it requires `bagging_fraction = 1`.
#' @param drop_rate (Tunable) Optional Numeric \[0, 1\]: Fraction of existing trees
#' to drop per iteration. Requires `boosting = "dart"`. NULL uses LightGBM's
#' default of 0.1.
#' @param max_drop (Tunable) Optional Integer: Most trees dropped in one iteration;
#' 0 or less removes the cap. Requires `boosting = "dart"`. NULL uses LightGBM's
#' default of 50.
#' @param skip_drop (Tunable) Optional Numeric \[0, 1\]: Probability of skipping
#' dropout for an iteration. Requires `boosting = "dart"`. NULL uses LightGBM's
#' default of 0.5.
#' @param uniform_drop (Tunable) Optional Logical: Drop trees uniformly rather than
#' weighted. Requires `boosting = "dart"`. NULL uses LightGBM's default of FALSE.
#' @param xgboost_dart_mode (Tunable) Optional Logical: Use the XGBoost DART
#' normalization. Requires `boosting = "dart"`. NULL uses LightGBM's default of
#' FALSE.
#' @param drop_seed Optional Integer: Random seed for choosing which trees to drop.
#' Requires `boosting = "dart"`. NULL uses LightGBM's default of 4.
#' @param top_rate (Tunable) Optional Numeric \[0, 1\]: Share of large-gradient cases
#' GOSS retains. Requires `data_sample_strategy = "goss"`. NULL uses LightGBM's
#' default of 0.2.
#' @param other_rate (Tunable) Optional Numeric \[0, 1\]: Share of the remaining cases
#' GOSS retains; with `top_rate` it must not exceed 1. Requires
#' `data_sample_strategy = "goss"`. NULL uses LightGBM's default of 0.1.
#' @param tweedie_variance_power (Tunable) Optional Numeric [1, 2): Tweedie power: 1 is
#' Poisson-like, near 2 is gamma-like. Requires `objective = "tweedie"`. NULL uses
#' LightGBM's default of 1.5.
#' @param fair_c (Tunable) Optional Numeric (0, Inf): Fair loss scale. Requires
#' `objective = "fair"`. NULL uses LightGBM's default of 1.
#' @param poisson_max_delta_step (Tunable) Optional Numeric (0, Inf): Step cap
#' safeguarding Poisson optimization. Requires `objective = "poisson"`. NULL uses
#' LightGBM's default of 0.7.
#' @param sigmoid (Tunable) Optional Numeric (0, Inf): Sigmoid slope, for a binary or
#' one-vs-all objective. NULL uses LightGBM's default of 1.
#' @param boost_from_average Optional Logical: Start from the outcome's mean, for a
#' regression, binary or cross-entropy objective. NULL uses LightGBM's default of TRUE.
#' @param reg_sqrt Optional Logical: Fit the square root of the outcome and square the
#' prediction back, for a regression objective. NULL uses LightGBM's default of FALSE.
#' @param ifw_lightgbm (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in the LightGBM
#' step.
#' @param alpha_glmnet (Tunable) Numeric \[0, 1\]: Alpha for GLMNET.
#' @param lambda_glmnet Optional Numeric [0, Inf) vector: Lambda for GLMNET. NULL = determined by cv.glmnet.
#' @param ifw_glmnet (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in the GLMNET step.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification. This applies IFW
#' to both LightGBM and GLMNET; cannot be combined with `ifw_lightgbm` or `ifw_glmnet`.
#'
#' @return LightRuleFitHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' lightrulefit_hyperparams <- setup_LightRuleFit(nrounds = 300L, max_depth = 3L)
#' lightrulefit_hyperparams
setup_LightRuleFit <- function(
  nrounds = 200L,
  num_leaves = 32L,
  max_depth = 4L,
  learning_rate = 0.1,
  bagging_fraction = 0.666,
  bagging_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  objective = NULL,
  device_type = "cpu",
  force_col_wise = TRUE,
  tree_learner = "serial",
  top_k = NULL,
  feature_fraction = 1.0,
  feature_fraction_bynode = 1,
  feature_fraction_seed = 2L,
  linear_tree = FALSE,
  linear_lambda = NULL,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  alpha = NULL,
  deterministic = FALSE,
  objective_seed = 5L,
  force_row_wise = FALSE,
  histogram_pool_size = -1,
  pos_bagging_fraction = 1,
  neg_bagging_fraction = 1,
  bagging_seed = 3L,
  cegb_tradeoff = 1,
  cegb_penalty_split = 0,
  cegb_penalty_feature_lazy = NULL,
  cegb_penalty_feature_coupled = NULL,
  use_quantized_grad = FALSE,
  num_grad_quant_bins = NULL,
  quant_train_renew_leaf = NULL,
  stochastic_rounding = NULL,
  monotone_constraints = NULL,
  monotone_constraints_method = NULL,
  monotone_penalty = NULL,
  interaction_constraints = NULL,
  feature_contri = NULL,
  min_data_in_leaf = 20L,
  min_sum_hessian_in_leaf = 1e-3,
  min_gain_to_split = 0,
  max_delta_step = 0,
  path_smooth = 0,
  extra_trees = FALSE,
  extra_seed = 6L,
  max_bin = 255L,
  min_data_in_bin = 3L,
  use_missing = TRUE,
  zero_as_missing = FALSE,
  cat_l2 = 10,
  cat_smooth = 10,
  max_cat_to_onehot = 4L,
  boosting = "gbdt",
  data_sample_strategy = "bagging",
  drop_rate = NULL,
  max_drop = NULL,
  skip_drop = NULL,
  uniform_drop = NULL,
  xgboost_dart_mode = NULL,
  drop_seed = NULL,
  top_rate = NULL,
  other_rate = NULL,
  tweedie_variance_power = NULL,
  fair_c = NULL,
  poisson_max_delta_step = NULL,
  sigmoid = NULL,
  boost_from_average = NULL,
  reg_sqrt = NULL,
  ifw_lightgbm = FALSE,
  alpha_glmnet = 1,
  lambda_glmnet = NULL,
  ifw_glmnet = FALSE,
  ifw = FALSE
) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  bagging_freq <- clean_posint(bagging_freq)
  LightRuleFitHyperparameters(
    nrounds = nrounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    bagging_fraction = bagging_fraction,
    bagging_freq = bagging_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    objective = objective,
    device_type = device_type,
    force_col_wise = force_col_wise,
    tree_learner = tree_learner,
    top_k = top_k,
    feature_fraction = feature_fraction,
    feature_fraction_bynode = feature_fraction_bynode,
    feature_fraction_seed = feature_fraction_seed,
    linear_tree = linear_tree,
    linear_lambda = linear_lambda,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    alpha = alpha,
    deterministic = deterministic,
    objective_seed = objective_seed,
    force_row_wise = force_row_wise,
    histogram_pool_size = histogram_pool_size,
    pos_bagging_fraction = pos_bagging_fraction,
    neg_bagging_fraction = neg_bagging_fraction,
    bagging_seed = bagging_seed,
    cegb_tradeoff = cegb_tradeoff,
    cegb_penalty_split = cegb_penalty_split,
    cegb_penalty_feature_lazy = cegb_penalty_feature_lazy,
    cegb_penalty_feature_coupled = cegb_penalty_feature_coupled,
    use_quantized_grad = use_quantized_grad,
    num_grad_quant_bins = num_grad_quant_bins,
    quant_train_renew_leaf = quant_train_renew_leaf,
    stochastic_rounding = stochastic_rounding,
    monotone_constraints = monotone_constraints,
    monotone_constraints_method = monotone_constraints_method,
    monotone_penalty = monotone_penalty,
    interaction_constraints = interaction_constraints,
    feature_contri = feature_contri,
    min_data_in_leaf = min_data_in_leaf,
    min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
    min_gain_to_split = min_gain_to_split,
    max_delta_step = max_delta_step,
    path_smooth = path_smooth,
    extra_trees = extra_trees,
    extra_seed = extra_seed,
    max_bin = max_bin,
    min_data_in_bin = min_data_in_bin,
    use_missing = use_missing,
    zero_as_missing = zero_as_missing,
    cat_l2 = cat_l2,
    cat_smooth = cat_smooth,
    max_cat_to_onehot = max_cat_to_onehot,
    boosting = boosting,
    data_sample_strategy = data_sample_strategy,
    drop_rate = drop_rate,
    max_drop = max_drop,
    skip_drop = skip_drop,
    uniform_drop = uniform_drop,
    xgboost_dart_mode = xgboost_dart_mode,
    drop_seed = drop_seed,
    top_rate = top_rate,
    other_rate = other_rate,
    tweedie_variance_power = tweedie_variance_power,
    fair_c = fair_c,
    poisson_max_delta_step = poisson_max_delta_step,
    sigmoid = sigmoid,
    boost_from_average = boost_from_average,
    reg_sqrt = reg_sqrt,
    ifw_lightgbm = ifw_lightgbm,
    alpha_glmnet = alpha_glmnet,
    lambda_glmnet = lambda_glmnet,
    ifw_glmnet = ifw_glmnet,
    ifw = ifw
  )
} # /rtemis::setup_LightRuleFit


# %% IsotonicHyperparameters ----
#' @title IsotonicHyperparameters
#'
#' @description
#' Hyperparameters subclass for Isotonic Regression.
#'
#' @author EDG
#' @keywords internal
#' @noRd
IsotonicHyperparameters <- new_class(
  name = "IsotonicHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("Isotonic"),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::IsotonicHyperparameters


# %% setup_Isotonic ----
#' Setup Isotonic Hyperparameters
#'
#' Setup hyperparameters for Isotonic Regression.
#'
#' There are not hyperparameters for this algorithm at this moment.
#'
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return IsotonicHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' isotonic_hyperparams <- setup_Isotonic(ifw = TRUE)
#' isotonic_hyperparams
setup_Isotonic <- function(ifw = FALSE) {
  IsotonicHyperparameters(ifw = ifw)
} # /rtemis::setup_Isotonic


# %% LinearSVMHyperparameters ----
#' @title LinearSVMHyperparameters
#'
#' @description
#' Hyperparameters subclass for SVM with linear kernel. The kernel is a
#' constant: determined by the class, not settable.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LinearSVMHyperparameters <- new_class(
  name = "LinearSVMHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("LinearSVM"),
    kernel = prop_const(
      "linear",
      description = "SVM kernel; determined by the class."
    ),
    cost = prop_float(
      1,
      exclusive_min = 0,
      tunable = TRUE,
      description = "Cost of constraints violation."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::LinearSVMHyperparameters

# %% setup_LinearSVM ----
#' Setup LinearSVM Hyperparameters
#'
#' Setup hyperparameters for LinearSVM training.
#'
#' Get more information from [e1071::svm].
#'
#' @param cost (Tunable) Numeric (0, Inf): Cost of constraints violation.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return LinearSVMHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' linear_svm_hyperparams <- setup_LinearSVM(cost = 0.5, ifw = TRUE)
#' linear_svm_hyperparams
setup_LinearSVM <- function(
  cost = 1,
  ifw = FALSE
) {
  LinearSVMHyperparameters(
    cost = cost,
    ifw = ifw
  )
} # /setup_LinearSVM


# %% RadialSVMHyperparameters ----
#' @title RadialSVMHyperparameters
#'
#' @description
#' Hyperparameters subclass for SVM with radial kernel. The kernel is a
#' constant: determined by the class, not settable.
#'
#' @author EDG
#' @keywords internal
#' @noRd
RadialSVMHyperparameters <- new_class(
  name = "RadialSVMHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("RadialSVM"),
    kernel = prop_const(
      "radial",
      description = "SVM kernel; determined by the class."
    ),
    cost = prop_float(
      1,
      exclusive_min = 0,
      tunable = TRUE,
      description = "Cost of constraints violation."
    ),
    gamma = prop_float(
      0.01,
      exclusive_min = 0,
      tunable = TRUE,
      description = "Kernel coefficient."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::RadialSVMHyperparameters

# %% setup_RadialSVM ----
#' Setup RadialSVM Hyperparameters
#'
#' Setup hyperparameters for RadialSVM training.
#'
#' Get more information from [e1071::svm].
#'
#' @param cost (Tunable) Numeric (0, Inf): Cost of constraints violation.
#' @param gamma (Tunable) Numeric (0, Inf): Kernel coefficient.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return RadialSVMHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' radial_svm_hyperparams <- setup_RadialSVM(cost = 10, gamma = 0.1, ifw = TRUE)
#' radial_svm_hyperparams
setup_RadialSVM <- function(
  cost = 1,
  gamma = 0.01,
  ifw = FALSE
) {
  RadialSVMHyperparameters(
    cost = cost,
    gamma = gamma,
    ifw = ifw
  )
} # /setup_RadialSVM


# %% MLP_SHAPES ----
# Profiles the hidden-layer widths can be generated in. The vocabulary is
# Talos's (<https://mikkokotila.github.io/slate/#shapes>), by way of
# AutoPyTorch's `get_shaped_neuron_counts()`. One rename: the literature's
# "brick" is `constant` here, which is what the tabular deep-learning benchmarks
# call the same thing and is self-describing beside the other six.
MLP_SHAPES <- c(
  "funnel",
  "constant",
  "triangle",
  "long_funnel",
  "diamond",
  "hexagon",
  "stairs"
)


# %% MLPHyperparameters ----
#' @title MLPHyperparameters
#'
#' @description
#' Hyperparameters subclass for MLP, a multilayer perceptron built and trained
#' with `torch`.
#'
#' The hidden architecture is given in one of two mutually exclusive ways:
#' `hidden_units` states the widths directly, or the `shape_*` trio generates
#' them. Supplying both is rejected by `setup_MLP()` rather than silently
#' resolved -- and by `setup_MLP()` rather than by this class's validator,
#' because training resolves the widths *into* `hidden_units`, so a fitted
#' object legitimately carries both and a class-level rule would reject the
#' result of a valid run. Every path that builds one of these from user input
#' goes through `setup_MLP()`, including `.list_to_Hyperparameters()`, so
#' nothing escapes the check.
#'
#' @author EDG
#' @keywords internal
#' @noRd
MLPHyperparameters <- new_class(
  name = "MLPHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("MLP"),
    # Architecture ----
    hidden_units = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      vector = TRUE,
      description = "Units in each hidden layer, one value per layer, so the length is the depth. NULL generates the widths from the shape settings."
    ),
    shape = prop_string(
      NULL,
      enum = MLP_SHAPES,
      nullable = TRUE,
      tunable = TRUE,
      description = "Profile of the generated hidden layer widths. Ignored when hidden_units is set."
    ),
    shape_layers = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Number of hidden layers to generate. Ignored when hidden_units is set."
    ),
    shape_max_units = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Widest generated hidden layer. NULL derives it from the encoded input width. Ignored when hidden_units is set."
    ),
    activation = prop_string(
      "relu",
      enum = TORCH_ACTIVATIONS,
      tunable = TRUE,
      description = "Activation applied after every hidden layer."
    ),
    norm = prop_string(
      NULL,
      enum = TORCH_NORMS,
      nullable = TRUE,
      tunable = TRUE,
      description = "Normalization applied in every hidden layer. NULL applies none."
    ),
    norm_first = prop_boolean(
      FALSE,
      description = "Normalize before the activation rather than after it."
    ),
    bias = prop_boolean(
      TRUE,
      description = "Give every hidden layer and the output layer a bias term."
    ),
    residual = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Add a residual connection around every hidden layer, projected when the layer changes width."
    ),
    # Regularization ----
    dropout = prop_float(
      0,
      min = 0,
      exclusive_max = 1,
      tunable = TRUE,
      description = "Dropout probability applied after every hidden layer."
    ),
    input_dropout = prop_float(
      0,
      min = 0,
      exclusive_max = 1,
      tunable = TRUE,
      description = "Dropout probability applied to the encoded input."
    ),
    weight_decay = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "L2 penalty, decoupled from the gradient under the adamw optimizer."
    ),
    l1_penalty = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "L1 penalty on the linear weights, added to the loss. Not interchangeable with weight_decay, which is L2."
    ),
    # Categorical embeddings ----
    embeddings = prop_boolean(
      TRUE,
      description = "Represent each categorical feature by a learned embedding. FALSE one-hot encodes them instead."
    ),
    embedding_dim = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Width of every embedding. NULL sizes each from its feature's cardinality."
    ),
    embedding_dropout = prop_float(
      0,
      min = 0,
      exclusive_max = 1,
      tunable = TRUE,
      description = "Dropout probability applied to the concatenated embeddings."
    ),
    # Optimization ----
    optimizer = prop_string(
      "adamw",
      enum = TORCH_OPTIMIZERS,
      tunable = TRUE,
      description = "Optimization algorithm."
    ),
    lr = prop_float(
      1e-3,
      exclusive_min = 0,
      tunable = TRUE,
      description = "Learning rate."
    ),
    beta1 = prop_float(
      NULL,
      min = 0,
      exclusive_max = 1,
      nullable = TRUE,
      applies_when = list(optimizer = c("adamw", "adam")),
      description = "Exponential decay rate of the first moment estimate. NULL leaves the torch default."
    ),
    beta2 = prop_float(
      NULL,
      min = 0,
      exclusive_max = 1,
      nullable = TRUE,
      applies_when = list(optimizer = c("adamw", "adam")),
      description = "Exponential decay rate of the second moment estimate. NULL leaves the torch default."
    ),
    eps = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      applies_when = list(optimizer = c("adamw", "adam", "rmsprop")),
      description = "Term added to the denominator for numerical stability. NULL leaves the torch default."
    ),
    momentum = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      applies_when = list(optimizer = c("sgd", "rmsprop")),
      description = "Momentum factor. NULL leaves the torch default."
    ),
    lr_scheduler = prop_string(
      NULL,
      enum = TORCH_SCHEDULERS,
      nullable = TRUE,
      description = "Learning-rate schedule, configured from the epoch budget. NULL holds the learning rate fixed."
    ),
    batch_size = prop_integer(
      256L,
      min = 1L,
      tunable = TRUE,
      description = "Cases per optimization step."
    ),
    max_epochs = prop_integer(
      100L,
      min = 1L,
      tunable = TRUE,
      description = "Largest number of passes over the training set."
    ),
    patience = prop_integer(
      10L,
      min = 1L,
      description = "Epochs without validation improvement before stopping early. Requires validation data; without it the fit runs the full epoch budget."
    ),
    max_grad_norm = prop_float(
      NULL,
      exclusive_min = 0,
      nullable = TRUE,
      tunable = TRUE,
      description = "Clip the gradient norm to this value before each step. NULL does not clip."
    ),
    loss = prop_string(
      NULL,
      enum = TORCH_LOSSES,
      nullable = TRUE,
      default_on_null = TRUE,
      description = "Training objective. NULL = set from outcome type."
    ),
    # Execution ----
    device = prop_string(
      NULL,
      enum = TORCH_DEVICES,
      nullable = TRUE,
      description = "Compute device. On mps a seed does not reach dropout, so a fit using it is not reproducible; the run says so."
    ),
    seed = prop_integer(
      NULL,
      nullable = TRUE,
      description = "Random seed for weight initialization, dropout and batch shuffling. NULL leaves them drawn from the ambient RNG."
    ),
    num_workers = prop_integer(
      0L,
      min = 0L,
      description = "Subprocesses used to load batches. 0 loads them in the main process."
    ),
    drop_last = prop_boolean(
      FALSE,
      description = "Drop the last incomplete batch of each training epoch."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  ),
  validator = function(self) {
    check_applies_when(self)
  }
) # /rtemis::MLPHyperparameters


# %% setup_MLP ----
#' Setup MLP Hyperparameters
#'
#' Setup hyperparameters for MLP (Multilayer Perceptron) training.
#'
#' A fully connected feedforward network built and trained with `torch`, for
#' regression, binary and multiclass classification.
#'
#' **Architecture.** Give the hidden layers directly with `hidden_units` --
#' `c(256L, 128L, 64L)` is three layers of those widths -- or leave it NULL and
#' let `shape`, `shape_layers` and `shape_max_units` generate them. Setting both
#' is an error rather than a silent override. The generated profiles are
#' `funnel` (a linear taper from the widest layer down to a third of it),
#' `constant`, `triangle` (a linear rise from the input width), `long_funnel`,
#' `diamond`, `hexagon` and `stairs`; the vocabulary is Talos's, by way of
#' AutoPyTorch. `shape_max_units` defaults to four times the encoded input
#' width, clamped to \[64, 512\] and never below that width.
#'
#' The tabular deep-learning benchmarks tune and publish *constant*-width MLPs,
#' so `shape = "constant"` is what to compare against even though `funnel` is
#' the better default before tuning on the small-n, wide-p data rtemis is usually
#' pointed at.
#'
#' **Tuning.** `hidden_units` is tunable like any other hyperparameter, with one
#' architecture per candidate:
#' `setup_MLP(hidden_units = tune_over(c(64L, 32L), c(128L, 64L, 32L)))`. A
#' single bare vector there is one architecture, not a set of candidates, and is
#' rejected as such.
#'
#' **Categorical features** are represented by learned embeddings, each sized
#' `min(600, round(1.6 * cardinality^0.56))` unless `embedding_dim` fixes them
#' all. `embeddings = FALSE` one-hot encodes instead. Numeric features are
#' always centered and scaled -- an unscaled network fails quietly rather than
#' loudly -- and the fitted encoder is re-applied at predict time.
#'
#' **Device and reproducibility.** `device = NULL` picks `cuda` where available
#' and `cpu` otherwise, and [train] names the one it resolved. `"mps"` is
#' supported but never chosen automatically: on Apple silicon it is slower than
#' the CPU for networks of the size tabular data calls for -- the matrices are
#' small enough that dispatch dominates -- and a `seed` governs weight
#' initialization and batch shuffling there but **not dropout**, so a seeded
#' `mps` fit reproduces exactly until a dropout rate is non-zero. That
#' combination warns.
#'
#' **Early stopping** needs a validation set: pass `dat_validation` to [train],
#' and the fit keeps the weights of the best validation epoch rather than the
#' last. Without one it runs the full `max_epochs` and `patience` has no effect.
#'
#' **`l1_penalty` is not `weight_decay`.** `weight_decay` is the L2 term the
#' torch optimizer applies, decoupled from the gradient under `adamw`;
#' `l1_penalty` has no torch equivalent and is accumulated over the linear
#' weights and added to the loss.
#'
#' **Batch normalization and dropout interact badly** when combined; both are
#' off by default.
#'
#' **The scheduler takes no settings of its own** -- each configures itself from
#' the run's budget: `step` decays by 0.1 every `max_epochs / 3` epochs,
#' `cosine_annealing` anneals over `max_epochs`, `one_cycle` peaks at `lr` over
#' the run's real step count, and `reduce_on_plateau` decays by 0.1 after half
#' the early-stopping patience.
#'
#' `get_varimp()` returns NULL: a torch MLP has no native importance measure.
#'
#' @param hidden_units (Tunable) Optional Integer [1, Inf) vector: Units in each hidden layer, one value per layer. NULL generates the widths from the shape settings.
#' @param shape (Tunable) Optional Character \{"funnel", "constant", "triangle", "long_funnel", "diamond", "hexagon", "stairs"\}: Profile of the generated hidden layer widths. Ignored when hidden_units is set.
#' @param shape_layers (Tunable) Optional Integer [1, Inf): Number of hidden layers to generate. Ignored when hidden_units is set.
#' @param shape_max_units (Tunable) Optional Integer [1, Inf): Widest generated hidden layer. NULL derives it from the encoded input width.
#' @param activation (Tunable) Character \{"relu", "gelu", "silu", "elu", "selu", "leaky_relu", "tanh"\}: Activation applied after every hidden layer.
#' @param norm (Tunable) Optional Character \{"batch_norm", "layer_norm"\}: Normalization applied in every hidden layer. NULL applies none.
#' @param norm_first Logical: If TRUE, normalize before the activation rather than after it.
#' @param bias Logical: If TRUE, give every hidden layer and the output layer a bias term.
#' @param residual (Tunable) Logical: If TRUE, add a residual connection around every hidden layer, projected when the layer changes width.
#' @param dropout (Tunable) Numeric [0, 1): Dropout probability applied after every hidden layer.
#' @param input_dropout (Tunable) Numeric [0, 1): Dropout probability applied to the encoded input.
#' @param weight_decay (Tunable) Numeric [0, Inf): L2 penalty, decoupled from the gradient under the adamw optimizer.
#' @param l1_penalty (Tunable) Numeric [0, Inf): L1 penalty on the linear weights, added to the loss.
#' @param embeddings Logical: If TRUE, represent each categorical feature by a learned embedding; if FALSE, one-hot encode them.
#' @param embedding_dim (Tunable) Optional Integer [1, Inf): Width of every embedding. NULL sizes each from its feature's cardinality.
#' @param embedding_dropout (Tunable) Numeric [0, 1): Dropout probability applied to the concatenated embeddings.
#' @param optimizer (Tunable) Character \{"adamw", "adam", "sgd", "rmsprop"\}: Optimization algorithm.
#' @param lr (Tunable) Numeric (0, Inf): Learning rate.
#' @param beta1 Optional Numeric [0, 1): Exponential decay rate of the first moment estimate. Applies to the adam and adamw optimizers.
#' @param beta2 Optional Numeric [0, 1): Exponential decay rate of the second moment estimate. Applies to the adam and adamw optimizers.
#' @param eps Optional Numeric (0, Inf): Term added to the denominator for numerical stability. Applies to the adam, adamw and rmsprop optimizers.
#' @param momentum Optional Numeric [0, Inf): Momentum factor. Applies to the sgd and rmsprop optimizers.
#' @param lr_scheduler Optional Character \{"step", "cosine_annealing", "one_cycle", "reduce_on_plateau"\}: Learning-rate schedule. NULL holds the learning rate fixed.
#' @param batch_size (Tunable) Integer [1, Inf): Cases per optimization step.
#' @param max_epochs (Tunable) Integer [1, Inf): Largest number of passes over the training set.
#' @param patience Integer [1, Inf): Epochs without validation improvement before stopping early.
#' @param max_grad_norm (Tunable) Optional Numeric (0, Inf): Clip the gradient norm to this value before each step. NULL does not clip.
#' @param loss Optional Character \{"mse", "l1", "smooth_l1", "cross_entropy"\}: Training objective. NULL sets it from the outcome type.
#' @param device Optional Character \{"cpu", "cuda", "mps"\}: Compute device.
#' @param seed Optional Integer: Random seed for weight initialization, dropout and batch shuffling.
#' @param num_workers Integer [0, Inf): Subprocesses used to load batches.
#' @param drop_last Logical: If TRUE, drop the last incomplete batch of each training epoch.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return MLPHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' mlp_hyperparams <- setup_MLP(hidden_units = c(64L, 32L), max_epochs = 20L)
#' mlp_hyperparams
setup_MLP <- function(
  # tunable
  hidden_units = NULL,
  shape = NULL,
  shape_layers = NULL,
  shape_max_units = NULL,
  activation = "relu",
  norm = NULL,
  residual = FALSE,
  dropout = 0,
  input_dropout = 0,
  weight_decay = 0,
  l1_penalty = 0,
  embedding_dim = NULL,
  embedding_dropout = 0,
  optimizer = "adamw",
  lr = 1e-3,
  batch_size = 256L,
  max_epochs = 100L,
  max_grad_norm = NULL,
  ifw = FALSE,
  # fixed
  norm_first = FALSE,
  bias = TRUE,
  embeddings = TRUE,
  beta1 = NULL,
  beta2 = NULL,
  eps = NULL,
  momentum = NULL,
  lr_scheduler = NULL,
  patience = 10L,
  loss = NULL,
  device = NULL,
  seed = NULL,
  num_workers = 0L,
  drop_last = FALSE
) {
  hidden_units <- clean_posint(hidden_units)
  shape_layers <- clean_posint(shape_layers)
  shape_max_units <- clean_posint(shape_max_units)
  embedding_dim <- clean_posint(embedding_dim)
  batch_size <- clean_posint(batch_size)
  max_epochs <- clean_posint(max_epochs)
  patience <- clean_posint(patience)
  seed <- clean_int(seed)
  num_workers <- clean_int(num_workers)
  # The two architecture paths are alternatives, and silently overriding one
  # with the other is what makes a two-path API confusing. Checked here rather
  # than in the class validator because training writes the widths it resolved
  # back into `hidden_units` -- so a fitted object legitimately holds both, and
  # a class-level rule would reject the result of a valid run. Every path that
  # builds these from user input arrives here, `.list_to_Hyperparameters()`
  # included.
  shape_given <- c(
    shape = !is.null(shape),
    shape_layers = !is.null(shape_layers),
    shape_max_units = !is.null(shape_max_units)
  )
  if (!is.null(hidden_units) && any(shape_given)) {
    rtemis.core::abort(
      "`hidden_units` and ",
      paste0("`", names(shape_given)[shape_given], "`", collapse = ", "),
      " are two ways to give the same thing.\n",
      "Set `hidden_units` for the widths themselves, or leave it NULL and set `shape` / `shape_layers` / `shape_max_units` to generate them.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  MLPHyperparameters(
    hidden_units = hidden_units,
    shape = shape,
    shape_layers = shape_layers,
    shape_max_units = shape_max_units,
    activation = activation,
    norm = norm,
    norm_first = norm_first,
    bias = bias,
    residual = residual,
    dropout = dropout,
    input_dropout = input_dropout,
    weight_decay = weight_decay,
    l1_penalty = l1_penalty,
    embeddings = embeddings,
    embedding_dim = embedding_dim,
    embedding_dropout = embedding_dropout,
    optimizer = optimizer,
    lr = lr,
    beta1 = beta1,
    beta2 = beta2,
    eps = eps,
    momentum = momentum,
    lr_scheduler = lr_scheduler,
    batch_size = batch_size,
    max_epochs = max_epochs,
    patience = patience,
    max_grad_norm = max_grad_norm,
    loss = loss,
    device = device,
    seed = seed,
    num_workers = num_workers,
    drop_last = drop_last,
    ifw = ifw
  )
} # /setup_MLP


# %% TabNetHyperparameters ----
#' @title TabNetHyperparameters
#'
#' @description
#' Hyperparameters subclass for TabNet. `optimizer` and `lr_scheduler`
#' accept either a character name or a torch function, so they are plain
#' properties (non-tunable, excluded from schema generation).
#'
#' @author EDG
#' @keywords internal
#' @noRd
TabNetHyperparameters <- new_class(
  name = "TabNetHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("TabNet"),
    batch_size = prop_integer(
      500L,
      min = 1L,
      tunable = TRUE,
      description = "Batch size."
    ),
    penalty = prop_float(
      0.001,
      min = 0,
      tunable = TRUE,
      description = "Sparsity regularization penalty."
    ),
    clip_value = prop_float(
      NULL,
      nullable = TRUE,
      tunable = TRUE,
      description = "Gradient clip value."
    ),
    loss = prop_string(
      "auto",
      tunable = TRUE,
      description = "Loss function. auto = set from outcome type."
    ),
    epochs = prop_integer(
      50L,
      min = 1L,
      tunable = TRUE,
      description = "Number of training epochs."
    ),
    drop_last = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Drop the last incomplete batch."
    ),
    decision_width = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Decision prediction layer width."
    ),
    attention_width = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Attention embedding width."
    ),
    num_steps = prop_integer(
      3L,
      min = 1L,
      tunable = TRUE,
      description = "Number of decision steps."
    ),
    feature_reusage = prop_float(
      1.3,
      min = 0,
      tunable = TRUE,
      description = "Feature reusage coefficient."
    ),
    mask_type = prop_string(
      "sparsemax",
      enum = c("sparsemax", "entmax"),
      tunable = TRUE,
      description = "Masking function."
    ),
    virtual_batch_size = prop_integer(
      65536L,
      min = 1L,
      tunable = TRUE,
      description = "Virtual batch size (ghost batch normalization)."
    ),
    valid_split = prop_float(
      0,
      min = 0,
      exclusive_max = 1,
      tunable = TRUE,
      description = "Fraction of data used for (tabnet-internal) validation."
    ),
    learn_rate = prop_float(
      0.02,
      exclusive_min = 0,
      tunable = TRUE,
      description = "Learning rate."
    ),
    optimizer = prop_string(
      "adam",
      description = "Optimizer name, resolved by the tabnet backend."
    ),
    lr_scheduler = prop_string(
      NULL,
      enum = c("step", "reduce_on_plateau"),
      nullable = TRUE,
      description = "Learning-rate scheduler. NULL = none."
    ),
    lr_decay = prop_float(
      0.1,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Learning rate decay."
    ),
    step_size = prop_integer(
      30L,
      min = 1L,
      tunable = TRUE,
      description = "Learning rate scheduler step size."
    ),
    checkpoint_epochs = prop_integer(
      10L,
      min = 1L,
      tunable = TRUE,
      description = "Checkpoint interval in epochs."
    ),
    cat_emb_dim = prop_integer(
      1L,
      min = 1L,
      tunable = TRUE,
      description = "Categorical embedding dimension."
    ),
    num_independent = prop_integer(
      2L,
      min = 1L,
      tunable = TRUE,
      description = "Number of independent GLU layers at each encoder step."
    ),
    num_shared = prop_integer(
      2L,
      min = 1L,
      tunable = TRUE,
      description = "Number of shared GLU layers at each encoder step."
    ),
    num_independent_decoder = prop_integer(
      1L,
      min = 1L,
      tunable = TRUE,
      description = "Number of independent GLU layers for pretraining."
    ),
    num_shared_decoder = prop_integer(
      1L,
      min = 1L,
      tunable = TRUE,
      description = "Number of shared GLU layers for pretraining."
    ),
    momentum = prop_float(
      0.02,
      min = 0,
      tunable = TRUE,
      description = "Momentum for batch normalization."
    ),
    pretraining_ratio = prop_float(
      0.5,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Ratio of features to mask during pretraining."
    ),
    device = prop_string(
      "auto",
      enum = c("auto", "cpu", "cuda"),
      description = "Compute device."
    ),
    importance_sample_size = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Sample size for importance calculation."
    ),
    early_stopping_monitor = prop_string(
      "auto",
      enum = c("auto", "valid_loss", "train_loss"),
      tunable = TRUE,
      description = "Metric monitored for early stopping."
    ),
    early_stopping_tolerance = prop_float(
      0,
      min = 0,
      tunable = TRUE,
      description = "Minimum relative improvement to reset the patience counter."
    ),
    early_stopping_patience = prop_integer(
      0L,
      min = 0L,
      tunable = TRUE,
      description = "Number of epochs without improvement before stopping."
    ),
    num_workers = prop_integer(
      0L,
      min = 0L,
      description = "Number of subprocesses for data loading."
    ),
    skip_importance = prop_boolean(
      FALSE,
      description = "Skip importance calculation."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::TabNetHyperparameters


# %% setup_TabNet ----
#' Setup TabNet Hyperparameters
#'
#' Setup hyperparameters for TabNet training.
#'
# Get more information from [tabnet::tabnet_config]
#'
#' @param batch_size (Tunable) Integer [1, Inf): Batch size.
#' @param penalty (Tunable) Numeric [0, Inf): Regularization penalty.
#' @param clip_value (Tunable) Optional Numeric: Clip value.
#' @param loss (Tunable) Character: Loss function.
#' @param epochs (Tunable) Integer [1, Inf): Number of epochs.
#' @param drop_last (Tunable) Logical: If TRUE, drop last batch.
#' @param decision_width (Tunable) Optional Integer [1, Inf): Decision width.
#' @param attention_width (Tunable) Optional Integer [1, Inf): Attention width.
#' @param num_steps (Tunable) Integer [1, Inf): Number of steps.
#' @param feature_reusage (Tunable) Numeric [0, Inf): Feature reusage.
#' @param mask_type (Tunable) Character \{"sparsemax", "entmax"\}: Mask type.
#' @param virtual_batch_size (Tunable) Integer [1, Inf): Virtual batch size.
#' @param valid_split (Tunable) Numeric [0, 1): Validation split.
#' @param learn_rate (Tunable) Numeric (0, Inf): Learning rate.
#' @param optimizer Character: Optimizer name, resolved by the tabnet backend.
#' @param lr_scheduler Optional Character \{"step", "reduce_on_plateau"\}: Learning-rate scheduler.
#' @param lr_decay (Tunable) Numeric \[0, 1\]: Learning rate decay.
#' @param step_size (Tunable) Integer [1, Inf): Step size.
#' @param checkpoint_epochs (Tunable) Integer [1, Inf): Checkpoint epochs.
#' @param cat_emb_dim (Tunable) Integer [1, Inf): Categorical embedding dimension.
#' @param num_independent (Tunable) Integer [1, Inf): Number of independent Gated Linear Units (GLU)
#' at each step of the encoder.
#' @param num_shared (Tunable) Integer [1, Inf): Number of shared Gated Linear Units (GLU) at each
#' step of the encoder.
#' @param num_independent_decoder (Tunable) Integer [1, Inf): Number of independent GLU layers for
#' pretraining.
#' @param num_shared_decoder (Tunable) Integer [1, Inf): Number of shared GLU layers for
#' pretraining.
#' @param momentum (Tunable) Numeric [0, Inf): Momentum.
#' @param pretraining_ratio (Tunable) Numeric \[0, 1\]: Pretraining ratio.
#' @param device Character \{"auto", "cpu", "cuda"\}: Compute device.
#' @param importance_sample_size (Tunable) Optional Integer [1, Inf): Importance sample size.
#' @param early_stopping_monitor (Tunable) Character \{"auto", "valid_loss", "train_loss"\}: Early stopping monitor.
#' @param early_stopping_tolerance (Tunable) Numeric [0, Inf): Minimum relative improvement to reset the patience
#' counter.
#' @param early_stopping_patience (Tunable) Integer [0, Inf): Number of epochs without improving before
#' stopping.
#' @param num_workers Integer [0, Inf): Number of subprocesses for data loading.
#' @param skip_importance Logical: If TRUE, skip importance calculation.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return TabNetHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' tabnet_hyperparams <- setup_TabNet(epochs = 100L, learn_rate = 0.01)
#' tabnet_hyperparams
setup_TabNet <- function(
  batch_size = 500L,
  penalty = 0.001,
  clip_value = NULL,
  loss = "auto",
  epochs = 50L,
  drop_last = FALSE,
  decision_width = NULL,
  attention_width = NULL,
  num_steps = 3L,
  feature_reusage = 1.3,
  mask_type = "sparsemax",
  virtual_batch_size = 65536L, # 256^2
  valid_split = 0,
  learn_rate = 0.02,
  optimizer = "adam",
  lr_scheduler = NULL,
  lr_decay = 0.1,
  step_size = 30L,
  checkpoint_epochs = 10L,
  cat_emb_dim = 1L,
  num_independent = 2L,
  num_shared = 2L,
  num_independent_decoder = 1L,
  num_shared_decoder = 1L,
  momentum = 0.02,
  pretraining_ratio = 0.5,
  device = "auto",
  importance_sample_size = NULL,
  early_stopping_monitor = "auto",
  early_stopping_tolerance = 0,
  early_stopping_patience = 0L,
  num_workers = 0L,
  skip_importance = FALSE,
  ifw = FALSE
) {
  batch_size <- clean_posint(batch_size)
  epochs <- clean_posint(epochs)
  decision_width <- clean_posint(decision_width)
  attention_width <- clean_posint(attention_width)
  num_steps <- clean_posint(num_steps)
  virtual_batch_size <- clean_posint(virtual_batch_size)
  step_size <- clean_posint(step_size)
  checkpoint_epochs <- clean_posint(checkpoint_epochs)
  cat_emb_dim <- clean_posint(cat_emb_dim)
  num_independent <- clean_posint(num_independent)
  num_shared <- clean_posint(num_shared)
  num_independent_decoder <- clean_posint(num_independent_decoder)
  num_shared_decoder <- clean_posint(num_shared_decoder)
  importance_sample_size <- clean_posint(importance_sample_size)
  early_stopping_patience <- clean_int(early_stopping_patience)
  num_workers <- clean_int(num_workers)
  TabNetHyperparameters(
    batch_size = batch_size,
    penalty = penalty,
    clip_value = clip_value,
    loss = loss,
    epochs = epochs,
    drop_last = drop_last,
    decision_width = decision_width,
    attention_width = attention_width,
    num_steps = num_steps,
    feature_reusage = feature_reusage,
    mask_type = mask_type,
    virtual_batch_size = virtual_batch_size,
    valid_split = valid_split,
    learn_rate = learn_rate,
    optimizer = optimizer,
    lr_scheduler = lr_scheduler,
    lr_decay = lr_decay,
    step_size = step_size,
    checkpoint_epochs = checkpoint_epochs,
    cat_emb_dim = cat_emb_dim,
    num_independent = num_independent,
    num_shared = num_shared,
    num_independent_decoder = num_independent_decoder,
    num_shared_decoder = num_shared_decoder,
    momentum = momentum,
    pretraining_ratio = pretraining_ratio,
    device = device,
    importance_sample_size = importance_sample_size,
    early_stopping_monitor = early_stopping_monitor,
    early_stopping_tolerance = early_stopping_tolerance,
    early_stopping_patience = early_stopping_patience,
    num_workers = num_workers,
    skip_importance = skip_importance,
    ifw = ifw
  )
} # /setup_TabNet

get_tabnet_config <- function(hyperparameters) {
  check_is_S7(hyperparameters, TabNetHyperparameters)
  hpr <- hyperparameters@hyperparameters
  hpr[["ifw"]] <- NULL
  do.call(tabnet::tabnet_config, hpr)
} # /get_tabnet_config


# %% RangerHyperparameters ----
#' @title RangerHyperparameters
#'
#' @description
#' Hyperparameters subclass for Ranger Random Forest. `split_select_weights`
#' (numeric vector or list of vectors), `respect_unordered_factors`
#' (character or logical), and `inbag` (list) have union / list types the
#' `prop_*` factories do not express, so they are declared as plain S7 union
#' properties (non-tunable) and their JSON Schema is supplied by hand via
#' @author EDG
#' @keywords internal
#' @noRd
RangerHyperparameters <- new_class(
  name = "RangerHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("Ranger"),
    num_trees = prop_integer(
      500L,
      min = 1L,
      tunable = TRUE,
      description = "Number of trees."
    ),
    mtry = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      data_bound = "n_features",
      description = "Number of features considered at each split. NULL = ranger default."
    ),
    importance = prop_string(
      "impurity",
      enum = c("none", "impurity", "impurity_corrected", "permutation"),
      description = "Variable importance mode."
    ),
    write_forest = prop_boolean(
      TRUE,
      description = "Save the forest object (required for prediction)."
    ),
    probability = prop_boolean(
      FALSE,
      description = "Grow a probability forest (classification only)."
    ),
    min_node_size = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Minimal node size. NULL = ranger default by task type."
    ),
    min_bucket = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Minimal number of samples in a terminal node (survival only)."
    ),
    max_depth = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Maximal tree depth. NULL or 0 = unlimited."
    ),
    replace = prop_boolean(
      TRUE,
      tunable = TRUE,
      description = "Sample with replacement."
    ),
    sample_fraction = prop_float(
      1,
      exclusive_min = 0,
      max = 1,
      tunable = TRUE,
      description = "Fraction of observations to sample per tree."
    ),
    case_weights = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_cases",
      description = "Per-observation sampling weights."
    ),
    class_weights = prop_float(
      NULL,
      min = 0,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_classes",
      description = "Per-class weights (classification only)."
    ),
    splitrule = prop_string(
      NULL,
      nullable = TRUE,
      tunable = TRUE,
      description = "Splitting rule (task-dependent). NULL = ranger default."
    ),
    num_random_splits = prop_integer(
      1L,
      min = 1L,
      tunable = TRUE,
      description = "Random splits per candidate variable (extratrees splitrule)."
    ),
    alpha = prop_float(
      0.5,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Significance threshold to allow splitting (maxstat splitrule)."
    ),
    minprop = prop_float(
      0.1,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Lower quantile of covariate distribution considered for splitting (maxstat splitrule)."
    ),
    poisson_tau = prop_float(
      1,
      exclusive_min = 0,
      description = "Tau parameter (poisson splitrule)."
    ),
    split_select_weights = prop_array(
      prop_float(0, min = 0, max = 1, vector = TRUE, data_bound = "n_features"),
      nullable = TRUE,
      broadcast = TRUE,
      description = "Per-feature split-selection probabilities: one vector applied to every tree, or one vector per tree."
    ),
    always_split_variables = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "feature_names",
      description = "Variables always included as split candidates."
    ),
    respect_unordered_factors = prop_string(
      NULL,
      enum = c("partition", "ignore", "order"),
      nullable = TRUE,
      description = "Handling of unordered factors. NULL uses the ranger default."
    ),
    scale_permutation_importance = prop_boolean(
      FALSE,
      description = "Scale permutation importance by its standard error."
    ),
    local_importance = prop_boolean(
      FALSE,
      description = "Compute local (per-observation) permutation importance."
    ),
    regularization_factor = prop_float(
      1,
      min = 0,
      tunable = TRUE,
      description = "Regularization factor penalizing variables with many split points."
    ),
    regularization_usedepth = prop_boolean(
      FALSE,
      description = "Apply the regularization factor with node depth."
    ),
    keep_inbag = prop_boolean(
      FALSE,
      description = "Record how often each observation is in-bag per tree."
    ),
    inbag = prop_array(
      prop_integer(0L, min = 0L, vector = TRUE, data_bound = "n_cases"),
      nullable = TRUE,
      description = "Manually set in-bag counts: one per-case count vector per tree."
    ),
    holdout = prop_boolean(
      FALSE,
      description = "Hold-out mode: hold out samples with case weight 0."
    ),
    quantreg = prop_boolean(
      FALSE,
      description = "Prepare quantile prediction (regression only)."
    ),
    time_interest = prop_float(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Time points of interest for survival prediction."
    ),
    oob_error = prop_boolean(
      TRUE,
      description = "Compute the OOB prediction error."
    ),
    save_memory = prop_boolean(
      FALSE,
      description = "Use the memory-saving (slower) splitting mode."
    ),
    verbose = prop_boolean(
      TRUE,
      description = "Show ranger computation status."
    ),
    node_stats = prop_boolean(
      FALSE,
      description = "Save additional node statistics."
    ),
    seed = prop_integer(
      NULL,
      nullable = TRUE,
      description = "Random seed. NULL = generated from R."
    ),
    na_action = prop_string(
      "na.learn",
      enum = c("na.learn", "na.omit", "na.fail"),
      description = "How to handle missing values."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::RangerHyperparameters


# %% setup_Ranger ----
#' Setup Ranger Hyperparameters
#'
#' Setup hyperparameters for Ranger Random Forest training.
#'
#' Get more information from [ranger::ranger].
#'
#' @param num_trees (Tunable) Integer [1, Inf): Number of trees.
#' @param mtry (Tunable) Optional Integer [1, Inf): Number of features to consider at each split.
#' @param importance Character \{"none", "impurity", "impurity_corrected", "permutation"\}: Variable importance mode. "impurity" is the Gini index for classification, the response variance for regression.
#' @param write_forest Logical: If TRUE, save the forest object (required for prediction). Set to FALSE to reduce memory if no prediction is intended.
#' @param probability Logical: If TRUE, grow a probability forest. Classification only.
#' @param min_node_size (Tunable) Optional Integer [1, Inf): Minimal node size. If NULL, ranger uses 1 for classification, 5 for regression, 3 for survival, and 10 for probability.
#' @param min_bucket Optional Integer [1, Inf): Minimal number of samples in a terminal node. Survival only. Deprecated in favor of `min_node_size`.
#' @param max_depth (Tunable) Optional Integer [0, Inf): Maximal tree depth. NULL or 0 means unlimited depth, 1 means tree stumps.
#' @param replace (Tunable) Logical: If TRUE, sample with replacement.
#' @param sample_fraction (Tunable) Numeric (0, 1]: Fraction of observations to sample. Default is 1 with replacement and 0.632 without.
#' @param case_weights Optional Numeric [0, Inf) vector: Per-observation sampling weights; larger weights raise selection probability in each tree's sample.
#' @param class_weights Optional Numeric [0, Inf) vector: Per-class weights for classification. Length equal to the number of classes, named by class label.
#' @param splitrule (Tunable) Optional Character: Splitting rule. Classification: "gini", "extratrees", "hellinger"; regression: "variance", "extratrees", "maxstat", "beta"; survival: "logrank", "extratrees", "C", "maxstat".
#' @param num_random_splits (Tunable) Integer [1, Inf): Number of random splits per candidate variable, for the "extratrees" splitrule.
#' @param alpha (Tunable) Numeric \[0, 1\]: Significance threshold to allow splitting, for the "maxstat" splitrule.
#' @param minprop (Tunable) Numeric \[0, 1\]: Lower quantile of the covariate distribution considered for splitting, for the "maxstat" splitrule.
#' @param poisson_tau Numeric (0, Inf): Tau parameter, for the "poisson" regression splitrule.
#' @param split_select_weights Optional List: Per-feature probabilities of being selected for splitting, in \[0, 1\]. One vector applied to every tree, or a list of length `num_trees` with one vector per tree.
#' @param always_split_variables Optional Character vector: Names of variables to always include as split candidates, in addition to the `mtry` variables.
#' @param respect_unordered_factors Optional Character \{"partition", "ignore", "order"\}: Handling of unordered factors. "partition" considers all 2-partitions, "ignore" orders levels by first occurrence, "order" orders levels by mean response.
#' @param scale_permutation_importance Logical: If TRUE, scale permutation importance by its standard error. Permutation importance only.
#' @param local_importance Logical: If TRUE, compute local (per-observation) permutation importance.
#' @param regularization_factor (Tunable) Numeric [0, Inf): Regularization factor penalizing variables with many split points. Requires `splitrule = "variance"`.
#' @param regularization_usedepth Logical: If TRUE, apply the regularization factor with node depth. Requires `regularization_factor`.
#' @param keep_inbag Logical: If TRUE, record how often each observation is in-bag per tree.
#' @param inbag Optional List: Manually set in-bag counts; a list of length `num_trees`, each a per-case count vector. Can be used for stratified sampling.
#' @param holdout Logical: If TRUE, use hold-out mode: hold out samples with case weight 0 and use them for variable importance and prediction error.
#' @param quantreg Logical: If TRUE, prepare quantile prediction (quantile regression forests). Regression only; set `keep_inbag = TRUE` for out-of-bag quantile prediction.
#' @param time_interest Optional Numeric vector: Time points of interest for survival prediction. Survival only. Deprecated.
#' @param oob_error Logical: If TRUE, compute the OOB prediction error. Set to FALSE to save time if only the forest is needed.
#' @param save_memory Logical: If TRUE, use the memory-saving (slower) splitting mode. Use only if you encounter memory problems.
#' @param verbose Logical: If TRUE, show computation status and estimated runtime.
#' @param node_stats Logical: If TRUE, save additional node statistics (terminal nodes only).
#' @param seed Optional Integer: Random seed. If NULL, the seed is generated from R. Set to 0 to ignore the R seed.
#' @param na_action Character \{"na.learn", "na.omit", "na.fail"\}: How to handle missing values. "na.learn" uses observations with missing values in splitting, treating missing as a separate category.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return RangerHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' ranger_hyperparams <- setup_Ranger(num_trees = 1000L, ifw = FALSE)
#' ranger_hyperparams
setup_Ranger <- function(
  num_trees = 500L,
  mtry = NULL,
  importance = "impurity",
  write_forest = TRUE,
  probability = FALSE,
  min_node_size = NULL,
  min_bucket = NULL,
  max_depth = NULL,
  replace = TRUE,
  sample_fraction = ifelse(replace, 1, 0.632),
  case_weights = NULL,
  class_weights = NULL,
  splitrule = NULL,
  num_random_splits = 1L,
  alpha = 0.5,
  minprop = 0.1,
  poisson_tau = 1,
  split_select_weights = NULL,
  always_split_variables = NULL,
  respect_unordered_factors = NULL,
  scale_permutation_importance = FALSE,
  local_importance = FALSE,
  regularization_factor = 1,
  regularization_usedepth = FALSE,
  keep_inbag = FALSE,
  inbag = NULL,
  holdout = FALSE,
  quantreg = FALSE,
  time_interest = NULL,
  oob_error = TRUE,
  save_memory = FALSE,
  verbose = TRUE,
  node_stats = FALSE,
  seed = NULL,
  na_action = "na.learn",
  ifw = FALSE
) {
  num_trees <- clean_posint(num_trees)
  mtry <- clean_posint(mtry)
  min_node_size <- clean_posint(min_node_size)
  min_bucket <- clean_posint(min_bucket)
  max_depth <- clean_int(max_depth)
  num_random_splits <- clean_posint(num_random_splits)
  seed <- clean_int(seed)
  # One count vector per tree, cleaned element by element: a count is an
  # integer, and `list(c(1, 0, 2))` is how a user writes one.
  inbag <- clean_int(inbag)
  RangerHyperparameters(
    num_trees = num_trees,
    mtry = mtry,
    importance = importance,
    write_forest = write_forest,
    probability = probability,
    min_node_size = min_node_size,
    min_bucket = min_bucket,
    max_depth = max_depth,
    replace = replace,
    sample_fraction = sample_fraction,
    case_weights = case_weights,
    class_weights = class_weights,
    splitrule = splitrule,
    num_random_splits = num_random_splits,
    alpha = alpha,
    minprop = minprop,
    poisson_tau = poisson_tau,
    split_select_weights = split_select_weights,
    always_split_variables = always_split_variables,
    respect_unordered_factors = respect_unordered_factors,
    scale_permutation_importance = scale_permutation_importance,
    local_importance = local_importance,
    regularization_factor = regularization_factor,
    regularization_usedepth = regularization_usedepth,
    keep_inbag = keep_inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    time_interest = time_interest,
    oob_error = oob_error,
    save_memory = save_memory,
    verbose = verbose,
    node_stats = node_stats,
    seed = seed,
    na_action = na_action,
    ifw = ifw
  )
} # /setup_Ranger


# %% SPLSHyperparameters ----
#' @title SPLSHyperparameters
#'
#' @description
#' Hyperparameters subclass for SPLS.
#'
#' One class covers both backends: `spls::spls` for regression and
#' `spls::splsda` for classification. `select`, `fit`, `scale_y`, `eps` and
#' `maxstep` reach the regression backend only; `classifier` reaches the
#' classification backend only. `splsda` fixes the others internally and does
#' not forward them.
#'
#' @author EDG
#' @keywords internal
#' @noRd
SPLSHyperparameters <- new_class(
  name = "SPLSHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("SPLS"),
    k = prop_integer(
      2L,
      min = 1L,
      tunable = TRUE,
      data_bound = "n_features",
      description = "Number of latent components."
    ),
    eta = prop_float(
      0.5,
      min = 0,
      exclusive_max = 1,
      tunable = TRUE,
      description = "Sparsity threshold: higher values select fewer features."
    ),
    kappa = prop_float(
      0.5,
      min = 0,
      max = 0.5,
      tunable = TRUE,
      description = "Concavity of the surrogate direction vector problem. Used with a multivariate coded outcome, i.e. multiclass classification."
    ),
    select = prop_string(
      "pls2",
      enum = c("pls2", "simpls"),
      description = "Feature selection algorithm (regression only)."
    ),
    fit = prop_string(
      "simpls",
      enum = c("kernelpls", "widekernelpls", "simpls", "oscorespls"),
      description = "PLS algorithm used for model fitting (regression only)."
    ),
    classifier = prop_string(
      "lda",
      enum = c("lda", "logistic"),
      description = "Classifier fit on the latent components (classification only)."
    ),
    scale_x = prop_boolean(
      TRUE,
      description = "Scale features to unit variance."
    ),
    scale_y = prop_boolean(
      FALSE,
      description = "Scale the outcome to unit variance (regression only)."
    ),
    eps = prop_float(
      1e-4,
      exclusive_min = 0,
      description = "Convergence tolerance (regression only)."
    ),
    maxstep = prop_integer(
      100L,
      min = 1L,
      description = "Maximum number of iterations per component (regression only)."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::SPLSHyperparameters


# %% setup_SPLS ----
#' Setup SPLS Hyperparameters
#'
#' Setup hyperparameters for Sparse Partial Least Squares training.
#'
#' Regression is fit with [spls::spls] and classification with
#' [spls::splsda], chosen from the outcome type. Parameters marked
#' "regression only" or "classification only" are passed to the backend that
#' accepts them and ignored by the other.
#'
#' `spls` provides no case weights, so `ifw` cannot be honored: enabling it
#' makes training abort rather than silently fit an unweighted model.
#'
#' @param k (Tunable) Integer [1, Inf): Number of latent components.
#' @param eta (Tunable) Numeric [0, 1): Sparsity threshold. Higher values select fewer features.
#' @param kappa (Tunable) Numeric \[0, 0.5\]: Concavity of the surrogate direction vector problem.
#' @param select Character \{"pls2", "simpls"\}: Feature selection algorithm (regression only).
#' @param fit Character \{"kernelpls", "widekernelpls", "simpls", "oscorespls"\}: PLS algorithm used for model fitting (regression only).
#' @param classifier Character \{"lda", "logistic"\}: Classifier fit on the latent components (classification only).
#' @param scale_x Logical: If TRUE, scale features to unit variance.
#' @param scale_y Logical: If TRUE, scale the outcome to unit variance (regression only).
#' @param eps Numeric (0, Inf): Convergence tolerance (regression only).
#' @param maxstep Integer [1, Inf): Maximum number of iterations per component (regression only).
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return SPLSHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' spls_hyperparams <- setup_SPLS(k = 3L, eta = 0.7)
#' spls_hyperparams
setup_SPLS <- function(
  # tunable
  k = 2L,
  eta = 0.5,
  kappa = 0.5,
  # fixed
  select = "pls2",
  fit = "simpls",
  classifier = "lda",
  scale_x = TRUE,
  scale_y = FALSE,
  eps = 1e-4,
  maxstep = 100L,
  ifw = FALSE
) {
  k <- clean_posint(k)
  maxstep <- clean_posint(maxstep)
  SPLSHyperparameters(
    k = k,
    eta = eta,
    kappa = kappa,
    select = select,
    fit = fit,
    classifier = classifier,
    scale_x = scale_x,
    scale_y = scale_y,
    eps = eps,
    maxstep = maxstep,
    ifw = ifw
  )
} # /rtemis::setup_SPLS


# %% KNNHyperparameters ----
#' @title KNNHyperparameters
#'
#' @description
#' Hyperparameters subclass for KNN.
#'
#' One class covers both outcome types: `kknn::train.kknn` fits a regression or
#' a classification model depending on the outcome, and every property reaches
#' it in either case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
KNNHyperparameters <- new_class(
  name = "KNNHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("KNN"),
    k = prop_integer(
      7L,
      min = 1L,
      tunable = TRUE,
      description = "Number of neighbors. Must be less than the number of training cases."
    ),
    kernel = prop_string(
      "optimal",
      enum = c(
        "rectangular",
        "triangular",
        "epanechnikov",
        "biweight",
        "triweight",
        "cos",
        "inv",
        "gaussian",
        "rank",
        "optimal"
      ),
      tunable = TRUE,
      description = "Kernel used to weight neighbors by distance; \"rectangular\" gives unweighted KNN."
    ),
    distance = prop_float(
      2,
      exclusive_min = 0,
      tunable = TRUE,
      description = "Parameter of the Minkowski distance: 1 is Manhattan, 2 is Euclidean."
    ),
    scale = prop_boolean(
      TRUE,
      description = "Scale features to unit variance before computing distances."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::KNNHyperparameters


# %% setup_KNN ----
#' Setup KNN Hyperparameters
#'
#' Setup hyperparameters for k-Nearest Neighbors training.
#'
#' Both outcome types are fit with [kknn::train.kknn], which selects between
#' regression and classification from the outcome. Factors are one-hot encoded
#' by rtemis first, as an algorithm-internal preprocessor that is re-applied at
#' predict time: the backend's own factor handling resolves a contrast function
#' off the search path, which is not reachable when `kknn` is loaded but not
#' attached, as a suggested package is.
#'
#' `kknn` provides no case weights, so `ifw` cannot be honored: enabling it
#' makes training abort rather than silently fit an unweighted model.
#'
#' @param k (Tunable) Integer [1, Inf): Number of neighbors. Must be less than the number of training cases.
#' @param kernel (Tunable) Character \{"rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"\}: Kernel used to weight neighbors by distance.
#' @param distance (Tunable) Numeric (0, Inf): Parameter of the Minkowski distance.
#' @param scale Logical: If TRUE, scale features to unit variance before computing distances.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return KNNHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' knn_hyperparams <- setup_KNN(k = 11L, kernel = "rectangular")
#' knn_hyperparams
setup_KNN <- function(
  # tunable
  k = 7L,
  kernel = "optimal",
  distance = 2,
  # fixed
  scale = TRUE,
  ifw = FALSE
) {
  k <- clean_posint(k)
  KNNHyperparameters(
    k = k,
    kernel = kernel,
    distance = distance,
    scale = scale,
    ifw = ifw
  )
} # /rtemis::setup_KNN


# %% BARTHyperparameters ----
#' @title BARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for Bayesian Additive Regression Trees.
#'
#' One class covers both outcome types: a continuous outcome is sampled with an
#' identity link and a binary outcome with the link named by `link`, which is
#' therefore the only classification-only property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
BARTHyperparameters <- new_class(
  name = "BARTHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("BART"),
    num_trees = prop_integer(
      200L,
      min = 1L,
      tunable = TRUE,
      description = "Number of trees in the mean forest."
    ),
    alpha = prop_float(
      0.95,
      exclusive_min = 0,
      exclusive_max = 1,
      tunable = TRUE,
      description = "Base of the tree split prior alpha * (1 + depth)^-beta."
    ),
    beta = prop_float(
      2,
      min = 0,
      tunable = TRUE,
      description = "Depth penalty exponent of the tree split prior alpha * (1 + depth)^-beta."
    ),
    min_samples_leaf = prop_integer(
      5L,
      min = 1L,
      tunable = TRUE,
      data_bound = "n_cases",
      description = "Minimum number of training cases in a leaf."
    ),
    max_depth = prop_integer(
      10L,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      description = "Maximum depth of any tree. NULL imposes no limit."
    ),
    num_features_subsample = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      tunable = TRUE,
      data_bound = "n_features",
      description = "Number of features subsampled when growing each tree. NULL uses every feature."
    ),
    variance_forest_num_trees = prop_integer(
      0L,
      min = 0L,
      tunable = TRUE,
      description = "Number of trees in the conditional variance forest. 0 fits a homoskedastic model."
    ),
    num_gfr = prop_integer(
      5L,
      min = 0L,
      description = "Number of grow-from-root warm-start iterations."
    ),
    num_burnin = prop_integer(
      0L,
      min = 0L,
      description = "Number of burn-in MCMC iterations."
    ),
    num_mcmc = prop_integer(
      100L,
      min = 1L,
      description = "Number of retained MCMC iterations per chain."
    ),
    num_chains = prop_integer(
      1L,
      min = 1L,
      description = "Number of independent MCMC chains. Cannot exceed num_gfr unless num_gfr is 0."
    ),
    keep_every = prop_integer(
      1L,
      min = 1L,
      description = "Thinning interval: retain one MCMC sample in every keep_every."
    ),
    cutpoint_grid_size = prop_integer(
      100L,
      min = 1L,
      description = "Maximum number of candidate cutpoints considered by the grow-from-root algorithm."
    ),
    standardize = prop_boolean(
      TRUE,
      description = "Center and scale the outcome before sampling."
    ),
    link = prop_string(
      "probit",
      enum = c("probit", "cloglog"),
      description = "Link function of the binary outcome model (classification only). \"cloglog\" cannot be combined with case weights."
    ),
    seed = prop_integer(
      NULL,
      nullable = TRUE,
      description = "Random seed for the sampler. NULL leaves the sampler seeded by the system."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  ),
  validator = function(self) {
    # Each MCMC chain is seeded from its own grow-from-root ensemble, so there
    # must be at least as many of those as there are chains. num_gfr = 0 runs
    # every chain from root instead and lifts the requirement.
    if (any(self@num_gfr > 0L) && any(self@num_chains > self@num_gfr)) {
      "@num_chains cannot exceed @num_gfr when @num_gfr is greater than 0."
    }
  }
) # /rtemis::BARTHyperparameters


# %% setup_BART ----
#' Setup BART Hyperparameters
#'
#' Setup hyperparameters for Bayesian Additive Regression Trees training.
#'
#' Both outcome types are fit with [stochtree::bart], which samples a sum-of-trees
#' model by MCMC, optionally warm-started by the grow-from-root algorithm.
#' Regression uses a continuous outcome model, binary classification a discrete
#' one with the link named by `link`. Multiclass classification is not supported.
#'
#' Factors are expanded by the backend, so no encoding is needed beforehand.
#' Case weights scale the residual variance and are honored under the default
#' `link = "probit"`, but `stochtree` rejects them under `"cloglog"`, so that
#' combination makes training abort rather than silently fit an unweighted model.
#'
#' Because the fit is a posterior rather than a point estimate, `se()` returns
#' the standard deviation of the retained draws, and `get_varimp()` reports two
#' measures rather than one:
#' - `importance`: the variable inclusion proportion, the share of splitting
#'   rules that use the feature, averaged across draws.
#' - `inclusion_sd`: its standard deviation across draws, separating a feature
#'   the sampler uses consistently from one whose importance rests on a few
#'   draws.
#'
#' `plot_varimp(mod, measure = "inclusion_sd")` plots the second; the first is
#' the default.
#'
#' Inclusion proportions only discriminate when trees compete for splits. At
#' the default `num_trees` each tree is a weak learner and uninformative
#' features still get used, so the proportions flatten towards `1/n_features`;
#' refit with a small ensemble (`num_trees = 10L` to `20L`) when the goal is
#' variable selection rather than prediction.
#'
#' @param num_trees (Tunable) Integer [1, Inf): Number of trees in the mean forest.
#' @param alpha (Tunable) Numeric (0, 1): Base of the tree split prior `alpha * (1 + depth)^-beta`.
#' @param beta (Tunable) Numeric [0, Inf): Depth penalty exponent of the tree split prior `alpha * (1 + depth)^-beta`.
#' @param min_samples_leaf (Tunable) Integer [1, Inf): Minimum number of training cases in a leaf.
#' @param max_depth (Tunable) Optional Integer [1, Inf): Maximum depth of any tree. NULL imposes no limit.
#' @param num_features_subsample (Tunable) Optional Integer [1, Inf): Number of features subsampled when growing each tree. NULL uses every feature.
#' @param variance_forest_num_trees (Tunable) Integer [0, Inf): Number of trees in the conditional variance forest. 0 fits a homoskedastic model, any larger value a heteroskedastic one.
#' @param num_gfr Integer [0, Inf): Number of grow-from-root warm-start iterations.
#' @param num_burnin Integer [0, Inf): Number of burn-in MCMC iterations.
#' @param num_mcmc Integer [1, Inf): Number of retained MCMC iterations per chain.
#' @param num_chains Integer [1, Inf): Number of independent MCMC chains. Cannot exceed `num_gfr` unless `num_gfr` is 0.
#' @param keep_every Integer [1, Inf): Thinning interval: retain one MCMC sample in every `keep_every`.
#' @param cutpoint_grid_size Integer [1, Inf): Maximum number of candidate cutpoints considered by the grow-from-root algorithm.
#' @param standardize Logical: If TRUE, center and scale the outcome before sampling.
#' @param link Character \{"probit", "cloglog"\}: Link function of the binary outcome model. Classification only.
#' @param seed Optional Integer: Random seed for the sampler. NULL leaves the sampler seeded by the system.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return BARTHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' bart_hyperparams <- setup_BART(num_trees = 50L, num_mcmc = 200L)
#' bart_hyperparams
setup_BART <- function(
  # tunable
  num_trees = 200L,
  alpha = 0.95,
  beta = 2,
  min_samples_leaf = 5L,
  max_depth = 10L,
  num_features_subsample = NULL,
  variance_forest_num_trees = 0L,
  # fixed
  num_gfr = 5L,
  num_burnin = 0L,
  num_mcmc = 100L,
  num_chains = 1L,
  keep_every = 1L,
  cutpoint_grid_size = 100L,
  standardize = TRUE,
  link = "probit",
  seed = NULL,
  ifw = FALSE
) {
  num_trees <- clean_posint(num_trees)
  min_samples_leaf <- clean_posint(min_samples_leaf)
  max_depth <- clean_posint(max_depth)
  num_features_subsample <- clean_posint(num_features_subsample)
  variance_forest_num_trees <- clean_int(variance_forest_num_trees)
  num_gfr <- clean_int(num_gfr)
  num_burnin <- clean_int(num_burnin)
  num_mcmc <- clean_posint(num_mcmc)
  num_chains <- clean_posint(num_chains)
  keep_every <- clean_posint(keep_every)
  cutpoint_grid_size <- clean_posint(cutpoint_grid_size)
  seed <- clean_int(seed)
  BARTHyperparameters(
    num_trees = num_trees,
    alpha = alpha,
    beta = beta,
    min_samples_leaf = min_samples_leaf,
    max_depth = max_depth,
    num_features_subsample = num_features_subsample,
    variance_forest_num_trees = variance_forest_num_trees,
    num_gfr = num_gfr,
    num_burnin = num_burnin,
    num_mcmc = num_mcmc,
    num_chains = num_chains,
    keep_every = keep_every,
    cutpoint_grid_size = cutpoint_grid_size,
    standardize = standardize,
    link = link,
    seed = seed,
    ifw = ifw
  )
} # /rtemis::setup_BART


# %% NNLSHyperparameters ----
#' @title NNLSHyperparameters
#'
#' @description
#' Hyperparameters subclass for non-negative least squares.
#'
#' @author EDG
#' @keywords internal
#' @noRd
NNLSHyperparameters <- new_class(
  name = "NNLSHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("NNLS"),
    normalize = prop_boolean(
      TRUE,
      tunable = TRUE,
      description = "Scale the coefficients to sum to 1, making the fit a convex combination of the predictors."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
) # /rtemis::NNLSHyperparameters


# %% setup_NNLS ----
#' Setup NNLS Hyperparameters
#'
#' Setup hyperparameters for non-negative least squares.
#'
#' NNLS fits `y ~ Xb` subject to `b >= 0`, with no intercept. It exists as the
#' default meta learner of the stacked meta learners ([setup_SuperLearner],
#' [setup_ModalityStacking]), where the predictors are the base learners'
#' cross-validated predictions and a non-negative, sum-to-one coefficient vector
#' is the ensemble weighting. It is a poor general-purpose learner: with no
#' intercept and a sign constraint it can only fit outcomes that the predictors
#' already span.
#'
#' With `normalize = TRUE` the coefficients are scaled to sum to 1, so the fit is
#' a convex combination of the predictors. For classification the outcome is
#' coded 0/1 on the second factor level and the fitted values are read as
#' probabilities of that level; unnormalized coefficients do not guarantee a
#' value in \[0, 1\], so predictions are clamped.
#'
#' Get more information from `nnls::nnls`.
#'
#' @param normalize (Tunable) Logical: If TRUE, scale the coefficients to sum to
#' 1.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in
#' classification.
#'
#' @return NNLSHyperparameters object.
#'
#' @author EDG
#' @export
#' @examples
#' nnls_hyperparams <- setup_NNLS(normalize = FALSE)
#' nnls_hyperparams
setup_NNLS <- function(normalize = TRUE, ifw = FALSE) {
  NNLSHyperparameters(normalize = normalize, ifw = ifw)
} # /rtemis::setup_NNLS


# %% .list_to_Hyperparameters ----
#' Convert a list to a Hyperparameters object
#'
#' Internal function used by `rtemis.server` to reconstruct a `Hyperparameters`
#' object from a wire-format list. Not intended for direct use by end users.
#'
#' @param x Named list with two elements:
#'   \describe{
#'     \item{`algorithm`}{Character: algorithm name, e.g. `"GLM"`, `"RF"`.}
#'     \item{`hyperparameters`}{Named list of hyperparameter name-value pairs
#'       passed to the corresponding `setup_<algorithm>()` function.}
#'   }
#'
#' @return A `Hyperparameters` object as returned by `setup_<algorithm>()`.
#'
#' @author EDG
#' @keywords internal
#' @export
#' @examples
#' .list_to_Hyperparameters(list(algorithm = "GLMNET", hyperparameters = list(alpha = 1)))
.list_to_Hyperparameters <- function(x) {
  algorithm <- x[["algorithm"]]
  fn <- paste0("setup_", algorithm)
  if (!exists(fn, mode = "function")) {
    # The published enum lists canonical names only, so a case mismatch is the
    # likely error and is worth naming.
    canonical <- supervised_algorithms[["name"]][
      tolower(algorithm) == tolower(supervised_algorithms[["name"]])
    ]
    rtemis.core::abort(
      "Invalid algorithm: ",
      algorithm,
      if (length(canonical) == 1L) {
        paste0(". Did you mean \"", canonical, "\"?")
      } else {
        "."
      },
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  check_wire_keys(x, c("algorithm", "hyperparameters"), "hyperparameters")
  args <- from_wire(
    .drop_meta_keys(x[["hyperparameters"]]),
    get(paste0(algorithm, "Hyperparameters"))
  )
  check_wire_keys(
    args,
    names(formals(get(fn))),
    paste(algorithm, "hyperparameter")
  )
  do.call(fn, args)
}
