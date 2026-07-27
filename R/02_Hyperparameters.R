# 02_Hyperparameters.R
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
# `prop_*` factories (00_Props.R): one declaration per hyperparameter carries
# the type, default, bounds, enum, tunability, and description, from which S7
# validators, the tunable/fixed name vectors, the `hyperparameters` list, and
# the JSON Schema (S7_to_JSONSchema) are all generated. Parameters whose R
# type cannot be expressed as a JSON type (e.g. character-or-function) are
# declared as plain S7 properties: still stored and validated by class, but
# non-tunable and excluded from schemas via `exclude`.
#
# The abstract `Hyperparameters` superclass provides computed properties:
# - `hyperparameters`: assembles the named list from the subclass's own
#   properties plus `hp_constants()` (getter), and routes assignments back to
#   the properties, where they are validated (setter). Changing a constant
#   is an error.
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
#' All properties declared by the subclass itself — factory-built or plain —
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


# %% hp_constants ----
#' Unsettable hyperparameter constants of a Hyperparameters object
#'
#' Named list of algorithm parameters that are fixed by the algorithm
#' definition itself (e.g. `boosting_type = "rf"` for LightRF,
#' `kernel = "linear"` for LinearSVM). They are appended to the
#' `hyperparameters` list (so training backends receive them) but are not
#' properties, cannot be changed, and are excluded from generated schemas.
#'
#' @param x `Hyperparameters` object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hp_constants <- new_generic("hp_constants", "x")

method(hp_constants, class_any) <- function(x) {
  list()
} # /rtemis::hp_constants.default


# %% tune_on_null ----
#' Hyperparameters that need tuning when unset
#'
#' Names of hyperparameters whose NULL (unset) value means "determine by
#' tuning": GLMNET's `lambda` (cv.glmnet) and LightGBM's `nrounds` (early
#' stopping). `nullable + tunable` alone does not imply NULL => tune, so
#' this is declared per class.
#'
#' @param x `Hyperparameters` object.
#'
#' @return Character vector of property names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tune_on_null <- new_generic("tune_on_null", "x")

method(tune_on_null, class_any) <- function(x) {
  character()
} # /rtemis::tune_on_null.default


# %% resolve_data_bounds ----
#' Resolve training-data dimensions referenced by `data_bound` declarations
#'
#' @param x tabular data: Training data.
#'
#' @return Named list with elements "n_features", "n_cases", "n_classes",
#'   "feature_names". `n_classes` is NULL for regression, where the outcome has
#'   no levels.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_data_bounds <- function(x) {
  feats <- features(x)
  y <- outcome(x)
  list(
    n_features = NCOL(feats),
    n_cases = NROW(x),
    n_classes = if (is.factor(y)) nlevels(y) else NULL,
    feature_names = names(feats)
  )
} # /rtemis::resolve_data_bounds


# %% check_data_bounds ----
#' Check all `data_bound` hyperparameters against the training data
#'
#' Engine behind the default `validate_hyperparameters()` method. Walks the
#' hyperparameter class's properties, and for each one declaring a `data_bound`
#' (see `DATA_BOUNDS` in 00_Props.R) checks the current value against the
#' resolved dimension:
#'
#' - scalar property: every value must be `<=` the dimension. Tunable
#'   hyperparameters hold their whole search space here, hence `any()`.
#' - `vector` property: `length(value)` must equal the dimension.
#' - "feature_names": values must name training features.
#'
#' Unset (NULL) values are skipped, as is `n_classes` in regression.
#'
#' @param hyperparameters `Hyperparameters` object.
#' @param x tabular data: Training data.
#'
#' @return `hyperparameters`, invisibly. Throws if any bound is violated.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_data_bounds <- function(hyperparameters, x) {
  bounds <- data_bound_props(S7_class(hyperparameters))
  if (length(bounds) == 0L) {
    return(invisible(hyperparameters))
  }
  dims <- resolve_data_bounds(x)
  specs <- S7_class(hyperparameters)@properties
  for (nm in names(bounds)) {
    value <- hyperparameters[[nm]]
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
    if (bound == "feature_names") {
      unknown <- setdiff(value, dim_value)
      if (length(unknown) > 0L) {
        rtemis.core::abort(
          "`",
          nm,
          "` must name training features; not found: ",
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
  invisible(hyperparameters)
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
#'   the subclass's properties plus `hp_constants()`; assignment routes back
#'   to the properties and validates).
#' @field tunable_hyperparameters Character: Names of tunable hyperparameters
#'   (derived from specs).
#' @field fixed_hyperparameters Character: Names of fixed hyperparameters
#'   (derived from specs, plus constants).
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
        c(hp_prop_values(self), hp_constants(self))
      },
      setter = function(self, value) {
        route_config_assignment(
          self,
          Hyperparameters,
          value,
          constants = hp_constants(self),
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
        c(
          setdiff(hp_prop_names(cls), tunable_spec_names(cls)),
          names(hp_constants(self))
        )
      }
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
  out <- paste0(
    out,
    repr_ls(
      props(x)[c(
        "hyperparameters",
        "tunable_hyperparameters",
        "fixed_hyperparameters",
        "tuned",
        "resampled",
        "n_workers"
      )],
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
# A serialized config is `{algorithm, hyperparameters}` — exactly what
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
#' values), or any `tune_on_null()` hyperparameter that is unset, means
#' "needs tuning".
#'
#' @keywords internal
#' @noRd
method(get_tuned_status, Hyperparameters) <- function(x) {
  tunable <- x@tunable_hyperparameters
  if (length(tunable) == 0L) {
    return(TUNED_STATUS_NOT_TUNABLE)
  }
  values <- x@hyperparameters
  if (any(lengths(values[tunable]) > 1L)) {
    return(TUNED_STATUS_UNTUNED)
  }
  null_tune <- vapply(
    tune_on_null(x),
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
#' Shared engine for the `update()` methods. Values are assigned to the
#' corresponding properties (validated). The literal string "null" is the
#' grid sentinel produced by `expand_grid()` for NULL search entries and is
#' converted back to NULL.
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
    if (identical(value, "null")) {
      value <- NULL
    }
    prop(object, hp) <- value
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


# %% get_hyperparams_need_tuning.Hyperparameters ----
#' Get hyperparameters that need tuning.
#'
#' Tunable hyperparameters with more than one value (search values), plus
#' any `tune_on_null()` hyperparameter that is unset (as a NULL entry, which
#' `expand_grid()` converts to its "null" sentinel).
#'
#' @keywords internal
#' @noRd
method(get_hyperparams_need_tuning, Hyperparameters) <- function(x) {
  # -> list
  values <- x@hyperparameters
  tunable <- x@tunable_hyperparameters
  out <- values[tunable[lengths(values[tunable]) > 1L]]
  for (nm in tune_on_null(x)) {
    if (is.null(values[[nm]])) {
      out <- c(out, stats::setNames(list(NULL), nm))
    }
  }
  out
} # /get_hyperparams_need_tuning.Hyperparameters


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


# %% GLMNETHyperparameters ----
#' @title GLMNETHyperparameters
#'
#' @description
#' Hyperparameters subclass for GLMNET. `lambda.min` and `lambda.1se` are
#' runtime state written by the Tuner (from cv.glmnet), not settable
#' hyperparameters — exclude them from schema generation.
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
    `lambda.min` = prop_state(NULL | class_double),
    `lambda.1se` = prop_state(NULL | class_double)
  )
) # /rtemis::GLMNETHyperparameters

method(tune_on_null, GLMNETHyperparameters) <- function(x) {
  "lambda"
} # /rtemis::tune_on_null.GLMNETHyperparameters


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
  properties = list(
    algorithm = prop_algorithm("LightCART"),
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
    min_data_in_leaf = prop_integer(
      20L,
      min = 1L,
      tunable = TRUE,
      description = "Minimum number of cases in a leaf."
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
      description = "LightGBM objective. NULL = set from outcome type."
    ),
    ifw = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in classification."
    )
  )
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
#' @param min_data_in_leaf (Tunable) Integer [1, Inf): Minimum number of data in a leaf.
#' @param max_cat_threshold (Tunable) Integer [1, Inf): Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Integer [1, Inf): Minimum number of observations per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
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
  min_data_in_leaf = 20L,
  max_cat_threshold = 32L,
  min_data_per_group = 100L,
  linear_tree = FALSE,
  objective = NULL,
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
    min_data_in_leaf = min_data_in_leaf,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    objective = objective,
    ifw = ifw
  )
} # /rtemis::setup_LightCART


# %% LightRFHyperparameters ----
# LightGBM parameters fixed by the RF mode: not settable and excluded from
# the generated JSON Schema; appended to the `hyperparameters` list so
# lgb.train receives them.
LightRF_constants <- list(
  boosting_type = "rf",
  learning_rate = 1, # no effect? in boosting_type 'rf', but set for clarity
  subsample_freq = 1L, # a.k.a. bagging_freq
  early_stopping_rounds = -1L
)

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
  properties = list(
    algorithm = prop_algorithm("LightRF"),
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
      0.7,
      exclusive_min = 0,
      max = 1,
      tunable = TRUE,
      description = "Fraction of features sampled per tree."
    ),
    subsample = prop_float(
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
  )
) # /rtemis::LightRFHyperparameters

method(hp_constants, LightRFHyperparameters) <- function(x) {
  LightRF_constants
} # /rtemis::hp_constants.LightRFHyperparameters


# %% setup_LightRF ----
#' Setup LightRF Hyperparameters
#'
#' Setup hyperparameters for LightRF training.
#'
#' Get more information from [lightgbm::lgb.train].
#' Note that hyperparameters subsample_freq and early_stopping_rounds are fixed,
#' and cannot be set because they are what makes `lightgbm` train a random forest.
#' These can all be set when training gradient boosting with LightGBM.
#'
#' @param nrounds (Tunable) Integer [1, Inf): Number of boosting rounds.
#' @param num_leaves (Tunable) Integer [1, Inf): Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees. -1 = no limit.
#' @param feature_fraction (Tunable) Numeric (0, 1]: Fraction of features to use.
#' @param subsample (Tunable) Numeric (0, 1]: Fraction of data to use.
#' @param lambda_l1 (Tunable) Numeric [0, Inf): L1 regularization.
#' @param lambda_l2 (Tunable) Numeric [0, Inf): L2 regularization.
#' @param max_cat_threshold (Tunable) Integer [1, Inf): Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Integer [1, Inf): Minimum number of observations per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
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
  feature_fraction = 0.7,
  subsample = .623, # a.k.a. bagging_fraction
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  ifw = FALSE,
  # fixed
  objective = NULL,
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
    subsample = subsample,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective,
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
#' runtime state written by the Tuner — exclude both from schema generation.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightGBMHyperparameters <- new_class(
  name = "LightGBMHyperparameters",
  parent = Hyperparameters,
  properties = list(
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
    subsample = prop_float(
      1.0,
      exclusive_min = 0,
      max = 1,
      tunable = TRUE,
      description = "Fraction of cases sampled per tree (bagging fraction)."
    ),
    subsample_freq = prop_integer(
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
    nrounds = prop_state(NULL | class_integer),
    # Run state: best iteration, written by the Tuner.
    best_iter = prop_state(NULL | class_numeric)
  )
) # /rtemis::LightGBMHyperparameters

method(tune_on_null, LightGBMHyperparameters) <- function(x) {
  "nrounds"
} # /rtemis::tune_on_null.LightGBMHyperparameters

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
#' @param subsample (Tunable) Numeric (0, 1]: Fraction of data to use.
#' @param subsample_freq (Tunable) Integer [1, Inf): Frequency of subsample.
#' @param lambda_l1 (Tunable) Numeric [0, Inf): L1 regularization.
#' @param lambda_l2 (Tunable) Numeric [0, Inf): L2 regularization.
#' @param max_cat_threshold (Tunable) Integer [1, Inf): Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Integer [1, Inf): Minimum number of observations per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
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
#'   learning_rate = c(0.001, 0.01, 0.05), ifw = TRUE
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
  subsample = 1.0, # a.k.a. bagging_fraction
  subsample_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  ifw = FALSE,
  objective = NULL,
  device_type = "cpu",
  tree_learner = "serial",
  force_col_wise = TRUE
) {
  max_nrounds <- clean_posint(max_nrounds)
  force_nrounds <- clean_posint(force_nrounds)
  early_stopping_rounds <- clean_posint(early_stopping_rounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  subsample_freq <- clean_posint(subsample_freq)
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
    subsample = subsample,
    subsample_freq = subsample_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective,
    device_type = device_type,
    tree_learner = tree_learner,
    force_col_wise = force_col_wise,
    nrounds = force_nrounds
  )
} # /rtemis::setup_LightGBM


# %% LightRuleFitHyperparameters ----
# Names of hyperparameters forwarded to each step's setup function by
# train_LightRuleFit.
LightRuleFit_lightgbm_params <- c(
  "nrounds",
  "num_leaves",
  "max_depth",
  "learning_rate",
  "subsample",
  "subsample_freq",
  "lambda_l1",
  "lambda_l2",
  "objective"
)
LightRuleFit_glmnet_params <- c("alpha", "lambda")

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
  properties = list(
    algorithm = prop_algorithm("LightRuleFit"),
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
    subsample = prop_float(
      0.666,
      exclusive_min = 0,
      max = 1,
      tunable = TRUE,
      description = "Fraction of cases sampled per tree (LightGBM step)."
    ),
    subsample_freq = prop_integer(
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
      description = "LightGBM objective. NULL = set from outcome type."
    ),
    ifw_lightgbm = prop_boolean(
      FALSE,
      tunable = TRUE,
      description = "Inverse Frequency Weighting in the LightGBM step."
    ),
    alpha = prop_float(
      1,
      min = 0,
      max = 1,
      tunable = TRUE,
      description = "Elastic net mixing parameter (GLMNET step)."
    ),
    lambda = prop_float(
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
  validator = function(self) {
    if (any(self@ifw) && (any(self@ifw_lightgbm) || any(self@ifw_glmnet))) {
      "@ifw cannot be combined with @ifw_lightgbm or @ifw_glmnet."
    }
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
#' @param subsample (Tunable) Numeric (0, 1]: Fraction of data to use.
#' @param subsample_freq (Tunable) Integer [1, Inf): Frequency of subsample.
#' @param lambda_l1 (Tunable) Numeric [0, Inf): L1 regularization.
#' @param lambda_l2 (Tunable) Numeric [0, Inf): L2 regularization.
#' @param objective Optional Character: Objective function. NULL = set from outcome type.
#' @param ifw_lightgbm (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in the LightGBM
#' step.
#' @param alpha (Tunable) Numeric \[0, 1\]: Alpha for GLMNET.
#' @param lambda Optional Numeric [0, Inf) vector: Lambda for GLMNET. NULL = determined by cv.glmnet.
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
  subsample = 0.666,
  subsample_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  objective = NULL,
  ifw_lightgbm = FALSE,
  alpha = 1,
  lambda = NULL,
  ifw_glmnet = FALSE,
  ifw = FALSE
) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  subsample_freq <- clean_posint(subsample_freq)
  LightRuleFitHyperparameters(
    nrounds = nrounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    subsample = subsample,
    subsample_freq = subsample_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    objective = objective,
    ifw_lightgbm = ifw_lightgbm,
    alpha = alpha,
    lambda = lambda,
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
#' constant (see `hp_constants`).
#'
#' @author EDG
#' @keywords internal
#' @noRd
LinearSVMHyperparameters <- new_class(
  name = "LinearSVMHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("LinearSVM"),
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

method(hp_constants, LinearSVMHyperparameters) <- function(x) {
  list(kernel = "linear")
} # /rtemis::hp_constants.LinearSVMHyperparameters


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
#' constant (see `hp_constants`).
#'
#' @author EDG
#' @keywords internal
#' @noRd
RadialSVMHyperparameters <- new_class(
  name = "RadialSVMHyperparameters",
  parent = Hyperparameters,
  properties = list(
    algorithm = prop_algorithm("RadialSVM"),
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

method(hp_constants, RadialSVMHyperparameters) <- function(x) {
  list(kernel = "radial")
} # /rtemis::hp_constants.RadialSVMHyperparameters


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
      1048576L,
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
    # Accept an R function or a name; only the name has a JSON form, supplied
    # by `.tabnet_hyperparameters_schema_extra`.
    optimizer = prop_external(
      class_character | class_function,
      default = "adam"
    ),
    lr_scheduler = prop_external(NULL | class_character | class_function),
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


# %% .tabnet_hyperparameters_schema_extra ----
# Schema fragments for TabNetHyperparameters `optimizer` and `lr_scheduler`.
# Both accept a torch function in R (not JSON-serializable) or a string; only
# the string form is schematized, so a portable config validates while the
# function form remains an R-only runtime option. Merged into the generated
# schema. See generate_schemas.R.
.tabnet_hyperparameters_schema_extra <- list(
  properties = list(
    optimizer = list(
      type = "string",
      default = "adam",
      `$comment` = "A torch optimizer function may also be supplied in R; only the string form is serializable.",
      description = "Optimizer name (e.g. \"adam\")."
    ),
    lr_scheduler = list(
      oneOf = list(
        list(type = "null"),
        list(type = "string", enum = I(c("step", "reduce_on_plateau")))
      ),
      `$comment` = "A torch scheduler function may also be supplied in R; only the string form is serializable.",
      description = "Learning-rate scheduler: \"step\" or \"reduce_on_plateau\". null = none."
    )
  )
)


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
#' @param optimizer Character or torch function: Optimizer.
#' @param lr_scheduler Optional Character or torch function: "step", "reduce_on_plateau".
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
  batch_size = 1048576L, # 1024^2
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
#' `.ranger_hyperparameters_schema_extra`, merged into the generated
#' `hyperparameters/ranger/v1` schema in `generate_schemas.R`.
#'
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
    split_select_weights = prop_external(NULL | class_numeric | class_list),
    always_split_variables = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "feature_names",
      description = "Variables always included as split candidates."
    ),
    respect_unordered_factors = prop_external(
      NULL | class_character | class_logical
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
    inbag = prop_external(NULL | class_list),
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


# %% .ranger_hyperparameters_schema_extra ----
# Schema fragments for RangerHyperparameters properties whose R types are union
# / list types not expressible via the prop_* factories. They are `exclude`d
# from generation and their JSON Schema merged in here, so the generated
# `hyperparameters/ranger/v1` schema still describes them. See generate_schemas.R.
.ranger_hyperparameters_schema_extra <- list(
  properties = list(
    split_select_weights = list(
      oneOf = list(
        list(type = "null"),
        list(
          type = "array",
          items = list(type = "number", minimum = 0, maximum = 1),
          minItems = 1L
        ),
        list(
          type = "array",
          items = list(
            type = "array",
            items = list(type = "number", minimum = 0, maximum = 1),
            minItems = 1L
          ),
          minItems = 1L
        )
      ),
      description = paste0(
        "Optional per-feature split-selection probabilities in [0, 1]: a ",
        "single vector applied to every tree, or a list of length `num_trees` ",
        "with one weight vector per tree. null uses the ranger default."
      )
    ),
    respect_unordered_factors = list(
      oneOf = list(
        list(type = "null"),
        list(type = "string", enum = I(c("partition", "ignore", "order"))),
        list(type = "boolean")
      ),
      description = paste0(
        "Handling of unordered factors: \"partition\", \"ignore\", or ",
        "\"order\"; or a logical (TRUE corresponds to \"partition\"). null ",
        "uses the ranger default."
      )
    ),
    inbag = list(
      oneOf = list(
        list(type = "null"),
        list(
          type = "array",
          items = list(
            type = "array",
            items = list(type = "integer", minimum = 0L)
          ),
          minItems = 1L
        )
      ),
      description = paste0(
        "Optional manually-set in-bag counts: a list of length `num_trees`, ",
        "each a per-observation vector of non-negative counts. null uses the ",
        "ranger default (bootstrap sampling)."
      )
    )
  )
)


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
#' @param split_select_weights Optional Numeric \[0, 1\] vector: Per-feature probabilities of being selected for splitting. Alternatively a list of length `num_trees`, one weight vector per tree.
#' @param always_split_variables Optional Character vector: Names of variables to always include as split candidates, in addition to the `mtry` variables.
#' @param respect_unordered_factors Optional Character or logical: Handling of unordered factors: "partition" considers all 2-partitions, "ignore" orders levels by first occurrence, "order" orders levels by mean response. TRUE corresponds to "partition".
#' @param scale_permutation_importance Logical: If TRUE, scale permutation importance by its standard error. Permutation importance only.
#' @param local_importance Logical: If TRUE, compute local (per-observation) permutation importance.
#' @param regularization_factor (Tunable) Numeric [0, Inf): Regularization factor penalizing variables with many split points. Requires `splitrule = "variance"`.
#' @param regularization_usedepth Logical: If TRUE, apply the regularization factor with node depth. Requires `regularization_factor`.
#' @param keep_inbag Logical: If TRUE, record how often each observation is in-bag per tree.
#' @param inbag Optional List: Manually set in-bag counts per tree; list of length `num_trees`. Can be used for stratified sampling.
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
  fn <- paste0("setup_", x[["algorithm"]])
  if (!exists(fn, mode = "function")) {
    rtemis.core::abort(
      "Invalid algorithm: ",
      x[["algorithm"]],
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  args <- x[["hyperparameters"]]
  # Keep only arguments that are in the setup function
  setup_formals <- names(formals(get(fn)))
  args <- args[names(args) %in% setup_formals]
  do.call(fn, args)
}
