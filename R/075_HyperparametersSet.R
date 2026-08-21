# 075_HyperparametersSet.R
# ::rtemis::
# 2026- EDG rtemis.org

# A union of search spaces. `R/005_HyperparameterCandidates.R` names the levels
# of the vocabulary this completes:
#
#   domain      what one hyperparameter may take    -- the general idea
#   candidates  a domain given by enumeration       -- HyperparameterCandidates
#   space       the product of the domains          -- implicit
#   grid        the space enumerated, one row each  -- `tuning_grid()`
#   set         a union of spaces                   -- this class
#
# `gate_tuning_grid()` already removes the combinations that are *invalid* --
# `nvmax` where `node_model` is "constant". This is for the ones that are
# perfectly valid and simply not the combinations meant: LINAD's CART point is
# `node_model = "constant"` **and** `gamma = 0` **and** `learning_rate = 1`
# **and** `line_search = "none"`, a conjunction of properties that gate nothing,
# so no product over them can express it and no gate can exclude the cells
# around it.
#
# The class is internal. A user passes a plain list of `Hyperparameters` objects
# and `train()` coerces it here, so there is no constructor to learn.

# %% HyperparametersSet ----
#' @title HyperparametersSet
#'
#' @description
#' Several `Hyperparameters` objects searched as one space: each member is
#' expanded and gated on its own, and the tuner selects across their union.
#'
#' Members share an algorithm. That is checked on `algorithm` rather than on the
#' class or on which `setup_*` produced them, so a future constructor exposing a
#' subset of one algorithm's hyperparameters would compose with this without the
#' validator knowing it exists.
#'
#' @field members List of `Hyperparameters`, named. Unnamed members are labelled
#' by position when the set is built.
#' @field algorithm Character: The algorithm every member shares.
#'
#' @author EDG
#' @keywords internal
#' @noRd
HyperparametersSet <- new_class(
  name = "HyperparametersSet",
  package = "rtemis",
  properties = list(
    members = class_list,
    algorithm = new_property(
      class_character,
      getter = function(self) {
        if (length(self@members) == 0L) {
          return(NA_character_)
        }
        self@members[[1L]]@algorithm
      }
    ),
    # `train()` writes `n_workers` and `resampled` onto the hyperparameters it
    # was handed, before it knows whether tuning will collapse a set. Both
    # write through to every member, because a member is what eventually trains
    # and the value has to be there when it does. Reading returns the first
    # member's, which is the same as every other's by construction.
    #
    # **Both setters ignore a zero-length value, and must.** S7 initializes
    # every declared property at construction, and a property with a setter has
    # that setter *called* with the class prototype -- `integer(0)` here, since
    # neither declares a default. Without the guard, building a set would write
    # `integer(0)` into each member's `n_workers` and `resampled`, and the
    # failure surfaces later and elsewhere: `train()` reads
    # `hyperparameters@resampled == 0L` and gets `argument is of length zero`.
    n_workers = new_property(
      class_integer,
      getter = function(self) {
        if (length(self@members) == 0L) {
          return(1L)
        }
        self@members[[1L]]@n_workers
      },
      setter = function(self, value) {
        if (length(value) == 0L) {
          return(self)
        }
        self@members <- lapply(self@members, function(member) {
          member@n_workers <- value
          member
        })
        self
      }
    ),
    resampled = new_property(
      class_integer,
      getter = function(self) {
        if (length(self@members) == 0L) {
          return(0L)
        }
        self@members[[1L]]@resampled
      },
      setter = function(self, value) {
        if (length(value) == 0L) {
          return(self)
        }
        self@members <- lapply(self@members, function(member) {
          member@resampled <- value
          member
        })
        self
      }
    )
  ),
  validator = function(self) {
    if (length(self@members) == 0L) {
      return("must hold at least one Hyperparameters object.")
    }
    is_hyperparameters <- vapply(
      self@members,
      function(member) S7_inherits(member, Hyperparameters),
      logical(1L)
    )
    if (!all(is_hyperparameters)) {
      return(paste0(
        "every member must be a Hyperparameters object; member ",
        which(!is_hyperparameters)[[1L]],
        " is not."
      ))
    }
    algorithms <- vapply(
      self@members,
      function(member) member@algorithm,
      character(1L)
    )
    if (length(unique(algorithms)) > 1L) {
      return(paste0(
        "every member must be for the same algorithm, not ",
        paste(unique(algorithms), collapse = " and "),
        "."
      ))
    }
    if (is.null(names(self@members)) || any(!nzchar(names(self@members)))) {
      return("every member must be named.")
    }
    NULL
  }
) # /rtemis::HyperparametersSet


# %% as_HyperparametersSet ----
#' Coerce a list of `Hyperparameters` into a set
#'
#' The boundary between what a user writes and what the package carries. A user
#' passes `list(setup_LINAD(...), setup_LINAD(...))`; everything inside sees a
#' `HyperparametersSet`.
#'
#' Coercing here rather than teaching the internals to accept a list is what
#' avoids a `class_list` method on `needs_tuning()` and its siblings. Those are
#' S7 generics, and claiming the bare `list` type for one meaning across the
#' package would reach every future caller that passes an ordinary list.
#'
#' The errors are raised here rather than left to the validator because this is
#' where a user's mistake is: the validator's messages are a backstop for
#' internal construction.
#'
#' @param x List of `Hyperparameters` objects, optionally named, or a
#' `HyperparametersSet`, which is returned unchanged.
#'
#' @return `HyperparametersSet` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_HyperparametersSet <- function(x) {
  if (S7_inherits(x, HyperparametersSet)) {
    return(x)
  }
  if (!is.list(x)) {
    rtemis.core::abort(
      "Expected a list of Hyperparameters objects, not ",
      class(x)[[1L]],
      ".",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (length(x) == 0L) {
    rtemis.core::abort(
      "A list of hyperparameters must hold at least one Hyperparameters object.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  for (i in seq_along(x)) {
    if (S7_inherits(x[[i]], HyperparametersSet)) {
      rtemis.core::abort(
        "Member ",
        i,
        " is itself a set of hyperparameters. Sets do not nest: pass its members directly.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
    if (!S7_inherits(x[[i]], Hyperparameters)) {
      rtemis.core::abort(
        "Member ",
        i,
        " is a ",
        class(x[[i]])[[1L]],
        ", not a Hyperparameters object. Build each member with a setup_* function.",
        class = c("rtemis_type_error", "rtemis_input_error")
      )
    }
  }
  algorithms <- vapply(x, function(member) member@algorithm, character(1L))
  if (length(unique(algorithms)) > 1L) {
    rtemis.core::abort(
      "All members must be for the same algorithm, but found ",
      paste(unique(algorithms), collapse = " and "),
      ". Tuning across algorithms is not supported.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  HyperparametersSet(members = name_set_members(x))
} # /rtemis::as_HyperparametersSet


# %% name_set_members ----
#' Give every member of a set a name
#'
#' A name is how the winning member is reported, so every member has one whether
#' or not the user supplied it. Unnamed members are labelled by position, and
#' partial naming is allowed because R allows it.
#'
#' @param members List of `Hyperparameters` objects.
#'
#' @return The list, named.
#'
#' @author EDG
#' @keywords internal
#' @noRd
name_set_members <- function(members) {
  labels <- names(members)
  if (is.null(labels)) {
    labels <- rep("", length(members))
  }
  unnamed <- !nzchar(labels)
  labels[unnamed] <- paste0("variant_", seq_along(members))[unnamed]
  if (anyDuplicated(labels) > 0L) {
    rtemis.core::abort(
      "Members must have distinct names, but '",
      labels[[anyDuplicated(labels)]],
      "' is used more than once.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  names(members) <- labels
  members
} # /rtemis::name_set_members


# %% repr.HyperparametersSet ----
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, HyperparametersSet) <- function(x, pad = 0L, output_type = NULL) {
  n <- length(x@members)
  paste0(
    repr_S7name("HyperparametersSet", pad = pad, output_type = output_type),
    "Searching ",
    highlight(n, output_type = output_type),
    ngettext(n, " configuration", " configurations"),
    " of ",
    highlight(x@algorithm, output_type = output_type),
    ": ",
    paste(names(x@members), collapse = ", "),
    "\n"
  )
} # /rtemis::repr.HyperparametersSet


# %% print.HyperparametersSet ----
#' Print method for HyperparametersSet
#'
#' @param x `HyperparametersSet` object.
#' @param output_type Character: Output format.
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#'
#' @author EDG
#' @noRd
method(print, HyperparametersSet) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.HyperparametersSet


# %% `[[`.HyperparametersSet ----
# Members by name or position, so a set indexes like the list it was written as.
method(`[[`, HyperparametersSet) <- function(x, name) {
  x@members[[name]]
}


# %% length.HyperparametersSet ----
method(length, HyperparametersSet) <- function(x) {
  length(x@members)
}


# %% BENCH_VARIANT_COLUMN ----
# The grid column naming which member produced a row. Leading dot to mark it as
# not a hyperparameter: `.update_hyperparameters()` rejects any name that is not
# a settable hyperparameter, so this column is excluded by name wherever a row
# is applied.
VARIANT_COLUMN <- ".variant"


# %% needs_tuning.HyperparametersSet ----
#' @author EDG
#' @keywords internal
#' @noRd
method(needs_tuning, HyperparametersSet) <- function(x) {
  # More than one member is a choice to make even when no member varies
  # anything: the members are the candidates.
  length(x@members) > 1L ||
    any(vapply(x@members, needs_tuning, logical(1L)))
} # /rtemis::needs_tuning.HyperparametersSet


# %% member_grid_fill ----
#' The value a member holds for a hyperparameter another member tunes
#'
#' A grid row has to specify a configuration **completely**, because it is
#' applied to a member with `update()` and whatever it does not mention keeps
#' whatever the member already had -- which is right, but only if the row says
#' so explicitly rather than by omission. Two members tuning different
#' hyperparameters would otherwise produce a grid where a column means "not
#' varied here" in some rows and "varied to this" in others.
#'
#' NA is the fill for a member holding the hyperparameter unset, because
#' `.update_hyperparameters()` already reads NA back as NULL. That is the same
#' convention `gate_tuning_grid()` uses for a hyperparameter its gate excluded,
#' and the two do not conflict: both mean "this configuration does not set it".
#'
#' @param member `Hyperparameters` object.
#' @param name Character: The hyperparameter.
#' @param n Integer: Rows to fill.
#'
#' @return A column of length `n`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
member_grid_fill <- function(member, name, n) {
  value <- prop(member, name)
  if (is.null(value)) {
    return(rep(NA, n))
  }
  spec <- get_spec(S7_class(member)@properties[[name]])
  if (!is.null(spec) && spec@container != "none") {
    # A container hyperparameter is a list column: one cell holds one whole
    # value rather than one element of it.
    return(rep(list(value), n))
  }
  rep(value, n)
} # /rtemis::member_grid_fill


# %% tuning_grid.HyperparametersSet ----
#' The union of the members' grids
#'
#' Each member expands and gates on its own -- nothing about `applies_when`
#' changes here -- and the grids are row-bound, with every row naming the member
#' it came from in `.variant`.
#'
#' A member with nothing to tune still contributes one row. It is a
#' configuration to be compared against the others, which is the whole reason a
#' set of fixed members is a search at all.
#'
#' @param x `HyperparametersSet` object.
#'
#' @return data.frame: One row per configuration, with a `.variant` column.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(tuning_grid, HyperparametersSet) <- function(x) {
  labels <- names(x@members)
  grids <- lapply(labels, function(label) {
    grid <- tuning_grid(x@members[[label]])
    if (is.null(grid)) {
      # `data.frame()` with no columns has no rows either, so the single row a
      # fixed member contributes is made explicitly.
      grid <- data.frame(row.names = 1L)
    }
    grid
  })
  columns <- unique(unlist(lapply(grids, names), use.names = FALSE))
  filled <- lapply(seq_along(grids), function(i) {
    grid <- grids[[i]]
    for (name in setdiff(columns, names(grid))) {
      grid[[name]] <- member_grid_fill(
        x@members[[i]],
        name,
        max(NROW(grid), 1L)
      )
    }
    grid <- grid[, columns, drop = FALSE]
    grid[[VARIANT_COLUMN]] <- labels[[i]]
    grid
  })
  out <- do.call(rbind, filled)
  rownames(out) <- NULL
  out
} # /rtemis::tuning_grid.HyperparametersSet


# %% tuning_members ----
#' The members a tuner searches over, uniformly
#'
#' A single `Hyperparameters` object is a set of one. Returning that shape lets
#' the tuner apply a grid row to "the member it came from" without branching on
#' whether there was a set at all.
#'
#' @param x `Hyperparameters` or `HyperparametersSet` object.
#'
#' @return Named list of `Hyperparameters`, or NULL when `x` is a single object
#' -- NULL rather than a list of one, so a caller can tell the two apart and
#' leave `variant` unset on a fit that came from neither.
#'
#' @author EDG
#' @keywords internal
#' @noRd
tuning_members <- function(x) {
  if (S7_inherits(x, HyperparametersSet)) {
    return(x@members)
  }
  NULL
} # /rtemis::tuning_members


# %% grid_variant ----
#' The member name a grid row came from, or NULL
#'
#' @param grid data.frame: A tuning grid.
#' @param index Integer: Row to read.
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
grid_variant <- function(grid, index) {
  if (!VARIANT_COLUMN %in% names(grid)) {
    return(NULL)
  }
  as.character(grid[[VARIANT_COLUMN]][[index]])
} # /rtemis::grid_variant


# %% grid_hyperparameter_columns ----
#' The columns of a grid that are hyperparameters
#'
#' Everything but the bookkeeping columns. Selected by **name**, not by
#' position: the grid is prefixed with `resample_id` or `param_combo_id`
#' depending on which copy is being read, and `.variant` may or may not be
#' present, so positional indexing would have to know all three.
#'
#' @param grid data.frame: A tuning grid.
#'
#' @return Character vector of column names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
grid_hyperparameter_columns <- function(grid) {
  setdiff(
    names(grid),
    c(VARIANT_COLUMN, "resample_id", "param_combo_id")
  )
} # /rtemis::grid_hyperparameter_columns


# %% is_wire_hyperparameters_set ----
#' Is this parsed JSON a set of hyperparameters?
#'
#' The tag `serializable_props()` writes, `{"variants": {...}}`. An exact-names
#' test, as `is_wire_candidates()` uses: a reader tells a set from a single
#' configuration with no reference to what the property declares, and a document
#' carrying anything beside `variants` is not one.
#'
#' @param x Value to test, as parsed from JSON.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_wire_hyperparameters_set <- function(x) {
  identical(names(x), "variants")
} # /rtemis::is_wire_hyperparameters_set


# %% serializable_props.HyperparametersSet ----
#' The wire form of a set
#'
#' `{"variants": {"cart": {...}, "addtree": {...}}}` -- a JSON object keyed by
#' member name, so the names survive the round trip. They have to: the name is
#' what the tuner reports as the winner, so a set whose names regenerated on read
#' would report a different answer than the run that produced it.
#'
#' This is why the default is not enough. A list of S7 objects is published as
#' an array of `$ref`s and **unnamed** -- `base_learners` re-derives its names
#' from each entry's `algorithm`, which a set cannot do, since every member
#' shares one.
#'
#' `algorithm` is left out: it is a computed getter over the members, so the
#' members already carry it and a set is not a document that could disagree
#' with itself.
#'
#' @param x `HyperparametersSet` object.
#'
#' @return Named list with one element, `variants`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(serializable_props, HyperparametersSet) <- function(x) {
  list(variants = x@members)
} # /rtemis::serializable_props.HyperparametersSet


# %% to_json.HyperparametersSet ----
#' @name to_json
#' @keywords internal
#' @noRd
method(to_json, HyperparametersSet) <- function(x, ...) {
  list(variants = lapply(x@members, to_json))
} # /rtemis::to_json.HyperparametersSet


# %% .list_to_HyperparametersSet ----
#' Rebuild a set from parsed JSON
#'
#' The inverse of `serializable_props()`. Each member is rebuilt by
#' `.list_to_Hyperparameters()`, which routes through the member's own
#' `setup_*` -- so every path that reconstructs a configuration from user input
#' passes through the same validation, sets included.
#'
#' @param x Named list: A parsed `{"variants": {...}}` document.
#'
#' @return `HyperparametersSet` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_HyperparametersSet <- function(x) {
  variants <- x[["variants"]]
  if (!is.list(variants) || length(variants) == 0L) {
    rtemis.core::abort(
      "A hyperparameters set must carry at least one variant.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  as_HyperparametersSet(lapply(variants, .list_to_Hyperparameters))
} # /rtemis::.list_to_HyperparametersSet


# %% validate_hyperparameters.HyperparametersSet ----
#' Validate every member against the data
#'
#' Each member is a configuration the tuner may select, so each must satisfy the
#' data-dependent bounds -- a member whose `mtry` exceeds the feature count is
#' not made acceptable by sitting beside one that does not.
#'
#' Checked before tuning rather than when a member is reached, so an
#' unsatisfiable member is reported as the mistake it is rather than as one grid
#' cell failing among many. The message names the member, since "mtry cannot
#' exceed the number of features" is not actionable when four configurations
#' were supplied.
#'
#' @param hyperparameters `HyperparametersSet` object.
#' @param x tabular data: The training set.
#'
#' @return `hyperparameters`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(validate_hyperparameters, HyperparametersSet) <- function(
  hyperparameters,
  x
) {
  for (label in names(hyperparameters@members)) {
    withCallingHandlers(
      validate_hyperparameters(hyperparameters@members[[label]], x),
      rtemis_error = function(e) {
        rtemis.core::abort(
          "Variant '",
          label,
          "': ",
          conditionMessage(e),
          class = class(e)
        )
      }
    )
  }
  invisible(hyperparameters)
} # /rtemis::validate_hyperparameters.HyperparametersSet


# %% check_hyperparameters ----
#' Accept either a single configuration or a set of them
#'
#' The three entry points into tuning -- `train()`, `tune()` and
#' `tune_GridSearch()` -- each type-checked `Hyperparameters`. A set is a valid
#' input to all three, so the check is named once rather than branched three
#' times.
#'
#' @param hyperparameters `Hyperparameters` or `HyperparametersSet` object.
#'
#' @return Invisible NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_hyperparameters <- function(hyperparameters) {
  if (!S7_inherits(hyperparameters, HyperparametersSet)) {
    check_is_S7(hyperparameters, Hyperparameters)
  }
  invisible(NULL)
} # /rtemis::check_hyperparameters
