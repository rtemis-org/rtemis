# writeup_hyperparameters.R
# ::rtemis::
# 2026- EDG rtemis.org

# The hyperparameter tables of a writeup.
#
# One row per hyperparameter the model's record carries. Each fit's value and
# origin are those of `config_record()` of the authored hyperparameters against
# the fit, so a value and its origin are those the record states. The main
# table lists the primary hyperparameters (the class's `reporting`
# declaration, or `include_hyperparameters`), every hyperparameter that was
# tuned or specified, and run state selected during fitting; the supplement
# lists every hyperparameter that applies to the fit. Every cell is a template
# of values-table tokens.
#
# spec: rtemis/writeup#hyperparameter-tables

# %% WRITEUP_HP_ORIGIN_SOURCES ----
# Record origin to table source.
WRITEUP_HP_ORIGIN_SOURCES <- c(
  user = "specified",
  default = "default",
  derived = "resolved",
  tuned = "tuned",
  unset = "unset"
)

# %% WRITEUP_MLP_SHAPE_SETTINGS ----
# MLP settings that generate the hidden layer widths when `hidden_units` is
# unset.
WRITEUP_MLP_SHAPE_SETTINGS <- c("shape", "shape_layers", "shape_max_units")


# %% hyperparameter_applies ----
#' Whether a hyperparameter has an effect on a fit
#'
#' The default reads the declared `applies_when` gates. Methods add the rules a
#' gate cannot state: the LightGBM objective-specific parameters
#' (`LIGHTGBM_OBJECTIVE_PARAMETERS`) and the MLP width-generating settings,
#' which a supplied `hidden_units` bypasses.
#'
#' @param hyperparameters `Hyperparameters` object: As the fit used them.
#' @param name Character: Hyperparameter name.
#' @param selected Named list: The fit's authored values, a searched one
#'   replaced by the value tuning selected for the fit
#'   (`writeup_hp_selected()`).
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hyperparameter_applies <- new_generic(
  "hyperparameter_applies",
  "hyperparameters",
  function(hyperparameters, name, selected) {
    force_supplied()
    S7_dispatch()
  }
)

method(hyperparameter_applies, Hyperparameters) <- function(
  hyperparameters,
  name,
  selected
) {
  hyperparameter_in_effect(hyperparameters, name)
}

# %% hyperparameter_applies.LightGBM family ----
lightgbm_hyperparameter_applies <- function(hyperparameters, name, selected) {
  if (!hyperparameter_in_effect(hyperparameters, name)) {
    return(FALSE)
  }
  objective <- hyperparameters[["objective"]]
  if (!name %in% names(LIGHTGBM_OBJECTIVE_PARAMETERS) || is.null(objective)) {
    return(TRUE)
  }
  objective %in% LIGHTGBM_OBJECTIVE_PARAMETERS[[name]]
} # /rtemis::lightgbm_hyperparameter_applies

method(hyperparameter_applies, LightGBMHyperparameters) <-
  lightgbm_hyperparameter_applies
method(hyperparameter_applies, LightRFHyperparameters) <-
  lightgbm_hyperparameter_applies
method(hyperparameter_applies, LightCARTHyperparameters) <-
  lightgbm_hyperparameter_applies
method(hyperparameter_applies, LightRuleFitHyperparameters) <-
  lightgbm_hyperparameter_applies

method(hyperparameter_applies, MLPHyperparameters) <- function(
  hyperparameters,
  name,
  selected
) {
  hyperparameter_in_effect(hyperparameters, name) &&
    !(name %in%
      WRITEUP_MLP_SHAPE_SETTINGS &&
      !is.null(selected[["hidden_units"]]))
}


# %% writeup_hp_selected ----
#' A fit's authored values, with tuning's selections in place of searches
#'
#' @param input `Hyperparameters` object: As authored for the fit.
#' @param fit `Supervised` object.
#'
#' @return Named list of values; a selected NULL stays an entry.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_selected <- function(input, fit) {
  values <- input@hyperparameters
  best <- if (is.null(fit@tuner)) NULL else fit@tuner@best_hyperparameters
  for (nm in names(values)) {
    if (is_candidates(values[[nm]]) && nm %in% names(best)) {
      values[nm] <- list(best[[nm]])
    }
  }
  values
} # /rtemis::writeup_hp_selected


# %% writeup_hp_authored ----
#' The hyperparameters as the user authored them
#'
#' The model's input config where it carries one (every model a top-level
#' `train()` call returns); otherwise the search space. A single-split model's
#' `@hyperparameters` holds the values the fit resolved, so it is the authored
#' form only through the config or the tuner.
#'
#' @param ctx Context list.
#'
#' @return `Hyperparameters` or `HyperparametersSet` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_authored <- function(ctx) {
  config <- ctx[["x"]]@config
  if (!is.null(config) && !is.null(config@hyperparameters)) {
    return(config@hyperparameters)
  }
  if (ctx[["is_set"]]) ctx[["set"]] else ctx[["search_space"]]
} # /rtemis::writeup_hp_authored


# %% writeup_hp_authored_root ----
writeup_hp_authored_root <- function(ctx) {
  if (!is.null(ctx[["x"]]@config)) {
    return("config.hyperparameters")
  }
  if (ctx[["is_set"]] && !ctx[["resampled"]]) {
    "tuner.searched_set"
  } else {
    ctx[["space_root"]]
  }
} # /rtemis::writeup_hp_authored_root


# %% writeup_hp_input ----
#' The authored hyperparameters a fit is compared against
#'
#' The authored hyperparameters, or for a hyperparameter set the member the fit
#' came from.
#'
#' @param ctx Context list.
#' @param fit `Supervised` object.
#'
#' @return `Hyperparameters` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_input <- function(ctx, fit) {
  authored <- writeup_hp_authored(ctx)
  if (!S7_inherits(authored, HyperparametersSet)) {
    return(authored)
  }
  variant <- fit@hyperparameters@variant
  authored@variants[[variant %||% 1L]] %||% authored@variants[[1L]]
} # /rtemis::writeup_hp_input


# %% writeup_hp_object_text ----
#' A config-valued hyperparameter as text
#'
#' A `Hyperparameters` object is named by its algorithm, a named list of them
#' by name and algorithm, and another config by its family discriminator.
#'
#' @param value S7 object, or list of them.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_object_text <- function(value) {
  one <- function(v) {
    if (S7_inherits(v, Hyperparameters)) {
      return(v@algorithm)
    }
    discriminator <- family_discriminator(v)
    if (!is.null(discriminator)) {
      return(as.character(prop(v, discriminator)))
    }
    S7_class(v)@name
  }
  if (S7_inherits(value)) {
    return(one(value))
  }
  texts <- vapply(value, one, character(1L))
  if (!is.null(names(value))) {
    texts <- ifelse(
      names(value) == texts,
      texts,
      paste0(names(value), " (", texts, ")")
    )
  }
  paste(texts, collapse = ", ")
} # /rtemis::writeup_hp_object_text
# %% writeup_hp_object_origin ----
#' Origin of a config-valued hyperparameter
#'
#' The record carries such a value as a nested block with its own origins, so
#' the table reads it as the default when it equals the value of the
#' algorithm's setup function called without arguments, and as specified
#' otherwise.
#'
#' @param input `Hyperparameters` object: As authored.
#' @param value The value the fit used.
#' @param name Character: Hyperparameter name.
#'
#' @return Character: One of `VALUE_ORIGINS`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_object_origin <- function(input, value, name) {
  declared <- config_origins(input)
  if (name %in% names(declared)) {
    return(declared[[name]])
  }
  setup <- get0(paste0("setup_", input@algorithm), mode = "function")
  default <- if (is.null(setup)) {
    NULL
  } else {
    tryCatch(setup(), error = function(e) NULL)
  }
  if (
    !is.null(default) &&
      identical(S7_to_list(prop(default, name)), S7_to_list(value))
  ) {
    "default"
  } else {
    "user"
  }
} # /rtemis::writeup_hp_object_origin
# %% writeup_hp_tokens ----
#' Record a hyperparameter value and return its cell template
#'
#' A number is written in full (kind "exact" or "integer"), a vector element by
#' element, and any other value as compact JSON text.
#'
#' @param w Collector.
#' @param key Character: Base token name.
#' @param value Wire value, or Character scalar for an object summary.
#' @param source Character: Path of the field.
#'
#' @return Character: Template.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_tokens <- function(w, key, value, source) {
  scalar_token <- function(k, v, s) {
    if (is.logical(v)) {
      writeup_value(w, k, if (isTRUE(v)) "true" else "false", "text", s)
    } else if (is.integer(v)) {
      writeup_value(w, k, v, "integer", s)
    } else if (is.numeric(v)) {
      writeup_value(w, k, v, "exact", s)
    } else {
      writeup_value(w, k, as.character(v), "text", s)
    }
  }
  if (is.atomic(value) && is.null(names(value)) && length(value) == 1L) {
    return(scalar_token(key, value, source))
  }
  if (is.atomic(value) && is.null(names(value)) && length(value) > 1L) {
    tokens <- vapply(
      seq_along(value),
      function(i) {
        scalar_token(
          writeup_key(key, i),
          value[[i]],
          paste0(source, "[", i, "]")
        )
      },
      character(1L)
    )
    return(paste(tokens, collapse = ", "))
  }
  writeup_value(
    w,
    key,
    as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )),
    "text",
    source
  )
} # /rtemis::writeup_hp_tokens


# %% writeup_hp_identity ----
#' A value's identity for grouping: its serialized settings
#'
#' @param value A wire value, an S7 config object, or a list of them.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_identity <- function(value) {
  if (S7_inherits(value) || is_S7_list(value)) {
    value <- S7_to_list(value)
  }
  as.character(jsonlite::toJSON(value, null = "null", digits = NA))
} # /rtemis::writeup_hp_identity


# %% writeup_hp_summaries ----
#' Texts of config-valued entries
#'
#' Each entry's summary (`writeup_hp_object_text()`), or its serialized
#' settings where two entries share a summary.
#'
#' @param values List of S7 config objects, lists of them, or NULL.
#' @param identities Character: `writeup_hp_identity()` of each value.
#'
#' @return Character vector, "" for NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_summaries <- function(values, identities) {
  texts <- vapply(
    values,
    function(v) if (is.null(v)) "" else writeup_hp_object_text(v),
    character(1L)
  )
  collide <- nzchar(texts) &
    (duplicated(texts) | duplicated(texts, fromLast = TRUE))
  texts[collide] <- identities[collide]
  texts
} # /rtemis::writeup_hp_summaries


# %% writeup_hp_tried ----
#' Values a hyperparameter took in the configurations tuning evaluated
#'
#' Read from each tuner's evaluated grid (`tuning_results$param_grid`): the
#' grid's column for a searched hyperparameter, or, for a hyperparameter set,
#' the value each evaluated member gives it. An unset value is an alternative
#' of its own.
#'
#' @param ctx Context list.
#' @param name Character: Hyperparameter name.
#'
#' @return List with `values` (list, NULL for unset), `sources` and
#'   `identities` (character), or NULL when tuning evaluated fewer than two
#'   values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_tried <- function(ctx, name) {
  authored <- writeup_hp_authored(ctx)
  grid_path <- paste0(ctx[["tuner_root"]], ".tuning_results.param_grid.", name)
  values <- list()
  sources <- character()
  for (tuner in Filter(Negate(is.null), ctx[["tuners"]])) {
    grid <- tuner@tuning_results[["param_grid"]]
    if (is.null(grid)) {
      next
    }
    if (name %in% names(grid)) {
      column <- grid[[name]]
      for (v in if (is.list(column)) column else as.list(column)) {
        values <- c(values, list(if (length(v) == 1L && is.na(v)) NULL else v))
        sources <- c(sources, paste0("derived: distinct values of ", grid_path))
      }
    } else if (
      S7_inherits(authored, HyperparametersSet) && ".variant" %in% names(grid)
    ) {
      for (variant in unique(grid[[".variant"]])) {
        member <- authored@variants[[variant]]
        values <- c(values, list(member@hyperparameters[[name]]))
        sources <- c(
          sources,
          paste0(
            writeup_hp_authored_root(ctx),
            ".variants.",
            variant,
            ".",
            name
          )
        )
      }
    }
  }
  ids <- vapply(values, writeup_hp_identity, character(1L))
  keep <- !duplicated(ids)
  if (sum(keep) < 2L) {
    return(NULL)
  }
  list(values = values[keep], sources = sources[keep], identities = ids[keep])
} # /rtemis::writeup_hp_tried


# %% writeup_hyperparameters ----
#' Rows of the hyperparameter tables
#'
#' @param ctx Context list.
#' @param w Collector.
#' @param include Optional Character: Hyperparameters the main table lists in
#'   place of the class's primary hyperparameters.
#'
#' @return data.frame with columns `name`, `main`, `applies`, `value`, `tried`
#'   and `source`, one row per hyperparameter in record order; zero rows when
#'   the model records none.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hyperparameters <- function(ctx, w, include = NULL) {
  fits <- ctx[["fits"]]
  cls <- S7_class(fits[[1L]]@hyperparameters)
  names_ <- record_names(cls, family_base(cls))
  primary <- include %||% schema_reporting(cls)[["primary"]] %||% character()
  props <- cls@properties
  inputs <- lapply(fits, function(fit) writeup_hp_input(ctx, fit))
  records <- lapply(seq_along(fits), function(i) {
    config_record(inputs[[i]], fits[[i]]@hyperparameters)
  })
  selected <- lapply(seq_along(fits), function(i) {
    writeup_hp_selected(inputs[[i]], fits[[i]])
  })
  rows <- lapply(names_, function(nm) {
    prop_def <- props[[nm]]
    state <- identical(prop_role(prop_def), "state")
    objects <- lapply(fits, function(fit) prop(fit@hyperparameters, nm))
    object_valued <- any(vapply(
      objects,
      function(v) S7_inherits(v) || is_S7_list(v),
      logical(1L)
    ))
    # One entry per fit: the value as recorded, its identity, its source, and
    # whether the hyperparameter had an effect on that fit.
    fit_values <- lapply(seq_along(fits), function(i) {
      value <- if (object_valued) objects[[i]] else records[[i]][[nm]]
      origin <- records[[i]][["origin"]][[nm]] %||%
        writeup_hp_object_origin(inputs[[i]], objects[[i]], nm)
      if (is.null(value) && !state) {
        origin <- "unset"
      }
      list(
        value = value,
        identity = writeup_hp_identity(value),
        source = unname(WRITEUP_HP_ORIGIN_SOURCES[[origin]]),
        applies = hyperparameter_applies(
          fits[[i]]@hyperparameters,
          nm,
          selected[[i]]
        )
      )
    })
    applies <- vapply(fit_values, `[[`, logical(1L), "applies")
    sources <- unique(vapply(
      fit_values[applies],
      `[[`,
      character(1L),
      "source"
    ))
    # Run state the run never reached reports nothing.
    if (state && (!any(applies) || identical(sources, "unset"))) {
      return(NULL)
    }
    if (!any(applies)) {
      return(data.frame(
        name = nm,
        main = FALSE,
        applies = FALSE,
        value = "",
        tried = "",
        source = vapply(fit_values, `[[`, character(1L), "source")[[1L]]
      ))
    }
    key <- writeup_key("table_hp", nm)
    value <- writeup_hp_cell(
      w,
      key,
      fit_values,
      ctx[["path"]]("hyperparameters", nm),
      object_valued = object_valued,
      unset = unset_meaning(cls, nm),
      unset_source = paste0("declaration: ", cls@name, ".", nm)
    )
    tried <- writeup_hp_tried(ctx, nm)
    tried_cell <- if (is.null(tried)) {
      ""
    } else {
      summaries <- if (object_valued) {
        writeup_hp_summaries(tried[["values"]], tried[["identities"]])
      }
      paste(
        vapply(
          seq_along(tried[["values"]]),
          function(j) {
            k <- writeup_key(key, "tried", j)
            v <- tried[["values"]][[j]]
            if (is.null(v)) {
              # As in the value cell: the meaning, once per hyperparameter,
              # is listed under the table.
              writeup_value(
                w,
                writeup_key(key, "meaning"),
                unset_meaning(cls, nm) %||% "Unset.",
                "text",
                paste0("declaration: ", cls@name, ".", nm)
              )
              writeup_value(w, k, "unset", "text", tried[["sources"]][[j]])
            } else if (object_valued) {
              writeup_value(
                w,
                k,
                summaries[[j]],
                "text",
                tried[["sources"]][[j]]
              )
            } else {
              writeup_hp_tokens(
                w,
                k,
                S7_to_list(wire_value(v, prop_def)),
                tried[["sources"]][[j]]
              )
            }
          },
          character(1L)
        ),
        collapse = "; "
      )
    }
    data.frame(
      name = nm,
      main = nm %in%
        primary ||
        any(sources %in% c("specified", "tuned")) ||
        (state && "resolved" %in% sources),
      applies = TRUE,
      value = value,
      tried = tried_cell,
      source = if (length(sources) == 1L) sources else "varied"
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame(
      name = character(),
      main = logical(),
      applies = logical(),
      value = character(),
      tried = character(),
      source = character()
    ))
  }
  do.call(rbind, rows)
} # /rtemis::writeup_hyperparameters


# %% writeup_hp_cell ----
#' The value cell of one hyperparameter
#'
#' One value when every fit used the same value from the same source.
#' Otherwise each distinct value, with the number of resamples that used it and,
#' when sources differ, how it was chosen; resamples in which the
#' hyperparameter had no effect are counted as not applicable. A config-valued
#' hyperparameter is counted by configuration and written as its summary, or
#' as its settings where two configurations share a summary. An unset value
#' reads "unset"; its declared meaning is recorded once, as `<key>_meaning`.
#'
#' @param w Collector.
#' @param key Character: Base token name.
#' @param fit_values List: Per fit, `value`, `identity`, `source` and
#'   `applies`.
#' @param path Character: Path of the field.
#' @param object_valued Logical: Whether the values are config objects.
#' @param unset Optional Character: What the hyperparameter means unset.
#' @param unset_source Character: Source of `unset`.
#'
#' @return Character: Template.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_cell <- function(
  w,
  key,
  fit_values,
  path,
  object_valued = FALSE,
  unset = NULL,
  unset_source = "declaration"
) {
  applies <- vapply(fit_values, `[[`, logical(1L), "applies")
  used <- fit_values[applies]
  groups <- vapply(
    used,
    function(f) paste(f[["identity"]], f[["source"]]),
    character(1L)
  )
  distinct <- unique(groups)
  entries <- used[match(distinct, groups)]
  counts <- vapply(distinct, function(g) sum(groups == g), integer(1L))
  sources <- unique(vapply(entries, `[[`, character(1L), "source"))
  summaries <- if (object_valued) {
    writeup_hp_summaries(
      lapply(entries, `[[`, "value"),
      vapply(entries, `[[`, character(1L), "identity")
    )
  }
  one <- function(i, k) {
    e <- entries[[i]]
    if (is.null(e[["value"]])) {
      # The cell reads "unset"; what that means is one token per hyperparameter,
      # listed under the table.
      writeup_value(
        w,
        writeup_key(key, "meaning"),
        unset %||% "Unset.",
        "text",
        unset_source
      )
      writeup_value(w, writeup_key(k, "unset"), "unset", "text", path)
    } else if (object_valued) {
      writeup_value(w, k, summaries[[i]], "text", path)
    } else {
      writeup_hp_tokens(w, k, e[["value"]], path)
    }
  }
  if (length(entries) == 1L && all(applies)) {
    return(one(1L, key))
  }
  parts <- vapply(
    seq_along(entries),
    function(i) {
      k <- writeup_key(key, "value", i)
      n <- writeup_value(
        w,
        writeup_key(k, "n"),
        counts[[i]],
        "count",
        "derived: number of resamples with this value"
      )
      paste0(
        one(i, k),
        " (",
        n,
        if (length(sources) > 1L) {
          paste0(", ", WRITEUP_HP_SOURCE_LABELS[[entries[[i]][["source"]]]])
        },
        ")"
      )
    },
    character(1L)
  )
  if (!all(applies)) {
    parts <- c(
      parts,
      paste0(
        "not applicable (",
        writeup_value(
          w,
          writeup_key(key, "not_applicable", "n"),
          sum(!applies),
          "count",
          "derived: number of resamples in which it had no effect"
        ),
        ")"
      )
    )
  }
  paste(parts, collapse = "; ")
} # /rtemis::writeup_hp_cell


# %% check_include_hyperparameters ----
#' Check the hyperparameters a writeup's main table is asked to list
#'
#' @param include Optional Character: Hyperparameter names.
#' @param ctx Context list.
#'
#' @return `include`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_include_hyperparameters <- function(include, ctx) {
  if (is.null(include)) {
    return(invisible(include))
  }
  if (!is.character(include) || !is.null(dim(include)) || anyNA(include)) {
    rtemis.core::abort(
      "`include_hyperparameters` must be a character vector of hyperparameter names, or NULL.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  cls <- S7_class(ctx[["fits"]][[1L]]@hyperparameters)
  available <- record_names(cls, family_base(cls))
  unknown <- setdiff(include, available)
  if (length(unknown) > 0L) {
    rtemis.core::abort(
      "`include_hyperparameters` names no hyperparameter of ",
      cls@name,
      ": ",
      paste(unknown, collapse = ", "),
      ". Available: ",
      paste(available, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(include)
} # /rtemis::check_include_hyperparameters


# %% writeup_hp_main_scope ----
#' What the main hyperparameter table lists
#'
#' @param include Optional Character: `include_hyperparameters` as the writeup
#'   was asked.
#'
#' @return Character phrase, starting in lowercase.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_main_scope <- function(include) {
  paste0(
    if (is.null(include)) {
      "the primary hyperparameters of the algorithm, "
    } else if (length(include) > 0L) {
      "the hyperparameters selected for this table, "
    },
    "every hyperparameter that was tuned or specified, and the values selected during fitting"
  )
} # /rtemis::writeup_hp_main_scope


# %% writeup_hp_sentence ----
#' The Model paragraph's sentence citing the hyperparameter tables
#'
#' @param ctx Context list.
#' @param w Collector.
#'
#' @return Character sentence, or NULL when the model records no
#'   hyperparameter.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_sentence <- function(ctx, w) {
  rows <- ctx[["hyperparameter_rows"]]
  if (NROW(rows) == 0L || !any(rows[["applies"]])) {
    return(NULL)
  }
  supplement <- paste0(
    "Supplementary Table S",
    writeup_value(
      w,
      "table_hyperparameters_supplement",
      1L,
      "integer",
      "writeup: table number"
    ),
    " lists every hyperparameter that applied to the fit, its value, how it was chosen and the values tuning evaluated."
  )
  if (!any(rows[["main"]])) {
    return(supplement)
  }
  paste0(
    "Table ",
    writeup_value(
      w,
      "table_hyperparameters",
      1L,
      "integer",
      "writeup: table number"
    ),
    " lists ",
    writeup_hp_main_scope(ctx[["include_hyperparameters"]]),
    ". ",
    supplement
  )
} # /rtemis::writeup_hp_sentence
