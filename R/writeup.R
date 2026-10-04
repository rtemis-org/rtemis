# writeup.R
# ::rtemis::
# 2026- EDG rtemis.org

# Methods and Results sections for a trained supervised model.
#
# Each section builder reads the context assembled by `writeup_context()` and
# returns a `WriteupSection` or NULL. Numbers and identifiers read from the
# model enter a template only through `writeup_value()`, which records the
# value, its formatted text and its source and returns the `{key}` token;
# citations enter only through `writeup_cite_package()` and
# `writeup_cite_method()`.
#
# spec: rtemis/writeup

# %% writeup.Supervised ----
method(writeup, Supervised) <- function(x, confidence_level = NULL, ...) {
  writeup_supervised(x, confidence_level)
} # /rtemis::writeup.Supervised


# %% writeup.SupervisedRes ----
method(writeup, SupervisedRes) <- function(x, confidence_level = NULL, ...) {
  writeup_supervised(x, confidence_level)
} # /rtemis::writeup.SupervisedRes


# %% writeup_supervised ----
#' Write up a single-split or resampled supervised model
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param confidence_level Optional Numeric (0, 1): Confidence level.
#'
#' @return `SupervisedWriteup` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_supervised <- function(x, confidence_level) {
  rv <- review(x, confidence_level = confidence_level)
  ctx <- writeup_context(x, rv)
  w <- writeup_collector()
  ctx[["selection"]] <- writeup_selection(ctx[["member"]], ctx, w)
  sections <- Filter(
    Negate(is.null),
    list(
      writeup_data(ctx, w),
      writeup_preprocessing(ctx, w),
      writeup_model(ctx, w),
      writeup_tuning(ctx, w),
      writeup_evaluation(ctx, w),
      writeup_measures(ctx, w),
      writeup_software(ctx, w),
      writeup_results_sample(ctx, w),
      writeup_results_performance(ctx, w),
      writeup_results_baseline(ctx, w),
      writeup_results_tuning(ctx, w)
    )
  )
  SupervisedWriteup(
    algorithm = x@algorithm,
    type = x@type,
    review = rv,
    values = writeup_values_table(w),
    sections = sections,
    references = writeup_references_table(w, sections),
    not_reported = c(writeup_not_reported(ctx), w[["not_reported"]])
  )
} # /rtemis::writeup_supervised


# %% Collector ----

# %% writeup_collector ----
#' Accumulate the values, references and unrecorded items of a writeup
#'
#' @return Environment with `values`, `references` and `not_reported`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_collector <- function() {
  w <- new.env(parent = emptyenv())
  w[["values"]] <- list()
  w[["references"]] <- list()
  w[["not_reported"]] <- character()
  w
} # /rtemis::writeup_collector


# %% writeup_value ----
#' Record a value and return its token
#'
#' A key may be recorded more than once with the same text, so two
#' paragraphs can cite one value.
#'
#' @param w Collector from `writeup_collector()`.
#' @param key Character: Token name, lowercase snake case.
#' @param value Numeric scalar, or Character for kind "text".
#' @param kind Character: One of `WRITEUP_VALUE_KINDS`.
#' @param source Character: Where the value was read from. A value computed
#'   from fields starts with "derived:".
#'
#' @return Character: The token, `{key}`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_value <- function(w, key, value, kind, source) {
  if (!grepl("^[a-z][a-z0-9_]*$", key)) {
    rtemis.core::abort(
      "Writeup value key '",
      key,
      "' must be lowercase snake case.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  text <- fmt_writeup_value(value, kind)
  previous <- w[["values"]][[key]]
  if (!is.null(previous) && !identical(previous[["text"]], text)) {
    rtemis.core::abort(
      "Writeup value key '",
      key,
      "' was recorded with two different values.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  w[["values"]][[key]] <- list(
    key = key,
    value = if (kind == "text") NA_real_ else as.numeric(value),
    kind = kind,
    text = text,
    source = source
  )
  paste0("{", key, "}")
} # /rtemis::writeup_value


# %% writeup_key ----
# A token name from free text: lowercase, non-alphanumerics to underscores.
writeup_key <- function(...) {
  key <- tolower(paste(..., sep = "_"))
  key <- gsub("[^a-z0-9]+", "_", key)
  gsub("^_+|_+$", "", key)
} # /rtemis::writeup_key


# %% writeup_cite_package ----
#' Cite a package and return its citation token
#'
#' The reference is the installed package's `citation()`, or a reference
#' naming the package and its recorded version when it is not installed.
#'
#' @param w Collector.
#' @param package Character: Package name, or "R".
#' @param version Optional Character: Version used to fit the model.
#'
#' @return Character: The citation token.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_cite_package <- function(w, package, version = NULL) {
  if (is.null(w[["references"]][[package]])) {
    citation <- tryCatch(
      suppressWarnings(
        if (package == "R") utils::citation() else utils::citation(package)
      ),
      error = function(e) NULL
    )
    text <- if (length(citation) > 0L) {
      writeup_format_citation(citation[[1L]])
    } else {
      paste0(
        package,
        ": R package",
        if (!is.null(version)) paste0(", version ", version),
        "."
      )
    }
    w[["references"]][[package]] <- list(
      key = package,
      package = package,
      citation = text
    )
  }
  paste0("{ref:", package, "}")
} # /rtemis::writeup_cite_package


# %% writeup_format_citation ----
#' Format one bibliography entry as plain text
#'
#' @param entry `bibentry` of length 1.
#'
#' @return Character scalar, whitespace collapsed.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_format_citation <- function(entry) {
  text <- paste(format(entry, style = "text"), collapse = " ")
  text <- gsub("[_*]([^_*]+)[_*]", "\\1", text)
  text <- gsub("<([^>]+)>", "\\1", text)
  gsub("\\s+", " ", trimws(text))
} # /rtemis::writeup_format_citation


# %% writeup_cite_method ----
#' Cite a statistical method and return its citation token
#'
#' @param w Collector.
#' @param key Character: Name in `WRITEUP_METHOD_REFERENCES`.
#'
#' @return Character: The citation token.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_cite_method <- function(w, key) {
  if (is.null(w[["references"]][[key]])) {
    w[["references"]][[key]] <- list(
      key = key,
      package = NA_character_,
      citation = unname(WRITEUP_METHOD_REFERENCES[[key]])
    )
  }
  paste0("{ref:", key, "}")
} # /rtemis::writeup_cite_method


# %% writeup_values_table ----
writeup_values_table <- function(w) {
  v <- w[["values"]]
  data.frame(
    key = vapply(v, `[[`, character(1L), "key"),
    value = vapply(v, `[[`, numeric(1L), "value"),
    kind = vapply(v, `[[`, character(1L), "kind"),
    text = vapply(v, `[[`, character(1L), "text"),
    source = vapply(v, `[[`, character(1L), "source"),
    row.names = NULL
  )
} # /rtemis::writeup_values_table


# %% writeup_references_table ----
#' References in order of first citation in the sections
#'
#' @param w Collector.
#' @param sections List of `WriteupSection`.
#'
#' @return data.frame with `key`, `package` and `citation`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_references_table <- function(w, sections) {
  text <- paste(
    unlist(lapply(sections, function(s) s@paragraphs)),
    collapse = " "
  )
  tokens <- regmatches(text, gregexpr(WRITEUP_REF_TOKEN, text))[[1L]]
  keys <- unique(trimws(unlist(strsplit(
    substr(tokens, 6L, nchar(tokens) - 1L),
    ",",
    fixed = TRUE
  ))))
  refs <- w[["references"]][keys]
  data.frame(
    key = keys,
    package = vapply(
      refs,
      function(r) {
        if (is.na(r[["package"]])) NA_character_ else r[["package"]]
      },
      character(1L)
    ),
    citation = vapply(refs, `[[`, character(1L), "citation"),
    row.names = NULL
  )
} # /rtemis::writeup_references_table


# %% writeup_section ----
writeup_section <- function(part, heading, paragraphs) {
  paragraphs <- Filter(function(p) !is.null(p) && nzchar(p), paragraphs)
  if (length(paragraphs) == 0L) {
    return(NULL)
  }
  WriteupSection(
    part = part,
    heading = heading,
    paragraphs = unlist(paragraphs)
  )
} # /rtemis::writeup_section


# %% Text helpers ----

# %% writeup_list ----
# Join items as "a", "a and b", or "a, b and c".
writeup_list <- function(items, conjunction = "and") {
  n <- length(items)
  if (n == 0L) {
    ""
  } else if (n == 1L) {
    items
  } else {
    paste0(
      paste(items[-n], collapse = ", "),
      " ",
      conjunction,
      " ",
      items[[n]]
    )
  }
} # /rtemis::writeup_list


# %% writeup_code ----
# An identifier as code. Digits inside a code span are part of a name.
writeup_code <- function(x) paste0("`", x, "`")


# %% writeup_plural ----
writeup_plural <- function(n, singular, plural = paste0(singular, "s")) {
  if (isTRUE(n == 1)) singular else plural
}


# %% writeup_metric_label ----
# A metric name as it reads in a sentence: acronyms stay capitalized, other
# names are lowercase.
writeup_metric_label <- function(metric) {
  label <- label_metrics(metric)
  label <- ifelse(
    metric %in% c(CAP_METRICS, "rsq", "f1"),
    label,
    tolower(label)
  )
  sub("^brier", "Brier", label)
} # /rtemis::writeup_metric_label


# %% writeup_capitalize ----
writeup_capitalize <- function(x) {
  paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}


# %% writeup_prop ----
# A property of an S7 object, or NULL when its class does not declare it.
writeup_prop <- function(x, name) {
  if (S7::prop_exists(x, name)) S7::prop(x, name) else NULL
}


# %% writeup_range ----
#' Token for one value, or a range of tokens for values that vary
#'
#' @param w Collector.
#' @param key Character: Key prefix.
#' @param values Numeric vector: One value per fit.
#' @param kind Character: Value kind.
#' @param source Character: Where each value was read from.
#'
#' @return Character: "{key}" when all values agree, else
#'   "between {key_min} and {key_max}".
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_range <- function(w, key, values, kind, source) {
  values <- unique(values[!is.na(values)])
  if (length(values) == 1L) {
    return(writeup_value(w, key, values, kind, source))
  }
  paste0(
    "between ",
    writeup_value(
      w,
      paste0(key, "_min"),
      min(values),
      kind,
      paste0("derived: minimum of ", source)
    ),
    " and ",
    writeup_value(
      w,
      paste0(key, "_max"),
      max(values),
      kind,
      paste0("derived: maximum of ", source)
    )
  )
} # /rtemis::writeup_range


# %% Context ----

# %% writeup_context ----
#' What the section builders read from a model
#'
#' Single-split and resampled models store the same facts in different
#' places; this gathers them once, with the path each was read from.
#'
#' @param x `Supervised` or `SupervisedRes` object.
#' @param rv `SupervisedReview` of `x`.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_context <- function(x, rv) {
  resampled <- S7_inherits(x, SupervisedRes)
  fits <- if (resampled) x@models else list(x)
  fit_root <- if (resampled) "models[]" else ""
  path <- function(...) {
    sub("^\\.", "", paste(c(fit_root, ...), collapse = "."))
  }
  tuners <- lapply(fits, function(f) f@tuner)
  tuner_index <- Position(Negate(is.null), tuners)
  tuner <- if (is.na(tuner_index)) NULL else tuners[[tuner_index]]
  # The hyperparameters as authored: the search space, or for a tuned single
  # split the space the tuner searched; a set when one was searched.
  set <- if (resampled && S7_inherits(x@hyperparameters, HyperparametersSet)) {
    x@hyperparameters
  } else if (!is.null(tuner)) {
    tuner@searched_set
  } else {
    NULL
  }
  search_space <- if (resampled) {
    x@hyperparameters
  } else if (!is.null(tuner)) {
    tuner@hyperparameters
  } else {
    x@hyperparameters
  }
  space_root <- if (resampled) {
    "hyperparameters"
  } else if (!is.null(tuner)) {
    "tuner.hyperparameters"
  } else {
    "hyperparameters"
  }
  member <- if (!is.null(set)) set@variants[[1L]] else search_space
  preprocessor_config <- if (resampled) {
    x@preprocessor_config
  } else if (!is.null(x@preprocessor)) {
    x@preprocessor@config
  } else {
    NULL
  }
  decompositions <- Filter(
    Negate(is.null),
    lapply(fits, function(f) f@decomposition)
  )
  fit_metrics <- fits[[1L]]@metrics_test %||% fits[[1L]]@metrics_training
  positive_class <- if (
    x@type == "Classification" && nlevels(fits[[1L]]@y_training) == 2L
  ) {
    fit_metrics@metrics[["positive_class"]]
  } else {
    NULL
  }
  fingerprint <- x@data_fingerprint
  list(
    x = x,
    review = rv,
    resampled = resampled,
    fits = fits,
    path = path,
    tuners = tuners,
    tuner = tuner,
    tuner_root = if (resampled) "models[].tuner" else "tuner",
    grid = if (!is.null(tuner)) tuner@tuning_results[["param_grid"]],
    set = set,
    is_set = !is.null(set),
    search_space = search_space,
    space_root = space_root,
    member = member,
    preprocessor_config = preprocessor_config,
    decompositions = decompositions,
    positive_class = positive_class,
    levels = if (x@type == "Classification") {
      levels(fits[[1L]]@y_training)
    } else {
      NULL
    },
    outcome = if (!is.null(fingerprint)) {
      utils::tail(fingerprint@column_names, 1L)
    } else {
      NULL
    },
    n_cases = if (!is.null(fingerprint)) fingerprint@n_rows else NULL,
    has_test = !is.null(rv@sample[["n_test"]]),
    level = rv@confidence_level,
    session_info = x@session_info
  )
} # /rtemis::writeup_context


# %% writeup_version ----
#' Version of R or a package recorded when the model was trained
#'
#' @param session_info `sessionInfo` object or NULL.
#' @param package Character: Package name, or "R".
#'
#' @return Character version, or NULL when not recorded.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_version <- function(session_info, package) {
  if (is.null(session_info)) {
    return(NULL)
  }
  r_version <- paste(
    session_info[["R.version"]][["major"]],
    session_info[["R.version"]][["minor"]],
    sep = "."
  )
  if (package == "R" || package %in% session_info[["basePkgs"]]) {
    return(r_version)
  }
  for (group in c("otherPkgs", "loadedOnly")) {
    entry <- session_info[[group]][[package]]
    if (!is.null(entry)) {
      return(entry[["Version"]])
    }
  }
  NULL
} # /rtemis::writeup_version


# %% Resampling phrases ----

# %% writeup_resampling ----
#' Describe a resampling scheme
#'
#' @param configs List of `ResamplerConfig` objects: The scheme as it ran,
#'   one per fit that used it, all of one type.
#' @param ctx Context list.
#' @param w Collector.
#' @param prefix Character: Key prefix, "outer" or "inner".
#' @param source Character: Path of the configs in the model.
#' @param resamples Optional list of integer vectors: The training indices of
#'   each resample, when recorded.
#'
#' @return Character: A noun phrase, e.g. "5-fold cross-validation, stratified
#'   by outcome class".
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_resampling <- function(
  configs,
  ctx,
  w,
  prefix,
  source,
  resamples = NULL
) {
  config <- configs[[1L]]
  setting <- function(name) writeup_prop(config, name)
  values <- function(name) {
    unlist(lapply(configs, function(cfg) writeup_prop(cfg, name)))
  }
  n <- function() {
    writeup_range(
      w,
      paste0(prefix, "_resamples"),
      values("n_resamples"),
      "integer",
      paste0(source, ".n_resamples")
    )
  }
  train_p <- function() {
    writeup_value(
      w,
      paste0(prefix, "_train_percent"),
      setting("train_p"),
      "percent",
      paste0(source, ".train_p")
    )
  }
  phrase <- switch(
    config@type,
    KFold = paste0(n(), "-fold cross-validation"),
    StratSub = paste0(
      n(),
      " random subsamples, each using ",
      train_p(),
      "% of the cases for training"
    ),
    StratBoot = paste0(
      n(),
      " stratified bootstrap resamples, each drawing ",
      train_p(),
      "% of the cases without replacement and adding cases drawn from that ",
      "subsample, with replacement only when more are added than it holds, ",
      if (!is.null(resamples)) {
        paste0(
          "to a training set of ",
          writeup_range(
            w,
            paste0(prefix, "_training_size"),
            lengths(resamples),
            "count",
            paste0(source, " resamples (lengths)")
          ),
          " cases"
        )
      } else {
        paste0(
          "up to the requested training set size, or the number of cases ",
          "when none is requested or it is smaller than the subsample"
        )
      }
    ),
    Bootstrap = paste0(
      n(),
      " bootstrap resamples, each drawing cases with replacement"
    ),
    LOOCV = "leave-one-out cross-validation",
    Custom = paste0(n(), " user-defined resamples"),
    paste0(n(), " resamples")
  )
  if (config@type %in% c("KFold", "StratSub", "StratBoot")) {
    stratify_var <- setting("stratify_var")
    phrase <- paste0(
      phrase,
      if (!is.null(stratify_var)) {
        paste0(
          ", stratified by ",
          writeup_code(writeup_value(
            w,
            paste0(prefix, "_stratify_var"),
            stratify_var,
            "text",
            paste0(source, ".stratify_var")
          ))
        )
      } else if (ctx[["x"]]@type == "Classification") {
        ", stratified by outcome class"
      } else {
        paste0(
          ", stratified by ",
          writeup_range(
            w,
            paste0(prefix, "_strat_bins"),
            values("strat_n_bins"),
            "integer",
            paste0(source, ".strat_n_bins")
          ),
          " equal-width intervals of the outcome"
        )
      }
    )
  }
  id_strat <- setting("id_strat")
  if (!is.null(id_strat)) {
    phrase <- paste0(
      phrase,
      ", keeping cases that share a value of ",
      writeup_code(writeup_value(
        w,
        paste0(prefix, "_id_strat"),
        id_strat,
        "text",
        paste0(source, ".id_strat")
      )),
      " together"
    )
  }
  seed <- setting("seed")
  if (!is.null(seed)) {
    phrase <- paste0(
      phrase,
      " (random seed ",
      writeup_value(
        w,
        paste0(prefix, "_seed"),
        seed,
        "integer",
        paste0(source, ".seed")
      ),
      ")"
    )
  }
  phrase
} # /rtemis::writeup_resampling


# %% Internal selection ----

# %% writeup_selection ----
#' Describe what a fit selects internally
#'
#' Some algorithms choose a quantity inside the fit: a penalty by internal
#' cross-validation, a number of iterations by early stopping, a tree size on
#' validation cases. A method states how, for the model as fitted.
#'
#' @param hyperparameters `Hyperparameters` object: The authored settings
#'   (the first member, for a set).
#' @param ctx Context list.
#' @param w Collector.
#'
#' @return NULL when the algorithm has no method, so its selection is not
#'   described; otherwise a list with optional `model` and `tuning` sentences
#'   and `collected`, the grid columns the sentences describe.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_selection <- new_generic(
  "writeup_selection",
  "hyperparameters",
  function(hyperparameters, ctx, w) {
    force_supplied()
    S7_dispatch()
  }
)

method(writeup_selection, Hyperparameters) <- function(
  hyperparameters,
  ctx,
  w
) {
  NULL
}

# %% writeup_aggregate ----
# The function tuning used to combine inner-resample values.
writeup_aggregate <- function(ctx) {
  ctx[["tuner"]]@tuner_config@config[["metrics_aggregate_fn"]]
}

method(writeup_selection, GLMNETHyperparameters) <- function(
  hyperparameters,
  ctx,
  w
) {
  if (!is.null(hyperparameters[["lambda"]]) || is.null(ctx[["tuner"]])) {
    return(list())
  }
  nfolds <- if (requireNamespace("glmnet", quietly = TRUE)) {
    formals(glmnet::cv.glmnet)[["nfolds"]]
  } else {
    NULL
  }
  rule <- switch(
    hyperparameters[["which_lambda_cv"]],
    lambda.1se = paste0(
      "the largest value whose cross-validated error was within one ",
      "standard error of the minimum"
    ),
    lambda.min = "the value minimizing the cross-validated error"
  )
  list(
    tuning = paste0(
      "Within each inner training set, the penalty ",
      writeup_code("lambda"),
      " was selected by ",
      if (!is.null(nfolds)) {
        paste0(
          writeup_value(
            w,
            "glmnet_nfolds",
            nfolds,
            "integer",
            "glmnet::cv.glmnet default nfolds"
          ),
          "-fold "
        )
      },
      "cross-validation with ",
      writeup_code("cv.glmnet"),
      " as ",
      rule,
      ", and the ",
      writeup_aggregate(ctx),
      " of the selected values over inner resamples was used."
    ),
    collected = "lambda"
  )
} # /rtemis::writeup_selection.GLMNETHyperparameters

method(writeup_selection, LightGBMHyperparameters) <- function(
  hyperparameters,
  ctx,
  w
) {
  if (!is.null(hyperparameters[["force_nrounds"]]) || is.null(ctx[["tuner"]])) {
    return(list())
  }
  root <- paste0(ctx[["space_root"]], ".")
  stops <- !identical(hyperparameters[["boosting"]], "dart") &&
    !is.null(hyperparameters[["early_stopping_rounds"]])
  max_nrounds <- writeup_value(
    w,
    "max_nrounds",
    hyperparameters[["max_nrounds"]],
    "integer",
    paste0(root, "max_nrounds")
  )
  text <- if (stops) {
    paste0(
      "Within each inner training set, the number of boosting iterations ",
      "was selected by early stopping on the inner validation cases, ",
      "stopping after ",
      writeup_value(
        w,
        "early_stopping_rounds",
        hyperparameters[["early_stopping_rounds"]],
        "integer",
        paste0(root, "early_stopping_rounds")
      ),
      " iterations without improvement and at most ",
      max_nrounds,
      "; the ",
      writeup_aggregate(ctx),
      " of the selected values over inner resamples, rounded, was used."
    )
  } else {
    paste0(
      "Within each inner training set, ",
      max_nrounds,
      " boosting iterations were fitted without early stopping",
      if (identical(hyperparameters[["boosting"]], "dart")) {
        ", which DART boosting does not support"
      },
      ", and the iteration with the best score on the inner validation ",
      "cases was selected; the ",
      writeup_aggregate(ctx),
      " of the selected values over inner resamples, rounded, was used."
    )
  }
  list(tuning = text, collected = "nrounds")
} # /rtemis::writeup_selection.LightGBMHyperparameters

# %% writeup_hal_selection ----
writeup_hal_selection <- function(hyperparameters, nfolds) {
  list(
    model = paste0(
      "In each fit, the lasso penalty was selected by ",
      nfolds,
      "-fold cross-validation with ",
      writeup_code("hal9001"),
      " as ",
      if (isTRUE(hyperparameters[["use_min"]])) {
        "the value minimizing the cross-validated error"
      } else {
        paste0(
          "the largest value whose cross-validated error was within one ",
          "standard error of the minimum"
        )
      },
      "."
    )
  )
} # /rtemis::writeup_hal_selection

method(writeup_selection, HALHyperparameters) <- function(
  hyperparameters,
  ctx,
  w
) {
  writeup_hal_selection(
    hyperparameters,
    writeup_value(
      w,
      "hal_nfolds",
      hyperparameters[["nfolds"]],
      "integer",
      paste0(ctx[["space_root"]], ".nfolds")
    )
  )
} # /rtemis::writeup_selection.HALHyperparameters

method(writeup_selection, MonotonicHALHyperparameters) <- function(
  hyperparameters,
  ctx,
  w
) {
  folds <- vapply(
    ctx[["fits"]],
    function(f) {
      as.numeric(monotonic_hal_nfolds(
        hyperparameters[["nfolds"]],
        length(f@y_training)
      ))
    },
    numeric(1L)
  )
  writeup_hal_selection(
    hyperparameters,
    writeup_range(
      w,
      "hal_nfolds",
      folds,
      "integer",
      paste0(
        "derived: ",
        ctx[["space_root"]],
        ".nfolds, reduced to what the training set supports"
      )
    )
  )
} # /rtemis::writeup_selection.MonotonicHALHyperparameters

method(writeup_selection, LINADHyperparameters) <- function(
  hyperparameters,
  ctx,
  w
) {
  if (isTRUE(hyperparameters[["force_max_leaves"]])) {
    return(list())
  }
  if ("best_n_leaves" %in% names(ctx[["grid"]])) {
    return(list(
      tuning = paste0(
        "Within each inner training set, the number of leaves was selected ",
        "as the tree size minimizing the loss on the inner validation cases; ",
        "the ",
        writeup_aggregate(ctx),
        " of the selected sizes over inner resamples, rounded, was applied ",
        "to the tree grown on the whole training set."
      ),
      collected = "best_n_leaves"
    ))
  }
  list(
    model = paste0(
      "With no validation cases, the tree keeps every leaf it grows, up to ",
      writeup_code("max_leaves"),
      "."
    )
  )
} # /rtemis::writeup_selection.LINADHyperparameters


# %% Methods ----

# %% writeup_data ----
writeup_data <- function(ctx, w) {
  x <- ctx[["x"]]
  s <- ctx[["review"]]@sample
  outcome <- if (!is.null(ctx[["outcome"]])) {
    paste0(
      ", ",
      writeup_code(writeup_value(
        w,
        "outcome_name",
        ctx[["outcome"]],
        "text",
        "data_fingerprint.column_names (last)"
      )),
      ","
    )
  } else {
    ""
  }
  predictors <- if (!is.null(s[["n_predictors"]])) {
    paste0(
      writeup_value(
        w,
        "n_predictors",
        s[["n_predictors"]],
        "count",
        "review.sample.n_predictors"
      ),
      " predictors"
    )
  }
  cases <- if (ctx[["resampled"]]) {
    if (!is.null(ctx[["n_cases"]])) {
      paste0(
        writeup_value(
          w,
          "n_cases",
          ctx[["n_cases"]],
          "count",
          "data_fingerprint.n_rows"
        ),
        " cases"
      )
    }
  } else {
    paste0(
      writeup_value(
        w,
        "n_training",
        s[["n_training"]],
        "count",
        "review.sample.n_training"
      ),
      " training cases",
      if (ctx[["has_test"]]) {
        paste0(
          " and ",
          writeup_value(
            w,
            "n_test",
            s[["n_test"]],
            "count",
            "review.sample.n_test"
          ),
          " test cases"
        )
      }
    )
  }
  size <- if (!is.null(cases) && !is.null(predictors)) {
    paste0(
      "The data comprised ",
      cases,
      if (ctx[["has_test"]] && !ctx[["resampled"]]) ", with " else " and ",
      predictors,
      "."
    )
  } else if (!is.null(cases)) {
    paste0("The data comprised ", cases, ".")
  } else if (!is.null(predictors)) {
    paste0("The data comprised ", predictors, ".")
  }
  outcome_sentence <- if (x@type == "Classification") {
    levels <- ctx[["levels"]]
    paste0(
      "The outcome",
      outcome,
      " had ",
      writeup_value(
        w,
        "n_classes",
        length(levels),
        "count",
        "derived: number of levels of y_training"
      ),
      " classes: ",
      writeup_list(vapply(
        seq_along(levels),
        function(i) {
          writeup_code(writeup_value(
            w,
            paste0("class_", i),
            levels[[i]],
            "text",
            paste0(ctx[["path"]]("y_training"), " level ", i)
          ))
        },
        character(1L)
      )),
      "."
    )
  } else {
    paste0("The outcome", outcome, " was continuous.")
  }
  writeup_section(
    "methods",
    "Data",
    list(paste(c(size, outcome_sentence), collapse = " "))
  )
} # /rtemis::writeup_data


# %% writeup_preprocessing ----
writeup_preprocessing <- function(ctx, w) {
  config <- ctx[["preprocessor_config"]]
  decompositions <- ctx[["decompositions"]]
  steps <- if (!is.null(config)) desc_preprocessor_steps(config) else NULL
  if (length(steps) == 0L && length(decompositions) == 0L) {
    return(NULL)
  }
  sentences <- character()
  if (length(steps) > 0L) {
    sentences <- c(
      sentences,
      paste0("Preprocessing comprised ", writeup_list(steps), ".")
    )
  }
  if (length(decompositions) > 0L) {
    decomposition <- decompositions[[1L]]
    algorithm <- decomposition@algorithm
    package <- decom_algorithms[["package"]][
      match(algorithm, decom_algorithms[["name"]])
    ]
    cite <- if (!is.na(package) && package != "stats") {
      paste0(
        " ",
        writeup_cite_package(
          w,
          package,
          writeup_version(ctx[["session_info"]], package)
        )
      )
    } else {
      ""
    }
    n_features <- vapply(
      decompositions,
      function(d) as.numeric(length(d@config@features)),
      numeric(1L)
    )
    n_components <- vapply(
      decompositions,
      function(d) as.numeric(NCOL(d@transformed)),
      numeric(1L)
    )
    sentences <- c(
      sentences,
      paste0(
        get_decom_desc(algorithm),
        " (",
        algorithm,
        ")",
        cite,
        " reduced ",
        writeup_range(
          w,
          "decomposition_features",
          n_features,
          "count",
          ctx[["path"]]("decomposition.config.features (length)")
        ),
        " numeric features to ",
        writeup_range(
          w,
          "decomposition_components",
          n_components,
          "count",
          ctx[["path"]]("decomposition.transformed (columns)")
        ),
        " ",
        writeup_plural(max(n_components), "component"),
        if (ctx[["resampled"]] && length(unique(n_components)) > 1L) {
          " across resamples"
        },
        "."
      )
    )
  }
  fitted <- c(
    if (length(steps) > 0L) "preprocessing",
    if (length(decompositions) > 0L) "decomposition"
  )
  sentences <- c(
    sentences,
    paste0(
      writeup_capitalize(writeup_list(fitted)),
      if (length(fitted) > 1L) " were" else " was",
      " fitted on the training cases",
      if (ctx[["resampled"]]) " of each resample",
      if (ctx[["has_test"]]) " and applied to the test cases",
      "."
    )
  )
  writeup_section(
    "methods",
    "Preprocessing",
    list(paste(sentences, collapse = " "))
  )
} # /rtemis::writeup_preprocessing


# %% writeup_setting_value ----
#' Token for a hyperparameter value
#'
#' @return Character token, or NULL for a value that is not a scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_setting_value <- function(w, key, value, source) {
  if (length(value) != 1L) {
    return(NULL)
  }
  if (is.numeric(value)) {
    whole <- is.integer(value) || (is.finite(value) && value == round(value))
    writeup_value(w, key, value, if (whole) "integer" else "quantity", source)
  } else {
    writeup_value(w, key, as.character(value), "text", source)
  }
} # /rtemis::writeup_setting_value


# %% writeup_default_hyperparameters ----
#' Default hyperparameter values of an algorithm
#'
#' @param algorithm Character: Algorithm name.
#'
#' @return Named list of default values, or NULL when the algorithm's setup
#'   function needs arguments.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_default_hyperparameters <- function(algorithm) {
  setup <- get0(paste0("setup_", algorithm), mode = "function")
  if (is.null(setup)) {
    return(NULL)
  }
  default <- tryCatch(setup(), error = function(e) NULL)
  if (is.null(default)) NULL else default@hyperparameters
} # /rtemis::writeup_default_hyperparameters


# %% writeup_model ----
writeup_model <- function(ctx, w) {
  x <- ctx[["x"]]
  algorithm <- x@algorithm
  packages <- setdiff(SUPERVISED_BACKENDS[[algorithm]], "stats")
  cites <- vapply(
    packages,
    function(p) {
      writeup_cite_package(w, p, writeup_version(ctx[["session_info"]], p))
    },
    character(1L)
  )
  description <- desc_alg(algorithm)
  sentence <- paste0(
    description,
    if (!grepl(algorithm, description, ignore.case = TRUE)) {
      paste0(" (", algorithm, ")")
    },
    " was used for ",
    tolower(x@type),
    if (length(packages) > 0L) {
      paste0(
        ", fitted with the ",
        writeup_list(paste(packages, cites)),
        if (length(packages) > 1L) " packages" else " package"
      )
    },
    "."
  )
  settings <- NULL
  if (!ctx[["is_set"]]) {
    hp <- ctx[["search_space"]]
    values <- hp@hyperparameters
    defaults <- writeup_default_hyperparameters(algorithm)
    untuned <- if (is.null(ctx[["tuner"]])) {
      "Hyperparameters were"
    } else {
      "Hyperparameters that were not tuned were"
    }
    names <- hp@tunable_hyperparameters
    names <- names[vapply(
      names,
      function(nm) {
        v <- values[[nm]]
        !is.null(v) &&
          !is_candidates(v) &&
          length(v) == 1L &&
          (is.null(defaults) || !identical(v, defaults[[nm]]))
      },
      logical(1L)
    )]
    settings <- if (length(names) > 0L) {
      paste0(
        untuned,
        if (is.null(defaults)) {
          " set to "
        } else {
          " at their rtemis defaults, except "
        },
        writeup_list(vapply(
          names,
          function(nm) {
            paste0(
              writeup_code(nm),
              " = ",
              writeup_setting_value(
                w,
                writeup_key("hp", nm),
                values[[nm]],
                paste0(ctx[["space_root"]], ".", nm)
              )
            )
          },
          character(1L)
        )),
        "."
      )
    } else if (!is.null(defaults)) {
      paste0(untuned, " at their rtemis defaults.")
    }
  }
  writeup_section(
    "methods",
    "Model",
    list(paste(
      c(sentence, settings, ctx[["selection"]][["model"]]),
      collapse = " "
    ))
  )
} # /rtemis::writeup_model


# %% writeup_searched ----
#' Hyperparameters the user asked the search to vary
#'
#' Candidates marked with `tune_over()`, over every member of a set. Values a
#' fit selects internally and the tuner collects into the grid are not
#' searched.
#'
#' @param ctx Context list.
#'
#' @return Character vector of names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_searched <- function(ctx) {
  members <- if (ctx[["is_set"]]) {
    ctx[["set"]]@variants
  } else {
    list(ctx[["search_space"]])
  }
  unique(unlist(lapply(members, function(m) {
    values <- m@hyperparameters[m@tunable_hyperparameters]
    names(values)[vapply(values, is_candidates, logical(1L))]
  })))
} # /rtemis::writeup_searched


# %% writeup_variant ----
#' One configuration of a hyperparameter set
#'
#' @param variant Character: Configuration name.
#' @param ctx Context list.
#' @param searched Character: Hyperparameters searched over.
#' @param differs Character: Hyperparameters whose fixed values differ
#'   between members.
#' @param w Collector.
#'
#' @return Character: The name with its distinguishing settings, e.g.
#'   "`{variant_deep}` (`maxdepth` {..} or {..})".
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_variant <- function(variant, ctx, searched, differs, w) {
  member <- ctx[["set"]]@variants[[variant]]
  root <- paste0(
    if (ctx[["resampled"]]) "hyperparameters" else "tuner.searched_set",
    ".variants.",
    variant
  )
  key <- writeup_key("variant", variant)
  name <- writeup_code(writeup_value(
    w,
    key,
    variant,
    "text",
    paste0(root, " (name)")
  ))
  settings <- unlist(lapply(c(differs, searched), function(nm) {
    value <- member@hyperparameters[[nm]]
    if (is.null(value)) {
      return(NULL)
    }
    values <- if (is_candidates(value)) candidate_values(value) else list(value)
    tokens <- vapply(
      seq_along(values),
      function(i) {
        writeup_setting_value(
          w,
          writeup_key(key, nm, i),
          values[[i]],
          paste0(root, ".", nm, if (length(values) > 1L) paste0("[", i, "]"))
        ) %||%
          ""
      },
      character(1L)
    )
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) == 0L) {
      return(NULL)
    }
    paste0(
      writeup_code(nm),
      if (length(tokens) == 1L) " = " else " ",
      writeup_list(tokens, "or")
    )
  }))
  paste0(
    name,
    if (length(settings) > 0L) {
      paste0(" (", paste(settings, collapse = ", "), ")")
    }
  )
} # /rtemis::writeup_variant


# %% writeup_set_differences ----
#' Hyperparameters whose fixed values differ between the members of a set
#'
#' @param set `HyperparametersSet` object.
#' @param searched Character: Hyperparameters searched over.
#'
#' @return Character vector of names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_set_differences <- function(set, searched) {
  members <- set@variants
  names <- setdiff(
    unique(unlist(lapply(members, function(m) m@tunable_hyperparameters))),
    searched
  )
  names[vapply(
    names,
    function(nm) {
      values <- lapply(members, function(m) m@hyperparameters[[nm]])
      scalar <- vapply(
        values,
        function(v) length(v) == 1L && !is_candidates(v),
        logical(1L)
      )
      all(scalar) &&
        !all(vapply(values, identical, logical(1L), values[[1L]]))
    },
    logical(1L)
  )]
} # /rtemis::writeup_set_differences


# %% writeup_tuning ----
writeup_tuning <- function(ctx, w) {
  tuner <- ctx[["tuner"]]
  if (is.null(tuner)) {
    return(NULL)
  }
  config <- tuner@tuner_config
  grid <- ctx[["grid"]]
  searched <- writeup_searched(ctx)
  each <- if (ctx[["resampled"]]) {
    "each outer training set"
  } else {
    "the training set"
  }
  sentences <- character()
  if (ctx[["is_set"]]) {
    variants <- names(ctx[["set"]]@variants)
    differs <- writeup_set_differences(ctx[["set"]], searched)
    sentences <- c(
      sentences,
      paste0(
        "Hyperparameters were tuned by grid search over ",
        writeup_value(
          w,
          "n_variants",
          length(variants),
          "count",
          "derived: number of members of the hyperparameter set"
        ),
        " configurations: ",
        writeup_list(vapply(
          variants,
          writeup_variant,
          character(1L),
          ctx = ctx,
          searched = searched,
          differs = differs,
          w = w
        )),
        "."
      )
    )
  } else if (length(searched) > 0L) {
    hp <- ctx[["search_space"]]
    ranges <- vapply(
      searched,
      function(nm) {
        values <- candidate_values(hp[[nm]])
        paste0(
          writeup_code(nm),
          " (",
          writeup_list(vapply(
            seq_along(values),
            function(i) {
              writeup_setting_value(
                w,
                writeup_key("grid", nm, i),
                values[[i]],
                paste0(ctx[["space_root"]], ".", nm, "[", i, "]")
              ) %||%
                ""
            },
            character(1L)
          )),
          ")"
        )
      },
      character(1L)
    )
    sentences <- c(
      sentences,
      paste0(
        "Hyperparameters were tuned by grid search over ",
        writeup_list(ranges),
        "."
      )
    )
  }
  if (identical(config@config[["search_type"]], "randomized")) {
    n_eligible <- tuner@tuning_results[["n_combinations"]]
    sentences <- c(
      sentences,
      paste0(
        "A random ",
        writeup_value(
          w,
          "grid_evaluated",
          NROW(grid),
          "count",
          paste0(ctx[["tuner_root"]], ".tuning_results.param_grid (rows)")
        ),
        if (!is.null(n_eligible)) {
          paste0(
            " of the ",
            writeup_value(
              w,
              "grid_eligible",
              n_eligible,
              "count",
              paste0(ctx[["tuner_root"]], ".tuning_results.n_combinations")
            ),
            " combinations"
          )
        } else {
          " combinations"
        },
        " were evaluated."
      )
    )
  }
  inner_configs <- lapply(
    Filter(Negate(is.null), ctx[["tuners"]]),
    function(t) {
      t@tuning_results[["resampler_config"]] %||%
        t@tuner_config@config[["resampler_config"]]
    }
  )
  inner <- writeup_resampling(
    inner_configs,
    ctx,
    w,
    "inner",
    paste0(ctx[["tuner_root"]], ".tuning_results.resampler_config")
  )
  metric <- config@config[["metric"]]
  aggregate <- config@config[["metrics_aggregate_fn"]]
  sentences <- c(
    sentences,
    paste0(
      "Within ",
      each,
      ", tuning used ",
      inner,
      if (NROW(grid) > 1L) {
        paste0(
          "; the combination with the ",
          if (isTRUE(config@config[["maximize"]])) "highest " else "lowest ",
          aggregate,
          " ",
          writeup_metric_label(metric),
          " over inner resamples was selected"
        )
      },
      ", and the model was refit on the whole ",
      if (ctx[["resampled"]]) "outer training set" else "training set",
      " with the selected values."
    )
  )
  writeup_section(
    "methods",
    "Hyperparameter tuning",
    list(paste(
      c(sentences, ctx[["selection"]][["tuning"]]),
      collapse = " "
    ))
  )
} # /rtemis::writeup_tuning


# %% writeup_evaluation ----
writeup_evaluation <- function(ctx, w) {
  x <- ctx[["x"]]
  inside <- c(
    if (!is.null(ctx[["preprocessor_config"]])) "preprocessing",
    if (length(ctx[["decompositions"]]) > 0L) "decomposition",
    if (!is.null(ctx[["tuner"]])) "hyperparameter tuning"
  )
  text <- if (ctx[["resampled"]]) {
    resampler <- x@outer_resampler
    paste0(
      "Performance was estimated by ",
      writeup_resampling(
        list(resampler@config),
        ctx,
        w,
        "outer",
        "outer_resampler.config",
        resamples = resampler@resamples
      ),
      ". In each resample, ",
      writeup_list(c(inside, "model fitting")),
      " used only the training cases, and the model was evaluated on the ",
      "cases not used for training."
    )
  } else if (ctx[["has_test"]]) {
    paste0(
      "Performance was evaluated on the test set, which was not used for ",
      "fitting the model",
      if (length(inside) > 0L) paste0(" or for ", writeup_list(inside)),
      "."
    )
  } else {
    "Performance was evaluated on the training cases only."
  }
  writeup_section("methods", "Evaluation", list(text))
} # /rtemis::writeup_evaluation


# %% WRITEUP_REFERENCE_TEXT ----
# Each baseline reference as it reads in a sentence.
WRITEUP_REFERENCE_TEXT <- c(
  majority_class = "always predicting the most common training class",
  chance = "the chance level",
  training_prevalence = "a constant forecast of the training proportion of the positive class",
  training_mean = "predicting the training mean"
)


# %% writeup_measures ----
writeup_measures <- function(ctx, w) {
  rv <- ctx[["review"]]
  metrics <- rv@performance[["metric"]]
  measured <- paste0(
    "Performance was measured by ",
    writeup_list(writeup_metric_label(metrics)),
    if (!is.null(ctx[["positive_class"]])) {
      paste0(
        ", with ",
        writeup_code(writeup_value(
          w,
          "positive_class",
          ctx[["positive_class"]],
          "text",
          ctx[["path"]]("metrics_test.metrics.positive_class")
        )),
        " as the positive class"
      )
    },
    "."
  )
  definitions <- c(
    if ("balanced_accuracy" %in% metrics) {
      "Balanced accuracy is the mean over classes of the proportion of each class predicted correctly."
    },
    if ("brier_score" %in% metrics) {
      "The Brier score is the mean squared difference between the predicted probability of the positive class and the outcome."
    },
    if ("rsq" %in% metrics) {
      "R\u00b2 is one minus the ratio of the residual sum of squares to the total sum of squares of the cases evaluated."
    }
  )
  b <- rv@baseline
  statistics <- if (!ctx[["has_test"]]) {
    NULL
  } else if (ctx[["resampled"]]) {
    paste0(
      "Performance is reported as the mean and standard deviation over the ",
      "resamples. The resamples share training cases, so their estimates are ",
      "not independent, and no confidence intervals or tests were computed. ",
      "In each resample, the model was compared with a baseline fitted to the ",
      "training cases of that resample: ",
      writeup_list(vapply(
        unique(b[["reference"]]),
        function(ref) {
          paste0(
            WRITEUP_REFERENCE_TEXT[[ref]],
            " (",
            writeup_list(writeup_metric_label(b[["metric"]][
              b[["reference"]] == ref
            ])),
            ")"
          )
        },
        character(1L)
      )),
      "."
    )
  } else {
    writeup_statistics_single(
      ctx,
      w,
      writeup_value(
        w,
        "confidence_percent",
        ctx[["level"]],
        "percent",
        "review.confidence_level"
      )
    )
  }
  writeup_section(
    "methods",
    "Performance measures and statistical analysis",
    list(paste(c(measured, definitions), collapse = " "), statistics)
  )
} # /rtemis::writeup_measures


# %% writeup_statistics_single ----
#' Statistical methods of a single-split review
#'
#' States the interval method of each metric whose interval `review()`
#' obtained, names the metrics without one, and states the method of each
#' baseline comparison.
#'
#' @return Character paragraph.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_statistics_single <- function(ctx, w, level) {
  p <- ctx[["review"]]@performance
  b <- ctx[["review"]]@baseline
  obtained <- p[["metric"]][!is.na(p[["lower"]]) & !is.na(p[["upper"]])]
  proportions <- intersect(
    c("accuracy", "sensitivity", "specificity", "ppv", "npv"),
    obtained
  )
  mean_losses <- intersect(c("mae", "mse"), obtained)
  intervals <- c(
    if (length(proportions) > 0L) {
      paste0(
        "by the Clopper-Pearson method ",
        writeup_cite_method(w, "clopper_pearson"),
        " for ",
        writeup_list(writeup_metric_label(proportions))
      )
    },
    if ("balanced_accuracy" %in% obtained) {
      paste0(
        "by a normal approximation with per-class binomial variances, each ",
        "computed with one success and one failure added, for balanced accuracy"
      )
    },
    if ("auc" %in% obtained) {
      paste0(
        "by the method of DeLong et al. ",
        writeup_cite_method(w, "delong"),
        " for AUC"
      )
    },
    if (length(mean_losses) > 0L) {
      paste0(
        "as t intervals of the mean per-case error for ",
        writeup_list(writeup_metric_label(mean_losses)),
        if ("rmse" %in% obtained) {
          ", the RMSE interval being the square root of the MSE interval"
        }
      )
    }
  )
  # Metrics for which the review computes an interval, and of those, the ones
  # it could not obtain on these test cases.
  interval_metrics <- c(
    "accuracy",
    "sensitivity",
    "specificity",
    "ppv",
    "npv",
    "balanced_accuracy",
    "auc",
    "mae",
    "mse",
    "rmse"
  )
  missing <- setdiff(intersect(interval_metrics, p[["metric"]]), obtained)
  out <- c(
    if (length(intervals) > 0L) {
      paste0(
        writeup_capitalize(level),
        "% confidence intervals of test performance were computed ",
        paste(intervals, collapse = "; "),
        "."
      )
    },
    if (length(missing) > 0L) {
      paste0(
        "No confidence interval could be computed for ",
        writeup_list(writeup_metric_label(missing)),
        " on these test cases."
      )
    }
  )
  comparisons <- character()
  for (i in seq_len(NROW(b))) {
    metric <- b[["metric"]][[i]]
    label <- writeup_metric_label(metric)
    reference <- WRITEUP_REFERENCE_TEXT[[b[["reference"]][[i]]]]
    comparisons <- c(
      comparisons,
      switch(
        b[["method"]][[i]],
        exact_mcnemar = paste0(
          writeup_capitalize(label),
          " was compared with ",
          reference,
          " by the exact McNemar test ",
          writeup_cite_method(w, "mcnemar"),
          "."
        ),
        interval_vs_reference = NULL,
        paired_t = paste0(
          writeup_capitalize(label),
          " was compared with ",
          reference,
          " through a t interval of the per-case reduction in ",
          if (metric == "mae") "absolute error" else "squared error",
          "."
        ),
        descriptive = paste0(
          writeup_capitalize(label),
          " is reported beside that of ",
          reference,
          ", without inference."
        )
      )
    )
  }
  chance <- which(b[["method"]] == "interval_vs_reference")
  if (length(chance) > 0L) {
    labels <- writeup_metric_label(b[["metric"]][chance])
    levels <- vapply(
      chance,
      function(i) {
        metric <- b[["metric"]][[i]]
        writeup_value(
          w,
          writeup_key("chance", metric),
          b[["baseline"]][[i]],
          "metric",
          paste0("review.baseline[", metric, "].baseline")
        )
      },
      character(1L)
    )
    several <- length(chance) > 1L
    comparisons <- c(
      comparisons,
      paste0(
        writeup_capitalize(writeup_list(labels)),
        if (several) " were" else " was",
        " compared with ",
        if (several) "their chance levels (" else "its chance level (",
        writeup_list(levels),
        if (several) ", respectively)" else ")",
        " through ",
        if (several) "their" else "its",
        " confidence ",
        if (several) "intervals." else "interval."
      )
    )
  }
  paste(
    c(out, comparisons, "All tests and intervals are two-sided."),
    collapse = " "
  )
} # /rtemis::writeup_statistics_single


# %% writeup_software ----
writeup_software <- function(ctx, w) {
  r_version <- writeup_version(ctx[["session_info"]], "R")
  rtemis_version <- writeup_version(ctx[["session_info"]], "rtemis")
  missing <- c(
    if (is.null(r_version)) "R",
    if (is.null(rtemis_version)) "rtemis"
  )
  version_token <- function(package, version) {
    if (is.null(version)) {
      return("")
    }
    paste0(
      " ",
      writeup_value(
        w,
        writeup_key("version", package),
        version,
        "text",
        paste0("session_info: ", package)
      )
    )
  }
  packages <- setdiff(SUPERVISED_BACKENDS[[ctx[["x"]]@algorithm]], "stats")
  backend <- vapply(
    packages,
    function(p) {
      version <- writeup_version(ctx[["session_info"]], p)
      if (is.null(version)) {
        missing <<- c(missing, p)
      }
      paste0(
        p,
        version_token(p, version),
        " ",
        writeup_cite_package(w, p, version)
      )
    },
    character(1L)
  )
  if (length(missing) > 0L) {
    w[["not_reported"]] <- c(
      w[["not_reported"]],
      paste0(
        "The version of ",
        writeup_list(missing),
        " used to train the model."
      )
    )
  }
  text <- paste0(
    "Analyses were performed in R",
    version_token("R", r_version),
    " ",
    writeup_cite_package(w, "R", r_version),
    " with the rtemis package",
    version_token("rtemis", rtemis_version),
    " ",
    writeup_cite_package(w, "rtemis", rtemis_version),
    if (length(backend) > 0L) {
      paste0(" and ", writeup_list(backend))
    },
    "."
  )
  writeup_section("methods", "Software", list(text))
} # /rtemis::writeup_software


# %% Results ----

# %% writeup_results_sample ----
writeup_results_sample <- function(ctx, w) {
  rv <- ctx[["review"]]
  s <- rv@sample
  sentences <- character()
  if (ctx[["resampled"]]) {
    sentences <- c(
      sentences,
      paste0(
        "Outer resampling completed ",
        writeup_value(
          w,
          "n_resamples",
          s[["n_resamples"]],
          "count",
          "review.sample.n_resamples"
        ),
        " of ",
        writeup_value(
          w,
          "n_resamples_requested",
          s[["n_resamples_requested"]],
          "count",
          "review.sample.n_resamples_requested"
        ),
        " resamples; the smallest training set had ",
        writeup_value(
          w,
          "n_training_min",
          s[["n_training"]],
          "count",
          "review.sample.n_training"
        ),
        " cases."
      )
    )
  }
  counts <- rv@class_counts
  if (!is.null(counts)) {
    count <- function(column, i) {
      writeup_value(
        w,
        writeup_key("count", column, i),
        counts[[column]][[i]],
        "count",
        paste0("review.class_counts[", i, "].", column)
      )
    }
    clause <- function(column) {
      writeup_list(vapply(
        seq_len(NROW(counts)),
        function(i) {
          paste0(count(column, i), " ", writeup_code(paste0("{class_", i, "}")))
        },
        character(1L)
      ))
    }
    numbers <- function(column) {
      writeup_list(vapply(
        seq_len(NROW(counts)),
        function(i) count(column, i),
        character(1L)
      ))
    }
    covers_all <- ctx[["resampled"]] &&
      !is.null(ctx[["n_cases"]]) &&
      identical(s[["n_test"]], s[["n_test_cases"]]) &&
      identical(s[["n_test_cases"]], ctx[["n_cases"]])
    sentences <- c(
      sentences,
      if (!ctx[["resampled"]]) {
        paste0(
          "The training set included ",
          clause("training"),
          " cases",
          if (ctx[["has_test"]]) {
            paste0(", and the test set ", numbers("test"), ", respectively")
          },
          "."
        )
      } else if (covers_all) {
        paste0("The data included ", clause("test"), " cases.")
      } else {
        paste0(
          "The smallest training sets included ",
          clause("training"),
          " cases."
        )
      }
    )
  }
  writeup_section("results", "Sample", list(paste(sentences, collapse = " ")))
} # /rtemis::writeup_results_sample


# %% writeup_headline_metrics ----
# The metrics stated in the text, by kind of outcome; the table holds all.
writeup_headline_metrics <- function(ctx) {
  if (ctx[["x"]]@type == "Regression") {
    c("rmse", "mae", "rsq")
  } else if (length(ctx[["levels"]]) == 2L) {
    c("balanced_accuracy", "auc", "sensitivity", "specificity", "brier_score")
  } else {
    c("balanced_accuracy", "accuracy")
  }
} # /rtemis::writeup_headline_metrics


# %% WRITEUP_TABLE_COLUMNS ----
# Performance columns of the review that the table shows.
WRITEUP_TABLE_COLUMNS <- c(
  "training",
  "training_sd",
  "test",
  "test_sd",
  "lower",
  "upper"
)


# %% writeup_results_performance ----
writeup_results_performance <- function(ctx, w) {
  rv <- ctx[["review"]]
  p <- rv@performance
  # Every table cell is a value, keyed table_<column>_<metric>.
  for (i in seq_len(NROW(p))) {
    for (column in WRITEUP_TABLE_COLUMNS) {
      v <- p[[column]][[i]]
      if (!is.na(v)) {
        writeup_value(
          w,
          paste0("table_", column, "_", p[["metric"]][[i]]),
          v,
          "metric",
          paste0("review.performance[", p[["metric"]][[i]], "].", column)
        )
      }
    }
  }
  table <- writeup_value(
    w,
    "table_performance",
    1L,
    "integer",
    "writeup: table number"
  )
  metrics <- intersect(writeup_headline_metrics(ctx), p[["metric"]])
  value <- function(metric, column) {
    v <- p[[column]][[match(metric, p[["metric"]])]]
    if (is.na(v)) {
      return(NULL)
    }
    paste0("{table_", column, "_", metric, "}")
  }
  clause <- function(metric, column, sd_column, interval = FALSE) {
    v <- value(metric, column)
    if (is.null(v)) {
      return(NULL)
    }
    detail <- if (ctx[["resampled"]]) {
      sd <- value(metric, sd_column)
      if (!is.null(sd)) paste0(" (SD ", sd, ")")
    } else if (interval) {
      lo <- value(metric, "lower")
      hi <- value(metric, "upper")
      if (!is.null(lo) && !is.null(hi)) {
        paste0(" ({confidence_percent}% CI ", lo, "\u2013", hi, ")")
      }
    }
    paste0(writeup_metric_label(metric), " was ", v, detail)
  }
  sentences <- paste0(
    "Performance on ",
    if (ctx[["has_test"]]) "training and test cases" else "the training cases",
    " is shown in Table ",
    table,
    "."
  )
  if (ctx[["has_test"]]) {
    test <- unlist(lapply(metrics, clause, "test", "test_sd", interval = TRUE))
    if (length(test) > 0L) {
      sentences <- c(
        sentences,
        paste0(
          if (ctx[["resampled"]]) {
            "Averaged over resamples, on the test cases, "
          } else {
            "On the test set, "
          },
          writeup_list(test),
          "."
        )
      )
    }
  }
  training <- if (length(metrics) > 0L) {
    clause(metrics[[1L]], "training", "training_sd")
  }
  if (!is.null(training)) {
    sentences <- c(
      sentences,
      paste0(
        if (ctx[["resampled"]]) {
          "Averaged over resamples, on the training cases, "
        } else {
          "On the training set, "
        },
        training,
        "."
      )
    )
  }
  writeup_section(
    "results",
    "Performance",
    list(paste(sentences, collapse = " "))
  )
} # /rtemis::writeup_results_performance


# %% writeup_results_baseline ----
writeup_results_baseline <- function(ctx, w) {
  b <- ctx[["review"]]@baseline
  if (is.null(b) || NROW(b) == 0L) {
    return(NULL)
  }
  num <- function(i, column, kind = "metric") {
    v <- b[[column]][[i]]
    if (is.na(v)) {
      return(NULL)
    }
    metric <- b[["metric"]][[i]]
    writeup_value(
      w,
      writeup_key("baseline", metric, column),
      v,
      kind,
      paste0("review.baseline[", metric, "].", column)
    )
  }
  sentence <- function(i) {
    metric <- b[["metric"]][[i]]
    label <- writeup_metric_label(metric)
    reference <- WRITEUP_REFERENCE_TEXT[[b[["reference"]][[i]]]]
    outcome <- b[["outcome"]][[i]]
    if (ctx[["resampled"]]) {
      compared <- b[["resamples_compared"]][[i]]
      if (is.na(compared) || compared == 0L) {
        return(paste0(
          writeup_capitalize(label),
          " could not be compared with ",
          reference,
          " in any resample."
        ))
      }
      model <- num(i, "model")
      baseline <- num(i, "baseline")
      return(paste0(
        "The model's ",
        label,
        " improved on ",
        reference,
        " in ",
        num(i, "resamples_better", "count"),
        " of the ",
        num(i, "resamples_compared", "count"),
        " ",
        writeup_plural(compared, "resample"),
        " in which both were defined",
        if (!is.null(model) && !is.null(baseline)) {
          paste0(" (mean ", model, " against ", baseline, ")")
        },
        "."
      ))
    }
    method <- b[["method"]][[i]]
    lower_is_better <- method == "paired_t"
    relation <- if (is.na(outcome)) {
      NULL
    } else if (outcome == "indistinguishable") {
      "did not differ significantly from"
    } else if ((outcome == "better") != lower_is_better) {
      "was higher than"
    } else {
      "was lower than"
    }
    switch(
      method,
      exact_mcnemar = paste0(
        "Test ",
        label,
        " (",
        num(i, "model"),
        ") ",
        relation %||% "was compared with",
        " that of ",
        reference,
        " (",
        num(i, "baseline"),
        "; exact McNemar test, ",
        num(i, "p_value", "p_value"),
        ")."
      ),
      interval_vs_reference = {
        lo <- num(i, "model_lower")
        hi <- num(i, "model_upper")
        if (is.null(lo) || is.null(hi) || is.na(outcome)) {
          paste0(
            "Test ",
            label,
            " was ",
            num(i, "model"),
            ", against a chance level of ",
            num(i, "baseline"),
            "; its confidence interval could not be computed."
          )
        } else {
          paste0(
            "The {confidence_percent}% confidence interval of test ",
            label,
            " (",
            lo,
            "\u2013",
            hi,
            ") ",
            switch(
              outcome,
              better = "lay above",
              worse = "lay below",
              indistinguishable = "included"
            ),
            " its chance level of ",
            num(i, "baseline"),
            "."
          )
        }
      },
      paired_t = paste0(
        "Test ",
        label,
        " (",
        num(i, "model"),
        ") ",
        relation %||% "was compared with",
        " that of ",
        reference,
        " (",
        num(i, "baseline"),
        ")",
        {
          d <- num(i, "difference")
          dl <- num(i, "difference_lower")
          du <- num(i, "difference_upper")
          if (!is.null(d) && !is.null(dl) && !is.null(du)) {
            paste0(
              ": mean reduction ",
              d,
              " ({confidence_percent}% CI ",
              dl,
              "\u2013",
              du,
              ")"
            )
          } else {
            ""
          }
        },
        {
          sk <- num(i, "skill")
          if (!is.null(sk)) paste0(", skill score ", sk) else ""
        },
        "."
      ),
      descriptive = paste0(
        "Test ",
        label,
        " was ",
        num(i, "model"),
        ", and that of ",
        reference,
        " was ",
        num(i, "baseline"),
        "."
      )
    )
  }
  writeup_section(
    "results",
    "Comparison with a baseline",
    list(paste(
      vapply(seq_len(NROW(b)), sentence, character(1L)),
      collapse = " "
    ))
  )
} # /rtemis::writeup_results_baseline


# %% writeup_results_tuning ----
writeup_results_tuning <- function(ctx, w) {
  tuners <- Filter(Negate(is.null), ctx[["tuners"]])
  if (length(tuners) == 0L) {
    return(NULL)
  }
  root <- paste0(ctx[["tuner_root"]], ".best_hyperparameters")
  selected <- lapply(tuners, function(t) t@best_hyperparameters)
  names <- unique(unlist(lapply(selected, names)))
  variants <- unlist(lapply(tuners, function(t) t@best_variant))
  clauses <- character()
  if (length(variants) > 0L) {
    counts <- table(variants)
    clauses <- c(
      clauses,
      paste0(
        "configuration ",
        writeup_list(vapply(
          names(counts),
          function(v) {
            paste0(
              writeup_code(writeup_value(
                w,
                writeup_key("variant", v),
                v,
                "text",
                paste0(ctx[["tuner_root"]], ".best_variant")
              )),
              if (ctx[["resampled"]]) {
                paste0(
                  " in ",
                  writeup_value(
                    w,
                    writeup_key("selected_variant", v),
                    counts[[v]],
                    "count",
                    paste0(
                      "derived: count of ",
                      ctx[["tuner_root"]],
                      ".best_variant"
                    )
                  ),
                  " ",
                  writeup_plural(counts[[v]], "resample")
                )
              }
            )
          },
          character(1L)
        ))
      )
    )
  }
  for (nm in names) {
    values <- unlist(lapply(selected, function(s) s[[nm]]))
    if (length(values) == 0L) {
      next
    }
    source <- paste0(root, ".", nm)
    clauses <- c(
      clauses,
      if (!ctx[["resampled"]] || length(unique(values)) == 1L) {
        paste0(
          writeup_code(nm),
          " = ",
          writeup_setting_value(
            w,
            writeup_key("selected", nm),
            values[[1L]],
            source
          ),
          if (ctx[["resampled"]]) " in every resample"
        )
      } else if (is.numeric(values)) {
        paste0(
          writeup_code(nm),
          " from ",
          writeup_setting_value(
            w,
            writeup_key("selected", nm, "min"),
            min(values),
            paste0("derived: minimum of ", source)
          ),
          " to ",
          writeup_setting_value(
            w,
            writeup_key("selected", nm, "max"),
            max(values),
            paste0("derived: maximum of ", source)
          ),
          " (median ",
          writeup_setting_value(
            w,
            writeup_key("selected", nm, "median"),
            stats::median(values),
            paste0("derived: median of ", source)
          ),
          ")"
        )
      } else {
        counts <- table(as.character(values))
        paste0(
          writeup_code(nm),
          " = ",
          writeup_list(vapply(
            names(counts),
            function(v) {
              paste0(
                writeup_value(
                  w,
                  writeup_key("selected", nm, v),
                  v,
                  "text",
                  source
                ),
                " in ",
                writeup_value(
                  w,
                  writeup_key("selected", nm, v, "count"),
                  counts[[v]],
                  "count",
                  paste0("derived: count of ", source)
                ),
                " ",
                writeup_plural(counts[[v]], "resample")
              )
            },
            character(1L)
          ))
        )
      }
    )
  }
  if (length(clauses) == 0L) {
    return(NULL)
  }
  text <- paste0(
    if (ctx[["resampled"]]) {
      "Across resamples, tuning selected "
    } else {
      "Tuning selected "
    },
    writeup_list(clauses),
    "."
  )
  writeup_section("results", "Hyperparameter tuning", list(text))
} # /rtemis::writeup_results_tuning


# %% writeup_not_reported ----
#' What a Methods section states that the model does not record
#'
#' @param ctx Context list.
#'
#' @return Character vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_not_reported <- function(ctx) {
  algorithm <- ctx[["x"]]@algorithm
  selection <- ctx[["selection"]]
  collected <- if (!is.null(ctx[["grid"]])) {
    setdiff(
      names(ctx[["grid"]]),
      c("param_combo_id", VARIANT_COLUMN, writeup_searched(ctx))
    )
  } else {
    character()
  }
  undescribed <- setdiff(collected, selection[["collected"]])
  c(
    "The source of the data, the study design, the setting and the eligibility criteria for cases.",
    "How the outcome and the predictors were defined and measured.",
    "Missing data and any processing applied before the data was passed to rtemis.",
    if (!ctx[["resampled"]] && ctx[["has_test"]]) {
      "How the test set was selected."
    },
    if (is.null(ctx[["outcome"]])) "The name of the outcome.",
    if (is.null(selection) && algorithm %in% SUPERVISED_INTERNAL_SELECTION) {
      paste0(
        "How ",
        algorithm,
        " selects values inside each fit, for example by internal ",
        "cross-validation, pruning or early stopping."
      )
    },
    if (length(undescribed) > 0L) {
      paste0(
        "How ",
        writeup_list(writeup_code(undescribed)),
        " ",
        writeup_plural(length(undescribed), "was", "were"),
        " selected inside each fit during tuning."
      )
    }
  )
} # /rtemis::writeup_not_reported
