# validate_data.R
# ::rtemis::
# 2026- EDG rtemis.org

# The data half of `validate_config()`: what a schema cannot see, because it is
# a fact about the dataset rather than about the document.
#
# Each check is written over the *parts* a config carries -- a preprocessor, a
# resampler, an algorithm, an outcome -- rather than over one config class. A
# `SuperConfig` carries all of them and gets every applicable check; a
# standalone `PreprocessorConfig` carries one and gets the checks about it.
# `config_parts()` is what makes that one code path instead of a branch per
# family, and it is also what decides a check's scope: a check whose parts are
# absent does not run, and reports nothing, rather than reporting on values it
# had to invent.

# %% config_parts ----
#' Pull the validatable parts out of any rtemis config
#'
#' A config family decides which parts exist, not which checks are written. A
#' part that a family does not carry comes back NULL and the checks about it are
#' skipped.
#'
#' @param config Resolved rtemis config object.
#'
#' @return Named list with elements `preprocessor_config`,
#'   `resamplers` (named list of `ResamplerConfig`, keyed by the JSON Pointer of
#'   the property holding each), `hyperparameters`, `positive_class`, and
#'   `supervised`. Each is NULL, or empty, where the config does not carry it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_parts <- function(config) {
  out <- list(
    preprocessor_config = NULL,
    resamplers = list(),
    hyperparameters = NULL,
    positive_class = NULL,
    # Whether the config is for a run that designates an outcome, which decides
    # whether the last column means anything (see `resolve_outcome()`). The
    # `hyperparameters` block is the marker: a decomposition or clustering
    # config has none, because those methods model every column jointly.
    supervised = "hyperparameters" %in% names(S7_class(config)@properties)
  )
  if (S7_inherits(config, PreprocessorConfig)) {
    out[["preprocessor_config"]] <- config
    return(out)
  }
  # Read by property name rather than by class. `SuperConfig` and
  # `SuperConfigLive` declare the same blocks under the same names and differ
  # only in how the data reaches them, so a class test would validate the
  # portable recipe and quietly find nothing in the live one.
  out[["preprocessor_config"]] <- config_prop(config, "preprocessor_config")
  out[["hyperparameters"]] <- config_prop(config, "hyperparameters")
  out[["positive_class"]] <- config_prop(config, "positive_class")
  # Both resamplers partition the same cases and fail the same way, so both are
  # checked. Each is keyed by the JSON Pointer of the property that holds it, so
  # a finding's `fix` patches the one it is about.
  outer <- config_prop(config, "outer_resampling_config")
  if (!is.null(outer)) {
    out[["resamplers"]][["/outer_resampling_config"]] <- outer
  }
  tuner <- config_prop(config, "tuner_config")
  if (
    !is.null(tuner) &&
      S7_inherits(tuner, GridSearchConfig) &&
      !is.null(tuner@resampler_config)
  ) {
    out[["resamplers"]][["/tuner_config/config/resampler_config"]] <-
      tuner@resampler_config
  }
  out
} # /rtemis::config_parts


# %% config_prop ----
#' One property of a config, or NULL if the class does not declare it
#'
#' @param config Resolved rtemis config object.
#' @param name Character: Property name.
#'
#' @return The property's value, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_prop <- function(config, name) {
  if (!name %in% names(S7_class(config)@properties)) {
    return(NULL)
  }
  prop(config, name)
} # /rtemis::config_prop


# %% validate_data ----
#' Run every applicable data check for one config
#'
#' @param config Resolved rtemis config object.
#' @param data tabular data: The dataset the config would run on.
#' @param outcome Optional Character: Name of the outcome column.
#' @param step Optional Integer [1, Inf): Position in the plan.
#'
#' @return List of `Diagnostic` objects, in the order the checks were made.
#'
#' @author EDG
#' @keywords internal
#' @noRd
validate_data <- function(config, data, outcome = NULL, step = NULL) {
  parts <- config_parts(config)
  dat <- as.data.table(data)
  cd <- check_data(dat, name = "data", get_duplicates = FALSE)

  # Outcome ----
  # Resolving the outcome comes first and can end the pass: every check below
  # that reads it would otherwise report on a column that is not there.
  resolution <- resolve_outcome(dat, outcome, parts[["supervised"]])
  if (!is.null(resolution[["diagnostic"]])) {
    return(list(set_step(resolution[["diagnostic"]], step)))
  }
  outcome_name <- resolution[["name"]]
  outcome_value <- if (is.null(outcome_name)) NULL else dat[[outcome_name]]
  feature_names <- setdiff(names(dat), outcome_name)

  out <- c(
    check_outcome_type(outcome_value, outcome_name, parts),
    unlist(
      lapply(
        names(parts[["resamplers"]]),
        function(pointer) {
          resampler <- parts[["resamplers"]][[pointer]]
          c(
            check_resample_min_class(resampler, outcome_value, pointer),
            check_resample_n_rows(resampler, cd@n_rows, pointer)
          )
        }
      ),
      recursive = FALSE
    ),
    check_feature_constant(dat, feature_names, parts),
    check_dim_p_gt_n(cd, dat, feature_names),
    check_missing_incompatible(cd, dat, outcome_name, parts)
  )
  out <- Filter(Negate(is.null), out)
  lapply(out, set_step, step = step)
} # /rtemis::validate_data


# %% set_step ----
#' Record a finding's position in the plan
#'
#' Set once, where the findings are collected, so that no check has to carry the
#' argument through to every `new_diagnostic()` call it makes.
#'
#' @param x `Diagnostic` object.
#' @param step Optional Integer [1, Inf): Position in the plan.
#'
#' @return `Diagnostic` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
set_step <- function(x, step) {
  if (is.null(step)) {
    return(x)
  }
  x@step <- step
  x
} # /rtemis::set_step


# %% resolve_outcome ----
#' Resolve which column of the data is the outcome
#'
#' A named `outcome` must be a column of the data, whatever the config is for.
#' An unnamed one takes rtemis's convention, the last column -- but only for a
#' config that designates an outcome at all: a decomposition or clustering run
#' models every column jointly, and calling the last one an outcome there would
#' silently drop a column from the feature checks.
#'
#' Data with a single column likewise has no outcome to resolve against
#' features, so the outcome is left unresolved and every check that reads it is
#' skipped rather than pointed at the only column there is.
#'
#' @param dat data.table: The dataset.
#' @param outcome Optional Character: Name of the outcome column.
#' @param supervised Logical: Whether the config designates an outcome.
#'
#' @return Named list with `name` (Character or NULL) and `diagnostic`
#'   (`Diagnostic` or NULL). A non-NULL `diagnostic` means the outcome could not
#'   be resolved and the pass should end.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_outcome <- function(dat, outcome, supervised) {
  if (is.null(outcome)) {
    return(list(
      name = if (supervised && NCOL(dat) >= 2L) {
        names(dat)[[NCOL(dat)]]
      } else {
        NULL
      },
      diagnostic = NULL
    ))
  }
  if (length(outcome) != 1L || !outcome %in% names(dat)) {
    return(list(
      name = NULL,
      diagnostic = new_diagnostic(
        code = "OUTCOME_MISSING",
        severity = "error",
        message = paste0(
          "Outcome column '",
          paste(outcome, collapse = "', '"),
          "' is not in the data. Columns are: ",
          paste0("'", names(dat), "'", collapse = ", "),
          "."
        ),
        evidence = list(
          outcome = outcome,
          columns = names(dat),
          nearest = Filter(nzchar, nearest_hint(outcome, names(dat)))
        )
      )
    ))
  }
  list(name = outcome, diagnostic = NULL)
} # /rtemis::resolve_outcome


# %% check_outcome_type ----
#' Is the outcome the kind of column this config predicts?
#'
#' Two independent ways the pair can disagree, and both are fatal:
#'
#' - The column is a type rtemis cannot use as an outcome at all. A character
#'   column is the common case: it looks categorical but `check_supervised()`
#'   rejects it, because a factor's *level order* is what decides the positive
#'   class and a character vector has none.
#' - The config declares a task the column cannot serve. `positive_class` is a
#'   binary-classification setting, so it names a factor level; on a numeric
#'   outcome rtemis infers regression and the setting is a statement about a
#'   question the run is not asking. An algorithm that performs only one of the
#'   two tasks says the same thing about itself.
#'
#' @param outcome_value Vector or NULL: The outcome column.
#' @param outcome_name Character or NULL: Its name.
#' @param parts Named list from `config_parts()`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_outcome_type <- function(outcome_value, outcome_name, parts) {
  if (is.null(outcome_value)) {
    return(list())
  }
  observed <- class(outcome_value)[[1L]]
  if (!inherits(outcome_value, c("integer", "numeric", "factor"))) {
    return(list(new_diagnostic(
      code = "OUTCOME_TYPE_MISMATCH",
      severity = "error",
      message = paste0(
        "Outcome '",
        outcome_name,
        "' is ",
        observed,
        "; rtemis requires an integer, numeric, or factor outcome. Convert it ",
        "to a factor to classify, or to a number to regress."
      ),
      evidence = list(outcome = outcome_name, outcome_class = observed)
    )))
  }
  data_task <- if (is.factor(outcome_value)) "Classification" else "Regression"
  declared <- declared_task(parts)
  if (is.null(declared) || identical(declared, data_task)) {
    return(list())
  }
  list(new_diagnostic(
    code = "OUTCOME_TYPE_MISMATCH",
    severity = "error",
    message = paste0(
      "Config declares ",
      tolower(declared),
      " (",
      declared_task_reason(parts),
      ") but outcome '",
      outcome_name,
      "' is ",
      observed,
      ", which rtemis reads as ",
      tolower(data_task),
      "."
    ),
    evidence = list(
      outcome = outcome_name,
      outcome_class = observed,
      declared_task = declared,
      data_task = data_task
    )
  ))
} # /rtemis::check_outcome_type


# %% declared_task ----
#' The task a config states it is for, or NULL if it states none
#'
#' rtemis infers the task from the outcome, so most configs declare nothing and
#' NULL is the common answer. Only a setting that is meaningless under the other
#' task counts as a declaration: `positive_class` names a factor level, and an
#' algorithm that performs one task performs one task. A hyperparameter that
#' merely *implies* a task (an objective, a family) is not read here -- each
#' would need its own per-algorithm mapping, and a wrong reading of one would
#' reject a config that runs.
#'
#' @param parts Named list from `config_parts()`.
#'
#' @return Character \{"Classification", "Regression"\} or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
declared_task <- function(parts) {
  if (!is.null(parts[["positive_class"]])) {
    return("Classification")
  }
  algorithm <- config_algorithm(parts)
  if (is.null(algorithm)) {
    return(NULL)
  }
  idx <- match(
    tolower(algorithm),
    tolower(supervised_algorithms[["name"]])
  )
  if (is.na(idx)) {
    return(NULL)
  }
  does_class <- as.logical(supervised_algorithms[["class"]][[idx]])
  does_reg <- as.logical(supervised_algorithms[["reg"]][[idx]])
  if (isTRUE(does_class) && !isTRUE(does_reg)) {
    return("Classification")
  }
  if (isTRUE(does_reg) && !isTRUE(does_class)) {
    return("Regression")
  }
  NULL
} # /rtemis::declared_task


# %% declared_task_reason ----
#' Which setting made the config's task declaration, for the message
#'
#' @param parts Named list from `config_parts()`.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
declared_task_reason <- function(parts) {
  if (!is.null(parts[["positive_class"]])) {
    return(paste0("`positive_class` is '", parts[["positive_class"]], "'"))
  }
  paste0("algorithm ", config_algorithm(parts))
} # /rtemis::declared_task_reason


# %% config_algorithm ----
#' The algorithm a config names, or NULL
#'
#' A hyperparameter *set* is a search over one algorithm, so it names one too.
#'
#' @param parts Named list from `config_parts()`.
#'
#' @return Character or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_algorithm <- function(parts) {
  hp <- parts[["hyperparameters"]]
  if (is.null(hp)) {
    return(NULL)
  }
  # Both classes declare `@algorithm`; a set derives it from its first member,
  # every member being a configuration of the same algorithm.
  algorithm <- hp@algorithm
  if (length(algorithm) != 1L || is.na(algorithm)) {
    return(NULL)
  }
  algorithm
} # /rtemis::config_algorithm


# %% check_resample_min_class ----
#' Can the rarer class fill every resample?
#'
#' A stratified resampler puts each class into every part in the class's own
#' proportion, so a class with fewer cases than there are parts cannot reach all
#' of them. At least one part then scores a class it never saw, and the score
#' for it is not wrong so much as meaningless.
#'
#' Only stratified types are checked. A plain bootstrap draws with replacement
#' and makes no promise about class balance, so there is no count it fails to
#' meet.
#'
#' @param resampler `ResamplerConfig` object.
#' @param outcome_value Vector or NULL: The outcome column.
#' @param pointer Character: JSON Pointer of the property holding `resampler`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_resample_min_class <- function(resampler, outcome_value, pointer) {
  if (is.null(outcome_value) || !is.factor(outcome_value)) {
    return(list())
  }
  if (!resampler@type %in% c("KFold", "StratSub", "StratBoot")) {
    return(list())
  }
  counts <- table(droplevels(outcome_value))
  min_class <- as.integer(min(counts))
  n_resamples <- resampler@n_resamples
  if (min_class >= n_resamples) {
    return(list())
  }
  list(new_diagnostic(
    code = "RESAMPLE_MIN_CLASS",
    severity = "error",
    message = paste0(
      "Class '",
      names(counts)[[which.min(counts)]],
      "' has ",
      min_class,
      " ",
      ngettext(min_class, "case", "cases"),
      " but the ",
      resampler@type,
      " resampler at `",
      pointer,
      "` asks for ",
      n_resamples,
      " resamples."
    ),
    evidence = list(
      resampler = pointer,
      type = resampler@type,
      n_resamples = n_resamples,
      min_class = names(counts)[[which.min(counts)]],
      min_class_n = min_class,
      class_counts = as.list(counts)
    ),
    # Fewer parts than the rarest class has cases is the one repair that needs
    # no judgment. Below two there is no fold count that works, so nothing is
    # offered rather than something that fails differently.
    fix = if (min_class >= 2L) {
      list(list(
        op = "replace",
        path = paste0(pointer, "/n_resamples"),
        value = min_class
      ))
    }
  ))
} # /rtemis::check_resample_min_class


# %% check_resample_n_rows ----
#' Are there enough rows for the resamples asked for?
#'
#' Two failures, one fatal and one merely useless. More parts than rows cannot
#' be made at all. Parts that *can* be made but hold a single case can: a score
#' computed on one case carries no information, and averaging such scores across
#' parts does not create any.
#'
#' Which quantity decides it depends on the type -- a fold count for KFold, a
#' training fraction for the subsampling types, the row count alone for the rest
#' -- so each names its own, and `Custom` names none: its resamples are supplied
#' rather than drawn, and `resample()` checks their indices against the data.
#'
#' @param resampler `ResamplerConfig` object.
#' @param n_rows Integer: Number of rows in the data.
#' @param pointer Character: JSON Pointer of the property holding `resampler`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_resample_n_rows <- function(resampler, n_rows, pointer) {
  type <- resampler@type
  if (type == "Custom") {
    return(list())
  }
  base_evidence <- list(resampler = pointer, type = type, n_rows = n_rows)
  if (type %in% c("LOOCV", "Bootstrap")) {
    if (n_rows >= 2L) {
      return(list())
    }
    return(list(new_diagnostic(
      code = "RESAMPLE_N_ROWS",
      severity = "error",
      message = paste0(
        "The ",
        type,
        " resampler at `",
        pointer,
        "` needs at least 2 rows; the data has ",
        n_rows,
        "."
      ),
      evidence = base_evidence
    )))
  }
  if (type == "KFold") {
    n_resamples <- resampler@n_resamples
    evidence <- c(base_evidence, list(n_resamples = n_resamples))
    if (n_rows < n_resamples) {
      return(list(new_diagnostic(
        code = "RESAMPLE_N_ROWS",
        severity = "error",
        message = paste0(
          "The KFold resampler at `",
          pointer,
          "` asks for ",
          n_resamples,
          " folds but the data has ",
          n_rows,
          " ",
          ngettext(n_rows, "row", "rows"),
          "."
        ),
        evidence = evidence,
        # The largest fold count that both fits the data and leaves a usable
        # test fold: any k above `n %/% 2` gives folds of one case, which is
        # what the warning below is about, so repairing to `n_rows` would only
        # trade an error for a warning. Below four rows no such k exists and
        # nothing is offered.
        fix = if (n_rows >= 4L) {
          list(list(
            op = "replace",
            path = paste0(pointer, "/n_resamples"),
            value = n_rows %/% 2L
          ))
        }
      )))
    }
    n_test <- n_rows %/% n_resamples
    if (n_test >= 2L) {
      return(list())
    }
    return(list(new_diagnostic(
      code = "RESAMPLE_N_ROWS",
      severity = "warning",
      message = paste0(
        "The KFold resampler at `",
        pointer,
        "` leaves ",
        n_test,
        " ",
        ngettext(n_test, "case", "cases"),
        " per test fold (",
        n_rows,
        " rows over ",
        n_resamples,
        " folds)."
      ),
      evidence = c(evidence, list(n_test = n_test))
    )))
  }
  # StratSub and StratBoot, which split by fraction rather than by count.
  train_p <- resampler@train_p
  n_train <- round(n_rows * train_p)
  n_test <- n_rows - n_train
  evidence <- c(
    base_evidence,
    list(train_p = train_p, n_train = n_train, n_test = n_test)
  )
  if (n_test < 1L || n_train < 2L) {
    return(list(new_diagnostic(
      code = "RESAMPLE_N_ROWS",
      severity = "error",
      message = paste0(
        "The ",
        type,
        " resampler at `",
        pointer,
        "` splits ",
        n_rows,
        " ",
        ngettext(n_rows, "row", "rows"),
        " at train_p ",
        train_p,
        " into ",
        n_train,
        " training and ",
        n_test,
        " test; neither side can be empty and training needs at least 2."
      ),
      evidence = evidence
    )))
  }
  if (n_test >= 2L) {
    return(list())
  }
  list(new_diagnostic(
    code = "RESAMPLE_N_ROWS",
    severity = "warning",
    message = paste0(
      "The ",
      type,
      " resampler at `",
      pointer,
      "` leaves ",
      n_test,
      " test case per resample (",
      n_rows,
      " rows at train_p ",
      train_p,
      ")."
    ),
    evidence = evidence
  ))
} # /rtemis::check_resample_n_rows


# %% check_feature_constant ----
#' Do any predictors hold one value throughout?
#'
#' A column that never varies cannot explain a column that does. rtemis trains
#' on it regardless -- so this is a warning, not an error -- but the coefficient
#' or split it produces is an artifact of the column being there, not a finding.
#'
#' A column that is entirely missing counts: `is_constant()` reads it as one
#' (there is no varying observed value) and `remove_constants` removes it, so
#' this reports what preprocessing would act on.
#'
#' Constants the config already removes are not reported: `remove_constants`
#' drops all of them and `remove_features` drops the ones it names, so a config
#' that has dealt with the problem is clean rather than repeatedly told about it.
#'
#' @param dat data.table: The dataset.
#' @param feature_names Character: The predictor columns.
#' @param parts Named list from `config_parts()`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_feature_constant <- function(dat, feature_names, parts) {
  if (length(feature_names) == 0L) {
    return(list())
  }
  pp <- parts[["preprocessor_config"]]
  if (!is.null(pp) && pp@remove_constants) {
    return(list())
  }
  skip_missing <- if (is.null(pp)) TRUE else pp@remove_constants_skip_missing
  constant <- feature_names[vapply(
    feature_names,
    function(nm) is_constant(dat[[nm]], skip_missing = skip_missing),
    logical(1L)
  )]
  already <- if (is.null(pp)) {
    character()
  } else {
    pp@remove_features %||% character()
  }
  constant <- setdiff(constant, already)
  if (length(constant) == 0L) {
    return(list())
  }
  list(new_diagnostic(
    code = "FEATURE_CONSTANT",
    severity = "warning",
    message = paste0(
      length(constant),
      ngettext(
        length(constant),
        " predictor never varies",
        " predictors never vary"
      ),
      ": ",
      paste0("'", constant, "'", collapse = ", "),
      "."
    ),
    evidence = list(features = constant, n_features = length(constant)),
    # Naming the columns is deterministic; the union with what the config
    # already removes is what keeps the patch from discarding that list.
    fix = list(list(
      op = "add",
      path = if (is.null(pp)) {
        "/preprocessor_config"
      } else {
        "/preprocessor_config/remove_features"
      },
      value = if (is.null(pp)) {
        list(remove_features = as.list(constant))
      } else {
        as.list(union(already, constant))
      }
    ))
  ))
} # /rtemis::check_feature_constant


# %% check_dim_p_gt_n ----
#' Are there more encoded predictors than rows?
#'
#' Counted after categorical encoding, because that is the width the model
#' actually fits: a factor contributes one column per level it takes -- rtemis's
#' `one_hot()` encodes every level, dropping no reference -- while a numeric or
#' date column contributes one. `CheckData@n_distinct_per_col` supplies the
#' level counts.
#'
#' A warning rather than an error: p > n is the situation regularized methods
#' exist for, and a config that chose one is not wrong. What it costs is the
#' ability of an unregularized fit to be checked at all, which is worth saying.
#'
#' @param cd `CheckData` object for the data.
#' @param dat data.table: The dataset.
#' @param feature_names Character: The predictor columns.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_dim_p_gt_n <- function(cd, dat, feature_names) {
  if (length(feature_names) == 0L) {
    return(list())
  }
  widths <- vapply(
    feature_names,
    function(nm) {
      if (is.factor(dat[[nm]]) || is.character(dat[[nm]])) {
        cd@n_distinct_per_col[[nm]]
      } else {
        1L
      }
    },
    integer(1L)
  )
  encoded_p <- sum(widths)
  n_rows <- cd@n_rows
  if (encoded_p <= n_rows) {
    return(list())
  }
  categorical <- names(widths)[widths > 1L]
  list(new_diagnostic(
    code = "DIM_P_GT_N",
    severity = "warning",
    message = paste0(
      length(feature_names),
      ngettext(
        length(feature_names),
        " predictor encodes",
        " predictors encode"
      ),
      " to ",
      encoded_p,
      " columns, more than the ",
      n_rows,
      " ",
      ngettext(n_rows, "row", "rows"),
      " available."
    ),
    evidence = list(
      n_features = length(feature_names),
      encoded_p = encoded_p,
      n_rows = n_rows,
      categorical_features = categorical
    )
  ))
} # /rtemis::check_dim_p_gt_n


# %% check_missing_incompatible ----
#' Will missing values reach a run that cannot take them?
#'
#' Three ways they can, in the order they bite:
#'
#' - **A missing outcome.** No preprocessing option addresses it: `preprocess()`
#'   transforms features, and `check_supervised()` rejects a training set whose
#'   outcome has gaps. Always an error.
#' - **Preprocessing that cannot run on this pattern.** `complete_cases` on data
#'   with no complete row leaves nothing to train on, and imputation has nothing
#'   to learn from in a column that is entirely absent.
#' - **Gaps that survive to the learner.** Whether that is fatal is the
#'   algorithm's own answer, read from `algorithm_allows_missing()`: an error
#'   where the algorithm refuses them, a warning where the answer is not the
#'   algorithm's to give (a meta learner defers to its base learners) or where
#'   no algorithm is named.
#'
#' Only `complete_cases` and `impute` count as resolving the gaps.
#' `remove_features_thres` and `remove_cases_thres` drop what is missing *above
#' a threshold*, so they leave whatever falls below it; treating them as a
#' remedy would clear the finding for a config that still fails.
#'
#' @param cd `CheckData` object for the data.
#' @param dat data.table: The dataset.
#' @param outcome_name Character or NULL: The outcome column.
#' @param parts Named list from `config_parts()`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_missing_incompatible <- function(cd, dat, outcome_name, parts) {
  if (cd@n_na == 0L) {
    return(list())
  }
  pp <- parts[["preprocessor_config"]]
  out <- list()

  if (!is.null(outcome_name)) {
    n_na_outcome <- sum(is.na(dat[[outcome_name]]))
    if (n_na_outcome > 0L) {
      out <- c(
        out,
        list(new_diagnostic(
          code = "MISSING_INCOMPATIBLE",
          severity = "error",
          message = paste0(
            "Outcome '",
            outcome_name,
            "' has ",
            n_na_outcome,
            " missing ",
            ngettext(n_na_outcome, "value", "values"),
            ". Preprocessing transforms features, so no setting removes them: ",
            "drop those rows before training."
          ),
          evidence = list(
            outcome = outcome_name,
            n_missing = n_na_outcome,
            n_rows = cd@n_rows
          )
        ))
      )
    }
  }

  if (!is.null(pp) && pp@complete_cases) {
    n_complete <- sum(complete.cases(dat))
    if (n_complete < 2L) {
      out <- c(
        out,
        list(new_diagnostic(
          code = "MISSING_INCOMPATIBLE",
          severity = "error",
          message = paste0(
            "`complete_cases` keeps rows with no gaps, and only ",
            n_complete,
            " of ",
            cd@n_rows,
            " ",
            ngettext(cd@n_rows, "row has", "rows have"),
            " none."
          ),
          evidence = list(n_complete = n_complete, n_rows = cd@n_rows)
        ))
      )
    }
    return(out)
  }

  all_missing <- names(dat)[vapply(dat, function(v) all(is.na(v)), logical(1L))]
  if (!is.null(pp) && pp@impute) {
    if (length(all_missing) > 0L) {
      out <- c(
        out,
        list(new_diagnostic(
          code = "MISSING_INCOMPATIBLE",
          severity = "error",
          message = paste0(
            "Imputation fills gaps from the values a column does have, and ",
            paste0("'", all_missing, "'", collapse = ", "),
            ngettext(length(all_missing), " has none.", " have none."),
            " Drop ",
            ngettext(length(all_missing), "it", "them"),
            " before imputing."
          ),
          evidence = list(
            features = all_missing,
            impute_type = pp@impute_type
          )
        ))
      )
    }
    return(out)
  }

  # Nothing in the config removes the gaps, so they reach the learner.
  algorithm <- config_algorithm(parts)
  allows <- if (is.null(algorithm)) NA else algorithm_allows_missing(algorithm)
  evidence <- list(
    n_missing = cd@n_na,
    n_features_missing = cd@n_cols_anyna,
    algorithm = algorithm %||% NA_character_,
    algorithm_allows_missing = allows
  )
  if (isTRUE(allows)) {
    return(out)
  }
  c(
    out,
    list(new_diagnostic(
      code = "MISSING_INCOMPATIBLE",
      severity = if (isFALSE(allows)) "error" else "warning",
      message = if (isFALSE(allows)) {
        paste0(
          algorithm,
          " does not accept missing values, and nothing in this config ",
          "removes them: ",
          cd@n_na,
          " missing ",
          ngettext(cd@n_na, "value", "values"),
          " across ",
          cd@n_cols_anyna,
          " ",
          ngettext(cd@n_cols_anyna, "column", "columns"),
          ". Impute, or keep complete cases only."
        )
      } else {
        paste0(
          cd@n_na,
          " missing ",
          ngettext(cd@n_na, "value", "values"),
          " across ",
          cd@n_cols_anyna,
          " ",
          ngettext(cd@n_cols_anyna, "column", "columns"),
          " reach the learner unchanged; not every algorithm accepts them."
        )
      },
      evidence = evidence
    ))
  )
} # /rtemis::check_missing_incompatible
