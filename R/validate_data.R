# validate_data.R
# ::rtemis::
# 2026- EDG rtemis.org

# The data half of `validate_config()`: what a schema cannot see, because it is
# a fact about the dataset rather than about the document.
#
# Each check reads a `DataProfile` rather than the data. That is what makes the
# same rule runnable outside R: the profile is a bounded document any language
# can compute in one pass, so a rule expressed over its fields ports, while one
# expressed over an R data.table does not. `missing_after_preprocessing()`
# reads the rows in one case -- a threshold on a standalone
# `PreprocessorConfig` -- and says so where it is defined; a supervised config
# cannot reach it.
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
#'   `decomposition_config`, `resamplers` (named list of `ResamplerConfig`,
#'   keyed by the JSON Pointer of the property holding each),
#'   `hyperparameters`, `positive_class`, and `supervised`. Each is NULL, or
#'   empty, where the config does not carry it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
config_parts <- function(config) {
  out <- list(
    preprocessor_config = NULL,
    decomposition_config = NULL,
    resamplers = list(),
    hyperparameters = NULL,
    positive_class = NULL,
    # Whether the config is for a run that designates an outcome, which decides
    # whether the last column means anything (see `resolve_outcome()`). The
    # `hyperparameters` block is the marker: a decomposition or clustering
    # config has none, because those methods model every column jointly.
    supervised = "hyperparameters" %in% names(S7_class(config)@properties)
  )
  # Either preprocessor config standing on its own. They are siblings, so
  # neither test implies the other.
  if (
    S7_inherits(config, PreprocessorConfig) ||
      S7_inherits(config, SupervisedPreprocessorConfig)
  ) {
    out[["preprocessor_config"]] <- config
    return(out)
  }
  # Read by property name rather than by class. `SuperConfig` and
  # `SuperConfigLive` declare the same blocks under the same names and differ
  # only in how the data reaches them, so a class test would validate the
  # portable recipe and quietly find nothing in the live one.
  out[["preprocessor_config"]] <- config_prop(config, "preprocessor_config")
  out[["decomposition_config"]] <- config_prop(config, "decomposition_config")
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


# %% profile_columns ----
#' The profile's column table
#'
#' @param profile `DataProfile` object.
#'
#' @return `data.frame` with `name`, `dtype`, `n_distinct`, `n_missing`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
profile_columns <- function(profile) {
  profile@columns
} # /rtemis::profile_columns


# %% profile_field ----
#' One measured field of one column, or NULL
#'
#' @param profile `DataProfile` object.
#' @param column Character: Column name.
#' @param field Character: One of the `columns` table's fields.
#'
#' @return The value, or NULL when the column is not in the profile.
#'
#' @author EDG
#' @keywords internal
#' @noRd
profile_field <- function(profile, column, field) {
  if (is.null(column)) {
    return(NULL)
  }
  cols <- profile_columns(profile)
  i <- match(column, cols[["name"]])
  if (is.na(i)) {
    return(NULL)
  }
  cols[[field]][[i]]
} # /rtemis::profile_field


# %% profile_level_counts ----
#' Observed level counts for one column
#'
#' Empty where the column is not categorical, and *also* empty where it has more
#' than `PROFILE_MAX_LEVELS` levels -- the profile omits those deliberately. A
#' caller that needs to tell the two apart compares against `n_distinct`.
#'
#' @param profile `DataProfile` object.
#' @param column Character: Column name.
#'
#' @return `data.frame` with `level` and `n`, possibly with no rows.
#'
#' @author EDG
#' @keywords internal
#' @noRd
profile_level_counts <- function(profile, column) {
  counts <- profile@level_counts
  if (is.null(column) || is.null(counts) || NROW(counts) == 0L) {
    return(data.frame(level = character(), n = integer()))
  }
  counts[counts[["column"]] == column, c("level", "n"), drop = FALSE]
} # /rtemis::profile_level_counts


# %% validate_data ----
#' Run every applicable data check for one config
#'
#' @param config Resolved rtemis config object.
#' @param data tabular data: The dataset the config would run on. Profiled
#'   here; only `check_missing_incompatible()` reads the rows themselves.
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
  # Duplicates are the one whole-table scan and no check reads them, so the
  # profile a validation makes is the cheap one.
  profile <- data_profile(dat, n_duplicates = FALSE)

  # Outcome ----
  # Resolving the outcome comes first and can end the pass: every check below
  # that reads it would otherwise report on a column that is not there.
  resolution <- resolve_outcome(profile, outcome, parts[["supervised"]])
  if (!is.null(resolution[["diagnostic"]])) {
    return(list(set_step(resolution[["diagnostic"]], step)))
  }
  outcome_name <- resolution[["name"]]
  feature_names <- setdiff(profile_columns(profile)[["name"]], outcome_name)

  out <- c(
    check_outcome_type(profile, outcome_name, parts),
    unlist(
      lapply(
        names(parts[["resamplers"]]),
        function(pointer) {
          resampler <- parts[["resamplers"]][[pointer]]
          c(
            check_resample_min_class(resampler, profile, outcome_name, pointer),
            check_resample_n_rows(resampler, profile@n_rows, pointer)
          )
        }
      ),
      recursive = FALSE
    ),
    check_feature_constant(profile, feature_names, parts),
    check_feature_type(profile, feature_names),
    check_dim_p_gt_n(profile, feature_names, parts),
    check_missing_incompatible(profile, dat, feature_names, outcome_name, parts)
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
#' @param profile `DataProfile` object for the dataset.
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
resolve_outcome <- function(profile, outcome, supervised) {
  nms <- profile_columns(profile)[["name"]]
  if (is.null(outcome)) {
    return(list(
      name = if (supervised && length(nms) >= 2L) nms[[length(nms)]] else NULL,
      diagnostic = NULL
    ))
  }
  if (length(outcome) != 1L || !outcome %in% nms) {
    return(list(
      name = NULL,
      diagnostic = new_diagnostic(
        code = "OUTCOME_MISSING",
        severity = "error",
        message = paste0(
          "Outcome column '",
          paste(outcome, collapse = "', '"),
          "' is not in the data. Columns are: ",
          paste0("'", nms, "'", collapse = ", "),
          "."
        ),
        evidence = list(
          outcome = outcome,
          columns = nms,
          nearest = Filter(nzchar, nearest_hint(outcome, nms))
        )
      )
    ))
  }
  list(name = outcome, diagnostic = NULL)
} # /rtemis::resolve_outcome


# %% SUPERVISED_OUTCOME_DTYPES ----
# Profile dtypes `check_supervised()` accepts as an outcome. The R side states
# the same set as `inherits(x, c("integer", "numeric", "factor"))`; this is that
# set in the profile's vocabulary, and the two must change together.
SUPERVISED_OUTCOME_DTYPES <- c("integer", "number", "categorical")

# Profile dtypes `check_numeric_or_factor()` accepts as a predictor. Narrower
# than the outcome set only in that it is the same set -- both require numeric
# or factor -- but they are separate rules in `check_supervised()` and are kept
# separate here so a change to one does not silently move the other.
SUPERVISED_FEATURE_DTYPES <- c("integer", "number", "categorical")


# %% check_outcome_type ----
#' Is the outcome the kind of column this config predicts?
#'
#' Two independent ways the pair can disagree, and only the first is fatal:
#'
#' - The column is a type rtemis cannot use as an outcome at all. A character
#'   column is the common case: it looks categorical but `check_supervised()`
#'   rejects it, because a factor's *level order* is what decides the positive
#'   class and a character vector has none.
#' - The config declares a task the column cannot serve. `positive_class` is a
#'   binary-classification setting, so it names a factor level; on a numeric
#'   outcome rtemis infers regression and the setting is a statement about a
#'   question the run is not asking. An algorithm that performs only one of the
#'   two tasks says the same thing about itself. A warning rather than an error,
#'   because the run completes -- the setting is ignored, not fatal.
#'
#' @param profile `DataProfile` object for the dataset.
#' @param outcome_name Character or NULL: The outcome column.
#' @param parts Named list from `config_parts()`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_outcome_type <- function(profile, outcome_name, parts) {
  if (is.null(outcome_name)) {
    return(list())
  }
  observed <- profile_field(profile, outcome_name, "dtype")
  if (!observed %in% SUPERVISED_OUTCOME_DTYPES) {
    return(list(new_diagnostic(
      code = "OUTCOME_TYPE_MISMATCH",
      severity = "error",
      message = paste0(
        "Outcome '",
        outcome_name,
        "' holds ",
        observed,
        " values; rtemis requires an integer, numeric, or factor outcome. ",
        "Convert it to a factor to classify, or to a number to regress."
      ),
      evidence = list(outcome = outcome_name, outcome_dtype = observed)
    )))
  }
  data_task <- if (observed == "categorical") "Classification" else "Regression"
  declared <- declared_task(parts)
  if (is.null(declared) || identical(declared, data_task)) {
    return(list())
  }
  list(new_diagnostic(
    code = "OUTCOME_TYPE_MISMATCH",
    # A warning, not an error: `train()` ignores `positive_class` on a numeric
    # outcome and completes, so stopping the run would reject a config that
    # works -- a portable recipe reused across a classification and a
    # regression dataset is the ordinary way to reach this. What it costs is
    # that the run answers the question the *data* poses rather than the one the
    # config states, which the caller should see and decide about.
    severity = "warning",
    message = paste0(
      "Config declares ",
      tolower(declared),
      " (",
      declared_task_reason(parts),
      ") but outcome '",
      outcome_name,
      "' holds ",
      observed,
      " values, which rtemis reads as ",
      tolower(data_task),
      "."
    ),
    evidence = list(
      outcome = outcome_name,
      outcome_dtype = observed,
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
#' @param profile `DataProfile` object for the dataset.
#' @param outcome_name Character or NULL: The outcome column.
#' @param pointer Character: JSON Pointer of the property holding `resampler`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_resample_min_class <- function(
  resampler,
  profile,
  outcome_name,
  pointer
) {
  if (is.null(outcome_name)) {
    return(list())
  }
  if (
    !identical(profile_field(profile, outcome_name, "dtype"), "categorical")
  ) {
    return(list())
  }
  if (!resampler@type %in% c("KFold", "StratSub", "StratBoot")) {
    return(list())
  }
  n_resamples <- resampler@n_resamples
  counts <- profile_level_counts(profile, outcome_name)
  if (NROW(counts) == 0L) {
    # The profile omits level counts above `PROFILE_MAX_LEVELS`, so this check
    # cannot run rather than passing. Saying so is the point: a silent skip is
    # indistinguishable from a clean result.
    return(list(new_diagnostic(
      code = "RESAMPLE_MIN_CLASS",
      severity = "note",
      message = paste0(
        "Outcome '",
        outcome_name,
        "' has ",
        profile_field(profile, outcome_name, "n_distinct"),
        " levels, more than the ",
        PROFILE_MAX_LEVELS,
        " a profile carries counts for, so the class balance of the ",
        resampler@type,
        " resampler at `",
        pointer,
        "` was not checked."
      ),
      evidence = list(
        resampler = pointer,
        type = resampler@type,
        n_resamples = n_resamples,
        n_levels = profile_field(profile, outcome_name, "n_distinct"),
        max_levels = PROFILE_MAX_LEVELS
      )
    )))
  }
  min_class <- min(counts[["n"]])
  if (min_class >= n_resamples) {
    return(list())
  }
  min_level <- counts[["level"]][[which.min(counts[["n"]])]]
  list(new_diagnostic(
    code = "RESAMPLE_MIN_CLASS",
    severity = "error",
    message = paste0(
      "Class '",
      min_level,
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
      min_class = min_level,
      min_class_n = min_class,
      # Long form, one record per level, as `profile/v1` carries them. A
      # name -> count map cannot be iterated by an expression language, so the
      # rule set could not reproduce it and the two would state the same fact
      # in shapes a conformance run could not compare.
      class_counts = lapply(
        seq_len(NROW(counts)),
        function(i) {
          list(level = counts[["level"]][[i]], n = counts[["n"]][[i]])
        }
      )
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
#' @param profile `DataProfile` object for the dataset.
#' @param feature_names Character: The predictor columns.
#' @param parts Named list from `config_parts()`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_feature_constant <- function(profile, feature_names, parts) {
  if (length(feature_names) == 0L) {
    return(list())
  }
  pp <- parts[["preprocessor_config"]]
  if (!is.null(pp) && pp@remove_constants) {
    return(list())
  }
  skip_missing <- if (is.null(pp)) TRUE else pp@remove_constants_skip_missing
  # `is_constant()` in the profile's terms. Skipping missing values, a column
  # with at most one observed value never varies -- an entirely missing one
  # included, which is what `na.exclude()` leaves. Not skipping them, a single
  # `NA` makes the comparison undecidable and so not constant, which is exactly
  # one observed value and nothing missing.
  constant <- feature_names[vapply(
    feature_names,
    function(nm) {
      n_distinct <- profile_field(profile, nm, "n_distinct")
      if (skip_missing) {
        n_distinct <= 1L
      } else {
        n_distinct == 1L && profile_field(profile, nm, "n_missing") == 0L
      }
    },
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
    #
    # With no block to patch into, two operations rather than one carrying a
    # constructed object: an expression language has no object constructor, so
    # this is the spelling a second implementation can also produce. RFC 6902
    # applies them in order, and the result is the same document.
    fix = if (is.null(pp)) {
      list(
        list(
          op = "add",
          path = "/preprocessor_config",
          value = stats::setNames(list(), character())
        ),
        list(
          op = "add",
          path = "/preprocessor_config/remove_features",
          value = as.list(constant)
        )
      )
    } else {
      list(list(
        op = "add",
        path = "/preprocessor_config/remove_features",
        value = as.list(union(already, constant))
      ))
    }
  ))
} # /rtemis::check_feature_constant


# %% check_feature_type ----
#' Are all the predictors a type rtemis can train on?
#'
#' `check_supervised()` requires every predictor to be numeric or a factor and
#' aborts otherwise, so a character or date predictor is a guaranteed failure
#' rather than a risk.
#'
#' No preprocessing rescues it, which is why this check is unconditional:
#' `check_supervised()` runs *before* `preprocess()` in `train()`, so
#' `character2factor` -- which would convert exactly this column -- never gets
#' the chance. Converting the column is a change to the data rather than to the
#' config, so there is no patch to offer either.
#'
#' @param profile `DataProfile` object for the dataset.
#' @param feature_names Character: The predictor columns.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_feature_type <- function(profile, feature_names) {
  if (length(feature_names) == 0L) {
    return(list())
  }
  dtypes <- vapply(
    feature_names,
    function(nm) profile_field(profile, nm, "dtype"),
    character(1L)
  )
  unsupported <- feature_names[!dtypes %in% SUPERVISED_FEATURE_DTYPES]
  if (length(unsupported) == 0L) {
    return(list())
  }
  classes <- dtypes[unsupported]
  list(new_diagnostic(
    code = "FEATURE_TYPE_UNSUPPORTED",
    severity = "error",
    message = paste0(
      length(unsupported),
      ngettext(length(unsupported), " predictor is", " predictors are"),
      " neither numeric nor a factor: ",
      paste0("'", unsupported, "' (", classes, ")", collapse = ", "),
      "."
    ),
    evidence = list(
      features = unname(unsupported),
      dtypes = unname(classes),
      n_features = length(unsupported)
    )
  ))
} # /rtemis::check_feature_type


# %% check_dim_p_gt_n ----
#' Does the learner see more predictors than there are rows?
#'
#' Counted at the width the model actually fits, which is two steps from the
#' column count:
#'
#' - **Categorical encoding.** A factor contributes one column per level it
#'   takes -- rtemis's `one_hot()` encodes every level, dropping no reference --
#'   while a numeric or date column contributes one.
#'   The profile's `n_distinct` supplies the level counts.
#' - **Decomposition.** `train()` applies `decomposition_config` before fitting,
#'   so a pipeline that extracts `k` components hands the learner `k` features
#'   however wide the data was. Reporting the encoded width there would state a
#'   number the model never sees.
#'
#' Severity is the algorithm's answer, not a judgment: `p_gt_n` is FALSE only
#' where the fit is an unregularized least squares and goes rank-deficient, and
#' those are the runs that complete while producing aliased coefficients -- a
#' warning by the definition of the level. Everything else regularizes, selects,
#' or cannot be rank-deficient, so the situation is worth recording and nothing
#' is wrong: a note. Which remedy to reach for -- a regularized algorithm, a
#' decomposition step, more rows -- is a choice this reports the numbers for
#' rather than makes.
#'
#' @param profile `DataProfile` object for the dataset.
#' @param feature_names Character: The predictor columns.
#' @param parts Named list from `config_parts()`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_dim_p_gt_n <- function(profile, feature_names, parts) {
  if (length(feature_names) == 0L) {
    return(list())
  }
  widths <- vapply(
    feature_names,
    function(nm) {
      if (identical(profile_field(profile, nm, "dtype"), "categorical")) {
        profile_field(profile, nm, "n_distinct")
      } else {
        1L
      }
    },
    integer(1L)
  )
  encoded_p <- sum(widths)
  n_rows <- profile@n_rows

  decomposition <- parts[["decomposition_config"]]
  effective_p <- encoded_p
  decomposition_evidence <- NULL
  if (!is.null(decomposition)) {
    k <- config_prop(decomposition, "k")
    if (!is.null(k)) {
      effective_p <- k
      decomposition_evidence <- list(
        decomposition = decomposition@algorithm,
        decomposition_k = k
      )
    }
  }
  if (effective_p <= n_rows) {
    return(list())
  }

  algorithm <- config_algorithm(parts)
  handles <- if (is.null(algorithm)) {
    NA
  } else {
    algorithm_handles_p_gt_n(algorithm)
  }
  what <- if (is.null(decomposition_evidence)) {
    paste0(
      length(feature_names),
      ngettext(
        length(feature_names),
        " predictor encodes",
        " predictors encode"
      ),
      " to ",
      effective_p,
      " columns"
    )
  } else {
    paste0(
      decomposition_evidence[["decomposition"]],
      " extracts ",
      effective_p,
      ngettext(effective_p, " component", " components")
    )
  }
  list(new_diagnostic(
    code = "DIM_P_GT_N",
    severity = if (isFALSE(handles)) "warning" else "note",
    message = paste0(
      what,
      ", more than the ",
      n_rows,
      " ",
      ngettext(n_rows, "row", "rows"),
      " available.",
      if (isFALSE(handles)) {
        paste0(
          " ",
          algorithm,
          " fits an unregularized least squares, so the fit is rank-deficient."
        )
      } else if (!is.null(algorithm)) {
        paste0(" ", algorithm, " fits in this regime.")
      }
    ),
    evidence = c(
      list(
        n_features = length(feature_names),
        encoded_p = encoded_p,
        effective_p = effective_p,
        n_rows = n_rows,
        categorical_features = names(widths)[widths > 1L],
        algorithm = algorithm %||% NA_character_,
        algorithm_handles_p_gt_n = handles
      ),
      decomposition_evidence
    )
  ))
} # /rtemis::check_dim_p_gt_n


# %% missing_after_preprocessing ----
#' How many gaps are left in the features once preprocessing has run
#'
#' `remove_cases_thres` and `remove_features_thres` drop what is missing *above
#' a threshold*, so whether they resolve a dataset's gaps depends on the
#' dataset. Treating them as no help reports an error on a run that completes;
#' treating them as a remedy misses one that does not. Both are wrong, and the
#' data is in hand, so this simulates them exactly instead.
#'
#' With neither threshold set the profile answers on its own, and that is the
#' only path a supervised config can take: `SupervisedPreprocessorConfig`
#' declares neither, so every check a supervised config receives is readable
#' from a profile alone, and the same rule runs outside R.
#'
#' **The one place this file reads the rows** is a threshold on a standalone
#' `PreprocessorConfig`. The case step changes each feature's missing fraction,
#' so the answer depends on the joint missingness pattern, and carrying that
#' would cost one entry per feature per missing-count bucket -- unbounded
#' exactly where it matters, on wide data. `checks/v1` declares this case
#' unevaluable rather than reproducing it.
#'
#' Mirrors `preprocess()`: cases first, at a fraction of the *feature* count
#' (`train()` preprocesses `features(x)` and re-attaches the outcome), then
#' features on the cases that remain, each dropping at `>=` the threshold. The
#' two must stay in step -- a change to either rule in `preprocess()` belongs
#' here in the same edit.
#'
#' @param profile `DataProfile` object for the dataset.
#' @param dat data.table: The dataset. Read only where a threshold is set.
#' @param feature_names Character: The predictor columns.
#' @param pp `PreprocessorConfig` or `SupervisedPreprocessorConfig`, or NULL.
#'
#' @return Integer: Missing values remaining in the features.
#'
#' @author EDG
#' @keywords internal
#' @noRd
missing_after_preprocessing <- function(profile, dat, feature_names, pp) {
  if (length(feature_names) == 0L) {
    return(0L)
  }
  remove_cases_thres <- if (is.null(pp)) {
    NULL
  } else {
    pp_opt(pp, "remove_cases_thres")
  }
  remove_features_thres <- if (is.null(pp)) {
    NULL
  } else {
    pp_opt(pp, "remove_features_thres")
  }
  # Nothing shrinks the gaps, so the profile already holds the answer. This is
  # the only path a supervised config can take -- it declares neither threshold
  # -- which is what keeps every check it gets readable from a profile alone.
  if (is.null(remove_cases_thres) && is.null(remove_features_thres)) {
    cols <- profile_columns(profile)
    return(sum(cols[["n_missing"]][cols[["name"]] %in% feature_names]))
  }
  feat <- as.data.frame(dat)[, feature_names, drop = FALSE]
  if (!is.null(remove_cases_thres)) {
    keep <- rowSums(is.na(feat)) / NCOL(feat) < remove_cases_thres
    feat <- feat[keep, , drop = FALSE]
  }
  if (NROW(feat) == 0L) {
    return(0L)
  }
  if (!is.null(remove_features_thres)) {
    fraction <- vapply(
      feat,
      function(v) sum(is.na(v)) / length(v),
      numeric(1L)
    )
    feat <- feat[, fraction < remove_features_thres, drop = FALSE]
  }
  sum(is.na(feat))
} # /rtemis::missing_after_preprocessing


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
#' `complete_cases` and `impute` resolve the gaps outright.
#' `remove_features_thres` and `remove_cases_thres` drop what is missing above a
#' threshold, so whether they resolve *these* gaps depends on the data --
#' `missing_after_preprocessing()` simulates both rather than assuming either
#' way.
#'
#' @param profile `DataProfile` object for the dataset.
#' @param dat data.table: The dataset. Read only by
#'   `missing_after_preprocessing()`; see the note there.
#' @param feature_names Character: The predictor columns.
#' @param outcome_name Character or NULL: The outcome column.
#' @param parts Named list from `config_parts()`.
#'
#' @return List of `Diagnostic` objects, possibly empty.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_missing_incompatible <- function(
  profile,
  dat,
  feature_names,
  outcome_name,
  parts
) {
  if (sum(profile_columns(profile)[["n_missing"]]) == 0L) {
    return(list())
  }
  pp <- parts[["preprocessor_config"]]
  out <- list()

  if (!is.null(outcome_name)) {
    n_na_outcome <- profile_field(profile, outcome_name, "n_missing")
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
            n_rows = profile@n_rows
          )
        ))
      )
    }
  }

  # Whether a step runs is now the config's type: a supervised config carries no
  # `PREPROCESSOR_TRAIN_EXCLUDED` property, so `pp_opt()` reports the declared
  # default -- off -- and there is nothing to gate.
  runs <- function(op) {
    !is.null(pp) && isTRUE(pp_opt(pp, op))
  }
  if (runs("complete_cases")) {
    n_complete <- profile@n_complete_cases
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
            profile@n_rows,
            " ",
            ngettext(profile@n_rows, "row has", "rows have"),
            " none."
          ),
          evidence = list(n_complete = n_complete, n_rows = profile@n_rows)
        ))
      )
    }
    return(out)
  }

  cols <- profile_columns(profile)
  all_missing <- cols[["name"]][
    cols[["n_distinct"]] == 0L & cols[["n_missing"]] > 0L
  ]
  if (runs("impute")) {
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

  # What actually reaches the learner, with the threshold steps simulated: a
  # feature that is 90% missing is dropped by `remove_features_thres = 0.5`, and
  # a run whose gaps leave with it completes.
  # A supervised config carries no threshold property, so nothing is credited
  # there without the call site having to say so.
  n_remaining <- missing_after_preprocessing(profile, dat, feature_names, pp)
  if (n_remaining == 0L) {
    return(out)
  }
  algorithm <- config_algorithm(parts)
  allows <- if (is.null(algorithm)) NA else algorithm_allows_missing(algorithm)
  evidence <- list(
    n_missing = n_remaining,
    n_features_missing = sum(vapply(
      feature_names,
      function(nm) profile_field(profile, nm, "n_missing") > 0L,
      logical(1L)
    )),
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
          n_remaining,
          " missing ",
          ngettext(n_remaining, "value", "values"),
          " across ",
          evidence[["n_features_missing"]],
          " ",
          ngettext(evidence[["n_features_missing"]], "column", "columns"),
          ". Impute, or drop the incomplete cases before training."
        )
      } else {
        paste0(
          n_remaining,
          " missing ",
          ngettext(n_remaining, "value", "values"),
          " across ",
          evidence[["n_features_missing"]],
          " ",
          ngettext(evidence[["n_features_missing"]], "column", "columns"),
          " reach the learner unchanged; not every algorithm accepts them."
        )
      },
      evidence = evidence
    ))
  )
} # /rtemis::check_missing_incompatible
