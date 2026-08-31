# preprocess.R
# ::rtemis::
# 2017- EDG rtemis.org

# %% preprocessor_ops_set ----
#' Which of a set of preprocessing steps a config actually turns on
#'
#' @param config `PreprocessorConfig` object.
#' @param ops Character: Property names to test.
#'
#' @return Character: The subset that is set to something other than NULL/FALSE.
#'
#' @author EDG
#' @keywords internal
#' @noRd
preprocessor_ops_set <- function(config, ops) {
  Filter(
    function(nm) {
      value <- prop(config, nm)
      !is.null(value) && !identical(value, FALSE)
    },
    ops
  )
} # /rtemis::preprocessor_ops_set


# %% check_preprocessor_for_train ----
#' Require the preprocessor type `train()` can fit
#'
#' The rule itself is the type: `SupervisedPreprocessorConfig` does not carry
#' `PREPROCESSOR_TRAIN_EXCLUDED`, so a supervised config cannot express those
#' operations and no run has to be stopped for them. What is left here is the
#' corrective message for the one way a caller still meets the distinction --
#' handing `train()` a `PreprocessorConfig` built for `preprocess()`.
#'
#' Naming the operations they actually set is the useful part: a config that
#' happens to set none of the four differs from the supervised type in nothing
#' but its class, and saying so is what makes the fix obvious.
#'
#' @param config `PreprocessorConfig` or `SupervisedPreprocessorConfig` object.
#'
#' @return Invisible NULL. Called for the check.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_preprocessor_for_train <- function(config) {
  if (S7_inherits(config, SupervisedPreprocessorConfig)) {
    return(invisible(NULL))
  }
  # Anything that is not a preprocessor config at all fails as a type error
  # here, so the message below speaks only to the two that are.
  check_is_S7(config, PreprocessorConfig)
  excluded <- preprocessor_ops_set(config, PREPROCESSOR_TRAIN_EXCLUDED)
  rtemis.core::abort(
    "`preprocessor_config` must be a SupervisedPreprocessorConfig: use ",
    "setup_SupervisedPreprocessor() rather than setup_Preprocessor().\n",
    if (length(excluded) > 0L) {
      paste0(
        "It sets ",
        paste(excluded, collapse = ", "),
        ", which train() cannot fit: a preprocessor learned here is replayed at ",
        "predict() time, where dropping rows would return fewer predictions than ",
        "rows, and a threshold learned per fold trains the folds on different ",
        "features.\nDo those steps first, with preprocess() on the full dataset.\n"
      )
    } else {
      paste0(
        "It sets none of ",
        paste(PREPROCESSOR_TRAIN_EXCLUDED, collapse = ", "),
        ", so the same arguments build one.\n"
      )
    },
    class = c("rtemis_type_error", "rtemis_input_error")
  )
} # /rtemis::check_preprocessor_for_train


# %% frame_structure ----
#' Name the tabular structure of an object
#'
#' The three structures `preprocess()` accepts and returns. Read on entry and
#' handed to `restore_frame_structure()` on exit, so what a caller passes in is
#' what they get back.
#'
#' @param x tabular data.
#'
#' @return Character, one of "data.table", "tibble", "data.frame".
#'
#' @author EDG
#' @keywords internal
#' @noRd
frame_structure <- function(x) {
  if (data.table::is.data.table(x)) {
    "data.table"
  } else if (inherits(x, "tbl_df")) {
    "tibble"
  } else {
    "data.frame"
  }
} # /rtemis::frame_structure


# %% as_working_dt ----
#' Copy tabular data into a data.table to work on
#'
#' The one copy `preprocess()` pays, and it is not optional: `setDT()` converts
#' *by reference*, so calling it on a caller's data.frame would turn their object
#' into a data.table underneath them, and a caller's data.table would be
#' rewritten by the first `set()`. Every step downstream mutates this copy by
#' reference and costs nothing.
#'
#' Row names are dropped here, which is deliberate -- see `preprocess()`.
#'
#' @param x tabular data.
#'
#' @return data.table sharing no column with `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_working_dt <- function(x) {
  if (data.table::is.data.table(x)) {
    data.table::copy(x)
  } else {
    data.table::as.data.table(x)
  }
} # /rtemis::as_working_dt


# %% restore_frame_structure ----
#' Return a working data.table as the structure it came in as
#'
#' Free for all three: a data.table is already one, `setDF()` re-classes by
#' reference, and `as_tibble()` on the resulting data.frame only re-classes
#' again. Calling `as_tibble()` on the data.table *directly* would copy every
#' column, which is why the two-step path is taken.
#'
#' @param x data.table: Working table.
#' @param structure Character: Structure to restore, from `frame_structure()`.
#'
#' @return `x` as a data.table, data.frame or tibble.
#'
#' @author EDG
#' @keywords internal
#' @noRd
restore_frame_structure <- function(x, structure) {
  if (structure == "data.table") {
    return(x)
  }
  data.table::setDF(x)
  if (structure == "tibble") {
    check_dependencies("tibble")
    x <- tibble::as_tibble(x)
  }
  x
} # /rtemis::restore_frame_structure


# %% dt_filter_cases ----
#' Keep a subset of a working table's cases
#'
#' Subsetting each column rather than writing `x[keep]` because data.table
#' evaluates `i` against the table's own columns first: a column named `keep`
#' would silently be used in place of the filter.
#'
#' @param x data.table: Working table.
#' @param keep Logical vector, one element per case.
#'
#' @return data.table holding the kept cases, sharing no column with `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
dt_filter_cases <- function(x, keep) {
  data.table::setDT(lapply(x, function(column) column[keep]))[]
} # /rtemis::dt_filter_cases


# %% check_columns_remain ----
#' Reject a step that left the working table with no columns
#'
#' A data.table with no columns has no cases either, so the pipeline cannot
#' carry a case count past this point -- and a preprocessed dataset with no
#' features is not usable by anything downstream regardless.
#'
#' @param x data.table: Working table.
#' @param step Character: Name of the configuration field responsible.
#'
#' @return Invisible NULL. Called for the check.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_columns_remain <- function(x, step) {
  if (ncol(x) == 0L) {
    rtemis.core::abort(
      "`",
      step,
      "` left no columns to preprocess.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis::check_columns_remain


# %% is_integer_column ----
#' Is a column integer-valued
#'
#' `bit64::integer64` is a double carrying a class, so `is.integer()` is FALSE
#' for it, and `integer2factor` / `integer2numeric` are documented to cover it.
#'
#' @param x Vector: Column to test.
#'
#' @return Logical.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_integer_column <- function(x) {
  is.integer(x) || inherits(x, "integer64")
} # /rtemis::is_integer_column


# %% dt_impute_columns ----
#' Fill a working table's missing values, one column at a time
#'
#' Each column is replaced whole rather than assigned into at the missing
#' positions: a partial `set()` coerces the value into the column's existing
#' type, so a mean imputed into an integer column would be truncated where a
#' data.frame subassignment widens the column. `impute_discrete` and
#' `impute_continuous` are free-text function names, so the value's type is not
#' knowable here.
#'
#' @param x data.table: Working table. Modified in place.
#' @param fn Character: Name of a function returning one value from a vector.
#' @param select Function: Predicate choosing the columns `fn` applies to.
#'
#' @return Invisible NULL. Called for its effect on `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
dt_impute_columns <- function(x, fn, select) {
  for (nm in names(x)) {
    column <- x[[nm]]
    if (!select(column) || !anyNA(column)) {
      next
    }
    column[is.na(column)] <- do_call(fn, list(column, na.rm = TRUE))
    data.table::set(x, j = nm, value = column)
  }
  invisible(NULL)
} # /rtemis::dt_impute_columns


# %% preprocess(x, AnyPreprocessorConfig, ...) ----
# Registered for the union, so the one method serves both configs: S7 registers
# a union signature against each member class.
method(
  preprocess,
  list(class_tabular, AnyPreprocessorConfig)
) <- function(
  x,
  config,
  dat_validation = NULL,
  dat_test = NULL,
  verbosity = 1L
) {
  # -> Preprocessor
  # Intro ----
  start_time <- intro(verbosity = verbosity - 1L)
  # Init values list for Preprocessor output.
  values <- list(
    scale_centers = NULL,
    scale_coefficients = NULL,
    one_hot_levels = NULL,
    factor2integer_levels = NULL,
    remove_features = NULL
  )

  # Data ----
  # Every step below runs against a data.table, so a column is replaced or added
  # by reference instead of the whole frame being copied. Entry costs one copy
  # and exit costs none; the caller's structure is restored at the end and their
  # object is never written through.
  #
  # Row names do not survive, by design: a data.table has none, a case
  # identifier that matters belongs in a column where it can be selected,
  # joined, validated and serialized, and nothing a run record reports can carry
  # one.
  input_structure <- frame_structure(x)
  x <- as_working_dt(x)
  if (ncol(x) == 0L) {
    rtemis.core::abort(
      "`x` has no columns.",
      class = c("rtemis_dim_error", "rtemis_data_error")
    )
  }

  # Complete cases ----
  if (isTRUE(pp_opt(config, "complete_cases"))) {
    msg("Filtering complete cases...", verbosity = verbosity)
    keep <- complete.cases(x)
    if (!all(keep)) {
      x <- dt_filter_cases(x, keep)
    }
  }

  # Set aside excluded ----
  # Held as a plain list of columns: they take part in no step, only in the case
  # filters below, and are appended after the last step that adds columns.
  excluded <- NULL
  if (!is.null(config@exclude) && length(config@exclude) > 0) {
    excluded_names <- names(x)[config@exclude]
    excluded <- stats::setNames(
      lapply(excluded_names, function(nm) x[[nm]]),
      excluded_names
    )
    data.table::set(x, j = excluded_names, value = NULL)
    check_columns_remain(x, "exclude")
  }

  # Remove named features ----
  if (!is.null(config@remove_features)) {
    msg(
      "Removing",
      length(config@remove_features),
      "features...",
      verbosity = verbosity
    )
    values$remove_features <- config@remove_features
    present <- intersect(names(x), config@remove_features)
    if (length(present) > 0L) {
      data.table::set(x, j = present, value = NULL)
    }
    check_columns_remain(x, "remove_features")
  }

  # Remove constants ----
  # Must be ahead of numeric quantile at least
  if (config@remove_constants) {
    constant <- names(x)[vapply(
      x,
      is_constant,
      logical(1L),
      skip_missing = config@remove_constants_skip_missing
    )]
    if (length(constant) > 0) {
      if (verbosity > 0L) {
        msg0(
          "Removing ",
          singorplu(length(constant), "constant feature"),
          "..."
        )
      }
      data.table::set(x, j = constant, value = NULL)
      check_columns_remain(x, "remove_constants")
    }
  }

  # Remove duplicates ----
  if (isTRUE(pp_opt(config, "remove_duplicates"))) {
    duplicate <- duplicated(x, by = names(x))
    Ndups <- sum(duplicate)
    if (Ndups > 0) {
      if (verbosity > 0L) {
        msg0("Removing ", singorplu(Ndups, "duplicate case"), "...")
      }
      x <- dt_filter_cases(x, !duplicate)
      excluded <- lapply(excluded, function(column) column[!duplicate])
    }
  }

  # Remove Cases by missing feature threshold ----
  remove_cases_thres <- pp_opt(config, "remove_cases_thres")
  if (!is.null(remove_cases_thres) && anyNA(x)) {
    na_fraction_bycase <- rowSums(is.na(x)) / ncol(x)
    over_thres <- na_fraction_bycase >= remove_cases_thres
    if (any(over_thres)) {
      msg(
        "Removing",
        sum(over_thres),
        "cases with >=",
        remove_cases_thres,
        "missing data...",
        verbosity = verbosity
      )
      x <- dt_filter_cases(x, !over_thres)
      excluded <- lapply(excluded, function(column) column[!over_thres])
    }
  }

  # Remove Features by missing feature threshold ----
  remove_features_thres <- pp_opt(config, "remove_features_thres")
  if (!is.null(remove_features_thres) && anyNA(x)) {
    na_fraction_byfeat <- vapply(
      x,
      function(column) sum(is.na(column)) / length(column),
      numeric(1L)
    )
    over_thres <- names(x)[
      na_fraction_byfeat >= remove_features_thres
    ]
    if (length(over_thres) > 0) {
      msg(
        "Removing",
        length(over_thres),
        "features with >=",
        remove_features_thres,
        "missing data...",
        verbosity = verbosity
      )
      data.table::set(x, j = over_thres, value = NULL)
      check_columns_remain(x, "remove_features_thres")
    }
  }

  # Add date features ----
  # Feature *creation*, so it runs ahead of every transformation below rather
  # than after them: the weekday and month factors are then one-hot encoded or
  # coded to integers like any other factor, and the year is scaled like any
  # other numeric. It runs *after* the case and feature filters, which judge the
  # data as it was given rather than what was derived from it.
  if (config@add_date_features) {
    msg("Extracting date features...", verbosity = verbosity)
    # Find date columns
    date_names <- names(x)[vapply(
      x,
      function(column) inherits(column, "Date"),
      logical(1L)
    )]
    # For each date column, extract features
    for (nm in date_names) {
      .date_features <- dates2features(
        x[[nm]],
        features = config@date_features
      )
      data.table::set(
        x,
        j = paste0(nm, "_", names(.date_features)),
        value = .date_features
      )
    }
  }

  # Add holidays ----
  if (config@add_holidays) {
    msg("Extracting holidays...", verbosity = verbosity)
    # Find date columns
    date_names <- names(x)[vapply(
      x,
      function(column) inherits(column, "Date"),
      logical(1L)
    )]
    # For each date column, extract holidays
    for (nm in date_names) {
      data.table::set(
        x,
        j = paste0(nm, "_holidays"),
        value = get_holidays(x[[nm]], config@holidays)
      )
    }
  }

  # Integer to factor ----
  if (config@integer2factor) {
    integer_names <- names(x)[vapply(x, is_integer_column, logical(1L))]
    if (verbosity > 0L) {
      if (length(integer_names) > 0) {
        msg(
          "Converting",
          singorplu(length(integer_names), "integer"),
          "to factor..."
        )
      } else {
        msg("No integers to convert to factor...")
      }
    }
    for (nm in integer_names) {
      data.table::set(x, j = nm, value = as.factor(x[[nm]]))
    }
  }

  # Logical to factor ----
  if (config@logical2factor) {
    logical_names <- names(x)[vapply(x, is.logical, logical(1L))]
    if (verbosity > 0L) {
      if (length(logical_names) > 0) {
        msg0(
          "Converting ",
          singorplu(length(logical_names), "logical feature"),
          " to ",
          ngettext(length(logical_names), "factor", "factors"),
          "..."
        )
      } else {
        msg("No logicals to convert to factor...")
      }
    }
    for (nm in logical_names) {
      data.table::set(x, j = nm, value = as.factor(x[[nm]]))
    }
  }

  # Numeric to factor ----
  if (config@numeric2factor) {
    numeric_names <- names(x)[vapply(x, is.numeric, logical(1L))]
    msg("Converting numeric to factors...", verbosity = verbosity)
    for (nm in numeric_names) {
      converted <- if (is.null(config@numeric2factor_levels)) {
        as.factor(x[[nm]])
      } else {
        factor(x[[nm]], levels = config@numeric2factor_levels)
      }
      data.table::set(x, j = nm, value = converted)
    }
  }

  # Character to factor ----
  if (config@character2factor) {
    character_names <- names(x)[vapply(x, is.character, logical(1L))]
    if (verbosity > 0L) {
      if (length(character_names) > 0) {
        msg0(
          "Converting ",
          singorplu(length(character_names), "character feature"),
          " to ",
          ngettext(length(character_names), "a factor", "factors"),
          "..."
        )
      } else {
        msg("No character features to convert to factors found.")
      }
    }
    for (nm in character_names) {
      data.table::set(x, j = nm, value = as.factor(x[[nm]]))
    }
  }

  # unique_len2factor ----
  if (config@unique_len2factor > 1) {
    short_names <- names(x)[vapply(
      x,
      function(column) {
        !is.factor(column) &&
          length(unique(column)) <= config@unique_len2factor
      },
      logical(1L)
    )]
    if (verbosity > 0L) {
      if (length(short_names) > 0) {
        msg(
          "Converting",
          singorplu(length(short_names), "feature"),
          "with <=",
          config@unique_len2factor,
          "unique values to factors..."
        )
      } else {
        msg(
          "No features with <=",
          config@unique_len2factor,
          "unique values found."
        )
      }
    }
    for (nm in short_names) {
      data.table::set(x, j = nm, value = factor(x[[nm]]))
    }
  }

  # Integer to numeric ----
  # Read from the data as it stands: `integer2factor` above leaves nothing
  # integer behind, so the two options together convert once, not twice.
  if (config@integer2numeric) {
    integer_names <- names(x)[vapply(x, is_integer_column, logical(1L))]
    if (verbosity > 0L) {
      if (length(integer_names) > 0) {
        msg(
          "Converting",
          singorplu(length(integer_names), "integer"),
          "to numeric..."
        )
      } else {
        msg("No integers to convert to numeric...")
      }
    }
    for (nm in integer_names) {
      data.table::set(x, j = nm, value = as.numeric(x[[nm]]))
    }
  }

  # Logical to numeric ----
  if (config@logical2numeric) {
    logical_names <- names(x)[vapply(x, is.logical, logical(1L))]
    msg("Converting logicals to numeric...", verbosity = verbosity)
    for (nm in logical_names) {
      data.table::set(x, j = nm, value = as.numeric(x[[nm]]))
    }
  }

  # Numeric cut ----
  if (config@numeric_cut_n > 0) {
    numeric_names <- names(x)[vapply(x, is.numeric, logical(1L))]
    msg(
      "Cutting numeric features in",
      config@numeric_cut_n,
      "bins...",
      verbosity = verbosity
    )
    for (nm in numeric_names) {
      data.table::set(
        x,
        j = nm,
        value = factor(
          cut(
            x[[nm]],
            breaks = config@numeric_cut_n,
            labels = config@numeric_cut_labels
          )
        )
      )
    }
  }

  # Numeric quantile ----
  if (config@numeric_quant_n > 0) {
    quant_names <- if (config@numeric_quant_NAonly) {
      names(x)[vapply(
        x,
        function(column) is.numeric(column) && anyNA(column),
        logical(1L)
      )]
    } else {
      names(x)[vapply(x, is.numeric, logical(1L))]
    }
    if (length(quant_names) > 0) {
      msg(
        "Cutting numeric features in",
        config@numeric_quant_n,
        "quantiles...",
        verbosity = verbosity
      )
      for (nm in quant_names) {
        rng <- abs(diff(range(x[[nm]], na.rm = TRUE)))
        quantiles <- quantile(
          x[[nm]],
          probs = seq(0, 1, length.out = config@numeric_quant_n),
          na.rm = TRUE
        )
        quantiles[1] <- quantiles[1] - .02 * rng
        quantiles[config@numeric_quant_n] <- quantiles[
          config@numeric_quant_n
        ] +
          .02 * rng
        quantiles <- unique(quantiles)
        data.table::set(
          x,
          j = nm,
          value = factor(cut(x[[nm]], breaks = quantiles))
        )
      }
    }
  }

  # factor NA to level ----
  if (config@factorNA2missing) {
    factor_names <- names(x)[vapply(x, is.factor, logical(1L))]
    if (verbosity > 0L) {
      if (length(factor_names) > 0) {
        msg0(
          "Converting ",
          length(factor_names),
          ngettext(length(factor_names), " factor's", " factors'"),
          " NA values to level '",
          config@factorNA2missing_level,
          "'..."
        )
      } else {
        msg("No factors found.")
      }
    }
    for (nm in factor_names) {
      data.table::set(
        x,
        j = nm,
        value = factor_NA2missing(x[[nm]], config@factorNA2missing_level)
      )
    }
  }

  # Factor to integer ----
  # e.g. for algorithms that do not support factors directly, but can handle integers
  # as categorical (e.g. LightGBM)
  if (config@factor2integer) {
    factor_names <- names(x)[vapply(x, is.factor, logical(1L))]
    if (verbosity > 0L) {
      if (length(factor_names) > 0) {
        msg(
          "Converting",
          singorplu(length(factor_names), "factor"),
          "to integer..."
        )
      } else {
        msg("No factors found to convert to integer...")
      }
    }
    # Each feature is coded against a fixed set of levels, taken from
    # `factor2integer_levels` when it carries the feature and learned from the
    # data otherwise. Learned levels are published in `values` so that
    # `apply_preprocessor()` codes new data the way the training data was coded.
    learned_levels <- list()
    for (nm in factor_names) {
      feature_levels <- config@factor2integer_levels[[nm]]
      if (is.null(feature_levels)) {
        feature_levels <- levels(x[[nm]])
      }
      learned_levels[[nm]] <- feature_levels
      data.table::set(
        x,
        j = nm,
        value = factor2integer_code(
          x[[nm]],
          factor_levels = feature_levels,
          startat0 = config@factor2integer_startat0,
          xname = nm,
          verbosity = verbosity
        )
      )
    }
    if (length(learned_levels) > 0L) {
      values$factor2integer_levels <- learned_levels
    }
  }

  # Missingness ----
  if (config@missingness) {
    names_with_na <- names(x)[vapply(x, anyNA, logical(1L))]
    for (nm in names_with_na) {
      data.table::set(
        x,
        j = paste0(nm, "_missing"),
        value = factor(as.numeric(is.na(x[[nm]])))
      )
      if (verbosity > 0L) {
        msg0("Created missingness indicator for ", nm, "...")
      }
    }
  }

  # Impute ----
  if (config@impute) {
    if (config@impute_type == "missRanger") {
      # '- missRanger ----
      check_dependencies("missRanger")
      if (verbosity > 0L) {
        if (config@impute_missRanger_params[["pmm.k"]] > 0) {
          msg(
            "Imputing missing values using predictive mean matching with missRanger..."
          )
        } else {
          msg("Imputing missing values using missRanger...")
        }
      }
      # The external imputers rebuild the frame, so they are handed a
      # data.frame and their result is adopted as the working table. `setDT()`
      # is by reference, which is safe on an object they just created.
      x <- data.table::setDT(missRanger::missRanger(
        data.table::setDF(x),
        pmm.k = config@impute_missRanger_params[["pmm.k"]],
        verbose = verbosity
      ))
    } else if (config@impute_type == "micePMM") {
      check_dependencies("mice")
      msg(
        "Imputing missing values by predictive mean matching using mice...",
        verbosity = verbosity
      )
      x <- data.table::setDT(mice::complete(mice::mice(
        data.table::setDF(x),
        m = 1,
        method = "pmm"
      )))
    } else {
      # '- mean/mode ----
      msg(
        "Imputing missing values using",
        config@impute_discrete,
        "(discrete) and",
        config@impute_continuous,
        "(continuous)...",
        verbosity = verbosity
      )
      # Discrete first: `is_discrete()` claims integer, so an integer column is
      # imputed with the discrete function and never meets the continuous one.
      dt_impute_columns(x, config@impute_discrete, is_discrete)
      dt_impute_columns(x, config@impute_continuous, is.numeric)
    }
  }

  # Scale +/- center ----
  if (config@scale || config@center) {
    # Get names of numeric features
    numeric_names <- names(x)[vapply(x, is.numeric, logical(1L))]
    # `factor2integer` runs above and emits codes that are `is.numeric()`, but a
    # category code is not a numeric feature: standardizing it yields a fraction
    # of an index, which no consumer can read back -- an embedding indexes with
    # it, LightGBM reads it as a category. The coded columns are exactly those
    # just recorded, so they are dropped here by name.
    numeric_names <- setdiff(
      numeric_names,
      names(values[["factor2integer_levels"]])
    )
    sc <- if (config@scale) "Scaling" else NULL
    ce <- if (config@center) "centering" else NULL
    if (length(numeric_names) > 0) {
      msg(
        paste(c(sc, ce), collapse = " and "),
        length(numeric_names),
        "numeric features...",
        verbosity = verbosity
      )
      # Info: scale outputs a matrix.
      scale_ <- if (!is.null(config@scale_coefficients)) {
        # Check names match
        stopifnot(identical(names(config@scale_coefficients), numeric_names))
        config@scale_coefficients
      } else {
        config@scale
      }
      center_ <- if (!is.null(config@scale_centers)) {
        # Check names match
        stopifnot(identical(names(config@scale_centers), numeric_names))
        config@scale_centers
      } else {
        config@center
      }
      x_num_scaled <- scale(
        matrix(
          unlist(
            lapply(numeric_names, function(nm) x[[nm]]),
            use.names = FALSE
          ),
          nrow = nrow(x),
          dimnames = list(NULL, numeric_names)
        ),
        scale = scale_,
        center = center_
      )

      # Collect scale and center values
      values$scale_centers <- attr(x_num_scaled, "scaled:center")
      values$scale_coefficients <- attr(x_num_scaled, "scaled:scale")

      # Insert into original dataset
      for (k in seq_along(numeric_names)) {
        data.table::set(x, j = numeric_names[k], value = x_num_scaled[, k])
      }
    } else {
      msg(
        paste(c(sc, ce), collapse = " and "),
        "was requested \n                                but no numeric features were found: Please check data.",
        verbosity = verbosity
      )
    }
  }

  # One Hot Encoding ----
  if (config@one_hot) {
    # Each feature is encoded against a fixed set of levels, taken from
    # `one_hot_levels` when it carries the feature and learned from the data
    # otherwise. Learned levels are published in `values` so that
    # `apply_preprocessor()` gives new data the same columns, in the same order,
    # holding the same values.
    factor_names <- names(x)[vapply(x, is.factor, logical(1L))]
    learned_levels <- list()
    for (nm in factor_names) {
      feature_levels <- config@one_hot_levels[[nm]]
      if (is.null(feature_levels)) {
        feature_levels <- levels(x[[nm]])
      }
      learned_levels[[nm]] <- feature_levels
    }
    if (length(learned_levels) > 0L) {
      values$one_hot_levels <- learned_levels
    }
    # The data.table method assembles a new table, which becomes the working
    # one; it is the native encoder this pipeline exists to reach.
    x <- one_hot(
      x,
      verbosity = verbosity,
      factor_levels = learned_levels
    )
  }

  # Add back excluded ----
  # Appended, so an excluded column ends up after the preprocessed ones rather
  # than in the position it held on entry.
  if (!is.null(excluded)) {
    # A step that adds columns can coin a name an excluded column already holds
    # -- `missingness` makes `<feature>_missing`, `one_hot` makes
    # `<feature>_<level>`. `set()` would overwrite the new column with the
    # excluded one, losing it silently.
    collision <- intersect(names(x), names(excluded))
    if (length(collision) > 0L) {
      rtemis.core::abort(
        "Preprocessing created ",
        ngettext(length(collision), "a column", "columns"),
        " named after excluded ",
        ngettext(length(collision), "one", "ones"),
        ": ",
        paste(collision, collapse = ", "),
        ".\nRename the excluded ",
        ngettext(length(collision), "column", "columns"),
        " before calling preprocess().",
        class = c("rtemis_value_error", "rtemis_data_error")
      )
    }
    data.table::set(x, j = names(excluded), value = excluded)
  } # /add back excluded

  x <- restore_frame_structure(x, input_structure)
  msg("Preprocessing done.", verbosity = verbosity)

  preprocessed <- list(training = x)

  if (!is.null(dat_validation)) {
    msg("Applying preprocessing to validation data...", verbosity = verbosity)
    preprocessed$validation <- apply_preprocessor(
      preprocessor = Preprocessor(
        config = config,
        preprocessed = list(),
        scale_centers = values[["scale_centers"]],
        scale_coefficients = values[["scale_coefficients"]],
        one_hot_levels = values[["one_hot_levels"]],
        factor2integer_levels = values[["factor2integer_levels"]],
        remove_features = values[["remove_features"]]
      ),
      new_data = dat_validation,
      verbosity = verbosity
    )
  }
  if (!is.null(dat_test)) {
    msg("Applying preprocessing to test data...", verbosity = verbosity)
    preprocessed$test <- apply_preprocessor(
      preprocessor = Preprocessor(
        config = config,
        preprocessed = list(),
        scale_centers = values[["scale_centers"]],
        scale_coefficients = values[["scale_coefficients"]],
        one_hot_levels = values[["one_hot_levels"]],
        factor2integer_levels = values[["factor2integer_levels"]],
        remove_features = values[["remove_features"]]
      ),
      new_data = dat_test,
      verbosity = verbosity
    )
  }
  outro(start_time, verbosity = verbosity - 1L)
  Preprocessor(
    config = config,
    preprocessed = if (length(preprocessed) == 1) {
      preprocessed[[1]]
    } else {
      preprocessed
    },
    scale_centers = values[["scale_centers"]],
    scale_coefficients = values[["scale_coefficients"]],
    one_hot_levels = values[["one_hot_levels"]],
    factor2integer_levels = values[["factor2integer_levels"]],
    remove_features = values[["remove_features"]]
  )
} # /rtemis::preprocess(PreprocessorConfig, ...)


# %% preprocess(x, missing, ...) ----
method(
  preprocess,
  list(class_tabular, class_missing)
) <- function(
  x,
  config,
  dat_validation = NULL,
  dat_test = NULL,
  verbosity = 1L,
  ...
) {
  # -> Preprocessor
  if (...length() == 0L) {
    rtemis.core::abort(
      "No preprocessing parameters specified.\n",
      "Pass a PreprocessorConfig created with setup_Preprocessor(), ",
      "or pass setup_Preprocessor() arguments directly, e.g. preprocess(x, scale = TRUE).",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  preprocess(
    x,
    config = setup_Preprocessor(...),
    dat_validation = dat_validation,
    dat_test = dat_test,
    verbosity = verbosity
  )
} # /rtemis::preprocess(missing, ...)


# %% apply_preprocessor ----
#' Apply trained Preprocessor to new data
#'
#' Apply a trained `Preprocessor` to new data, reusing the values learned from the training
#' data. For example, the same scale centers and coefficients, one-hot levels, and removed
#' features will be applied to the new data.
#'
#' @param preprocessor `Preprocessor`: Trained preprocessor, i.e. the output of [preprocess].
#' @param new_data Tabular data, i.e. data.frame, data.table, or tbl_df (tibble):
#' New data to preprocess.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Preprocessed data, in the same structure as `new_data`.
#'
#' @author EDG
#' @seealso [preprocess], [setup_Preprocessor]
#' @export
#' @examples
#' res <- resample(iris, setup_Resampler(seed = 2026))
#' iris_train <- iris[res[[1]], ]
#' iris_test <- iris[-res[[1]], ]
#'
#' # Preprocess training data
#' iris_pre <- preprocess(iris_train, setup_Preprocessor(scale = TRUE, center = TRUE))
#'
#' # Apply the same preprocessing to test data
#' iris_test_pre <- apply_preprocessor(iris_pre, iris_test)
apply_preprocessor <- function(preprocessor, new_data, verbosity = 1L) {
  # -> data.frame or data.table
  check_is_S7(preprocessor, Preprocessor)
  preprocessed(preprocess(
    new_data,
    fitted_config(preprocessor),
    verbosity = verbosity
  ))
} # /rtemis::apply_preprocessor


# %% fitted_config ----
#' A `Preprocessor`'s config with its learned values filled in
#'
#' `@config` holds what the user asked for and `@values` what preprocessing
#' learned, which keeps the input intact -- but re-applying to new data needs
#' the two merged, and so does a run record, which must report the values
#' actually used. One merge, so the two cannot disagree.
#'
#' The merged fields are settable inputs: supplying `scale_centers` makes
#' `preprocess()` use it instead of computing one. That is why the merge is
#' `@values` over `@config` and not the reverse -- a learned value only fills a
#' slot the user left empty.
#'
#' @param preprocessor `Preprocessor` object.
#'
#' @return `PreprocessorConfig` with the learned values applied.
#'
#' @author EDG
#' @seealso [apply_preprocessor], [setup_Preprocessor]
#' @export
#' @examples
#' iris_pre <- preprocess(iris, setup_Preprocessor(scale = TRUE, center = TRUE))
#' # The config as supplied carries no centers; the fitted one carries the
#' # centers `preprocess()` computed.
#' fitted_config(iris_pre)@scale_centers
fitted_config <- function(preprocessor) {
  config <- preprocessor@config
  for (nm in c(
    "scale_centers",
    "scale_coefficients",
    "one_hot_levels",
    "factor2integer_levels",
    "remove_features"
  )) {
    learned <- preprocessor@values[[nm]]
    if (!is.null(learned)) {
      prop(config, nm) <- learned
    }
  }
  config
} # /rtemis::fitted_config


# %% pinned_level_index ----
#' Index a factor's values into a fixed set of levels
#'
#' `as.integer()` reads whatever levels the factor itself carries, so the same
#' value lands at a different position in data whose levels are fewer or
#' differently ordered. Indexing into a fixed set instead is what lets
#' `preprocess()` learn a coding on the training data and `apply_preprocessor()`
#' reproduce it, and it is shared by both encoders so that "unseen" means the
#' same thing to each.
#'
#' Mapping level positions once and then indexing by the factor's existing codes
#' does this in a single pass, with no character conversion.
#'
#' @param x Factor: Feature to index.
#' @param factor_levels Character vector: Levels to index into, in order.
#'
#' @return Integer vector of positions in `factor_levels`, `NA` where the case
#' is `NA` or its level is absent from `factor_levels`. Callers decide what an
#' absent level means.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pinned_level_index <- function(x, factor_levels) {
  match(levels(x), factor_levels)[as.integer(x)]
} # /rtemis::pinned_level_index


# %% factor2integer_code ----
#' Integer-code a factor against a fixed set of levels
#'
#' A value whose level is absent from `factor_levels` is out of vocabulary and
#' takes the single index above the known levels: `length(factor_levels)` when
#' `startat0` is TRUE, `length(factor_levels) + 1L` otherwise. A model consuming
#' the codes therefore sizes the feature at `length(factor_levels) + 1L`
#' categories. `NA` stays `NA`.
#'
#' Both `startat0` branches return integer. A category code indexes something --
#' an embedding table, a LightGBM category -- and a double cannot.
#'
#' @param x Factor: Feature to code.
#' @param factor_levels Character vector: Levels to code against, in order.
#' @param startat0 Logical: If TRUE, code the first level as 0 instead of 1.
#' @param xname Character: Feature name, used in messages.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Integer vector of codes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
factor2integer_code <- function(
  x,
  factor_levels,
  startat0,
  xname,
  verbosity = 1L
) {
  oov <- length(factor_levels) + 1L
  code <- pinned_level_index(x, factor_levels)
  unseen <- is.na(code) & !is.na(x)
  code[unseen] <- oov
  n_oov <- sum(unseen)
  if (n_oov > 0L && verbosity > 0L) {
    msg0(
      "Feature '",
      xname,
      "': coding ",
      singorplu(n_oov, "case"),
      " with unseen levels as ",
      if (startat0) oov - 1L else oov,
      "..."
    )
  }
  if (startat0) code - 1L else code
} # /rtemis::factor2integer_code


# %% one_hot_index ----
#' Resolve what a feature is one-hot encoded into
#'
#' The policy half of one-hot encoding, shared by every `one_hot()` method:
#' which levels the feature is encoded against, which of them each case takes,
#' what the resulting columns are called, and how many cases had no column to
#' take. Only the materialization differs per data structure -- a matrix for a
#' data.frame, a column per level for a data.table -- so keeping this here is
#' what stops two encoders from drifting into two encodings.
#'
#' Levels come from `factor_levels` when it carries the feature and from the
#' feature itself otherwise. Lookup is by name and tolerant of entries with no
#' matching column: `train()` learns a map on data that includes the outcome and
#' applies it to features alone.
#'
#' @param x Factor: Feature to encode.
#' @param factor_levels Optional named list of the form "feature_name" =
#' "levels": Levels to encode each feature against.
#' @param xname Character: Feature name, used for lookup and column names.
#'
#' @return List of `levels` (character), `index` (integer position in `levels`
#' per case, `NA` where the case is `NA` or its level is absent), `names`
#' (character, one column name per level) and `n_unseen` (integer count of
#' non-`NA` cases whose level is absent from `levels`).
#'
#' @author EDG
#' @keywords internal
#' @noRd
one_hot_index <- function(x, factor_levels, xname) {
  .levels <- factor_levels[[xname]]
  if (is.null(.levels)) {
    .levels <- levels(x)
  }
  index <- pinned_level_index(x, .levels)
  list(
    levels = .levels,
    index = index,
    names = paste0(xname, "_", .levels),
    n_unseen = sum(is.na(index) & !is.na(x))
  )
} # /rtemis::one_hot_index


# %% report_unseen_levels ----
#' Report the cases one-hot encoding left all-zero
#'
#' @param n_unseen Named integer vector: Count of cases with unseen levels, one
#' entry per encoded feature.
#'
#' @return `NULL`, invisibly. Called for its console output.
#'
#' @author EDG
#' @keywords internal
#' @noRd
report_unseen_levels <- function(n_unseen) {
  for (feature in names(n_unseen)[n_unseen > 0L]) {
    msg0(
      "Feature '",
      feature,
      "': encoding ",
      singorplu(n_unseen[[feature]], "case"),
      " with unseen levels as all-zero..."
    )
  }
  invisible(NULL)
} # /rtemis::report_unseen_levels


# %% one_hot ----
#' @name one_hot
#'
#' @title
#' One hot encoding
#'
#' @description
#' One hot encode a vector or factors in a data.frame
#'
#' @details
#' A vector input will be one-hot encoded regardless of type by looking at all unique values. With data.frame input,
#' only column of type factor will be one-hot encoded.
#' This function is used by [preprocess].
#'
#' The data.frame and data.table methods encode identically and return the
#' structure they were given, each assembled with that structure's own
#' operations. Both operate on a copy of their input; `dt_set_one_hot()` encodes
#' a data.table ***in-place*** instead. Each column keeps its own type: an
#' expanded factor contributes its indicator columns, in its own position, and
#' every other column is passed through as it was.
#'
#' Each feature is encoded against `factor_levels` when it carries an entry for
#' it, and against the feature's own levels otherwise. The pinned set fixes both
#' the columns and which column each value takes, so new data whose factor has
#' fewer or differently ordered levels is encoded exactly as the training data
#' was. A case whose level is absent from the pinned set -- or which is `NA` --
#' has no column to take and stays all-zero, which is the width-preserving
#' degradation; `factor2integer` instead reserves an index, because an embedding
#' must index something.
#'
#' @param x Vector or data.frame
#' @param xname Character: Variable name
#' @param factor_levels Optional Named list of the form "feature_name" =
#' "levels": Levels to encode each feature against. Lookup is by name and
#' tolerant of entries with no matching column.
#' @param verbosity Integer: Verbosity level.
#'
#' @return For vector input, a one-hot-encoded matrix; for tabular input, an
#' expanded object of the same class where all factors are one-hot encoded.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' # factor with only one unique value but 2 levels:
#' vf <- factor(rep("alpha", 20), levels = c("alpha", "beta"))
#' vf_one_hot <- one_hot(vf)
#' vf_one_hot
method(one_hot, class_any) <- function(x, xname = NULL, verbosity = 1L) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  # ensures if factor without all levels present, gets all columns created
  if (!is.factor(x)) {
    x <- factor(x)
  }
  .levels <- levels(x)
  ncases <- NROW(x)
  index <- as.integer(x)
  oh <- matrix(0, ncases, length(.levels))
  colnames(oh) <- paste(xname, .levels, sep = "_")
  for (i in seq(ncases)) {
    oh[i, index[i]] <- 1
  }
  oh
} # /rtemis::one_hot.default


# included for benchmarking mostly
one_hotcm <- function(
  x,
  xname = deparse(substitute(x)),
  return = "data.frame"
) {
  stopifnot(is.factor(x))
  dt <- data.table(
    ID = seq_along(x),
    x = x
  )
  setnames(dt, "x", xname)
  out <- dcast(
    melt(dt, id.vars = "ID"),
    ID ~ variable + value,
    fun.aggregate = length
  )[, -1]
  if (return == "data.frame") {
    setDF(out)
  }
  out
}

# loop is faster than dcast/melt
# x <- iris$Species
# microbenchmark::microbenchmark(loop = one_hot.default(x), dt = one_hotcm(x))

# %% one_hot.data.frame ----
#' @rdname one_hot
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' one_hot(iris) |> head()
method(one_hot, class_data.frame) <- function(
  x,
  factor_levels = NULL,
  verbosity = 1L
) {
  ncases <- NROW(x)
  factor_index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  n_unseen <- integer()
  one.hot <- as.list(x)
  for (i in factor_index) {
    if (verbosity > 0L) {
      msgstart("One hot encoding ", .names[i], "...")
    }
    enc <- one_hot_index(x[[i]], factor_levels, .names[i])
    index <- enc[["index"]]
    oh <- matrix(0, ncases, length(enc[["levels"]]))
    colnames(oh) <- enc[["names"]]
    # A case with no column to take -- an unseen level, or NA -- stays all-zero.
    present <- which(!is.na(index))
    oh[cbind(present, index[present])] <- 1
    n_unseen[[.names[i]]] <- enc[["n_unseen"]]
    # Replace list element that was a factor with one-hot encoded matrix
    one.hot[[i]] <- oh
  }
  if (verbosity > 0L) {
    msgdone()
    report_unseen_levels(n_unseen)
  }
  # `cbind` would build a matrix and coerce every column to the widest common
  # type, so a single character column would make the whole frame character.
  # `cbind.data.frame` keeps each column's type but prefixes an expanded
  # matrix's columns with its list element's name (`grp.grp_a`). Dropping the
  # list names is what stops the prefixing; the names are then restored from
  # what each element actually contributes -- an encoded factor's matrix
  # columns, or the column's own name.
  out <- do.call(cbind.data.frame, unname(one.hot))
  names(out) <- unlist(
    lapply(seq_along(one.hot), function(i) {
      if (is.matrix(one.hot[[i]])) {
        colnames(one.hot[[i]])
      } else {
        names(one.hot)[[i]]
      }
    }),
    use.names = FALSE
  )
  out
} # /rtemis::one_hot.data.frame


# %% one_hot.data.table ----
#' @rdname one_hot
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' ir <- data.table::as.data.table(iris)
#' one_hot(ir)
method(one_hot, class_data.table) <- function(
  x,
  factor_levels = NULL,
  verbosity = 1L
) {
  .names <- names(x)
  factor_index <- which(vapply(x, is.factor, logical(1L)))
  if (length(factor_index) == 0L) {
    return(copy(x))
  }
  n_unseen <- integer()
  # One list element per input column, holding the columns that element
  # contributes: an encoded factor contributes one per level, everything else
  # contributes itself. Assembling in input order is what puts the indicator
  # columns where the factor stood, with no reordering pass afterwards.
  columns <- vector("list", length(.names))
  for (i in seq_along(.names)) {
    if (!i %in% factor_index) {
      columns[[i]] <- stats::setNames(list(copy(x[[i]])), .names[i])
      next
    }
    if (verbosity > 0L) {
      msgstart("One hot encoding ", .names[i], "...")
    }
    enc <- one_hot_index(x[[i]], factor_levels, .names[i])
    n_unseen[[.names[i]]] <- enc[["n_unseen"]]
    # A case with no column to take -- an unseen level, or NA -- stays all-zero,
    # so coding those cases 0 leaves them matching no level.
    index <- enc[["index"]]
    index[is.na(index)] <- 0L
    indicators <- lapply(seq_along(enc[["levels"]]), function(k) {
      as.double(index == k)
    })
    names(indicators) <- enc[["names"]]
    columns[[i]] <- indicators
  }
  if (verbosity > 0L) {
    msgdone()
    report_unseen_levels(n_unseen)
  }
  # `setDT()` takes the assembled columns by reference, which is why each
  # pass-through column was copied above: a column shared with `x` would carry a
  # later `:=` on either object through to the other. `as.data.table()` would
  # copy instead, but it would copy the fresh indicator columns too.
  # `dt_set_one_hot()` is the deliberate in-place encoder.
  setDT(unlist(columns, recursive = FALSE))[]
} # /rtemis::one_hot.data.table


#' Convert data.table's factor to one-hot encoding ***in-place***
#'
#' @param x data.table: Input data.table. Will be modified ***in-place***.
#' @param xname Character, optional: Dataset name.
#' @param verbosity Integer: Verbosity level.
#'
#' @return The input, invisibly, after it has been modified ***in-place***.
#'
#' @author EDG
#' @export
#' @examples
#' ir <- data.table::as.data.table(iris)
#' # dt_set_one_hot operates ***in-place***; therefore no assignment is used:
#' dt_set_one_hot(ir)
#' ir
dt_set_one_hot <- function(x, xname = NULL, verbosity = 1L) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  ncases <- NROW(x)
  factor_index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  for (i in factor_index) {
    if (verbosity > 0L) {
      info("One hot encoding ", .names[i], "...")
    }
    .levels <- levels(x[[i]])
    index <- as.numeric(x[[i]])
    oh <- as.data.table(matrix(0, ncases, length(.levels)))
    .colnames <- colnames(oh) <- paste(xname, .levels, sep = "_")
    for (k in seq_along(.levels)) {
      oh[index == k, (.colnames[k]) := 1]
    }
    x[, (paste(.names[i], .levels, sep = "_")) := oh]
  }
  # remove original factor(s)
  x[, paste(.names[factor_index]) := NULL]
  msg("Done", verbosity = verbosity)
  invisible(x)
} # /rtemis::dt_set_one_hot


#' Convert one-hot encoded matrix to factor
#'
#' @details If input has a single column, it will be converted to factor and
#' returned
#'
#' @param x one-hot encoded matrix or data.frame.
#' @param labels Character vector of level names.
#'
#' @return A factor.
#'
#' @author EDG
#' @export
#' @examples
#' x <- data.frame(matrix(FALSE, 10, 3))
#' colnames(x) <- c("Dx1", "Dx2", "Dx3")
#' x$Dx1[1:3] <- x$Dx2[4:6] <- x$Dx3[7:10] <- TRUE
#' one_hot2factor(x)
one_hot2factor <- function(x, labels = colnames(x)) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  if (any(na.exclude(rowSums(x)) > 1)) {
    rtemis.core::abort(
      "Input must be one-hot encoded.",
      class = "rtemis_data_error"
    )
  }
  out <- factor(rep(NA, NROW(x)), levels = labels)
  for (i in seq_along(labels)) {
    out[x[, i] == 1] <- labels[i]
  }
  out
} # /rtemis::one_hot2factor


#' Binary matrix times character vector
#'
#' Collapse a binary indicator matrix into a character vector of labels. For each
#' row, the labels of all columns equal to 1 are pasted together, comma-separated.
#' This is the inverse of multi-hot encoding: unlike [one_hot2factor], which assumes
#' a single 1 per row, `%BC%` supports rows with multiple 1s (multi-label data, e.g.
#' multiple-choice survey responses, tags, or set membership). Rows of all zeros
#' return `NA`.
#'
#' @param x A binary matrix or data.frame
#' @param labels Character vector length equal to `ncol(x)`
#'
#' @return a character vector
#'
#' @author EDG
#' @export
#' @examples
#' # Multi-hot matrix: each row can belong to multiple categories
#' x <- rbind(
#'   c(1, 0, 1),
#'   c(0, 1, 0),
#'   c(1, 1, 1),
#'   c(0, 0, 0)
#' )
#' labels <- c("apple", "banana", "cherry")
#' x %BC% labels
#' # -> "apple,cherry", "banana", "apple,banana,cherry", NA
`%BC%` <- function(x, labels) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  dt <- as.data.table(x)
  fn <- \(r) paste(unique(labels[which(r == 1)]), collapse = ",")
  out <- dt[, list(fn(.SD)), by = seq_len(NROW(dt))][[2]]
  out[out == ""] <- NA
  out
} # /rtemis::`%BC%`


#' Binary matrix to list vector
#'
#' @author EDG
#' @keywords internal
#' @noRd
binmat2lvec <- function(x, labels = colnames(x), return.list = FALSE) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  dt <- as.data.table(x)
  if (return.list) {
    fn <- \(r) list(labels[which(r == 1)])
    out <- dt[, list(fn(.SD)), by = seq_len(NROW(dt))][[2]]
    out[sapply(out, length) == 0] <- NA
  } else {
    fn <- \(r) paste(unique(labels[which(r == 1)]), collapse = ",")
    out <- dt[, list(fn(.SD)), by = seq_len(NROW(dt))]
    out[out == ""] <- NA
  }
  out
} # /rtemis::binmat2lvec


# %% feature_matrix ----
#' Convert tabular data to feature matrix
#'
#' Convert a tabular dataset to a matrix, one-hot encoding factors, if present.
#'
#' @details
#' This is a convenience function that uses  [features()], [preprocess()], `as.matrix()`.
#'
#' @param x tabular data: Input data to convert to a feature matrix.
#'
#' @return Matrix with features. Factors are one-hot encoded, if present.
#'
#' @author EDG
#' @export
#' @examples
#' # reorder columns so that we have a categorical feature
#' x <- set_outcome(iris, "Sepal.Length")
#' feature_matrix(x) |> head()
feature_matrix <- function(x) {
  x |>
    features() |>
    preprocess(setup_Preprocessor(one_hot = TRUE)) |>
    preprocessed() |>
    as.matrix()
} # /rtemis::feature_matrix
