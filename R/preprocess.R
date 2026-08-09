# preprocess.R
# ::rtemis::
# 2017- EDG rtemis.org

# %% PREPROCESSOR_CASE_OPS ----
# Preprocessing steps that drop *cases* rather than transform values. They have
# no place in a preprocessor fitted by `train()`: that one is replayed on new
# data at predict time, and there is nothing a prediction call may drop --
# asked for n rows it must return n predictions.
PREPROCESSOR_CASE_OPS <- c(
  "complete_cases",
  "remove_duplicates",
  "remove_cases_thres"
)


# %% check_preprocessor_replayable ----
#' Reject a training preprocessor that would drop cases
#'
#' A preprocessor fitted by `train()` is re-applied to every later dataset:
#' validation, test, and whatever `predict()` is handed. A step that removes
#' rows cannot be replayed -- it returns fewer predictions than rows, with no
#' way for the caller to tell which are missing -- so it is rejected here
#' rather than producing a model whose `predict()` silently loses cases.
#'
#' Case removal is data preparation, not part of the model: do it before
#' `train()`, where the outcome is still attached and the dropped rows are
#' visible.
#'
#' @param config `PreprocessorConfig` object.
#'
#' @return Invisible NULL. Called for the check.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_preprocessor_replayable <- function(config) {
  set <- Filter(
    function(nm) {
      value <- prop(config, nm)
      !is.null(value) && !identical(value, FALSE)
    },
    PREPROCESSOR_CASE_OPS
  )
  if (length(set) > 0L) {
    rtemis.core::abort(
      "`preprocessor_config` removes cases, which a fitted preprocessor cannot replay: ",
      paste(set, collapse = ", "),
      ".\n",
      "A preprocessor learned by train() is re-applied to new data at predict() time, ",
      "where dropping rows would return fewer predictions than rows.\n",
      "Remove cases before calling train(), with preprocess() on the full dataset.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  invisible(NULL)
} # /rtemis::check_preprocessor_replayable


# %% preprocess(x, PreprocessorConfig, ...) ----
method(
  preprocess,
  list(class_tabular, PreprocessorConfig)
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

  # Data
  isdatatable <- data.table::is.data.table(x)
  x <- as.data.frame(x)

  # Complete cases ----
  if (config@complete_cases) {
    msg("Filtering complete cases...", verbosity = verbosity)
    x <- x[complete.cases(x), ]
  }

  # Set aside excluded ----
  if (!is.null(config@exclude) && length(config@exclude) > 0) {
    excluded <- x[, config@exclude, drop = FALSE]
    excluded_names <- colnames(x)[config@exclude]
    x <- x[, -config@exclude, drop = FALSE]
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
    x <- x[, !names(x) %in% config@remove_features, drop = FALSE]
  }

  # Remove constants ----
  # Must be ahead of numeric quantile at least
  if (config@remove_constants) {
    constant <- which(sapply(
      x,
      is_constant,
      skip_missing = config@remove_constants_skip_missing
    ))
    if (length(constant) > 0) {
      if (verbosity > 0L) {
        msg0(
          "Removing ",
          singorplu(length(constant), "constant feature"),
          "..."
        )
      }
      x <- x[, -constant]
    }
  }

  # Remove duplicates ----
  if (config@remove_duplicates) {
    # Ndups <- sum(duplicated(x))
    duplicate_index <- which(duplicated(x))
    Ndups <- length(duplicate_index)
    if (Ndups > 0) {
      if (verbosity > 0L) {
        msg0("Removing ", singorplu(Ndups, "duplicate case"), "...")
      }
      x <- unique(x)
    }
  } else {
    duplicate_index <- NULL
  }

  # Remove Cases by missing feature threshold ----
  if (!is.null(config@remove_cases_thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      # na_fraction_bycase <- apply(x, 1, function(i) sum(is.na(i))/length(i))
      na_fraction_bycase <- data.table::transpose(xt)[, lapply(
        .SD,
        function(i) {
          sum(is.na(i)) / length(i)
        }
      )]
      index_remove_cases_thres <- which(
        na_fraction_bycase >= config@remove_cases_thres
      )
      if (length(index_remove_cases_thres) > 0) {
        msg(
          "Removing",
          length(index_remove_cases_thres),
          "cases with >=",
          config@remove_cases_thres,
          "missing data...",
          verbosity = verbosity
        )
        xt <- xt[-index_remove_cases_thres, ]
      }
      x <- as.data.frame(xt)
    }
  }

  # Remove Features by missing feature threshold ----
  if (!is.null(config@remove_features_thres)) {
    if (anyNA(x)) {
      xt <- data.table::as.data.table(x)
      na.fraction.byfeat <- xt[, lapply(.SD, function(i) {
        sum(is.na(i)) / length(i)
      })]
      removeFeat_thres_index <- which(
        na.fraction.byfeat >= config@remove_features_thres
      )
      if (length(removeFeat_thres_index) > 0) {
        msg(
          "Removing",
          length(removeFeat_thres_index),
          "features with >=",
          config@remove_features_thres,
          "missing data...",
          verbosity = verbosity
        )
        x <- x[, -removeFeat_thres_index]
      }
    }
  }

  # Integer to factor ----
  index_integer <- NULL
  if (config@integer2factor) {
    index_integer <- c(
      which(sapply(x, is.integer)),
      which(sapply(x, bit64::is.integer64))
    )
    if (verbosity > 0L) {
      if (length(index_integer) > 0) {
        msg(
          "Converting",
          singorplu(length(index_integer), "integer"),
          "to factor..."
        )
      } else {
        msg("No integers to convert to factor...")
      }
    }
    for (i in index_integer) {
      x[, i] <- as.factor(x[, i])
    }
  }

  # Logical to factor ----
  if (config@logical2factor) {
    index_logical <- which(sapply(x, is.logical))
    if (verbosity > 0L) {
      if (length(index_logical) > 0) {
        msg0(
          "Converting ",
          singorplu(length(index_logical), "logical feature"),
          " to ",
          ngettext(length(index_logical), "factor", "factors"),
          "..."
        )
      } else {
        msg("No logicals to convert to factor...")
      }
    }
    for (i in index_logical) {
      x[, i] <- as.factor(x[, i])
    }
  }

  # Numeric to factor ----
  if (config@numeric2factor) {
    index_numeric <- which(sapply(x, is.numeric))
    msg("Converting numeric to factors...", verbosity = verbosity)
    if (is.null(config@numeric2factor_levels)) {
      for (i in index_numeric) {
        x[, i] <- as.factor(x[, i])
      }
    } else {
      for (i in index_numeric) {
        x[, i] <- factor(x[, i], levels = config@numeric2factor_levels)
      }
    }
  }

  # Character to factor ----
  if (config@character2factor) {
    index_char <- which(sapply(x, is.character))
    if (verbosity > 0L) {
      if (length(index_char) > 0) {
        msg0(
          "Converting ",
          singorplu(length(index_char), "character feature"),
          " to ",
          ngettext(length(index_char), "a factor", "factors"),
          "..."
        )
      } else {
        msg("No character features to convert to factors found.")
      }
    }
    for (i in index_char) {
      x[, i] <- as.factor(x[, i])
    }
  }

  # unique_len2factor ----
  if (config@unique_len2factor > 1) {
    index_len <- which(sapply(
      x,
      \(i) length(unique(i)) <= config@unique_len2factor
    ))
    # Exclude factors
    index_factor <- which(sapply(x, is.factor))
    index_len <- setdiff(index_len, index_factor)
    if (verbosity > 0L) {
      if (length(index_len) > 0) {
        msg(
          "Converting",
          singorplu(length(index_len), "feature"),
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
    for (i in index_len) {
      x[, i] <- factor(x[, i])
    }
  }

  # Integer to numeric ----
  if (config@integer2numeric) {
    if (is.null(index_integer)) {
      index_integer <- c(
        which(sapply(x, is.integer)),
        which(sapply(x, bit64::is.integer64))
      )
    }
    if (verbosity > 0L) {
      if (length(index_integer) > 0) {
        msg(
          "Converting",
          singorplu(length(index_integer), "integer"),
          "to numeric..."
        )
      } else {
        msg("No integers to convert to numeric...")
      }
    }
    for (i in index_integer) {
      x[, i] <- as.numeric(x[, i])
    }
  }

  # Logical to numeric ----
  if (config@logical2numeric) {
    index_logical <- which(sapply(x, is.logical))
    msg("Converting logicals to numeric...", verbosity = verbosity)
    for (i in index_logical) {
      x[, i] <- as.numeric(x[, i])
    }
  }

  # Numeric cut ----
  if (config@numeric_cut_n > 0) {
    index_numeric <- which(sapply(x, is.numeric))
    msg(
      "Cutting numeric features in",
      config@numeric_cut_n,
      "bins...",
      verbosity = verbosity
    )
    if (length(index_numeric) > 0) {
      for (i in index_numeric) {
        x[, i] <- factor(
          cut(
            x[, i],
            breaks = config@numeric_cut_n,
            labels = config@numeric_cut_labels
          )
        )
      }
    }
  }

  # Numeric quantile ----
  if (config@numeric_quant_n > 0) {
    index_numeric2q <- if (config@numeric_quant_nAonly) {
      index_numeric2q <- which(sapply(x, is.numeric) & sapply(x, anyNA))
    } else {
      which(sapply(x, is.numeric))
    }
    if (length(index_numeric2q) > 0) {
      msg(
        "Cutting numeric features in",
        config@numeric_quant_n,
        "quantiles...",
        verbosity = verbosity
      )
      for (i in index_numeric2q) {
        rng <- abs(diff(range(x[, i], na.rm = TRUE)))
        quantiles <- quantile(
          x[, i],
          probs = seq(0, 1, length.out = config@numeric_quant_n),
          na.rm = TRUE
        )
        quantiles[1] <- quantiles[1] - .02 * rng
        quantiles[config@numeric_quant_n] <- quantiles[
          config@numeric_quant_n
        ] +
          .02 * rng
        quantiles <- unique(quantiles)
        x[, i] <- factor(
          cut(
            x[, i],
            breaks = quantiles
          )
        )
      }
    }
  }

  # factor NA to level ----
  if (config@factorNA2missing) {
    index_factor <- which(sapply(x, is.factor))
    if (verbosity > 0L) {
      if (length(index_factor) > 0) {
        msg0(
          "Converting ",
          length(index_factor),
          ngettext(length(index_factor), " factor's", " factors'"),
          " NA values to level '",
          config@factorNA2missing_level,
          "'..."
        )
      } else {
        msg("No factors found.")
      }
    }
    for (i in index_factor) {
      x[, i] <- factor_NA2missing(x[, i], config@factorNA2missing_level)
    }
  }

  # Factor to integer ----
  # e.g. for algorithms that do not support factors directly, but can handle integers
  # as categorical (e.g. LightGBM)
  if (config@factor2integer) {
    index_factor <- which(sapply(x, is.factor))
    if (verbosity > 0L) {
      if (length(index_factor) > 0) {
        msg(
          "Converting",
          singorplu(length(index_factor), "factor"),
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
    for (i in index_factor) {
      feature <- names(x)[i]
      feature_levels <- config@factor2integer_levels[[feature]]
      if (is.null(feature_levels)) {
        feature_levels <- levels(x[[i]])
      }
      learned_levels[[feature]] <- feature_levels
      x[, i] <- factor2integer_code(
        x[[i]],
        levels = feature_levels,
        startat0 = config@factor2integer_startat0,
        xname = feature,
        verbosity = verbosity
      )
    }
    if (length(learned_levels) > 0L) {
      values$factor2integer_levels <- learned_levels
    }
  }

  # Missingness ----
  if (config@missingness) {
    cols_with_na <- which(apply(x, 2, anyNA))
    .colnames <- colnames(x)
    for (i in cols_with_na) {
      x[, paste0(.colnames[i], "_missing")] <- factor(as.numeric(is.na(x[, i])))
      if (verbosity > 0L) {
        msg0("Created missingness indicator for ", .colnames[i], "...")
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
      x <- missRanger::missRanger(
        x,
        pmm.k = config@impute_missRanger_params[["pmm.k"]],
        verbose = verbosity
      )
    } else if (config@impute_type == "micePMM") {
      check_dependencies("mice")
      msg(
        "Imputing missing values by predictive mean matching using mice...",
        verbosity = verbosity
      )
      x <- mice::complete(mice::mice(x, m = 1, method = "pmm"))
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

      index_discrete <- which(sapply(x, function(i) is_discrete(i) && anyNA(i)))
      if (length(index_discrete) > 0) {
        for (i in index_discrete) {
          index <- which(is.na(x[, i]))
          imputed <- do_call(
            config@impute_discrete,
            list(x[[i]], na.rm = TRUE)
          )
          x[index, i] <- imputed
        }
      }

      index_numeric <- which(sapply(x, function(i) is.numeric(i) && anyNA(i)))
      if (length(index_numeric) > 0) {
        for (i in index_numeric) {
          index <- which(is.na(x[, i]))
          imputed <- do_call(
            config@impute_continuous,
            list(x[[i]], na.rm = TRUE)
          )
          x[index, i] <- imputed
        }
      }
    }
  }

  # Scale +/- center ----
  if (config@scale || config@center) {
    # Get index of numeric features
    numeric_index <- which(sapply(x, is.numeric))
    # `factor2integer` runs above and emits codes that are `is.numeric()`, but a
    # category code is not a numeric feature: standardizing it yields a fraction
    # of an index, which no consumer can read back -- an embedding indexes with
    # it, LightGBM reads it as a category. The coded columns are exactly those
    # just recorded, so they are dropped here by name.
    coded_features <- names(values[["factor2integer_levels"]])
    if (length(coded_features) > 0L) {
      numeric_index <- numeric_index[
        !names(numeric_index) %in% coded_features
      ]
    }
    sc <- if (config@scale) "Scaling" else NULL
    ce <- if (config@center) "centering" else NULL
    if (length(numeric_index) > 0) {
      msg(
        paste(c(sc, ce), collapse = " and "),
        length(numeric_index),
        "numeric features...",
        verbosity = verbosity
      )
      # Info: scale outputs a matrix.
      scale_ <- if (!is.null(config@scale_coefficients)) {
        # Check names match
        stopifnot(identical(
          names(config@scale_coefficients),
          names(x)[numeric_index]
        ))
        config@scale_coefficients
      } else {
        config@scale
      }
      center_ <- if (!is.null(config@scale_centers)) {
        # Check names match
        stopifnot(identical(
          names(config@scale_centers),
          names(x)[numeric_index]
        ))
        config@scale_centers
      } else {
        config@center
      }
      x_num_scaled <- scale(
        x[, numeric_index, drop = FALSE],
        scale = scale_,
        center = center_
      )

      # Collect scale and center values
      values$scale_centers <- attr(x_num_scaled, "scaled:center")
      values$scale_coefficients <- attr(x_num_scaled, "scaled:scale")

      x_num_scaled <- as.data.frame(x_num_scaled)

      # Insert into original dataset
      x[, numeric_index] <- x_num_scaled
      # j <- 0
      # for (i in numeric_index) {
      #   j <- j + 1
      #   x[, i] <- x_num_scaled[, j]
      # }
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
    factor_index <- which(sapply(x, is.factor))
    learned_levels <- list()
    for (i in factor_index) {
      feature <- names(x)[i]
      feature_levels <- config@one_hot_levels[[feature]]
      if (is.null(feature_levels)) {
        feature_levels <- levels(x[[i]])
      }
      learned_levels[[feature]] <- feature_levels
    }
    if (length(learned_levels) > 0L) {
      values$one_hot_levels <- learned_levels
    }
    x <- one_hot(
      x,
      verbosity = verbosity,
      factor_levels = learned_levels
    )
  }

  # Add date features ----
  if (config@add_date_features) {
    msg("Extracting date features...", verbosity = verbosity)
    # Find date columns
    date_cols <- which(sapply(x, function(col) inherits(col, "Date")))
    # For each date column, extract features
    for (i in date_cols) {
      .date_features <- dates2features(
        x[[i]],
        features = config@date_features
      )
      names(.date_features) <- paste0(names(x)[i], "_", names(.date_features))
      x <- cbind(x, .date_features)
    }
  }

  # Add holidays ----
  if (config@add_holidays) {
    msg("Extracting holidays...", verbosity = verbosity)
    # Find date columns
    date_cols <- which(sapply(x, \(col) inherits(col, "Date")))
    # For each date column, extract holidays
    for (i in date_cols) {
      .holidays <- get_holidays(x[, i])
      x[[paste0(names(x)[i], "_holidays")]] <- .holidays
    }
  }

  # Add back excluded ----
  if (!is.null(config@exclude) && length(config@exclude) > 0) {
    # remove any duplicates
    if (!is.null(duplicate_index)) {
      excluded <- excluded[-duplicate_index, , drop = FALSE]
    }

    # remove by case thres
    if (
      !is.null(config@remove_cases_thres) &&
        length(index_remove_cases_thres) > 0
    ) {
      n_feat_inc <- NCOL(x)
      x <- cbind(x, excluded[-index_remove_cases_thres, ])
      colnames(x)[-c(seq(n_feat_inc))] <- excluded_names
    } else {
      x <- cbind(x, excluded)
    }
  } # /add back excluded

  if (isdatatable) {
    data.table::setDT(x)
  }
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
#' @param new_data data.frame or data.table: New data to preprocess.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Preprocessed data of the same class as `new_data` (data.frame or data.table).
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
#' @keywords internal
#' @noRd
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
#' @param levels Character vector: Levels to index into, in order.
#'
#' @return Integer vector of positions in `levels`, `NA` where the case is `NA`
#' or its level is absent from `levels`. Callers decide what an absent level
#' means.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pinned_level_index <- function(x, levels) {
  match(levels(x), levels)[as.integer(x)]
} # /rtemis::pinned_level_index


# %% factor2integer_code ----
#' Integer-code a factor against a fixed set of levels
#'
#' A value whose level is absent from `levels` is out of vocabulary and takes
#' the single index above the known levels: `length(levels)` when `startat0` is
#' TRUE, `length(levels) + 1L` otherwise. A model consuming the codes therefore
#' sizes the feature at `length(levels) + 1L` categories. `NA` stays `NA`.
#'
#' Both `startat0` branches return integer. A category code indexes something --
#' an embedding table, a LightGBM category -- and a double cannot.
#'
#' @param x Factor: Feature to code.
#' @param levels Character vector: Levels to code against, in order.
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
  levels,
  startat0,
  xname,
  verbosity = 1L
) {
  oov <- length(levels) + 1L
  code <- pinned_level_index(x, levels)
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
#' `one_hot.data.table` operates on a copy of its input.
#' `one_hot_` performs one-hot encoding ***in-place***.
#'
#' `one_hot.data.frame` encodes each feature against `factor_levels` when it
#' carries an entry for it, and against the feature's own levels otherwise. The
#' pinned set fixes both the columns and which column each value takes, so new
#' data whose factor has fewer or differently ordered levels is encoded exactly
#' as the training data was. A case whose level is absent from the pinned set --
#' or which is `NA` -- has no column to take and stays all-zero, which is the
#' width-preserving degradation; `factor2integer` instead reserves an index,
#' because an embedding must index something.
#'
#' @param x Vector or data.frame
#' @param xname Character: Variable name
#' @param factor_levels Optional Named list of the form "feature_name" =
#' "levels": Levels to encode each feature against. Lookup is by name and
#' tolerant of entries with no matching column.
#' @param verbosity Integer: Verbosity level.
#'
#' @return For vector input, a one-hot-encoded matrix, for data.frame frame
#' input, an expanded data.frame where all factors are one-hot encoded
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
    # Lookup is by feature name and tolerant of map entries with no matching
    # column: `train()` learns a map on data that includes the outcome and
    # applies it to features alone.
    .levels <- factor_levels[[.names[i]]]
    if (is.null(.levels)) {
      .levels <- levels(x[[i]])
    }
    index <- pinned_level_index(x[[i]], .levels)
    oh <- matrix(0, ncases, length(.levels))
    colnames(oh) <- paste0(.names[i], "_", .levels)
    # A case with no column to take -- an unseen level, or NA -- stays all-zero.
    present <- which(!is.na(index))
    oh[cbind(present, index[present])] <- 1
    n_unseen[[.names[i]]] <- sum(is.na(index) & !is.na(x[[i]]))
    # Replace list element that was a factor with one-hot encoded matrix
    one.hot[[i]] <- oh
  }
  if (verbosity > 0L) {
    msgdone()
    for (feature in names(n_unseen)[n_unseen > 0L]) {
      msg0(
        "Feature '",
        feature,
        "': encoding ",
        singorplu(n_unseen[[feature]], "case"),
        " with unseen levels as all-zero..."
      )
    }
  }
  # do.call below creates a matrix, maintaining column names in one.hot matrix.
  # as.data.frame on one.hot would have added {name_of_oh_element}.{column_names}
  as.data.frame(do.call(cbind, one.hot))
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
#' ir_oh <- one_hot(ir)
#' ir_oh
method(one_hot, class_data.table) <- function(x, verbosity = 1L) {
  x <- copy(x)
  ncases <- NROW(x)
  factor_index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  for (i in factor_index) {
    if (verbosity > 0L) {
      info("One hot encoding ", .names[i], "...")
    }
    .levels <- levels(x[[i]])
    index <- as.integer(x[[i]])
    oh <- as.data.table(matrix(0, ncases, length(.levels)))
    .colnames <- colnames(oh) <- .levels
    for (k in seq_along(.levels)) {
      oh[index == k, (.colnames[k]) := 1]
    }
    x[, (paste(.names[i], .levels, sep = "_")) := oh]
  }
  # remove original factor(s)
  x[, paste(.names[factor_index]) := NULL]
  msg("Done", verbosity = verbosity)
  invisible(x)
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
