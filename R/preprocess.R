# preprocess.R
# ::rtemis::
# 2017- EDG rtemis.org

#' @title
#' Preprocess Data
#'
#' @description
#' Preprocess data for analysis and visualization.
#'
#' @usage
#' ## S7 generic
#' preprocess(x, config, ...)
#'
#' @param x data.frame or similar: Data to be preprocessed.
#' @param config `PreprocessorConfig` or `Preprocessor`: PreprocessorConfig when
#' preprocessing training set data. Setup using [setup_Preprocessor].
#' Preprocessor when preprocessing validation and test set data.
#' @param ... Used to pass `dat_validation` and `dat_test` to the method for Preprocessor.
#'
#' @details
#' Methods are provided for preprocessing training set data, which accepts a PreprocessorConfig
#' object, and for preprocessing validation and test set data, which accept a Preprocessor
#' object.
#'
#' Order of operations:
#'
#'   * keep complete cases only
#'   * remove constants
#'   * remove duplicates
#'   * remove cases by missingness threshold
#'   * remove features by missingness threshold
#'   * integer to factor
#'   * integer to numeric
#'   * logical to factor
#'   * logical to numeric
#'   * numeric to factor
#'   * cut numeric to n bins
#'   * cut numeric to n quantiles
#'   * numeric with less than N unique values to factor
#'   * character to factor
#'   * factor NA to named level
#'   * add missingness column
#'   * impute
#'   * scale and/or center
#'   * one-hot encoding
#'
#' @return `Preprocessor` object.
#'
#' @author EDG
#' @rdname preprocess
#' @export
#'
#' @examples
#' # Setup a `Preprocessor`: this outputs a `PreprocessorConfig` object.
#' prp <- setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE)
#'
#' # Includes a long list of parameters
#' prp
#'
#' # Resample iris to get train and test data
#' res <- resample(iris, setup_Resampler(seed = 2026))
#' iris_train <- iris[res[[1]], ]
#' iris_test <- iris[-res[[1]], ]
#'
#' # Preprocess training data
#' iris_pre <- preprocess(iris_train, prp)
#'
#' # Access preprocessd training data with `preprocessed()`
#' preprocessed(iris_pre)
#'
#' # Apply the same preprocessing to test data
#' # In this case, the scale and center values from training data will be used.
#' # Note how `preprocess()` accepts either a `PreprocessorConfig` or `Preprocessor` object for
#' # this reason.
#' iris_test_pre <- preprocess(iris_test, iris_pre)
#'
#' # Access preprocessed test data
#' preprocessed(iris_test_pre)
preprocess <- new_generic("preprocess", c("x", "config"))

# preprocess(x, PreprocessorConfig, ...) ----
method(preprocess, list(class_data.frame, PreprocessorConfig)) <- function(
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
    remove_features = NULL
  )

  # Data
  isdatatable <- data.table::is.data.table(x)
  x <- as.data.frame(x)

  # Complete cases ----
  if (config@complete_cases) {
    if (verbosity > 0L) {
      msg("Filtering complete cases...")
    }
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
    if (verbosity > 0L) {
      msg("Removing", length(config@remove_features), "features...")
    }
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
        if (verbosity > 0L) {
          msg(
            "Removing",
            length(index_remove_cases_thres),
            "cases with >=",
            config@remove_cases_thres,
            "missing data..."
          )
        }
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
        if (verbosity > 0L) {
          msg(
            "Removing",
            length(removeFeat_thres_index),
            "features with >=",
            config@remove_features_thres,
            "missing data..."
          )
        }
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
    if (verbosity > 0L) {
      msg("Converting numeric to factors...")
    }
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
    if (verbosity > 0L) {
      msg("Converting logicals to numeric...")
    }
    for (i in index_logical) {
      x[, i] <- as.numeric(x[, i])
    }
  }

  # Numeric cut ----
  if (config@numeric_cut_n > 0) {
    index_numeric <- which(sapply(x, is.numeric))
    if (length(index_numeric) > 0) {
      if (verbosity > 0L) {
        msg("Cutting numeric features in", config@numeric_cut_n, "bins...")
      }
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
      if (verbosity > 0L) {
        msg(
          "Cutting numeric features in",
          config@numeric_quant_n,
          "quantiles..."
        )
      }
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
    if (config@factor2integer_startat0) {
      for (i in index_factor) {
        x[, i] <- as.integer(x[, i]) - 1
      }
    } else {
      for (i in index_factor) {
        x[, i] <- as.integer(x[, i])
      }
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
      if (verbosity > 0L) {
        msg(
          "Imputing missing values by predictive mean matching using mice..."
        )
      }
      x <- mice::complete(mice::mice(x, m = 1, method = "pmm"))
    } else {
      # '- mean/mode ----
      if (verbosity > 0L) {
        msg(
          "Imputing missing values using",
          config@impute_discrete,
          "(discrete) and",
          config@impute_continuous,
          "(continuous)..."
        )
      }

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
    sc <- if (config@scale) "Scaling" else NULL
    ce <- if (config@center) "centering" else NULL
    if (length(numeric_index) > 0) {
      if (verbosity > 0L) {
        msg(
          paste(c(sc, ce), collapse = " and "),
          length(numeric_index),
          "numeric features..."
        )
      }
      # Info: scale outputs a matrix.
      scale_ <- if (!is.null(config@scale_coefficients)) {
        # Check names match
        stopifnot(identical(
          names(config@scale_coefficients),
          names(x[, numeric_index])
        ))
        config@scale_coefficients
      } else {
        config@scale
      }
      center_ <- if (!is.null(config@scale_centers)) {
        # Check names match
        stopifnot(identical(
          names(config@scale_centers),
          names(x[, numeric_index])
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
        "was requested \n                                but no numeric features were found: Please check data."
      )
    }
  }

  # One Hot Encoding ----
  if (config@one_hot) {
    x <- one_hot(
      x,
      verbosity = verbosity,
      factor_levels = config@one_hot_levels
    )
  }

  # Add date features ----
  if (config@add_date_features) {
    if (verbosity > 0L) {
      msg("Extracting date features...")
    }
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
    if (verbosity > 0L) {
      msg("Extracting holidays...")
    }
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
  if (verbosity > 0L) {
    msg("Preprocessing done.")
  }

  preprocessed <- list(training = x)

  if (!is.null(dat_validation)) {
    if (verbosity > 0L) {
      msg("Applying preprocessing to validation data...")
    }
    prp_validation <- preprocess(
      x = dat_validation,
      config = Preprocessor(
        config = config,
        preprocessed = list(),
        scale_centers = values[["scale_centers"]],
        scale_coefficients = values[["scale_coefficients"]],
        one_hot_levels = values[["one_hot_levels"]],
        remove_features = values[["remove_features"]]
      ),
      verbosity = verbosity
    )
    preprocessed$validation <- prp_validation@preprocessed
  }
  if (!is.null(dat_test)) {
    if (verbosity > 0L) {
      msg("Applying preprocessing to test data...")
    }
    prp_test <- preprocess(
      x = dat_test,
      config = Preprocessor(
        config = config,
        preprocessed = list(),
        scale_centers = values[["scale_centers"]],
        scale_coefficients = values[["scale_coefficients"]],
        one_hot_levels = values[["one_hot_levels"]],
        remove_features = values[["remove_features"]]
      ),
      verbosity = verbosity
    )
    preprocessed$test <- prp_test@preprocessed
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
    remove_features = values[["remove_features"]]
  )
} # /rtemis::preprocess(PreprocessorConfig, ...)

# preprocess(x, Preprocessor, ...) ----
method(preprocess, list(class_data.frame, Preprocessor)) <- function(
  x,
  config,
  verbosity = 1L
) {
  # -> Preprocessor
  params <- config@config
  # Overwrite scale_centers, scale_coefficients, one_hot_levels, and remove_features
  params@scale_centers <- config@values[["scale_centers"]]
  params@scale_coefficients <- config@values[["scale_coefficients"]]
  params@one_hot_levels <- config@values[["one_hot_levels"]]
  params@remove_features <- config@values[["remove_features"]]

  preprocess(x, params, verbosity = verbosity)
} # /rtemis::preprocess(Preprocessor, ...)


# one_hot.R
# ::rtemis::
# 2019 EDG rtemis.org

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
#' @param x Vector or data.frame
#' @param xname Character: Variable name
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
#' \dontrun{
#' iris_oh <- one_hot(iris)
#' # factor with only one unique value but 2 levels:
#' vf <- factor(rep("alpha", 20), levels = c("alpha", "beta"))
#' vf_one_hot <- one_hot(vf)
#' }
one_hot <- new_generic("one_hot", "x")
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

# one_hot.data.frame ----
#' @rdname one_hot
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' one_hot(iris) |> head()
#' }
# one_hot.data.frame
method(one_hot, class_data.frame) <- function(
  x,
  factor_levels = NULL,
  verbosity = 1L
) {
  ncases <- NROW(x)
  factor_index <- which(sapply(x, is.factor))
  # If factor_levels list is provided, check column names match
  if (!is.null(factor_levels)) {
    stopifnot(identical(names(factor_levels), colnames(x[, factor_index])))
  }
  one.hot <- as.list(x)
  if (verbosity > 0L) {
    .names <- colnames(x)
  }
  for (i in factor_index) {
    if (verbosity > 0L) {
      msgstart("One hot encoding ", .names[i], "...")
    }
    .levels <- if (!is.null(factor_levels)) {
      factor_levels[[i]]
    } else {
      levels(x[[i]])
    }
    index <- as.integer(x[, i])
    oh <- matrix(0, ncases, length(.levels))
    colnames(oh) <- paste0(names(x)[i], "_", .levels)
    for (j in seq(ncases)) {
      oh[j, index[j]] <- 1
    }
    # Replace list element that was a factor with one-hot encoded matrix
    one.hot[[i]] <- oh
  }
  if (verbosity > 0L) {
    msgdone()
  }
  # do.call below creates a matrix, maintaining column names in one.hot matrix.
  # as.data.frame on one.hot would have added {name_of_oh_element}.{column_names}
  as.data.frame(do.call(cbind, one.hot))
} # /rtemis::one_hot.data.frame


# one_hot.data.table ----
#' @rdname one_hot
#'
#' @author EDG
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' ir <- data.table::as.data.table(iris)
#' ir_oh <- one_hot(ir)
#' ir_oh
#' }
method(one_hot, class_data.table) <- function(x, verbosity = 1L) {
  x <- copy(x)
  ncases <- NROW(x)
  factor_index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  for (i in factor_index) {
    if (verbosity > 0L) {
      msg_info("One hot encoding ", .names[i], "...")
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
  if (verbosity > 0L) {
    msg("Done")
  }
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
#' \dontrun{
#' ir <- data.table::as.data.table(iris)
#' # dt_set_one_hot operates ***in-place***; therefore no assignment is used:
#' dt_set_one_hot(ir)
#' ir
#' }
dt_set_one_hot <- function(x, xname = NULL, verbosity = 1L) {
  if (is.null(xname)) {
    xname <- deparse(substitute(x))
  }
  ncases <- NROW(x)
  factor_index <- which(sapply(x, is.factor))
  .names <- colnames(x)
  for (i in factor_index) {
    if (verbosity > 0L) {
      msg_info("One hot encoding ", .names[i], "...")
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
  if (verbosity > 0L) {
    msg("Done")
  }
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
#' \dontrun{
#' x <- data.frame(matrix(F, 10, 3))
#' colnames(x) <- c("Dx1", "Dx2", "Dx3")
#' x$Dx1[1:3] <- x$Dx2[4:6] <- x$Dx3[7:10] <- T
#' one_hot2factor(x)
#' }
#'
one_hot2factor <- function(x, labels = colnames(x)) {
  if (NCOL(x) == 1) {
    return(factor(x))
  }
  if (any(na.exclude(rowSums(x)) > 1)) {
    cli::cli_abort("Input must be one-hot encoded.")
  }
  out <- factor(rep(NA, NROW(x)), levels = labels)
  for (i in seq_along(labels)) {
    out[x[, i] == 1] <- labels[i]
  }
  out
} # /rtemis::one_hot2factor


#' Binary matrix times character vector
#'
#' @param x A binary matrix or data.frame
#' @param labels Character vector length equal to `ncol(x)`
#'
#' @return a character vector
#'
#' @author EDG
#' @export
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
