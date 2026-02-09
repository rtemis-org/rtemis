# check_supervised.R
# ::rtemis::
# EDG rtemis.org

# Notes:
# Some algorithms do not work with variable names containing dots (SparkML)

method(check_factor_levels, class_data.frame) <- function(x, y, z) {
  if (!is.null(y) || !is.null(z)) {
    index_factor <- which(sapply(x, is.factor))
    x_levels <- lapply(x[, index_factor, drop = FALSE], levels)
    if (!is.null(y)) {
      y_levels <- lapply(y[, index_factor, drop = FALSE], levels)
      if (
        !all(sapply(seq_along(x_levels), function(i) {
          identical(x_levels[[i]], y_levels[[i]])
        }))
      ) {
        cli::cli_abort(
          "Training and validation set factor levels do not match."
        )
      }
    }
    if (!is.null(z)) {
      z_levels <- lapply(z[, index_factor, drop = FALSE], levels)
      if (
        !all(sapply(seq_along(x_levels), function(i) {
          identical(x_levels[[i]], z_levels[[i]])
        }))
      ) {
        cli::cli_abort("Training and test set factor levels do not match.")
      }
    }
  }
  invisible()
} # /method(check_factor_levels, class_data.frame)

method(check_factor_levels, class_data.table) <- function(x, y, z) {
  if (!is.null(y) || !is.null(z)) {
    index_factor <- which(sapply(x, is.factor))
    x_levels <- lapply(x[, .SD, .SDcols = index_factor], levels)
    if (!is.null(y)) {
      y_levels <- lapply(y[, .SD, .SDcols = index_factor], levels)
      if (
        !all(sapply(seq_along(x_levels), function(i) {
          identical(x_levels[[i]], y_levels[[i]])
        }))
      ) {
        cli::cli_abort(
          "Training and validation set factor levels do not match."
        )
      }
    }
    if (!is.null(z)) {
      z_levels <- lapply(z[, .SD, .SDcols = index_factor], levels)
      if (
        !all(sapply(seq_along(x_levels), function(i) {
          identical(x_levels[[i]], z_levels[[i]])
        }))
      ) {
        cli::cli_abort("Training and test set factor levels do not match.")
      }
    }
  }
  invisible()
} # /method(check_factor_levels, class_data.table)

#' Check data ahead of supervised learning
#'
#' @param x Data frame: Training set features and outcome in the last column.
#' @param dat_validation Data frame: Validation set features and outcome in the last column.
#' @param dat_test Data frame: Test set features and outcome in the last column.
#' @param allow_missing Logical: If TRUE, allow missing values in the data.
#' @param verbosity Integer: Verbosity level.
#'
#' @return NULL, invisibly. Stops execution if checks fail.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_supervised(training_data, validation_data, test_data)
#' }
check_supervised <- function(
  x,
  dat_validation = NULL,
  dat_test = NULL,
  allow_missing = TRUE,
  verbosity = 1L
) {
  # if (upsample && downsample) cli::cli_abort("Only one of upsample and downsample can be TRUE")

  if (verbosity > 0L) {
    msgstart("Checking data is ready for training...")
  }

  # Check types ----
  check_inherits(x, "data.frame")
  if (!is.null(dat_validation)) {
    check_inherits(dat_validation, "data.frame")
  }
  if (!is.null(dat_test)) {
    check_inherits(dat_test, "data.frame")
  }

  # Check dimensions ----
  ncols <- NCOL(x)
  # Since one column must be outcome, need min of 2 columns
  if (ncols < 2) {
    cli::cli_abort("Data must contain at least 1 feature and 1 outcome column.")
  }
  if (!is.null(dat_validation)) {
    if (NCOL(dat_validation) != ncols) {
      cli::cli_abort(
        "\nValidation set must contain same number of columns as training set."
      )
    }
  }
  if (!is.null(dat_test)) {
    if (NCOL(dat_test) != ncols) {
      cli::cli_abort(
        "Test set must contain same number of columns as training set."
      )
    }
  }

  # Missing values ----
  if (anyNA(outcome(x))) {
    cli::cli_abort("Training set outcome cannot contain any missing values.")
  }
  if (!allow_missing && anyNA(x)) {
    cli::cli_abort("Data should not contain missing values.")
  }

  # Outcome class ----
  outcome_class <- class(x[[ncols]])
  if (!outcome_class %in% c("integer", "numeric", "factor")) {
    cli::cli_abort("Outcome must be integer, numeric, or factor.")
  }
  if (!is.null(dat_validation)) {
    if (class(dat_validation[[ncols]]) != outcome_class) {
      cli::cli_abort("Training and validation outcome must be of same class.")
    }
  }
  if (!is.null(dat_test)) {
    if (class(dat_test[[ncols]]) != outcome_class) {
      cli::cli_abort("Training and test outcome must be of same class.")
    }
  }

  # Factor levels ----
  # Check that factors across training, validation, and test contain the same levels.
  check_factor_levels(x = x, y = dat_validation, z = dat_test)

  if (verbosity > 0L) {
    msgdone()
  }
  invisible()
} # /rtemis::check_supervised


#' Check data ahead of unsupervised learning
#'
#' @param x Data frame: Features for unsupervised learning.
#' @param allow_missing Logical: If TRUE, allow missing values in the data. Default is FALSE.
#'
#' @return NULL, invisibly. Stops execution if checks fail.
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_unsupervised_data(features_data)
#' }
check_unsupervised_data <- function(x, allow_missing = FALSE, verbosity = 1L) {
  if (verbosity > 0L) {
    msgstart("Checking unsupervised data...")
  }
  if (NCOL(x) < 2) {
    cli::cli_abort("Data must contain at least 2 columns.")
  }
  if (any(sapply(x, function(x) !is.numeric(x)))) {
    cli::cli_abort("All columns must be numeric.")
  }
  if (!allow_missing && anyNA(x)) {
    cli::cli_abort("Data should not contain missing values.")
  }
  if (verbosity > 0L) {
    msgdone()
  }
  invisible()
} # /rtemis::check_unsupervised_data
