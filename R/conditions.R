# conditions.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% rtemis_conditions ----
#' rtemis error conditions
#'
#' @description
#' Errors signalled by \pkg{rtemis} are classed R conditions, so they can be
#' handled selectively with [tryCatch()] or [withCallingHandlers()] rather than
#' matched by message text. Each error carries a class vector ordered from the
#' most specific failure mode to the most general, so a handler can catch
#' narrowly (e.g. only type errors) or broadly (e.g. any invalid input).
#'
#' Every rtemis error inherits, in order: its specific class (when applicable),
#' its category class, and finally `"rtemis_error"`, `"error"`, `"condition"`.
#' Catching `rtemis_error` therefore catches *all* rtemis errors.
#'
#' @details
#' # Categories and specific classes
#'
#' ## `rtemis_input_error`
#' A function argument supplied by the caller is invalid. Specific classes:
#' \describe{
#'   \item{`rtemis_type_error`}{Argument is of the wrong type or class.}
#'   \item{`rtemis_value_error`}{Argument has a disallowed value or choice.}
#'   \item{`rtemis_length_error`}{Argument has the wrong length or count.}
#'   \item{`rtemis_range_error`}{Argument is outside its allowed range.}
#'   \item{`rtemis_null_input`}{A required argument is `NULL` or empty.}
#' }
#'
#' ## `rtemis_data_error`
#' The supplied dataset is unsuitable for the requested operation. Specific
#' classes:
#' \describe{
#'   \item{`rtemis_dim_error`}{Data has the wrong dimensions (e.g. too few columns).}
#'   \item{`rtemis_level_mismatch`}{Factor levels do not match across data splits.}
#'   \item{`rtemis_missing_data`}{Data contains missing values where none are allowed.}
#'   \item{`rtemis_outcome_error`}{The outcome column has an invalid or mismatched type.}
#' }
#'
#' ## `rtemis_io_error`
#' A filesystem operation failed. Specific classes:
#' \describe{
#'   \item{`rtemis_file_not_found`}{A path or file does not exist.}
#'   \item{`rtemis_file_exists`}{A file already exists and overwriting was not permitted.}
#' }
#'
#' ## `rtemis_unsupported_error`
#' The requested algorithm, feature, or combination is not supported (e.g.
#' multiclass classification for an algorithm that only handles binary
#' outcomes). No specific subclass.
#'
#' ## `rtemis_runtime_error`
#' A model or algorithm call failed during execution. The original error is
#' attached as the condition's `$parent`. No specific subclass.
#'
#' @return Not applicable. This page documents the condition classes; the
#' functions that signal them do not return when they error.
#'
#' @seealso [rtemis-package]
#'
#' @author EDG
#'
#' @examples
#' # Every rtemis error inherits `rtemis_error`. Catch a category broadly:
#' result <- tryCatch(
#'   set_outcome(iris, "nonexistent_column"),
#'   rtemis_input_error = function(e) {
#'     message("Caught an input error: ", conditionMessage(e))
#'     NULL
#'   }
#' )
#'
#' # Or catch a specific failure mode only:
#' result <- tryCatch(
#'   set_outcome(iris, "nonexistent_column"),
#'   rtemis_value_error = function(e) {
#'     message("Caught a value error: ", conditionMessage(e))
#'     NULL
#'   }
#' )
#'
#' @name rtemis_conditions
#' @aliases rtemis_error rtemis_input_error rtemis_type_error rtemis_value_error rtemis_length_error rtemis_range_error rtemis_null_input rtemis_data_error rtemis_dim_error rtemis_level_mismatch rtemis_missing_data rtemis_outcome_error rtemis_io_error rtemis_file_not_found rtemis_file_exists rtemis_unsupported_error rtemis_runtime_error
NULL
