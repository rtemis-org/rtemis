#' Example longitudinal dataset
#'
#' A small synthetic dataset demonstrating various participation patterns
#' in longitudinal data, suitable for examples with \code{\link{xtdescribe}}.
#'
#' @format A data frame with 30 rows and 4 variables:
#' \describe{
#'   \item{patient_id}{Integer: Patient identifier (1-10).}
#'   \item{year}{Integer: Year of measurement (2020-2024).}
#'   \item{blood_pressure}{Numeric: Systolic blood pressure measurement.}
#'   \item{treatment}{Character: Treatment group ("A" or "B").}
#' }
#'
#' @details
#' This dataset includes 10 patients measured at up to 5 time points (years 2020-2024).
#' The dataset demonstrates various participation patterns typical in longitudinal studies:
#' \itemize{
#'   \item Complete participation (all time points)
#'   \item Early dropout
#'   \item Late entry
#'   \item Intermittent participation
#'   \item Single time point participation
#' }
#'
#' @examples
#' \dontrun{
#' data(xt_example)
#' xtdescribe(xt_example)
#' }
#'
#' @keywords datasets
"xt_example"
