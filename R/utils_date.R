# utils_date.R
# ::rtemis::
# 2024- EDG rtemis.org

#' Extract features from dates
#'
#' @details weekday and month will be extracted as factors, year as integer
#'
#' @param dates Date vector.
#' @param features Character vector: features to extract.
#' @param drop_dates Logical: If TRUE, drop original date column.
#'
#' @return data.table with extracted features
#'
#' @author EDG
#' @keywords internal
#' @noRd
dates2features <- function(
  dates,
  features = c("weekday", "month", "year"),
  drop_dates = TRUE
) {
  # appease R CMD check
  weekday <- NULL
  # to factors: dow, month
  dt <- data.table(dates = dates)
  if ("weekday" %in% features) {
    dt[, weekday := factor(weekdays(dates))]
  }
  if ("month" %in% features) {
    dt[, month := factor(months(dates))]
  }
  if ("year" %in% features) {
    dt[, year := year(dates)]
  }
  if (drop_dates) {
    dt[, dates := NULL]
  }
  dt
} # /rtemis::dates2features


#' Get holidays from date vector
#'
#' @param dates Date vector
#' @param holidays Character vector: holidays to extract
#'
#' @return Factor of length `length(dates)` with levels "Not Holiday", "Holiday"
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_holidays <- function(
  dates,
  holidays = c("LaborDay", "NewYearsDay", "ChristmasDay")
) {
  # Get years from dates
  years <- unique(data.table::year(dates))
  # Get all holidays in all years
  .holidays <- do.call(
    "c",
    lapply(years, function(year) {
      do.call(
        "c",
        lapply(holidays, function(holiday) {
          timeDate::as.Date.timeDate(timeDate::holiday(
            year = year,
            Holiday = holiday
          ))
        })
      )
    })
  )
  # Return intersection of dates and holidays
  holidays_fct <- factor(
    rep(0, length(dates)),
    levels = c(0, 1),
    labels = c("Not Holiday", "Holiday")
  )
  holidays_fct[dates %in% .holidays] <- "Holiday"
  holidays_fct
} # /rtemis::get_holidays

#' Date to factor time bin
#'
#' Convert Date to time bin factor.
#'
#' Order of levels will be chronological (important e.g. for plotting)
#' Additionally, can output ordered factor with `ordered = TRUE`
#'
#' @param x Date vector
#' @param time_bin Character: "year", "quarter", "month", or "day"
#' @param make_bins Character: "range" or "preseent". If "range" the factor levels will include all
#' time periods define by `time_bin` within `bin_range`. This means factor levels can be
#' empty. Otherwise, if "present", factor levels only include time periods present in data.
#' @param bin_range Date, vector, length 2: Range of dates to make levels for. Defaults to range of
#' input dates `x`
#' @param ordered Logical: If TRUE, factor output is ordered.
#'
#' @return factor of time periods
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' startDate <- as.Date("2018-01-01")
#' endDate <- as.Date("2020-12-31")
#' time <- sample(seq(startDate, endDate, length.out = 100))
#' date2factor(time)
#' date2factor(time, "quarter")
#' date2factor(time, "month")
#' date2factor(time, "day")
#' # range vs present
#' x <- sample(seq(as.Date("2018-01-01"), as.Date("2021-01-01"), by = 1), 10)
#' date2factor(x, time_bin = "quarter", make_bins = "present")
#' date2factor(x, time_bin = "quarter", make_bins = "range")
#' }
date2factor <- function(
  x,
  time_bin = c("year", "quarter", "month", "day"),
  make_bins = c("range", "present"),
  bin_range = range(x, na.rm = TRUE),
  ordered = FALSE
) {
  time_bin <- match.arg(time_bin)
  make_bins <- match.arg(make_bins)

  if (time_bin == "year") {
    if (make_bins == "present") {
      factor(data.table::year(x), ordered = ordered)
    } else {
      out <- as.character(data.table::year(x))
      factor(
        out,
        levels = as.character(seq(
          data.table::year(bin_range[1]),
          data.table::year(bin_range[2])
        )),
        ordered = ordered
      )
    }
  } else if (time_bin == "quarter") {
    if (make_bins == "present") {
      factor(
        paste0(data.table::year(x), " Q", data.table::quarter(x)),
        ordered = ordered
      )
    } else {
      factor(
        paste0(data.table::year(x), " Q", data.table::quarter(x)),
        levels = levels(date2yq(seq(
          bin_range[1],
          bin_range[2],
          by = "quarter"
        ))),
        ordered = ordered
      )
    }
  } else if (time_bin == "month") {
    ym <- paste(substr(months(x), 1, 3), data.table::year(x))
    if (make_bins == "present") {
      .levels <- unique(ym[order(x)])
      factor(ym, levels = .levels, ordered = ordered)
    } else {
      factor(
        ym,
        levels = levels(date2ym(seq(bin_range[1], bin_range[2], by = "month"))),
        ordered = ordered
      )
    }
  } else if (time_bin == "day") {
    if (make_bins == "present") {
      factor(x, levels = as.character(unique(x[order(x)])), ordered = ordered)
    } else {
      factor(
        x,
        levels = as.character(seq(bin_range[1], bin_range[2], by = 1)),
        ordered = ordered
      )
    }
  }
} # /rtemis::date2factor


#' Date to year-quarter factor
#'
#' @param x Date vector
#' @param ordered Logical: If TRUE, return ordered factor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
date2yq <- function(x, ordered = FALSE) {
  factor(
    paste0(data.table::year(x), " Q", data.table::quarter(x)),
    ordered = ordered
  )
} # /rtemis::date2yq


#' Date to year-month factor
#'
#' @param x Date vector
#' @param ordered Logical: If TRUE, return ordered factor.
#'
#' @author EDG
#' @keywords internal
#' @noRd
date2ym <- function(x, ordered = FALSE) {
  ym <- paste(substr(months(x), 1, 3), data.table::year(x))
  .levels <- unique(ym[order(x)])
  factor(ym, levels = .levels, ordered = ordered)
} # /rtemis::date2ym
