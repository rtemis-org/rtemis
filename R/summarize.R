# summarize.R
# ::rtemis::
# 2021 EDG rtemis.org

#' Summarize numeric variables
#'
#' @param x data.frame or data.table (will be coerced to data.table)
#' @param varname Character, vector: Variable name(s) to summarize. Must be column names in `x`
#' of type numeric.
#' @param group_by Character, vector: Variable name(s) of factors to group by. Must be column names
#' in `x`.
#' @param type Character: "all", "median-range" or "mean-sd".
#' which returns Mean, SD, Median, Range, NA (number of NA values)
#' @param na.rm Logical: Passed to `median` and `mean`.
#'
#' @return `data.table` with summary
#'
#' @author EDG
#' @export

summarize <- function(
  x,
  varname,
  group_by = NULL,
  type = c("all", "median-range", "mean-sd"),
  na.rm = TRUE
) {
  UseMethod("summarize")
} # /rtemis::summarize

#' @method summarize data.frame
#' @export
summarize.data.frame <- function(
  x,
  varname,
  group_by = NULL,
  type = c("all", "median-range", "mean-sd"),
  na.rm = TRUE
) {
  summarize(
    as.data.table(x),
    varname,
    group_by,
    type,
    na.rm
  )
} # /rtemis::summarize.data.frame

#' @method summarize data.table
#' @export
summarize.data.table <- function(
  x,
  varname,
  group_by = NULL,
  type = c("all", "median-range", "mean-sd"),
  na.rm = TRUE
) {
  type <- match.arg(type)

  if (type == "all") {
    x[,
      list(
        Var = varname,
        N = sapply(.SD, length),
        Mean = sapply(.SD, mean, na.rm = na.rm),
        SD = sapply(.SD, sd, na.rm = na.rm),
        Median = sapply(.SD, median, na.rm = na.rm),
        Range = sapply(.SD, catrange),
        `N missing` = sapply(.SD, function(i) sum(is.na(i)))
      ),
      .SDcols = varname,
      by = group_by
    ]
  } else if (type == "median-range") {
    x[,
      list(
        Var = varname,
        N = sapply(.SD, length),
        Median = sapply(.SD, median, na.rm = na.rm),
        Range = sapply(.SD, catrange),
        `N missing` = sapply(.SD, function(i) sum(is.na(i)))
      ),
      .SDcols = varname,
      by = group_by
    ]
  } else {
    x[,
      list(
        Var = varname,
        N = sapply(.SD, length),
        Mean = sapply(.SD, mean, na.rm = na.rm),
        SD = sapply(.SD, sd, na.rm = na.rm),
        `N missing` = sapply(.SD, function(i) sum(is.na(i)))
      ),
      .SDcols = varname,
      by = group_by
    ]
  }
} # /rtemis::summarize.data.table
