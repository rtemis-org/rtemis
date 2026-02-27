# check_data.R
# ::rtemis::
# 2022- EDG rtemis.org

# %% check_data ----
#' Check Data
#'
#' @param x tabular data: Input to be checked.
#' @param name Character: Name of dataset.
#' @param get_duplicates Logical: If TRUE, check for duplicate cases.
#' @param get_na_case_pct Logical: If TRUE, calculate percent of NA values per
#' case.
#' @param get_na_feature_pct Logical: If TRUE, calculate percent of NA values
#' per feature.
#'
#' @return `CheckData` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' n <- 1000
#' x <- rnormmat(n, 50, return_df = TRUE)
#' x$char1 <- sample(letters, n, TRUE)
#' x$char2 <- sample(letters, n, TRUE)
#' x$fct <- factor(sample(letters, n, TRUE))
#' x <- rbind(x, x[1, ])
#' x$const <- 99L
#' x[sample(nrow(x), 20), 3] <- NA
#' x[sample(nrow(x), 20), 10] <- NA
#' x$fct[30:35] <- NA
#' check_data(x)
check_data <- function(
  x,
  name = NULL,
  get_duplicates = TRUE,
  get_na_case_pct = FALSE,
  get_na_feature_pct = FALSE
) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  # Check is tabular
  check_tabular(x)
  # Get class of x before converting to data.table
  object_class <- class(x)[1]
  # Convert to data.table
  x <- as.data.table(x)
  n_rows <- NROW(x)
  n_cols <- NCOL(x)

  # Data Types ----
  classes <- sapply(x, \(v) class(v)[1])
  counts <- table(classes)

  ## Numeric ----
  n_numeric <- max0(counts["numeric"])

  ## Integers ----
  n_integer <- max0(counts["integer"])

  ## Characters ----
  n_character <- max0(counts["character"])

  ## Factors ----
  index_factor <- which(sapply(x, is.factor))
  n_factor <- length(index_factor)
  index_ordered <- which(sapply(x, is.ordered))
  n_ordered <- length(index_ordered)

  ## Dates ----
  n_date <- sum(
    max0(counts["Date"]),
    max0(counts["IDate"]),
    max0(counts["POSIXct"]),
    max0(counts["POSIXlt"])
  )

  # Issues ----

  ## Constants ----
  index_constant <- which(sapply(x, is_constant))
  n_constant <- length(index_constant)

  ## Duplicates ----
  n_duplicates <- if (get_duplicates) {
    n_rows - uniqueN(x)
  } else {
    NA
  }

  ## NAs ----
  cols_anyna <- which(sapply(x, anyNA))
  n_cols_anyna <- length(cols_anyna)
  index_na <- which(is.na(x))
  n_na <- length(index_na)

  ## Get percent of NA values per feature and per case
  if (n_cols_anyna > 0) {
    na_feature_pct <- if (get_na_feature_pct) {
      data.frame(
        Feature = names(cols_anyna),
        Pct_NA = sapply(seq_len(n_cols_anyna), \(i) {
          sum(is.na(x[[cols_anyna[1]]])) / n_cols
        })
      )
    } else {
      NULL
    }

    index_incomplete <- which(!complete.cases(x))
    n_incomplete <- length(index_incomplete)

    na_case_pct <- if (get_na_case_pct) {
      data.frame(
        Case = index_incomplete,
        Pct_NA = sapply(seq_len(n_incomplete), \(i) {
          sum(is.na(x[index_incomplete[i], ])) / n_rows
        })
      )
    } else {
      NULL
    }

    # Get types of features with NA
    classes_na <- table(classes[cols_anyna])

    # Get N of NAs in last column
    n_na_last_col <- sum(is.na(x[[n_cols]]))
  } else {
    n_na_last_col <- 0
    classes_na <- NULL
    na_feature_pct <- na_case_pct <- rep(0, n_cols)
  }

  # CheckData ----
  CheckData(
    object_class = object_class,
    name = name,
    n_rows = n_rows,
    n_cols = n_cols,
    n_numeric = n_numeric,
    n_integer = n_integer,
    n_character = n_character,
    n_factor = n_factor,
    n_ordered = n_ordered,
    n_date = n_date,
    n_constant = n_constant,
    n_duplicates = n_duplicates,
    n_cols_anyna = n_cols_anyna,
    n_na = n_na,
    classes_na = classes_na,
    na_feature_pct = na_feature_pct,
    na_case_pct = na_case_pct,
    n_na_last_col = n_na_last_col
  )
} # /rtemis::check_data


# %% max0 ----
#' Helper function to get max or 0
#'
#' @param x Numeric vector
#'
#' @return Numeric: max(x, 0)
#'
#' @author EDG
#' @keywords internal
#' @noRd
max0 <- function(x) max(x, 0, na.rm = TRUE)


# %% to_html.CheckData ----
#' Generate `CheckData` object description in HTML
#'
#' @param x `CheckData` object
#' @param name Character: Name of the data set
#' @param css List: CSS styles
#'
#' @return `shiny.tag` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_html, CheckData) <- function(
  x,
  name = NULL,
  css = list(
    font_family = "Helvetica",
    color = "#fff",
    background_color = "#242424"
  )
) {
  n_rows <- x[["n_rows"]]
  n_cols <- x[["n_cols"]]
  n_numeric <- x[["n_numeric"]]
  n_integer <- x[["n_integer"]]
  n_character <- x[["n_character"]]
  n_factor <- x[["n_factor"]]
  n_ordered <- x[["n_ordered"]]
  n_date <- x[["n_date"]]
  n_constant <- x[["n_constant"]]
  n_duplicates <- x[["n_duplicates"]]
  n_cols_anyna <- x[["n_cols_anyna"]]
  n_na <- x[["n_na"]]
  classes_na <- x[["classes_na"]]

  ## Data Types ----
  numeric <- HTML(paste(
    strong(n_numeric),
    "numeric",
    ngettext(n_numeric, "feature", "features")
  ))
  integer <- HTML(paste(
    strong(n_integer),
    "integer",
    ngettext(n_integer, "feature", "features")
  ))
  categorical <- HTML(paste0(
    strong(n_factor),
    ngettext(n_factor, " factor", " factors"),
    if (n_factor == 1) {
      paste(", which", ngettext(n_ordered, "is", "is not"), "ordered")
    } else if (n_factor > 1) {
      paste(
        ", of which",
        strong(n_ordered),
        ngettext(n_ordered, "is", "are"),
        "ordered"
      )
    }
  ))
  # .col <- if (n_character > 0) html_orange else strong
  .col <- strong
  characters <- HTML(paste(
    .col(n_character),
    "character",
    ngettext(n_character, "feature", "features")
  ))
  dates <- HTML(paste(
    strong(n_date),
    "date",
    ngettext(n_date, "feature", "features")
  ))

  ## Issues ----
  .col <- if (n_constant > 0) html_red else strong
  constants <- HTML(paste(
    .col(n_constant),
    "constant",
    ngettext(n_constant, "feature", "features")
  ))
  .col <- if (n_duplicates > 0) html_orange else strong
  duplicates <- HTML(paste(
    .col(n_duplicates),
    "duplicate",
    ngettext(n_duplicates, "case", "cases")
  ))

  .col <- if (n_cols_anyna > 0) html_orange else strong
  nas <- if (n_cols_anyna > 0) {
    HTML(paste(
      .col(n_cols_anyna),
      ngettext(n_cols_anyna, "feature includes", "features include"),
      "'NA' values; ",
      .col(n_na),
      "'NA'",
      ngettext(n_na, "value", "values"),
      "total",
      tags[["ul"]](
        lapply(seq_along(classes_na), \(i) {
          tags[["li"]](HTML(paste(
            .col(classes_na[i]),
            tolower(names(classes_na)[i])
            # ngettext(classes_na[i], "feature", "features")
          )))
        })
      )
    ))
  } else {
    HTML(paste(strong("0"), "missing values"))
  }

  ## Recs ----
  rec_constant <- if (n_constant > 0) {
    tags[["li"]](HTML(paste(html_orange(
      "Remove the constant",
      ngettext(n_constant, "feature", "features")
    ))))
  } else {
    NULL
  }

  rec_dups <- if (n_duplicates > 0) {
    tags[["li"]](HTML(paste(html_orange(
      "Consider removing the duplicate",
      ngettext(n_duplicates, "case", "cases")
    ))))
  } else {
    NULL
  }

  rec_na <- if (n_cols_anyna > 0) {
    list(
      if (isTRUE(classes_na["factor"] > 0)) {
        tags[["li"]](HTML(paste(html_orange(
          "Consider assigning factor 'NA' values to new 'missing' level"
        ))))
      },
      tags[["li"]](HTML(paste(html_orange(
        "Consider imputing missing values or using algorithms that can handle missing values"
      ))))
    )
  } else {
    NULL
  }

  recs <- if (sum(n_constant, n_duplicates, n_cols_anyna) == 0) {
    tags[["li"]](html_success("Everything looks good"))
  } else {
    list(
      rec_constant,
      rec_dups,
      rec_na
    )
  }
  ## out ----
  div(
    p(
      div(
        html_highlight(name),
        ": A",
        x[["class"]],
        "with",
        html_highlight(n_rows),
        ngettext(n_rows, "row", "rows"),
        "and",
        html_highlight(n_cols),
        ngettext(n_cols, "feature", "features"),
        class = "checkdata-header"
      )
    ),
    p(
      span(strong("Data types"), class = "sidelined"),
      tags[["ul"]](
        tags[["li"]](numeric),
        tags[["li"]](integer),
        tags[["li"]](categorical),
        tags[["li"]](characters),
        tags[["li"]](dates)
      )
    ), # p Data Types
    p(
      span(strong("Issues"), class = "sidelined"),
      tags[["ul"]](
        tags[["li"]](constants),
        tags[["li"]](duplicates),
        tags[["li"]](nas)
      )
    ), # p Issues
    p(
      span(strong("Recommendations"), class = "sidelined"),
      tags[["ul"]](
        recs
      )
    ), # p Recommendations
    class = "checkData",
    style = paste0(
      "font-family:",
      css[["font_family"]],
      "; color:",
      css[["color"]],
      "; background-color:",
      css[["background_color"]],
      ";"
    )
  )
} # /rtemis::to_html.CheckData
