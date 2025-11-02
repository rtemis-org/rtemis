# S7_utils
# ::rtemis::
# 2025 EDG rtemis.org

# SuperWorkers ----
#' @keywords internal
#' @noRd
SuperWorkers <- new_class(
  name = "SuperWorkers",
  properties = list(
    algorithm = class_character,
    max_workers = class_integer,
    max_workers_algorithm = class_integer,
    max_workers_tuning = class_integer,
    max_workers_resampling = class_integer
  ),
  constructor = function(
    algorithm,
    max_workers,
    max_workers_algorithm,
    max_workers_tuning,
    max_workers_resampling
  ) {
    max_workers <- clean_posint(max_workers)
    max_workers_algorithm <- clean_posint(max_workers_algorithm)
    max_workers_tuning <- clean_posint(max_workers_tuning)
    max_workers_resampling <- clean_posint(max_workers_resampling)
    # Validate input
    if (
      max_workers_algorithm + max_workers_tuning + max_workers_resampling >
        max_workers
    ) {
      cli::cli_abort(
        "Total workers for algorithm, tuning, and resampling cannot exceed max_workers."
      )
    }
    new_object(
      S7_object(),
      algorithm = algorithm,
      max_workers = max_workers,
      max_workers_algorithm = max_workers_algorithm,
      max_workers_tuning = max_workers_tuning,
      max_workers_resampling = max_workers_resampling
    )
  }
) # /rtemis::SuperWorkers

# CheckData ----
#' @author EDG
#' @noRd
CheckData <- new_class(
  name = "CheckData",
  properties = list(
    object_class = class_character,
    name = class_character,
    n_rows = class_integer,
    n_cols = class_integer,
    n_numeric = class_integer,
    n_integer = class_integer,
    n_character = class_integer,
    n_factor = class_integer,
    n_ordered = class_integer,
    n_date = class_integer,
    n_constant = class_integer,
    n_duplicates = class_integer,
    n_cols_anyna = class_integer,
    n_na = class_integer,
    classes_na = class_any | NULL,
    na_feature_pct = class_double | NULL,
    na_case_pct = class_double | NULL,
    n_na_last_col = class_integer | NULL
  ),
  constructor = function(
    object_class,
    name,
    n_rows,
    n_cols,
    n_numeric,
    n_integer,
    n_character,
    n_factor,
    n_ordered,
    n_date,
    n_constant,
    n_duplicates,
    n_cols_anyna,
    n_na,
    classes_na = NULL,
    na_feature_pct = NULL,
    na_case_pct = NULL,
    n_na_last_col = NULL
  ) {
    n_rows <- clean_int(n_rows)
    n_cols <- clean_int(n_cols)
    n_numeric <- clean_int(n_numeric)
    n_integer <- clean_int(n_integer)
    n_character <- clean_int(n_character)
    n_factor <- clean_int(n_factor)
    n_ordered <- clean_int(n_ordered)
    n_date <- clean_int(n_date)
    n_constant <- clean_int(n_constant)
    n_duplicates <- clean_int(n_duplicates)
    n_cols_anyna <- clean_int(n_cols_anyna)
    n_na <- clean_int(n_na)
    check_float01inc(na_case_pct)
    n_na_last_col <- clean_int(n_na_last_col)
    new_object(
      S7_object(),
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
  }
) # /rtemis::CheckData

# Make CheckData properties `$`-accessible
method(`$`, CheckData) <- function(x, name) {
  prop(x, name)
} # /rtemis::`$`.CheckData

# `$`-autocomplete CheckData properties
method(`.DollarNames`, CheckData) <- function(x, pattern = "") {
  all_names <- names(x)
  grep(pattern, all_names, value = TRUE)
} # /rtemis::`.DollarNames`.CheckData

# Make CheckData properties `[[`-accessible
method(`[[`, CheckData) <- function(x, name) {
  prop(x, name)
} # /rtemis::`[[`.CheckData

# Print CheckData ----
#' Print `CheckData` object
#'
#' @method print CheckData
#' @param x `CheckData` object.
#' @param type Character: Output type: "ansi" or "html".
#' @param name Character: Dataset name.
#' @param check_integers Logical: If TRUE and there are integer features, prints a
#' message to consider converting to factors.
#' @param css List with `font.family`, `color`, and `background.color` elements.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
print.CheckData <- function(
  x,
  type = c("ansi", "html"),
  name = NULL,
  check_integers = FALSE,
  css = list(
    font.family = "Helvetica",
    color = "#fff",
    background.color = "#242424"
  ),
  ...
) {
  if (is.null(name)) {
    name <- x[["name"]]
    if (is.null(name)) name <- deparse(substitute(x))
  }
  type <- match.arg(type)

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
  na_feature_pct <- x[["na_feature_pct"]]
  na_case_pct <- x[["na_case_pct"]]
  n_na_last_col <- x[["n_na_last_col"]]

  if (type == "ansi") {
    # ansi out ----
    out <- paste0(
      "  ",
      highlight(name),
      paste(
        ": A",
        x[["object_class"]],
        "with",
        highlight(n_rows),
        ngettext(n_rows, "row", "rows"),
        "and",
        highlight(n_cols),
        ngettext(n_cols, "column.", "columns.")
      )
    )
    ## Data Types ----
    out <- paste(
      out,
      bold("\n  Data types"),
      paste(
        "  *",
        bold(n_numeric),
        "numeric",
        ngettext(n_numeric, "feature", "features")
      ),
      paste(
        "  *",
        bold(n_integer),
        "integer",
        ngettext(n_integer, "feature", "features")
      ),
      sep = "\n"
    )
    isOrdered <- if (n_factor == 1) {
      paste(", which", ngettext(n_ordered, "is", "is not"), "ordered")
    } else if (n_factor > 1) {
      paste(
        ", of which",
        bold(n_ordered),
        ngettext(n_ordered, "is", "are"),
        "ordered"
      )
    } else {
      ""
    }
    out <- paste(
      out,
      paste0(
        "  * ",
        bold(n_factor),
        ngettext(n_factor, " factor", " factors"),
        isOrdered
      ),
      sep = "\n"
    )
    out <- paste(
      out,
      paste(
        "  *",
        bold(n_character),
        "character",
        ngettext(n_character, "feature", "features")
      ),
      sep = "\n"
    )
    out <- paste(
      out,
      paste(
        "  *",
        bold(n_date),
        "date",
        ngettext(n_date, "feature", "features")
      ),
      sep = "\n"
    )
    ## Issues ----
    out <- paste(out, bold("\n  Issues"), sep = "\n")
    fmt <- ifelse(n_constant > 0, red, I)
    out <- paste(
      out,
      paste(
        "  *",
        bold(fmt(n_constant)),
        "constant",
        ngettext(n_constant, "feature", "features")
      ),
      sep = "\n"
    )
    fmt <- ifelse(n_duplicates > 0, orange, I)
    out <- paste(
      out,
      paste(
        "  *",
        bold(fmt(n_duplicates)),
        "duplicate",
        ngettext(n_duplicates, "case", "cases")
      ),
      sep = "\n"
    )
    nas <- if (n_cols_anyna > 0) {
      classes_na <- x[["classes_na"]]
      .col <- if (n_cols_anyna > 0) orange else I
      out_nas <- paste(
        bold(.col(n_cols_anyna)),
        ngettext(n_cols_anyna, "feature includes", "features include"),
        "'NA' values;",
        bold(.col(n_na)),
        "'NA'",
        ngettext(n_na, "value", "values"),
        "total\n    *",
        paste0(
          sapply(seq_along(classes_na), \(i) {
            paste(
              bold(.col(classes_na[i])),
              tolower(names(classes_na)[i])
            )
          }),
          collapse = "; "
        )
      )
      if (n_na_last_col > 0) {
        out_nas <- paste(
          out_nas,
          paste0(
            "\n    * ",
            bold(.col(n_na_last_col)),
            ngettext(n_na_last_col, " missing value", " missing values"),
            " in the last column"
          )
        )
      }
      out_nas
    } else {
      paste(bold("0"), "missing values")
    }
    out <- paste0(out, "\n  * ", nas)

    ## Recommendations ----
    out <- paste(out, bold("\n  Recommendations"), sep = "\n")
    if (sum(n_character, n_constant, n_duplicates, n_cols_anyna) > 0) {
      if (n_character > 0) {
        out <- paste(
          out,
          bold(orange(
            "  * Consider converting character features to factors or excluding them."
          )),
          sep = "\n"
        )
      }
      if (n_constant > 0) {
        out <- paste(
          out,
          bold(red(paste(
            "  * Remove the constant",
            ngettext(n_constant, "feature.", "features.")
          ))),
          sep = "\n"
        )
      }

      if (n_duplicates > 0) {
        out <- paste(
          out,
          bold(orange(paste(
            "  * Consider removing the duplicate",
            ngettext(n_duplicates, "case.", "cases.")
          ))),
          sep = "\n"
        )
      }

      if (n_cols_anyna > 0) {
        out <- paste(
          out,
          bold(blue(paste(
            "  * Consider using algorithms that can handle missingness or imputing missing values."
          ))),
          sep = "\n"
        )
        # Note regarding missing values in last column
        if (n_na_last_col > 0) {
          out <- paste(
            out,
            bold(orange(
              "\n  * Filter cases with missing values in the last column if using dataset for supervised learning.\n"
            ))
          )
        }
      }
      if (check_integers && n_integer > 0) {
        out <- paste(
          out,
          paste0(
            "  * Check the",
            ifelse(n_integer > 1, paste("", n_integer, ""), " "),
            "integer",
            ngettext(n_integer, " feature", " features"),
            " and consider if",
            ngettext(n_integer, " it", " they"),
            " should be converted to ",
            ngettext(n_integer, "factor", "factors")
          ),
          sep = "\n"
        )
      }
    } else {
      out <- paste(
        out,
        bold(highlight("  * Everything looks good")),
        sep = "\n"
      )
    }
    cat(out, "\n")
  } else {
    # HTML ----
    htmltools::html_print(
      tohtml(x),
      background = css[["background.color"]]
    )
  }
  invisible(x)
} # /rtemis::print.CheckData
method(print, CheckData) <- function(x, ...) {
  print.CheckData(x)
} # /rtemis::print.CheckData


# BiasVariance ----
BiasVariance <- new_class(
  name = "BiasVariance",
  properties = list(
    bias_squared = class_numeric,
    mean_bias_squared = class_numeric,
    sd_bias_squared = class_numeric,
    variance = class_numeric,
    mean_variance = class_numeric,
    sd_variance = class_numeric
  )
)

# Print BiasVariance ----
#' Print method for BiasVariance
#'
#' @param x BiasVariance object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, BiasVariance) <- function(x, ...) {
  objcat("BiasVariance")
  cat("Mean squared bias: ")
  cat(highlight(ddSci(x[["mean_bias_squared"]])))
  cat(" (", ddSci(x[["sd_bias_squared"]]), ")\n", sep = "")
  cat("    Mean variance: ")
  cat(highlight(ddSci(x[["mean_variance"]])))
  cat(" (", ddSci(x[["sd_variance"]]), ")\n", sep = "")
  cat("\n")
} # /rtemis::print.BiasVariance
