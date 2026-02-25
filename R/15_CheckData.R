# CheckData.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% CheckData ----
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


# %% `$`.CheckData ----
# Make CheckData properties `$`-accessible
method(`$`, CheckData) <- function(x, name) {
  prop(x, name)
} # /rtemis::`$`.CheckData


# %% `.DollarNames`.CheckData ----
# `$`-autocomplete CheckData properties
method(`.DollarNames`, CheckData) <- function(x, pattern = "") {
  all_names <- names(x)
  grep(pattern, all_names, value = TRUE)
} # /rtemis::`.DollarNames`.CheckData


# %% `[[`.CheckData ----
# Make CheckData properties `[[`-accessible
method(`[[`, CheckData) <- function(x, name) {
  prop(x, name)
} # /rtemis::`[[`.CheckData


# %% repr.CheckData ----
#' Repr method for CheckData
#'
#' @param x CheckData object.
#'
#' @return Character: String representation of CheckData object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, CheckData) <- function(
  x,
  name = NULL,
  check_integers = FALSE,
  pad = 0L,
  output_type = NULL
) {
  out <- repr_S7name(x)
  if (is.null(name)) {
    name <- x[["name"]]
    if (is.null(name)) name <- deparse(substitute(x))
  }
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
  n_na_last_col <- x[["n_na_last_col"]]

  ## Object class and dimensions ----
  out <- paste0(
    "  ",
    highlight(name, pad = pad, output_type = output_type),
    paste(
      ": A",
      x[["object_class"]],
      "with",
      highlight(n_rows, pad = pad, output_type = output_type),
      ngettext(n_rows, "row", "rows"),
      "and",
      highlight(n_cols, pad = pad, output_type = output_type),
      ngettext(n_cols, "column.", "columns.")
    )
  )

  ## Data Types ----
  out <- paste(
    out,
    fmt("\n  Data types", bold = TRUE, pad = pad, output_type = output_type),
    paste(
      "  *",
      fmt(n_numeric, bold = TRUE, pad = pad, output_type = output_type),
      "numeric",
      ngettext(n_numeric, "feature", "features")
    ),
    paste(
      "  *",
      fmt(n_integer, bold = TRUE, pad = pad, output_type = output_type),
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
      fmt(n_ordered, bold = TRUE, pad = pad, output_type = output_type),
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
      fmt(n_factor, bold = TRUE, pad = pad, output_type = output_type),
      ngettext(n_factor, " factor", " factors"),
      isOrdered
    ),
    sep = "\n"
  )
  out <- paste(
    out,
    paste(
      "  *",
      fmt(n_character, bold = TRUE, pad = pad, output_type = output_type),
      "character",
      ngettext(n_character, "feature", "features")
    ),
    sep = "\n"
  )
  out <- paste(
    out,
    paste(
      "  *",
      fmt(n_date, bold = TRUE, pad = pad, output_type = output_type),
      "date",
      ngettext(n_date, "feature", "features")
    ),
    sep = "\n"
  )

  ## Issues ----
  out <- paste(
    out,
    fmt("\n  Issues", bold = TRUE, pad = pad, output_type = output_type),
    sep = "\n"
  )
  out <- paste(
    out,
    paste(
      "  *",
      fmt(
        n_constant,
        col = if (n_constant > 0) rt_red else NULL,
        bold = TRUE,
        pad = pad,
        output_type = output_type
      ),
      "constant",
      ngettext(n_constant, "feature", "features")
    ),
    sep = "\n"
  )

  out <- paste(
    out,
    paste(
      "  *",
      fmt(
        n_duplicates,
        col = if (n_duplicates > 0) rt_orange else NULL,
        bold = TRUE,
        pad = pad,
        output_type = output_type
      ),
      "duplicate",
      ngettext(n_duplicates, "case", "cases")
    ),
    sep = "\n"
  )

  nas <- if (n_cols_anyna > 0) {
    classes_na <- x[["classes_na"]]
    .col <- if (n_cols_anyna > 0) rt_orange else NULL
    out_nas <- paste(
      fmt(
        n_cols_anyna,
        col = .col,
        bold = TRUE,
        pad = pad,
        output_type = output_type
      ),
      ngettext(n_cols_anyna, "feature includes", "features include"),
      "'NA' values;",
      fmt(n_na, col = .col, bold = TRUE, pad = pad, output_type = output_type),
      "'NA'",
      ngettext(n_na, "value", "values"),
      "total\n    *",
      paste0(
        sapply(seq_along(classes_na), \(i) {
          paste(
            fmt(
              classes_na[i],
              col = .col,
              bold = TRUE,
              pad = pad,
              output_type = output_type
            ),
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
          fmt(
            n_na_last_col,
            col = .col,
            bold = TRUE,
            pad = pad,
            output_type = output_type
          ),
          ngettext(n_na_last_col, " missing value", " missing values"),
          " in the last column"
        )
      )
    }
    out_nas
  } else {
    paste(
      fmt("0", bold = TRUE, pad = pad, output_type = output_type),
      "missing values"
    )
  }
  out <- paste0(out, "\n  * ", nas)

  ## Recommendations ----
  out <- paste(
    out,
    fmt(
      "\n  Recommendations",
      bold = TRUE,
      pad = pad,
      output_type = output_type
    ),
    sep = "\n"
  )

  if (sum(n_character, n_constant, n_duplicates, n_cols_anyna) > 0) {
    if (n_character > 0) {
      out <- paste(
        out,
        fmt(
          "  * Consider converting character features to factors or excluding them.",
          col = rt_orange,
          bold = TRUE,
          pad = pad,
          output_type = output_type
        ),
        sep = "\n"
      )
    }
    if (n_constant > 0) {
      out <- paste(
        out,
        fmt(
          (paste(
            "  * Remove the constant",
            ngettext(n_constant, "feature.", "features.")
          )),
          col = rt_red,
          bold = TRUE,
          pad = pad,
          output_type = output_type
        ),
        sep = "\n"
      )
    }

    if (n_duplicates > 0) {
      out <- paste(
        out,
        fmt(
          paste(
            "  * Consider removing the duplicate",
            ngettext(n_duplicates, "case.", "cases.")
          ),
          col = rt_orange,
          bold = TRUE,
          pad = pad,
          output_type = output_type
        ),
        sep = "\n"
      )
    }

    if (n_cols_anyna > 0) {
      out <- paste(
        out,
        fmt(
          paste(
            "  * Consider using algorithms that can handle missingness or imputing missing values."
          ),
          col = rt_blue,
          bold = TRUE,
          pad = pad,
          output_type = output_type
        ),
        sep = "\n"
      )
      # Note regarding missing values in last column
      if (n_na_last_col > 0) {
        out <- paste(
          out,
          fmt(
            "\n  * Filter cases with missing values in the last column if using dataset for supervised learning.\n",
            col = rt_orange,
            bold = TRUE,
            pad = pad,
            output_type = output_type
          )
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
      fmt(
        "  * Everything looks good",
        col = highlight_col,
        pad = pad,
        output_type = output_type
      ),
      sep = "\n"
    )
  }
  paste0(out, "\n")
} # /rtemis::repr.CheckData


# %% print.CheckData ----
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
#' @return `CheckData` object, invisibly.
#'
#' @author EDG
#' @noRd
method(print, CheckData) <- function(
  x,
  name = NULL,
  check_integers = FALSE,
  output_type = NULL
) {
  cat(repr(
    x,
    name = name,
    check_integers = check_integers,
    output_type = output_type
  ))
  invisible(x)
} # /rtemis::print.CheckData
