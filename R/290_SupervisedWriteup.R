# 290_SupervisedWriteup.R
# ::rtemis::
# 2026- EDG rtemis.org

# What `writeup()` returns: the Methods and Results sections describing a
# trained supervised model.
#
# Paragraphs are templates. Every number in them is a `{key}` token naming a
# row of the values table, which holds the number, its formatted text and the
# path of the field it was read from; every citation is a `{ref:key}` token
# naming a row of the references table. Rendering replaces the tokens, so each
# number in the text traces to its source, and `ai_writeup()` can reword a
# template while the numbers stay those of the model.
#
# spec: rtemis/writeup

# %% WRITEUP_PARTS ----
WRITEUP_PARTS <- c("methods", "results")

# %% WRITEUP_VALUE_KINDS ----
# How a value is formatted in text:
# - "count"    whole number of cases or items, with thousands separators
# - "integer"  whole-number setting, written in full
# - "exact"    real-valued setting, written in full (15 significant digits)
# - "metric"   performance metric, three decimals
# - "quantity" other real number, three significant digits
# - "p_value"  "p = 0.012", or "p < 0.001"
# - "percent"  proportion written as a percentage, at most one decimal
# - "text"     a value that is text: a software version, an outcome level,
#              a hyperparameter setting
WRITEUP_VALUE_KINDS <- c(
  "count",
  "integer",
  "exact",
  "metric",
  "quantity",
  "p_value",
  "percent",
  "text"
)

# %% WRITEUP_HP_SOURCES ----
# How a hyperparameter's value was chosen, from the record's origin:
# - "specified" supplied by the user
# - "default"   the declared default
# - "resolved"  determined during fitting, from the data or by the backend
# - "tuned"     selected by tuning from the values tried
# - "unset"     left unset; the description states what the fit did
# - "varied"    chosen differently in different resamples; the value cell
#               states how each value was chosen
WRITEUP_HP_SOURCES <- c(
  "specified",
  "default",
  "resolved",
  "tuned",
  "unset",
  "varied"
)

# %% WRITEUP_HP_SOURCE_LABELS ----
WRITEUP_HP_SOURCE_LABELS <- c(
  specified = "specified",
  default = "default",
  resolved = "resolved during fitting",
  tuned = "tuning",
  unset = "unset",
  varied = "varied between resamples"
)

# %% WRITEUP_VALUE_TOKEN ----
# A number token: `{key}`, the key in lowercase snake case. A citation token:
# `{ref:a}` or `{ref:a,b}`, naming rows of the references table.
WRITEUP_VALUE_TOKEN <- "\\{([a-z][a-z0-9_]*)\\}"
WRITEUP_REF_TOKEN <- "\\{ref:([^}]+)\\}"

# %% WRITEUP_METHOD_REFERENCES ----
# Statistical methods `writeup()` cites, keyed as in its templates.
WRITEUP_METHOD_REFERENCES <- c(
  clopper_pearson = paste0(
    "Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits ",
    "illustrated in the case of the binomial. Biometrika, 26(4), 404-413."
  ),
  delong = paste0(
    "DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas ",
    "under two or more correlated receiver operating characteristic curves: ",
    "a nonparametric approach. Biometrics, 44(3), 837-845."
  ),
  mcnemar = paste0(
    "McNemar Q (1947). Note on the sampling error of the difference between ",
    "correlated proportions or percentages. Psychometrika, 12(2), 153-157."
  )
)


# %% WriteupSection ----
#' WriteupSection Class
#'
#' @description
#' One subsection of a writeup: the part it belongs to, its heading and its
#' paragraphs, each a template whose numbers and citations are tokens.
#'
#' @field part Character \{"methods", "results"\}: Section the subsection
#'   belongs to.
#' @field heading Character: Subsection heading.
#' @field paragraphs Character vector: Paragraph templates.
#'
#' @author EDG
#' @keywords internal
#' @noRd
WriteupSection <- schema_class(
  name = "WriteupSection",
  package = "rtemis",
  properties = list(
    part = prop_string(
      WRITEUP_PARTS[[1L]],
      enum = WRITEUP_PARTS,
      description = "Section of the paper the subsection belongs to."
    ),
    heading = prop_string("", description = "Subsection heading."),
    paragraphs = prop_string(
      "",
      vector = TRUE,
      description = "Paragraph templates. A number appears as {key}, naming a row of the writeup's values table; a citation appears as {ref:key} or {ref:key1,key2}, naming rows of its references table."
    )
  ),
  publication = SchemaPublication(
    role = "document",
    slug = "writeupsection",
    title = "rtemis WriteupSection",
    description = "One subsection of a writeup of a trained supervised model: the section it belongs to, its heading, and its paragraphs as templates whose numbers and citations are tokens.",
    order = 32L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::WriteupSection


# %% SupervisedWriteup ----
#' SupervisedWriteup Class
#'
#' @description
#' What `writeup()` returns: the Methods and Results sections describing a
#' trained supervised model, with the values and references the text uses and
#' the review it draws on.
#'
#' @field algorithm Character \{"GLM", "GAM", "GLMNET", "GLMTree", "SPLS", "MARS", "LinearSVM", "RadialSVM", "CART", "Ranger", "LightCART", "LightRF", "LightGBM", "LightRuleFit", "BART", "Isotonic", "MLP", "TabNet", "KNN", "HAL", "MonotonicHAL", "LINAD", "LINADForest", "NNLS", "SuperLearner", "ModalityStacking", "ConditionalSuperLearner"\}:
#'   Algorithm of the model.
#' @field type Character \{"Regression", "Classification"\}: Kind of supervised
#'   learning.
#' @field review `SupervisedReview`: The review the writeup draws on.
#' @field values data.frame: One row per number in the text.
#' @field sections List of `WriteupSection` objects, in order.
#' @field references data.frame: One row per cited work.
#' @field hyperparameters data.frame: One row per hyperparameter: the rows of
#'   the main and supplementary hyperparameter tables.
#' @field primary_listed Logical: Whether the main table lists the algorithm's
#'   primary hyperparameters.
#' @field include_hyperparameters Optional Character vector: Hyperparameters the main
#'   table lists in place of the algorithm's primary hyperparameters.
#' @field not_reported Character vector: Information a Methods section
#'   usually states that the model does not record.
#'
#' @author EDG
#' @noRd
SupervisedWriteup <- schema_class(
  name = "SupervisedWriteup",
  package = "rtemis",
  properties = list(
    algorithm = prop_string(
      enum = names(schema_algorithm_descriptions(Hyperparameters)),
      description = "Algorithm identifier of the model."
    ),
    type = prop_string(
      SUPERVISED_TYPES[[1L]],
      enum = SUPERVISED_TYPES,
      description = "Kind of supervised learning the model performs."
    ),
    review = prop_object(
      SupervisedReview,
      description = "The review of the model the writeup draws on."
    ),
    values = prop_state(prop_table(
      columns = list(
        key = prop_string(
          description = "Token name, as it appears in braces in the paragraph templates."
        ),
        value = prop_float(
          NULL,
          nullable = TRUE,
          description = "The number. Unset for a value of kind 'text'."
        ),
        kind = prop_string(
          enum = WRITEUP_VALUE_KINDS,
          description = "How the value is formatted: 'count' a whole number with thousands separators; 'integer' a whole-number setting in full; 'exact' a real-valued setting in full, to 15 significant digits; 'metric' three decimals; 'quantity' three significant digits; 'p_value' as 'p = 0.012' or 'p < 0.001'; 'percent' a proportion as a percentage with at most one decimal; 'text' a value that is text, such as a software version, an outcome level or a hyperparameter setting."
        ),
        text = prop_string(
          description = "The value as it appears in the text."
        ),
        source = prop_string(
          description = "Where the value was read from: a path of properties in the model or its review, or the package whose version it is."
        )
      ),
      description = "One row per number in the text."
    )),
    sections = prop_state(prop_collection(
      WriteupSection,
      description = "Subsections in reading order: Methods first, then Results."
    )),
    references = prop_state(prop_table(
      columns = list(
        key = prop_string(
          description = "Citation key, as it appears in {ref:} tokens."
        ),
        package = prop_string(
          NULL,
          nullable = TRUE,
          description = "Software package cited. Unset for a publication describing a method."
        ),
        citation = prop_string(description = "Formatted reference.")
      ),
      description = "One row per cited work, in order of first citation."
    )),
    hyperparameters = prop_state(prop_table(
      columns = list(
        name = prop_string(description = "Hyperparameter name."),
        main = prop_boolean(
          FALSE,
          description = "Whether the main hyperparameter table lists it: a primary hyperparameter of the algorithm (or one named by include_hyperparameters in their place), one that was tuned or specified, or run state selected during fitting."
        ),
        applies = prop_boolean(
          TRUE,
          description = "Whether it had an effect on at least one fit, under the values of the hyperparameters that gate it and the algorithm's rules for its backend. One that had none is named in the supplement table's caption and has no value."
        ),
        value = prop_string(
          description = "Value cell: a template whose values are tokens naming rows of the values table. Where resamples differ, it lists each distinct value with the number of resamples that used it (and how it was chosen, when that differs), and the number of resamples in which the hyperparameter had no effect."
        ),
        tried = prop_string(
          description = "Values the configurations evaluated in tuning gave it, read from the tuning grid, as a template of tokens; an unset alternative reads unset. Empty when the evaluated configurations did not vary it."
        ),
        source = prop_string(
          enum = WRITEUP_HP_SOURCES,
          description = "How the value was chosen: 'specified' by the user; 'default' the declared default; 'resolved' during fitting, from the data or by the backend; 'tuned' by tuning; 'unset' left unset, the value cell stating what that means; 'varied' differently in different resamples, the value cell stating how for each value."
        )
      ),
      description = "One row per hyperparameter of the model, in declaration order: the rows of the main and supplementary hyperparameter tables."
    )),
    primary_listed = prop_boolean(
      TRUE,
      description = "Whether the main table lists the algorithm's primary hyperparameters. False when the writeup was asked for other hyperparameters (include_hyperparameters) or for none."
    ),
    include_hyperparameters = prop_string(
      NULL,
      vector = TRUE,
      nullable = TRUE,
      description = "Hyperparameters the main table lists in place of the algorithm's primary hyperparameters, as the writeup was asked. Unset when it lists the primary hyperparameters or none beside those tuned or specified and the values selected during fitting."
    ),
    not_reported = prop_string(
      "",
      vector = TRUE,
      description = "Information a Methods section usually states that the model does not record, for the author to add."
    )
  ),
  constructor = function(
    algorithm,
    type,
    review,
    values,
    sections,
    references,
    hyperparameters,
    primary_listed,
    include_hyperparameters,
    not_reported
  ) {
    new_object(
      S7_object(),
      algorithm = algorithm,
      type = type,
      review = review,
      values = values,
      sections = sections,
      references = references,
      hyperparameters = hyperparameters,
      primary_listed = primary_listed,
      include_hyperparameters = include_hyperparameters,
      not_reported = not_reported
    )
  },
  publication = SchemaPublication(
    role = "document",
    slug = "supervisedwriteup",
    title = "rtemis SupervisedWriteup",
    description = "Methods and Results sections describing a trained supervised model. Paragraphs are templates whose numbers are tokens naming rows of a values table, each with its formatted text and the field it was read from, and whose citations name rows of a references table; the review the writeup draws on and the information the model does not record are included.",
    order = 33L,
    kind = "report",
    scope = "shared"
  )
) # /rtemis::SupervisedWriteup


# %% fmt_writeup_value ----
#' Format a writeup value by kind
#'
#' @param value Numeric scalar, or Character for kind "text".
#' @param kind Character: One of `WRITEUP_VALUE_KINDS`.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fmt_writeup_value <- function(value, kind) {
  switch(
    kind,
    count = formatC(value, format = "d", big.mark = ","),
    integer = formatC(value, format = "d"),
    exact = trimws(formatC(value, digits = 15L, format = "g")),
    metric = sprintf("%.3f", value),
    quantity = trimws(formatC(signif(value, 3L), digits = 3L, format = "fg")),
    p_value = if (value < 0.001) "p < 0.001" else sprintf("p = %.3f", value),
    percent = sub("\\.0$", "", sprintf("%.1f", 100 * value)),
    text = as.character(value)
  )
} # /rtemis::fmt_writeup_value


# %% render_writeup_template ----
#' Replace the tokens of a paragraph template
#'
#' @param template Character scalar: Paragraph template.
#' @param values data.frame: Values table.
#' @param ref_numbers Named integer: Reference number by citation key.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
render_writeup_template <- function(template, values, ref_numbers) {
  text <- template
  keys <- regmatches(text, gregexpr(WRITEUP_VALUE_TOKEN, text))[[1L]]
  for (token in unique(keys)) {
    key <- substr(token, 2L, nchar(token) - 1L)
    idx <- match(key, values[["key"]])
    if (is.na(idx)) {
      rtemis.core::abort(
        "Template token {",
        key,
        "} names no row of the values table.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    text <- gsub(token, values[["text"]][[idx]], text, fixed = TRUE)
  }
  refs <- regmatches(text, gregexpr(WRITEUP_REF_TOKEN, text))[[1L]]
  for (token in unique(refs)) {
    keys <- strsplit(substr(token, 6L, nchar(token) - 1L), ",", fixed = TRUE)[[
      1L
    ]]
    numbers <- ref_numbers[trimws(keys)]
    if (anyNA(numbers)) {
      rtemis.core::abort(
        "Citation token ",
        token,
        " names a key with no reference.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    text <- gsub(
      token,
      paste0("[", paste(sort(numbers), collapse = ", "), "]"),
      text,
      fixed = TRUE
    )
  }
  text
} # /rtemis::render_writeup_template


# %% writeup_ref_numbers ----
#' Number references in order of first citation
#'
#' @param x `SupervisedWriteup` object.
#'
#' @return Named integer: reference number by key, in the order of
#'   `x@references`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_ref_numbers <- function(x) {
  stats::setNames(seq_len(NROW(x@references)), x@references[["key"]])
} # /rtemis::writeup_ref_numbers


# %% writeup_table_rows ----
#' The performance table as rows of text
#'
#' One row per metric of the review: training and test values, with the test
#' interval for a single split, or mean (SD) over resamples. Every cell is a
#' row of the values table, keyed `table_<column>_<metric>`.
#'
#' @param x `SupervisedWriteup` object.
#'
#' @return Character matrix: header row first.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_table_rows <- function(x) {
  metrics <- x@review@performance[["metric"]]
  resampled <- !is.null(x@review@sample[["n_resamples"]])
  text <- function(key) {
    idx <- match(key, x@values[["key"]])
    if (is.na(idx)) "" else x@values[["text"]][[idx]]
  }
  cell <- function(column, metric) text(paste0("table_", column, "_", metric))
  has_test <- any(nzchar(vapply(metrics, cell, character(1L), column = "test")))
  mean_sd <- function(column, metric) {
    m <- cell(column, metric)
    s <- cell(paste0(column, "_sd"), metric)
    if (!nzchar(m) || !nzchar(s)) m else paste0(m, " (", s, ")")
  }
  header <- if (resampled) {
    c("Metric", "Training, mean (SD)", if (has_test) "Test, mean (SD)")
  } else {
    c(
      "Metric",
      "Training",
      if (has_test) c("Test", paste0(text("confidence_percent"), "% CI"))
    )
  }
  rows <- lapply(metrics, function(metric) {
    if (resampled) {
      c(
        label_metrics(metric),
        mean_sd("training", metric),
        if (has_test) mean_sd("test", metric)
      )
    } else {
      lower <- cell("lower", metric)
      upper <- cell("upper", metric)
      c(
        label_metrics(metric),
        cell("training", metric),
        if (has_test) {
          c(
            cell("test", metric),
            if (nzchar(lower) && nzchar(upper)) {
              paste0(lower, "\u2013", upper)
            } else {
              ""
            }
          )
        }
      )
    }
  })
  do.call(rbind, c(list(header), rows))
} # /rtemis::writeup_table_rows


# %% writeup_table_caption ----
writeup_table_caption <- function(x) {
  text <- function(key) x@values[["text"]][match(key, x@values[["key"]])]
  resampled <- !is.null(x@review@sample[["n_resamples"]])
  paste0(
    "Table ",
    text("table_performance"),
    ". Performance on ",
    if (is.null(x@review@sample[["n_test"]])) {
      "the training cases"
    } else {
      "training and test cases"
    },
    if (resampled) {
      paste0(", mean (SD) over ", text("n_resamples"), " resamples")
    },
    "."
  )
} # /rtemis::writeup_table_caption


# %% writeup_hp_table_rows ----
#' A hyperparameter table as rows of text
#'
#' @param x `SupervisedWriteup` object.
#' @param main Logical: The main table (TRUE) or the supplementary table.
#'
#' @return Character matrix, header row first, or NULL when the table has no
#'   rows. A value left unset reads "unset" in its cell, and attribute `notes`
#'   holds what that means for each such hyperparameter, as "name: meaning".
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_hp_table_rows <- function(x, main = TRUE) {
  rows <- x@hyperparameters
  if (NROW(rows) == 0L) {
    return(NULL)
  }
  keep <- rows[["applies"]] & (!main | rows[["main"]])
  rows <- rows[keep, , drop = FALSE]
  if (NROW(rows) == 0L) {
    return(NULL)
  }
  ref_numbers <- writeup_ref_numbers(x)
  render <- function(cells) {
    vapply(
      cells,
      render_writeup_template,
      character(1L),
      values = x@values,
      ref_numbers = ref_numbers,
      USE.NAMES = FALSE
    )
  }
  has_tried <- any(nzchar(rows[["tried"]]))
  values <- render(rows[["value"]])
  meaning <- x@values[["text"]][match(
    writeup_key("table_hp", rows[["name"]], "meaning"),
    x@values[["key"]]
  )]
  unset <- !is.na(meaning)
  notes <- if (any(unset)) {
    paste0(rows[["name"]][unset], ": ", meaning[unset])
  } else {
    character()
  }
  header <- c(
    "Hyperparameter",
    "Value",
    if (!main) "Chosen by",
    if (has_tried) "Values tried"
  )
  body <- cbind(
    rows[["name"]],
    values,
    if (!main) {
      ifelse(
        rows[["source"]] == "unset",
        "",
        unname(WRITEUP_HP_SOURCE_LABELS[rows[["source"]]])
      )
    },
    if (has_tried) render(rows[["tried"]])
  )
  out <- rbind(header, body, deparse.level = 0L)
  attr(out, "notes") <- notes
  out
} # /rtemis::writeup_hp_table_rows


# %% writeup_hp_table_caption ----
writeup_hp_table_caption <- function(x, main = TRUE) {
  text <- function(key) x@values[["text"]][match(key, x@values[["key"]])]
  resampled <- !is.null(x@review@sample[["n_resamples"]])
  rows <- x@hyperparameters
  per_resample <- if (resampled) {
    " Where resamples differ, each value is listed with the number of resamples that used it, and resamples in which a hyperparameter had no effect are counted as not applicable."
  }
  if (main) {
    scope <- writeup_hp_main_scope(
      if (x@primary_listed) NULL else x@include_hyperparameters %||% character()
    )
    return(paste0(
      "Table ",
      text("table_hyperparameters"),
      ". ",
      toupper(substr(scope, 1L, 1L)),
      substr(scope, 2L, nchar(scope)),
      ".",
      per_resample
    ))
  }
  omitted <- rows[["name"]][!rows[["applies"]]]
  paste0(
    "Table S",
    text("table_hyperparameters_supplement"),
    ". Every hyperparameter that applied to the fit, its value, how it was chosen and the values tuning evaluated.",
    per_resample,
    if (length(omitted) > 0L) {
      paste0(
        " Not applicable under this configuration: ",
        paste(omitted, collapse = ", "),
        "."
      )
    }
  )
} # /rtemis::writeup_hp_table_caption


# %% writeup_md_table ----
#' A hyperparameter table as Markdown lines
#'
#' @param x `SupervisedWriteup` object.
#' @param rows Character matrix from `writeup_hp_table_rows()`, or NULL.
#' @param main Logical: The main table (TRUE) or the supplementary table.
#'
#' @return Character vector of lines, empty when `rows` is NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_md_table <- function(x, rows, main) {
  if (is.null(rows)) {
    return(character())
  }
  md_row <- function(cells) {
    paste0(
      "| ",
      paste(gsub("|", "\\|", cells, fixed = TRUE), collapse = " | "),
      " |"
    )
  }
  c(
    "",
    writeup_hp_table_caption(x, main),
    "",
    md_row(rows[1L, ]),
    paste0("|", paste(rep("---", NCOL(rows)), collapse = "|"), "|"),
    vapply(
      seq_len(NROW(rows))[-1L],
      function(i) md_row(rows[i, ]),
      character(1L)
    ),
    if (length(attr(rows, "notes")) > 0L) {
      c("", paste0("- ", attr(rows, "notes")))
    }
  )
} # /rtemis::writeup_md_table


# %% writeup_markdown ----
#' Render a writeup as Markdown
#'
#' @param x `SupervisedWriteup` object.
#'
#' @return Character scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
writeup_markdown <- function(x) {
  ref_numbers <- writeup_ref_numbers(x)
  md_row <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")
  out <- character()
  for (part in WRITEUP_PARTS) {
    out <- c(
      out,
      paste0("## ", if (part == "methods") "Methods" else "Results")
    )
    for (s in x@sections) {
      if (s@part != part) {
        next
      }
      out <- c(out, "", paste0("### ", s@heading), "")
      out <- c(
        out,
        paste(
          vapply(
            s@paragraphs,
            render_writeup_template,
            character(1L),
            values = x@values,
            ref_numbers = ref_numbers
          ),
          collapse = "\n\n"
        )
      )
    }
    if (part == "methods") {
      out <- c(out, writeup_md_table(x, writeup_hp_table_rows(x), TRUE))
    }
    if (part == "results") {
      rows <- writeup_table_rows(x)
      out <- c(
        out,
        "",
        writeup_table_caption(x),
        "",
        md_row(rows[1L, ]),
        paste0("|", paste(rep("---", NCOL(rows)), collapse = "|"), "|"),
        vapply(
          seq_len(NROW(rows))[-1L],
          function(i) md_row(rows[i, ]),
          character(1L)
        )
      )
    }
    out <- c(out, "")
  }
  out <- c(
    out,
    "## References",
    "",
    paste0(ref_numbers, ". ", x@references[["citation"]])
  )
  supplement <- writeup_hp_table_rows(x, main = FALSE)
  if (!is.null(supplement)) {
    out <- c(
      out,
      "",
      "## Supplementary material",
      writeup_md_table(x, supplement, FALSE)
    )
  }
  if (length(x@not_reported) > 0L && any(nzchar(x@not_reported))) {
    out <- c(
      out,
      "",
      "## Not recorded by the model",
      "",
      paste0("- ", x@not_reported[nzchar(x@not_reported)])
    )
  }
  paste0(paste(out, collapse = "\n"), "\n")
} # /rtemis::writeup_markdown


# %% repr.SupervisedWriteup ----
#' repr SupervisedWriteup
#'
#' The rendered sections, the performance table, the references and the
#' information the model does not record.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, SupervisedWriteup) <- function(x, pad = 0L, output_type = NULL) {
  indent <- strrep(" ", pad + 2L)
  width <- max(40L, min(getOption("width", 80L), 100L) - nchar(indent) - 2L)
  ref_numbers <- writeup_ref_numbers(x)
  heading <- function(text, level = 1L) {
    paste0(
      "\n",
      indent,
      if (level == 2L) "  ",
      fmt(text, bold = TRUE, output_type = output_type),
      "\n"
    )
  }
  wrap <- function(text, extra = "  ") {
    paste0(
      paste0(indent, extra, strwrap(text, width = width), collapse = "\n"),
      "\n"
    )
  }
  hp_table <- function(main) {
    rows <- writeup_hp_table_rows(x, main)
    if (is.null(rows)) {
      return("")
    }
    notes <- attr(rows, "notes")
    paste0(
      "\n",
      wrap(writeup_hp_table_caption(x, main), "    "),
      paste(
        review_text_table(rows, paste0(indent, "  "), align_left = TRUE),
        collapse = "\n"
      ),
      "\n",
      if (length(notes) > 0L) {
        paste0(
          "\n",
          paste(vapply(notes, wrap, character(1L), "    "), collapse = "")
        )
      }
    )
  }
  out <- repr_S7name("SupervisedWriteup", pad = pad, output_type = output_type)
  for (part in WRITEUP_PARTS) {
    out <- paste0(
      out,
      heading(if (part == "methods") "Methods" else "Results")
    )
    for (s in x@sections) {
      if (s@part != part) {
        next
      }
      out <- paste0(out, heading(s@heading, level = 2L))
      for (p in s@paragraphs) {
        out <- paste0(
          out,
          wrap(render_writeup_template(p, x@values, ref_numbers), "    ")
        )
      }
    }
    if (part == "methods") {
      out <- paste0(out, hp_table(TRUE))
    }
    if (part == "results") {
      out <- paste0(
        out,
        "\n",
        wrap(writeup_table_caption(x), "    "),
        paste(
          review_text_table(writeup_table_rows(x), paste0(indent, "  ")),
          collapse = "\n"
        ),
        "\n"
      )
    }
  }
  out <- paste0(out, heading("References"))
  for (i in seq_len(NROW(x@references))) {
    out <- paste0(
      out,
      wrap(paste0("[", i, "] ", x@references[["citation"]][[i]]), "  ")
    )
  }
  if (!is.null(writeup_hp_table_rows(x, main = FALSE))) {
    out <- paste0(out, heading("Supplementary material"), hp_table(FALSE))
  }
  if (length(x@not_reported) > 0L && any(nzchar(x@not_reported))) {
    out <- paste0(out, heading("Not recorded by the model"))
    for (item in x@not_reported) {
      out <- paste0(out, wrap(paste0("- ", item), "  "))
    }
  }
  out
} # /rtemis::repr.SupervisedWriteup


# %% print.SupervisedWriteup ----
#' Print `SupervisedWriteup`
#'
#' @param x `SupervisedWriteup` object.
#' @param pad Integer: Left padding.
#' @param output_type Optional Character: Output format.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, SupervisedWriteup) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.SupervisedWriteup


# %% write_writeup ----
#' Write a writeup to a Markdown file
#'
#' @description
#' Writes the Methods and Results sections of a [writeup], the performance
#' table and the numbered references as Markdown, ready to convert with Pandoc
#' or Quarto or to paste into a manuscript.
#'
#' @param x `SupervisedWriteup` object, as returned by [writeup].
#' @param file Character: Path of the Markdown file to write.
#' @param overwrite Logical: If TRUE, replace an existing file.
#' @param verbosity Integer: Verbosity level.
#'
#' @return The path of the file written, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' idx <- c(1:40, 51:90, 101:140)
#' mod <- train(
#'   iris[idx, ],
#'   dat_test = iris[-idx, ],
#'   hyperparameters = setup_CART(),
#'   verbosity = 0L
#' )
#' path <- file.path(tempdir(), "writeup.md")
#' write_writeup(writeup(mod), path)
write_writeup <- function(x, file, overwrite = FALSE, verbosity = 1L) {
  check_is_S7(x, SupervisedWriteup)
  rtemis.core::check_character_scalar(file)
  rtemis.core::check_logical_scalar(overwrite)
  if (file.exists(file) && !overwrite) {
    rtemis.core::abort(
      "File ",
      file,
      " exists. Set `overwrite = TRUE` to replace it.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  writeLines(writeup_markdown(x), file, sep = "", useBytes = FALSE)
  if (verbosity > 0L) {
    msg0("Wrote writeup to ", file, ".")
  }
  invisible(file)
} # /rtemis::write_writeup
