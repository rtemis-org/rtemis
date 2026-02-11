# utils_rules.R
# ::rtemis::
# EDG rtemis.org

#' Match Rules to Cases
#'
#' @param x Matrix / data frame: Input features
#' @param rules Character vector: Rules
#' @param verbosity Integer: Verbosity level.
#'
#' @return cases-by-rules matrix (binary; 1: match, 0: no match)
#' @author EDG
#'
#' @keywords internal
#' @noRd
match_cases_by_rules <- function(x, rules, prefix = "Rule_", verbosity = 1L) {
  n_cases <- NROW(x)
  n_rules <- length(rules)
  if (!is.data.table(x)) {
    # {data.table}
    x <- data.table::as.data.table(x)
  } else {
    # Either make copy, or set ID to NULL before exit
    # x <- copy(x)
    on.exit(x[, ID := NULL])
  }
  # appease R CMD check
  ID <- NULL
  x[, ID := seq_len(n_cases)]
  cxr <- matrix(0, n_cases, n_rules)
  if (verbosity > 0L) {
    msgstart(
      "Matching",
      highlight(n_rules),
      "rules to",
      highlight(n_cases),
      "cases..."
    )
  }
  for (i in seq_along(rules)) {
    match <- x[eval(parse(text = rules[i])), ID]
    cxr[match, i] <- 1
  }
  if (!is.null(prefix)) {
    colnames(cxr) <- paste0(prefix, seq_len(n_rules))
  }
  if (verbosity > 0L) {
    msgdone()
  }
  cxr
} # /rtemis::match_cases_by_rules


#' Index cases by rules
#'
#' Get an index of which cases match which rule - meant for cases where each case matches one rule
#' and one rule only
#'
#' @inheritParams match_cases_by_rules
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
index_cases_by_rules <- function(x, rules, verbosity = 1L) {
  cxr <- match_cases_by_rules(x, rules, verbosity)
  apply(cxr, 1, \(i) which(i == 1))
}


#' Prune a rule to a maximum length
#'
#' @param rule Character: A rule.
#' @param max_length Integer: The maximum number of conditions to keep.
#' @param sep Character: The separator between conditions.
#'
#' @return Character: The pruned rule.
#' @author EDG
#'
#' @keywords internal
#' @noRd
simple_prune_ <- function(rule, max_length, sep = " & ") {
  conditions <- strsplit(rule, sep)[[1]]
  if (length(conditions) > max_length) {
    conditions <- conditions[1:max_length]
    paste(conditions, collapse = sep)
  } else {
    rule
  }
} # /rtemis::simple_prune_


#' Prune rules to a maximum length
#'
#' @param rule Character vector: Rules.
#' @param max_length Integer: The maximum number of conditions to keep.
#' @param sep Character: The separator between conditions.
#'
#' @return Character: The pruned rule.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
simple_prune <- function(rules, max_length, sep = " & ") {
  rules <- sapply(
    rules,
    simple_prune_,
    max_length = max_length,
    sep = sep,
    USE.NAMES = FALSE
  )
  rules
} # /rtemis::simple_prune


#' Extract variable names from rules
#'
#' @param rules Character vector: Rules.
#' @param unique Logical: If TRUE, return only unique variables.
#'
#' @return Character vector: Variable names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_vars_from_rules <- function(rules, unique = FALSE) {
  # Extract variables from rules
  vars <- unique(unlist(strsplit(rules, " & ")))
  # Get string up to first "<", ">", "=", "!", or "%in%"
  vars <- gsub("(<|>|=|!|%in%).*", "", vars)
  vars <- gsub(" .*", "", vars)
  if (unique) {
    vars <- unique(vars)
  }
  vars
} # /rtemis::get_vars_from_rules

#' Format rules
#'
#' Converts R-executable logical expressions to a more human-friendly format
#'
#' @param x Vector, string: Logical expressions
#' @param space_after_comma Logical: If TRUE, place spaces after commas.
#' @param decimal_places Integer: Limit all floats (numbers of the form 9.9) to this many
#' decimal places
#'
#' @return Character vector: Formatted rules.
#'
#' @author EDG
#' @keywords internal
#' @noRd
format_rules <- function(x, space_after_comma = FALSE, decimal_places = NULL) {
  x <- gsub("[&+]", "AND", x)
  x <- gsub(">", " > ", x)
  x <- gsub("<=", " <= ", x)
  x <- gsub("%in%", "IN", x)
  x <- gsub("c\\(", "{", x)
  x <- gsub("\\)", "}", x)
  x <- gsub("'", "", x)
  if (space_after_comma) {
    x <- gsub(",", ", ", x)
  }
  if (!is.null(decimal_places)) {
    x <- gsubfn::gsubfn(
      "([0-9.]+[0-9])",
      function(i) ddSci(i, decimal_places = decimal_places),
      x,
      engine = "R"
    )
  }
  x
} # /rtemis::format_rules


#' Format LightRuleFit rules
#'
#' Converts R-executable logical expressions to a more human-friendly format
#'
#' @param x Vector, string: Logical expressions
#' @param space_after_comma Logical: If TRUE, place spaces after commas.
#' @param decimal_places Integer: Limit all floats (numbers of the form 9.9) to this many
#' decimal places
#'
#' @return Character vector: Formatted rules.
#'
#' @author EDG
#' @keywords internal
#' @noRd
format_LightRuleFit_rules <- function(
  x,
  space_after_comma = FALSE,
  decimal_places = NULL
) {
  x <- gsub("[&+]", "AND", x)
  x <- gsub(">", " > ", x)
  x <- gsub("<=", " <= ", x)
  x <- gsub("%in%", "IN", x)
  x <- gsub("%notin%", "NOT IN", x)
  x <- gsub("c\\(", "{", x)
  x <- gsub("\\)", "}", x)
  x <- gsub("'", "", x)
  if (space_after_comma) {
    x <- gsub(",", ", ", x)
  }
  if (!is.null(decimal_places)) {
    x <- gsubfn::gsubfn(
      "([0-9.]+[0-9])",
      function(i) ddSci(i, decimal_places = decimal_places),
      x,
      engine = "R"
    )
  }
  gsub("  ", " ", x)
} # /rtemis::format_LightRuleFit_rules


# rules2medmod
# ::rtemis::
# 2018 EDG rtemis.org

#' Convert rules from cutoffs to median/mode and range
#'
#' Convert rules from cutoffs to `median (range)` and `mode (range)` format
#'
#' @param rules Character, vector: Input rules
#' @param x Data frame: Data to evaluate rules
#' @param .ddSci Logical: If TRUE, format all continuous variables using
#' [ddSci], which will give either 2 decimal places, or scientific
#' notation if two decimal places result in 0.00
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character vector.
#'
#' @author EDG
#' @keywords internal
#' @noRd
rules2medmod <- function(rules, x, .ddSci = TRUE, verbosity = 1L) {
  cxr <- match_cases_by_rules(x, rules, verbosity = verbosity)
  nrules <- length(rules)
  rules_f <- vector("character", nrules)
  frmt <- if (.ddSci) ddSci else I
  if (verbosity > 0L) {
    msg("Converting rules...")
  }
  for (i in seq(rules)) {
    if (verbosity > 1L) {
      msg_info("#", i, "/", nrules, "...")
    }
    dat <- x[cxr[, i] == 1, ]
    sub <- strsplit(rules[i], "&")[[1]]
    rule <- character()
    for (j in seq(sub)) {
      categorical <- grepl("%in%", sub[j])
      if (categorical) {
        var <- gsub("\\s", "", strsplit(sub[j], "%in%")[[1]][1])
        vals <- dat[[var]]
        value <- paste0(
          get_mode(vals),
          " (",
          paste(levels(droplevels(vals)), collapse = ", "),
          ")"
        )
        rule[j] <- paste0(var, " = ", value)
      } else {
        sub[j] <- gsub(">|>=|<|<=", "@", sub[j])
        var <- gsub("\\s", "", strsplit(sub[j], "@")[[1]][1])
        vals <- dat[[var]]
        value <- paste0(
          frmt(median(vals)),
          " (",
          frmt(min(vals)),
          ":",
          frmt(max(vals)),
          ")"
        )
        rule[j] <- paste0(var, " = ", value)
      }
    } # /loop through each rule's conditions
    # This consolidates conditions like a > 3 & a > 5 to one
    rules_f[i] <- paste(unique(rule), collapse = " & ")
  } # /loop through rules

  if (verbosity > 0L) {
    msg("Done")
  }
  rules_f
} # /rtemis::rules2medmod
