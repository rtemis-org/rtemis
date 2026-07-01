# super_ops.R
# ::rtemis::
# 2024- EDG rtemis.org

supervised_type <- function(dat) {
  if (is.factor(outcome(dat))) {
    "Classification"
  } else {
    "Regression"
  }
} # /rtemis::supervised_type

#' Convert probabilities to categorical (factor)
#'
#' @param x Numeric vector: Probabilities
#' @param levels Character vector: Class labels
#' @param binclasspos Integer: Index of the positive class for binary classification
#'
#' @return Factor
#' @author EDG
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' # Binary classification where "A" is the positive class, so .1 => B, .5 & .9 => A
#' prob2categorical(c(.1, .5, .9), c("A", "B"), 1)
#' # Binary classification where "B" is the positive class, so .1 => A, .5 & .9 => B
#' prob2categorical(c(.1, .5, .9), c("A", "B"), 2)
#' # Multi-class classification
#' prob <- matrix(c(.1, .3, .6, .05, .6, .35, .4, .3, .3), nrow = 3, byrow = TRUE)
#' prob2categorical(prob, c("A", "B", "C"))
prob2categorical <- function(x, levels, binclasspos = 2L) {
  n_classes <- length(levels)
  if (n_classes == 2) {
    # Binary classification
    stopifnot(binclasspos %in% c(1, 2))
    if (binclasspos == 1L) {
      levels <- rev(levels)
    }
    fitted <- factor(
      ifelse(x >= .5, 1, 0),
      levels = c(0, 1),
      labels = levels
    )
  } else {
    # Multi-class classification
    stopifnot(length(levels) == ncol(x))
    fitted <- factor(
      apply(x, 1, which.max),
      levels = seq_len(n_classes),
      labels = levels
    )
  }
  fitted
} # /rtemis::prob2categorical


#' @keywords internal
#' @noRd
check_supervised_inputs <- function(x, y = NULL) {
  if (is.null(y) && NCOL(x) < 2) {
    rtemis.core::abort(
      "y is missing.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
}

#' Move outcome to last column
#'
#' @param dat tabular dataset.
#' @param outcome_column Character: Name of outcome column.
#'
#' @return object of same class as `data`
#'
#' @author EDG
#' @export
#' @examples
#' ir <- set_outcome(iris, "Sepal.Length")
#' head(ir)
set_outcome <- function(dat, outcome_column) {
  # Get index of outcome column
  id <- grep(outcome_column, names(dat))
  # Check
  if (length(id) == 0) {
    rtemis.core::abort(
      'Column "',
      outcome_column,
      '" not found in data.',
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  # Reorder columns
  # => Make S7 generic
  if (is.data.table(dat)) {
    dat[, c(setdiff(seq_len(NCOL(dat)), id), id), with = FALSE]
  } else {
    dat[, c(setdiff(seq_len(NCOL(dat)), id), id)]
  }
} # /rtemis::set_outcome


#' Set positive class
#'
#' Checks and sets the positive class for binary classification in a tabular dataset.
#'
#' @param x Tabular data, i.e. data.frame, data.table, or tbl_df (tibble): Input dataset. The last column will be treated as the outcome variable.
#' @param positive_class Character scalar: The name of the positive class. Must be one of the levels of the outcome variable.
#'
#' @return Tabular data of the same class as `x`, with the positive class set as the second level of the outcome factor.
#' @author EDG
#' @export
#' @examples
#' dat <- data.frame(x = rnorm(100), y = factor(sample(c("Case", "Control"), 100, replace = TRUE)))
#' levels(dat[["y"]])
#' dat <- set_positive_class(dat, "Case")
#' levels(dat[["y"]])
set_positive_class <- function(x, positive_class) {
  # Check outcome is factor with 2 levels and positive_class is one of them
  if (!is.factor(outcome(x))) {
    rtemis.core::abort(
      "You defined `positive_class`, but the outcome is not a factor.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (nlevels(outcome(x)) != 2L) {
    rtemis.core::abort(
      "You defined `positive_class`, but the outcome does not have 2 levels.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!positive_class %in% levels(outcome(x))) {
    rtemis.core::abort(
      "You defined `positive_class` as '",
      positive_class,
      "', but the outcome levels are: ",
      paste(levels(outcome(x)), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (positive_class == levels(outcome(x))[2L]) {
    # Already in correct order
    return(x)
  }
  # Reorder factor levels to put positive_class second
  if (is.data.table(x)) {
    set(
      x,
      j = outcome_name(x),
      value = factor(
        outcome(x),
        levels = c(
          setdiff(levels(outcome(x)), positive_class),
          positive_class
        )
      )
    )
  } else {
    x[[outcome_name(x)]] <- factor(
      outcome(x),
      levels = c(setdiff(levels(outcome(x)), positive_class), positive_class)
    )
    x
  }
} # /rtemis::set_positive_class


#' Make formula
#'
#' Makes a formula from a data.frame assuming the last column is the outcome
#'
#' @param x data.frame
#'
#' @return character
#' @author EDG
#'
#' @keywords internal
#' @noRd
make_formula <- function(x, output = "character") {
  outcome <- names(x)[NCOL(x)]
  out <- paste(outcome, "~ .")
  if (output == "formula") {
    as.formula(out, env = parent.env(parent.frame()))
  } else {
    out
  }
} # /rtemis::make_formula


# glm2table.R
# ::rtemis::
# 2021 EDG rtemis.org

#' Collect summary table from list of massGLMs with same predictors, different outcome
#' ("mass-y")
#'
#' @param x list of [glm] models
#' @param xnames Character, vector: names of models
#' @param include_anova Integer vector {1, 2, 3}: Output ANOVA Type I, II, and/or III
#' p-vals. Type I uses base R `anova()` (sequential); Types II and III use `car::Anova()`.
#' NA to skip.
#'
#' @return `data.table` with glm summaries
#' @author EDG
#'
#' @keywords internal
#' @noRd

glm2table <- function(x, xnames = NULL, include_anova = NA) {
  if (is.null(xnames)) {
    xnames <- if (!is.null(names(x))) {
      names(x)
    } else {
      paste0("Variable_", seq_along(x))
    }
  }

  if (any(c(2L, 3L) %in% include_anova)) {
    check_dependencies("car")
  }

  out <- data.table(
    Variable = xnames,
    do.call(
      rbind,
      c(lapply(x, function(l) {
        out <- t(coef(summary(l))[-1, , drop = FALSE])
        varnames <- gsub(".*\\$", "", colnames(out))
        parnames <- c("Coefficient_", "SE_", "t_value_", "p_value_")
        out <- c(out)
        names(out) <- c(outer(parnames, varnames, paste0))
        out
      }))
    )
  )

  # Convert p-vals equal to 0 to machine double eps
  # eps <- .Machine[["double.eps"]]
  # pvals_idc <- getnames(out, starts_with = "p_value")
  # # appease R CMD check:, use with = FALSE, not ..i
  # for (i in pvals_idc) {
  #   lteps <- out[, i, with = FALSE] < eps
  #   if (length(lteps) > 0) {
  #     if (info) {
  #       cli::cli_inform("Values < machine double eps converted to double eps")
  #     }
  #     out[, i, with = FALSE][lteps] <- eps
  #   }
  # }

  term_labels <- x[[1]] |>
    terms() |>
    attr("term.labels")

  if (1 %in% include_anova) {
    pvals1 <- t(sapply(
      x,
      \(i) anova(i, test = "F")[seq_along(term_labels), 5]
    ))
    colnames(pvals1) <- paste(
      "p_value type I",
      term_labels
    )
    out <- cbind(out, pvals1)
  }

  if (2 %in% include_anova) {
    pvals2 <- t(sapply(
      x,
      \(i) car::Anova(i, type = 2)[seq_along(term_labels), 3]
    ))
    colnames(pvals2) <- paste(
      "p_value type II",
      term_labels
    )
    out <- cbind(out, pvals2)
  }

  if (3 %in% include_anova) {
    pvals3 <- t(sapply(
      x,
      \(i) car::Anova(i, type = 3)[seq_along(term_labels) + 1, 3]
    ))
    colnames(pvals3) <- paste(
      "p_value type III",
      term_labels
    )
    out <- cbind(out, pvals3)
  }

  out
} # /rtemis::glm2table


#' Collect summary table (p-values) from list of massGAMs with same predictors,
#' different outcome ("massy")
#'
#' @param mods list of [mgcv::gam] models.
#' @param modnames Character, vector: names of models.
#'
#' @return `data.table` with GAM p-value summaries.
#' @author EDG
#'
#' @keywords internal
#' @noRd
gam2table <- function(mods, modnames = NULL) {
  if (is.null(modnames)) {
    modnames <- if (!is.null(names(mods))) {
      names(mods)
    } else {
      paste0("Model_", seq_along(mods))
    }
  }

  out <- data.table(
    Variable = modnames,
    do.call(
      rbind,
      c(lapply(mods, get_gam_pvals))
    )
  )
  setnames(out, names(out)[-1], paste("p_value", names(out)[-1]))
  out
} # /rtemis::gam2table


#' Get GAM model's p-values for parametric and spline terms
#'
#' @keywords internal
#' @noRd
get_gam_pvals <- function(m, warn = TRUE) {
  eps <- .Machine[["double.eps"]]
  ms <- summary(m)
  pvals <- cbind(
    # s terms
    as.data.frame(t(ms[["s.table"]][, 4])),
    # p terms
    as.data.frame(t(ms[["p.table"]][, 4]))[-1]
  )
  lteps <- pvals < eps
  if (any(lteps)) {
    if (warn) {
      warning("Values < machine double eps converted to double eps")
    }
    pvals[lteps] <- eps
  }
  pvals
} # rtemis::get_gam_pvals


#' Class Imbalance
#'
#' Calculate class imbalance as given by:
#' \deqn{I = K\cdot\sum_{i=1}^K (n_i/N - 1/K)^2}{I = K * sum(n_i/N - 1/K)^2}
#' where \eqn{K} is the number of classes, and \eqn{n_i} is the number of
#' instances of class \eqn{i}
#'
#' @param x Vector, factor: Outcome.
#'
#' @return Numeric.
#'
#' @author EDG
#' @export
#' @examples
#' # iris is perfectly balanced
#' class_imbalance(iris[["Species"]])
#' # Simulate imbalanced outcome
#' x <- factor(sample(c("A", "B"), size = 500L, replace = TRUE, prob = c(0.9, 0.1)))
#' class_imbalance(x)
class_imbalance <- function(x) {
  if (!is.factor(x)) {
    rtemis.core::abort(
      "Input must be a factor.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  K <- nlevels(x)
  N <- length(x)
  freq <- as.data.frame(table(x))

  K * sum(sapply(seq(K), function(i) (freq[["Freq"]][i] / N - 1 / K)^2))
} # /rtemis::class_imbalance


# expand_grid.R
# ::rtemis::
# 2025- EDG rtemis.org

#' Expand Grid
#'
#' Expand grid, converting NULL values to "null"
#'
#' Since the "null" characters in the resulting data.frame cannot be replaced to NULL,
#' they have to be converted back to NULL as needed downstream.
#' So make sure your data does not have cheeky character vector with "null" values in it that are
#' not actually NULLs.
#'
#' @param x named list
#'
#' @return data.frame
#'
#' @author EDG
#' @keywords internal
#' @noRd
#'
#' @examples
#' x <- list(a = c(1, 2, 3), b = NULL, c = c("z", "v"))
#' expand_grid(x)
expand_grid <- function(x, stringsAsFactors = FALSE) {
  stopifnot(is.list(x))
  # Convert all NULL to "null"
  x <- lapply(x, function(e) if (is.null(e)) "null" else e)
  # Expand grid
  expand.grid(x, stringsAsFactors = stringsAsFactors)
} # /expand_grid
