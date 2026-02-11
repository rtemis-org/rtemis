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
#' \dontrun{
#' # Binary classification where "A" is the positive class, so .1 => B, .5 & .9 => A
#' prob2categorical(c(.1, .5, .9), c("A", "B"), 1)
#' # Binary classification where "B" is the positive class, so .1 => A, .5 & .9 => B
#' prob2categorical(c(.1, .5, .9), c("A", "B"), 2)
#' # Multi-class classification
#' prob <- matrix(c(.1, .3, .6, .05, .6, .35, .4, .3, .3), nrow = 3, byrow = TRUE)
#' prob2categorical(prob, c("A", "B", "C"))
#' }
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
    cli::cli_abort("y is missing")
  }
}

#' Move outcome to last column
#'
#' @param dat data.frame or similar.
#' @param outcome_column Character: Name of outcome column.
#'
#' @return object of same class as `data`
#'
#' @author EDG
#' @export
#'
#' @examples
#' ir <- set_outcome(iris, "Sepal.Length")
#' head(ir)
set_outcome <- function(dat, outcome_column) {
  # Get index of outcome column
  id <- grep(outcome_column, names(dat))
  # Check
  if (length(id) == 0) {
    cli::cli_abort('Column "', outcome_column, '" not found in data.')
  }
  # Reorder columns
  # => Make S7 generic
  if (is.data.table(dat)) {
    dat[, c(setdiff(seq_len(NCOL(dat)), id), id), with = FALSE]
  } else {
    dat[, c(setdiff(seq_len(NCOL(dat)), id), id)]
  }
} # /rtemis::set_outcome


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
#' @param include_anova Integer vector {1, 3}: Output ANOVA I and/or III p-vals. NA to not.
#' @param info Logical: If TRUE, warn when values < than machine eps are replaced by
#' machine eps
#'
#' @return `data.table` with glm summaries
#' @author EDG
#'
#' @keywords internal
#' @noRd

glm2table <- function(x, xnames = NULL, include_anova = NA, info = TRUE) {
  if (is.null(xnames)) {
    xnames <- if (!is.null(names(x))) {
      names(x)
    } else {
      paste0("Variable_", seq_along(x))
    }
  }

  if (!is.na(include_anova)) {
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

  if (1 %in% include_anova) {
    pvals2 <- t(sapply(x, \(i) car::Anova(i, type = 2)[, 3]))
    colnames(pvals2) <- paste(
      "p_value type II",
      x[[1]] |> terms() |> attr("term.labels")
    )
    out <- cbind(out, pvals2)
  }

  if (3 %in% include_anova) {
    pvals3 <- t(sapply(x, \(i) car::Anova(i, type = 3)[, 3]))
    colnames(pvals3) <- paste(
      "p_value type III",
      x[[1]] |> terms() |> attr("term.labels")
    )
    out <- cbind(out, pvals3)
  }

  out
} # /rtemis::glm2table


#' Collect summary table (p-values) from list of massGAMs with same predictors,
#' different outcome ("massy")
#'
#' @param x list of [mgcv::gam] models
#' @param xnames Character, vector: names of models
#' @param include_anova Integer: 1 or 3; to output ANOVA I or III p-vals. NA to not
#'
#' @return `data.table` with glm summaries
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
}

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
  if (length(lteps) > 0) {
    if (warn) {
      warning("Values < machine double eps converted to double eps")
    }
    pvals[lteps] <- eps
  }
  pvals
} # /rtemis::get_gam_pvals


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
#'
#' @examples
#' # iris is perfectly balanced
#' class_imbalance(iris[["Species"]])
#' # Simulate imbalanced outcome
#' x <- factor(sample(c("A", "B"), size = 500L, replace = TRUE, prob = c(0.9, 0.1)))
#' class_imbalance(x)
class_imbalance <- function(x) {
  if (!is.factor(x)) {
    cli::cli_abort("Input must be a factor")
  }
  K <- nlevels(x)
  N <- length(x)
  freq <- as.data.frame(table(x))

  K * sum(sapply(seq(K), function(i) (freq[["Freq"]][i] / N - 1 / K)^2))
} # /rtemis::class_imbalance


# expand_grid.R
# ::rtemis::
# 2025 EDG rtemis.org

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
