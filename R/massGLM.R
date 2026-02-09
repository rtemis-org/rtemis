# massGLM.R
# ::rtemis::
# 2021- EDG rtemis.org

#' Mass-univariate GLM Analysis
#'
#' @param x data.frame or similar: Predictor variables. Usually a small number of covariates.
#' @param y data.frame or similar: Each column is a different outcome. The function will train one
#' GLM for each column of `y`. Usually a large number of features.
#' @param scale_y Logical: If TRUE, scale each column of `y` to have mean 0 and sd 1. If `NULL`,
#' defaults to TRUE if `y` is numeric, FALSE otherwise.
#' @param center_y Logical: If TRUE, center each column of `y` to have mean 0. If `NULL`, defaults
#' to TRUE if `scale_y` is TRUE, FALSE otherwise.
# @param include_anova Logical: If TRUE, include ANOVA results in the summary.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `MassGLM` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' set.seed(2022)
#' y <- rnormmat(500, 40, return_df = TRUE)
#' x <- data.frame(
#'   x1 = y[[3]] - y[[5]] + y[[14]] + rnorm(500),
#'   x2 = y[[21]] + rnorm(500)
#' )
#' massmod <- massGLM(x, y)
#' # Print table of coefficients, p-values, etc. for all models
#' summary(massmod)
massGLM <- function(
  x,
  y,
  scale_y = NULL,
  center_y = NULL,
  # include_anova = TRUE,
  verbosity = 1L
) {
  # Init ----
  start_time <- intro(verbosity = verbosity)

  # Check y ----
  # all y columns must be numeric or all factors with 2 levels
  y_class <- sapply(y, class)
  if (y_class[1] == "numeric") {
    # Check all are numeric
    if (!all(y_class == "numeric")) {
      cli::cli_abort(
        "All columns of y must be the same type: either numeric or factors with 2 levels"
      )
    }
    .family <- "gaussian"
  } else if (y_class[1] == "factor") {
    n_levels <- sapply(y, nlevels)
    if (!all(n_levels == 2)) {
      cli::cli_abort("All factor columns of y must have 2 levels")
    }
    .family <- "binomial"
  } else {
    cli::cli_abort(
      "All columns of y must be either numeric or factors with 2 levels. Found: {.val {y_class}}"
    )
  }

  # Preprocessing ----
  if (is.null(scale_y)) {
    scale_y <- if (y_class[1] == "numeric") {
      TRUE
    } else {
      FALSE
    }
  }
  if (is.null(center_y)) {
    center_y <- if (scale_y) {
      TRUE
    } else {
      FALSE
    }
  }
  if (scale_y || center_y) {
    y <- preprocess(
      y,
      config = setup_Preprocessor(scale = scale_y, center = center_y),
      verbosity = verbosity
    )[["preprocessed"]]
  }

  # Data ----
  xnames <- colnames(x)
  ynames <- colnames(y)
  dat <- data.table(x, y)

  # fit1: Loop function ----
  fit1 <- function(index, dat, family, ynames) {
    formula1 <- as.formula(paste(
      ynames[index],
      "~",
      paste(xnames, collapse = " + ")
    ))
    mod1 <- glm(formula1, family = family, data = dat)
    glm2table(list(mod1), xnames = ynames[index], include_anova = NA)
  }

  # Fit models ----
  if (verbosity > 0L) {
    msg(
      "Fitting",
      highlight(length(ynames)),
      "GLMs of family",
      bold(.family),
      "with",
      highlight(length(xnames)),
      ngettext(length(xnames), "predictor", "predictors"),
      "each..."
    )
  }
  tbls <- lapply(
    cli_progress_along(seq_along(y), name = "GLMs", type = "tasks"),
    function(i) {
      fit1(index = i, dat = dat, family = .family, ynames = ynames)
    }
  )
  tbl <- rbindlist(tbls)

  # MassGLM ----
  # ynames should be the same as tbl[["Variable"]]
  # <> Check in MassGLM constructor
  if (!all(ynames == tbl[["Variable"]])) {
    cli::cli_warn(c(
      "The names of the outcome variables in y ({.val ynames}) do not match the names in the summary table ({.val summary[['Variable']]})",
      "Check the summary table."
    ))
  }
  outro(start_time)
  MassGLM(
    summary = tbl,
    ynames = ynames,
    xnames = xnames,
    coefnames = gsub("Coefficient_", "", getnames(tbl, "Coefficient")),
    family = .family
  )
} # /rtemis::massGLM
