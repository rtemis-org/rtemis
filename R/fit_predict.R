# fit_predict.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% fit_predict ----
#' Fit a Learner and Predict New Data
#'
#' Train one regression model on `x` and `y` with an algorithm named as a string,
#' then return its predictions for `newdata`, their standard errors when the
#' algorithm provides them, and the training R-squared.
#'
#' This is the bridge behind `fit = "<algorithm>"` in [rtemis.draw::draw_scatter()],
#' [rtemis.draw::draw_fit()] and [rtemis.draw::draw_scatter3d()], which need a
#' fitted curve or surface from a name alone. For full control over training,
#' use [train()].
#'
#' @param x data.frame: Features, one column per predictor.
#' @param y Numeric: Outcome, one value per row of `x`.
#' @param newdata data.frame: Features to predict, with the columns of `x`.
#' @param algorithm Character: Supervised learning algorithm name, matched
#'   case-insensitively, e.g. `"LINAD"`, `"CART"`, `"LightGBM"`. See
#'   [available_supervised()].
#' @param params Optional Named list: Arguments to the algorithm's setup
#'   function, e.g. `list(max_leaves = 8)` for [setup_LINAD()]. `NULL` uses the
#'   algorithm's defaults.
#' @param se Logical: If TRUE, compute standard errors of the fit for `newdata`.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list: `fitted` (Numeric predictions for `newdata`), `se`
#'   (Numeric standard errors, or NULL when `se = FALSE` or the algorithm has
#'   none), `rsq` (Numeric training R-squared), and `algorithm` (Character
#'   canonical algorithm name).
#'
#' @author EDG
#' @export
#' @examples
#' fit <- fit_predict(
#'   mtcars["wt"], mtcars[["mpg"]],
#'   newdata = data.frame(wt = c(2, 3, 4)),
#'   algorithm = "GLM",
#'   se = TRUE
#' )
#' fit[["fitted"]]
#' fit_predict(
#'   mtcars["wt"], mtcars[["mpg"]],
#'   newdata = data.frame(wt = c(2, 3, 4)),
#'   algorithm = "LINAD",
#'   params = list(max_leaves = 2)
#' )[["fitted"]]
fit_predict <- function(
  x,
  y,
  newdata,
  algorithm,
  params = NULL,
  se = FALSE,
  verbosity = 0L
) {
  if (!is.data.frame(x) || !is.data.frame(newdata)) {
    abort(
      "Supply `x` and `newdata` as data frames.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!is.numeric(y) || length(y) != nrow(x)) {
    abort(
      "Supply a numeric `y` with one value per row of `x`.",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  if (!identical(names(newdata), names(x))) {
    abort(
      "Supply `newdata` with the same columns as `x`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  check_logical(se)
  hyperparameters <- fit_params_hyperparameters(algorithm, params)
  # train() reads the last column as the outcome.
  dat <- x
  dat[[".outcome"]] <- y
  model <- train(dat, hyperparameters = hyperparameters, verbosity = verbosity)
  if (!S7_inherits(model, Regression)) {
    abort(
      hyperparameters@algorithm,
      " did not produce a regression model; supply a numeric outcome.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  list(
    fitted = as.numeric(predict(model, newdata)),
    se = if (se) {
      se_fit <- se(model, newdata)
      if (!is.null(se_fit)) as.numeric(se_fit)
    },
    rsq = model@metrics_training[["rsq"]],
    algorithm = hyperparameters@algorithm
  )
} # /rtemis::fit_predict


# %% fit_params_hyperparameters ----
#' Build hyperparameters from an algorithm name and plain arguments
#'
#' Calls the algorithm's `setup_*()` function with `params`, so every value is
#' validated exactly as in a direct call. Names the setup function's arguments
#' when `params` holds one it does not take.
#'
#' @param algorithm Character: Supervised learning algorithm name.
#' @param params Optional Named list: Arguments to the setup function.
#'
#' @return `Hyperparameters` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fit_params_hyperparameters <- function(algorithm, params) {
  setup_name <- paste0("setup_", get_alg_name(algorithm))
  if (length(params) == 0L) {
    return(do.call(setup_name, list()))
  }
  if (
    !is.list(params) ||
      is.null(names(params)) ||
      any(!nzchar(names(params))) ||
      anyDuplicated(names(params))
  ) {
    abort(
      "Supply `params` as a list of distinctly named arguments to ",
      setup_name,
      "().",
      class = c("rtemis_type_error", "rtemis_input_error")
    )
  }
  accepted <- names(formals(get(setup_name, mode = "function")))
  unknown <- setdiff(names(params), accepted)
  if (length(unknown) > 0L) {
    abort(
      setup_name,
      "() does not take ",
      paste0("`", unknown, "`", collapse = ", "),
      "; use one of: ",
      paste(accepted, collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  do.call(setup_name, params)
} # /rtemis::fit_params_hyperparameters
