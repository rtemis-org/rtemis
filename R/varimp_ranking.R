# varimp_ranking.R
# ::rtemis::
# 2026- EDG rtemis.org
# spec: rtemis/varimp-ranking

# %% varimp_ranking ----
#' Rank a Model's Variable Importance
#'
#' Summarize each importance measure across resamples and rank the variables
#' by it, as [plot_varimp()] draws them.
#'
#' @details
#' Each measure is ranked as its `direction` declares: by larger values, by
#' larger absolute values, or by smaller values; a measure with direction
#' `"none"` is listed by decreasing value. A variable's summary is the mean of
#' its available scores across resamples, computed by
#' [rtemis.draw::summarize_varimp()]; an `"absolute"` measure ranks by the
#' absolute value of that mean. NA scores and resamples without importance are
#' excluded. A measure with no available score is omitted.
#'
#' The table is in rank order within each measure, and in resample order
#' within each variable, so the rows of the first `n` ranks of a measure are
#' the rows of the `n` most important variables.
#'
#' @param x `Supervised`, `SupervisedRes` or `SHAP`: Fitted model result or
#'   explanation.
#'
#' @return data.frame with one row per measure, variable and resample with an
#'   available score: `measure`, `variable`, `fold` (resampled results only),
#'   `value` (the score), `summary` (the variable's mean across resamples), and
#'   `rank` (1 for the most important variable of the measure).
#'
#' @author EDG
#' @export
#'
#' @examples
#' mod <- train(iris, hyperparameters = setup_CART(), verbosity = 0L)
#' head(varimp_ranking(mod))
varimp_ranking <- function(x) {
  records <- varimp_plot_data(x)
  data <- records[["data"]]
  folds <- records[["folds"]]
  resampled <- !is.null(folds)
  measures <- setdiff(names(data), VARIMP_TABLE_RESERVED)
  parts <- lapply(measures, function(measure) {
    if (all(is.na(data[[measure]]))) {
      return(NULL)
    }
    ranking <- VARIMP_RANKING[[varimp_measure_direction(x, measure)]]
    summary <- rtemis.draw::summarize_varimp(
      data,
      measure = measure,
      rank_by = ranking[["rank_by"]],
      decreasing = ranking[["decreasing"]],
      folds = folds
    )
    at <- which(!is.na(data[[measure]]))
    rank <- match(data[["variable"]][at], summary[["variable"]])
    at <- at[!is.na(rank)]
    rank <- rank[!is.na(rank)]
    order_at <- if (resampled) {
      order(rank, match(data[["fold"]][at], folds))
    } else {
      order(rank)
    }
    at <- at[order_at]
    rank <- rank[order_at]
    out <- data.frame(measure = measure, variable = data[["variable"]][at])
    if (resampled) {
      out[["fold"]] <- data[["fold"]][at]
    }
    out[["value"]] <- data[[measure]][at]
    out[["summary"]] <- summary[["importance"]][rank]
    out[["rank"]] <- rank
    out
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    abort(
      "No variable importance is available. Use a model that records importance.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
} # /rtemis::varimp_ranking
