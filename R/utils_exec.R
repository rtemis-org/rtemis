# utils_exec.Ranger
# ::rtemis::
# 2025- EDG rtemis.org

#' Do call with tryCatch and suggestion
#'
#' @param fn Function to call.
#' @param args List of arguments to pass to function.
#' @param error_pattern_suggestion Named list of the form pattern = "suggestion". If the pattern is
#'  found in the error message, the suggestion is appended to the error message.
#' @param warning_pattern_suggestion Named list of the form pattern = "suggestion". If the pattern is
#'
#' @return Result of function call.
#'
#' @author EDG
#' @keywords internal
#' @noRd
do_call <- function(
  fn,
  args,
  error_pattern_suggestion = NULL,
  warning_pattern_suggestion = NULL
) {
  call <- parent.frame(n = 1L)
  common_errors <- list(
    "object '(.*)' not found" = "Check that the object exists and is spelled correctly.",
    "object of type 'closure' is not subsettable" = "Check that the object is a list or data.frame."
  )
  common_warnings <- list(
    "NAs introduced by coercion" = "Check that the input is of the correct type.",
    # "glm.fit: algorithm did not converge" =
    # "Same reasons as for 'glm.fit: fitted probabilities numerically 0 or 1 occurred'.",
    "glm.fit: fitted probabilities numerically 0 or 1 occurred" = paste(
      bold("Reasons for this warning include:"),
      "1) Perfect Separation of classes.",
      "2) Highly Imbalanced data.",
      "3) Extreme values in predictors.",
      "4) Too many predictors for the number of observations.",
      "5) Multicollinearity.",
      bold("Suggestion:"),
      "Try using GLMNET or tree-based algorithms",
      sep = "\n  "
    )
  )
  err_pat_sug <- c(common_errors, error_pattern_suggestion)
  warn_pat_sug <- c(common_warnings, warning_pattern_suggestion)
  tryCatch(
    {
      withCallingHandlers(
        {
          do.call(fn, args)
        },
        warning = function(w) {
          fnwarn <- conditionMessage(w)
          message("Warning caught: ", fnwarn)
          idi <- which(sapply(
            names(warn_pat_sug),
            function(i) grepl(i, fnwarn)
          ))
          if (length(idi) > 0) {
            for (i in idi) {
              cat(fmt(
                paste0(warn_pat_sug[[i]], "\n"),
                col = rtemis_colors[["orange"]]
              ))
            }
          }
          invokeRestart("muffleWarning")
        } # /warning
      ) # /withCallingHandlers
    },
    error = function(e) {
      fnerr <- e[["message"]]
      errmsg <- paste0(highlight(fn), " failed with error:\n", fnerr, "\n")
      idi <- which(sapply(names(err_pat_sug), function(i) grepl(i, fnerr)))
      if (length(idi) > 0) {
        suggestions <- sapply(idi, function(i) err_pat_sug[[i]])
        errmsg <- paste0(
          red(errmsg),
          fmt(
            paste0(
              bold("\nSuggestion:\n  "),
              paste0(suggestions, collapse = "\n  ")
            ),
            col = rtemis_colors[["orange"]]
          )
        )
      }
      cat("\n")
      rtemis.core::abort(errmsg, class = "rtemis_runtime_error", parent = e)
    } # /error
  ) # /tryCatch
} # /rtemis::do_call
