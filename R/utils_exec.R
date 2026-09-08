# utils_exec.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% do_call ----
#' Do call with tryCatch and suggestion
#'
#' Call `fn` with `args`, adding rtemis' advice to a failure and to a warning
#' whose text matches a known pattern.
#'
#' Warnings raised by `fn` are passed through untouched: they are not muffled,
#' rewritten or re-signalled, so `suppressWarnings()`, `tryCatch(warning = )`
#' and `options(warn = 2)` around `do_call()` behave exactly as they would
#' around a direct call. A warning is a condition and belongs to the caller,
#' not to `verbosity`. What `verbosity` governs is the suggestion, which is
#' rtemis output about the warning rather than the warning itself, and which is
#' printed only when a pattern matches -- an unrecognized warning is left to
#' speak for itself.
#'
#' An error's suggestion is part of the error message rather than a separate
#' print, so it survives at any `verbosity`.
#'
#' @param fn Function to call.
#' @param args List of arguments to pass to function.
#' @param error_pattern_suggestion Named list of the form pattern = "suggestion". If the pattern is
#'  found in the error message, the suggestion is appended to the error message.
#' @param warning_pattern_suggestion Named list of the form pattern = "suggestion". If the pattern is
#'  found in a warning message, the suggestion is printed alongside it.
#' @param verbosity Integer: Verbosity level.
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
  warning_pattern_suggestion = NULL,
  verbosity = 1L
) {
  # Callers pass the function itself (`do_call(Rtsne::Rtsne, args)`), which has
  # no character form, so the name for the error message comes from the
  # unevaluated argument. `theme.R` passes a string instead; both must label.
  fn_label <- if (is.character(fn)) fn else deparse1(substitute(fn))
  common_errors <- list(
    "object '(.*)' not found" = "Check that the object exists and is spelled correctly.",
    "object of type 'closure' is not subsettable" = "Check that the object is a list or data.frame."
  )
  common_warnings <- list(
    "NAs introduced by coercion" = "Check that the input is of the correct type.",
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
  # A backend called over a loop repeats one warning per iteration. The
  # condition repeats with it, which is the caller's to handle; the advice is
  # the same every time, so it is given once per `do_call()`.
  advised <- character()
  tryCatch(
    {
      withCallingHandlers(
        {
          do.call(fn, args)
        },
        warning = function(w) {
          # No `invokeRestart("muffleWarning")`: the warning continues to the
          # caller as the condition the backend raised. Removing this line
          # would silently make every backend warning uncatchable again.
          if (verbosity < 1L) {
            return(invisible(NULL))
          }
          fnwarn <- conditionMessage(w)
          idi <- which(vapply(
            names(warn_pat_sug),
            function(pattern) grepl(pattern, fnwarn),
            logical(1L)
          ))
          if (length(idi) == 0L || fnwarn %in% advised) {
            return(invisible(NULL))
          }
          advised <<- c(advised, fnwarn)
          # The warning itself is deferred by R until the top-level call
          # returns, so the advice quotes what it is advising on rather than
          # arriving without an antecedent.
          info(
            highlight(fn_label),
            " warned: ",
            fnwarn,
            "\n  ",
            paste0(
              vapply(idi, function(i) warn_pat_sug[[i]], character(1L)),
              collapse = "\n  "
            ),
            verbosity = verbosity
          )
          invisible(NULL)
        } # /warning
      ) # /withCallingHandlers
    },
    error = function(e) {
      fnerr <- e[["message"]]
      errmsg <- paste0(
        highlight(fn_label),
        " failed with error:\n",
        fnerr,
        "\n"
      )
      idi <- which(vapply(
        names(err_pat_sug),
        function(pattern) grepl(pattern, fnerr),
        logical(1L)
      ))
      if (length(idi) > 0L) {
        suggestions <- vapply(idi, function(i) err_pat_sug[[i]], character(1L))
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
      if (verbosity > 0L) {
        cat("\n")
      }
      rtemis.core::abort(
        errmsg,
        class = "rtemis_runtime_error",
        parent = e,
        verbosity = verbosity
      )
    } # /error
  ) # /tryCatch
} # /rtemis::do_call
