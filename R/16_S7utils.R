# S7_utils
# ::rtemis::
# 2025 EDG rtemis.org

# %% SuperWorkers ----
#' @keywords internal
#' @noRd
SuperWorkers <- new_class(
  name = "SuperWorkers",
  properties = list(
    algorithm = class_character,
    max_workers = class_integer,
    max_workers_algorithm = class_integer,
    max_workers_tuning = class_integer,
    max_workers_resampling = class_integer
  ),
  constructor = function(
    algorithm,
    max_workers,
    max_workers_algorithm,
    max_workers_tuning,
    max_workers_resampling
  ) {
    max_workers <- clean_posint(max_workers)
    max_workers_algorithm <- clean_posint(max_workers_algorithm)
    max_workers_tuning <- clean_posint(max_workers_tuning)
    max_workers_resampling <- clean_posint(max_workers_resampling)
    # Validate input
    if (
      max_workers_algorithm + max_workers_tuning + max_workers_resampling >
        max_workers
    ) {
      cli::cli_abort(
        "Total workers for algorithm, tuning, and resampling cannot exceed max_workers."
      )
    }
    new_object(
      S7_object(),
      algorithm = algorithm,
      max_workers = max_workers,
      max_workers_algorithm = max_workers_algorithm,
      max_workers_tuning = max_workers_tuning,
      max_workers_resampling = max_workers_resampling
    )
  }
) # /rtemis::SuperWorkers


# %% BiasVariance ----
BiasVariance <- new_class(
  name = "BiasVariance",
  properties = list(
    bias_squared = class_numeric,
    mean_bias_squared = class_numeric,
    sd_bias_squared = class_numeric,
    variance = class_numeric,
    mean_variance = class_numeric,
    sd_variance = class_numeric
  )
)


# %% print.BiasVariance ----
#' Print method for BiasVariance
#'
#' @param x BiasVariance object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, BiasVariance) <- function(x, ...) {
  objcat("BiasVariance")
  cat("Mean squared bias: ")
  cat(highlight(ddSci(x[["mean_bias_squared"]])))
  cat(" (", ddSci(x[["sd_bias_squared"]]), ")\n", sep = "")
  cat("    Mean variance: ")
  cat(highlight(ddSci(x[["mean_variance"]])))
  cat(" (", ddSci(x[["sd_variance"]]), ")\n", sep = "")
  cat("\n")
} # /rtemis::print.BiasVariance
