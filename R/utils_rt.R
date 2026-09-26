# info
# ::rtemis::
# 2016- EDG rtemis.org

#' `rtemis-internals`: `intro`
#'
#' Intro
#'
#' Starts function execution timer and opens log file.
#' Pairs with `outro`.
#'
#' @keywords internal
#' @noRd
intro <- function(
  .message = "\u25b6",
  logfile = NULL,
  call_depth = 1,
  caller = NULL,
  newline_pre = FALSE,
  use_sink = FALSE,
  verbosity = 1L
) {
  if (!is.null(logfile)) {
    logfile <- normalizePath(logfile, mustWork = FALSE)
    outdir <- dirname(logfile)
    if (!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    }
    if (use_sink) {
      sink(logfile, append = TRUE, split = verbosity > 0L)
    }
    log_to_file("Started.", logfile = logfile)
  }
  start_time <- proc.time()
  if (verbosity > 0L || !is.null(logfile)) {
    if (newline_pre) {
      cat("\n")
    }
    msg(
      .message,
      call_depth = call_depth,
      sep = "",
      caller_id = 2,
      caller = caller
    )
  }
  invisible(start_time)
} # /rtemis::intro


# Format duration: seconds if < 60, otherwise M:SS or H:MM:SS
# (matching the format used by rtemis.core progress reporting)
#' @keywords internal
#' @noRd
format_seconds <- function(seconds) {
  if (seconds < 60) {
    paste0(bold(ddSci(seconds)), " seconds")
  } else {
    s <- as.integer(round(seconds))
    h <- s %/% 3600L
    m <- (s %% 3600L) %/% 60L
    sec <- s %% 60L
    if (h > 0L) {
      bold(sprintf("%d:%02d:%02d", h, m, sec))
    } else {
      bold(sprintf("%d:%02d", m, sec))
    }
  }
}


#' `rtemis-internals`: `outro`
#'
#' Outro
#'
#' Stops function execution timer and closes log file.
#'
#' Second part to `intro`
#'
#' @keywords internal
#' @noRd
outro <- function(
  start_time,
  sink_off = FALSE,
  logfile = NULL,
  newline_pre = FALSE,
  real_user_system = FALSE,
  verbosity = 1L
) {
  elapsed <- as.numeric(proc.time() - start_time)
  if (verbosity > 0L || sink_off) {
    if (newline_pre) {
      cat("\n")
    }
    if (real_user_system) {
      msg0(
        paste0(
          "Done in ",
          format_seconds(elapsed[3]),
          " (",
          "Real:",
          ddSci(elapsed[3]),
          "/User:",
          ddSci(elapsed[1]),
          "/System:",
          ddSci(elapsed[2]),
          ")."
        ),
        caller_id = 2
      )
    } else {
      msg0(
        paste0(
          "Done in ",
          format_seconds(elapsed[3]),
          "."
        ),
        caller_id = 2
      )
    }
  }

  if (sink_off) {
    sink()
  }
  if (!is.null(logfile)) {
    log_to_file("Done.", logfile = logfile)
  }
  invisible(elapsed)
} # /rtemis::outro


#' Summarize supervised inputs
#'
#' @param x tabular data: Training set data.
#' @param dat_validation data.frame or similar: Validation set data.
#' @param dat_test data.frame or similar: Test set data.
#'
#' @author EDG
#' @keywords internal
#' @noRd
summarize_supervised <- function(
  x,
  dat_validation = NULL,
  dat_test = NULL
) {
  # msg("Input data summary:")
  msg0(
    if (!is.null(dat_validation)) "  ",
    "Training set: ",
    highlight(NROW(x)),
    " cases x ",
    highlight(NCOL(x) - 1),
    " features."
  )
  if (!is.null(dat_validation)) {
    msg0(
      "Validation set: ",
      highlight(NROW(dat_validation)),
      " cases x ",
      highlight(NCOL(dat_validation) - 1),
      " features."
    )
  }
  if (!is.null(dat_test)) {
    msg0(
      if (!is.null(dat_validation)) "  ",
      "    Test set: ",
      highlight(NROW(dat_test)),
      " cases x ",
      highlight(NCOL(dat_test) - 1),
      " features."
    )
  }
} # /rtemis::summarize_supervised


#' Summarize unsupervised inputs
#'
#' @param x tabular data: Training set data.
#'
#' @author EDG
#' @keywords internal
#' @noRd
summarize_unsupervised <- function(x) {
  msg(
    "Input:",
    highlight(NROW(x)),
    "cases x",
    highlight(NCOL(x)),
    "features."
  )
} # /rtemis::summarize_unsupervised


#' Resolve an unsupervised run's feature columns
#'
#' The columns a clustering or decomposition run uses when its config names
#' none: every numeric column of `x`. `cluster()`, `decomp()` and the
#' decomposition step inside `train()` all resolve it here, so the fit, the run
#' record and `checks/v1` name the same columns, and the published schemas'
#' "null = all numeric features" is one rule rather than three.
#'
#' Resolved from the data rather than declared, so no default policy can
#' express it: which columns are numeric is a fact about the dataset.
#'
#' @param x data.frame: The features to run on; no outcome column.
#' @param what Character: What is being fit, for the error message.
#'
#' @return Character vector: Names of the numeric columns of `x`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_unsupervised_features <- function(x, what) {
  features <- getnumericnames(x)
  if (length(features) < 2L) {
    rtemis.core::abort(
      what,
      " requires at least 2 numeric feature columns; ",
      NCOL(x),
      ngettext(NCOL(x), " column", " columns"),
      " supplied, of which ",
      length(features),
      if (length(features) == 1L) " is numeric." else " are numeric.",
      "\nName the columns to use in the config's `features`, or supply ",
      "numeric data.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }
  features
} # /rtemis::resolve_unsupervised_features


#' Log to file
#'
#' @param x Character: Message to log.
#' @param logfile Character: Path to log file.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
log_to_file <- function(x, logfile) {
  cat(
    paste0(
      datetime(),
      " ",
      x,
      "\n"
    ),
    file = logfile,
    append = TRUE
  )
} # /rtemis::log_to_file
