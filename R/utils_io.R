# rt_io.R
# ::rtemis::
# 2022 EDG rtemis.org

#' Write \pkg{rtemis} model to RDS file
#'
#' @param object Supervised object.
#' @param outdir Path to output directory.
#' @param file_prefix Character: Prefix for filename.
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @keywords internal
#' @noRd

rt_save <- function(
  object,
  outdir,
  file_prefix,
  print_load_info = TRUE,
  verbosity = 1L
) {
  # Message before expanding outdir to preserve privacy when using relative paths.
  if (verbosity > 0L) {
    start_time <- Sys.time()
    msg0(
      "Writing data to ",
      outdir,
      "...",
      caller = NA,
      newline = FALSE
    )
  }
  outdir <- normalizePath(outdir, mustWork = FALSE)
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }
  rds_path <- file.path(outdir, paste0(file_prefix, ".rds"))
  try(saveRDS(object, rds_path))
  if (verbosity > 0L) {
    elapsed <- Sys.time() - start_time
  }
  if (file.exists(rds_path)) {
    if (verbosity > 0L) {
      yay(format(elapsed, digits = 2), gray(" [rt_save]"), sep = "")
      if (print_load_info) {
        msg0(gray(
          paste0(
            "Reload with:",
            "> obj <- readRDS('",
            rds_path,
            "')"
          )
        ))
      }
    }
  } else {
    if (verbosity > 0L) {
      nay(
        "Failed after ",
        format(elapsed, digits = 2),
        gray(" [rt_save]"),
        sep = ""
      )
    }
    cli::cli_abort("Error: Saving model to ", outdir, " failed.")
  }
} # /rtemis::rt_save

#' Check file(s) exist
#'
#' @param paths Character vector of paths
#' @param verbosity Integer: Verbosity level.
#' @param pad Integer: Number of spaces to pad to the left
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
check_files <- function(paths, verbosity = 1L, pad = 0) {
  if (verbosity > 0L) {
    msg0("Checking ", singorplu(length(paths), "file"), ":")
  }

  for (f in paths) {
    if (file.exists(f)) {
      if (verbosity > 0L) {
        yay(f, pad = pad)
      }
    } else {
      if (verbosity > 0L) {
        nay(paste(f, red(" not found!")), pad = pad)
      }
      cli::cli_abort("File not found")
    }
  }
} # /rtemis::check_files


#' Write list elements to CSV files
#'
#' @param x List containing R objects to be written to CSV (e.g. data.frames,
#' matrices, etc.)
#' @param outdir Character: Path to output directory
#'
#' @return Nothing, writes CSV files to `outdir`.
#'
#' @author EDG
#' @export
#'
#' @examples
#' /dontrun{
#' x <- list(
#'    iris = iris,
#'    iris_normalized = as.data.frame(scale(iris[, -5]))
#' )
#' outdir <- "./exports"
#' list2csv(x, outdir)
#' }
list2csv <- function(x, outdir) {
  if (!inherits(x, "list")) {
    cli::cli_abort("Input must be a list")
  }

  xname <- deparse(substitute(x))
  outnames <- names(x)
  if (is.null(outnames)) {
    outnames <- paste(xname, seq_along(x), sep = "_")
  }
  outnames <- paste0(outnames, ".csv")
  for (i in seq_along(x)) {
    write.csv(x[[i]], file.path(outdir, outnames[i]), row.names = FALSE)
  }
} # /rtemis::list2csv
