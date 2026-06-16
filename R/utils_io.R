# utils_io.R
# ::rtemis::
# 2022 EDG rtemis.org

#' Write \pkg{rtemis} model to RDS file
#'
#' @param object `Supervised` object.
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
  outdir <- sanitize_path(outdir, must_exist = FALSE, type = "any")
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
            "Reload with: ",
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
    rtemis.core::abort(
      "Saving model to ",
      outdir,
      " failed.",
      class = "rtemis_io_error"
    )
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
      rtemis.core::abort(
        "File not found: ",
        f,
        class = c("rtemis_file_not_found", "rtemis_io_error")
      )
    }
  }
} # /rtemis::check_files

# %% sanitize_path ----
#' Sanitize and validate file paths for security
#'
#' Validates and normalizes file paths to prevent security vulnerabilities
#' including command injection, path traversal, and unauthorized file access.
#'
#' @param path Character: File or directory path to sanitize.
#' @param must_exist Logical: If TRUE, abort if path does not exist. Default = FALSE.
#' @param allowed_base Character: Optional base directory to restrict paths to. If provided,
#' the normalized path must be within this directory. Default = NULL (no restriction).
#' @param allow_urls Logical: If TRUE, allow URL schemes (http://, https://, etc.).
#' Default = FALSE.
#' @param type Character: Expected path type - "file", "directory", or "any". Only checked
#' if `must_exist = TRUE`. Default = "any".
#'
#' @return Character: Sanitized and normalized absolute path.
#'
#' @details
#' Security checks performed:
#' - Rejects paths starting with pipe character (prevents command injection in R readers)
#' - Rejects paths containing null bytes
#' - Rejects URL schemes unless `allow_urls = TRUE`
#' - Normalizes path to absolute form
#' - Optionally validates path exists and is correct type
#' - Optionally validates path is within allowed base directory
#'
#' @author EDG
#' @keywords internal
#' @noRd
sanitize_path <- function(
  path,
  must_exist = FALSE,
  allowed_base = NULL,
  allow_urls = FALSE,
  type = c("any", "file", "directory")
) {
  type <- match.arg(type)

  # Check for NULL or empty
  if (is.null(path) || length(path) == 0L || nchar(path) == 0L) {
    rtemis.core::abort(
      "Path cannot be NULL or empty.",
      class = c("rtemis_null_input", "rtemis_input_error")
    )
  }

  # Check for multiple paths
  if (length(path) > 1L) {
    rtemis.core::abort(
      "Function accepts a single path. Got ",
      length(path),
      " paths.",
      class = c("rtemis_length_error", "rtemis_input_error")
    )
  }

  # Check for null bytes (check if raw bytes contain 0x00)
  if (any(charToRaw(path) == 0L)) {
    rtemis.core::abort(
      "Path contains null byte: `",
      path,
      "`",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Check for pipe character at start (command injection vector)
  if (grepl("^\\s*\\|", path)) {
    rtemis.core::abort(
      "Path cannot start with pipe character: `",
      path,
      "`",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Check for URL schemes unless explicitly allowed
  if (!allow_urls && grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", path)) {
    rtemis.core::abort(
      "URL schemes not allowed: `",
      path,
      "`",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Normalize to absolute path
  # mustWork = FALSE allows non-existent paths, will check separately if needed
  normalized_path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  # Validate against allowed base directory if specified
  if (!is.null(allowed_base)) {
    allowed_base_norm <- normalizePath(
      allowed_base,
      winslash = "/",
      mustWork = TRUE
    )
    # Check if normalized path starts with allowed base
    if (!startsWith(normalized_path, allowed_base_norm)) {
      rtemis.core::abort(
        "Path `",
        path,
        "` is outside allowed directory: `",
        allowed_base,
        "`",
        class = "rtemis_io_error"
      )
    }
  }

  # Check existence and type if required
  if (must_exist) {
    if (!file.exists(normalized_path)) {
      rtemis.core::abort(
        "Path does not exist: `",
        normalized_path,
        "`",
        class = c("rtemis_file_not_found", "rtemis_io_error")
      )
    }

    if (type == "file" && dir.exists(normalized_path)) {
      rtemis.core::abort(
        "Path is not a file: `",
        normalized_path,
        "`",
        class = "rtemis_io_error"
      )
    }

    if (type == "directory" && !dir.exists(normalized_path)) {
      rtemis.core::abort(
        "Path is not a directory: `",
        normalized_path,
        "`",
        class = "rtemis_io_error"
      )
    }
  }

  normalized_path
} # /rtemis::sanitize_path
