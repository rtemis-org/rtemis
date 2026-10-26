# utils_io.R
# ::rtemis::
# 2022- EDG rtemis.org

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
            "Saved ",
            rds_path
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
#' @param must_exist Logical: If TRUE, abort if path does not exist.
#' @param allowed_base Character: Optional base directory to restrict paths to. If provided,
#' the normalized path must be within this directory; if NULL, no restriction.
#' @param allow_urls Logical: If TRUE, allow URL schemes (http://, https://, etc.).
#' @param type Character: Expected path type - "file", "directory", or "any". Only checked
#' if `must_exist = TRUE`.
#' @param normalize Logical: If TRUE, return the absolute, normalized path. If FALSE, return
#' `path` exactly as given; every check still runs against its normalized form. A path that is
#' *stored* (a config's `outdir` or data path) passes FALSE: a config is a portable recipe, and
#' resolving a path against the current working directory pins it to one machine.
#'
#' @return Character: Sanitized path; absolute and normalized unless `normalize = FALSE`.
#'
#' @details
#' Security checks performed:
#' - Rejects paths starting with pipe character (prevents command injection in R readers)
#' - Rejects paths containing null bytes
#' - Rejects URL schemes unless `allow_urls = TRUE`
#' - Normalizes path to absolute form for the checks below
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
  type = c("any", "file", "directory"),
  normalize = TRUE
) {
  type <- match.arg(type)

  # Check for NULL, NA, or empty length
  if (is.null(path) || length(path) == 0L || anyNA(path)) {
    rtemis.core::abort(
      "Path cannot be NULL, NA, or empty.",
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

  # Check for empty string
  if (nchar(path) == 0L) {
    rtemis.core::abort(
      "Path cannot be empty.",
      class = c("rtemis_null_input", "rtemis_input_error")
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

  # `normalizePath()` resolves a relative path against the working directory --
  # on Windows even one that does not exist, on POSIX only one that does -- and
  # expands `~`, so what it returns is machine- and platform-specific. Callers
  # that store the result ask for the path as written instead.
  if (normalize) {
    normalized_path
  } else {
    path
  }
} # /rtemis::sanitize_path


# %% is_url ----
#' Test whether a path is an HTTP or HTTPS URL
#'
#' @param path Character: Path or URL.
#'
#' @return Logical scalar: TRUE if `path` starts with `http://` or `https://`,
#' in any case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_url <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    grepl("^https?://", path, ignore.case = TRUE)
} # /rtemis::is_url


# %% url_filename ----
#' The file name a URL's path ends in
#'
#' @param url Character: HTTP or HTTPS URL.
#'
#' @return Character: the last segment of the URL path, with the query and
#' fragment removed and every character outside `[A-Za-z0-9._-]` replaced by
#' `_`, so that it is a safe local file name that keeps the remote extension.
#' A URL carrying user credentials is an input error.
#'
#' @author EDG
#' @keywords internal
#' @noRd
url_filename <- function(url) {
  # User information in the authority, as in `https://user:pass@host/`, would
  # reach messages and the ingest manifest.
  if (grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]*@", url)) {
    rtemis.core::abort(
      "The URL carries user credentials. ",
      "Use a URL without a `user:password@` part.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  url_path <- sub("^[a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]*", "", url)
  url_path <- sub("[?#].*$", "", url_path)
  name <- gsub("[^A-Za-z0-9._-]", "_", basename(url_path))
  if (!nzchar(name) || grepl("^\\.+$", name)) {
    rtemis.core::abort(
      "The URL path names no file: `",
      url,
      "`. Use a URL whose path ends in the file name, such as ",
      "`https://example.org/data/iris.csv`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  name
} # /rtemis::url_filename


# %% download_file ----
#' Download a URL to a local file
#'
#' One call site for `utils::download.file()`, which tests replace to run
#' offline.
#'
#' @param url Character: HTTP or HTTPS URL.
#' @param destfile Character: Local path to write to.
#' @param quiet Logical: If TRUE, suppress the progress display.
#'
#' @return Integer: 0 on success, as `utils::download.file()` returns.
#'
#' @author EDG
#' @keywords internal
#' @noRd
download_file <- function(url, destfile, quiet) {
  utils::download.file(url, destfile = destfile, mode = "wb", quiet = quiet)
} # /rtemis::download_file


# %% download_url ----
#' Download a URL to a temporary file named after the remote file
#'
#' The file is written to a new temporary directory under the name
#' `url_filename()` returns, so its extension, including a compound one such as
#' `.csv.gz`, selects the reader. The caller deletes the directory,
#' `dirname()` of the returned path, once the file is read. The download is
#' bounded by `getOption("timeout")`.
#'
#' @param url Character: HTTP or HTTPS URL.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character: Path to the downloaded file.
#'
#' @author EDG
#' @keywords internal
#' @noRd
download_url <- function(url, verbosity = 1L) {
  if (!is_url(url)) {
    rtemis.core::abort(
      "Expected an http:// or https:// URL. Got: `",
      url,
      "`",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  name <- url_filename(url)
  dir <- tempfile("rtemis_download_")
  dir.create(dir)
  destfile <- file.path(dir, name)
  downloaded <- FALSE
  on.exit(
    if (!downloaded) unlink(dir, recursive = TRUE),
    add = TRUE
  )
  msg0(
    bold(highlight("▶")),
    " Downloading ",
    highlight(name),
    "...",
    verbosity = verbosity
  )
  # The condition is returned, not handled, so the error is raised from this
  # function and names it as the caller.
  status <- tryCatch(
    download_file(url, destfile, quiet = verbosity < 1L),
    error = identity,
    warning = identity
  )
  if (inherits(status, "condition")) {
    cause <- conditionMessage(status)
    rtemis.core::abort(
      "Downloading `",
      url,
      "` failed: ",
      cause,
      if (grepl("timeout|timed out", cause, ignore.case = TRUE)) {
        paste0(
          " The download is bounded by `getOption(\"timeout\")`, ",
          getOption("timeout"),
          " seconds; set `options(timeout = )` to a larger value."
        )
      },
      class = "rtemis_io_error"
    )
  }
  if (!identical(as.integer(status), 0L) || !file.exists(destfile)) {
    rtemis.core::abort(
      "Downloading `",
      url,
      "` failed with status ",
      status,
      ".",
      class = "rtemis_io_error"
    )
  }
  downloaded <- TRUE
  destfile
} # /rtemis::download_url
