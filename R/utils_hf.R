# utils_hf.R
# ::rtemis::
# 2026- EDG rtemis.org

# spec: rtemis/read-urls#hugging-face-datasets

# %% is_hf ----
#' Test whether a path is a Hugging Face `hf://` path
#'
#' @param path Character: Path or URL.
#'
#' @return Logical scalar: TRUE if `path` starts with `hf://`, in any case.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_hf <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    grepl("^hf://", path, ignore.case = TRUE)
} # /rtemis::is_hf


# %% parse_hf ----
#' Parse an `hf://datasets/` path
#'
#' The form is `hf://datasets/<owner>/<name>[@<revision>][/<file>]`, the form
#' of `huggingface_hub`'s `HfFileSystem`. A revision containing `/` is written
#' URL-encoded, as `refs%2Fconvert%2Fparquet`.
#'
#' @param path Character: `hf://` path.
#'
#' @return Named list: `repo` (`<owner>/<name>`), `revision` (`"main"` when the
#' path names none), and `file` (path of the file in the repository, or NULL
#' when the path names the repository only).
#'
#' @author EDG
#' @keywords internal
#' @noRd
parse_hf <- function(path) {
  if (!grepl("^hf://datasets/", path, ignore.case = TRUE)) {
    rtemis.core::abort(
      "`hf://` paths name datasets only: `",
      path,
      "`. Use `hf://datasets/<owner>/<name>/<file>`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  pattern <- paste0(
    "^hf://datasets/",
    "([A-Za-z0-9][A-Za-z0-9._-]*)/([A-Za-z0-9][A-Za-z0-9._-]*)",
    "(?:@([A-Za-z0-9._%-]+))?",
    "(?:/(.*))?$"
  )
  parts <- regmatches(
    path,
    regexec(pattern, path, perl = TRUE, ignore.case = TRUE)
  )[[
    1L
  ]]
  if (length(parts) == 0L) {
    rtemis.core::abort(
      "Cannot parse the Hugging Face path `",
      path,
      "`. Use `hf://datasets/<owner>/<name>[@<revision>][/<file>]`, as ",
      "`hf://datasets/rtemis/tcga-pancan-hiseq/data/tcga-pancan-hiseq.parquet`.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  file <- sub("/+$", "", parts[[5L]])
  if (nzchar(file)) {
    segments <- strsplit(file, "/", fixed = TRUE)[[1L]]
    if (any(segments %in% c("", ".", ".."))) {
      rtemis.core::abort(
        "The file part of `",
        path,
        "` has an empty, `.`, or `..` segment. ",
        "Name the file by its path in the repository, as `data/x.parquet`.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
  }
  list(
    repo = paste0(parts[[2L]], "/", parts[[3L]]),
    revision = if (nzchar(parts[[4L]])) parts[[4L]] else "main",
    file = if (nzchar(file)) file
  )
} # /rtemis::parse_hf


# %% hf_endpoint ----
#' The Hugging Face Hub endpoint
#'
#' @return Character: the value of the `HF_ENDPOINT` environment variable, as
#' `huggingface_hub` reads it, or `https://huggingface.co`, without a trailing
#' slash.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_endpoint <- function() {
  endpoint <- sub(
    "/+$",
    "",
    Sys.getenv("HF_ENDPOINT", "https://huggingface.co")
  )
  if (!is_url(endpoint)) {
    rtemis.core::abort(
      "The `HF_ENDPOINT` environment variable must be an http:// or https:// ",
      "URL. Got: `",
      endpoint,
      "`",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  endpoint
} # /rtemis::hf_endpoint


# %% hf_data_files ----
#' Repository files `read()` reads as data
#'
#' @param files Character: File paths in a repository.
#'
#' @return Character: the files whose extension is `parquet`, `csv`, `tsv`,
#' `csv.gz`, `tsv.gz`, `rds`, `xlsx`, `dta`, `arff`, or `fasta`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hf_data_files <- function(files) {
  files[grepl(
    "\\.(parquet|csv|tsv|csv\\.gz|tsv\\.gz|rds|xlsx|dta|arff|fasta)$",
    files,
    ignore.case = TRUE
  )]
} # /rtemis::hf_data_files


# %% resolve_hf ----
#' Resolve an `hf://datasets/` path to a commit and a file
#'
#' Reads the repository's file list at the revision from the Hub API
#' (`/api/datasets/<repo>/revision/<revision>?blobs=true`). A path that names
#' the repository only resolves to its one data file, as `hf_data_files()`
#' selects it. The download URL names the commit, so the file downloaded is the
#' file at the recorded commit.
#'
#' @param path Character: `hf://` path.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list: `repo`, `revision`, `commit` (40-character SHA), `file`,
#' `size` (bytes), `lfs_sha256` (SHA-256 of an LFS file, or NULL), `blob_id`
#' (git blob SHA-1 of a file stored in git, or NULL), and `url`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_hf <- function(path, verbosity = 1L) {
  check_dependencies("jsonlite")
  ref <- parse_hf(path)
  endpoint <- hf_endpoint()
  api_url <- paste0(
    endpoint,
    "/api/datasets/",
    ref[["repo"]],
    "/revision/",
    ref[["revision"]],
    "?blobs=true"
  )
  info_file <- tempfile("rtemis_hf_", fileext = ".json")
  on.exit(unlink(info_file), add = TRUE)
  status <- tryCatch(
    download_file(api_url, info_file, quiet = TRUE),
    error = identity,
    warning = identity
  )
  if (inherits(status, "condition") || !file.exists(info_file)) {
    rtemis.core::abort(
      "The Hugging Face Hub returned no dataset `",
      ref[["repo"]],
      "` at revision `",
      ref[["revision"]],
      "`",
      if (inherits(status, "condition")) {
        paste0(": ", conditionMessage(status))
      },
      ". Check the owner, name, and revision. `read()` reads public, ",
      "ungated datasets.",
      class = "rtemis_io_error"
    )
  }
  info <- jsonlite::read_json(info_file, simplifyVector = FALSE)
  commit <- info[["sha"]]
  if (!is.character(commit) || !grepl("^[0-9a-f]{40}$", commit)) {
    rtemis.core::abort(
      "The Hugging Face Hub response for `",
      ref[["repo"]],
      "` carries no commit SHA.",
      class = "rtemis_io_error"
    )
  }
  if (!isFALSE(info[["gated"]]) && !is.null(info[["gated"]])) {
    rtemis.core::abort(
      "The dataset `",
      ref[["repo"]],
      "` is gated. `read()` reads public, ungated datasets.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  siblings <- info[["siblings"]]
  files <- vapply(siblings, function(s) s[["rfilename"]], character(1L))
  file <- ref[["file"]]
  if (is.null(file)) {
    candidates <- hf_data_files(files)
    if (length(candidates) != 1L) {
      rtemis.core::abort(
        "The dataset `",
        ref[["repo"]],
        "` at commit ",
        substr(commit, 1L, 7L),
        if (length(candidates) == 0L) {
          paste0(
            " has no data file. Its files: ",
            paste0("`", files, "`", collapse = ", "),
            "."
          )
        } else {
          paste0(
            " has ",
            length(candidates),
            " data files: ",
            paste0("`", candidates, "`", collapse = ", "),
            ". Name one, as `hf://datasets/",
            ref[["repo"]],
            "/",
            candidates[[1L]],
            "`."
          )
        },
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    file <- candidates[[1L]]
  }
  idx <- match(file, files)
  if (is.na(idx)) {
    candidates <- hf_data_files(files)
    rtemis.core::abort(
      "The dataset `",
      ref[["repo"]],
      "` at commit ",
      substr(commit, 1L, 7L),
      " has no file `",
      file,
      "`.",
      if (length(candidates) > 0L) {
        paste0(
          " Its data files: ",
          paste0("`", candidates, "`", collapse = ", "),
          "."
        )
      },
      class = c("rtemis_file_not_found", "rtemis_io_error")
    )
  }
  sibling <- siblings[[idx]]
  lfs <- sibling[["lfs"]]
  msg0(
    bold(highlight("▶")),
    " Resolved ",
    highlight(ref[["repo"]]),
    "@",
    ref[["revision"]],
    " to commit ",
    substr(commit, 1L, 7L),
    ", file ",
    file,
    verbosity = verbosity
  )
  list(
    repo = ref[["repo"]],
    revision = ref[["revision"]],
    commit = commit,
    file = file,
    size = as.numeric(sibling[["size"]]),
    lfs_sha256 = if (!is.null(lfs)) lfs[["sha256"]],
    blob_id = if (is.null(lfs)) sibling[["blobId"]],
    url = paste0(
      endpoint,
      "/datasets/",
      ref[["repo"]],
      "/resolve/",
      commit,
      "/",
      utils::URLencode(file, reserved = FALSE)
    )
  )
} # /rtemis::resolve_hf


# %% file_sha256 ----
#' SHA-256 of a file
#'
#' @param path Character: Path to the file.
#'
#' @return Character: lowercase hexadecimal SHA-256 of the file's bytes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
file_sha256 <- function(path) {
  # `raw = TRUE`: `file()` otherwise decompresses a gzip file (RDS, `.csv.gz`)
  # as it reads, and the hash would be of the decompressed content.
  .hash_bytes(file(path, raw = TRUE), "sha256")
} # /rtemis::file_sha256


# %% download_hf ----
#' Download a resolved Hugging Face file and verify it
#'
#' The file's size must equal the size the Hub lists. An LFS file's SHA-256
#' must equal its listed `lfs.sha256`; a file stored in git must have the
#' listed git blob SHA-1, the SHA-1 of `blob <size>`, a NUL byte, and the
#' content.
#'
#' @param hf Named list: Output of `resolve_hf()`.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character: Path to the downloaded file, in a temporary directory the
#' caller deletes.
#'
#' @author EDG
#' @keywords internal
#' @noRd
download_hf <- function(hf, verbosity = 1L) {
  path <- download_url(hf[["url"]], verbosity = verbosity)
  verified <- FALSE
  on.exit(
    if (!verified) unlink(dirname(path), recursive = TRUE),
    add = TRUE
  )
  mismatch <- function(what, expected, actual) {
    rtemis.core::abort(
      "The file `",
      hf[["file"]],
      "` downloaded from `",
      hf[["repo"]],
      "` at commit ",
      substr(hf[["commit"]], 1L, 7L),
      " has ",
      what,
      " ",
      actual,
      " where the Hub lists ",
      expected,
      ". Download it again.",
      class = "rtemis_io_error"
    )
  }
  size <- file.size(path)
  if (!isTRUE(size == hf[["size"]])) {
    mismatch("size", hf[["size"]], size)
  }
  if (!is.null(hf[["lfs_sha256"]])) {
    actual <- file_sha256(path)
    if (!identical(actual, hf[["lfs_sha256"]])) {
      mismatch("SHA-256", hf[["lfs_sha256"]], actual)
    }
  } else if (!is.null(hf[["blob_id"]])) {
    actual <- .hash_bytes(
      c(
        charToRaw(paste0("blob ", format(size, scientific = FALSE))),
        as.raw(0L),
        readBin(path, "raw", n = size)
      ),
      "sha1"
    )
    if (!identical(actual, hf[["blob_id"]])) {
      mismatch("git blob SHA-1", hf[["blob_id"]], actual)
    }
  }
  verified <- TRUE
  path
} # /rtemis::download_hf
