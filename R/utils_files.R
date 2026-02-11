# utils_files.R
# ::rtemis::
# 2025 EDG rtemis.org

#' Expand, normalize, concatenate, clean path
#'
#' @param ... Character: Parts of path to concatenate.
#' @param expand_path Logical: If TRUE, expand concatenated path using [path.expand].
#'
#' @return Character: Path.
#'
#' @author EDG
#' @keywords internal
#' @noRd
make_path <- function(..., expand_path = TRUE) {
  path <- list(...)
  # Remove final "/"
  path <- lapply(path, \(x) gsub("\\/$", "", x))
  # Concat
  path <- do.call(file.path, path)
  # Expand
  if (expand_path) {
    path <- path.expand(path)
  }
  path
} # /rtemis::make_path
