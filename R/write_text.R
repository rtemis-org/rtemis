# write_text.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% TEXT_FORMATS ----
# Formats `write_text()` writes, each with the file extensions that select it.
TEXT_FORMATS <- list(
  markdown = c("md", "markdown")
)


# %% write_text ----
#' Write a report as text
#'
#' Write the human-readable rendering of a writeup, review or AI review to a
#' file: the text print shows, laid out for a document. [write_result] writes
#' the same objects as JSON documents for programs to read.
#'
#' Markdown output starts at `##` headings and uses pipe tables, ready to
#' convert with Pandoc or Quarto or to paste into a manuscript.
#'
#' @param x `SupervisedWriteup` (from [writeup]), `SupervisedReview` (from
#'   [review]) or `AISupervisedReview` (from [ai_review]) object.
#' @param file Character: Path of the file to write.
#' @param format Optional Character \{"markdown"\}: Output format. NULL selects
#'   the format from the extension of `file`: ".md" or ".markdown" for
#'   Markdown.
#' @param overwrite Logical: If TRUE, replace an existing file.
#' @param verbosity Integer: Verbosity level.
#' @param ... Not used.
#'
#' @return The path of the file written, invisibly.
#'
#' @author EDG
#' @export
#'
#' @examples
#' idx <- c(1:40, 51:90, 101:140)
#' mod <- train(
#'   iris[idx, ],
#'   dat_test = iris[-idx, ],
#'   hyperparameters = setup_CART(),
#'   verbosity = 0L
#' )
#' write_text(writeup(mod), file.path(tempdir(), "writeup.md"))
#' write_text(review(mod), file.path(tempdir(), "review.md"))
write_text <- new_generic(
  "write_text",
  "x",
  function(
    x,
    file,
    format = NULL,
    overwrite = FALSE,
    verbosity = 1L,
    ...
  ) {
    # See the generics note in `030_init.R`.
    force_supplied()
    S7_dispatch()
  }
) # /rtemis::write_text


# %% resolve_text_format ----
#' Resolve the output format of `write_text()`
#'
#' @param format Optional Character: Format name, one of `names(TEXT_FORMATS)`.
#' @param file Character: Output path, whose extension selects the format when
#'   `format` is NULL.
#'
#' @return Character scalar: The format name.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_text_format <- function(format, file) {
  if (!is.null(format)) {
    rtemis.core::check_character_scalar(format)
    rtemis.core::check_enum(format, names(TEXT_FORMATS))
    return(format)
  }
  ext <- tolower(tools::file_ext(file))
  match <- names(TEXT_FORMATS)[vapply(
    TEXT_FORMATS,
    function(exts) ext %in% exts,
    logical(1L)
  )]
  if (length(match) == 0L) {
    rtemis.core::abort(
      "Cannot select a format from the extension of ",
      file,
      ". Set `format` to one of: ",
      paste0("\"", names(TEXT_FORMATS), "\"", collapse = ", "),
      "; or use one of the extensions: ",
      paste0(".", unlist(TEXT_FORMATS), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  match
} # /rtemis::resolve_text_format


# %% write_text.TextReport ----
method(
  write_text,
  new_union(SupervisedWriteup, SupervisedReview, AISupervisedReview)
) <- function(
  x,
  file,
  format = NULL,
  overwrite = FALSE,
  verbosity = 1L,
  ...
) {
  rtemis.core::check_character_scalar(file)
  rtemis.core::check_logical_scalar(overwrite)
  format <- resolve_text_format(format, file)
  if (file.exists(file) && !overwrite) {
    rtemis.core::abort(
      "File ",
      file,
      " exists. Set `overwrite = TRUE` to replace it.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  text <- switch(format, markdown = to_markdown(x))
  writeLines(text, file, sep = "", useBytes = FALSE)
  if (verbosity > 0L) {
    msg0("Wrote ", format, " to ", file, ".")
  }
  invisible(file)
} # /rtemis::write_text.TextReport
