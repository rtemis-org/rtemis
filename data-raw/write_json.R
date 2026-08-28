# write_json.R
# ::rtemis::
# 2026- EDG rtemis.org

# How every generated artifact is serialized. One definition, sourced by each
# generator, so that every document published to the registry writes a null, an
# NA and a long double the same way.

# %% write_json_document ----
# One document, serialized the way `defaults/v1` already is.
write_json_document <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_lines(
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      pretty = TRUE,
      na = "null",
      null = "null",
      digits = NA
    )),
    file = path,
    overwrite = TRUE,
    verbosity = 0L
  )
} # /write_json_document
