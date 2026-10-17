# zzzz_schema_catalog.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% .published_class_names ----
# Names of the namespace bindings that hold a published class, computed when
# the namespace is built, after every class definition. `schema_catalog()`
# reads these bindings alone: reading every binding deserializes each function
# and generic from the lazy-load database, and the S7 method signatures of the
# generics hold copies of the classes they dispatch on.
.published_class_names <- local({
  ns <- topenv()
  names <- ls(ns, all.names = TRUE)
  names[vapply(
    names,
    function(name) {
      x <- get(name, envir = ns, inherits = FALSE)
      inherits(x, "S7_class") &&
        !is.null(attr(x, "rtemis_schema", exact = TRUE))
    },
    logical(1L)
  )]
})
