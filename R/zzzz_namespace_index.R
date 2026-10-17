# zzzz_namespace_index.R
# ::rtemis::
# 2026- EDG rtemis.org

# Names of the namespace bindings that hold an S7 class, and of those that hold
# a published class, computed when the namespace is built, after every class
# definition; this file collates last for that reason. Code that searches the
# namespace for classes reads these bindings alone: reading every binding
# deserializes each function and generic from the lazy-load database, and the
# S7 method signatures of the generics hold copies of the classes they
# dispatch on.

# %% .class_names ----
.class_names <- local({
  ns <- topenv()
  names <- ls(ns, all.names = TRUE)
  names[vapply(
    names,
    function(name) {
      inherits(get(name, envir = ns, inherits = FALSE), "S7_class")
    },
    logical(1L)
  )]
})


# %% .published_class_names ----
.published_class_names <- local({
  ns <- topenv()
  .class_names[vapply(
    .class_names,
    function(name) {
      cls <- get(name, envir = ns, inherits = FALSE)
      !is.null(attr(cls, "rtemis_schema", exact = TRUE))
    },
    logical(1L)
  )]
})
