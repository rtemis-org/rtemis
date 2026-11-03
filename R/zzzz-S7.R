# zzzz-S7.R
# ::rtemis::
# 2026- EDG rtemis.org

# S7 releases after 0.2.2 export `S7_on_build()`, which removes the temporary
# bindings that method registration leaves in the namespace. It runs at install
# time after every method declaration, so this file collates after every file
# that declares an S7 method.
if ("S7_on_build" %in% getNamespaceExports("S7")) {
  getExportedValue("S7", "S7_on_build")()
}
