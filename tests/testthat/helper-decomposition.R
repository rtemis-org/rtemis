# helper-decomposition.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% shared_decomposition ----
# Decompositions of the four iris measurements, shared by every test file in a
# process that needs a fitted UMAP or NMF decomposition without testing the
# fit's own settings. uwot runs three full garbage collections per fit and
# `NMF::nmf()` one per run, and a full collection costs in proportion to the
# heap the suite has built up, so each fit happens once, on first use, after
# the calling test's `skip_if_not_installed()`.
shared_decomposition <- local({
  fits <- list()
  configs <- list(
    # Two algorithm threads: the thread test in `test_Decomposition.R` applies
    # this fit under other thread counts.
    UMAP = function() {
      decomp(
        iris[, -5],
        config = setup_UMAP(n_epochs = 50L),
        execution_config = setup_SerialExecution(n_workers_algorithm = 2L),
        verbosity = 0L
      )
    },
    NMF = function() {
      decomp(iris[, -5], config = setup_NMF(k = 3L), verbosity = 0L)
    },
    # Full rank under a method other than the default.
    NMF_lee_full_rank = function() {
      decomp(
        iris[, -5],
        config = setup_NMF(k = 4L, method = "lee"),
        verbosity = 0L
      )
    }
  )
  function(name) {
    if (is.null(fits[[name]])) {
      fits[[name]] <<- configs[[name]]()
    }
    fits[[name]]
  }
})
