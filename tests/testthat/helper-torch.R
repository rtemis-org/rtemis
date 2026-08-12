# helper-torch.R
# ::rtemis::
# 2026- EDG rtemis.org

# Every test that fits a torch-backed algorithm -- MLP, TabNet -- gates on this.
# Two things have to be present and `requireNamespace()` answers only for the
# first: the R package installs without libtorch, which it downloads on first
# use. A check that stops at the package passes on a machine where the fit then
# fails inside `torch::cuda_is_available()`, which is what device resolution
# reaches before any module is built.

# %% torch_available ----
torch_available <- function() {
  requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed()
}


# %% skip_if_no_torch ----
skip_if_no_torch <- function() {
  testthat::skip_if_not(torch_available(), "libtorch is not installed")
}
