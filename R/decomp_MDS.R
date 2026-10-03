# decomp_MDS.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% mds_starts ----
#' Starting configurations for MDS
#'
#' The first start is the classical scaling solution, with uniform random
#' columns in place of any of the `k` dimensions whose eigenvalue is not
#' positive; the others are uniform on `[-1, 1]`. Every random value is drawn
#' from R's generator, which `decomp()` seeds from the execution config.
#'
#' @param dst `dist` object: Dissimilarities.
#' @param k Integer: Number of dimensions.
#' @param nstart Integer: Number of starts.
#'
#' @return List of `nstart` numeric matrices, cases by `k`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
mds_starts <- function(dst, k, nstart) {
  n <- attr(dst, "Size")
  classical <- suppressWarnings(stats::cmdscale(dst, k = k))
  if (NCOL(classical) < k) {
    n_missing <- k - NCOL(classical)
    classical <- cbind(
      classical,
      matrix(stats::runif(n * n_missing, -1, 1), nrow = n, ncol = n_missing)
    )
  }
  starts <- vector("list", nstart)
  starts[[1L]] <- unname(classical)
  for (i in seq_len(nstart - 1L)) {
    starts[[i + 1L]] <- matrix(stats::runif(n * k, -1, 1), nrow = n, ncol = k)
  }
  starts
} # /rtemis::mds_starts


# %% decomp_.MDSConfig ----
#' Multidimensional Scaling
#'
#' @details
#' Runs `vegan::monoMDS()` from each of `nstart` starting configurations and
#' keeps the fit with the lowest stress.
#'
#' @keywords internal
#' @noRd
method(decomp_, MDSConfig) <- function(
  config,
  x,
  execution_config = NULL,
  verbosity = 1L
) {
  # Checks ----
  check_is_S7(config, MDSConfig)
  check_dependencies("vegan")
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)
  k <- config[["k"]]
  xm <- as.matrix(x)
  if (k > nrow(xm) - 1L) {
    rtemis.core::abort(
      "MDS places ",
      nrow(xm),
      " cases in at most n - 1 = ",
      nrow(xm) - 1L,
      " dimensions; set k to at most ",
      nrow(xm) - 1L,
      ".",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }

  # Decompose ----
  msg("Decomposing with", config@algorithm, "...", verbosity = verbosity)
  dst <- vegdist_matrix(xm, config[["dist_method"]])
  decom <- NULL
  for (start in mds_starts(dst, k = k, nstart = config[["nstart"]])) {
    fit <- vegan::monoMDS(
      dst,
      y = start,
      k = k,
      model = config[["model"]],
      maxit = config[["max_iter"]]
    )
    check_inherits(fit, "monoMDS")
    if (is.null(decom) || fit[["stress"]] < decom[["stress"]]) {
      decom <- fit
    }
  }
  msg(
    "Lowest stress over",
    config[["nstart"]],
    "starts:",
    ddSci(decom[["stress"]]),
    verbosity = verbosity
  )
  # monoMDS stopping code 1: the iteration limit was reached.
  if (decom[["icause"]] == 1L) {
    rtemis.core::warn(
      "The lowest-stress MDS fit stopped at max_iter = ",
      config[["max_iter"]],
      " iterations before converging. Increase max_iter."
    )
  }
  transformed <- unname(decom[["points"]])
  colnames(transformed) <- paste0("MDS_", seq_len(NCOL(transformed)))
  list(decom = decom, transformed = transformed)
} # /rtemis::decomp_.MDSConfig
