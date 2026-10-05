# decomp_PCoA.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% pcoa_project ----
#' Principal coordinates from squared dissimilarities to the training cases
#'
#' Gower's (1968) out-of-sample formula. With `b` the diagonal of the doubly
#' centered matrix `B = -J A J / 2` of squared training dissimilarities `A`,
#' training coordinates `X` and their eigenvalues `L`, a case whose squared
#' dissimilarities to the training cases are `d` has coordinates
#' `(b - d) %*% X %*% diag(1 / L) / 2`.
#'
#' For a training case `i`, `b - A[i, ]` is `2 * B[i, ] - b[i]`, the constant
#' term vanishes because the columns of `X` sum to zero, and `B %*% X = X L`, so
#' the formula returns `X[i, ]` exactly. That identity holds for any symmetric
#' dissimilarity with a zero diagonal, Euclidean or not, which is why the fit's
#' components are computed through this function too: fitting and applying are
#' then the same map.
#'
#' @param d2 Numeric matrix: Squared dissimilarities, cases by training cases.
#' @param decom List: The fit, as built by `decomp_.PCoAConfig`.
#'
#' @return Numeric matrix: Principal coordinates, cases by `k`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
pcoa_project <- function(d2, decom) {
  centered <- sweep(-d2, 2L, decom[["b"]], FUN = "+")
  transformed <- 0.5 * centered %*% decom[["points"]]
  transformed <- sweep(transformed, 2L, decom[["values"]], FUN = "/")
  colnames(transformed) <- paste0("PCoA_", seq_len(NCOL(transformed)))
  transformed
} # /rtemis::pcoa_project


# %% decomp_.PCoAConfig ----
#' Principal Coordinates Analysis
#'
#' @details
#' The fit keeps the training feature matrix, because applying it to new data
#' needs the dissimilarities from each new case to every training case.
#'
#' @keywords internal
#' @noRd
method(decomp_, PCoAConfig) <- function(
  config,
  x,
  execution_config = NULL,
  verbosity = 1L
) {
  # Checks ----
  check_is_S7(config, PCoAConfig)
  check_dependencies("vegan")
  check_unsupervised_data(x = x, allow_missing = FALSE, verbosity = verbosity)
  k <- config[["k"]]
  dist_method <- config[["dist_method"]]
  xm <- as.matrix(x)
  if (k > nrow(xm) - 1L) {
    rtemis.core::abort(
      "PCoA extracts at most n - 1 = ",
      nrow(xm) - 1L,
      " components from ",
      nrow(xm),
      " cases; set k to at most ",
      nrow(xm) - 1L,
      ".",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }

  # Decompose ----
  msg("Decomposing with", config@algorithm, "...", verbosity = verbosity)
  dst <- vegdist_matrix(xm, dist_method)
  # cmdscale warns and returns fewer columns when fewer than k eigenvalues are
  # positive; the check below replaces that warning with an error that states
  # the usable number, using a tolerance relative to the largest eigenvalue.
  fit <- withCallingHandlers(
    stats::cmdscale(dst, k = k, eig = TRUE),
    warning = function(w) {
      if (grepl("eigenvalues are > 0", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  check_inherits(fit, "list")
  eig <- fit[["eig"]]
  n_positive <- sum(eig > sqrt(.Machine[["double.eps"]]) * max(eig))
  if (k > n_positive) {
    rtemis.core::abort(
      "PCoA with dist_method = \"",
      dist_method,
      "\" found ",
      n_positive,
      " dimensions with positive eigenvalues; set k to at most ",
      n_positive,
      ".",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  d2 <- as.matrix(dst)^2
  decom <- list(
    x = xm,
    points = unname(fit[["points"]][, seq_len(k), drop = FALSE]),
    values = eig[seq_len(k)],
    eig = eig,
    b = rowMeans(d2) - mean(d2) / 2
  )
  transformed <- pcoa_project(d2, decom)
  list(decom = decom, transformed = transformed)
} # /rtemis::decomp_.PCoAConfig


# %% apply_decomp_.PCoAConfig ----
#' Apply a fitted PCoA to new data
#'
#' @details
#' Computes the dissimilarities from each case of `new_data` to the training
#' cases and projects them with Gower's formula, `pcoa_project()`.
#'
#' @param config `PCoAConfig` object.
#' @param decom List: The fit, as built by `decomp_.PCoAConfig`.
#' @param new_data Tabular data: New data to project.
#' @param execution_config Optional `ExecutionConfig`: Where and with what the
#' work runs.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Numeric matrix: Principal coordinates, cases by components.
#'
#' @keywords internal
#' @noRd
method(apply_decomp_, PCoAConfig) <- function(
  config,
  decom,
  new_data,
  execution_config = NULL,
  verbosity = 1L
) {
  check_dependencies("vegan")
  d <- vegdist_cross(
    as.matrix(new_data),
    decom[["x"]],
    config[["dist_method"]]
  )
  pcoa_project(d^2, decom)
} # /rtemis::apply_decomp_.PCoAConfig
