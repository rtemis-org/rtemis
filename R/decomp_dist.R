# decomp_dist.R
# ::rtemis::
# 2026- EDG rtemis.org

# Dissimilarities for the distance-based decompositions, MDS and PCoA. The
# vocabulary, `vegdist_methods`, is declared with their configs.

# %% check_vegdist_input ----
#' Check data against the requirements of a vegdist method
#'
#' `vegan::vegdist()` warns rather than errors on negative entries for methods
#' defined only on non-negative data, and returns non-finite dissimilarities
#' for an all-zero case under a method that normalizes each case. Both are
#' rejected here, before computing, so that neither reaches an
#' eigendecomposition or a stress fit.
#'
#' @param xm Numeric matrix: Data, cases by features.
#' @param dist_method Character: One of `vegdist_methods`.
#'
#' @return `xm`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_vegdist_input <- function(xm, dist_method) {
  if (dist_method %in% vegdist_methods_nonneg && any(xm < 0)) {
    rtemis.core::abort(
      "dist_method = \"",
      dist_method,
      "\" is defined for non-negative data only, and the data has ",
      sum(xm < 0),
      " negative values. Use \"euclidean\", \"manhattan\" or \"chord\", or ",
      "transform the data to be non-negative.",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  if (dist_method %in% vegdist_methods_nonzero) {
    zero <- which(rowSums(xm != 0) == 0L)
    if (length(zero) > 0L) {
      rtemis.core::abort(
        "dist_method = \"",
        dist_method,
        "\" is undefined for a case whose values are all zero, and ",
        length(zero),
        " cases are (rows ",
        paste(utils::head(zero, 10L), collapse = ", "),
        if (length(zero) > 10L) ", ...",
        "). Remove those cases or use a different dist_method.",
        class = c("rtemis_value_error", "rtemis_data_error")
      )
    }
  }
  invisible(xm)
} # /rtemis::check_vegdist_input


# %% check_vegdist_finite ----
#' Reject non-finite dissimilarities
#'
#' The input checks in `check_vegdist_input()` cover every case known to give
#' an undefined dissimilarity; this is the backstop for any other.
#'
#' @param d Numeric matrix or `dist` object: Dissimilarities.
#' @param dist_method Character: The vegdist method that produced `d`.
#'
#' @return `d`, invisibly.
#'
#' @author EDG
#' @keywords internal
#' @noRd
check_vegdist_finite <- function(d, dist_method) {
  n_bad <- sum(!is.finite(d))
  if (n_bad > 0L) {
    rtemis.core::abort(
      "dist_method = \"",
      dist_method,
      "\" gave ",
      n_bad,
      " undefined dissimilarities. Check the data for cases the method ",
      "cannot compare, or use a different dist_method.",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  invisible(d)
} # /rtemis::check_vegdist_finite


# %% vegdist_matrix ----
#' Dissimilarities between the cases of one matrix
#'
#' @param xm Numeric matrix: Data, cases by features.
#' @param dist_method Character: One of `vegdist_methods`.
#'
#' @return `dist` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
vegdist_matrix <- function(xm, dist_method) {
  check_vegdist_input(xm, dist_method)
  d <- suppressWarnings(vegan::vegdist(xm, method = dist_method))
  check_vegdist_finite(d, dist_method)
  d
} # /rtemis::vegdist_matrix


# %% vegdist_cross ----
#' Dissimilarities from new cases to reference cases
#'
#' `vegan::vegdist()` computes all pairs within one matrix, so the cross block
#' is cut from the dissimilarities of the stacked matrix. That is exact only
#' because every method in `vegdist_methods` is a function of the two cases
#' alone. New cases are processed in chunks so that memory stays within a small
#' multiple of the reference block's.
#'
#' @param new Numeric matrix: New cases by features.
#' @param ref Numeric matrix: Reference cases by the same features.
#' @param dist_method Character: One of `vegdist_methods`.
#'
#' @return Numeric matrix: `nrow(new)` by `nrow(ref)` dissimilarities.
#'
#' @author EDG
#' @keywords internal
#' @noRd
vegdist_cross <- function(new, ref, dist_method) {
  check_vegdist_input(new, dist_method)
  n_ref <- nrow(ref)
  n_new <- nrow(new)
  chunk_size <- max(64L, n_ref %/% 2L)
  starts <- seq.int(1L, n_new, by = chunk_size)
  out <- matrix(NA_real_, nrow = n_new, ncol = n_ref)
  for (start in starts) {
    rows <- seq.int(start, min(start + chunk_size - 1L, n_new))
    stacked <- rbind(ref, new[rows, , drop = FALSE])
    d <- as.matrix(suppressWarnings(
      vegan::vegdist(stacked, method = dist_method)
    ))
    out[rows, ] <- d[n_ref + seq_along(rows), seq_len(n_ref), drop = FALSE]
  }
  check_vegdist_finite(out, dist_method)
  out
} # /rtemis::vegdist_cross
