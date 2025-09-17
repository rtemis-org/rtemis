# factorize.R
# ::rtemis::
# 2016 EDG rtemis.org

#' Factor Analysis
#'
#' Perform parallel analysis, factor analysis, bifactor analysis
#' and hierarchical clustering.
#'
#' Consult `psych::fa` for more information on the parameters.
#'
#' @param x Data. Will be coerced to data frame
#' @param n_factors Integer: If NULL, will be estimated using parallel analysis
#' @param method Character: Factor analysis method:
#' "minres": minimum residual (OLS), "wls": weighted least squares (WLS);
#' "gls": generalized weighted least squares (GLS); "pa": principal factor solution;
#' "ml": maximum likelihood;
#' "minchi": minimize the sample size weighted chi square when treating pairwise correlations with
#' different number of subjects per pair; "minrank": minimum rank factor analysis.
#' @param rotation Character: Rotation methods.
#' No rotation: "none";
#' Orthogonal: "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor";
#' Oblique: "promax", "oblimin", "simplimax", "bentlerQ, "geominQ", "biquartimin", "cluster".
#' @param scores Character: Factor score estimation method. Options: "regression",
#' "Thurstone": simple regression, "tenBerge": correlation-preserving, "Anderson", "Barlett".
#' @param cor Character: Correlation method:
#' "cor": Pearson correlation, "cov": Covariance, "tet": tetrachoric, "poly": polychoric,
#' "mixed": mixed cor for a mixture of tetrachorics, polychorics, Pearsons, biserials, and
#' polyserials, "Yuleb": Yulebonett, "Yuleq" and "YuleY": Yule coefficients
#' @param fa_n_iter Integer: Number of iterations for factor analysis.
#' @param omega_method Character: Factor analysis method for the bifactor analysis. Same options as
#' `method`
#' @param omega_rotation Character: Rotation method for bifactor analysis:
#' "oblimin", "simplimax", "promax", "cluster", "target".
#' @param omega_n_iter Integer: Number of iterations for bifactor analysis.
#' @param x_name Character: Name your dataset. Used for plotting
#' @param print_plot Logical: If TRUE, print plots along the way.
#' @param do_pa Logical: If TRUE, perform parallel analysis.
#' @param do_fa Logical: If TRUE, perform factor analysis.
#' @param do_bifactor Logical: If TRUE, perform bifactor analysis.
#' @param do_hclust Logical: If TRUE, perform hierarchical cluster analysis.
#' @param verbosity Integer: Verbosity level.
#' @param ... Additional arguments to pass to `psych::fa`
#'
#' @return List with the following elements:
#' \describe{
#'   \item{parallel_analysis}{Results from parallel analysis}
#'   \item{factor_analysis}{Results from factor analysis}
#'   \item{factor_scores}{Factor scores}
#'   \item{bifactor_analysis}{Results from bifactor analysis}
#'   \item{bifactor_scores}{Bifactor scores}
#'   \item{hclust}{Results from hierarchical cluster analysis}
#' }
#'
#' @author EDG
#' @export

factorize <- function(
  x,
  n_factors = NULL,
  method = "minres",
  rotation = "oblimin",
  scores = "regression",
  cor = "cor",
  fa_n_iter = 100,
  omega_method = "minres",
  omega_rotation = c("oblimin", "simplimax", "promax", "cluster", "target"),
  omega_n_iter = 1,
  x_name = NULL,
  print_plot = TRUE,
  do_pa = TRUE,
  do_fa = TRUE,
  do_bifactor = TRUE,
  do_hclust = FALSE,
  verbosity = 1L,
  ...
) {
  # Dependencies ----
  check_dependencies("psych")

  # Arguments ----
  if (is.null(x_name)) {
    x_name <- deparse(substitute(x))
  }
  if (is.null(n_factors)) {
    do_pa <- TRUE
  }
  omega_rotation <- match.arg(omega_rotation)

  # Parallel Analysis - Estimate Number of Factors ----
  if (do_pa) {
    if (verbosity > 0L) {
      msg("Running Parallel Analysis...")
    }
    parallel_analysis <- psych::fa.parallel(
      x,
      fm = method,
      main = "Parallel Analysis Scree Plot"
    )
    if (is.null(n_factors)) {
      n_factors <- parallel_analysis[["nfact"]]
      if (n_factors == 0) {
        warning("Parallel analysis returned 0 factors; will use 2")
        n_factors <- 2
      }
    } else {
      if (n_factors != parallel_analysis[["nfact"]]) {
        msg(
          n_factors,
          "requested; Parallel analysis suggests",
          parallel_analysis[["nfact"]],
          "factors."
        )
        msg("Check scree plot")
      }
    }
  } else {
    parallel_analysis <- NULL
  }
  if (verbosity > 0L) {
    msg("Using", n_factors, "factors")
  }

  # Factor Analysis ----
  if (do_fa) {
    if (verbosity > 0L) {
      msg("Running Exploratory Factor Analysis...")
    }
    x_fa <- psych::fa(
      x,
      nfactors = n_factors,
      fm = method,
      rotate = rotation,
      scores = scores,
      cor = cor,
      n.iter = fa_n_iter
    )
    # Plot factors
    if (print_plot) {
      psych::fa.diagram(x_fa, main = paste(x_name, "Factor Analysis"))
    }
    # Get factor scores
    fa_scores <- x_fa[["scores"]]
  } else {
    x_fa <- fa_scores <- NULL
  }

  # Bifactor Analysis ----
  if (do_bifactor) {
    if (verbosity > 0L) {
      msg("Running Bifactor Analysis...")
    }
    x_omega <- psych::omegaSem(
      x,
      nfactors = n_factors,
      fm = omega_method,
      n.iter = omega_n_iter,
      rotate = omega_rotation,
      plot = print_plot,
      title = paste(x_name, "Bifactor Analysis")
    )
    # Bifactor scores
    bifactor_scores <- x_omega[["omegaSem"]][["scores"]][, 1]
  } else {
    x_omega <- bifactor_scores <- NULL
  }

  # Hierarchical Cluster Analysis ----
  if (do_hclust) {
    if (verbosity > 0L) {
      msg("Performing hierarchical cluster analysis...")
    }
    x_pvclust <- pvclust::pvclust(x)
    if (print_plot) plot(x_pvclust)
  } else {
    x_pvclust <- NULL
  }

  # Output ----
  out <- list(
    parallel_analysis = parallel_analysis,
    factor_analysis = x_fa,
    factor_scores = fa_scores,
    bifactor_analysis = x_omega,
    bifactor_scores = bifactor_scores,
    hclust = x_pvclust
  )
  out
} # /rtemis::factorize
