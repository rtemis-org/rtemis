# algorithmDB.R
# ::rtemis::
# 2025- EDG rtemis.org

# Supervised Learning ----
supervised_algorithms <- data.frame(rbind(
  c("BART", "Bayesian Additive Regression Trees", TRUE, TRUE, FALSE),
  c("CART", "Classification and Regression Trees", TRUE, TRUE, TRUE),
  c(
    "ConditionalSuperLearner",
    "Conditional SuperLearner",
    TRUE,
    TRUE,
    FALSE
  ),
  c("GAM", "Generalized Additive Model", TRUE, TRUE, FALSE),
  c("GLM", "Generalized Linear Model", TRUE, TRUE, FALSE),
  c("GLMNET", "Elastic Net", TRUE, TRUE, TRUE),
  c("HAL", "Highly Adaptive Lasso", TRUE, TRUE, FALSE),
  c("Isotonic", "Isotonic Regression", TRUE, TRUE, FALSE),
  c("MonotonicHAL", "Monotonic Highly Adaptive Lasso", TRUE, TRUE, FALSE),
  c("KNN", "k-Nearest Neighbors", TRUE, TRUE, FALSE),
  c("LightCART", "Decision Tree", TRUE, TRUE, FALSE),
  c("LightGBM", "Gradient Boosting", TRUE, TRUE, FALSE),
  c("LightRF", "LightGBM Random Forest", TRUE, TRUE, FALSE),
  c("LightRuleFit", "LightGBM RuleFit", TRUE, TRUE, FALSE),
  c(
    "MARS",
    "Multivariate Adaptive Regression Splines",
    TRUE,
    TRUE,
    FALSE
  ),
  c("MLP", "Multilayer Perceptron", TRUE, TRUE, FALSE),
  c("ModalityStacking", "Per-Modality Stacked Ensemble", TRUE, TRUE, FALSE),
  c("NNLS", "Non-negative Least Squares", TRUE, TRUE, FALSE),
  c("Ranger", "Random Forest", TRUE, TRUE, FALSE),
  c("SuperLearner", "Cross-validated Stacked Ensemble", TRUE, TRUE, FALSE),
  c(
    "LinearSVM",
    "Support Vector Machine with Linear Kernel",
    TRUE,
    TRUE,
    FALSE
  ),
  c(
    "RadialSVM",
    "Support Vector Machine with Radial Kernel",
    TRUE,
    TRUE,
    FALSE
  ),
  c("SPLS", "Sparse Partial Least Squares", TRUE, TRUE, FALSE),
  c("TabNet", "Attentive Interpretable Tabular Learning", TRUE, TRUE, FALSE)
))
colnames(supervised_algorithms) <- c(
  "name",
  "description",
  "class",
  "reg",
  "surv"
)

supervised_multiclass <- c(
  "GLMNET",
  "CART",
  "KNN",
  "LightCART",
  "LightRF",
  "LightGBM",
  "LinearSVM",
  "MARS",
  "MLP",
  "RadialSVM",
  "Ranger",
  "SPLS"
)

# %% explanation_algorithms ----
# What `explain()` does to each algorithm, and whether the answer is exact.
#
# Applicability is data, not dispatch: the estimator cannot be chosen from the
# fitted object's class, because one backend class can back two algorithms that
# warrant different estimators -- LinearSVM and RadialSVM are both `e1071::svm`.
# So `explain()` reads the estimator from here and tells `explain_super()` what
# to compute, which is the opposite of `varimp_super()`'s arrangement.
#
# `exact` is NA where exactness is a property of the *fitted* model rather than
# of the algorithm: SPLS and LinearSVM have a single linear map only for a
# regression or binary fit -- a multiclass one is scores identified up to a
# constant, and one-vs-one voting, respectively; and MARS and HAL are additive
# in their features only when the fit selected no term reading two of them,
# which for both is a search setting rather than a property of the algorithm.
# Each is checked at explain time and refused with the reason, rather than
# approximated.
#
# The three ensembles take the kernel estimator rather than a weighted sum of
# their bases' explanations. The sum is exact for a linear meta-learner, but it
# needs every base to answer the *same* question, and rtemis's exact estimators
# are each locked to one value function -- LinearSHAP is interventional-only,
# LightGBM's TreeSHAP conditional-only -- so a mixed library has none in common.
# See `plan/explain.md`.
#
# Only seven algorithms are unconditionally exact. That is the honest shape of
# this: exactness is usually a property of the fitted model, and this column's
# job is to say which rather than to promise.
#
# Built column-wise so `exact` is `logical`, for the reason `decom_algorithms`
# gives.
explanation_algorithms <- data.frame(
  name = c(
    "BART",
    "CART",
    "ConditionalSuperLearner",
    "GAM",
    "GLM",
    "GLMNET",
    "HAL",
    "Isotonic",
    "MonotonicHAL",
    "KNN",
    "LightCART",
    "LightGBM",
    "LightRF",
    "LightRuleFit",
    "MARS",
    "MLP",
    "ModalityStacking",
    "NNLS",
    "Ranger",
    "SuperLearner",
    "LinearSVM",
    "RadialSVM",
    "SPLS",
    "TabNet"
  ),
  estimator = c(
    "KernelSHAP",
    "TreeSHAP",
    "ExpertSHAP",
    "GAMTerms",
    "LinearSHAP",
    "LinearSHAP",
    "HALBasis",
    "Isotonic",
    "HALBasis",
    "KernelSHAP",
    "TreeSHAP",
    "TreeSHAP",
    "TreeSHAP",
    "KernelSHAP",
    "MARSBasis",
    "KernelSHAP",
    "KernelSHAP",
    "LinearSHAP",
    "KernelSHAP",
    "KernelSHAP",
    "LinearSHAP",
    "KernelSHAP",
    "LinearSHAP",
    "KernelSHAP"
  ),
  exact = c(
    FALSE,
    TRUE,
    NA,
    TRUE,
    TRUE,
    TRUE,
    NA,
    TRUE,
    NA,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    NA,
    FALSE,
    FALSE,
    TRUE,
    FALSE,
    FALSE,
    NA,
    FALSE,
    NA,
    FALSE
  ),
  rationale = c(
    "No additive structure to read; the posterior gives a better uncertainty story than SHAP does.",
    "TreeSHAP over a single tree is a short traversal and is exact.",
    "An oracle routes each case to one expert, which predicts it alone -- so the expert explains it, exactly, with its own estimator.",
    "predict(type = 'terms') already returns the additive decomposition.",
    "Coefficients give phi_j = beta_j * (x_j - E[x_j]) in closed form.",
    "Coefficients give phi_j = beta_j * (x_j - E[x_j]) in closed form.",
    "A lasso over basis functions, exact where every selected basis reads one feature -- which needs max_degree = 1.",
    "One feature carries the whole deviation from the baseline.",
    "A lasso over basis functions, exact where every selected basis reads one feature -- which needs max_degree = 1.",
    "No parametric structure to read; distances are not additive in the features.",
    "TreeSHAP over a single tree is a short traversal and is exact.",
    "predict(type = 'contrib') returns contributions from the booster itself.",
    "predict(type = 'contrib') returns contributions from the booster itself.",
    "A sparse linear model over rules, not a booster; a rule is a conjunction of conditions, so it is not additive in single features.",
    "Additive in its basis functions for a regression fit; classification adds a GLM over them, and an interaction term reads two features.",
    "A network has no additive structure to read; Expected Gradients would be cheaper but is deferred.",
    "A weighted sum of base explanations would be exact for a linear meta-learner, but needs every base to answer the same question; see the plan.",
    "Coefficients give phi_j = beta_j * (x_j - E[x_j]) in closed form.",
    "TreeSHAP over ranger's forest is possible but unimplemented; see the plan's open question 2.",
    "A weighted sum of base explanations would be exact for a linear meta-learner, but needs every base to answer the same question; see the plan.",
    "The decision function is linear for a regression or binary fit; multiclass is one-vs-one voting.",
    "The kernel makes the decision function nonlinear in the features.",
    "Linear in the features for regression and binary; multiclass scores are identified only up to a constant.",
    "Attention masks are native and free but are not Shapley values."
  ),
  stringsAsFactors = FALSE
)

# The value function each algorithm's default estimator computes, derived rather
# than restated so the two cannot disagree.
explanation_algorithms[["perturbation"]] <- unname(
  SHAP_ESTIMATOR_PERTURBATION[explanation_algorithms[["estimator"]]]
)


# %% explanation_methods ----
#' Explanation Methods per Algorithm
#'
#' What [explain] does to a model fitted by each supervised algorithm: which
#' estimator `setup_SHAP(estimator = "auto")` resolves to, whether that answer
#' is exact, which value function it computes, and why.
#'
#' @details
#' The columns:
#'
#' \describe{
#'   \item{`name`}{The algorithm, as named by `setup_<name>()`.}
#'   \item{`estimator`}{The estimator `"auto"` resolves to. `"KernelSHAP"` is
#'     the model-agnostic fallback and applies to every algorithm; the others
#'     are exact and algorithm-specific.}
#'   \item{`exact`}{TRUE if the estimator returns Shapley values rather than an
#'     estimate of them, NA where that depends on the fitted model rather than
#'     on the algorithm -- an ensemble whose meta-learner turned out to be
#'     linear, or a model that has one linear map for a regression or binary
#'     outcome but not for a multiclass one. An NA row explains what it did
#'     when asked, and refuses with a message naming the reason when it cannot.}
#'   \item{`perturbation`}{The value function computed when
#'     `setup_SHAP(perturbation = )` is left NULL.}
#'   \item{`rationale`}{Why this estimator, in one line.}
#' }
#'
#' `estimator = "kernel"` is always available, so a row whose `exact` is FALSE
#' is saying that `estimator = "exact"` has nothing to offer for it -- not that
#' the algorithm cannot be explained.
#'
#' @param algorithm Optional Character: Name of a supervised algorithm, matched
#' case-insensitively. NULL returns every algorithm.
#'
#' @return data.frame: One row per algorithm.
#'
#' @author EDG
#' @export
#' @examples
#' explanation_methods()
#' explanation_methods("Ranger")
#' # Which algorithms have an exact estimator?
#' em <- explanation_methods()
#' em[["name"]][!is.na(em[["exact"]]) & em[["exact"]]]
explanation_methods <- function(algorithm = NULL) {
  if (is.null(algorithm)) {
    return(explanation_algorithms)
  }
  methods <- explanation_algorithms[
    explanation_algorithms[["name"]] == get_alg_name(algorithm),
    ,
    drop = FALSE
  ]
  rownames(methods) <- NULL
  methods
} # /rtemis::explanation_methods


# Algorithms whose fit is constrained monotonic non-decreasing, which is what
# makes an algorithm usable as a calibration map: a map that reorders scores
# changes the ranking of the predictions and so changes AUC. `calibrate()`
# accepts any `Hyperparameters` object, because it trains one like any other
# model, but only these carry that guarantee. The first is the default.
calibration_algorithms <- c(
  "Isotonic",
  "MonotonicHAL"
)

get_alg_name <- function(algorithm) {
  algname <- supervised_algorithms[["name"]][
    tolower(algorithm) == tolower(supervised_algorithms[["name"]])
  ]
  if (length(algname) == 0) {
    rtemis.core::abort(
      "Incorrect algorithm specified: ",
      algorithm,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  algname
}

#' Get algorithm description
#'
#' @param algorithm Character: Algorithm name.
#'
#' @return Character: Algorithm description.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
desc_alg <- function(algorithm) {
  algdesc <- supervised_algorithms[["description"]][
    tolower(algorithm) == tolower(supervised_algorithms[["name"]])
  ]
  if (length(algdesc) == 0) {
    rtemis.core::abort(
      "Incorrect algorithm specified: ",
      algorithm,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  algdesc
} # /rtemis::desc_alg

#' Algorithm description with short name
#'
#' @param algorithm Character: Algorithm name.
#'
#' @return Character: Algorithm description with short name in parentheses.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
desc_abb_alg <- function(algorithm) {
  paste0(
    desc_alg(algorithm),
    " (",
    get_alg_name(algorithm),
    ")"
  )
} # /rtemis::desc_abb_alg

get_train_fn <- function(algorithm) {
  paste0("train_", get_alg_name(algorithm))
} # /rtemis::get_train_fn

get_default_hyperparameters <- function(algorithm) {
  do.call(paste0("setup_", get_alg_name(algorithm)), list())
} # /rtemis::get_default_hyperparameters


#' Resolve a `fit` name and optional hyperparameters
#'
#' The `draw_*(fit = )` argument names an algorithm as a string, following the
#' plotting convention, so those functions need a name-to-`Hyperparameters`
#' bridge that the rest of the API does not.
#'
#' @param fit Character: Algorithm name.
#' @param fit_params Optional `Hyperparameters` object: Hyperparameters for
#' `fit`.
#'
#' @return `Hyperparameters` object.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
resolve_fit_hyperparameters <- function(fit, fit_params) {
  if (is.null(fit_params)) {
    return(get_default_hyperparameters(fit))
  }
  check_is_S7(fit_params, Hyperparameters)
  if (tolower(fit) != tolower(fit_params@algorithm)) {
    rtemis.core::abort(
      "`fit` is '",
      fit,
      "', but `fit_params` defines hyperparameters for ",
      fit_params@algorithm,
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  fit_params
} # /rtemis::resolve_fit_hyperparameters


# Clustering ----
clust_algorithms <- data.frame(rbind(
  c("CMeans", "Fuzzy C-means Clustering"),
  c("DBSCAN", "Density-based spatial clustering of applications with noise"),
  # c("EMC", "Expectation Maximization Clustering"),
  c("HardCL", "Hard Competitive Learning"),
  # c("HOPACH", "Hierarchical Ordered Partitioning And Collapsing Hybrid"),
  # c("H2OKMeans", "H2O K-Means Clustering"),
  c("KMeans", "K-Means Clustering"),
  # c("MeanShift", "Mean Shift Clustering"),
  c("NeuralGas", "Neural Gas Clustering")
  # c("PAM", "Partitioning Around Medoids"),
  # c("PAMK", "Partitioning Around Medoids with k estimation"),
  # c("SPEC", "Spectral Clustering")
))

get_clust_name <- function(algorithm) {
  clustname <- clust_algorithms[, 1][
    tolower(algorithm) == tolower(clust_algorithms[, 1])
  ]
  if (length(clustname) == 0) {
    rtemis.core::abort(
      "Incorrect clustering algorithm specified: ",
      algorithm,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  clustname
} # /rtemis::get_clust_name

get_clust_desc <- function(algorithm) {
  clustdesc <- clust_algorithms[, 2][
    tolower(algorithm) == tolower(clust_algorithms[, 1])
  ]
  if (length(clustdesc) == 0) {
    rtemis.core::abort(
      "Incorrect clustering algorithm specified: ",
      algorithm,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  clustdesc
} # /rtemis::get_clust_desc

get_clust_fn <- function(algorithm) {
  paste0("cluster_", get_clust_name(algorithm))
} # /rtemis::get_clust_fn

get_default_clusterparams <- function(algorithm) {
  do.call(paste0("setup_", get_clust_name(algorithm)), list())
}

get_clustpredict_fn <- function(algorithm) {
  paste0("clustpredict_", get_clust_name(algorithm))
}

get_clust_setup_fn <- function(algorithm) {
  paste0("setup_", get_clust_name(algorithm))
} # /rtemis::get_clust_setup_fn


# Decomposition ----
# Admission criterion for this table: a symmetric decomposition method models
# the joint structure of one or more variables without designating any variable
# or variable set as the outcome. An algorithm that designates an outcome is a
# supervised algorithm and belongs in `supervised_algorithms` -- PLS, for
# instance. A two-block method with no outcome, such as CCA, is
# cross-decomposition and belongs in neither table, because it has neither
# `decomp()`'s shape nor `train()`'s. That criterion, not a column, is what
# decides where a new algorithm goes.
#
# Built column-wise so the logical columns are `logical`. `rbind(c(...))`
# coerces every cell to character, which is why `supervised_algorithms` carries
# "TRUE"/"FALSE" strings.
decom_algorithms <- data.frame(
  name = c("ICA", "Isomap", "NMF", "PCA", "tSNE", "UMAP"),
  description = c(
    "Independent Component Analysis",
    "Isomap",
    "Non-negative Matrix Factorization",
    "Principal Component Analysis",
    "t-distributed Stochastic Neighbor Embedding",
    "Uniform Manifold Approximation and Projection"
  ),
  linear = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
  can_apply = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
  invertible = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
  orthogonal = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  ordered = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  deterministic = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
  preserves = c(
    "variance",
    "global",
    "reconstruction",
    "variance",
    "local",
    "local"
  ),
  nonneg = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
  package = c("fastICA", "vegan", "NMF", "stats", "Rtsne", "uwot"),
  stringsAsFactors = FALSE
)

# %% decom_algorithms_applicable ----
# The algorithms whose fitted result can be applied to new data, derived so it
# cannot disagree with the table. Tests assert that `can_apply` is TRUE exactly
# when `method(apply_decomp_, <Alg>Config)` exists.
decom_algorithms_applicable <- decom_algorithms[["name"]][
  decom_algorithms[["can_apply"]]
]

# %% decom_algorithms_invertible ----
# The algorithms whose components map back to input space, derived for the same
# reason: tests assert that `invertible` is TRUE exactly when
# `method(reconstruct_, <Alg>Config)` exists.
decom_algorithms_invertible <- decom_algorithms[["name"]][
  decom_algorithms[["invertible"]]
]

get_decom_name <- function(algorithm) {
  decomname <- decom_algorithms[["name"]][
    tolower(algorithm) == tolower(decom_algorithms[["name"]])
  ]
  if (length(decomname) == 0) {
    rtemis.core::abort(
      "Incorrect decomposition algorithm specified: ",
      algorithm,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  decomname
} # /rtemis::get_decom_name

get_decom_desc <- function(algorithm) {
  decomdesc <- decom_algorithms[["description"]][
    tolower(algorithm) == tolower(decom_algorithms[["name"]])
  ]
  if (length(decomdesc) == 0) {
    rtemis.core::abort(
      "Incorrect decomposition algorithm specified: ",
      algorithm,
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  decomdesc
} # /rtemis::get_decom_desc

get_decom_fn <- function(algorithm) {
  paste0("decom_", get_decom_name(algorithm))
} # /rtemis::get_decom_fn

get_default_decomparams <- function(algorithm) {
  do.call(paste0("setup_", get_decom_name(algorithm)), list())
} # /rtemis::get_default_decomparams

get_decom_setup_fn <- function(algorithm) {
  paste0("setup_", get_decom_name(algorithm))
} # /rtemis::get_decom_setup_fn

get_decom_predict_fn <- function(algorithm) {
  paste0("predict_", get_decom_name(algorithm))
} # /rtemis::get_decom_predict_fn


# %% decomposition_traits ----
#' Decomposition Algorithm Traits
#'
#' Machine-readable properties of each decomposition algorithm available to
#' [decomp]: whether the map is linear, whether it can be applied to new data,
#' whether it inverts, and what its objective preserves.
#'
#' @details
#' The columns:
#'
#' \describe{
#'   \item{`name`}{The name used by `setup_<name>()` and `decomp(algorithm =)`.}
#'   \item{`description`}{Full name of the algorithm.}
#'   \item{`linear`}{The map from input space to component space is linear.}
#'   \item{`can_apply`}{A fitted result can be applied to new data with
#'     [apply_decomp].}
#'   \item{`invertible`}{There is an inverse map from component space back to
#'     input space, so reconstruction error is defined.}
#'   \item{`orthogonal`}{Components are mutually orthogonal.}
#'   \item{`ordered`}{Components come in a meaningful order, so "the first j"
#'     is well-defined.}
#'   \item{`deterministic`}{The same input and configuration give the same
#'     result without setting a seed.}
#'   \item{`preserves`}{What the objective preserves: `"variance"`,
#'     `"global"`, `"local"`, or `"reconstruction"`.}
#'   \item{`nonneg`}{Requires non-negative input and produces non-negative
#'     factors.}
#'   \item{`package`}{Package supplying the backend implementation.}
#' }
#'
#' `can_apply` states what rtemis implements, not what is mathematically
#' possible. Parametric tSNE exists and Isomap admits a Nystrom-style
#' out-of-sample extension; neither is implemented here, so both are `FALSE`.
#' The column's contract is that [apply_decomp] on a fitted result of this
#' algorithm returns components rather than an error.
#'
#' @param algorithm Optional Character: Name of a decomposition algorithm,
#' matched case-insensitively. `NULL` returns every algorithm.
#'
#' @return data.frame: One row per algorithm, one column per trait.
#'
#' @author EDG
#' @export
#' @examples
#' decomposition_traits()
#' decomposition_traits("PCA")
#' # Which algorithms support out-of-sample projection?
#' decomposition_traits()[["name"]][decomposition_traits()[["can_apply"]]]
decomposition_traits <- function(algorithm = NULL) {
  if (is.null(algorithm)) {
    return(decom_algorithms)
  }
  traits <- decom_algorithms[
    decom_algorithms[["name"]] == get_decom_name(algorithm),
    ,
    drop = FALSE
  ]
  rownames(traits) <- NULL
  traits
} # /rtemis::decomposition_traits


#' Available Algorithms
#'
#' Print available algorithms for supervised learning, clustering, and decomposition.
#'
#' Each algorithm is set up with `setup_{Algorithm}()`, using the name printed
#' here: `setup_LightGBM()`, `setup_KMeans()`, `setup_PCA()`. Pass the result to
#' [train], [cluster], or [decomp].
#'
#' @rdname available_algorithms
#' @aliases available_algorithms
#'
#' @param verbosity Integer: Verbosity level.
#' @return Named list of algorithm descriptions, invisibly.
#'
#' @author EDG
#' @export
#' @examples
#' available_supervised()
#' # Train with one of them, at its default hyperparameters:
#' # train(iris, hyperparameters = setup_LightGBM())
available_supervised <- function(verbosity = 1L) {
  algs <- structure(
    supervised_algorithms[["description"]],
    names = supervised_algorithms[["name"]],
    class = "list"
  )
  if (verbosity > 0L) {
    printls(algs, print_class = FALSE, limit = -1L)
  }
  invisible(algs)
}

#' @rdname available_algorithms
#' @export
#' @examples
#' available_clustering()
available_clustering <- function(verbosity = 1L) {
  algs <- structure(
    clust_algorithms[, 2],
    names = clust_algorithms[, 1],
    class = "list"
  )
  if (verbosity > 0L) {
    printls(algs, print_class = FALSE, limit = -1L)
  }
  invisible(algs)
}


#' @rdname available_algorithms
#' @export
#' @examples
#' available_calibration()
#' # Calibrate with one of them:
#' # calibrate(mod, hyperparameters = setup_Isotonic())
available_calibration <- function(verbosity = 1L) {
  # Read the descriptions from the supervised table rather than restating
  # them, so a calibrator is described the same way wherever it is listed.
  idx <- match(calibration_algorithms, supervised_algorithms[["name"]])
  algs <- structure(
    supervised_algorithms[["description"]][idx],
    names = supervised_algorithms[["name"]][idx],
    class = "list"
  )
  if (verbosity > 0L) {
    printls(algs, print_class = FALSE, limit = -1L)
  }
  invisible(algs)
}


#' @rdname available_algorithms
#'
#' @param traits Logical: If TRUE, `available_decomposition()` prints and
#' returns the full trait table from [decomposition_traits] instead of the
#' named list of descriptions.
#'
#' @export
#' @examples
#' available_decomposition()
#' available_decomposition(traits = TRUE)
available_decomposition <- function(verbosity = 1L, traits = FALSE) {
  if (traits) {
    algs <- decomposition_traits()
    if (verbosity > 0L) {
      print(algs)
    }
    return(invisible(algs))
  }
  algs <- structure(
    decom_algorithms[["description"]],
    names = decom_algorithms[["name"]],
    class = "list"
  )
  if (verbosity > 0L) {
    printls(algs, print_class = FALSE, limit = -1L)
  }
  invisible(algs)
}

# Draw ----
draw_fns <- data.frame(
  rbind(
    c("draw_3DScatter", "3D Scatter Plot"),
    c("draw_bar", "Bar Plot"),
    c("draw_box", "Box Plot"),
    c("draw_calibration", "Calibration Plot"),
    c("draw_confusion", "Confusion Matrix"),
    c("draw_dist", "Density and Histogram Plots"),
    c("draw_fit", "Scatter Plot with Fit Line alias"),
    c("draw_graphD3", "Network Graph using networkD3"),
    c("draw_graphjs", "Network Graph using graphjs"),
    c("draw_heat", "Heatmap using plotly"),
    c("draw_heatmap", "Heatmap using heatmaply"),
    c("draw_leafleat", "Choropleth Map using leaflet"),
    c("draw_pie", "Pie Chart"),
    c("draw_protein", "Amino Acid Annotation Plot"),
    c("draw_roc", "ROC Curve"),
    c("draw_scatter", "Scatter Plot"),
    c("draw_spectrogram", "Spectrogram"),
    c("draw_table", "Table using plotly"),
    c("draw_ts", "Time Series Plot"),
    c("draw_varimp", "Barplot for Variable Importance alias"),
    c("draw_volcano", "Volcano Plot"),
    c("draw_xt", "Time Series Line Plot")
  )
)
colnames(draw_fns) <- c("Function Name", "Description")


#' Available Draw Functions
#'
#' Print available draw functions for visualization.
#'
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list of draw function descriptions, invisibly.
#'
#' @author EDG
#' @export
#' @examples
#' available_draw()
available_draw <- function(verbosity = 1L) {
  fns <- structure(
    draw_fns[, 2],
    names = draw_fns[, 1],
    class = "list"
  )
  if (verbosity > 0L) {
    cat("Available draw functions:\n")
    printls(fns, print_class = FALSE, limit = -1L)
  }
  invisible(fns)
} # /rtemis::available_draw
