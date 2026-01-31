# algorithmDB.R
# ::rtemis::
# 2025 EDG rtemis.org

# Supervised Learning ----
supervised_algorithms <- data.frame(rbind(
  c("CART", "Classification and Regression Trees", TRUE, TRUE, TRUE),
  c("GAM", "Generalized Additive Model", TRUE, TRUE, FALSE),
  c("GLM", "Generalized Linear Model", TRUE, TRUE, FALSE),
  c("GLMNET", "Elastic Net", TRUE, TRUE, TRUE),
  c("Isotonic", "Isotonic Regression", TRUE, TRUE, FALSE),
  c("LightCART", "Decision Tree", TRUE, TRUE, FALSE),
  c("LightGBM", "Gradient Boosting", TRUE, TRUE, FALSE),
  c("LightRF", "LightGBM Random Forest", TRUE, TRUE, FALSE),
  c("LightRuleFit", "LightGBM RuleFit", TRUE, TRUE, FALSE),
  c("Ranger", "Random Forest", TRUE, TRUE, FALSE),
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
  c("TabNet", "Attentive Interpretable Tabular Learning", TRUE, TRUE, FALSE)
))
colnames(supervised_algorithms) <- c(
  "Name",
  "Description",
  "Class",
  "Reg",
  "Surv"
)

supervised_multiclass <- c(
  "GLMNET",
  "CART",
  "LightCART",
  "LightRF",
  "LightGBM",
  "LinearSVM",
  "RadialSVM",
  "Ranger"
)

get_alg_name <- function(algorithm) {
  algname <- supervised_algorithms[, 1][
    tolower(algorithm) == tolower(supervised_algorithms[, 1])
  ]
  if (length(algname) == 0) {
    cli::cli_abort(algorithm, "Incorrect algorithm specified")
  }
  algname
}

get_alg_setup <- function(algorithm) {
  paste0("setup_", get_alg_name(algorithm))
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
  algdesc <- supervised_algorithms[, 2][
    tolower(algorithm) == tolower(supervised_algorithms[, 1])
  ]
  if (length(algdesc) == 0) {
    cli::cli_abort(algorithm, "Incorrect algorithm specified")
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

get_default_hyperparameters <- function(algorithm, type, ncols) {
  alg_name <- get_alg_name(algorithm)
  if (alg_name == "LightRF") {
    setup_LightRF(
      feature_fraction = if (type == "Classification") {
        sqrt(ncols) / ncols
      } else {
        0.33
      }
    )
  } else {
    do.call(paste0("setup_", get_alg_name(algorithm)), list())
  }
} # /rtemis::get_default_hyperparameters

get_predict_fn <- function(algorithm) {
  paste0("predict_", get_alg_name(algorithm))
}

get_se_fn <- function(algorithm) {
  paste0("se_", get_alg_name(algorithm))
}

# get_predict_prob_fn <- function(algorithm) {
#   paste0("predict_prob_", get_alg_name(algorithm))
# }

get_varimp_fn <- function(algorithm) {
  paste0("varimp_", get_alg_name(algorithm))
}

# use e.g. in draw_scatter
setup_alg <- function(algorithm, ...) {
  alg_name <- get_alg_name(algorithm)
  setup_fn <- get_alg_setup(algorithm)
  do_call(setup_fn, list(...))
} # /rtemis::setup_alg

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
    cli::cli_abort(algorithm, "Incorrect clustering algorithm specified")
  }
  clustname
} # /rtemis::get_clust_name

get_clust_desc <- function(algorithm) {
  clustdesc <- clust_algorithms[, 2][
    tolower(algorithm) == tolower(clust_algorithms[, 1])
  ]
  if (length(clustdesc) == 0) {
    cli::cli_abort(algorithm, "Incorrect clustering algorithm specified")
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
decom_algorithms <- data.frame(rbind(
  # c("H2OAE", "H2O Autoencoder"),
  # c("H2OGLRM", "H2O Generalized Low-Rank Model"),
  c("ICA", "Independent Component Analysis"),
  c("Isomap", "Isomap"),
  # c("KPCA", "Kernel Principal Component Analysis"),
  # c("LLE", "Locally Linear Embedding"),
  # c("MDS", "Multidimensional Scaling"),
  c("NMF", "Non-negative Matrix Factorization"),
  c("PCA", "Principal Component Analysis"),
  # c("SPCA", "Sparse Principal Component Analysis"),
  # c("SVD", "Singular Value Decomposition"),
  c("tSNE", "t-distributed Stochastic Neighbor Embedding"),
  c("UMAP", "Uniform Manifold Approximation and Projection")
))

get_decom_name <- function(algorithm) {
  decomname <- decom_algorithms[, 1][
    tolower(algorithm) == tolower(decom_algorithms[, 1])
  ]
  if (length(decomname) == 0) {
    cli::cli_abort(algorithm, "Incorrect decomposition algorithm specified")
  }
  decomname
} # /rtemis::get_decom_name

get_decom_desc <- function(algorithm) {
  decomdesc <- decom_algorithms[, 2][
    tolower(algorithm) == tolower(decom_algorithms[, 1])
  ]
  if (length(decomdesc) == 0) {
    cli::cli_abort(algorithm, "Incorrect decomposition algorithm specified")
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


#' Available Algorithms
#'
#' Print available algorithms for supervised learning, clustering, and decomposition.
#'
#' @rdname available_algorithms
#' @aliases available_algorithms
#'
#' @return Called for its side effect of printing available algorithms.
#'
#' @author EDG
#'
#' @export
available_supervised <- function() {
  algs <- structure(
    supervised_algorithms[, 2],
    names = supervised_algorithms[, 1],
    class = "list"
  )
  printls(algs, print_class = FALSE, limit = -1L)
  invisible()
}

#' @rdname available_algorithms
#' @export
available_clustering <- function() {
  algs <- structure(
    clust_algorithms[, 2],
    names = clust_algorithms[, 1],
    class = "list"
  )
  printls(algs, print_class = FALSE, limit = -1L)
  invisible()
}

#' @rdname available_algorithms
#' @export
available_decomposition <- function() {
  algs <- structure(
    decom_algorithms[, 2],
    names = decom_algorithms[, 1],
    class = "list"
  )
  printls(algs, print_class = FALSE, limit = -1L)
  invisible()
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
#' @return NULL, invisibly.
#'
#' @author EDG
#' @export
available_draw <- function() {
  fns <- structure(
    draw_fns[, 2],
    names = draw_fns[, 1],
    class = "list"
  )
  cat("Available draw functions:\n")
  printls(fns, print_class = FALSE, limit = -1L)
  invisible()
} # /rtemis::available_draw
