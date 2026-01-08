# S7_Hyperparameters.R
# ::rtemis::
# 2025 EDG rtemis.org

# References ----
# S7
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/
# LightGBM parameters
# https://lightgbm.readthedocs.io/en/latest/Parameters.html

# `tuned` values ----
# -9: Set by Tuner: Actively being tuned (Values fixed by Tuner).
# -2: Set by constructor: Not tunable (No tunable_hyperparameters).
# -1: Set by constructor: Not tunable (tunable_hyperparameters exist, but none of them have more than one value).
#  0: Set by constructor: Untuned but tunable (at least one of tunable_hyperparameters has more than one value).
#  1: Set by Tuner: Tuned (Started as 0, set to 1 when tuned).
TUNED_STATUS_TUNING <- -9L
TUNED_STATUS_NOT_TUNABLE <- -2L
TUNED_STATUS_NO_SEARCH_VALUES <- -1L
TUNED_STATUS_UNTUNED <- 0L
TUNED_STATUS_TUNED <- 1L

# `resampled` values ----
# 0: Running on single training set.
# 1: Running on resampled training sets.

#' @title Hyperparameters
#'
#' @description
#' Superclass for hyperparameters.
#'
#' @field algorithm Character: Algorithm name.
#' @field hyperparameters Named list of algorithm hyperparameter values.
#' @field tunable_hyperparameters Character: Names of tunable hyperparameters.
#' @field fixed_hyperparameters Character: Names of fixed hyperparameters.
#' @field tuned Integer: Tuning status.
#' @field resampled Integer: Outer resampling status.
#' @field n_workers Integer: Number of workers to use for tuning.
#'
#' @author EDG
#' @noRd
Hyperparameters <- new_class(
  name = "Hyperparameters",
  properties = list(
    algorithm = class_character,
    hyperparameters = class_list,
    tunable_hyperparameters = class_character,
    fixed_hyperparameters = class_character,
    tuned = class_integer,
    resampled = class_integer,
    n_workers = class_integer
  ),
  constructor = function(
    algorithm,
    hyperparameters,
    tunable_hyperparameters,
    fixed_hyperparameters,
    n_workers = 1L
  ) {
    # Test if any tunable_hyperparameters have more than one value
    if (length(tunable_hyperparameters) > 0) {
      if (any(sapply(hyperparameters[tunable_hyperparameters], length) > 1)) {
        tuned <- 0L # Search values defined for tunable hyperparameters.
      } else {
        tuned <- -1L # No search values defined for tunable hyperparameters.
      }
    } else {
      tuned <- -2L # No tunable hyperparameters
    }
    # GLMNET
    if (algorithm == "GLMNET") {
      if (is.null(hyperparameters[["lambda"]])) {
        tuned <- 0L
      }
    }
    # LightGBM
    if (algorithm == "LightGBM") {
      if (is.null(hyperparameters[["nrounds"]])) {
        tuned <- 0L
      }
    }
    # SVM
    # Check kernel-specific hyperparameters
    if (algorithm == "SVM") {
      # linear => cost
      if (hyperparameters[["kernel"]] == "linear") {
        if (length(hyperparameters[["cost"]]) > 1) {
          tuned <- 0L
        }
      } else if (hyperparameters[["kernel"]] == "polynomial") {
        if (length(hyperparameters[["degree"]]) > 1) {
          tuned <- 0L
        }
      } else if (hyperparameters[["kernel"]] == "radial") {
        if (length(hyperparameters[["sigma"]]) > 1) {
          tuned <- 0L
        }
      }
    }
    n_workers <- clean_posint(n_workers)
    new_object(
      S7_object(),
      algorithm = algorithm,
      hyperparameters = hyperparameters,
      tunable_hyperparameters = tunable_hyperparameters,
      fixed_hyperparameters = fixed_hyperparameters,
      tuned = tuned,
      resampled = 0L,
      n_workers = n_workers
    )
  }
) # /Hyperparameters


# Show Hyperparameters ----
#' Show Hyperparameters
#'
#' Show method for Hyperparameters object.
#'
#' @param x Hyperparameters object.
#' @param pad Integer: Left padding for printed output.
#' @param maxlength Integer: Maximum length of items to show using `headdot()` before truncating with ellipsis. `-1` means no limit.
#' @param limit Integer: Limit number of items to show. `-1` means no limit.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @author EDG
#' @noRd
method(repr, Hyperparameters) <- function(
  x,
  pad = 0L,
  maxlength = -1L,
  limit = -1L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  out <- repr_S7name(
    paste0(x@algorithm, "Hyperparameters"),
    pad = pad,
    output_type = output_type
  )
  out <- paste0(
    out,
    repr_ls(
      props(x)[-1],
      pad = pad,
      maxlength = maxlength,
      limit = limit,
      output_type = output_type
    )
  )
  if (x@tuned == TUNED_STATUS_TUNING) {
    out <- paste0(
      out,
      highlight2(
        "\n  Hyperparameters are being tuned.\n",
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_NOT_TUNABLE) {
    out <- paste0(
      out,
      highlight2(
        "\n  No hyperparameters are tunable.\n",
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_UNTUNED) {
    need_tuning <- names(get_hyperparams_need_tuning(x))
    out <- paste0(
      out,
      highlight2(
        paste0(
          "\n  ",
          ngettext(length(need_tuning), "Hyperparameter ", "Hyperparameters "),
          oxfordcomma(
            need_tuning
          ),
          ngettext(length(need_tuning), " needs ", " need "),
          "tuning.\n"
        ),
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_NO_SEARCH_VALUES) {
    out <- paste0(
      out,
      highlight2(
        "\n  No search values defined for tunable hyperparameters.\n",
        output_type = output_type
      )
    )
  } else if (x@tuned == TUNED_STATUS_TUNED) {
    out <- paste0(
      out,
      highlight2("\n  Hyperparameters are tuned.\n", output_type = output_type)
    )
  }
  out
} # /rtemis::show.Hyperparameters


# Print Hyperparameters ----
method(print, Hyperparameters) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # rtemis::print.Hyperparameters

# is_tuned() ----
is_tuned <- new_generic("is_tuned", "x")
method(is_tuned, Hyperparameters) <- function(x) {
  x@tuned == 1L
} # /is_tuned.Hyperparameters

# get_tuned_status() ----
get_tuned_status <- new_generic("get_tuned_status", "x")
method(get_tuned_status, Hyperparameters) <- function(x) {
  if (length(x@tunable_hyperparameters) > 0) {
    if (any(sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1)) {
      0L
    } else {
      -1L
    }
  } else {
    -2L
  }
} # /get_tuned_status.Hyperparameters

# Update Hyperparameters ----
#' Update Hyperparameters
#'
#' @param x Hyperparameters object.
#' @param hyperparameters Named list of algorithm hyperparameter values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
# update <- new_generic("update", "x")
method(update, Hyperparameters) <- function(
  object,
  hyperparameters,
  tuned = NULL,
  ...
) {
  for (hp in names(hyperparameters)) {
    object@hyperparameters[[hp]] <- hyperparameters[[hp]]
  }
  # Update tuned status
  if (is.null(tuned)) {
    object@tuned <- get_tuned_status(object)
  } else {
    object@tuned <- tuned
  }
  object
} # /update.Hyperparameters

# Freeze Hyperparameters ----
#' Freeze Hyperparameters
#'
#' @param x Hyperparameters object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
freeze <- new_generic("freeze", "x")
method(freeze, Hyperparameters) <- function(x) {
  x@tuned <- -1
}

# Lock Hyperparameters ----
#' Lock Hyperparameters
#'
#' @param x Hyperparameters object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lock <- new_generic("lock", "x")
method(lock, Hyperparameters) <- function(x) {
  x@tuned <- 1
}

# Make Hyperparameters@hyperparameters@name `$`-accessible ----
method(`$`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}

# `$`-autocomplete Hyperparameters@hyperparameters ----
method(`.DollarNames`, Hyperparameters) <- function(x, pattern = "") {
  all_names <- names(x@hyperparameters)
  grep(pattern, all_names, value = TRUE)
}

# Make Hyperparameters@hyperparameters@name `[[`-accessible ----
method(`[[`, Hyperparameters) <- function(x, name) {
  x@hyperparameters[[name]]
}

#' needs_tuning ----
#'
#' @keywords internal
#' @noRd
needs_tuning <- new_generic("needs_tuning", "x")
method(needs_tuning, Hyperparameters) <- function(x) {
  x@tuned == 0
} # /needs_tuning.Hyperparameters

# get_hyperparams_need_tuning ----
#' Get hyperparameters that need tuning in an algorithm-specific way.
#'
#' @keywords internal
#' @noRd
method(get_hyperparams_need_tuning, Hyperparameters) <- function(x) {
  # -> list
  # Get tunable hyperparameters with more than one value
  x@hyperparameters[x@tunable_hyperparameters[
    sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1
  ]]
} # /get_hyperparams_need_tuning.Hyperparameters

# get_hyperparams.(Hyperparameters, character) ----
method(get_hyperparams, list(Hyperparameters, class_character)) <- function(
  x,
  param_names
) {
  sapply(param_names, function(p) x@hyperparameters[p], USE.NAMES = FALSE)
}

# GLMHyperparameters ----
#' @author EDG
#'
#' @keywords internal
#' @noRd
GLMHyperparameters <- new_class(
  name = "GLMHyperparameters",
  parent = Hyperparameters,
  constructor = function(ifw) {
    new_object(
      Hyperparameters(
        algorithm = "GLM",
        hyperparameters = list(
          ifw = ifw
        ),
        tunable_hyperparameters = "ifw",
        fixed_hyperparameters = character()
      )
    )
  } # /constructor
) # /rtemis::GLMHyperparameters


#' Setup GLM Hyperparameters
#'
#' Setup hyperparameters for GLM training.
#'
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return GLMHyperparameters object.
#'
#' @author EDG
#' @export
setup_GLM <- function(ifw = FALSE) {
  GLMHyperparameters(ifw = ifw)
}


# GAMHyperparameters ----
GAM_tunable <- c("k", "ifw")
GAM_fixed <- character()

#' @author EDG
#' @keywords internal
#' @noRd
GAMHyperparameters <- new_class(
  name = "GAMHyperparameters",
  parent = Hyperparameters,
  constructor = function(k, ifw) {
    new_object(
      Hyperparameters(
        algorithm = "GAM",
        hyperparameters = list(
          k = k,
          ifw = ifw
        ),
        tunable_hyperparameters = GAM_tunable,
        fixed_hyperparameters = GAM_fixed
      )
    )
  } # /constructor
) # /rtemis::GAMHyperparameters


#' Setup GAM Hyperparameters
#'
#' Setup hyperparameters for GAM training.
#'
#' Get more information from [mgcv::gam].
#'
#' @param k (Tunable) Integer: Number of knots.
#' @param ifw (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return GAMHyperparameters object.
#'
#' @author EDG
#' @export
setup_GAM <- function(k = 5L, ifw = FALSE) {
  k <- clean_posint(k)
  GAMHyperparameters(k = k, ifw = ifw)
} # /rtemis::setup_GAM


# CARTHyperparameters ----
CART_tunable <- c("cp", "maxdepth", "minsplit", "minbucket", "prune_cp", "ifw")
CART_fixed <- c(
  "method",
  "model",
  "maxcompete",
  "maxsurrogate",
  "usesurrogate",
  "surrogatestyle",
  "xval",
  "cost"
)

#' @title CARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for CART.
#'
#' @author EDG
#' @keywords internal
#' @noRd
CARTHyperparameters <- new_class(
  name = "CARTHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    cp,
    maxdepth,
    minsplit,
    minbucket,
    prune_cp,
    method,
    model,
    maxcompete,
    maxsurrogate,
    usesurrogate,
    surrogatestyle,
    xval,
    cost,
    ifw
  ) {
    new_object(
      Hyperparameters(
        algorithm = "CART",
        hyperparameters = list(
          cp = cp,
          maxdepth = maxdepth,
          minsplit = minsplit,
          minbucket = minbucket,
          prune_cp = prune_cp,
          method = method,
          model = model,
          maxcompete = maxcompete,
          maxsurrogate = maxsurrogate,
          usesurrogate = usesurrogate,
          surrogatestyle = surrogatestyle,
          xval = xval,
          cost = cost,
          ifw = ifw
        ),
        tunable_hyperparameters = CART_tunable,
        fixed_hyperparameters = CART_fixed
      )
    )
  } # /constructor
) # /rtemis::CARTHyperparameters


#' Setup CART Hyperparameters
#'
#' Setup hyperparameters for CART training.
#'
#' Get more information from [rpart::rpart] and [rpart::rpart.control].
#'
#' @param cp (Tunable) Numeric: Complexity parameter.
#' @param maxdepth (Tunable) Integer: Maximum depth of tree.
#' @param minsplit (Tunable) Integer: Minimum number of observations in a node to split.
#' @param minbucket (Tunable) Integer: Minimum number of observations in a terminal node.
#' @param prune_cp (Tunable) Numeric: Complexity for cost-complexity pruning after tree is built
#' @param method String: Splitting method.
#' @param model Logical: If TRUE, return a model.
#' @param maxcompete Integer: Maximum number of competitive splits.
#' @param maxsurrogate Integer: Maximum number of surrogate splits.
#' @param usesurrogate Integer: Number of surrogate splits to use.
#' @param surrogatestyle Integer: Type of surrogate splits.
#' @param xval Integer: Number of cross-validation folds.
#' @param cost Numeric (>=0): One for each feature.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return CARTHyperparameters object.
#'
#' @author EDG
#' @export
setup_CART <- function(
  # tunable
  cp = 0.01,
  maxdepth = 20L,
  minsplit = 2L,
  minbucket = 1L, # round(minsplit / 3),
  prune_cp = NULL,
  # fixed
  method = "auto",
  model = TRUE,
  maxcompete = 4L,
  maxsurrogate = 5L,
  usesurrogate = 2L,
  surrogatestyle = 0L,
  xval = 0L,
  cost = NULL,
  ifw = FALSE
) {
  check_inherits(cp, "numeric")
  maxdepth <- clean_int(maxdepth)
  minsplit <- clean_int(minsplit)
  minbucket <- clean_int(minbucket)
  check_inherits(prune_cp, "numeric")
  check_inherits(method, "character")
  check_inherits(model, "logical")
  maxcompete <- clean_int(maxcompete)
  maxsurrogate <- clean_int(maxsurrogate)
  usesurrogate <- clean_int(usesurrogate)
  surrogatestyle <- clean_int(surrogatestyle)
  xval <- clean_int(xval)
  check_inherits(cost, "numeric")
  CARTHyperparameters(
    cp = cp,
    maxdepth = maxdepth,
    minsplit = minsplit,
    minbucket = minbucket,
    prune_cp = prune_cp,
    method = method,
    model = model,
    maxcompete = maxcompete,
    maxsurrogate = maxsurrogate,
    usesurrogate = usesurrogate,
    surrogatestyle = surrogatestyle,
    xval = xval,
    cost = cost,
    ifw = ifw
  )
} # /setup_CART

# Test that all CART hyperparameters are set by setup_CART
stopifnot(all(c(CART_tunable, CART_fixed) %in% names(formals(setup_CART))))


# GLMNETHyperparameters ----
GLMNET_tunable <- c("alpha", "ifw")
GLMNET_fixed <- c(
  "family",
  "offset",
  "which_lambda_cv",
  "nlambda",
  "penalty_factor",
  "standardize",
  "intercept"
)

#' @title GLMNETHyperparameters
#'
#' @description
#' Hyperparameters subclass for GLMNET.
#'
#' @author EDG
#' @keywords internal
#' @noRd
GLMNETHyperparameters <- new_class(
  name = "GLMNETHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    alpha,
    family,
    offset,
    which_lambda_cv,
    nlambda,
    lambda,
    penalty_factor,
    standardize,
    intercept,
    ifw
  ) {
    check_float01inc(alpha)
    check_inherits(which_lambda_cv, "character")
    nlambda <- clean_posint(nlambda)
    check_inherits(penalty_factor, "numeric")
    check_inherits(standardize, "logical")
    new_object(
      Hyperparameters(
        algorithm = "GLMNET",
        hyperparameters = list(
          alpha = alpha,
          family = family,
          offset = offset,
          which_lambda_cv = which_lambda_cv,
          nlambda = nlambda,
          lambda = lambda,
          penalty_factor = penalty_factor,
          standardize = standardize,
          intercept = intercept,
          ifw = ifw
        ),
        tunable_hyperparameters = GLMNET_tunable,
        fixed_hyperparameters = GLMNET_fixed
      )
    )
  } # /constructor
) # /rtemis::GLMNETHyperparameters

#' Setup GLMNET Hyperparameters
#'
#' Setup hyperparameters for GLMNET training.
#'
#' Get more information from [glmnet::glmnet].
#'
#' @param alpha (Tunable) Numeric: Mixing parameter.
#' @param family Character: Family for GLMNET.
#' @param offset Numeric: Offset for GLMNET.
#' @param which_lambda_cv Character: Which lambda to use for prediction:
#' "lambda.1se" or "lambda.min"
#' @param nlambda Positive integer: Number of lambda values.
#' @param lambda Numeric: Lambda values.
#' @param penalty_factor Numeric: Penalty factor for each feature.
#' @param standardize Logical: If TRUE, standardize features.
#' @param intercept Logical: If TRUE, include intercept.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return GLMNETHyperparameters object.
#'
#' @author EDG
#' @export
setup_GLMNET <- function(
  # tunable
  alpha = 1,
  # fixed
  family = NULL,
  offset = NULL,
  which_lambda_cv = "lambda.1se",
  nlambda = 100L,
  lambda = NULL,
  penalty_factor = NULL,
  standardize = TRUE,
  intercept = TRUE,
  ifw = TRUE
) {
  check_float01inc(alpha)
  check_inherits(which_lambda_cv, "character")
  nlambda <- clean_posint(nlambda)
  check_inherits(penalty_factor, "numeric")
  check_logical(standardize)
  check_logical(ifw)
  GLMNETHyperparameters(
    family = family,
    offset = offset,
    alpha = alpha,
    which_lambda_cv = which_lambda_cv,
    nlambda = nlambda,
    lambda = lambda,
    penalty_factor = penalty_factor,
    standardize = standardize,
    intercept = intercept,
    ifw = ifw
  )
} # /setup_GLMNET

# Test that all GLMNET hyperparameters are set by setup_GLMNET
stopifnot(all(
  c(GLMNET_tunable, GLMNET_fixed) %in% names(formals(setup_GLMNET))
))

method(get_hyperparams_need_tuning, GLMNETHyperparameters) <- function(x) {
  # Get tunable hyperparameters with more than one value
  out <- x@hyperparameters[x@tunable_hyperparameters[
    sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1
  ]]
  if (is.null(x[["lambda"]])) {
    out <- c(out, list(lambda = NULL))
  }
  out
} # /get_hyperparams_need_tuning.GLMNETHyperparameters

# LightCARTHyperparameters ----
LightCART_tunable <- c(
  "num_leaves",
  "max_depth",
  "lambda_l1",
  "lambda_l2",
  "min_data_in_leaf",
  "max_cat_threshold",
  "min_data_per_group",
  "linear_tree",
  "ifw"
)
LightCART_fixed <- c("objective")

#' @title LightCARTHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightCART
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightCARTHyperparameters <- new_class(
  name = "LightCARTHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    num_leaves,
    max_depth,
    lambda_l1,
    lambda_l2,
    min_data_in_leaf,
    max_cat_threshold,
    min_data_per_group,
    linear_tree,
    objective,
    ifw
  ) {
    new_object(
      Hyperparameters(
        algorithm = "LightCART",
        hyperparameters = list(
          num_leaves = num_leaves,
          max_depth = max_depth,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          min_data_in_leaf = min_data_in_leaf,
          max_cat_threshold = max_cat_threshold,
          min_data_per_group = min_data_per_group,
          linear_tree = linear_tree,
          objective = objective,
          ifw = ifw
        ),
        tunable_hyperparameters = LightCART_tunable,
        fixed_hyperparameters = LightCART_fixed
      )
    )
  } # /constructor
) # /rtemis::LightCARTHyperparameters

#' Setup LightCART Hyperparameters
#'
#' Setup hyperparameters for LightCART training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param min_data_in_leaf (Tunable) Positive integer: Minimum number of data in a leaf.
#' @param max_cat_threshold (Tunable) Positive integer: Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Positive integer: Minimum number of observations per categorical group.
#' @param linear_tree (Tunable) Logical: If TRUE, use linear trees.
#' @param objective Character: Objective function.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return LightCARTHyperparameters object.
#'
#' @author EDG
#' @export
setup_LightCART <- function(
  num_leaves = 32L,
  max_depth = -1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  min_data_in_leaf = 20L,
  max_cat_threshold = 32L,
  min_data_per_group = 100L,
  linear_tree = FALSE,
  objective = NULL,
  ifw = FALSE
) {
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  check_float0pos(lambda_l1)
  check_float0pos(lambda_l2)
  min_data_in_leaf <- clean_posint(min_data_in_leaf)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  check_logical(linear_tree)
  LightCARTHyperparameters(
    num_leaves = num_leaves,
    max_depth = max_depth,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    min_data_in_leaf = min_data_in_leaf,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    objective = objective,
    ifw = ifw
  )
} # /rtemis::setup_LightCART


# LightRFHyperparameters ----
LightRF_tunable <- c(
  "nrounds",
  "num_leaves",
  "max_depth",
  "feature_fraction",
  "subsample",
  "lambda_l1",
  "lambda_l2",
  "max_cat_threshold",
  "min_data_per_group",
  "ifw"
)
LightRF_fixed <- c(
  "objective",
  "device_type",
  "tree_learner",
  "boosting_type",
  "learning_rate",
  "subsample_freq",
  "early_stopping_rounds",
  "force_col_wise"
)

#' @title LightRFHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightRF
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightRFHyperparameters <- new_class(
  name = "LightRFHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    nrounds,
    num_leaves,
    max_depth,
    feature_fraction,
    subsample,
    lambda_l1,
    lambda_l2,
    max_cat_threshold,
    min_data_per_group,
    linear_tree,
    ifw,
    # fixed
    objective,
    device_type,
    tree_learner,
    force_col_wise
  ) {
    new_object(
      Hyperparameters(
        algorithm = "LightRF",
        hyperparameters = list(
          nrounds = nrounds,
          num_leaves = num_leaves,
          max_depth = max_depth,
          feature_fraction = feature_fraction,
          subsample = subsample,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          max_cat_threshold = max_cat_threshold,
          min_data_per_group = min_data_per_group,
          linear_tree = linear_tree,
          ifw = ifw,
          # fixed
          objective = objective,
          device_type = device_type,
          tree_learner = tree_learner,
          force_col_wise = force_col_wise,
          # unsettable: LightGBM params for RF
          boosting_type = "rf",
          learning_rate = 1, # no effect? in boosting_type 'rf', but set for clarity
          subsample_freq = 1L, # a.k.a. bagging_freq
          early_stopping_rounds = -1L
        ),
        tunable_hyperparameters = LightRF_tunable,
        fixed_hyperparameters = LightRF_fixed
      )
    )
  }
) # /rtemis::LightRFHyperparameters

#' Setup LightRF Hyperparameters
#'
#' Setup hyperparameters for LightRF training.
#'
#' Get more information from [lightgbm::lgb.train].
#' Note that hyperparameters subsample_freq and early_stopping_rounds are fixed,
#' and cannot be set because they are what makes `lightgbm` train a random forest.
#' These can all be set when training gradient boosting with LightGBM.
#'
#' @param nrounds (Tunable) Positive integer: Number of boosting rounds.
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param feature_fraction (Tunable) Numeric: Fraction of features to use.
#' @param subsample (Tunable) Numeric: Fraction of data to use.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param max_cat_threshold (Tunable) Positive integer: Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Positive integer: Minimum number of observations per categorical group.
#' @param linear_tree Logical: If TRUE, use linear trees.
#' @param objective Character: Objective function.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#' @param device_type Character: "cpu" or "gpu".
#' @param tree_learner Character: "serial", "feature", "data", or "voting".
#' @param force_col_wise Logical: Use only with CPU - If TRUE, force col-wise histogram building.
#'
#' @return LightRFHyperparameters object.
#'
#' @author EDG
#' @export
setup_LightRF <- function(
  nrounds = 500L,
  num_leaves = 4096L,
  max_depth = -1L,
  feature_fraction = 0.7,
  subsample = .623, # a.k.a. bagging_fraction
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  ifw = FALSE,
  # fixed
  objective = NULL,
  device_type = "cpu",
  tree_learner = "serial",
  force_col_wise = TRUE
) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  check_float01inc(feature_fraction)
  check_float01inc(subsample)
  check_float0pos(lambda_l1)
  check_float0pos(lambda_l2)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  check_logical(linear_tree)
  LightRFHyperparameters(
    nrounds = nrounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    feature_fraction = feature_fraction,
    subsample = subsample,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective,
    device_type = device_type,
    tree_learner = tree_learner,
    force_col_wise = force_col_wise
  )
} # /rtemis::setupLightRF

# Test that all LightRF hyperparameters are set by setup_LightRF
# LightRF fixed hyperparameters are not editable.
stopifnot(all(LightRF_tunable %in% names(formals(setup_LightRF))))


# LightGBMHyperparameters ----
LightGBM_tunable <- c(
  "num_leaves",
  "max_depth",
  "learning_rate",
  "feature_fraction",
  "subsample",
  "subsample_freq",
  "lambda_l1",
  "lambda_l2",
  "max_cat_threshold",
  "min_data_per_group",
  "linear_tree",
  "ifw"
)
LightGBM_fixed <- c(
  "max_nrounds",
  "force_nrounds",
  "early_stopping_rounds",
  "objective",
  "device_type",
  "tree_learner",
  "force_col_wise"
)

#' @title LightGBMHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightGBM
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightGBMHyperparameters <- new_class(
  name = "LightGBMHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    max_nrounds,
    force_nrounds,
    early_stopping_rounds,
    # tunable
    num_leaves,
    max_depth,
    learning_rate,
    feature_fraction,
    subsample,
    subsample_freq,
    lambda_l1,
    lambda_l2,
    max_cat_threshold,
    min_data_per_group,
    linear_tree,
    ifw,
    objective,
    device_type,
    tree_learner,
    force_col_wise
  ) {
    nrounds <- if (!is.null(force_nrounds)) {
      force_nrounds
    } else {
      NULL
    }
    new_object(
      Hyperparameters(
        algorithm = "LightGBM",
        hyperparameters = list(
          nrounds = nrounds,
          max_nrounds = max_nrounds,
          force_nrounds = force_nrounds,
          early_stopping_rounds = early_stopping_rounds,
          num_leaves = num_leaves,
          max_depth = max_depth,
          learning_rate = learning_rate,
          subsample = subsample,
          subsample_freq = subsample_freq,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          max_cat_threshold = max_cat_threshold,
          min_data_per_group = min_data_per_group,
          linear_tree = linear_tree,
          ifw = ifw,
          objective = objective,
          device_type = device_type,
          tree_learner = tree_learner,
          force_col_wise = force_col_wise
        ),
        tunable_hyperparameters = LightGBM_tunable,
        fixed_hyperparameters = LightGBM_fixed
      )
    )
  }
) # /rtemis::LightGBMHyperparameters

method(update, LightGBMHyperparameters) <- function(
  object,
  hyperparameters,
  tuned = NULL,
  ...
) {
  for (hp in names(hyperparameters)) {
    object@hyperparameters[[hp]] <- hyperparameters[[hp]]
  }
  # Update tuned status
  if (is.null(tuned)) {
    object@tuned <- get_tuned_status(object)
  } else {
    object@tuned <- tuned
  }
  # Update nrounds (e.g. in LightRuleFit)
  if (
    is.null(object@hyperparameters[["nrounds"]]) &&
      !is.null(object@hyperparameters[["force_nrounds"]])
  ) {
    object@hyperparameters[["nrounds"]] <- object@hyperparameters[[
      "force_nrounds"
    ]]
  }
  object
} # /update.Hyperparameters

# References:
# LightGBM parameters: https://lightgbm.readthedocs.io/en/latest/Parameters.html

#' Setup LightGBM Hyperparameters
#'
#' Setup hyperparameters for LightGBM training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param max_nrounds Positive integer: Maximum number of boosting rounds.
#' @param force_nrounds Positive integer: Use this many boosting rounds. Disable search for nrounds.
#' @param early_stopping_rounds Positive integer: Number of rounds without improvement to stop training.
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param learning_rate (Tunable) Numeric: Learning rate.
#' @param feature_fraction (Tunable) Numeric: Fraction of features to use.
#' @param subsample (Tunable) Numeric: Fraction of data to use.
#' @param subsample_freq (Tunable) Positive integer: Frequency of subsample.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param max_cat_threshold (Tunable) Positive integer: Maximum number of categories for categorical features.
#' @param min_data_per_group (Tunable) Positive integer: Minimum number of observations per categorical group.
#' @param linear_tree Logical: If TRUE, use linear trees.
#' @param objective Character: Objective function.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#' @param device_type Character: "cpu" or "gpu".
#' @param tree_learner Character: "serial", "feature", "data", or "voting".
#' @param force_col_wise Logical: Use only with CPU - If TRUE, force col-wise histogram building.
#'
#' @return LightGBMHyperparameters object.
#'
#' @author EDG
#' @export
setup_LightGBM <- function(
  # nrounds will be auto-tuned if force_nrounds is NULL with a value up to max_nrounds and
  # using early_stopping_rounds.
  max_nrounds = 1000L,
  force_nrounds = NULL,
  early_stopping_rounds = 10L,
  # tunable
  num_leaves = 8L,
  max_depth = -1L,
  learning_rate = 0.01,
  feature_fraction = 1.0,
  subsample = 1.0, # a.k.a. bagging_fraction {check:hyper}
  subsample_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  max_cat_threshold = 32L,
  min_data_per_group = 32L,
  linear_tree = FALSE,
  ifw = FALSE,
  objective = NULL,
  device_type = "cpu",
  tree_learner = "serial",
  force_col_wise = TRUE
) {
  max_nrounds <- clean_posint(max_nrounds)
  force_nrounds <- clean_posint(force_nrounds)
  early_stopping_rounds <- clean_posint(early_stopping_rounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  check_floatpos1(learning_rate)
  check_floatpos1(feature_fraction)
  check_floatpos1(subsample)
  subsample_freq <- clean_posint(subsample_freq)
  check_float0pos(lambda_l1)
  check_float0pos(lambda_l2)
  max_cat_threshold <- clean_posint(max_cat_threshold)
  min_data_per_group <- clean_posint(min_data_per_group)
  check_logical(linear_tree)
  LightGBMHyperparameters(
    max_nrounds = max_nrounds,
    force_nrounds = force_nrounds,
    early_stopping_rounds = early_stopping_rounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    feature_fraction = feature_fraction,
    subsample = subsample,
    subsample_freq = subsample_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    max_cat_threshold = max_cat_threshold,
    min_data_per_group = min_data_per_group,
    linear_tree = linear_tree,
    ifw = ifw,
    objective = objective,
    device_type = device_type,
    tree_learner = tree_learner,
    force_col_wise = force_col_wise
  )
} # /rtemis::setupLightGBM

# Test that all LightGBM hyperparameters are set by setup_LightGBM
stopifnot(all(
  c(LightGBM_tunable, LightGBM_fixed) %in% names(formals(setup_LightGBM))
))

method(get_hyperparams_need_tuning, LightGBMHyperparameters) <- function(x) {
  # Get tunable hyperparameters with more than one value
  out <- x@hyperparameters[x@tunable_hyperparameters[
    sapply(x@hyperparameters[x@tunable_hyperparameters], length) > 1
  ]]
  if (is.null(x[["nrounds"]])) {
    out <- c(out, list(nrounds = NULL))
  }
  out
} # /get_hyperparams_need_tuning.LightGBMHyperparameters


# LightRuleFitHyperparameters ----
LightRuleFit_tunable <- c(
  "nrounds",
  "num_leaves",
  "max_depth",
  "learning_rate",
  "subsample",
  "subsample_freq",
  "lambda_l1",
  "lambda_l2",
  "alpha",
  "ifw_lightgbm",
  "ifw_glmnet"
)
LightRuleFit_fixed <- c("lambda", "objective")
LightRuleFit_lightgbm_params <- c(
  "nrounds",
  "num_leaves",
  "max_depth",
  "learning_rate",
  "subsample",
  "subsample_freq",
  "lambda_l1",
  "lambda_l2",
  "objective"
)
LightRuleFit_glmnet_params <- c("alpha", "lambda")

#' @title LightRuleFitHyperparameters
#'
#' @description
#' Hyperparameters subclass for LightRuleFit.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LightRuleFitHyperparameters <- new_class(
  name = "LightRuleFitHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    nrounds,
    num_leaves,
    max_depth,
    learning_rate,
    subsample,
    subsample_freq,
    lambda_l1,
    lambda_l2,
    objective,
    ifw_lightgbm,
    # GLMNET
    alpha,
    lambda,
    ifw_glmnet,
    # IFW
    ifw
  ) {
    new_object(
      Hyperparameters(
        algorithm = "LightRuleFit",
        hyperparameters = list(
          nrounds = nrounds,
          num_leaves = num_leaves,
          max_depth = max_depth,
          learning_rate = learning_rate,
          subsample = subsample,
          subsample_freq = subsample_freq,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          objective = objective,
          ifw_lightgbm = ifw_lightgbm,
          # GLMNET
          alpha = alpha,
          lambda = lambda,
          ifw_glmnet = ifw_glmnet,
          # IFW
          ifw = ifw
        ),
        tunable_hyperparameters = LightRuleFit_tunable,
        fixed_hyperparameters = LightRuleFit_fixed
      )
    )
  }
) # /rtemis::LightRuleFitHyperparameters

#' Setup LightRuleFit Hyperparameters
#'
#' Setup hyperparameters for LightRuleFit training.
#'
#' Get more information from [lightgbm::lgb.train].
#'
#' @param nrounds (Tunable) Positive integer: Number of boosting rounds.
#' @param num_leaves (Tunable) Positive integer: Maximum number of leaves in one tree.
#' @param max_depth (Tunable) Integer: Maximum depth of trees.
#' @param learning_rate (Tunable) Numeric: Learning rate.
#' @param subsample (Tunable) Numeric: Fraction of data to use.
#' @param subsample_freq (Tunable) Positive integer: Frequency of subsample.
#' @param lambda_l1 (Tunable) Numeric: L1 regularization.
#' @param lambda_l2 (Tunable) Numeric: L2 regularization.
#' @param objective Character: Objective function.
#' @param ifw_lightgbm (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in the LightGBM
#' step.
#' @param objective Character: Objective function.
#' @param alpha (Tunable) Numeric: Alpha for GLMNET.
#' @param lambda Numeric: Lambda for GLMNET.
#' @param ifw_glmnet (Tunable) Logical: If TRUE, use Inverse Frequency Weighting in the GLMNET step.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification. This applies IFW
#' to both LightGBM and GLMNET.
#'
#' @return LightRuleFitHyperparameters object.
#'
#' @author EDG
#' @export
setup_LightRuleFit <- function(
  nrounds = 200L,
  num_leaves = 32L,
  max_depth = 4L,
  learning_rate = 0.1,
  subsample = 0.666,
  subsample_freq = 1L,
  lambda_l1 = 0,
  lambda_l2 = 0,
  objective = NULL,
  ifw_lightgbm = FALSE,
  alpha = 1,
  lambda = NULL,
  ifw_glmnet = FALSE,
  ifw = FALSE
) {
  nrounds <- clean_posint(nrounds)
  num_leaves <- clean_posint(num_leaves)
  max_depth <- clean_int(max_depth)
  check_floatpos1(learning_rate)
  check_floatpos1(subsample)
  subsample_freq <- clean_posint(subsample_freq)
  check_inherits(lambda_l1, "numeric")
  check_inherits(lambda_l2, "numeric")
  check_float01inc(alpha)
  check_inherits(lambda, "numeric")
  check_logical(ifw_lightgbm)
  check_logical(ifw_glmnet)
  check_logical(ifw)
  # If ifw, cannot have ifw_lightgbm or ifw_glmnet
  if (ifw) {
    if (ifw_lightgbm) {
      cli::cli_abort("Cannot set ifw and ifw_lightgbm at the same time.")
    }
    if (ifw_glmnet) {
      cli::cli_abort("Cannot set ifw and ifw_glmnet at the same time.")
    }
  }
  LightRuleFitHyperparameters(
    nrounds = nrounds,
    num_leaves = num_leaves,
    max_depth = max_depth,
    learning_rate = learning_rate,
    subsample = subsample,
    subsample_freq = subsample_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    objective = objective,
    ifw_lightgbm = ifw_lightgbm,
    alpha = alpha,
    lambda = lambda,
    ifw_glmnet = ifw_glmnet,
    ifw = ifw
  )
} # /rtemis::setup_LightRuleFit


# IsotonicHyperparameters ----
Isotonic_tunable <- character()
Isotonic_fixed <- character()

#' @title IsotonicHyperparameters
#'
#' @description
#' Hyperparameters subclass for Isotonic Regression.
#'
#' @author EDG
#' @keywords internal
#' @noRd
IsotonicHyperparameters <- new_class(
  name = "IsotonicHyperparameters",
  parent = Hyperparameters,
  constructor = function(ifw) {
    new_object(
      Hyperparameters(
        algorithm = "Isotonic",
        hyperparameters = list(
          ifw = ifw
        ),
        tunable_hyperparameters = "ifw",
        fixed_hyperparameters = Isotonic_fixed
      )
    )
  }
) # /rtemis::IsotonicHyperparameters

# setup_Isotonic ----
#' Setup Isotonic Hyperparameters
#'
#' Setup hyperparameters for Isotonic Regression.
#'
#' There are not hyperparameters for this algorithm at this moment.
#'
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return IsotonicHyperparameters object.
#'
#' @author EDG
#' @export

setup_Isotonic <- function(ifw = FALSE) {
  IsotonicHyperparameters(ifw = ifw)
} # /rtemis::setup_Isotonic


# SVMHyperparameters ----
#' @title SVMHyperparameters
#'
#' @description
#' Hyperparameters subclass for SVM.
#'
#' @author EDG
#' @keywords internal
#' @noRd
SVMHyperparameters <- new_class(
  name = "SVMHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    hyperparameters,
    tunable_hyperparameters,
    fixed_hyperparameters
  ) {
    new_object(
      Hyperparameters(
        algorithm = "SVM",
        hyperparameters = hyperparameters,
        tunable_hyperparameters = tunable_hyperparameters,
        fixed_hyperparameters = fixed_hyperparameters
      )
    )
  } # /constructor
) # /rtemis::SVMHyperparameters

# LinearSVMHyperparameters ----
LinearSVM_tunable <- c("cost", "ifw")
LinearSVM_fixed <- character()

#' @title LinearSVMHyperparameters
#'
#' @description
#' Hyperparameters subclass for SVM with linear kernel.
#'
#' @author EDG
#' @keywords internal
#' @noRd
LinearSVMHyperparameters <- new_class(
  name = "LinearSVMHyperparameters",
  parent = Hyperparameters,
  constructor = function(cost, ifw) {
    new_object(
      Hyperparameters(
        algorithm = "LinearSVM",
        hyperparameters = list(
          kernel = "linear",
          cost = cost,
          ifw = ifw
        ),
        tunable_hyperparameters = c("cost", "ifw"),
        fixed_hyperparameters = character()
      )
    )
  } # /constructor
) # /rtemis::LinearSVMHyperparameters

#' Setup LinearSVM Hyperparameters
#'
#' Setup hyperparameters for LinearSVM training.
#'
#' Get more information from [e1071::svm].
#' @param cost (Tunable) Numeric: Cost of constraints violation.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return LinearSVMHyperparameters object.
#'
#' @author EDG
#' @export
setup_LinearSVM <- function(
  cost = 1,
  ifw = FALSE
) {
  check_inherits(cost, "numeric")
  check_logical(ifw)
  LinearSVMHyperparameters(
    cost = cost,
    ifw = ifw
  )
} # /setup_LinearSVM

# Test that all SVM hyperparameters are set by setup_SVM
stopifnot(all(
  c(LinearSVM_tunable, LinearSVM_fixed) %in% names(formals(setup_LinearSVM))
))


# RadialSVMHyperparameters ----

RadialSVM_tunable <- c("cost", "gamma", "ifw")
RadialSVM_fixed <- character()

#' @title RadialSVMHyperparameters
#'
#' @description
#' Hyperparameters subclass for SVM with radial kernel.
#'
#' @author EDG
#' @keywords internal
#' @noRd
RadialSVMHyperparameters <- new_class(
  name = "RadialSVMHyperparameters",
  parent = Hyperparameters,
  constructor = function(cost, gamma, ifw) {
    new_object(
      Hyperparameters(
        algorithm = "RadialSVM",
        hyperparameters = list(
          kernel = "radial",
          cost = cost,
          gamma = gamma,
          ifw = ifw
        ),
        tunable_hyperparameters = c("cost", "gamma", "ifw"),
        fixed_hyperparameters = character()
      )
    )
  } # /constructor
) # /rtemis::RadialSVMHyperparameters

#' Setup RadialSVM Hyperparameters
#'
#' Setup hyperparameters for RadialSVM training.
#'
#' Get more information from [e1071::svm].
#'
#' @param cost (Tunable) Numeric: Cost of constraints violation.
#' @param gamma (Tunable) Numeric: Kernel coefficient.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return RadialSVMHyperparameters object.
#'
#' @author EDG
#' @export
setup_RadialSVM <- function(
  cost = 1,
  gamma = 0.01,
  ifw = FALSE
) {
  check_inherits(cost, "numeric")
  check_inherits(gamma, "numeric")
  check_logical(ifw)
  RadialSVMHyperparameters(
    cost = cost,
    gamma = gamma,
    ifw = ifw
  )
} # /setup_RadialSVM


# Test that all SVM hyperparameters are set by setup_SVM
stopifnot(all(
  c(RadialSVM_tunable, RadialSVM_fixed) %in% names(formals(setup_RadialSVM))
))

# TabNetHyperparameters ----
tabnet_tunable <- c(
  "batch_size",
  "penalty",
  "clip_value",
  "loss",
  "epochs",
  "drop_last",
  "decision_width",
  "attention_width",
  "num_steps",
  "feature_reusage",
  "mask_type",
  "virtual_batch_size",
  "valid_split",
  "learn_rate",
  "optimizer",
  "lr_scheduler",
  "lr_decay",
  "step_size",
  "checkpoint_epochs",
  "cat_emb_dim",
  "num_independent",
  "num_shared",
  "num_independent_decoder",
  "num_shared_decoder",
  "momentum",
  "pretraining_ratio",
  "importance_sample_size",
  "early_stopping_monitor",
  "early_stopping_tolerance",
  "early_stopping_patience",
  "num_workers",
  "skip_importance",
  "early_stopping_patience",
  "ifw"
)

tabnet_fixed <- c("device", "num_workers", "skip_importance")

#' @title TabNetHyperparameters
#'
#' @description
#' Hyperparameters subclass for TabNet.
#'
#' @author EDG
#' @keywords internal
#' @noRd
TabNetHyperparameters <- new_class(
  name = "TabNetHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    batch_size,
    penalty,
    clip_value,
    loss,
    epochs,
    drop_last,
    decision_width,
    attention_width,
    num_steps,
    feature_reusage,
    mask_type,
    virtual_batch_size,
    valid_split,
    learn_rate,
    optimizer,
    lr_scheduler,
    lr_decay,
    step_size,
    checkpoint_epochs,
    cat_emb_dim,
    num_independent,
    num_shared,
    num_independent_decoder,
    num_shared_decoder,
    momentum,
    pretraining_ratio,
    device,
    importance_sample_size,
    early_stopping_monitor,
    early_stopping_tolerance,
    early_stopping_patience,
    num_workers,
    skip_importance,
    ifw
  ) {
    new_object(
      Hyperparameters(
        algorithm = "TabNet",
        hyperparameters = list(
          batch_size = batch_size,
          penalty = penalty,
          clip_value = clip_value,
          loss = loss,
          epochs = epochs,
          drop_last = drop_last,
          decision_width = decision_width,
          attention_width = attention_width,
          num_steps = num_steps,
          feature_reusage = feature_reusage,
          mask_type = mask_type,
          virtual_batch_size = virtual_batch_size,
          valid_split = valid_split,
          learn_rate = learn_rate,
          optimizer = optimizer,
          lr_scheduler = lr_scheduler,
          lr_decay = lr_decay,
          step_size = step_size,
          checkpoint_epochs = checkpoint_epochs,
          cat_emb_dim = cat_emb_dim,
          num_independent = num_independent,
          num_shared = num_shared,
          num_independent_decoder = num_independent_decoder,
          num_shared_decoder = num_shared_decoder,
          momentum = momentum,
          pretraining_ratio = pretraining_ratio,
          device = device,
          importance_sample_size = importance_sample_size,
          early_stopping_monitor = early_stopping_monitor,
          early_stopping_tolerance = early_stopping_tolerance,
          early_stopping_patience = early_stopping_patience,
          num_workers = num_workers,
          skip_importance = skip_importance,
          ifw = ifw
        ),
        tunable_hyperparameters = tabnet_tunable,
        fixed_hyperparameters = tabnet_fixed
      )
    )
  } # /constructor
) # /rtemis::TabNetHyperparameters

#' Setup TabNet Hyperparameters
#'
#' Setup hyperparameters for TabNet training.
#'
# Get more information from [tabnet::tabnet_config]
#'
#' @param batch_size (Tunable) Positive integer: Batch size.
#' @param penalty (Tunable) Numeric: Regularization penalty.
#' @param clip_value Numeric: Clip value.
#' @param loss Character: Loss function.
#' @param epochs (Tunable) Positive integer: Number of epochs.
#' @param drop_last Logical: If TRUE, drop last batch.
#' @param decision_width (Tunable) Positive integer: Decision width.
#' @param attention_width (Tunable) Positive integer: Attention width.
#' @param num_steps (Tunable) Positive integer: Number of steps.
#' @param feature_reusage (Tunable) Numeric: Feature reusage.
#' @param mask_type Character: Mask type.
#' @param virtual_batch_size (Tunable) Positive integer: Virtual batch size.
#' @param valid_split Numeric: Validation split.
#' @param learn_rate (Tunable) Numeric: Learning rate.
#' @param optimizer Character or torch function: Optimizer.
#' @param lr_scheduler Character or torch function: "step", "reduce_on_plateau".
#' @param lr_decay Numeric: Learning rate decay.
#' @param step_size Positive integer: Step size.
#' @param checkpoint_epochs (Tunable) Positive integer: Checkpoint epochs.
#' @param cat_emb_dim (Tunable) Positive integer: Categorical embedding dimension.
#' @param num_independent (Tunable) Positive integer: Number of independent Gated Linear Units (GLU)
#' at each step of the encoder.
#' @param num_shared (Tunable) Positive integer: Number of shared Gated Linear Units (GLU) at each
#' step of the encoder.
#' @param num_independent_decoder (Tunable) Positive integer: Number of independent GLU layers for
#' pretraining.
#' @param num_shared_decoder (Tunable) Positive integer: Number of shared GLU layers for
#' pretraining.
#' @param momentum (Tunable) Numeric: Momentum.
#' @param pretraining_ratio (Tunable) Numeric: Pretraining ratio.
#' @param device Character: Device "cpu" or "cuda".
#' @param importance_sample_size Positive integer: Importance sample size.
#' @param early_stopping_monitor Character: Early stopping monitor. "valid_loss", "train_loss",
#' "auto".
#' @param early_stopping_tolerance Numeric: Minimum relative improvement to reset the patience
#' counter.
#' @param early_stopping_patience Positive integer: Number of epochs without improving before
#' stopping.
#' @param num_workers Positive integer: Number of subprocesses for data loacding.
#' @param skip_importance Logical: If TRUE, skip importance calculation.
#' @param ifw Logical: If TRUE, use Inverse Frequency Weighting in classification.
#'
#' @return TabNetHyperparameters object.
#'
#' @author EDG
#' @export
setup_TabNet <- function(
  batch_size = 1024^2,
  penalty = 0.001,
  clip_value = NULL,
  loss = "auto",
  epochs = 50L,
  drop_last = FALSE,
  decision_width = NULL,
  attention_width = NULL,
  num_steps = 3L,
  feature_reusage = 1.3,
  mask_type = "sparsemax",
  virtual_batch_size = 256^2,
  valid_split = 0,
  learn_rate = 0.02,
  optimizer = "adam",
  lr_scheduler = NULL,
  lr_decay = 0.1,
  step_size = 30,
  checkpoint_epochs = 10L,
  cat_emb_dim = 1L,
  num_independent = 2L,
  num_shared = 2L,
  num_independent_decoder = 1L,
  num_shared_decoder = 1L,
  momentum = 0.02,
  pretraining_ratio = 0.5,
  device = "auto",
  importance_sample_size = NULL,
  early_stopping_monitor = "auto",
  early_stopping_tolerance = 0,
  early_stopping_patience = 0,
  num_workers = 0L,
  skip_importance = FALSE,
  ifw = FALSE
) {
  TabNetHyperparameters(
    batch_size = batch_size,
    penalty = penalty,
    clip_value = clip_value,
    loss = loss,
    epochs = epochs,
    drop_last = drop_last,
    decision_width = decision_width,
    attention_width = attention_width,
    num_steps = num_steps,
    feature_reusage = feature_reusage,
    mask_type = mask_type,
    virtual_batch_size = virtual_batch_size,
    valid_split = valid_split,
    learn_rate = learn_rate,
    optimizer = optimizer,
    lr_scheduler = lr_scheduler,
    lr_decay = lr_decay,
    step_size = step_size,
    checkpoint_epochs = checkpoint_epochs,
    cat_emb_dim = cat_emb_dim,
    num_independent = num_independent,
    num_shared = num_shared,
    num_independent_decoder = num_independent_decoder,
    num_shared_decoder = num_shared_decoder,
    momentum = momentum,
    pretraining_ratio = pretraining_ratio,
    device = device,
    importance_sample_size = importance_sample_size,
    early_stopping_monitor = early_stopping_monitor,
    early_stopping_tolerance = early_stopping_tolerance,
    early_stopping_patience = early_stopping_patience,
    num_workers = num_workers,
    skip_importance = skip_importance,
    ifw = ifw
  )
} # /setup_TabNet

# Test that all TabNet hyperparameters are set by setup_TabNet
stopifnot(all(
  c(tabnet_tunable, tabnet_fixed) %in% names(formals(setup_TabNet))
))

get_tabnet_config <- function(hyperparameters) {
  check_is_S7(hyperparameters, TabNetHyperparameters)
  hpr <- hyperparameters@hyperparameters
  hpr[["ifw"]] <- NULL
  do.call(tabnet::tabnet_config, hpr)
} # /get_tabnet_config

# RangerHyperparameters ----
ranger_tunable <- c(
  "num_trees",
  "mtry",
  "min_node_size",
  "max_depth",
  "sample_fraction",
  "replace",
  "splitrule",
  "num_random_splits",
  "alpha",
  "minprop",
  "regularization_factor",
  "ifw"
)

ranger_fixed <- c(
  "importance",
  "write_forest",
  "probability",
  "min_bucket",
  "case_weights", # set by train
  "class_weights", # set by train
  "poisson_tau",
  "split_select_weights",
  "always_split_variables",
  "respect_unordered_factors",
  "scale_permutation_importance",
  "local_importance",
  "regularization_usedepth",
  "keep_inbag",
  "inbag",
  "holdout",
  "quantreg",
  "time_interest",
  "oob_error",
  "save_memory",
  "verbose",
  "node_stats",
  "seed",
  "na_action"
)

#' @title RangerHyperparameters
#'
#' @description
#' Hyperparameters subclass for Ranger Random Forest.
#'
#' @author EDG
#' @keywords internal
#' @noRd
RangerHyperparameters <- new_class(
  name = "RangerHyperparameters",
  parent = Hyperparameters,
  constructor = function(
    num_trees,
    mtry,
    importance,
    write_forest,
    probability,
    min_node_size,
    min_bucket,
    max_depth,
    replace,
    sample_fraction,
    case_weights,
    class_weights,
    splitrule,
    num_random_splits,
    alpha,
    minprop,
    poisson_tau,
    split_select_weights,
    always_split_variables,
    respect_unordered_factors,
    scale_permutation_importance,
    local_importance,
    regularization_factor,
    regularization_usedepth,
    keep_inbag,
    inbag,
    holdout,
    quantreg,
    time_interest,
    oob_error,
    save_memory,
    verbose,
    node_stats,
    seed,
    na_action,
    ifw
  ) {
    new_object(
      Hyperparameters(
        algorithm = "Ranger",
        hyperparameters = list(
          num_trees = num_trees,
          mtry = mtry,
          importance = importance,
          write_forest = write_forest,
          probability = probability,
          min_node_size = min_node_size,
          min_bucket = min_bucket,
          max_depth = max_depth,
          replace = replace,
          sample_fraction = sample_fraction,
          case_weights = case_weights,
          class_weights = class_weights,
          splitrule = splitrule,
          num_random_splits = num_random_splits,
          alpha = alpha,
          minprop = minprop,
          poisson_tau = poisson_tau,
          split_select_weights = split_select_weights,
          always_split_variables = always_split_variables,
          respect_unordered_factors = respect_unordered_factors,
          scale_permutation_importance = scale_permutation_importance,
          local_importance = local_importance,
          regularization_factor = regularization_factor,
          regularization_usedepth = regularization_usedepth,
          keep_inbag = keep_inbag,
          inbag = inbag,
          holdout = holdout,
          quantreg = quantreg,
          time_interest = time_interest,
          oob_error = oob_error,
          save_memory = save_memory,
          verbose = verbose,
          node_stats = node_stats,
          seed = seed,
          na_action = na_action,
          ifw = ifw
        ),
        tunable_hyperparameters = ranger_tunable,
        fixed_hyperparameters = ranger_fixed
      )
    )
  } # /constructor
) # /rtemis::RangerHyperparameters

#' Setup Ranger Hyperparameters
#'
#' Setup hyperparameters for Ranger Random Forest training.
#'
#' Get more information from [ranger::ranger].
#'
#' @param num_trees (Tunable) Positive integer: Number of trees.
#' @param mtry (Tunable) Positive integer: Number of features to consider at each split.
#' @param importance Character: Variable importance mode. "none", "impurity", "impurity_corrected", "permutation".
#' The "impurity" measure is the Gini index for classification, the variance of the responses for regression.
#' @param write_forest Logical: Save ranger.forest object, required for prediction. Set to FALSE to reduce memory usage if no prediction intended.
#' @param probability Logical: Grow a probability forest as in Malley et al. (2012). For classification only.
#' @param min_node_size (Tunable) Positive integer: Minimal node size. Default 1 for classification, 5 for regression, 3 for survival, and 10 for probability.
#' @param min_bucket Positive integer: Minimal number of samples in a terminal node. Only for survival. Deprecated in favor of min_node_size.
#' @param max_depth (Tunable) Positive integer: Maximal tree depth. A value of NULL or 0 (the default) corresponds to unlimited depth, 1 to tree stumps (1 split per tree).
#' @param replace Logical: Sample with replacement.
#' @param sample_fraction (Tunable) Numeric: Fraction of observations to sample. Default is 1 for sampling with replacement and 0.632 for sampling without replacement.
#' @param case_weights Numeric vector: Weights for sampling of training observations. Observations with larger weights will be selected with higher probability in the bootstrap (or subsampled) samples for the trees.
#' @param class_weights Numeric vector: Weights for the outcome classes for classification. Vector of the same length as the number of classes, with names corresponding to the class labels.
#' @param splitrule (Tunable) Character: Splitting rule. For classification: "gini", "extratrees", "hellinger". For regression: "variance", "extratrees", "maxstat", "beta". For survival: "logrank", "extratrees", "C", "maxstat".
#' @param num_random_splits (Tunable) Positive integer: For "extratrees" splitrule: Number of random splits to consider for each candidate splitting variable.
#' @param alpha (Tunable) Numeric: For "maxstat" splitrule: significance threshold to allow splitting.
#' @param minprop (Tunable) Numeric: For "maxstat" splitrule: lower quantile of covariate distribution to be considered for splitting.
#' @param poisson_tau Numeric: For "poisson" regression splitrule: tau parameter for Poisson regression.
#' @param split_select_weights Numeric vector: Numeric vector with weights between 0 and 1, representing the probability to select variables for splitting. Alternatively, a list of size num_trees, with one weight vector per tree.
#' @param always_split_variables Character vector: Character vector with variable names to be always selected in addition to the mtry variables tried for splitting.
#' @param respect_unordered_factors Character or logical: Handling of unordered factor covariates. For "partition" all 2^(k-1)-1 possible partitions are considered for splitting, where k is the number of factor levels. For "ignore", all factor levels are ordered by their first occurrence in the data. For "order", all factor levels are ordered by their average response. TRUE corresponds to "partition" for the randomForest package compatibility.
#' @param scale_permutation_importance Logical: Scale permutation importance by standard error as in (Breiman 2001). Only applicable if permutation variable importance mode selected.
#' @param local_importance Logical: For permutation variable importance, use local importance as in Breiman (2001) and Liaw & Wiener (2002).
#' @param regularization_factor (Tunable) Numeric: Regularization factor. Penalize variables with many split points. Requires splitrule = "variance".
#' @param regularization_usedepth Logical: Use regularization factor with node depth. Requires regularization_factor.
#' @param keep_inbag Logical: Save how often observations are in-bag in each tree. These will be used for (local) variable importance if inbag.counts in predict() is NULL.
#' @param inbag List: Manually set observations per tree. List of size num_trees, containing inbag counts for each observation. Can be used for stratified sampling.
#' @param holdout Logical: Hold-out mode. Hold-out all samples with case weight 0 and use these for variable importance and prediction error.
#' @param quantreg Logical: Prepare quantile prediction as in quantile regression forests (Meinshausen 2006). For regression only. Set keep_inbag = TRUE to prepare out-of-bag quantile prediction.
#' @param time_interest Numeric: For GWAS data: SNP with this number will be used as time variable. Only for survival. Deprecated, use time.var in formula instead.
#' @param oob_error Logical: Compute OOB prediction error. Set to FALSE to save computation time if only the forest is needed.
#' @param save_memory Logical: Use memory saving (but slower) splitting mode. No effect for survival and GWAS data. Warning: This option slows down the tree growing, use only if you encounter memory problems.
#' @param verbose Logical: Show computation status and estimated runtime.
#' @param node_stats Logical: Save additional node statistics. Only terminal nodes for now.
#' @param seed Positive integer: Random seed. Default is NULL, which generates the seed from R. Set to 0 to ignore the R seed.
#' @param na_action Character: Action to take if the data contains missing values. "na.learn" uses observations with missing values in splitting, treating missing values as a separate category.
#' @param ifw Logical: Inverse Frequency Weighting for classification. If TRUE, class weights are set inversely proportional to the class frequencies.
#' @return RangerHyperparameters object.
#' @author EDG
#' @export
setup_Ranger <- function(
  num_trees = 500,
  mtry = NULL,
  importance = "impurity",
  write_forest = TRUE,
  probability = FALSE,
  min_node_size = NULL,
  min_bucket = NULL,
  max_depth = NULL,
  replace = TRUE,
  sample_fraction = ifelse(replace, 1, 0.632),
  case_weights = NULL,
  class_weights = NULL,
  splitrule = NULL,
  num_random_splits = 1,
  alpha = 0.5,
  minprop = 0.1,
  poisson_tau = 1,
  split_select_weights = NULL,
  always_split_variables = NULL,
  respect_unordered_factors = NULL,
  scale_permutation_importance = FALSE,
  local_importance = FALSE,
  regularization_factor = 1,
  regularization_usedepth = FALSE,
  keep_inbag = FALSE,
  inbag = NULL,
  holdout = FALSE,
  quantreg = FALSE,
  time_interest = NULL,
  oob_error = TRUE,
  save_memory = FALSE,
  verbose = TRUE,
  node_stats = FALSE,
  seed = NULL,
  na_action = "na.learn",
  ifw = FALSE
) {
  num_trees <- clean_posint(num_trees)
  mtry <- clean_posint(mtry)
  check_inherits(importance, "character")
  check_inherits(write_forest, "logical")
  check_inherits(probability, "logical")
  min_node_size <- clean_posint(min_node_size)
  min_bucket <- clean_posint(min_bucket)
  max_depth <- clean_posint(max_depth)
  check_inherits(replace, "logical")
  check_float01inc(sample_fraction)
  check_inherits(case_weights, "numeric")
  check_inherits(class_weights, "numeric")
  check_inherits(splitrule, "character")
  num_random_splits <- clean_posint(num_random_splits)
  check_float01inc(alpha)
  check_float01inc(minprop)
  check_inherits(poisson_tau, "numeric")
  check_inherits(split_select_weights, "numeric")
  check_inherits(always_split_variables, "character")
  check_inherits(respect_unordered_factors, "logical")
  check_inherits(scale_permutation_importance, "logical")
  check_inherits(local_importance, "logical")
  check_inherits(regularization_factor, "numeric")
  check_inherits(regularization_usedepth, "logical")
  check_inherits(keep_inbag, "logical")
  check_inherits(inbag, "list")
  check_inherits(holdout, "logical")
  check_inherits(quantreg, "logical")
  check_inherits(time_interest, "numeric")
  check_inherits(oob_error, "logical")
  check_inherits(save_memory, "logical")
  check_inherits(verbose, "logical")
  check_inherits(node_stats, "logical")
  check_inherits(seed, "numeric")
  check_inherits(na_action, "character")
  check_logical(ifw)
  RangerHyperparameters(
    num_trees = num_trees,
    mtry = mtry,
    importance = importance,
    write_forest = write_forest,
    probability = probability,
    min_node_size = min_node_size,
    min_bucket = min_bucket,
    max_depth = max_depth,
    replace = replace,
    sample_fraction = sample_fraction,
    case_weights = case_weights,
    class_weights = class_weights,
    splitrule = splitrule,
    num_random_splits = num_random_splits,
    alpha = alpha,
    minprop = minprop,
    poisson_tau = poisson_tau,
    split_select_weights = split_select_weights,
    always_split_variables = always_split_variables,
    respect_unordered_factors = respect_unordered_factors,
    scale_permutation_importance = scale_permutation_importance,
    local_importance = local_importance,
    regularization_factor = regularization_factor,
    regularization_usedepth = regularization_usedepth,
    keep_inbag = keep_inbag,
    inbag = inbag,
    holdout = holdout,
    quantreg = quantreg,
    time_interest = time_interest,
    oob_error = oob_error,
    save_memory = save_memory,
    verbose = verbose,
    node_stats = node_stats,
    seed = seed,
    na_action = na_action,
    ifw = ifw
  )
} # /setup_Ranger

# Test that all Ranger hyperparameters are set by setup_Ranger
stopifnot(all(
  c(ranger_tunable, ranger_fixed) %in% names(formals(setup_Ranger))
))

# get_ranger_config ----
#' Get Ranger Configuration
#'
#' Get Ranger configuration from RangerHyperparameters object.
#'
#' @param hyperparameters RangerHyperparameters object.
#'
#' @return List with Ranger configuration.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
get_ranger_config <- function(hyperparameters) {
  check_is_S7(hyperparameters, RangerHyperparameters)
  hpr <- hyperparameters@hyperparameters
  hpr[["ifw"]] <- NULL
  hpr
} # /get_ranger_config
