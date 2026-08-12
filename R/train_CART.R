# train_CART.R
# ::rtemis::
# 2025- EDG rtemis.org

# %% train_.CARTHyperparameters ----
#' Train a CART decision tree
#'
#' Train a CART decision tree using `rpart`.
#'
#' CART does not need any special preprocessing.
#' It works with numeric and factor variables and handles missing values.
#' The "train_*" functions train a single model.
#' Use [train] for tuning and test using nested cross-validation.
#'
#' @param hyperparameters `CARTHyperparameters` object: make using [setup_CART].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Not used for CART.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, CARTHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_ExecutionConfig(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("rpart")

  # Arguments ----
  # Hyperparameters must be either untunable or frozen by `train`
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(
    x = x,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  if (is.null(weights)) {
    weights <- rep(1, NROW(x))
  }

  # Train ----
  # weights can't be NULL.
  # !If formula is character, the input to weights must be the unquoted column name in the data.frame
  # that contains weights, e.g. by doing cbind(x, weights = weights)
  model <- rpart::rpart(
    as.formula(make_formula(x)),
    data = x,
    weights = weights,
    control = rpart::rpart.control(
      minsplit = hyperparameters[["minsplit"]],
      minbucket = hyperparameters[["minbucket"]],
      cp = hyperparameters[["cp"]],
      maxcompete = hyperparameters[["maxcompete"]],
      maxsurrogate = hyperparameters[["maxsurrogate"]],
      usesurrogate = hyperparameters[["usesurrogate"]],
      surrogatestyle = hyperparameters[["surrogatestyle"]],
      maxdepth = hyperparameters[["maxdepth"]],
      xval = hyperparameters[["xval"]]
    )
  )

  # Cost-Complexity Pruning ----
  if (!is.null(hyperparameters[["prune_cp"]])) {
    model <- rpart::prune(model, cp = hyperparameters[["prune_cp"]])
  }
  check_inherits(model, "rpart")
  list(model = model, preprocessor = NULL)
} # /rtemis::train_.CARTHyperparameters


# %% predict_super.class_rpart ----
#' Predict from rpart model
#'
#' @param model rpart model.
#' @param newdata tabular data: Data to predict on.
#' @param type Character: Type of supervised learning ("Classification" or "Regression").
#'
#' @keywords internal
#' @noRd
method(predict_super, class_rpart) <- function(
  model,
  newdata,
  type = NULL,
  verbosity = 0L
) {
  if (type == "Classification") {
    # Classification
    # predict.rpart returns a matrix n_cases x n_classes,
    # with classes are ordered the same as factor levels
    predicted_prob <- predict(model, newdata = newdata, type = "prob") # binclasspos = 2L
    if (NCOL(predicted_prob) == 2L) {
      # In binary classification, rpart returns matrix with 2 columns
      predicted_prob <- predicted_prob[, 2L]
    }
    predicted_prob
  } else {
    predict(model, newdata = newdata, type = "vector")
  }
} # /rtemis::predict_super.rpart


# %% varimp_super.class_rpart ----
#' Get variable importance from rpart model
#'
#' @param model rpart model.
#'
#' @keywords internal
#' @noRd
method(varimp_super, class_rpart) <- function(model) {
  vi <- model[["variable.importance"]]
  VariableImportance(
    data.table(
      variable = names(vi),
      importance = unname(vi)
    )
  )
} # /rtemis::varimp_super.rpart


# %% cart_tree ----
#' Read an `rpart` tree into a routing-ready structure
#'
#' `model$splits` holds one row per *candidate* split -- the primary, then this
#' node's competitors, then its surrogates -- so a node's own split is found by
#' accumulating `1 + ncompete + nsurrogate` over the frame in order. Getting
#' that offset wrong routes cases to the wrong leaves and produces attributions
#' that look entirely reasonable, which is why `explain_super()` checks the
#' routing against the model's own predictions before returning anything.
#'
#' A categorical split stores a row index into `model$csplit`, whose codes are
#' 1 (left), 3 (right) and 2 (level not present).
#'
#' @param model `rpart` object.
#'
#' @return List describing the tree: leaf flags, split feature and rule per
#' node, child row indices, training weights, and per-leaf values.
#'
#' @keywords internal
#' @noRd
cart_tree <- function(model) {
  frame <- model[["frame"]]
  splits <- model[["splits"]]
  is_leaf <- frame[["var"]] == "<leaf>"
  nodes <- as.integer(rownames(frame))
  row_of <- integer(max(nodes))
  row_of[nodes] <- seq_along(nodes)

  primary <- integer(nrow(frame))
  cursor <- 1L
  for (i in seq_len(nrow(frame))) {
    if (is_leaf[[i]]) {
      next
    }
    primary[[i]] <- cursor
    cursor <- cursor +
      1L +
      frame[["ncompete"]][[i]] +
      frame[["nsurrogate"]][[i]]
  }

  # Leaf values, one column per output: the fitted value for a regression, and
  # the class probabilities for a classification, which `yval2` stores after the
  # class index and the counts.
  values <- if (is.null(frame[["yval2"]]) || is.null(dim(frame[["yval2"]]))) {
    matrix(frame[["yval"]], ncol = 1L)
  } else {
    n_classes <- (ncol(frame[["yval2"]]) - 2L) / 2L
    probabilities <- frame[["yval2"]][,
      (n_classes + 2L):(2L * n_classes + 1L),
      drop = FALSE
    ]
    # Binary reduces to the positive class, which is the second level and what
    # `predict_super()` returns. The negative class's contributions are its
    # exact negation.
    if (n_classes == 2L) {
      probabilities[, 2L, drop = FALSE]
    } else {
      probabilities
    }
  }

  # Filled at the internal rows only. Indexing `splits` with the whole `primary`
  # vector would not do: it holds 0 at every leaf, and R *drops* a zero index
  # rather than returning NA, so the result is shorter than the frame and every
  # split rule lands against the wrong node.
  internal <- which(!is_leaf)
  ncat <- rep(NA_integer_, nrow(frame))
  index <- rep(NA_real_, nrow(frame))
  left <- rep(NA_integer_, nrow(frame))
  right <- rep(NA_integer_, nrow(frame))
  ncat[internal] <- splits[primary[internal], "ncat"]
  index[internal] <- splits[primary[internal], "index"]
  left[internal] <- row_of[2L * nodes[internal]]
  right[internal] <- row_of[2L * nodes[internal] + 1L]

  list(
    is_leaf = is_leaf,
    feature = as.character(frame[["var"]]),
    ncat = ncat,
    index = index,
    csplit = model[["csplit"]],
    left = left,
    right = right,
    weight = frame[["wt"]],
    value = values
  )
} # /rtemis::cart_tree


# %% cart_goes_left ----
#' Which cases take the left child at one internal node
#'
#' @param tree List: `cart_tree()` output.
#' @param i Integer: Node row.
#' @param x data.frame: Cases.
#'
#' @return Logical vector, one per case.
#'
#' @keywords internal
#' @noRd
cart_goes_left <- function(tree, i, x) {
  value <- x[[tree[["feature"]][[i]]]]
  ncat <- tree[["ncat"]][[i]]
  if (ncat > 1L) {
    tree[["csplit"]][tree[["index"]][[i]], as.integer(value)] == 1L
  } else if (ncat == -1L) {
    value < tree[["index"]][[i]]
  } else {
    value >= tree[["index"]][[i]]
  }
} # /rtemis::cart_goes_left


# %% cart_coalition_value ----
#' Expected prediction with a subset of features known
#'
#' The path-dependent value function TreeSHAP is defined against: at a node
#' whose feature is known, follow the case; at one whose feature is not, take
#' the average of both children weighted by the training coverage that reached
#' them.
#'
#' Every node is visited once per coalition and returns a value for every case,
#' rather than recursing per case.
#'
#' @param tree List: `cart_tree()` output.
#' @param i Integer: Node row.
#' @param x data.frame: Cases.
#' @param known Named logical: Features in the coalition.
#'
#' @return Numeric matrix, one row per case and one column per output.
#'
#' @keywords internal
#' @noRd
cart_coalition_value <- function(tree, i, x, known) {
  if (tree[["is_leaf"]][[i]]) {
    return(matrix(
      tree[["value"]][i, ],
      nrow = NROW(x),
      ncol = ncol(tree[["value"]]),
      byrow = TRUE
    ))
  }
  left <- cart_coalition_value(tree, tree[["left"]][[i]], x, known)
  right <- cart_coalition_value(tree, tree[["right"]][[i]], x, known)
  if (isTRUE(known[[tree[["feature"]][[i]]]])) {
    takes_left <- cart_goes_left(tree, i, x)
    left[!takes_left, ] <- right[!takes_left, , drop = FALSE]
    return(left)
  }
  weight_left <- tree[["weight"]][[tree[["left"]][[i]]]]
  weight_right <- tree[["weight"]][[tree[["right"]][[i]]]]
  (weight_left * left + weight_right * right) / (weight_left + weight_right)
} # /rtemis::cart_coalition_value


# %% explain_super.class_rpart ----
#' Exact TreeSHAP contributions from a CART tree
#'
#' Shapley values by exact enumeration over the features the tree actually
#' splits on, against the path-dependent value function. A feature the tree
#' never split on receives exactly zero, so the enumeration is over that set
#' rather than over every column -- which is what keeps `2^p` small for a tree
#' of realistic size.
#'
#' Exact, not an estimate: every coalition is evaluated, so there is no sampling
#' and no convergence to check. The cost is `2^p` traversals, so a tree splitting
#' on more than `CART_SHAP_MAX_FEATURES` features is refused rather than left to
#' run.
#'
#' A tree has no link function, so for a classification the contributions
#' decompose the predicted **probability** directly -- as they do for NNLS, and
#' unlike every model with a margin.
#'
#' @param model `rpart` object.
#' @param newdata tabular data: Cases to explain.
#' @param background Optional tabular data: Unused. The value function's
#' expectations come from the training coverage stored in the tree.
#' @param estimator Character: Resolved estimator.
#' @param perturbation Character: Resolved value function.
#' @param scale Character: Scale the contributions are additive on.
#' @param type Character: "Regression" or "Classification".
#' @param verbosity Integer: Verbosity level.
#'
#' @return List with `phi`, `baseline`, `predicted` and `exact`.
#'
#' @keywords internal
#' @noRd
method(explain_super, class_rpart) <- function(
  model,
  newdata,
  background,
  estimator,
  perturbation,
  scale,
  type,
  verbosity = 0L
) {
  if (!identical(estimator, "TreeSHAP")) {
    rtemis.core::abort(
      "CART's explain_super() computes TreeSHAP, not ",
      estimator,
      ".",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  if (!identical(perturbation, "conditional")) {
    rtemis.core::abort(
      "Interventional TreeSHAP is not implemented for CART: the value function ",
      "marginalizes with the training coverage stored in the tree, which is a ",
      "conditional one.\n",
      "Use `setup_SHAP(perturbation = \"conditional\")`, or ",
      "`setup_SHAP(estimator = \"kernel\", perturbation = \"interventional\")`.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }
  newdata <- as.data.frame(newdata)
  tree <- cart_tree(model)
  split_features <- unique(tree[["feature"]][!tree[["is_leaf"]]])
  if (length(split_features) > CART_SHAP_MAX_FEATURES) {
    rtemis.core::abort(
      "This tree splits on ",
      length(split_features),
      " features, and exact TreeSHAP evaluates 2^",
      length(split_features),
      " coalitions.\n",
      "Use `setup_SHAP(estimator = \"kernel\")`, which samples instead.",
      class = c("rtemis_unsupported_error", "rtemis_input_error")
    )
  }

  n_features <- length(split_features)
  n_outputs <- ncol(tree[["value"]])
  n_cases <- nrow(newdata)
  # Every coalition's value, indexed by bitmask over `split_features`.
  coalitions <- vector("list", bitwShiftL(1L, n_features))
  for (mask in seq_along(coalitions) - 1L) {
    known <- stats::setNames(
      bitwAnd(mask, bitwShiftL(1L, seq_len(n_features) - 1L)) > 0L,
      split_features
    )
    coalitions[[mask + 1L]] <- cart_coalition_value(tree, 1L, newdata, known)
  }

  phi <- rep(
    list(matrix(
      0,
      nrow = n_cases,
      ncol = ncol(newdata),
      dimnames = list(NULL, names(newdata))
    )),
    n_outputs
  )
  # Shapley: a feature's average marginal contribution over every coalition it
  # could join, weighted by how many orderings put it there.
  factorials <- factorial(0:n_features)
  for (j in seq_len(n_features)) {
    bit <- bitwShiftL(1L, j - 1L)
    for (mask in seq_along(coalitions) - 1L) {
      if (bitwAnd(mask, bit) > 0L) {
        next
      }
      size <- sum(bitwAnd(mask, bitwShiftL(1L, seq_len(n_features) - 1L)) > 0L)
      weight <- factorials[[size + 1L]] *
        factorials[[n_features - size]] /
        factorials[[n_features + 1L]]
      difference <- coalitions[[mask + bit + 1L]] - coalitions[[mask + 1L]]
      for (k in seq_len(n_outputs)) {
        phi[[k]][, split_features[[j]]] <- phi[[k]][, split_features[[j]]] +
          weight * difference[, k]
      }
    }
  }

  # The empty coalition is the tree's own expected prediction, and the full one
  # must be what it predicts -- which is the check that the split parsing routed
  # every case correctly.
  baseline <- coalitions[[1L]][1L, ]
  predicted <- coalitions[[length(coalitions)]]
  reference <- as.matrix(predict_super(
    model = model,
    newdata = newdata,
    type = type
  ))
  deviation <- max(abs(predicted - reference))
  if (!is.finite(deviation) || deviation > 1e-8) {
    rtemis.core::abort(
      "TreeSHAP: traversing the tree gives predictions differing from the ",
      "model's own by ",
      format(deviation, digits = 3L),
      ". The tree was not read correctly.",
      class = c("rtemis_value_error", "rtemis_data_error")
    )
  }
  list(
    phi = phi,
    baseline = baseline,
    predicted = predicted,
    exact = TRUE
  )
} # /rtemis::explain_super.rpart
