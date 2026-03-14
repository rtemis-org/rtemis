# utils_lightgbm.R
# ::rtemis::
# 2023- EDG rtemis.org


# %% prepare_lgb_data ----
#' Prepare data for LightGBM-based learners
#'
#' Shared data preparation for LightGBM, LightRF, and LightCART.
#' Converts factors to 0-based integers, removes the outcome from
#' `categorical_feature`, and creates `lgb.Dataset` objects.
#'
#' @param x tabular data: Training set (features + outcome in last column).
#' @param dat_validation Optional tabular data: Validation set.
#' @param type Character: "Classification" or "Regression".
#' @param weights Optional numeric vector: Case weights for training data.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Named list with elements:
#' - `train_data`: `lgb.Dataset` for training.
#' - `valid_data`: `lgb.Dataset` for validation, or NULL.
#' - `preprocessor`: `Preprocessor` object if factors were converted, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prepare_lgb_data <- function(
  x,
  dat_validation = NULL,
  type,
  weights = NULL,
  verbosity = 1L
) {
  # Factor-to-integer preprocessing ----
  factor_index <- names(x)[which(sapply(x, is.factor))]
  if (length(factor_index) > 0L) {
    prp <- preprocess(
      x,
      config = setup_Preprocessor(
        factor2integer = TRUE,
        factor2integer_startat0 = TRUE
      ),
      dat_validation = dat_validation,
      verbosity = verbosity
    )
    if (is.null(dat_validation)) {
      x <- prp@preprocessed
    } else {
      x <- prp@preprocessed[["training"]]
      dat_validation <- prp@preprocessed[["validation"]]
    }
  } else {
    prp <- NULL
  }

  # Remove outcome from factor_index (outcome is last column).
  # For Classification, the outcome is a factor that was also converted;
  # it must not be listed as a categorical feature.
  if (type == "Classification" && length(factor_index) > 0L) {
    factor_index <- factor_index[seq_len(length(factor_index) - 1L)]
  }

  # Create lgb.Datasets ----
  train_data <- lightgbm::lgb.Dataset(
    data = as.matrix(features(x)),
    categorical_feature = factor_index,
    label = outcome(x),
    weight = weights
  )

  valid_data <- if (!is.null(dat_validation)) {
    lightgbm::lgb.Dataset(
      data = as.matrix(features(dat_validation)),
      categorical_feature = factor_index,
      label = outcome(dat_validation)
    )
  }

  list(
    train_data = train_data,
    valid_data = valid_data,
    preprocessor = prp
  )
} # /rtemis::prepare_lgb_data

#' Get LightGBM Booster Trees
#'
#' @return A list of trees
#'
#' @author EDG
#' @keywords internal
#' @noRd
get_lgb_tree <- function(x, n_iter = -1) {
  out <- lapply(
    jsonlite::fromJSON(
      lightgbm::lgb.dump(
        booster = x,
        num_iteration = n_iter
      ),
      simplifyVector = FALSE
    )[["tree_info"]],
    \(y) y[["tree_structure"]]
  )
  names(out) <- paste0("Tree_", seq_along(out))
  out
} # /rtemis::get_lgb_tree


# preorderlgb ----

#' Preorder Traversal of LightGBM Tree
#'
#' Called by `lgbtree2rules` and operates on `tree` environment in place.
#'
#' @param tree Environment that will hold the extracted rules.
#' @param node LightGBM tree.
#' @param rule Character: current rule.
#' @param left Character: left child label.
#' @param right Character: right child label.
#' @param split_feature Character: split feature label.
#' @param threshold Character: threshold label.
#' @param right_cat_type Character: "in" or "notin": operator for right categorical.
#' @param xnames Character vector: feature names.
#' @param factor_levels Named list of factor levels.
#' @param verbosity Integer: Verbosity level.
#'
#' @return Character vector of rules.
#'
#' @author EDG
#' @keywords internal
#' @noRd
preorderlgb <- function(
  tree,
  node,
  rule = "TRUE",
  left = "left_child",
  right = "right_child",
  split_feature = "split_feature",
  threshold = "threshold",
  right_cat_type = "in",
  xnames,
  factor_levels,
  verbosity = 0L
) {
  if (is.null(node[[split_feature]])) {
    names(rule) <- "leaf"
    if (verbosity > 0L) {
      message("Reached a leaf; rule is ", rule, ".")
    }
    tree[["leafs"]] <- c(tree[["leafs"]], rule)
    return(rule)
  }
  rule_left <- paste0(
    rule,
    " & ",
    xnames[node[[split_feature]] + 1],
    decision_left(node[["decision_type"]]),
    fmt_thresh(
      catsplit = node[["decision_type"]] == "==",
      feature = xnames[node[[split_feature]] + 1],
      threshold = node[["threshold"]],
      factor_levels = factor_levels
    )
  )
  rule_right <- paste0(
    rule,
    " & ",
    xnames[node[[split_feature]] + 1],
    decision_right(node[["decision_type"]], right_cat_type),
    fmt_thresh_right(
      catsplit = node[["decision_type"]] == "==",
      feature = xnames[node[[split_feature]] + 1],
      threshold = node[["threshold"]],
      factor_levels = factor_levels,
      cat_type = right_cat_type
    )
  )
  rule_left <- preorderlgb(
    tree,
    node[[left]],
    rule_left,
    left,
    right,
    split_feature,
    threshold,
    right_cat_type = right_cat_type,
    xnames = xnames,
    factor_levels = factor_levels,
    verbosity = verbosity
  )
  rule <- c(rule, rule_left)
  rule_right <- preorderlgb(
    tree,
    node[[right]],
    rule_right,
    left,
    right,
    split_feature,
    threshold,
    right_cat_type = right_cat_type,
    xnames = xnames,
    factor_levels = factor_levels,
    verbosity = verbosity
  )
  rule <- c(rule, rule_right)
} # /rtemis::preorderlgb


# lgbtree2rules ----
lgbtree2rules <- function(x, xnames, factor_levels, right_cat_type = "in") {
  tree <- new.env()
  tree[["leafs"]] <- character()
  preorderlgb(
    tree,
    x,
    xnames = xnames,
    right_cat_type = right_cat_type,
    factor_levels = factor_levels
  )
  # remove root node "TRUE & "
  substr(tree[["leafs"]], 8, 99999)
} # /rtemis::lgbtree2rules


# lgb2rules ----
#' Convert LightGBM Booster to set of rules
#'
#' @param x LightGBM Booster object
#' @param n_iter Integer: Number of trees to convert to rules
#' @param xnames Character vector: Names of features
#'
#' @return Character vector of rules
#'
#' @author EDG
#' @keywords internal
#' @noRd
lgb2rules <- function(
  Booster,
  n_iter = NULL,
  xnames,
  factor_levels,
  right_cat_type = "in",
  return_unique = TRUE,
  verbosity = 1L
) {
  if (verbosity > 0L) {
    msgstart("Extracting LightGBM rules...")
  }
  if (is.null(n_iter)) {
    n_iter <- length(Booster)
  }
  trees <- get_lgb_tree(Booster, n_iter)
  rules <- lapply(trees, function(x) {
    lgbtree2rules(
      x,
      xnames,
      factor_levels = factor_levels,
      right_cat_type = right_cat_type
    )
  }) |>
    unlist()
  if (verbosity > 0L) {
    msgdone()
  }
  if (return_unique) unique(rules) else rules
} # /rtemis::lgb2rules


# extract_rules.lgb.Booster ----
#' author EDG
#' @keywords internal
#' @noRd
method(extract_rules, class_lgb.Booster) <- function(
  x,
  n_iter = NULL,
  xnames,
  factor_levels,
  right_cat_type = "in",
  return_unique = TRUE,
  verbosity = 1L
) {
  if (verbosity > 0L) {
    msgstart("Extracting LightGBM rules...")
  }
  if (is.null(n_iter)) {
    n_iter <- length(x)
  }
  trees <- get_lgb_tree(x, n_iter)
  rules <- lapply(trees, function(x) {
    lgbtree2rules(
      x,
      xnames,
      factor_levels = factor_levels,
      right_cat_type = right_cat_type
    )
  }) |>
    unlist()

  rules <- if (return_unique) {
    unique(rules)
  } else {
    rules
  }
  if (verbosity > 0L) {
    msgdone()
    msg0(
      "Extracted ",
      highlight(length(rules)),
      ifelse(return_unique, " unique", ""),
      " rules."
    )
  }
  rules
} # /rtemis::extract_rules.lgb.Booster


# decision_left ----
#' @author EDG
#' @keywords internal
#' @noRd
decision_left <- function(decision_type) {
  switch(decision_type, "<=" = " <= ", "==" = " %in% ")
} # /rtemis::decision_left


#' @author EDG
#' @keywords internal
#' @noRd
decision_right <- function(decision_type, cat_type) {
  switch(
    decision_type,
    "<=" = " > ",
    "==" = if (cat_type == "in") " %in% " else " %notin% "
  )
} # /rtemis::decision_right


#' Format rule thresholds
#'
#' @param catsplit Logical: If TRUE, feature is categorical
#' @param feature Character: feature name
#' @param threshold Character: threshold as reported by lightgbm
#' @param factor_levels Named list of factor levels. Names should correspond to training
#' set column names.
#'
#' @author EDG
#' @keywords internal
#' @noRd
fmt_thresh <- function(catsplit, feature, threshold, factor_levels) {
  if (catsplit) {
    flevels <- as.integer(strsplit(threshold, "\\|\\|")[[1]]) + 1 # 0- to 1-based factor level index
    flevels <- factor_levels[[feature]][flevels]
    paste0(
      "c(",
      paste0("'", flevels, "'", collapse = ","),
      ")"
    )
  } else {
    threshold
  }
} # /rtemis::fmt_thresh


#' @author EDG
#' @keywords internal
#' @noRd
fmt_thresh_right <- function(
  catsplit,
  feature,
  threshold,
  factor_levels,
  cat_type
) {
  if (catsplit) {
    flevels <- as.integer(strsplit(threshold, "\\|\\|")[[1]]) + 1 # 0- to 1-based factor level index
    flevels <- factor_levels[[feature]][flevels]
    if (cat_type == "in") {
      flevels <- setdiff(factor_levels[[feature]], flevels)
    }
    paste0(
      "c(",
      paste0("'", flevels, "'", collapse = ","),
      ")"
    )
  } else {
    threshold
  }
} # /rtemis::fmt_thresh_right
