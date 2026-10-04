# train_LightRF.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# LightGBM parameters: https://lightgbm.readthedocs.io/en/latest/Parameters.html

# %% train_.LightRFHyperparameters ----
#' Random Forest using LightGBM
#'
#' @param hyperparameters `LightRFHyperparameters` object: make using [setup_LightRF].
#' @param x tabular data: Training set.
#' @param weights Numeric vector: Case weights.
#' @param dat_validation Optional tabular data: Validation set for early stopping.
#' @param verbosity Integer: If > 0, print messages.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(train_, LightRFHyperparameters) <- function(
  hyperparameters,
  x,
  weights = NULL,
  dat_validation = NULL,
  execution_config = setup_FutureExecution(),
  verbosity = 1L
) {
  # Dependencies ----
  check_dependencies("lightgbm")

  # Hyperparameters ----
  # Hyperparameters must be either untunable or frozen by `train`.
  if (needs_tuning(hyperparameters)) {
    rtemis.core::abort(
      "Hyperparameters must be fixed - use train() instead.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Data ----
  check_supervised(
    x = x,
    dat_validation = dat_validation,
    allow_missing = TRUE,
    verbosity = verbosity
  )
  type <- supervised_type(x)
  if (type == "Classification") {
    nclasses <- nlevels(outcome(x))
  } else {
    nclasses <- 1L
  }
  if (is.null(hyperparameters[["objective"]])) {
    hyperparameters@objective <- if (type == "Regression") {
      "regression"
    } else {
      if (nclasses == 2L) {
        "binary"
      } else {
        "multiclass"
      }
    }
  }
  # Resolved here, not at setup time, because it depends on the data: the
  # random-forest convention is sqrt(p) features per split for classification.
  if (is.null(hyperparameters[["feature_fraction"]])) {
    n_features <- NCOL(features(x))
    hyperparameters@feature_fraction <- if (type == "Classification") {
      sqrt(n_features) / n_features
    } else {
      0.33
    }
  }

  ## Preprocess & create lgb.Datasets ----
  lgb_data <- prepare_lgb_data(
    x = x,
    dat_validation = dat_validation,
    type = type,
    weights = weights,
    verbosity = verbosity
  )
  x <- lgb_data[["train_data"]]
  dat_validation <- lgb_data[["valid_data"]]
  prp <- lgb_data[["preprocessor"]]

  # Train ----
  params <- hyperparameters@hyperparameters
  # Remove params that are not used by LightGBM
  params[["ifw"]] <- NULL
  params[["nrounds"]] <- params[["early_stopping_rounds"]] <- NULL
  # num_class is required for multiclass classification only, must be 1 or unset for regression & binary classification
  if (nclasses > 2L) {
    params[["num_class"]] <- nclasses
  }
  # Set n threads
  params[["num_threads"]] <- prop(hyperparameters, "n_workers")

  # An unset hyperparameter is NULL, and LightGBM does not treat a NULL as
  # absent: it parses the empty value as 0 and fails its own range check
  # (`alpha = NULL` aborts with "Check failed: (alpha) > (0.0)"). So NULL means
  # "leave it to the backend", which is expressed by not sending it at all.
  params <- c(
    params,
    lightgbm_device_params(hyperparameters, execution_config@device)
  )
  params <- Filter(Negate(is.null), params)

  model <- lightgbm::lgb.train(
    params = params,
    data = x,
    nrounds = hyperparameters[["nrounds"]],
    valids = if (!is.null(dat_validation)) {
      list(training = x, validation = dat_validation)
    } else {
      list(training = x)
    },
    early_stopping_rounds = hyperparameters[["early_stopping_rounds"]],
    verbose = verbosity - 2L
  )
  check_inherits(model, "lgb.Booster")
  model <- score_unsplit_lightrf_trees(model, x, params, verbosity)
  # `hyperparameters` is returned because this method resolved values into
  # it (R copied the caller's object, so the caller cannot see them).
  # `train()` adopts them, and the fitted model reports what it used.
  hyperparameters <- record_backend_values(
    hyperparameters,
    lightgbm_backend_values(hyperparameters, model)
  )
  list(model = model, preprocessor = prp, hyperparameters = hyperparameters)
} # /rtemis::train_.LightRFHyperparameters


# %% score_unsplit_lightrf_trees ----
#' Add the initial score to LightRF trees that could not split
#'
#' LightGBM's random-forest mode leaves the initial score out of a tree that
#' cannot split -- the training mean for regression, the log-odds
#' of the base rate for binary classification, the log class prior for
#' multiclass. Only a tree with more than one leaf receives the initial score
#' as its bias (`src/boosting/rf.hpp`, `RF::TrainOneIter`, LightGBM 4.7.0). The
#' forest averages its trees, so every prediction is pulled toward 0 by the
#' share of single-leaf trees: with the defaults, a regression on 22 cases
#' predicted -0.01 for an outcome averaging 2.59. A tree cannot split when its
#' bagged sample cannot fill two leaves of `min_data_in_leaf` cases, or when its
#' sampled features offer no split, so it happens wholesale on small samples
#' and partly just above them.
#'
#' Each such tree's leaf value -- its bagged sample's mean residual from the
#' initial score, or 0 when the sample was too small to compute one -- gets the
#' initial score added, as a tree that split does, so it predicts its bagged
#' sample's center. The initial score is read from LightGBM itself: a one-round gradient-boosting model on a constant
#' feature cannot split either, and its constant tree carries exactly the
#' initial score for the same objective, labels and weights. The patched model
#' is reloaded from its text form; `tree_sizes` is dropped because the edited
#' trees change size, and LightGBM then parses the trees sequentially.
#'
#' @param model `lgb.Booster`: Trained random forest.
#' @param data `lgb.Dataset`: The training dataset the forest was trained on.
#' @param params List: The parameters the forest was trained with.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `lgb.Booster`, unchanged when every tree split.
#'
#' @author EDG
#' @keywords internal
#' @noRd
score_unsplit_lightrf_trees <- function(model, data, params, verbosity = 1L) {
  txt <- model$save_model_to_string()
  parts <- strsplit(txt, "\n(?=Tree=)", perl = TRUE)[[1L]]
  is_tree <- startsWith(parts, "Tree=")
  single <- is_tree & grepl("\nnum_leaves=1\n", parts, perl = TRUE)
  n_single <- sum(single)
  if (n_single == 0L) {
    return(model)
  }
  if (any(grepl("\nis_linear=1\n", parts[single], perl = TRUE))) {
    rtemis.core::abort(
      "LightRF: ",
      n_single,
      " trees could not split, and single-leaf linear trees cannot be ",
      "rescored. Lower min_data_in_leaf, or set linear_tree = FALSE.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }

  # Initial score per class, from a stump that cannot split ----
  label <- lightgbm::get_field(data, "label")
  weight <- lightgbm::get_field(data, "weight")
  stump_params <- params
  stump_params[c(
    "boosting",
    "bagging_fraction",
    "bagging_freq",
    "feature_fraction",
    "linear_tree",
    "gpu_device_id"
  )] <- NULL
  # One constant tree: the CPU, on one thread.
  stump_params[["device_type"]] <- "cpu"
  stump_params[["num_threads"]] <- 1L
  stump_params[["verbose"]] <- -1L
  stump <- lightgbm::lgb.train(
    params = stump_params,
    data = lightgbm::lgb.Dataset(
      data = matrix(0, nrow = length(label), ncol = 1L),
      label = label,
      weight = weight
    ),
    nrounds = 1L,
    verbose = -1L
  )
  stump_txt <- stump$save_model_to_string()
  initial <- regmatches(
    stump_txt,
    gregexpr("(?<=\\nleaf_value=)[^\\n]+", stump_txt, perl = TRUE)
  )[[1L]]

  # Rescore ----
  # Trees are stored iteration by iteration, one per class within each.
  tree_class <- (cumsum(is_tree) - 1L) %% length(initial) + 1L
  idx <- which(single)
  # A leaf value is the tree's bagged-sample mean residual from the initial
  # score; adding the initial score is what `AddBias()` does for a tree that
  # split, so the rescored tree predicts its bagged sample's center.
  initial <- as.numeric(initial)
  parts[idx] <- vapply(
    idx,
    function(i) {
      value <- as.numeric(regmatches(
        parts[[i]],
        regexpr("(?<=\nleaf_value=)[^\n]+", parts[[i]], perl = TRUE)
      ))
      sub(
        "\nleaf_value=[^\n]*\n",
        paste0(
          "\nleaf_value=",
          format(value + initial[[tree_class[[i]]]], digits = 17L),
          "\n"
        ),
        parts[[i]],
        perl = TRUE
      )
    },
    character(1L)
  )
  parts[[1L]] <- sub("\ntree_sizes=[^\n]*", "", parts[[1L]], perl = TRUE)
  if (verbosity > 0L) {
    n_bagged <- floor(length(label) * params[["bagging_fraction"]])
    msg0(
      n_single,
      " of ",
      sum(is_tree),
      " trees could not split; the initial score LightGBM leaves out of",
      " them was added back.",
      if (n_bagged < 2L * params[["min_data_in_leaf"]]) {
        paste0(
          " Each tree is grown on ",
          n_bagged,
          " cases, and a split needs 2 x min_data_in_leaf = ",
          2L * params[["min_data_in_leaf"]],
          "; set min_data_in_leaf to at most ",
          max(1L, n_bagged %/% 2L),
          " to let them split."
        )
      } else {
        " Lower min_data_in_leaf, or raise bagging_fraction or feature_fraction, to let them split."
      }
    )
  }
  # `lgb.train()` makes its booster serializable; a booster from `lgb.load()`
  # is not until asked, and would fail to predict after `readRDS()`.
  patched <- lightgbm::lgb.load(model_str = paste(parts, collapse = "\n"))
  lightgbm::lgb.make_serializable(patched)
  patched
} # /rtemis::score_unsplit_lightrf_trees
