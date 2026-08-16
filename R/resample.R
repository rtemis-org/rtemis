# resample.R
# ::rtemis::
# 2015- EDG rtemis.org

#' Resample data
#'
#' Create resamples of your data, e.g. for model building or validation.
#' "KFold" creates stratified folds, , "StratSub" creates stratified subsamples,
#' "Bootstrap" gives the standard bootstrap, i.e. random sampling with replacement,
#' while "StratBoot" uses StratSub and then randomly duplicates some of the training cases to
#' reach the original length of the input, or the length defined by `target_length`.
#'
#' Note that option 'KFold' may result in resamples of slightly different length. Avoid all
#' operations which rely on equal-length vectors. For example, you can't place resamples in a
#' data.frame, but must use a list instead.
#'
#' @param x Vector or data.frame: Usually the outcome; `NROW(x)` defines the sample size. A
#' config naming a column -- `stratify_var` or `id_strat` -- needs the data frame that column
#' lives in, since a name cannot be looked up in a bare vector.
#' @param config Resampler object created by [setup_Resampler].
#' @param verbosity Integer: Verbosity level.
#'
#' @return `Resampler` object.
#'
#' @author EDG
#' @export
#' @examples
#' y <- rnorm(200)
#' # 10-fold (stratified)
#' y_10fold <- resample(y, setup_Resampler(10L, "kfold"))
#' y_10fold
#' # 25 stratified subsamples
#' y_25strat <- resample(y, setup_Resampler(25L, "stratsub"))
#' y_25strat
#' # 100 stratified bootstraps
#' y_100strat <- resample(y, setup_Resampler(100L, "stratboot"))
#' y_100strat
#' # LOOCV
#' y_loocv <- resample(y, setup_Resampler(type = "LOOCV"))
#' y_loocv
#' # User-supplied resamples
#' y_custom <- resample(
#'   y,
#'   setup_Resampler(type = "Custom", resamples = list(1:150, 51:200))
#' )
#' y_custom
resample <- function(
  x,
  config = setup_Resampler(),
  #  index = NULL,
  #  group = NULL,
  verbosity = 1L
) {
  check_is_S7(config, ResamplerConfig)
  check_data_bounds(config, x)
  # Input ----
  type <- config@type
  # `stratify_var` and `id_strat` name columns, so they are resolved here, while
  # `x` is still the frame they live in: everything below works on the outcome
  # vector alone and could not look a name up.
  strat_values <- resolve_resampler_column(config, x, "stratify_var")
  id_strat <- if (type %in% c("LOOCV", "Custom")) {
    # LOOCV puts one case in each resample and Custom takes the resamples as
    # given, so neither has anything to group.
    NULL
  } else {
    resolve_resampler_column(config, x, "id_strat")
  }
  # Narrowed whenever `x` is tabular, a single column included: a 1-column
  # frame is still a frame, and left as one it reaches the resamplers as a list
  # and fails there on coercion rather than on anything a caller could act on.
  if (!is.null(dim(x))) {
    # `inherits()` rather than `survival::is.Surv()`, whose body this is: survival is a
    # Suggests, and reaching into it here both hard-fails when it is absent and loads its
    # namespace (~450 ms) on the first resample() of every session.
    if (inherits(x, "Surv")) {
      msg("Survival object will be stratified on time.", verbosity = verbosity)
      x <- x[, 1]
    } else {
      if (NCOL(x) > 1L) {
        # Stratifying on last column, i.e. outcome, is almost universal;
        # no need to print every single time
        msg(
          "Input contains more than one column; stratifying on last.",
          verbosity = verbosity - 1L
        )
      }
      # `[[` indexes a matrix in column-major order rather than by column.
      x <- if (is.data.frame(x)) x[[NCOL(x)]] else x[, NCOL(x)]
    }
  }

  # Stratify on case IDs ----
  if (!is.null(id_strat)) {
    # Only keep unique IDs
    idl <- !duplicated(id_strat)
    x <- x[idl]
    # The stratification values describe the same cases, so they are narrowed
    # with them or the two no longer line up.
    if (!is.null(strat_values)) {
      strat_values <- strat_values[idl]
    }
  }

  if (type == "StratBoot") {
    target_length <- if (is.null(config@target_length)) {
      NROW(x)
    } else {
      config@target_length
    }
  }

  # resample ----
  if (!type %in% c("Bootstrap", "LOOCV", "Custom")) {
    # Unnamed, so stratify on the outcome itself -- almost always what is
    # wanted, and the only column `resample()` is guaranteed to have.
    .stratify_var <- if (is.null(strat_values)) x else strat_values
  }

  n_resamples <- if (type == "LOOCV") length(x) else config@n_resamples

  # Print config ----
  if (verbosity > 1L) {
    print(config)
  }

  # Make resamples ----
  if (type == "StratSub") {
    ## StratSub ----
    res_part <- strat_sub(
      x = x,
      n_resamples = n_resamples,
      train_p = config@train_p,
      stratify_var = .stratify_var,
      strat_n_bins = config@strat_n_bins,
      seed = config@seed,
      verbosity = verbosity
    )
  } else if (type == "Bootstrap") {
    ## Bootstrap ----
    res_part <- bootstrap(
      x = x,
      n_resamples = n_resamples,
      seed = config@seed
    )
  } else if (type == "KFold") {
    ## KFold ----
    res_part <- kfold(
      x = x,
      k = n_resamples,
      stratify_var = .stratify_var,
      strat_n_bins = config@strat_n_bins,
      seed = config@seed,
      verbosity = verbosity
    )
  } else if (type == "LOOCV") {
    ## LOOCV ----
    res_part <- loocv(x = x)
    # Get number of resamples
    config@n_resamples <- length(res_part)
  } else if (type == "StratBoot") {
    ## StratBoot ----
    res_part <- strat_boot(
      x = x,
      n_resamples = n_resamples,
      train_p = config@train_p,
      stratify_var = .stratify_var,
      strat_n_bins = config@strat_n_bins,
      target_length = target_length,
      seed = config@seed,
      verbosity = verbosity
    )
  } else if (type == "Custom") {
    ## Custom ----
    # Supplied rather than drawn, so the only thing left to establish is that
    # the indices address these cases. Checked here because only `resample()`
    # sees the data; the class can check their shape but never their range.
    res_part <- custom_resamples(config@resamples, NROW(x))
    # Get number of resamples
    config@n_resamples <- length(res_part)
  }

  # Update strat_n_bins ----
  if (type == "StratSub" || type == "StratBoot") {
    actual_n_bins <- attr(res_part, "strat_n_bins")
    if (actual_n_bins != config@strat_n_bins) {
      if (verbosity > 0L) {
        msg0(
          "Updated strat_n_bins from ",
          config@strat_n_bins,
          " to ",
          actual_n_bins,
          " in ResamplerConfig object."
        )
      }
      config@strat_n_bins <- actual_n_bins
    }
  }

  if (!is.null(id_strat)) {
    ### Get ID by resample ----
    id_by_res <- lapply(res_part, \(x) id_strat[idl][x])
    ### Get resamples on original data with replicates ----
    res_part <- lapply(id_by_res, \(x) which(id_strat %in% x))
  }

  # Output ----
  Resampler(type, res_part, config)
} # /rtemis::resample


# %% resolve_resampler_column ----
#' Read the column a resampler config names
#'
#' `stratify_var` and `id_strat` hold the *name* of a column rather than its
#' values: a name is one string that means the same thing to any implementation
#' reading the config, while a per-case vector is only true of one dataset in
#' one row order. Resolving a name needs the frame, so this runs before
#' `resample()` narrows `x` to the outcome and loses it.
#'
#' @param config ResamplerConfig: Config to read the column name from.
#' @param x Vector or data.frame: What `resample()` was given.
#' @param name Character \{"stratify_var", "id_strat"\}: Property holding the
#' column name.
#'
#' @return Vector holding the column's values, or NULL if the property is unset
#' or this resampler type does not declare it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_resampler_column <- function(config, x, name) {
  if (!name %in% names(props(config))) {
    return(NULL)
  }
  column <- prop(config, name)
  if (is.null(column)) {
    return(NULL)
  }
  # Tabular or not, rather than how many columns: a 1-column frame carries a
  # name to look up just as a wider one does.
  if (is.null(dim(x))) {
    rtemis.core::abort(
      "@",
      name,
      " names the column '",
      column,
      "', but resample() was given a vector. Pass the data frame the ",
      "column lives in.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (!column %in% colnames(x)) {
    rtemis.core::abort(
      "@",
      name,
      " names the column '",
      column,
      "', which is not present. Available columns: ",
      paste(colnames(x), collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  if (is.data.frame(x)) x[[column]] else x[, column]
} # /rtemis::resolve_resampler_column


# %% drop_id_strat_column ----
#' Remove the identifier column a resampler config names
#'
#' The column `id_strat` names says which cases belong together; it identifies a
#' case rather than describing it, so it is not a feature. `resample()` needs the
#' frame that carries it, so a caller resamples on the original and models on
#' what this returns.
#'
#' A column that is named but absent is left to `resample()`, which reports it
#' against the full frame rather than one already narrowed here.
#'
#' @param x data.frame, data.table, or tibble: Training data.
#' @param config Optional ResamplerConfig: Config that may name the column.
#'
#' @return `x` without the identifier column, unchanged if none is named.
#'
#' @author EDG
#' @keywords internal
#' @noRd
drop_id_strat_column <- function(x, config) {
  if (is.null(config) || !"id_strat" %in% names(props(config))) {
    return(x)
  }
  column <- config@id_strat
  if (is.null(column) || !column %in% colnames(x)) {
    return(x)
  }
  exc(x, column)
} # /rtemis::drop_id_strat_column


# %% custom_resamples ----
#' User-supplied resampling
#'
#' The counterpart of `kfold()` and `bootstrap()` for resamples that are given
#' rather than drawn. The property spec has already checked their shape -- a
#' non-empty list of non-empty integer vectors, each index at least 1 -- so all
#' that is left is the upper bound, which only `resample()` can know: `n_cases`
#' is a fact about the data, not about the config, and none of `data_bound`'s
#' three meanings (scalar `<=` dim, vector length `==` dim, names within
#' columns) states "every index within the case count".
#'
#' @param resamples List of integer vectors: Training-case indices per resample.
#' @param n_cases Integer [1, Inf): Number of cases the indices address.
#'
#' @return List of integer vectors, named per resample.
#'
#' @author EDG
#' @keywords internal
#' @noRd
custom_resamples <- function(resamples, n_cases) {
  # Shape is the property spec's job and has already run: a non-empty list of
  # non-empty integer vectors, each at least 1, none missing. What is left is
  # the upper bound, which is the only part that depends on the data.
  for (i in seq_along(resamples)) {
    if (any(resamples[[i]] > n_cases)) {
      rtemis.core::abort(
        "@resamples[[",
        i,
        "]] indexes outside the data: indices must lie in [1, ",
        n_cases,
        "].",
        class = c("rtemis_range_error", "rtemis_input_error")
      )
    }
  }
  if (is.null(names(resamples))) {
    names(resamples) <- paste0("Custom_", seq_along(resamples))
  }
  resamples
} # /rtemis::custom_resamples


#' Bootstrap Resampling
#'
#' @param x Input vector.
#' @param n_resamples Integer: Number of resamples to make.
#' @param seed Integer: If provided, set seed for reproducibility.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
bootstrap <- function(x, n_resamples = 10, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ids <- seq_along(x)
  .length <- length(x)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  res <- lapply(
    seq(n_resamples),
    function(i) sort(sample(ids, .length, replace = TRUE))
  )
  names(res) <- paste0("Bootstrap_", seq(n_resamples))
  res
} # /rtemis::bootstrap


#' K-fold Resampling
#'
#' @inheritParams resample
#' @param x Input Vector.
#' @param k Integer: Number of folds.
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
kfold <- function(
  x,
  k = 10,
  stratify_var = NULL,
  strat_n_bins = 4,
  seed = NULL,
  verbosity = TRUE
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(stratify_var)) {
    stratify_var <- x
  }
  stratify_var <- as.numeric(stratify_var)
  # ->> update
  max.bins <- length(unique(stratify_var))
  if (max.bins < strat_n_bins) {
    if (max.bins == 1) {
      rtemis.core::abort(
        "Only one unique value present in stratify_var.",
        class = c("rtemis_value_error", "rtemis_input_error")
      )
    }
    if (verbosity > 0L) {
      msg0("Using max n bins possible = ", max.bins, ".")
    }
    strat_n_bins <- max.bins
  }

  ids <- seq_along(x)
  # cuts
  cuts <- cut(stratify_var, breaks = strat_n_bins, labels = FALSE)
  cut.bins <- sort(unique(cuts))

  # ids by cut
  idl <- lapply(seq_along(cut.bins), function(i) ids[cuts == cut.bins[i]])
  # length of each cut
  # idl.length <- sapply(idl, length)
  idl.length <- as.numeric(table(cuts))

  # split each idl into k folds after randomizing them
  idl.k <- vector("list", length(cut.bins))
  for (i in seq_along(cut.bins)) {
    cut1 <- cut(sample(idl.length[i]), breaks = k, labels = FALSE)
    idl.k[[i]] <- lapply(seq(k), function(j) idl[[i]][cut1 == j])
  }

  res <- lapply(
    seq(k),
    \(i) {
      seq(ids)[-sort(unlist(lapply(seq_along(cut.bins), \(j) idl.k[[j]][[i]])))]
    }
  )

  names(res) <- paste0("Fold_", seq(k))
  attr(res, "strat_n_bins") <- strat_n_bins
  res
} # /rtemis::kfold


#' Resample using Stratified Subsamples
#'
#' @inheritParams resample
#' @param x Input vector
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
strat_sub <- function(
  x,
  n_resamples = 10,
  train_p = .75,
  stratify_var = NULL,
  strat_n_bins = 4,
  seed = NULL,
  verbosity = TRUE
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(stratify_var)) {
    stratify_var <- x
  }
  stratify_var <- as.numeric(stratify_var)
  max.bins <- length(unique(stratify_var))
  if (max.bins < strat_n_bins) {
    if (verbosity > 0L) {
      msg("Using max n bins possible =", max.bins)
    }
    strat_n_bins <- max.bins
  }
  ids <- seq_along(x)
  cuts <- cut(stratify_var, breaks = strat_n_bins, labels = FALSE)
  cut.bins <- sort(unique(cuts))
  idl <- lapply(seq_along(cut.bins), function(i) ids[cuts == cut.bins[i]])
  idl.length <- as.numeric(table(cuts))
  res <- lapply(seq(n_resamples), function(i) {
    sort(unlist(sapply(seq_along(cut.bins), function(j) {
      sample(idl[[j]], train_p * idl.length[j])
    })))
  })
  names(res) <- paste0("Subsample_", seq(n_resamples))
  attr(res, "strat_n_bins") <- strat_n_bins
  res
} # /rtemis::strat_sub


#' Stratified Bootstrap Resampling
#'
#' @inheritParams resample
#' @param x Input vector
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
strat_boot <- function(
  x,
  n_resamples = 10,
  train_p = .75,
  stratify_var = NULL,
  strat_n_bins = 4,
  target_length = NULL,
  seed = NULL,
  verbosity = TRUE
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  res_part1 <- strat_sub(
    x = x,
    n_resamples = n_resamples,
    train_p = train_p,
    stratify_var = stratify_var,
    strat_n_bins = strat_n_bins,
    verbosity = verbosity
  )

  # Make sure target_length was not too short by accident
  res.length <- length(res_part1[[1]])
  if (is.null(target_length)) {
    target_length <- length(x)
  }
  if (target_length < res.length) {
    target_length <- length(x)
  }

  # Add back this many cases
  add.length <- target_length - res.length
  doreplace <- ifelse(add.length > res.length, 1, 0)
  res_part2 <- lapply(
    res_part1,
    function(i) sample(i, add.length, replace = doreplace)
  )
  res <- mapply(c, res_part1, res_part2, SIMPLIFY = FALSE)
  res <- lapply(res, sort)
  names(res) <- paste0("StratBoot_", seq(n_resamples))
  attr(res, "strat_n_bins") <- strat_n_bins
  res
} # /rtemis::strat_boot


#' Leave-one-out Resampling
#'
#' @param x Input vector
#'
#' @author EDG
#'
#' @keywords internal
#' @noRd
loocv <- function(x) {
  res <- lapply(seq(x), function(i) (seq(x))[-i])
  names(res) <- paste0("Fold_", seq(res))
  res
} # /rtemis::loocv
