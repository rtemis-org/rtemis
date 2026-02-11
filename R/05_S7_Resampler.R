# S7_Resampler.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7

# Description
# `ResamplerConfig` class and subclasses create objects that store resampling configuration.
# They are set by `setup_Resampler()` and perform type checking and validation.
# They are used by `resample()`.
# `Resampler` class stores resamples and their configuration.
# `Resampler` objects are created by `resample()`.

# Note: `id_strat` is used by `resample()`, not individual resamplers

# ResamplerConfig ----
#' @title ResamplerConfig
#'
#' @description
#' Superclass for resampler configuration.
#'
#' @field type Character: Type of resampler.
#' @field n Integer: Number of resamples.
#'
#' @author EDG
#' @noRd
ResamplerConfig <- new_class(
  name = "ResamplerConfig",
  properties = list(
    type = class_character,
    n = class_integer # scalar_int_pos
  ),
  constructor = function(type, n) {
    # LOOCV does not have a defined number of resamples, so n can be NA_integer_
    n <- clean_posint(n, allow_na = TRUE)
    new_object(
      S7_object(),
      type = type,
      n = n
    )
  }
) # /rtemis::ResamplerConfig

# Make S7 properties `$`-accessible
method(`$`, ResamplerConfig) <- function(x, name) {
  prop(x, name)
}

# Make S7 properties `[[`-accessible
method(`[[`, ResamplerConfig) <- function(x, name) {
  prop(x, name)
}


# repr ResamplerConfig ----
#' repr ResamplerConfig
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, ResamplerConfig) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(x, pad = pad, output_type = output_type),
    repr_ls(
      props(x)[-1],
      pad = pad + 2L,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.ResamplerConfig

#' Print ResamplerConfig
#'
#' @description
#' print ResamplerConfig object
#'
#' @param x ResamplerConfig object
#'
#' @author EDG
#' @noRd
method(print, ResamplerConfig) <- function(
  x,
  pad = 0L,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.ResamplerConfig

# desc ResamplerConfig ----
method(desc, ResamplerConfig) <- function(x) {
  switch(
    x@type,
    KFold = paste0(x@n, " independent folds"),
    StratSub = paste0(x@n, " stratified subsamples"),
    StratBoot = paste0(x@n, " stratified bootstraps"),
    Bootstrap = paste0(x@n, " bootstrap resamples"),
    Custom = paste0(x@n, " custom resamples"),
    LOOCV = paste0(x@n, " leave-one-out folds"),
    paste0(x@n, " resamples")
  )
} # /rtemis::desc.ResamplerConfig

# KFoldConfig ----
#' @title KFoldConfig
#'
#' @description
#' ResamplerConfig subclass for k-fold resampling.
#'
#' @author EDG
#' @noRd
KFoldConfig <- new_class(
  name = "KFoldConfig",
  parent = ResamplerConfig,
  properties = list(
    stratify_var = class_character | NULL,
    strat_n_bins = scalar_int_pos,
    id_strat = class_vector | NULL,
    seed = scalar_int_pos
  ),
  constructor = function(n, stratify_var, strat_n_bins, id_strat, seed) {
    new_object(
      ResamplerConfig(
        type = "KFold",
        n = n
      ),
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  }
) # /rtemis::KFoldConfig

# StratSubConfig ----
#' @title StratSubConfig
#'
#' @description
#' ResamplerConfig subclass for stratified subsampling.
#'
#' @author EDG
#' @noRd
StratSubConfig <- new_class(
  name = "StratSubConfig",
  parent = ResamplerConfig,
  properties = list(
    n = scalar_int_pos,
    train_p = scalar_dbl_01excl,
    stratify_var = class_character | NULL,
    strat_n_bins = scalar_int_pos,
    id_strat = class_vector | NULL,
    seed = scalar_int_pos
  ),
  constructor = function(
    n,
    train_p,
    stratify_var,
    strat_n_bins,
    id_strat,
    seed
  ) {
    new_object(
      ResamplerConfig(
        type = "StratSub",
        n = n
      ),
      train_p = train_p,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  }
) # /rtemis::StratSubConfig

# StratBootConfig ----
#' @title StratBootConfig
#'
#' @description
#' ResamplerConfig subclass for stratified bootstrapping.
#'
#' @author EDG
#' @noRd
StratBootConfig <- new_class(
  name = "StratBootConfig",
  parent = ResamplerConfig,
  properties = list(
    stratify_var = class_character | NULL,
    train_p = scalar_dbl_01excl,
    strat_n_bins = scalar_int_pos,
    target_length = scalar_int_pos,
    id_strat = class_vector | NULL,
    seed = scalar_int_pos
  ),
  constructor = function(
    n,
    stratify_var,
    train_p,
    strat_n_bins,
    target_length,
    id_strat,
    seed
  ) {
    new_object(
      ResamplerConfig(
        type = "StratBoot",
        n = n
      ),
      stratify_var = stratify_var,
      train_p = train_p,
      strat_n_bins = strat_n_bins,
      target_length = target_length,
      id_strat = id_strat,
      seed = seed
    )
  }
) # /rtemis::StratBootConfig

# BootstrapConfig ----
#' @title BootstrapConfig
#'
#' @description
#' ResamplerConfig subclass for bootstrap resampling.
#'
#' @author EDG
#' @noRd
BootstrapConfig <- new_class(
  name = "BootstrapConfig",
  parent = ResamplerConfig,
  properties = list(
    id_strat = class_vector | NULL,
    seed = scalar_int_pos
  ),
  constructor = function(n, id_strat, seed) {
    new_object(
      ResamplerConfig(
        type = "Bootstrap",
        n = n
      ),
      id_strat = id_strat,
      seed = seed
    )
  }
) # /rtemis::BootstrapConfig

# LOOCVConfig ----
#' @title LOOCVConfig
#'
#' @description
#' ResamplerConfig subclass for leave-one-out cross-validation.
#'
#' @author EDG
#' @noRd
LOOCVConfig <- new_class(
  name = "LOOCVConfig",
  parent = ResamplerConfig,
  constructor = function(n) {
    new_object(
      ResamplerConfig(
        type = "LOOCV",
        n = n
      )
    )
  }
) # /rtemis::LOOCVConfig

# CustomConfig ----
#' @title CustomConfig
#'
#' @description
#' ResamplerConfig subclass for custom resampling.
#'
#' @author EDG
#' @noRd
CustomConfig <- new_class(
  name = "CustomConfig",
  parent = ResamplerConfig,
  constructor = function(n) {
    new_object(
      ResamplerConfig(
        type = "Custom",
        n = n
      )
    )
  }
) # /rtemis::CustomConfig

# setup_Resampler() ----
#' Setup Resampler
#'
#' @param n_resamples Integer: Number of resamples to make.
#' @param type Character: Type of resampler: "KFold", "StratSub", "StratBoot", "Bootstrap", "LOOCV"
#' @param stratify_var Character: Variable to stratify by.
#' @param train_p Float: Training set percentage.
#' @param strat_n_bins Integer: Number of bins to stratify by.
#' @param target_length Integer: Target length for stratified bootstraps.
#' @param id_strat Integer: Vector of indices to stratify by. These may be, for example, case IDs
#' if your dataset contains repeated measurements. By specifying this vector, you can ensure that
#' each case can only be present in the training or test set, but not both.
#' @param seed Integer: Random seed.
#' @param verbosity Integer: Verbosity level.
#'
#' @return ResamplerConfig object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' tenfold_resampler <- setup_Resampler(n_resamples = 10L, type = "KFold", seed = 2026L)
#' tenfold_resampler
setup_Resampler <- function(
  n_resamples = 10L,
  type = c("KFold", "StratSub", "StratBoot", "Bootstrap", "LOOCV"),
  # index = NULL,
  # group = NULL,
  stratify_var = NULL,
  train_p = .75,
  strat_n_bins = 4L,
  target_length = NULL,
  id_strat = NULL,
  seed = NULL,
  verbosity = 1L
) {
  # Arguments
  type <- match_arg(
    type,
    c("KFold", "StratSub", "StratBoot", "Bootstrap", "LOOCV")
  )
  if (length(type) == 0) {
    cli::cli_abort(
      "Invalid resampler type. Must be one of: 'StratSub', 'StratBoot', 'KFold', 'Bootstrap', 'LOOCV'"
    )
  }
  seed <- clean_int(seed)

  if (type == "KFold") {
    KFoldConfig(
      n = n_resamples,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "StratSub") {
    StratSubConfig(
      n = n_resamples,
      train_p = train_p,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "StratBoot") {
    StratBootConfig(
      n = n_resamples,
      train_p = train_p,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      target_length = target_length,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "Bootstrap") {
    BootstrapConfig(
      n = n_resamples,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "LOOCV") {
    LOOCVConfig(
      n = NA_integer_
    )
  } else {
    cli::cli_abort(paste(
      "Resampler'",
      type,
      "'is not supported.",
      "Supported types are: 'KFold', 'StratSub', 'StratBoot', 'Bootstrap', 'LOOCV'."
    ))
  }
} # /rtemis::setup_Resampler

# Resampler ----
#' @title Resampler
#'
#' @description
#' Class for resampling objects.
#'
#' @author EDG
#' @noRd
Resampler <- new_class(
  name = "Resampler",
  properties = list(
    type = class_character,
    resamples = class_list,
    config = ResamplerConfig
  )
) # /rtemis::Resampler


# repr Resampler ----
#' repr Resampler
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Resampler) <- function(x, pad = 0L, output_type = NULL) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(x, pad = pad, output_type = output_type),
    repr_ls(
      props(x),
      pad = pad + 2L,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.Resampler

# Print Resampler ----
method(print, Resampler) <- function(
  x,
  output_type = c("ansi", "html", "plain"),
  ...
) {
  cat(repr(x, output_type = output_type))
  invisible(x)
}

# Names Resampler ----
method(names, Resampler) <- function(x) {
  names(x@resamples)
}

# Access Resampler$resamples resamples using `$` ----
method(`$`, Resampler) <- function(x, name) {
  x@resamples[[name]]
}

# DollarSign tab-complete Resampler@resamples names ----
method(`.DollarNames`, Resampler) <- function(x, pattern = "") {
  all_names <- names(x@resamples)
  grep(pattern, all_names, value = TRUE)
}

# Access Resampler$resamples resamples using `[[` ----
method(`[[`, Resampler) <- function(x, index) {
  x@resamples[[index]]
}

# desc Resampler ----
method(desc, Resampler) <- function(x) {
  desc(x@config)
}


# %% list_to_ResamplerConfig ----
list_to_ResamplerConfig <- function(x) {
  switch(
    x[["type"]],
    KFold = KFoldConfig(
      n = x[["n"]],
      stratify_var = x[["stratify_var"]],
      strat_n_bins = x[["strat_n_bins"]],
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    StratSub = StratSubConfig(
      n = x[["n"]],
      train_p = x[["train_p"]],
      stratify_var = x[["stratify_var"]],
      strat_n_bins = x[["strat_n_bins"]],
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    StratBoot = StratBootConfig(
      n = x[["n"]],
      train_p = x[["train_p"]],
      stratify_var = x[["stratify_var"]],
      strat_n_bins = x[["strat_n_bins"]],
      target_length = x[["target_length"]],
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    Bootstrap = BootstrapConfig(
      n = x[["n"]],
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    LOOCV = LOOCVConfig(
      n = NA_integer_
    ),
    Custom = CustomConfig(
      n = x[["n"]]
    )
  )
} # /rtemis::list_to_ResamplerConfig
