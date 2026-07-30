# 100_Resampler.R
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

# %% ResamplerConfig ----
#' ResamplerConfig
#'
#' @description
#' Superclass for resampler configuration.
#'
#' @field type Character: Type of resampler.
#'
#' @details
#' `n_resamples` is declared per subclass, not here: for every type but LOOCV it
#' is ordinary config with a default, while LOOCV derives it from the data
#' (`resample()` fills it in), which makes it run state. Declaring it twice over
#' is what lets each leaf schema state its own contract instead of the
#' dispatcher carrying a "required unless type is LOOCV" rule.
#'
#' @author EDG
#' @noRd
ResamplerConfig <- new_class(
  name = "ResamplerConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    type = class_character
  )
) # /rtemis::ResamplerConfig


# %% serializable_props.ResamplerConfig ----
# A resampler serializes its type-specific settings as siblings of `type`
# (there is no nested `config` object). `id_strat` is written like any other
# input: it decides which cases stay together, so a config that dropped it
# would resample differently on re-read.
method(serializable_props, ResamplerConfig) <- function(x) {
  # `type` is the only base property; everything else, `n_resamples` included,
  # is declared per subclass and arrives through `config_prop_values()` -- which
  # drops LOOCV's, that being state the data determine.
  c(list(type = x@type), config_prop_values(x, ResamplerConfig))
} # /rtemis::serializable_props.ResamplerConfig


# %% `$`.ResamplerConfig ----
# Make S7 properties `$`-accessible
method(`$`, ResamplerConfig) <- function(x, name) {
  prop(x, name)
}


# %% `[[`.ResamplerConfig ----
# Make S7 properties `[[`-accessible
method(`[[`, ResamplerConfig) <- function(x, name) {
  prop(x, name)
}


# %% repr.ResamplerConfig ----
#' repr ResamplerConfig
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, ResamplerConfig) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name(x, pad = pad, output_type = output_type),
    repr_ls(
      props(x)[-1],
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.ResamplerConfig


# %% print.ResamplerConfig ----
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
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.ResamplerConfig


# %% desc.ResamplerConfig ----
method(desc, ResamplerConfig) <- function(x) {
  switch(
    x@type,
    KFold = paste0(x@n_resamples, " independent folds"),
    StratSub = paste0(x@n_resamples, " stratified subsamples"),
    StratBoot = paste0(x@n_resamples, " stratified bootstraps"),
    Bootstrap = paste0(x@n_resamples, " bootstrap resamples"),
    Custom = paste0(x@n_resamples, " custom resamples"),
    LOOCV = paste0(x@n_resamples, " leave-one-out folds"),
    paste0(x@n_resamples, " resamples")
  )
} # /rtemis::desc.ResamplerConfig


# %% KFoldConfig ----
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
    type = prop_algorithm("KFold"),
    n_resamples = prop_integer(
      10L,
      min = 1L,
      description = "Number of resamples."
    ),
    stratify_var = prop_string(
      NULL,
      nullable = TRUE,
      description = "Name of the variable to stratify by."
    ),
    strat_n_bins = prop_integer(
      4L,
      min = 1L,
      description = "Number of bins to stratify a continuous variable into."
    ),
    id_strat = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_cases",
      data_dependent = TRUE,
      description = "Per-case grouping IDs; cases sharing an ID stay in the same resample."
    ),
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Random seed."
    )
  )
) # /rtemis::KFoldConfig


# %% StratSubConfig ----
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
    type = prop_algorithm("StratSub"),
    n_resamples = prop_integer(
      10L,
      min = 1L,
      description = "Number of resamples."
    ),
    train_p = prop_float(
      0.75,
      exclusive_min = 0,
      exclusive_max = 1,
      description = "Training set fraction."
    ),
    stratify_var = prop_string(
      NULL,
      nullable = TRUE,
      description = "Name of the variable to stratify by."
    ),
    strat_n_bins = prop_integer(
      4L,
      min = 1L,
      description = "Number of bins to stratify a continuous variable into."
    ),
    id_strat = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_cases",
      data_dependent = TRUE,
      description = "Per-case grouping IDs; cases sharing an ID stay in the same resample."
    ),
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Random seed."
    )
  )
) # /rtemis::StratSubConfig


# %% StratBootConfig ----
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
    type = prop_algorithm("StratBoot"),
    n_resamples = prop_integer(
      10L,
      min = 1L,
      description = "Number of resamples."
    ),
    stratify_var = prop_string(
      NULL,
      nullable = TRUE,
      description = "Name of the variable to stratify by."
    ),
    train_p = prop_float(
      0.75,
      exclusive_min = 0,
      exclusive_max = 1,
      description = "Training set fraction."
    ),
    strat_n_bins = prop_integer(
      4L,
      min = 1L,
      description = "Number of bins to stratify a continuous variable into."
    ),
    target_length = prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Target length for stratified bootstraps."
    ),
    id_strat = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_cases",
      data_dependent = TRUE,
      description = "Per-case grouping IDs; cases sharing an ID stay in the same resample."
    ),
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Random seed."
    )
  )
) # /rtemis::StratBootConfig


# %% BootstrapConfig ----
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
    type = prop_algorithm("Bootstrap"),
    n_resamples = prop_integer(
      10L,
      min = 1L,
      description = "Number of resamples."
    ),
    id_strat = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      data_bound = "n_cases",
      data_dependent = TRUE,
      description = "Per-case grouping IDs; cases sharing an ID stay in the same resample."
    ),
    seed = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      description = "Random seed."
    )
  )
) # /rtemis::BootstrapConfig


# %% LOOCVConfig ----
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
  properties = list(
    type = prop_algorithm("LOOCV"),
    # One resample per case, so only the data can say how many: `resample()`
    # writes it. A user never supplies it, hence state rather than config.
    n_resamples = prop_state(prop_integer(
      NULL,
      min = 1L,
      nullable = TRUE,
      description = "Number of resamples, one per case; set from the data."
    ))
  )
) # /rtemis::LOOCVConfig


# %% CustomConfig ----
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
  properties = list(
    type = prop_algorithm("Custom"),
    n_resamples = prop_integer(
      10L,
      min = 1L,
      description = "Number of resamples."
    )
  )
) # /rtemis::CustomConfig


# %% setup_Resampler ----
#' Setup Resampler
#'
#' @param n_resamples Integer [1, Inf): Number of resamples to make. LOOCV
#'   determines it from the data and ignores this.
#' @param type Character \{"KFold", "StratSub", "StratBoot", "Bootstrap", "LOOCV", "Custom"\}:
#'   Type of resampler.
#' @param stratify_var Optional Character: Variable to stratify by.
#' @param train_p Numeric (0, 1): Training set percentage.
#' @param strat_n_bins Integer [1, Inf): Number of bins to stratify by.
#' @param target_length Optional Integer [1, Inf): Target length for stratified bootstraps.
#' @param id_strat Optional Character vector: Per-case grouping IDs, e.g. subject IDs when the
#' dataset contains repeated measurements. Cases sharing an ID stay in the same resample, so a
#' case can only be present in the training or the test set, not both.
#' @param seed Optional Integer [0, Inf): Random seed.
#' @param verbosity Integer: Verbosity level.
#'
#' @return ResamplerConfig object.
#'
#' @author EDG
#' @export
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
    rtemis.core::abort(
      "Invalid resampler type. Must be one of: 'StratSub', 'StratBoot', 'KFold', 'Bootstrap', 'LOOCV'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  seed <- clean_int(seed)
  n_resamples <- clean_posint(n_resamples)
  strat_n_bins <- clean_posint(strat_n_bins)
  target_length <- clean_posint(target_length)

  if (type == "KFold") {
    KFoldConfig(
      n_resamples = n_resamples,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "StratSub") {
    StratSubConfig(
      n_resamples = n_resamples,
      train_p = train_p,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "StratBoot") {
    StratBootConfig(
      n_resamples = n_resamples,
      train_p = train_p,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      target_length = target_length,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "Bootstrap") {
    BootstrapConfig(
      n_resamples = n_resamples,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "LOOCV") {
    # `n_resamples` is left unset: determined by the data in `resample()`.
    LOOCVConfig()
  } else {
    rtemis.core::abort(
      "Resampler '",
      type,
      "' is not supported. ",
      "Supported types are: 'KFold', 'StratSub', 'StratBoot', 'Bootstrap', 'LOOCV'.",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
} # /rtemis::setup_Resampler


# %% Resampler ----
#' Resampler
#'
#' @description
#' Class for resampling objects.
#'
#' @author EDG
#' @noRd
Resampler <- new_class(
  name = "Resampler",
  package = "rtemis",
  properties = list(
    type = class_character,
    resamples = class_list,
    config = ResamplerConfig
  )
) # /rtemis::Resampler


# %% repr.Resampler ----
#' repr Resampler
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, Resampler) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name(x, pad = pad, output_type = output_type),
    repr_ls(
      props(x),
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.Resampler


# %% print.Resampler ----
method(print, Resampler) <- function(
  x,
  output_type = NULL,
  ...
) {
  cat(repr(x, output_type = output_type))
  invisible(x)
}

# %% names.Resampler ----
method(names, Resampler) <- function(x) {
  names(x@resamples)
}


# %% `$`.Resampler ----
# Access Resampler$resamples resamples using `$` ----
method(`$`, Resampler) <- function(x, name) {
  x@resamples[[name]]
}


# %% `.DollarNames`.Resampler ----
# DollarSign tab-complete Resampler@resamples names
method(`.DollarNames`, Resampler) <- function(x, pattern = "") {
  all_names <- names(x@resamples)
  grep(pattern, all_names, value = TRUE)
}


# %% `[[`.Resampler ----
# Access Resampler$resamples resamples using `[[` ----
method(`[[`, Resampler) <- function(x, index) {
  x@resamples[[index]]
}


# %% desc.Resampler ----
method(desc, Resampler) <- function(x) {
  desc(x@config)
}


# %% --- Internal functions ----

# %% .list_to_ResamplerConfig ----
#' Convert a list to a ResamplerConfig object
#'
#' Internal function used by `rtemis.server` and `SuperConfig` deserialization
#' to reconstruct a `ResamplerConfig` object from a named list. Not intended
#' for direct use by end users.
#'
#' @param x Named list with the following elements:
#'   \describe{
#'     \item{`type`}{Character: resampler type -- one of `"KFold"`,
#'       `"StratSub"`, `"StratBoot"`, `"Bootstrap"`, `"LOOCV"`, `"Custom"`.}
#'     \item{`n`}{Integer: number of resamples (not used for `"LOOCV"`).}
#'     \item{`train_p`}{Numeric: training proportion (used by `"StratSub"` and
#'       `"StratBoot"`).}
#'     \item{`stratify_var`}{Character or `NULL`: stratification variable name.}
#'     \item{`strat_n_bins`}{Integer: number of bins for stratification.}
#'     \item{`target_length`}{Integer or `NULL`: target resample length
#'       (`"StratBoot"` only).}
#'     \item{`id_strat`}{Character or `NULL`: ID stratification variable.}
#'     \item{`seed`}{Integer or `NULL`: random seed.}
#'   }
#'
#' @return A `ResamplerConfig` object of the appropriate subtype.
#'
#' @author EDG
#' @keywords internal
#' @export
#' @examples
#' .list_to_ResamplerConfig(list(type = "KFold", n_resamples = 5L))
.list_to_ResamplerConfig <- function(x) {
  n_resamples <- x[["n_resamples"]]
  # Drop absent (NULL) elements so class defaults apply for non-nullable
  # properties such as `strat_n_bins` and `train_p`.
  args <- switch(
    x[["type"]],
    KFold = list(
      constructor = KFoldConfig,
      n_resamples = n_resamples,
      stratify_var = x[["stratify_var"]],
      strat_n_bins = x[["strat_n_bins"]],
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    StratSub = list(
      constructor = StratSubConfig,
      n_resamples = n_resamples,
      train_p = x[["train_p"]],
      stratify_var = x[["stratify_var"]],
      strat_n_bins = x[["strat_n_bins"]],
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    StratBoot = list(
      constructor = StratBootConfig,
      n_resamples = n_resamples,
      train_p = x[["train_p"]],
      stratify_var = x[["stratify_var"]],
      strat_n_bins = x[["strat_n_bins"]],
      target_length = x[["target_length"]],
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    Bootstrap = list(
      constructor = BootstrapConfig,
      n_resamples = n_resamples,
      id_strat = x[["id_strat"]],
      seed = x[["seed"]]
    ),
    # LOOCV `n_resamples` is unset until `resample()` sees the data.
    LOOCV = list(
      constructor = LOOCVConfig
    ),
    Custom = list(
      constructor = CustomConfig,
      n_resamples = n_resamples
    ),
    rtemis.core::abort(
      "Unsupported resampler type:",
      x[["type"]],
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  )
  constructor <- args[["constructor"]]
  args[["constructor"]] <- NULL
  # The variant is chosen, so its own properties are exactly what it accepts:
  # `train_p` on a KFold is named rather than dropped.
  check_wire_keys(
    x,
    names(constructor@properties),
    paste(x[["type"]], "resampler")
  )
  do.call(constructor, Filter(Negate(is.null), args))
} # /rtemis::.list_to_ResamplerConfig
