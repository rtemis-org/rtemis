# S7_Resampler.R
# ::rtemis::
# 2025 EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7

# Description
# `ResamplerParameters` class and subclasses create objects that store resampling parameters
# They are set by `setup_Resampler()` and perform type checking and validation.
# They are used by `resample()`.
# `Resampler` class stores resamples and their parameters.
# `Resampler` objects are created by `resample()`.

# Notes
# `id_strat` is used by `resample()`, not individual resamplers

# ResamplerParameters ----
#' @title ResamplerParameters
#'
#' @description
#' Superclass for resampler parameters.
#'
#' @field type Character: Type of resampler.
#' @field n Integer: Number of resamples.
#'
#' @author EDG
#' @noRd
ResamplerParameters <- new_class(
  name = "ResamplerParameters",
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
) # /ResamplerParameters

# Make S7 properties `$`-accessible
method(`$`, ResamplerParameters) <- function(x, name) {
  prop(x, name)
}

# Make S7 properties `[[`-accessible
method(`[[`, ResamplerParameters) <- function(x, name) {
  prop(x, name)
}

#' Print ResamplerParameters
#'
#' @description
#' print ResamplerParameters object
#'
#' @param x ResamplerParameters object
#'
#' @author EDG
#' @noRd
method(print, ResamplerParameters) <- function(x, pad = 0L, ...) {
  objcat(paste(x@type, "ResamplerParameters"), pad = pad)
  printls(props(x)[-1], pad = pad + 2L)
  invisible(x)
} # rtemis::print.ResamplerParameters

# desc ResamplerParameters ----
method(desc, ResamplerParameters) <- function(x) {
  switch(
    x@type,
    KFold = paste0(x@n, "-fold crossvalidation"),
    StratSub = paste0(x@n, " stratified subsamples"),
    StratBoot = paste0(x@n, " stratified bootstraps"),
    Bootstrap = paste0(x@n, " bootstraps"),
    LOOCV = "Leave-one-out crossvalidation"
  )
} # /rtemis::desc.ResamplerParameters

# desc_alt ResamplerParameters ----
method(desc_alt, ResamplerParameters) <- function(x) {
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
} # /rtemis::desc.ResamplerParameters

# KFoldParams ----
#' @title KFoldParams
#'
#' @description
#' ResamplerParameters subclass for k-fold resampling.
#'
#' @author EDG
#' @noRd
KFoldParams <- new_class(
  name = "KFoldParams",
  parent = ResamplerParameters,
  properties = list(
    stratify_var = class_character | NULL,
    strat_n_bins = scalar_int_pos,
    id_strat = class_vector | NULL,
    seed = scalar_int_pos
  ),
  constructor = function(n, stratify_var, strat_n_bins, id_strat, seed) {
    new_object(
      ResamplerParameters(
        type = "KFold",
        n = n
      ),
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  }
) # /KFoldParams

# StratSubParams ----
#' @title StratSubParams
#'
#' @description
#' ResamplerParameters subclass for stratified subsampling.
#'
#' @author EDG
#' @noRd
StratSubParams <- new_class(
  name = "StratSubParams",
  parent = ResamplerParameters,
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
      ResamplerParameters(
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
) # /StratSubParams

# StratBootParams ----
#' @title StratBootParams
#'
#' @description
#' ResamplerParameters subclass for stratified bootstrapping.
#'
#' @author EDG
#' @noRd
StratBootParams <- new_class(
  name = "StratBootParams",
  parent = ResamplerParameters,
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
      ResamplerParameters(
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
) # /StratBootParams

# BootstrapParams ----
#' @title BootstrapParams
#'
#' @description
#' ResamplerParameters subclass for bootstrap resampling.
#'
#' @author EDG
#' @noRd
BootstrapParams <- new_class(
  name = "BootstrapParams",
  parent = ResamplerParameters,
  properties = list(
    id_strat = class_vector | NULL,
    seed = scalar_int_pos
  ),
  constructor = function(n, id_strat, seed) {
    new_object(
      ResamplerParameters(
        type = "Bootstrap",
        n = n
      ),
      id_strat = id_strat,
      seed = seed
    )
  }
) # /BootstrapParams

# LOOCVParams ----
#' @title LOOCVParams
#'
#' @description
#' ResamplerParameters subclass for leave-one-out cross-validation.
#'
#' @author EDG
#' @noRd
LOOCVParams <- new_class(
  name = "LOOCVParams",
  parent = ResamplerParameters,
  constructor = function(n) {
    new_object(
      ResamplerParameters(
        type = "LOOCV",
        n = n
      )
    )
  }
) # /LOOCVParams

# CustomParams ----
#' @title CustomParams
#'
#' @description
#' ResamplerParameters subclass for custom resampling.
#'
#' @author EDG
#' @noRd
CustomParams <- new_class(
  name = "CustomParams",
  parent = ResamplerParameters,
  constructor = function(n) {
    new_object(
      ResamplerParameters(
        type = "Custom",
        n = n
      )
    )
  }
) # /CustomParams

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
#' @return ResamplerParameters object.
#'
#' @author EDG
#' @export
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
    KFoldParams(
      n = n_resamples,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "StratSub") {
    StratSubParams(
      n = n_resamples,
      train_p = train_p,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "StratBoot") {
    StratBootParams(
      n = n_resamples,
      train_p = train_p,
      stratify_var = stratify_var,
      strat_n_bins = strat_n_bins,
      target_length = target_length,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "Bootstrap") {
    BootstrapParams(
      n = n_resamples,
      id_strat = id_strat,
      seed = seed
    )
  } else if (type == "LOOCV") {
    LOOCVParams(
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
} # /setup_Resampler

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
    parameters = ResamplerParameters
  )
) # /Resampler

# Print Resampler ----
#' Print Resampler
#'
#' Print Resampler object
#'
#' @param x Resampler object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
print.Resampler <- function(x, ...) {
  objcat(paste(x@type, "Resampler"))
  propsl <- props(x)
  # type is already printed by objcat
  propsl[["type"]] <- NULL
  printls(propsl)
  invisible(x)
}
method(print, Resampler) <- function(x, ...) {
  print.Resampler(x)
} # rtemis::print.Resampler

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
  desc(x@parameters)
} # /rtemis::desc.Resampler

# desc_alt Resampler ----
method(desc_alt, Resampler) <- function(x) {
  desc_alt(x@parameters)
} # /rtemis::desc_alt.Resampler

# print1.resample <- function(x, verbosity = 0L, ...) {
#   resampler <- attr(x, "resampler")
#   if (resampler == "loocv") {
#     .text <- "Leave-one-out crossvalidation"
#   } else {
#     .text <- paste0(
#       attr(x, "N"),
#       resamples <- switch(resampler,
#         strat_sub = " stratified subsamples",
#         bootstrap = " bootstraps",
#         strat_boot = " stratified bootstraps",
#         kfold = "-fold crossvalidation"
#       )
#     )
#   }

#   if (verbosity > 0L) print(.text)
#   invisible(.text)
# } # rtemis::print1.resample

# Plot Resampler ----
#' Plot Resampler
#'
#' Plot Resampler object
#'
#' @param x Resampler object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
# plot.Resampler <- function(x, col = NULL, ...) {
#   mplot3_res(x, col = col, ...)
# }
# method(plot, Resampler) <- function(x, col = NULL, ...) {
#   plot.Resampler(x, col = col, ...)
# } # rtemis::plot.Resampler
