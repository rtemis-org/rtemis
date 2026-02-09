# S7_SuperConfig.R
# ::rtemis::
# 2025- EDG rtemis.org

# References ----
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/

# SuperConfig ----
#' SuperConfig Class
#'
#' @description
#' Supervised Learning Configuration Class.
#'
#' @author EDG
#' @noRd
SuperConfig <- new_class(
  name = "SuperConfig",
  properties = list(
    dat_training_path = class_character,
    dat_validation_path = class_character,
    dat_test_path = class_character,
    algorithm = class_character,
    preprocessor_config = PreprocessorConfig | NULL,
    hyperparameters = Hyperparameters | NULL,
    tuner_config = TunerConfig | NULL,
    outer_resampling_config = ResamplerConfig | NULL,
    weights = class_character | NULL, # column name in dat_training
    question = class_character | NULL,
    outdir = class_character,
    verbosity = class_integer
  )
) # /rtemis::SuperConfig


# repr SuperConfig ----
#' Repr SuperConfig
#'
#' @param x `SuperConfig` object.

# Print `SuperConfig` ----
#' Print `SuperConfig`
#'
#' Print `SuperConfig` object
#'
#' @param x `SuperConfig` object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, SuperConfig) <- function(x, ...) {
  objcat("SuperConfig")
  printls(props(x))
  invisible(x)
} # /rtemis::print.SuperConfig
