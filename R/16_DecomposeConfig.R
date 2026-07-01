# 16_DecomposeConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# References ----
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/

# %% DecomposeConfig ----
#' DecomposeConfig Class
#'
#' @description
#' Decomposition Pipeline Configuration Class. A portable, data-agnostic recipe
#' for a decomposition pipeline: it bundles the input data path, the
#' algorithm-specific `DecompositionConfig`, and the output directory, so the
#' same config can be validated, shared, written to disk, or described without
#' data and have a path bound later (e.g. by the `rtemis` CLI) before [decomp].
#'
#' @author EDG
#' @noRd
DecomposeConfig <- new_class(
  name = "DecomposeConfig",
  package = "rtemis",
  properties = list(
    dat_path = class_character | NULL,
    algorithm = class_character | NULL,
    decomposition_config = DecompositionConfig | NULL,
    outdir = class_character,
    verbosity = class_integer
  )
) # /rtemis::DecomposeConfig


# %% repr.DecomposeConfig ----
#' Repr DecomposeConfig
#'
#' @param x `DecomposeConfig` object.
#' @param pad Integer: Number of spaces to pad the message with.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted string that can be printed with cat()
#'
#' @author EDG
#' @noRd
method(repr, DecomposeConfig) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("DecomposeConfig", pad = pad, output_type = output_type)
  out <- paste0(
    out,
    repr_ls(props(x), pad = pad, limit = 20L, output_type = output_type)
  )
  out
} # /rtemis::repr.DecomposeConfig


# %% print.DecomposeConfig ----
#' Print `DecomposeConfig`
#'
#' @param x `DecomposeConfig` object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, DecomposeConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.DecomposeConfig


# %% setup_DecomposeConfig ----
#' Setup DecomposeConfig
#'
#' Setup a `DecomposeConfig` object. `DecomposeConfig` is a portable,
#' data-agnostic recipe for a decomposition pipeline: `dat_path` is optional, so
#' the same config can be validated, shared, or described without data and have a
#' path bound later (e.g. by the `rtemis` CLI) before [decomp].
#'
#' @param dat_path Character or NULL: Path to input data file. NULL leaves the
#' recipe unbound; set it (or supply data) before [decomp].
#' @param algorithm Character or NULL: Decomposition algorithm. May be left NULL
#' if `decomposition_config` is supplied (it carries its own algorithm).
#' @param decomposition_config `DecompositionConfig` object or NULL: Configuration
#' for the decomposition itself. Setup with a decomposition `setup_*` function,
#' e.g. [setup_PCA]. If NULL, defaults for `algorithm` are used at [decomp] time.
#' @param outdir Character: Output directory for results.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `DecomposeConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' dc <- setup_DecomposeConfig(
#'   dat_path = "data.csv",
#'   decomposition_config = setup_PCA(k = 3L),
#'   outdir = "results/"
#' )
setup_DecomposeConfig <- function(
  dat_path = NULL,
  algorithm = NULL,
  decomposition_config = NULL,
  outdir = "results/",
  verbosity = 1L
) {
  # Sanitize paths for security
  if (!is.null(dat_path)) {
    dat_path <- sanitize_path(dat_path, must_exist = FALSE)
  }
  outdir <- sanitize_path(outdir, must_exist = FALSE, type = "any")

  DecomposeConfig(
    dat_path = dat_path,
    algorithm = algorithm,
    decomposition_config = decomposition_config,
    outdir = outdir,
    verbosity = as.integer(verbosity)
  )
} # /rtemis::setup_DecomposeConfig


# %% .list_to_DecomposeConfig ----
#' Convert a list to a `DecomposeConfig` object
#'
#' Internal function used by [read_config] to reconstruct a `DecomposeConfig`
#' from a named list, such as the result of parsing a JSON config. The nested
#' `decomposition_config` is rebuilt via `.list_to_DecompositionConfig`.
#'
#' @param x Named list carrying `DecomposeConfig` fields (e.g. `dat_path`,
#'   `decomposition_config`, `outdir`).
#'
#' @return `DecomposeConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_DecomposeConfig <- function(x) {
  args <- list(
    dat_path = x[["dat_path"]],
    algorithm = x[["algorithm"]],
    decomposition_config = if (is.null(x[["decomposition_config"]])) {
      NULL
    } else {
      .list_to_DecompositionConfig(x[["decomposition_config"]])
    }
  )
  # `outdir` and `verbosity` carry non-NULL defaults in `setup_DecomposeConfig`;
  # only override them when the config actually supplies a value, so a portable
  # recipe that omits them keeps the defaults.
  if (!is.null(x[["outdir"]])) {
    args[["outdir"]] <- x[["outdir"]]
  }
  if (!is.null(x[["verbosity"]])) {
    args[["verbosity"]] <- x[["verbosity"]]
  }
  do.call(setup_DecomposeConfig, args)
} # /rtemis::.list_to_DecomposeConfig
