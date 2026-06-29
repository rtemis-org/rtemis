# 17_ClusterConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# References ----
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7/

# %% ClusterConfig ----
#' ClusterConfig Class
#'
#' @description
#' Clustering Pipeline Configuration Class. A portable, data-agnostic recipe for
#' a clustering pipeline: it bundles the input data path, the algorithm-specific
#' `ClusteringConfig`, and the output directory, so the same config can be
#' validated, shared, written to disk, or described without data and have a path
#' bound later (e.g. by the `rtemis` CLI) before [cluster].
#'
#' @author EDG
#' @noRd
ClusterConfig <- new_class(
  name = "ClusterConfig",
  package = "rtemis",
  properties = list(
    dat_path = class_character | NULL,
    algorithm = class_character | NULL,
    clustering_config = ClusteringConfig | NULL,
    outdir = class_character,
    verbosity = class_integer
  )
) # /rtemis::ClusterConfig


# %% repr.ClusterConfig ----
#' Repr ClusterConfig
#'
#' @param x `ClusterConfig` object.
#' @param pad Integer: Number of spaces to pad the message with.
#' @param output_type Character {"ansi", "html", or "plain"}: Output type.
#'
#' @return Character: Formatted string that can be printed with cat()
#'
#' @author EDG
#' @noRd
method(repr, ClusterConfig) <- function(x, pad = 0L, output_type = NULL) {
  out <- repr_S7name("ClusterConfig", pad = pad, output_type = output_type)
  out <- paste0(
    out,
    repr_ls(props(x), pad = pad, limit = 20L, output_type = output_type)
  )
  out
} # /rtemis::repr.ClusterConfig


# %% print.ClusterConfig ----
#' Print `ClusterConfig`
#'
#' @param x `ClusterConfig` object.
#' @param ... Not used.
#'
#' @author EDG
#' @noRd
method(print, ClusterConfig) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.ClusterConfig


# %% setup_ClusterConfig ----
#' Setup ClusterConfig
#'
#' Setup a `ClusterConfig` object. `ClusterConfig` is a portable, data-agnostic
#' recipe for a clustering pipeline: `dat_path` is optional, so the same config
#' can be validated, shared, or described without data and have a path bound
#' later (e.g. by the `rtemis` CLI) before [cluster].
#'
#' @param dat_path Character or NULL: Path to input data file. NULL leaves the
#' recipe unbound; set it (or supply data) before [cluster].
#' @param algorithm Character or NULL: Clustering algorithm. May be left NULL if
#' `clustering_config` is supplied (it carries its own algorithm).
#' @param clustering_config `ClusteringConfig` object or NULL: Configuration for
#' the clustering itself. Setup with a clustering `setup_*` function, e.g.
#' [setup_KMeans]. If NULL, defaults for `algorithm` are used at [cluster] time.
#' @param outdir Character: Output directory for results.
#' @param verbosity Integer: Verbosity level.
#'
#' @return `ClusterConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' cc <- setup_ClusterConfig(
#'   dat_path = "data.csv",
#'   clustering_config = setup_KMeans(k = 3L),
#'   outdir = "results/"
#' )
setup_ClusterConfig <- function(
  dat_path = NULL,
  algorithm = NULL,
  clustering_config = NULL,
  outdir = "results/",
  verbosity = 1L
) {
  # Sanitize paths for security
  if (!is.null(dat_path)) {
    dat_path <- sanitize_path(dat_path, must_exist = FALSE)
  }
  outdir <- sanitize_path(outdir, must_exist = FALSE, type = "any")

  ClusterConfig(
    dat_path = dat_path,
    algorithm = algorithm,
    clustering_config = clustering_config,
    outdir = outdir,
    verbosity = as.integer(verbosity)
  )
} # /rtemis::setup_ClusterConfig


# %% .list_to_ClusterConfig ----
#' Convert a list to a `ClusterConfig` object
#'
#' Internal function used by [read_config] to reconstruct a `ClusterConfig` from
#' a named list, such as the result of parsing a JSON config. The nested
#' `clustering_config` is rebuilt via `.list_to_ClusteringConfig()`.
#'
#' @param x Named list carrying `ClusterConfig` fields (e.g. `dat_path`,
#'   `clustering_config`, `outdir`).
#'
#' @return `ClusterConfig` object.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_ClusterConfig <- function(x) {
  args <- list(
    dat_path = x[["dat_path"]],
    algorithm = x[["algorithm"]],
    clustering_config = if (is.null(x[["clustering_config"]])) {
      NULL
    } else {
      .list_to_ClusteringConfig(x[["clustering_config"]])
    }
  )
  # `outdir` and `verbosity` carry non-NULL defaults in `setup_ClusterConfig`;
  # only override them when the config actually supplies a value, so a portable
  # recipe that omits them keeps the defaults.
  if (!is.null(x[["outdir"]])) {
    args[["outdir"]] <- x[["outdir"]]
  }
  if (!is.null(x[["verbosity"]])) {
    args[["verbosity"]] <- x[["verbosity"]]
  }
  do.call(setup_ClusterConfig, args)
} # /rtemis::.list_to_ClusterConfig
