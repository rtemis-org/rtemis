# S7_Clustering.R
# ::rtemis::
# 2025- EDG rtemis.org

# Clustering ----
#' @title Clustering
#'
#' @description
#' Clustering class.
#'
#' @field algorithm Character: Algorithm name.
#' @field clust Any: Clustering object.
#' @field k Integer: Number of clusters.
#' @field clusters List: Cluster assignment.
#' @field config ClusteringConfig: Algorithm-specific config.
#'
#' @author EDG
#' @noRd
Clustering <- new_class(
  name = "Clustering",
  properties = list(
    algorithm = class_character,
    clust = class_any,
    k = class_integer,
    clusters = class_integer | class_list,
    config = ClusteringConfig
  )
) # /Clustering

# Make Clustering props `$`-accessible
method(`$`, Clustering) <- function(x, name) {
  prop(x, name)
}

# `$`-autocomplete Clustering props
method(`.DollarNames`, Clustering) <- function(x, pattern = "") {
  prop_names <- names(props(x))
  grep(pattern, prop_names, value = TRUE)
}

# Make Clustering props `[[`-accessible
method(`[[`, Clustering) <- function(x, index) {
  props(x, index)
}


# Show Clustering ----
method(repr, Clustering) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(paste(x$algorithm, "Clustering")),
    repr_ls(props(x)[-1], pad = pad, output_type = output_type)
  )
} # /rtemis::show.Clustering


# Print Clustering ----
method(print, Clustering) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.Clustering
