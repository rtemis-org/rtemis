# S7_Clustering.R
# ::rtemis::
# 2025 EDG rtemis.org

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
#' @field parameters ClusteringParameters: Algorithm-specific parameters.
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
    parameters = ClusteringParameters
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

# !! Make Clustering@clusters `[[`-accessible
# method(`[[`, Clustering) <- function(x, index) {
#   props(x, "clusters")[[index]]
# }

# Show Clustering ----
method(repr, Clustering) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    show_S7name(paste(x$algorithm, "Clustering")),
    show_ls(props(x)[-1], pad = pad, output_type = output_type)
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
