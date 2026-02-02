# S7_Decomposition.R
# ::rtemis::
# 2025 EDG rtemis.org

# Decomposition ----
#' @title Decomposition
#'
#' @description
#' Decomposition class.
#'
#' @field algorithm Character: Algorithm name.
#' @field decom Any: Decomposition object.
#' @field config List: Algorithm-specific config.
#' @field decom: Decomposition model.
#' @field transformed: transformedransformed data, i.e. either a projection or an embedding of the input data.
#'
#' @author EDG
#' @noRd
Decomposition <- new_class(
  name = "Decomposition",
  properties = list(
    algorithm = class_character,
    config = DecompositionConfig,
    decom = class_any,
    transformed = class_any
  )
) # /rtemis::Decomposition


# Make Decomposition properties `$`-accessible
method(`$`, Decomposition) <- function(x, name) {
  prop_names <- names(props(x))
  if (name %in% prop_names) {
    prop(x, name)
  } else {
    cli::cli_abort(paste0(
      "No property named '",
      name,
      "' in Decomposition object."
    ))
  }
}
method(`.DollarNames`, Decomposition) <- function(x, pattern = "") {
  prop_names <- names(props(x))
  grep(pattern, prop_names, value = TRUE)
}

# Make Decomposition@transformed `[[`-accessible
method(`[[`, Decomposition) <- function(x, index) {
  props(x, "transformed")[[index]]
}


# Show Decomposition ----
method(repr, Decomposition) <- function(
  x,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name(
      paste(x@algorithm, "Decomposition"),
      pad = pad,
      output_type = output_type
    ),
    repr_ls(props(x)[-1], pad = pad, output_type = output_type)
  )
} # /rtemis::show.Decomposition


# Print Decomposition ----
method(print, Decomposition) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.Decomposition
