# resolve_hyperparameters.R
# ::rtemis::
# 2026- EDG rtemis.org

# Recording the values a backend chose for hyperparameters left unset.
#
# A hyperparameter left NULL is passed to the backend as "choose", and the
# backend applies its own rule (ranger's `mtry` from the number of predictors,
# earth's `nk` from the design width). A `train_` method reads the value the
# backend used from the fitted object and records it on the hyperparameters it
# returns, so the fitted model, its record and its writeup state every value
# the fit used. The record reports such a value with origin "derived".
#
# spec: rtemis/writeup

# %% hyperparameter_in_effect ----
#' Whether a hyperparameter has an effect under its siblings' values
#'
#' A property declared with `applies_when` is in effect only when every gating
#' sibling holds one of the listed values.
#'
#' @param hyperparameters `Hyperparameters` object.
#' @param name Character: Property name.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
hyperparameter_in_effect <- function(hyperparameters, name) {
  cls <- S7_class(hyperparameters)
  gate <- get_spec_fields(cls@properties[[name]])[["applies_when"]]
  for (gate_name in names(gate)) {
    if (!any(prop(hyperparameters, gate_name) %in% gate[[gate_name]])) {
      return(FALSE)
    }
  }
  TRUE
} # /rtemis::hyperparameter_in_effect


# %% record_backend_values ----
#' Record the values a backend chose for unset hyperparameters
#'
#' Each value is recorded only where the hyperparameter is unset, declared by
#' the class, and in effect.
#'
#' @param hyperparameters `Hyperparameters` object: As passed to the backend.
#' @param values Named list: Value the backend used, by hyperparameter name.
#'   NULL entries are skipped.
#'
#' @return `Hyperparameters` object with the values recorded.
#'
#' @author EDG
#' @keywords internal
#' @noRd
record_backend_values <- function(hyperparameters, values) {
  properties <- S7_class(hyperparameters)@properties
  for (nm in names(values)) {
    value <- values[[nm]]
    if (
      is.null(value) ||
        !nm %in% names(properties) ||
        !is.null(prop(hyperparameters, nm)) ||
        !hyperparameter_in_effect(hyperparameters, nm)
    ) {
      next
    }
    prop(hyperparameters, nm) <- value
  }
  hyperparameters
} # /rtemis::record_backend_values


# %% lightgbm_model_parameters ----
#' The parameters a fitted LightGBM model ran with
#'
#' LightGBM writes every parameter, including the ones it resolved, to the
#' `parameters:` block of its model text, one `[name: value]` line each.
#'
#' @param model `lgb.Booster` object.
#'
#' @return Named character vector of parameter values.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_model_parameters <- function(model) {
  text <- model$save_model_to_string()
  start <- regexpr("\nparameters:\n", text, fixed = TRUE)
  if (start < 0L) {
    return(character())
  }
  block <- substring(text, start + nchar("\nparameters:\n"))
  block <- sub("\nend of parameters.*", "", block)
  lines <- strsplit(block, "\n", fixed = TRUE)[[1L]]
  lines <- lines[grepl("^\\[[a-z_0-9]+: .*\\]$", lines)]
  stats::setNames(
    sub("^\\[[a-z_0-9]+: (.*)\\]$", "\\1", lines),
    sub("^\\[([a-z_0-9]+): .*$", "\\1", lines)
  )
} # /rtemis::lightgbm_model_parameters


# %% lightgbm_backend_values ----
#' Values a fitted LightGBM model used for the unset hyperparameters
#'
#' Reads each unset scalar hyperparameter whose name is a LightGBM parameter
#' from the model's parameter block, converted to the property's declared type.
#'
#' @param hyperparameters `Hyperparameters` object of a LightGBM-backed
#'   algorithm.
#' @param model `lgb.Booster` object.
#'
#' @return Named list for `record_backend_values()`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
lightgbm_backend_values <- function(hyperparameters, model) {
  parameters <- lightgbm_model_parameters(model)
  properties <- S7_class(hyperparameters)@properties
  unset <- names(properties)[vapply(
    names(properties),
    function(nm) {
      nm %in%
        names(parameters) &&
        nzchar(parameters[[nm]]) &&
        is.null(prop(hyperparameters, nm)) &&
        !identical(prop_role(properties[[nm]]), "state")
    },
    logical(1L)
  )]
  values <- lapply(unset, function(nm) {
    fields <- get_spec_fields(properties[[nm]])
    if (is.null(fields) || !identical(fields[["container"]], "none")) {
      return(NULL)
    }
    raw <- parameters[[nm]]
    switch(
      fields[["type"]],
      integer = as.integer(raw),
      number = as.numeric(raw),
      boolean = as.integer(raw) != 0L,
      string = raw,
      NULL
    )
  })
  stats::setNames(values, unset)
} # /rtemis::lightgbm_backend_values
