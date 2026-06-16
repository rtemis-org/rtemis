# reexports.R
# ::rtemis::
# 2026- EDG rtemis.org

# Functions that moved to rtemis.core but remain part of the rtemis public API.
# Re-exported here so existing rtemis::<fn> usage continues to work unchanged.

#' @importFrom rtemis.core ddSci
#' @export
rtemis.core::ddSci


#' @importFrom rtemis.core labelify
#' @export
rtemis.core::labelify


#' @importFrom rtemis.core clean_names
#' @export
rtemis.core::clean_names


#' @importFrom rtemis.core clean_colnames
#' @export
rtemis.core::clean_colnames
