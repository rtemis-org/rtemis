# S7_Preprocessor.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7

# %% PreprocessorConfig ----
#' @title PreprocessorConfig
#'
#' @description
#' PreprocessorConfig class.
#'
#' @author EDG
#' @noRd
PreprocessorConfig <- new_class(
  name = "PreprocessorConfig",
  properties = list(
    complete_cases = class_logical,
    remove_features_thres = class_numeric | NULL,
    remove_cases_thres = class_numeric | NULL,
    missingness = class_logical,
    impute = class_logical,
    impute_type = class_character,
    impute_missRanger_params = class_list,
    impute_discrete = class_character,
    impute_continuous = class_character,
    integer2factor = class_logical,
    integer2numeric = class_logical,
    logical2factor = class_logical,
    logical2numeric = class_logical,
    numeric2factor = class_logical,
    numeric2factor_levels = class_character | NULL,
    numeric_cut_n = class_numeric,
    numeric_cut_labels = class_logical,
    numeric_quant_n = class_numeric,
    numeric_quant_NAonly = class_logical,
    unique_len2factor = class_numeric,
    character2factor = class_logical,
    factorNA2missing = class_logical,
    factorNA2missing_level = class_character,
    factor2integer = class_logical,
    factor2integer_startat0 = class_logical,
    scale = class_logical,
    center = class_logical,
    scale_centers = class_numeric | NULL,
    scale_coefficients = class_numeric | NULL,
    remove_constants = class_logical,
    remove_constants_skip_missing = class_logical,
    remove_duplicates = class_logical,
    remove_features = class_character | NULL,
    one_hot = class_logical,
    one_hot_levels = class_list | NULL,
    add_date_features = class_logical,
    date_features = class_character,
    add_holidays = class_logical,
    exclude = class_character | NULL
  )
) # /PreprocessorConfig


# %% names.PreprocessorConfig ----
# Names PreprocessorConfig
method(names, PreprocessorConfig) <- function(x) {
  names(props(x))
}


# %% `$`.PreprocessorConfig ----
# Make props `$`-accessible
method(`$`, PreprocessorConfig) <- function(x, name) {
  props(x)[[name]]
}


# %% `.DollarNames`.PreprocessorConfig ----
# DollarSign tab-complete property names
method(`.DollarNames`, PreprocessorConfig) <- function(x, pattern = "") {
  all_names <- names(props(x))
  grep(pattern, all_names, value = TRUE)
}


# %% `[[`.PreprocessorConfig ----
# Make proprs `[[`-accessible
method(`[[`, PreprocessorConfig) <- function(x, name) {
  props(x)[[name]]
}


# %% repr.PreprocessorConfig ----
method(repr, PreprocessorConfig) <- function(
  x,
  limit = -1L,
  pad = 0L,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("PreprocessorConfig", pad = pad, output_type = output_type),
    repr_ls(props(x), pad = pad, limit = limit, output_type = output_type)
  )
} # /rtemis::repr.PreprocessorConfig


# %% print.PreprocessorConfig ----
method(print, PreprocessorConfig) <- function(
  x,
  limit = -1L,
  output_type = NULL,
  ...
) {
  cat(repr(x, limit = limit, output_type = output_type))
  invisible(x)
} # /rtemis::print.PreprocessorConfig


# %% setup_Preprocessor ----
#' Setup Preprocessor
#'
#' @description
#' Creates a `PreprocessorConfig` object, which can be used in [preprocess].
#'
#' @param complete_cases Logical: If TRUE, only retain complete cases (no missing data).
#' @param remove_cases_thres Float (0, 1): Remove cases with >= to this fraction
#' of missing features.
#' @param remove_features_thres Float (0, 1): Remove features with missing
#' values in >= to this fraction of cases.
#' @param missingness Logical: If TRUE, generate new boolean columns for each
#' feature with missing values, indicating which cases were missing data.
#' @param impute Logical: If TRUE, impute missing cases. See `impute_discrete` and
#' `impute_continuous`.
#' @param impute_type Character: Package to use for imputation.
#' @param impute_missRanger_params Named list with elements "pmm.k" and
#' "maxiter", which are passed to `missRanger::missRanger`. `pmm.k`
#' greater than 0 results in predictive mean matching. Default `pmm.k = 3`
#' `maxiter = 10` `num.trees = 500`. Reduce `num.trees` for
#' faster imputation especially in large datasets. Set `pmm.k = 0` to
#' disable predictive mean matching.
#' @param impute_discrete Character: Name of function that returns single value: How to impute
#' discrete variables for `impute_type = "meanMode"`.
#' @param impute_continuous Character: Name of function that returns single value: How to impute
#' continuous variables for `impute_type = "meanMode"`.
#' @param integer2factor Logical: If TRUE, convert all integers to factors. This includes
#' `bit64::integer64` columns.
#' @param integer2numeric Logical: If TRUE, convert all integers to numeric
#' (will only work if `integer2factor = FALSE`). This includes
#' `bit64::integer64` columns.
#' @param logical2factor Logical: If TRUE, convert all logical variables to
#' factors.
#' @param logical2numeric Logical: If TRUE, convert all logical variables to
#' numeric.
#' @param numeric2factor Logical: If TRUE, convert all numeric variables to
#' factors.
#' @param numeric2factor_levels Character vector: Optional - will be passed to
#' `levels` arg of `factor()` if `numeric2factor = TRUE`. For advanced/
#' specific use cases; need to know unique values of numeric vector(s) and given all
#' numeric vars have same unique values.
#' @param numeric_cut_n Integer: If > 0, convert all numeric variables to factors by
#' binning using `base::cut` with `breaks` equal to this number.
#' @param numeric_cut_labels Logical: The `labels` argument of [base::cut].
#' @param numeric_quant_n Integer: If > 0, convert all numeric variables to factors by
#' binning using `base::cut` with `breaks` equal to this number of quantiles.
#' produced using `stats::quantile`.
#' @param numeric_quant_NAonly Logical: If TRUE, only bin numeric variables with
#' missing values.
#' @param unique_len2factor Integer (>=2): Convert all variables with less
#' than or equal to this number of unique values to factors.
#' For example, if binary variables are encoded with 1, 2, you could use
#' `unique_len2factor = 2` to convert them to factors.
#' @param character2factor Logical: If TRUE, convert all character variables to
#' factors.
#' @param factorNA2missing Logical: If TRUE, make NA values in factors be of
#' level `factorNA2missing_level`. In many cases this is the preferred way
#' to handle missing data in categorical variables. Note that since this step
#' is performed before imputation, you can use this option to handle missing
#' data in categorical variables and impute numeric variables in the same
#' `preprocess` call.
#' @param factorNA2missing_level Character: Name of level if
#' `factorNA2missing = TRUE`.
#' @param factor2integer Logical: If TRUE, convert all factors to integers.
#' @param factor2integer_startat0 Logical: If TRUE, start integer coding at 0.
#' @param scale Logical: If TRUE, scale columns of `x`.
#' @param center Logical: If TRUE, center columns of `x`. Note that by
#' default it is the same as `scale`.
#' @param scale_centers Named vector: Centering values for each feature.
#' @param scale_coefficients Named vector: Scaling values for each feature.
#' @param remove_constants Logical: If TRUE, remove constant columns.
#' @param remove_constants_skip_missing Logical: If TRUE, skip missing values, before
#' checking if feature is constant.
#' @param remove_features Character vector: Features to remove.
#' @param remove_duplicates Logical: If TRUE, remove duplicate cases.
#' @param one_hot Logical: If TRUE, convert all factors using one-hot encoding.
#' @param one_hot_levels List: Named list of the form "feature_name" = "levels". Used when applying
#' one-hot encoding to validation or test data using `Preprocessor`.
#' @param add_date_features Logical: If TRUE, extract date features from date columns.
#' @param date_features Character vector: Features to extract from dates.
#' @param add_holidays Logical: If TRUE, extract holidays from date columns.
#' @param exclude Integer, vector: Exclude these columns from preprocessing.
#'
#' @section Order of Operations:
#'
#'   * keep complete cases only
#'   * remove constants
#'   * remove duplicates
#'   * remove cases by missingness threshold
#'   * remove features by missingness threshold
#'   * integer to factor
#'   * integer to numeric
#'   * logical to factor
#'   * logical to numeric
#'   * numeric to factor
#'   * cut numeric to n bins
#'   * cut numeric to n quantiles
#'   * numeric with less than N unique values to factor
#'   * character to factor
#'   * factor NA to named level
#'   * add missingness column
#'   * impute
#'   * scale and/or center
#'   * one-hot encoding
#'
#' @return `PreprocessorConfig` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' preproc_config <- setup_Preprocessor(factorNA2missing = TRUE)
#' preproc_config
setup_Preprocessor <- function(
  complete_cases = FALSE,
  remove_features_thres = NULL,
  remove_cases_thres = NULL,
  missingness = FALSE,
  impute = FALSE,
  impute_type = c(
    "missRanger",
    "micePMM",
    "meanMode"
  ),
  impute_missRanger_params = list(
    pmm.k = 3,
    maxiter = 10,
    num.trees = 500
  ),
  impute_discrete = "get_mode",
  impute_continuous = "mean",
  integer2factor = FALSE,
  integer2numeric = FALSE,
  logical2factor = FALSE,
  logical2numeric = FALSE,
  numeric2factor = FALSE,
  numeric2factor_levels = NULL,
  numeric_cut_n = 0,
  numeric_cut_labels = FALSE,
  numeric_quant_n = 0,
  numeric_quant_NAonly = FALSE,
  unique_len2factor = 0,
  character2factor = FALSE,
  factorNA2missing = FALSE,
  factorNA2missing_level = "missing",
  #    nonzeroFactors = FALSE,
  factor2integer = FALSE,
  factor2integer_startat0 = TRUE,
  scale = FALSE,
  center = scale,
  scale_centers = NULL,
  scale_coefficients = NULL,
  remove_constants = FALSE,
  remove_constants_skip_missing = TRUE,
  remove_features = NULL,
  remove_duplicates = FALSE,
  one_hot = FALSE,
  one_hot_levels = NULL,
  #    cleanfactorlevels = FALSE,
  add_date_features = FALSE,
  date_features = c("weekday", "month", "year"),
  add_holidays = FALSE,
  exclude = NULL
) {
  # Match args
  impute_type <- match.arg(impute_type)
  # Checks performed in the `PreprocessorConfig` constructor
  PreprocessorConfig(
    complete_cases = complete_cases,
    remove_features_thres = remove_features_thres,
    remove_cases_thres = remove_cases_thres,
    missingness = missingness,
    impute = impute,
    impute_type = impute_type,
    impute_missRanger_params = impute_missRanger_params,
    impute_discrete = impute_discrete,
    impute_continuous = impute_continuous,
    integer2factor = integer2factor,
    integer2numeric = integer2numeric,
    logical2factor = logical2factor,
    logical2numeric = logical2numeric,
    numeric2factor = numeric2factor,
    numeric2factor_levels = numeric2factor_levels,
    numeric_cut_n = numeric_cut_n,
    numeric_cut_labels = numeric_cut_labels,
    numeric_quant_n = numeric_quant_n,
    numeric_quant_NAonly = numeric_quant_NAonly,
    unique_len2factor = unique_len2factor,
    character2factor = character2factor,
    factorNA2missing = factorNA2missing,
    factorNA2missing_level = factorNA2missing_level,
    factor2integer = factor2integer,
    factor2integer_startat0 = factor2integer_startat0,
    scale = scale,
    center = center,
    scale_centers = scale_centers,
    scale_coefficients = scale_coefficients,
    remove_constants = remove_constants,
    remove_constants_skip_missing = remove_constants_skip_missing,
    remove_features = remove_features,
    remove_duplicates = remove_duplicates,
    one_hot = one_hot,
    one_hot_levels = one_hot_levels,
    add_date_features = add_date_features,
    date_features = date_features,
    add_holidays = add_holidays,
    exclude = exclude
  )
} # /setup_Preprocessor

# Note:
# data_dependent_props <- c(
#   "scale_centers", # Named vector with feature scaling centers.
#   "scale_coefficients", # Named vector with feature scaling coefficients.
#   "one_hot_levels", # Named list of the form "feature_name" = "levels".
#   "remove_features" # Character vector of feature names to remove.
# )

# %% Preprocessor ----
#' @title Preprocessor
#'
#' @description
#' Class to hold output of preprocessing values after applying `PreprocessorConfig` to
#' training dataset, so that the same preprocessing can be applied to validation and test
#' datasets.
#'
#' @field config `PreprocessorConfig` object.
#' @field preprocessed Data frame or list: Preprocessed data. If a single data.frame is passed to
#' `preprocess`, this will be a data.frame. If additional data sets are passed to the
#' `dat_validation` and/or `dat_test` arguments, this will be a named list.
#' @field values List: Data-dependent preprocessing values to be used for validation and test set
#' preprocessing.
#'
#' @author EDG
#' @noRd
Preprocessor <- new_class(
  name = "Preprocessor",
  properties = list(
    config = PreprocessorConfig,
    preprocessed = class_data.frame | class_list,
    values = class_list
  ),
  constructor = function(
    config,
    preprocessed,
    scale_centers = NULL,
    scale_coefficients = NULL,
    one_hot_levels = NULL,
    remove_features = NULL
  ) {
    new_object(
      S7_object(),
      config = config,
      preprocessed = preprocessed,
      values = list(
        scale_centers = scale_centers,
        scale_coefficients = scale_coefficients,
        one_hot_levels = one_hot_levels,
        remove_features = remove_features
      )
    )
  }
) # /Preprocessor


# %% repr.Preprocessor ----
method(repr, Preprocessor) <- function(
  x,
  pad = 0L,
  print_df = FALSE,
  output_type = NULL
) {
  output_type <- get_output_type(output_type)
  paste0(
    repr_S7name("Preprocessor", pad = pad, output_type = output_type),
    repr_ls(props(x), pad = pad, print_df = print_df)
  )
} # /rtemis::repr.Preprocessor


# %% print.Preprocessor ----
method(print, Preprocessor) <- function(x, pad = 0L, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.Preprocessor


# %% names.Preprocessor ----
method(names, Preprocessor) <- function(x) {
  names(props(x))
}


# %% `$`.Preprocessor ----
# Make props `$`-accessible
method(`$`, Preprocessor) <- function(x, name) {
  props(x)[[name]]
}


# %% `.DollarNames`.Preprocessor ----
# DollarSign tab-complete property names
method(`.DollarNames`, Preprocessor) <- function(x, pattern = "") {
  all_names <- names(props(x))
  grep(pattern, all_names, value = TRUE)
}


# %% `[`.Preprocessor ----
# Make props `[`-accessible
method(`[`, Preprocessor) <- function(x, name) {
  props(x)[[name]]
}


# %% `[[`.Preprocessor ----
# Make props `[[`-accessible
method(`[[`, Preprocessor) <- function(x, name) {
  props(x)[[name]]
}


# %% preprocessed.Preprocessor ----
method(preprocessed, Preprocessor) <- function(x) {
  x@preprocessed
}
