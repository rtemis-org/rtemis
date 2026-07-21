# S7_Preprocessor.R
# ::rtemis::
# 2025- EDG rtemis.org

# References
# https://github.com/RConsortium/S7/
# https://rconsortium.github.io/S7

# %% PreprocessorConfig ----
#' PreprocessorConfig
#'
#' @description
#' PreprocessorConfig class.
#'
#' @author EDG
#' @noRd
PreprocessorConfig <- new_class(
  name = "PreprocessorConfig",
  package = "rtemis",
  properties = list(
    complete_cases = prop_boolean(
      FALSE,
      description = "Retain only complete cases."
    ),
    remove_features_thres = prop_float(
      NULL,
      exclusive_min = 0,
      max = 1,
      nullable = TRUE,
      description = "Remove features missing in >= this fraction of cases."
    ),
    remove_cases_thres = prop_float(
      NULL,
      exclusive_min = 0,
      max = 1,
      nullable = TRUE,
      description = "Remove cases missing >= this fraction of features."
    ),
    missingness = prop_boolean(
      FALSE,
      description = "Add a boolean missingness indicator per feature with NAs."
    ),
    impute = prop_boolean(FALSE, description = "Impute missing values."),
    impute_type = prop_string(
      "missRanger",
      enum = c("missRanger", "micePMM", "meanMode"),
      description = "Imputation method."
    ),
    # A named parameter list; not JSON-scalar, injected into the schema via
    # `.preprocessor_schema_extra`.
    impute_missRanger_params = new_property(class_list, default = list()),
    impute_discrete = prop_string(
      "get_mode",
      description = "Function name to impute discrete features."
    ),
    impute_continuous = prop_string(
      "mean",
      description = "Function name to impute continuous features."
    ),
    integer2factor = prop_boolean(
      FALSE,
      description = "Convert integers to factors."
    ),
    integer2numeric = prop_boolean(
      FALSE,
      description = "Convert integers to numeric."
    ),
    logical2factor = prop_boolean(
      FALSE,
      description = "Convert logicals to factors."
    ),
    logical2numeric = prop_boolean(
      FALSE,
      description = "Convert logicals to numeric."
    ),
    numeric2factor = prop_boolean(
      FALSE,
      description = "Convert numeric to factors."
    ),
    numeric2factor_levels = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Factor levels for numeric2factor."
    ),
    numeric_cut_n = prop_integer(
      0L,
      min = 0L,
      description = "Cut numeric features into this many bins (0 = off)."
    ),
    numeric_cut_labels = prop_boolean(
      FALSE,
      description = "Use labels for numeric_cut bins."
    ),
    numeric_quant_n = prop_integer(
      0L,
      min = 0L,
      description = "Cut numeric features into this many quantile bins (0 = off)."
    ),
    numeric_quant_NAonly = prop_boolean(
      FALSE,
      description = "Quantile-cut only features with NAs."
    ),
    unique_len2factor = prop_integer(
      0L,
      min = 0L,
      description = "Convert features with <= this many unique values to factors (0 = off)."
    ),
    character2factor = prop_boolean(
      FALSE,
      description = "Convert character features to factors."
    ),
    factorNA2missing = prop_boolean(
      FALSE,
      description = "Convert factor NAs to a 'missing' level."
    ),
    factorNA2missing_level = prop_string(
      "missing",
      description = "Level name for factorNA2missing."
    ),
    factor2integer = prop_boolean(
      FALSE,
      description = "Convert factors to integers."
    ),
    factor2integer_startat0 = prop_boolean(
      TRUE,
      description = "factor2integer starts at 0."
    ),
    scale = prop_boolean(FALSE, description = "Scale features."),
    center = prop_boolean(FALSE, description = "Center features."),
    # Data-dependent (learned during preprocess); injected via
    # `.preprocessor_schema_extra`.
    scale_centers = NULL | class_numeric,
    scale_coefficients = NULL | class_numeric,
    remove_constants = prop_boolean(
      FALSE,
      description = "Remove constant features."
    ),
    remove_constants_skip_missing = prop_boolean(
      TRUE,
      description = "Ignore missing values when detecting constants."
    ),
    remove_duplicates = prop_boolean(
      FALSE,
      description = "Remove duplicate cases."
    ),
    remove_features = prop_string(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Names of features to remove."
    ),
    one_hot = prop_boolean(FALSE, description = "One-hot encode factors."),
    # Data-dependent (learned during preprocess).
    one_hot_levels = NULL | class_list,
    add_date_features = prop_boolean(
      FALSE,
      description = "Add date-derived features."
    ),
    date_features = prop_string(
      c("weekday", "month", "year"),
      enum = c("weekday", "month", "year"),
      vector = TRUE,
      description = "Date features to add."
    ),
    add_holidays = prop_boolean(
      FALSE,
      description = "Add a holiday indicator feature."
    ),
    exclude = prop_integer(
      NULL,
      nullable = TRUE,
      vector = TRUE,
      description = "Column indices to exclude from preprocessing."
    )
  )
) # /PreprocessorConfig


# %% .preprocessor_schema_extra ----
# Schema fragments for PreprocessorConfig properties whose R types are not
# expressible via the prop_* factories (a params object and the
# data-dependent learned values). Merged into the generated schema so it
# matches the shape consumed by the CLI. See generate_schemas.R.
.preprocessor_schema_extra <- list(
  properties = list(
    impute_missRanger_params = list(
      type = "object",
      description = "Parameters passed to missRanger (e.g. pmm.k, maxiter, num.trees)."
    ),
    scale_centers = list(
      type = I(c("object", "null")),
      `$comment` = "Data-dependent: learned during preprocess(); per-feature scaling centers."
    ),
    scale_coefficients = list(
      type = I(c("object", "null")),
      `$comment` = "Data-dependent: learned during preprocess(); per-feature scaling coefficients."
    ),
    one_hot_levels = list(
      type = I(c("object", "null")),
      `$comment` = "Data-dependent: learned during preprocess(); per-feature one-hot levels."
    )
  )
)


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
#' @param impute_type Character \{"missRanger", "micePMM", "meanMode"\}: Package to use for
#'   imputation.
#' @param impute_missRanger_params Named list with elements "pmm.k",
#'   "maxiter", and "num.trees", which are passed to `missRanger::missRanger`. `pmm.k`
#'   greater than 0 results in predictive mean matching. Reduce `num.trees` for
#'   faster imputation especially in large datasets. Set `pmm.k = 0` to
#'   disable predictive mean matching.
#' @param impute_discrete Character: Name of function that returns single value: How to impute
#'   discrete variables for `impute_type = "meanMode"`.
#' @param impute_continuous Character: Name of function that returns single value: How to impute
#'   continuous variables for `impute_type = "meanMode"`.
#' @param integer2factor Logical: If TRUE, convert all integers to factors. This includes
#'   `bit64::integer64` columns.
#' @param integer2numeric Logical: If TRUE, convert all integers to numeric
#'   (will only work if `integer2factor = FALSE`). This includes
#'   `bit64::integer64` columns.
#' @param logical2factor Logical: If TRUE, convert all logical variables to
#'   factors.
#' @param logical2numeric Logical: If TRUE, convert all logical variables to
#'   numeric.
#' @param numeric2factor Logical: If TRUE, convert all numeric variables to
#'   factors.
#' @param numeric2factor_levels Character vector: Optional - will be passed to
#'   `levels` arg of `factor()` if `numeric2factor = TRUE`. For advanced/
#'   specific use cases; need to know unique values of numeric vector(s) and given all
#'   numeric vars have same unique values.
#' @param numeric_cut_n Integer: If > 0, convert all numeric variables to factors by
#'   binning using `base::cut` with `breaks` equal to this number.
#' @param numeric_cut_labels Logical: The `labels` argument of [base::cut].
#' @param numeric_quant_n Integer: If > 0, convert all numeric variables to factors by
#'   binning using `base::cut` with `breaks` equal to this number of quantiles.
#'   produced using `stats::quantile`.
#' @param numeric_quant_NAonly Logical: If TRUE, only bin numeric variables with
#'   missing values.
#' @param unique_len2factor Integer (>=2): Convert all variables with less
#'   than or equal to this number of unique values to factors.
#'   For example, if binary variables are encoded with 1, 2, you could use
#'   `unique_len2factor = 2` to convert them to factors.
#' @param character2factor Logical: If TRUE, convert all character variables to
#'   factors.
#' @param factorNA2missing Logical: If TRUE, make NA values in factors be of
#'   level `factorNA2missing_level`. In many cases this is the preferred way
#'   to handle missing data in categorical variables. Note that since this step
#'   is performed before imputation, you can use this option to handle missing
#'   data in categorical variables and impute numeric variables in the same
#'   `preprocess` call.
#' @param factorNA2missing_level Character: Name of level if
#'   `factorNA2missing = TRUE`.
#' @param factor2integer Logical: If TRUE, convert all factors to integers.
#' @param factor2integer_startat0 Logical: If TRUE, start integer coding at 0.
#' @param scale Logical: If TRUE, scale columns of `x`.
#' @param center Logical: If TRUE, center columns of `x`. If unset, follows `scale`.
#' @param scale_centers Named vector: Centering values for each feature.
#' @param scale_coefficients Named vector: Scaling values for each feature.
#' @param remove_constants Logical: If TRUE, remove constant columns.
#' @param remove_constants_skip_missing Logical: If TRUE, skip missing values, before
#'   checking if feature is constant.
#' @param remove_features Character vector: Features to remove.
#' @param remove_duplicates Logical: If TRUE, remove duplicate cases.
#' @param one_hot Logical: If TRUE, convert all factors using one-hot encoding.
#' @param one_hot_levels List: Named list of the form "feature_name" = "levels". Used when applying
#'   one-hot encoding to validation or test data using `Preprocessor`.
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
#' @examples
#' preproc_config <- setup_Preprocessor(factorNA2missing = TRUE)
#' preproc_config
setup_Preprocessor <- function(
  complete_cases = FALSE,
  remove_features_thres = NULL,
  remove_cases_thres = NULL,
  missingness = FALSE,
  impute = FALSE,
  impute_type = "missRanger",
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
  numeric_cut_n = 0L,
  numeric_cut_labels = FALSE,
  numeric_quant_n = 0L,
  numeric_quant_NAonly = FALSE,
  unique_len2factor = 0L,
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
  # Integer-typed properties; clean friendly numeric input (`exclude` is
  # vector-valued, which `clean_int()` handles elementwise).
  numeric_cut_n <- clean_int(numeric_cut_n)
  numeric_quant_n <- clean_int(numeric_quant_n)
  unique_len2factor <- clean_int(unique_len2factor)
  exclude <- clean_int(exclude)
  # Per-field validation performed by the `prop_*` property validators.
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
#' Preprocessor
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
  package = "rtemis",
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
