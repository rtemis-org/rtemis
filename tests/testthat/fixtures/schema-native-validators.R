# Frozen behavioral oracle for the reviewed native validators.
# Changes require an explicit contract decision and independent boundary cases.
linad_feature_role_rule <-
  function(self) {
    if (is.null(self@linear_features) || is.null(self@global_features)) {
      return(NULL)
    }
    outside <- setdiff(self@global_features, self@linear_features)
    if (length(outside) == 0L) {
      return(NULL)
    }
    paste0(
      "@global_features must be a subset of @linear_features: a shared slope is still a slope. Not in @linear_features: ",
      paste(outside, collapse = ", "),
      "."
    )
  }

check_lightgbm_sampling <-
  function(self) {
    if (!any(candidate_values(self@data_sample_strategy) == "goss")) {
      return(NULL)
    }
    fraction <- candidate_values(self@bagging_fraction)
    if (is.numeric(fraction) && !any(fraction >= 1)) {
      return(paste0(
        "@data_sample_strategy \"goss\" cannot be combined with bagging, and no ",
        "value of @bagging_fraction avoids it: ",
        paste(fraction, collapse = ", "),
        ". GOSS samples by gradient instead, so leave @bagging_fraction at 1."
      ))
    }
    top <- candidate_values(self@top_rate)
    other <- candidate_values(self@other_rate)
    if (
      is.numeric(top) &&
        is.numeric(other) &&
        length(top) > 0L &&
        length(other) > 0L &&
        min(top) + min(other) > 1
    ) {
      return(paste0(
        "@top_rate + @other_rate must not exceed 1: together they are a share of ",
        "the training cases, and the smallest they can sum to here is ",
        min(top) + min(other),
        "."
      ))
    }
    NULL
  }

.legacy_validators <- list(
  MARSHyperparameters = function(self) {
    if (identical(self@pmethod, "cv") && self@nfold < 2L) {
      "@pmethod \"cv\" selects the number of terms by cross-validation, so @nfold must be at least 2."
    }
  },
  HALHyperparameters = function(self) {
    if (!is.null(self@num_knots)) {
      if (is_candidates(self@max_degree)) {
        return(
          "@num_knots cannot be combined with a search over @max_degree: it needs one value per degree, so leave it NULL while tuning @max_degree."
        )
      }
      if (length(self@num_knots) != self@max_degree) {
        return(paste0(
          "@num_knots must have one value per interaction degree: expected length ",
          self@max_degree,
          ", got ",
          length(self@num_knots),
          "."
        ))
      }
      if (is.unsorted(rev(self@num_knots))) {
        return(
          "@num_knots must be non-increasing across degrees: higher-order interactions cannot use more knots than lower-order ones."
        )
      }
    }
    check_applies_when(self)
  },
  LightGBMHyperparameters = function(self) {
    sampling <- check_lightgbm_sampling(self)
    if (!is.null(sampling)) {
      return(sampling)
    }
    check_applies_when(self)
  },
  LightRuleFitHyperparameters = function(self) {
    if (any(self@ifw) && (any(self@ifw_lightgbm) || any(self@ifw_glmnet))) {
      return("@ifw cannot be combined with @ifw_lightgbm or @ifw_glmnet.")
    }
    sampling <- check_lightgbm_sampling(self)
    if (!is.null(sampling)) {
      return(sampling)
    }
    check_applies_when(self)
  },
  BARTHyperparameters = function(self) {
    if (any(self@num_gfr > 0L) && any(self@num_chains > self@num_gfr)) {
      "@num_chains cannot exceed @num_gfr when @num_gfr is greater than 0."
    }
  },
  LINADHyperparameters = function(self) {
    c(check_applies_when(self), linad_feature_role_rule(self))
  },
  LINADForestHyperparameters = function(self) {
    c(check_applies_when(self), linad_feature_role_rule(self))
  },
  GridSearchConfig = function(self) {
    if (self@search_type == "exhaustive" && !is.null(self@randomize_p)) {
      "@randomize_p must not be set when @search_type is 'exhaustive'."
    } else if (self@search_type == "randomized" && is.null(self@randomize_p)) {
      "@randomize_p must be set when @search_type is 'randomized'."
    }
  },
  MetaLearnerHyperparameters = function(self) {
    nms <- names(self@base_learners)
    if (!identical(make.names(nms), nms)) {
      return(paste0(
        "@base_learners names must be syntactically valid; these are not: ",
        paste(nms[make.names(nms) != nms], collapse = ", "),
        "."
      ))
    }
    NULL
  },
  DataRef = function(self) {
    if (!nzchar(self@path)) {
      return("@path must name a file.")
    }
    if (!nzchar(self@hash)) {
      return("@hash must be the digest of that file's bytes.")
    }
    NULL
  },
  DataFingerprint = function(self) {
    if (!nzchar(self@hash)) {
      return("@hash must not be empty.")
    }
    if (!nzchar(self@encoding)) {
      return("@encoding must not be empty.")
    }
    if (!nzchar(self@language)) {
      return("@language must not be empty.")
    }
    if (!nzchar(self@data_structure)) {
      return("@data_structure must not be empty.")
    }
    if (self@method == "file" && is.null(self@source)) {
      return("@source must be set when @method is 'file'.")
    }
    if (
      !is.null(self@column_names) && length(self@column_names) != self@n_cols
    ) {
      return("@column_names must have one entry per column (@n_cols).")
    }
    NULL
  }
)
