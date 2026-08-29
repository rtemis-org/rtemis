# 260_PredictionRegion.R
# ::rtemis::
# 2026- EDG rtemis.org

# References
# https://github.com/RConsortium/S7
# https://rconsortium.github.io/S7

# Description
# `ConformalConfig` and its subclasses configure conformal prediction; they are
# set by `setup_*()` and consumed by `conformal()`. `PredictionRegion` and its
# subclasses hold the result.
#
# Both hierarchies are tagged, and they subclass on different axes: the result
# by the *shape* of the region, the config by *how* it is constructed. An
# interval and a label set share nothing but their provenance, which is why they
# are two classes under an abstract base rather than one class whose half its
# properties are always NULL.

# %% Constants ----
# What the user asks for: the construction, tagged on the config.
CONFORMAL_METHODS <- c("Split", "CVPlus", "CQR")

# What actually ran, as recorded on a result. `setup_CVPlus()` resolves to three
# of these -- jackknife+ is CV+ over leave-one-out folds, and the set-valued
# case over folds is cross-conformal (Vovk, 2015) rather than CV+, which is
# defined for intervals. Asked and resolved are never confused, as for
# `SHAP@estimator`.
CONFORMAL_RESOLVED_METHODS <- c(
  "Split",
  "CVPlus",
  "JackknifePlus",
  "CrossConformal",
  "CQR"
)

# Nonconformity scores. "absolute" is the only one for a regression; the two
# set-valued ones differ in what they buy, not in how well they work -- see
# `setup_SplitConformal()`.
CONFORMAL_SCORES <- c("absolute", "LAC", "APS")

# The two shapes a region takes, decided by the outcome exactly as
# `SUPERVISED_TYPES` is.
PREDICTION_REGION_TYPES <- c("Interval", "Set")


# %% ConformalConfig ----
#' ConformalConfig
#'
#' @description
#' Superclass for conformal prediction configuration.
#'
#' @details
#' `alpha` is declared per subclass rather than here, as `n_resamples` is on
#' `ResamplerConfig`: `serializable_props()` writes `type` plus the leaf's own
#' properties, so a shared property declared on the base would be dropped from
#' every document.
#'
#' @field type Character: Construction used.
#'
#' @author EDG
#' @noRd
ConformalConfig <- new_class(
  name = "ConformalConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    type = class_character
  )
) # /rtemis::ConformalConfig


# %% serializable_props.ConformalConfig ----
# Type-specific settings serialize as siblings of `type`, as for
# `ExplanationConfig`: one flat object per construction, not a nested `config`.
method(serializable_props, ConformalConfig) <- function(x) {
  c(list(type = x@type), config_prop_values(x, ConformalConfig))
} # /rtemis::serializable_props.ConformalConfig


# %% `$`.ConformalConfig ----
# Make S7 properties `$`-accessible
method(`$`, ConformalConfig) <- function(x, name) {
  prop(x, name)
}


# %% `[[`.ConformalConfig ----
# Make S7 properties `[[`-accessible
method(`[[`, ConformalConfig) <- function(x, name) {
  prop(x, name)
}


# %% repr.ConformalConfig ----
#' repr ConformalConfig
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, ConformalConfig) <- function(x, pad = 0L, output_type = NULL) {
  paste0(
    repr_S7name(x, pad = pad, output_type = output_type),
    repr_ls(
      props(x)[-1],
      pad = pad,
      print_class = FALSE,
      output_type = output_type
    )
  )
} # /rtemis::repr.ConformalConfig


# %% print.ConformalConfig ----
#' Print ConformalConfig
#'
#' @param x `ConformalConfig` object.
#' @param pad Integer: Number of spaces to pad the output with.
#' @param output_type Character \{"ansi", "html", "plain"\}: Output type.
#'
#' @author EDG
#' @noRd
method(print, ConformalConfig) <- function(
  x,
  pad = 0L,
  output_type = NULL,
  ...
) {
  cat(repr(x, pad = pad, output_type = output_type))
  invisible(x)
} # /rtemis::print.ConformalConfig


# %% prop_conformal_alpha ----
#' Miscoverage rate
#'
#' Declared once and used by each leaf, which is what keeps three copies of one
#' decision from drifting apart.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_conformal_alpha <- function() {
  prop_float(
    0.1,
    exclusive_min = 0,
    exclusive_max = 1,
    description = "Miscoverage rate. The region covers with probability at least 1 - alpha."
  )
} # /rtemis::prop_conformal_alpha


# %% prop_conformal_score ----
#' Nonconformity score
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_conformal_score <- function() {
  prop_string(
    NULL,
    enum = CONFORMAL_SCORES,
    nullable = TRUE,
    default_on_null = TRUE,
    description = "Nonconformity score. NULL = 'absolute' for a regression, 'APS' for a classification."
  )
} # /rtemis::prop_conformal_score


# %% prop_conformal_seed ----
#' Random seed for the APS tie-break draw
#'
#' Resolved by the `setup_*` rather than at run time, as `ExecutionConfig`
#' resolves its own, so that the value that ran is on the config and therefore
#' in the record. APS is the only score that draws; the property is declared for
#' every construction that can reach APS rather than gated, since which score
#' runs is resolved from the outcome and a gate on an unresolved value would
#' misfire.
#'
#' @return S7 property.
#'
#' @author EDG
#' @keywords internal
#' @noRd
prop_conformal_seed <- function() {
  prop_integer(
    NULL,
    min = 0L,
    nullable = TRUE,
    description = "Random seed for the APS draw. NULL draws one and records it."
  )
} # /rtemis::prop_conformal_seed


# %% resolve_conformal_seed ----
#' Pin the seed the APS draw will use
#'
#' Resolved in the `setup_*` rather than at run time, as `ExecutionConfig`
#' resolves its own and for the same reason: an unseeded region would otherwise
#' be unreproducible, and "all outputs are auditable and reproducible" has to
#' hold on the default path. Drawing from the current stream keeps
#' `set.seed(1); conformal(...)` deterministic.
#'
#' @param seed Optional Integer: Requested seed.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
resolve_conformal_seed <- function(seed) {
  if (is.null(seed)) {
    sample.int(.Machine[["integer.max"]], 1L)
  } else {
    clean_int(seed)
  }
} # /rtemis::resolve_conformal_seed


# %% SplitConformalConfig ----
#' @title SplitConformalConfig
#'
#' @description
#' `ConformalConfig` subclass for split conformal prediction.
#'
#' @author EDG
#' @noRd
SplitConformalConfig <- new_class(
  name = "SplitConformalConfig",
  parent = ConformalConfig,
  properties = list(
    type = prop_algorithm("Split"),
    alpha = prop_conformal_alpha(),
    score = prop_conformal_score(),
    seed = prop_conformal_seed()
  )
) # /rtemis::SplitConformalConfig


# %% CVPlusConfig ----
#' @title CVPlusConfig
#'
#' @description
#' `ConformalConfig` subclass for CV+ and its relatives.
#'
#' @details
#' One leaf rather than one per relative. Jackknife+ is CV+ over leave-one-out
#' folds and cross-conformal is its set-valued counterpart; which of the three
#' runs follows from the object -- its resampler and its outcome -- so making it
#' the class would force a user to name the thing the object already decides.
#' The resolved method is recorded on the result.
#'
#' @author EDG
#' @noRd
CVPlusConfig <- new_class(
  name = "CVPlusConfig",
  parent = ConformalConfig,
  properties = list(
    type = prop_algorithm("CVPlus"),
    alpha = prop_conformal_alpha(),
    score = prop_conformal_score(),
    seed = prop_conformal_seed()
  )
) # /rtemis::CVPlusConfig


# %% CQRConfig ----
#' @title CQRConfig
#'
#' @description
#' `ConformalConfig` subclass for conformalized quantile regression.
#'
#' @details
#' No `score`: CQR's is `max(qlo - y, y - qhi)` and there is nothing to choose.
#' Regression only, quantiles of a factor being undefined.
#'
#' @author EDG
#' @noRd
CQRConfig <- new_class(
  name = "CQRConfig",
  parent = ConformalConfig,
  properties = list(
    type = prop_algorithm("CQR"),
    alpha = prop_conformal_alpha()
  )
) # /rtemis::CQRConfig


# %% desc.ConformalConfig ----
method(desc, ConformalConfig) <- function(x) {
  switch(
    x@type,
    Split = paste0("split conformal at alpha = ", x@alpha),
    CVPlus = paste0("CV+ at alpha = ", x@alpha),
    CQR = paste0("conformalized quantile regression at alpha = ", x@alpha),
    x@type
  )
} # /rtemis::desc.ConformalConfig


# %% setup_SplitConformal ----
#' Setup Split Conformal Prediction
#'
#' Configure split conformal prediction for [conformal].
#'
#' @details
#' The reference construction: score every calibration case, take `q` to be the
#' score at rank `ceiling((n + 1) * (1 - alpha))` from the bottom, and return
#' every outcome whose score would not exceed it. Coverage is at least
#' `1 - alpha`, and needs only that the calibration cases and the test case be
#' exchangeable.
#'
#' **`alpha` has a floor set by the calibration count.** A finite region needs
#' `ceiling((n + 1) * (1 - alpha)) <= n`, so `alpha = 0.05` needs at least 19
#' calibration cases and `alpha = 0.01` at least 99. `conformal()` aborts naming
#' the count rather than returning an infinite region.
#'
#' **`score` is the real choice, and only for a classification.** A regression
#' has one score, `abs(y - yhat)`, which is what NULL resolves to.
#'
#' - `"APS"`, the default, accumulates the predicted probabilities from the most
#'   probable class down to the true one. Larger sets, markedly better coverage
#'   on the hard cases -- which is the reason to want a set at all.
#' - `"LAC"` scores `1 - p(y)`. It produces the smallest sets attaining valid
#'   *marginal* coverage, and it buys them by under-covering exactly the cases
#'   where the model is unsure. Choose it when small sets are the point and the
#'   trade is understood.
#'
#' **APS is randomized** (Romano, Sesia and Candes, 2020): the true label's own
#' probability enters its score multiplied by a uniform draw, one per case. That
#' is not a refinement. Without it, every case whose true label ranks last
#' scores exactly 1, so a model erring on more than `alpha` of its cases puts
#' the threshold at 1 and every set holds every label -- valid coverage carrying
#' no information. `seed` keeps the draw reproducible: left NULL, one is drawn
#' and recorded on the returned config, so an unseeded region can still be
#' reproduced from what it reports. `"LAC"` draws nothing and ignores it.
#'
#' @param alpha Numeric (0, 1): Miscoverage rate. The region covers with
#' probability at least `1 - alpha`.
#' @param score Optional Character \{"absolute", "LAC", "APS"\}: Nonconformity
#' score. NULL resolves to "absolute" for a regression and "APS" for a
#' classification.
#' @param seed Optional Integer [0, Inf): Random seed for the APS draw. NULL
#' draws one and records it.
#'
#' @return `SplitConformalConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_SplitConformal()
#' setup_SplitConformal(alpha = 0.05)
#' setup_SplitConformal(score = "LAC")
setup_SplitConformal <- function(alpha = 0.1, score = NULL, seed = NULL) {
  # Before the seed is resolved: an unseeded call draws one, and a record
  # comparing that against the class default (NULL) would call it the caller's.
  origins <- supplied_origins()
  out <- SplitConformalConfig(
    alpha = alpha,
    score = score,
    seed = resolve_conformal_seed(seed)
  )
  config_origins(out) <- origins
  out
} # /rtemis::setup_SplitConformal


# %% setup_CVPlus ----
#' Setup CV+ Conformal Prediction
#'
#' Configure CV+, jackknife+ and cross-conformal prediction for [conformal]
#' over a model trained with outer resampling.
#'
#' @details
#' CV+ (Barber, Candes, Ramdas and Tibshirani, 2021) spends no data on
#' calibration: every case is out-of-fold exactly once, so every case both
#' trains and calibrates. Over a `SupervisedRes` the expensive part is already
#' paid -- the per-fold models and their out-of-fold predictions are stored --
#' and what remains is predicting `newdata` under each fold's model and taking
#' two order statistics.
#'
#' **It delivers `1 - 2 * alpha` in the worst case**, not `1 - alpha`. The bound
#' is conservative and observed coverage usually sits near `1 - alpha`, but the
#' region states the guarantee it carries rather than letting a reader assume
#' the tighter one.
#'
#' **The folds must partition the cases**, each case out-of-fold exactly once.
#' `setup_Resampler(type = "KFold")` and `"LOOCV"` do; the subsampling and
#' bootstrap types do not, and `conformal()` refuses them by checking the
#' indices rather than the type name.
#'
#' Which relative runs follows from the object: leave-one-out folds make it
#' jackknife+, and a classification outcome makes it cross-conformal (Vovk,
#' 2015), CV+ being defined for intervals. The result records which.
#'
#' @param alpha Numeric (0, 1): Miscoverage rate. The region covers with
#' probability at least `1 - 2 * alpha` in the worst case.
#' @param score Optional Character \{"absolute", "LAC", "APS"\}: Nonconformity
#' score. NULL resolves to "absolute" for a regression and "APS" for a
#' classification. See [setup_SplitConformal] for the trade between the two
#' set-valued scores, and for why APS draws.
#' @param seed Optional Integer [0, Inf): Random seed for the APS draw. NULL
#' draws one and records it.
#'
#' @return `CVPlusConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_CVPlus()
#' setup_CVPlus(alpha = 0.2)
setup_CVPlus <- function(alpha = 0.1, score = NULL, seed = NULL) {
  # Before the seed is resolved: an unseeded call draws one, and a record
  # comparing that against the class default (NULL) would call it the caller's.
  origins <- supplied_origins()
  out <- CVPlusConfig(
    alpha = alpha,
    score = score,
    seed = resolve_conformal_seed(seed)
  )
  config_origins(out) <- origins
  out
} # /rtemis::setup_CVPlus


# %% setup_CQR ----
#' Setup Conformalized Quantile Regression
#'
#' Configure conformalized quantile regression for [conformal].
#'
#' @details
#' Split conformal adds and subtracts one number, so every case gets the same
#' width whether the model is confident or not. CQR (Romano, Patterson and
#' Candes, 2019) starts from the model's own `alpha/2` and `1 - alpha/2`
#' quantiles and conformalizes the pair with the score
#' `max(qlo - y, y - qhi)`, which widens or narrows both ends by one calibrated
#' amount. Adaptive width, same finite-sample guarantee.
#'
#' **It needs a backend that answers a quantile query from the model already
#' fitted.** Today that is Ranger trained with `setup_Ranger(quantreg = TRUE)`:
#' a quantile regression forest keeps the training outcomes at its terminal
#' nodes, so one fit answers every level. A gradient booster on the `quantile`
#' objective targets the single level it was trained for, so a pair of bounds
#' would be a pair of models -- a training configuration, not something
#' `conformal()` can do to a model it is handed.
#'
#' **It always needs `calibration` data.** A `Supervised` stores its test
#' predictions but not its test features, and CQR must query the model for
#' quantiles at the calibration cases, so the stored-split default does not
#' apply here.
#'
#' @param alpha Numeric (0, 1): Miscoverage rate. Also sets the quantile levels
#' queried, `alpha/2` and `1 - alpha/2`.
#'
#' @return `CQRConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_CQR()
#' setup_CQR(alpha = 0.05)
setup_CQR <- function(alpha = 0.1) {
  CQRConfig(alpha = alpha)
} # /rtemis::setup_CQR


# %% PredictionRegion ----
#' PredictionRegion
#'
#' @description
#' Superclass for conformal prediction regions.
#'
#' @details
#' What every region must state regardless of its shape: which model produced
#' it, which construction ran, at what score and calibrated quantile, on how
#' many cases, and against which data. The last is not bookkeeping -- a region
#' is meaningless without knowing what calibrated it, two regions calibrated on
#' different data are not comparable, and nothing in the numbers says so.
#'
#' `alpha` is deliberately absent: it is on `@config`, and a second copy is a
#' second place for it to disagree. `@coverage` derives the guarantee from
#' `@config@alpha` and `@method` instead of storing it.
#'
#' @author EDG
#' @noRd
PredictionRegion <- new_class(
  name = "PredictionRegion",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    type = class_character,
    algorithm = class_character,
    config = ConformalConfig,
    method = prop_string(
      NULL,
      enum = CONFORMAL_RESOLVED_METHODS,
      nullable = TRUE,
      description = "Construction that ran, resolved."
    ),
    score = prop_string(
      NULL,
      enum = CONFORMAL_SCORES,
      nullable = TRUE,
      description = "Nonconformity score used, resolved."
    ),
    # The calibrated threshold. One number for a split construction and NULL for
    # the fold constructions, which compare a rank against every out-of-fold
    # score rather than against a single quantile of them.
    q = NULL | class_numeric,
    n_calibration = class_integer,
    # Identity of the cases bounded, and of the data that calibrated the bound.
    # Both, for the reason `Explanation` carries both of its own.
    data_fingerprint = NULL | DataFingerprint,
    calibration_fingerprint = NULL | DataFingerprint,
    # The guarantee the construction delivers: `1 - alpha` where the coverage
    # bound is exact, `1 - 2 * alpha` for the fold constructions, whose bound is
    # the conservative one. Derived rather than stored, so it cannot contradict
    # the config it is read from.
    coverage = new_property(
      getter = function(self) {
        alpha <- self@config@alpha
        conservative <- isTRUE(
          self@method %in% c("CVPlus", "JackknifePlus", "CrossConformal")
        )
        if (conservative) 1 - 2 * alpha else 1 - alpha
      }
    )
  )
) # /rtemis::PredictionRegion


# %% PredictionInterval ----
#' PredictionInterval
#'
#' @description
#' `PredictionRegion` subclass holding a lower and upper bound per case.
#'
#' @details
#' `@predicted` is the point prediction the interval accompanies. It is the
#' model's own prediction for a split construction and the fold average for a
#' fold construction, and in neither case is the interval required to be
#' symmetric around it -- CV+ takes two order statistics over an ensemble, and
#' CQR two conformalized quantiles, and neither is centered by construction.
#'
#' @author EDG
#' @noRd
PredictionInterval <- new_class(
  name = "PredictionInterval",
  parent = PredictionRegion,
  properties = list(
    type = prop_algorithm("Interval"),
    predicted = class_numeric,
    lower = class_numeric,
    upper = class_numeric,
    width = new_property(
      getter = function(self) {
        self@upper - self@lower
      }
    )
  ),
  validator = function(self) {
    # `@coverage` reads it to say which guarantee the region carries, so a
    # region that never named its construction could not describe itself.
    if (is.null(self@method)) {
      return("@method must name the construction that ran.")
    }
    n <- length(self@predicted)
    if (length(self@lower) != n || length(self@upper) != n) {
      return("@lower and @upper must hold one value per case of @predicted.")
    }
    if (n == 0L) {
      return("@predicted must hold at least one case.")
    }
    if (any(self@upper < self@lower)) {
      return("@upper must be at least @lower for every case.")
    }
    NULL
  }
) # /rtemis::PredictionInterval


# %% PredictionSet ----
#' PredictionSet
#'
#' @description
#' `PredictionRegion` subclass holding a set of labels per case.
#'
#' @details
#' A set may be empty -- no label is plausible at this level, which is
#' information rather than failure and is never silently widened to the top-1
#' label -- and may hold every class. `@predicted_prob` is the full probability
#' matrix the sets were cut from, one column per class including the binary
#' case, where `Classification` itself stores only the positive class's column.
#'
#' @author EDG
#' @noRd
PredictionSet <- new_class(
  name = "PredictionSet",
  parent = PredictionRegion,
  properties = list(
    type = prop_algorithm("Set"),
    sets = class_list,
    predicted_prob = class_matrix,
    classes = class_character,
    set_size = new_property(
      getter = function(self) {
        lengths(self@sets)
      }
    )
  ),
  validator = function(self) {
    if (is.null(self@method)) {
      return("@method must name the construction that ran.")
    }
    if (length(self@sets) == 0L) {
      return("@sets must hold at least one case.")
    }
    if (nrow(self@predicted_prob) != length(self@sets)) {
      return("@predicted_prob must hold one row per case of @sets.")
    }
    if (ncol(self@predicted_prob) != length(self@classes)) {
      return("@predicted_prob must hold one column per class of @classes.")
    }
    if (!all(vapply(self@sets, is.character, logical(1L)))) {
      return("@sets entries must be character vectors.")
    }
    if (!all(unlist(self@sets) %in% self@classes)) {
      return("@sets may only hold labels named in @classes.")
    }
    NULL
  }
) # /rtemis::PredictionSet


# %% region_n_cases ----
#' Number of cases a region bounds
#'
#' @param x `PredictionRegion` object.
#'
#' @return Integer.
#'
#' @author EDG
#' @keywords internal
#' @noRd
region_n_cases <- function(x) {
  if (S7_inherits(x, PredictionInterval)) {
    length(x@predicted)
  } else {
    length(x@sets)
  }
} # /rtemis::region_n_cases


# %% desc_conformal_method ----
#' Name a resolved construction as a reader would say it
#'
#' @param method Character: `PredictionRegion@method`.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
desc_conformal_method <- function(method) {
  switch(
    method,
    Split = "split conformal",
    CVPlus = "CV+",
    JackknifePlus = "jackknife+",
    CrossConformal = "cross-conformal",
    CQR = "CQR",
    method
  )
} # /rtemis::desc_conformal_method


# %% `$`.PredictionRegion ----
# Make PredictionRegion props `$`-accessible
method(`$`, PredictionRegion) <- function(x, name) {
  prop(x, name)
}


# %% `.DollarNames`.PredictionRegion ----
method(`.DollarNames`, PredictionRegion) <- function(x, pattern = "") {
  grep(pattern, names(props(x)), value = TRUE)
}


# %% `[[`.PredictionRegion ----
method(`[[`, PredictionRegion) <- function(x, name) {
  prop(x, name)
}


# %% repr_region_guarantee ----
#' State the requested level and the guarantee it buys
#'
#' Both, always. Printing the guarantee alone shows "80%" to a user who asked
#' for `alpha = 0.1` and reads as a defect; printing the requested level alone
#' would claim 90% coverage that a fold construction does not promise in the
#' worst case. The two agree wherever the bound is exact, and the line says so
#' by naming one number instead of two.
#'
#' @param x `PredictionRegion` object.
#' @param output_type Character \{"ansi", "html", "plain"\}: Output type.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
repr_region_guarantee <- function(x, output_type = NULL) {
  pct <- function(p) paste0(format(100 * p, trim = TRUE), "%")
  nominal <- 1 - x@config@alpha
  paste0(
    fmt(
      pct(x@coverage),
      col = highlight_col,
      bold = TRUE,
      output_type = output_type
    ),
    if (!isTRUE(all.equal(x@coverage, nominal))) {
      paste0(" guaranteed (", pct(nominal), " nominal)")
    } else {
      " guaranteed"
    }
  )
} # /rtemis::repr_region_guarantee


# %% repr.PredictionInterval ----
#' repr `PredictionInterval`
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, PredictionInterval) <- function(x, pad = 0L, output_type = NULL) {
  n <- region_n_cases(x)
  paste0(
    repr_S7name("PredictionInterval", pad = pad, output_type = output_type),
    strrep(" ", pad),
    desc_conformal_method(x@method),
    " intervals for ",
    fmt(n, col = highlight_col, bold = TRUE, output_type = output_type),
    ngettext(n, " case", " cases"),
    ", ",
    repr_region_guarantee(x, output_type = output_type),
    ", mean width ",
    fmt(
      ddSci(mean(x@width)),
      col = highlight_col,
      bold = TRUE,
      output_type = output_type
    ),
    "\n"
  )
} # /rtemis::repr.PredictionInterval


# %% repr.PredictionSet ----
#' repr `PredictionSet`
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(repr, PredictionSet) <- function(x, pad = 0L, output_type = NULL) {
  n <- region_n_cases(x)
  paste0(
    repr_S7name("PredictionSet", pad = pad, output_type = output_type),
    strrep(" ", pad),
    desc_conformal_method(x@method),
    " ",
    x@score,
    " sets for ",
    fmt(n, col = highlight_col, bold = TRUE, output_type = output_type),
    ngettext(n, " case", " cases"),
    " over ",
    fmt(
      length(x@classes),
      col = highlight_col,
      bold = TRUE,
      output_type = output_type
    ),
    " classes, ",
    repr_region_guarantee(x, output_type = output_type),
    ", mean size ",
    fmt(
      ddSci(mean(x@set_size)),
      col = highlight_col,
      bold = TRUE,
      output_type = output_type
    ),
    "\n"
  )
} # /rtemis::repr.PredictionSet


# %% print.PredictionRegion ----
#' Print `PredictionRegion`
#'
#' @param x `PredictionRegion` object.
#' @param output_type Character \{"ansi", "html", "plain"\}: Output type.
#'
#' @author EDG
#' @noRd
method(print, PredictionRegion) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type))
  invisible(x)
} # /rtemis::print.PredictionRegion


# %% desc.PredictionInterval ----
method(desc, PredictionInterval) <- function(x) {
  paste0(
    format(100 * x@coverage, trim = TRUE),
    "% ",
    desc_conformal_method(x@method),
    " intervals for ",
    region_n_cases(x),
    ngettext(region_n_cases(x), " case", " cases"),
    ", calibrated on ",
    x@n_calibration
  )
} # /rtemis::desc.PredictionInterval


# %% desc.PredictionSet ----
method(desc, PredictionSet) <- function(x) {
  paste0(
    format(100 * x@coverage, trim = TRUE),
    "% ",
    desc_conformal_method(x@method),
    " ",
    x@score,
    " sets for ",
    region_n_cases(x),
    ngettext(region_n_cases(x), " case", " cases"),
    ", calibrated on ",
    x@n_calibration
  )
} # /rtemis::desc.PredictionSet


# %% to_json.PredictionInterval ----
#' to_json `PredictionInterval`
#'
#' Publishes what the region *is* -- construction, score, guarantee, calibration
#' count and identities -- and the summary of its widths, but not the per-case
#' bounds, which are bulk data. `SHAP` draws the same line for its contribution
#' matrices, and `Supervised` for its prediction vectors.
#'
#' @param x `PredictionInterval` object.
#'
#' @return Named list. Pass to `jsonlite::toJSON(auto_unbox = TRUE)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_json, PredictionInterval) <- function(x, ...) {
  out <- c(
    to_json_region_common(x),
    list(
      mean_width = mean(x@width),
      median_width = stats::median(x@width),
      min_width = min(x@width),
      max_width = max(x@width)
    )
  )
  Filter(Negate(is.null), out)
} # /rtemis::to_json.PredictionInterval


# %% to_json.PredictionSet ----
#' to_json `PredictionSet`
#'
#' @param x `PredictionSet` object.
#'
#' @return Named list. Pass to `jsonlite::toJSON(auto_unbox = TRUE)`.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_json, PredictionSet) <- function(x, ...) {
  sizes <- x@set_size
  out <- c(
    to_json_region_common(x),
    list(
      classes = x@classes,
      mean_set_size = mean(sizes),
      singleton_rate = mean(sizes == 1L),
      empty_rate = mean(sizes == 0L)
    )
  )
  Filter(Negate(is.null), out)
} # /rtemis::to_json.PredictionSet


# %% to_json_region_common ----
#' The wire fields both region shapes publish
#'
#' @param x `PredictionRegion` object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
to_json_region_common <- function(x) {
  list(
    type = x@type,
    algorithm = x@algorithm,
    description = desc(x),
    method = x@method,
    score = x@score,
    alpha = x@config@alpha,
    coverage = x@coverage,
    q = x@q,
    n_cases = region_n_cases(x),
    n_calibration = x@n_calibration,
    config = .to_json_value(x@config),
    data_fingerprint = .to_json_value(x@data_fingerprint),
    calibration_fingerprint = .to_json_value(x@calibration_fingerprint)
  )
} # /rtemis::to_json_region_common
