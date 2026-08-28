# test_Preprocessor.R
# ::rtemis::
# 2025- EDG rtemis.org

# library(testthat)

# PreprocessorConfig ----
prp <- setup_Preprocessor()
prp
testthat::test_that("setup_Preprocessor() succeeds", {
  expect_s7_class(setup_Preprocessor(), PreprocessorConfig)
})

prp <- setup_Preprocessor(
  remove_constants = TRUE,
  remove_duplicates = TRUE
)
testthat::test_that("setup_Preprocessor() succeeds", {
  expect_s7_class(prp, PreprocessorConfig)
})

testthat::test_that("integer-typed args accept friendly numeric input", {
  # Every integer-typed property is cleaned, so users need not write `L`.
  # `exclude` is vector-valued, and `c(1, 2)` is a double vector in R.
  expect_identical(setup_Preprocessor(exclude = c(1, 2))@exclude, c(1L, 2L))
  expect_identical(setup_Preprocessor(exclude = 3)@exclude, 3L)
  expect_identical(setup_Preprocessor(numeric_cut_n = 4)@numeric_cut_n, 4L)
  expect_null(setup_Preprocessor()@exclude)
  # Non-integral input is still an error, not a silent truncation.
  expect_error(setup_Preprocessor(exclude = 1.5), class = "rtemis_type_error")
})

# Preprocessor: preprocess(PreprocessorConfig) ----
res <- resample(iris, setup_Resampler(seed = 2025))
iris_train <- iris[res$Fold_1, ]
iris_test <- iris[-res$Fold_1, ]
iris_Pre <- preprocess(
  iris_train,
  setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE)
)
test_that("preprocess(x, PreprocessorConfig) succeeds", {
  expect_s7_class(iris_Pre, Preprocessor)
})
iris_Pre
iris_Pre@preprocessed
iris_Pre@values

iris_test_pre <- apply_preprocessor(iris_Pre, iris_test)
test_that("apply_preprocessor(Preprocessor, new_data) returns preprocessed data", {
  expect_s3_class(iris_test_pre, "data.frame")
})

iris_Pre_too <- preprocess(
  iris_train,
  setup_Preprocessor(remove_duplicates = TRUE, scale = TRUE, center = TRUE),
  dat_test = iris_test
)
test_that("preprocess(x, PreprocessorConfig) succeeds", {
  expect_s7_class(iris_Pre_too, Preprocessor)
})

test_that("preprocess(x, PreprocessorConfig) and apply_preprocessor() give same test set", {
  expect_equal(iris_Pre_too@preprocessed$test, iris_test_pre)
})

# Preprocessor: preprocess(x, ...) with setup_Preprocessor arguments ----
iris_Pre_direct <- preprocess(
  iris_train,
  remove_duplicates = TRUE,
  scale = TRUE,
  center = TRUE
)
test_that("preprocess(x, ...) with direct arguments matches PreprocessorConfig call", {
  expect_s7_class(iris_Pre_direct, Preprocessor)
  expect_equal(iris_Pre_direct@preprocessed, iris_Pre@preprocessed)
})

test_that("preprocess(x) with no preprocessing parameters errors", {
  expect_error(preprocess(iris_train), class = "rtemis_input_error")
})

test_that("preprocess(x, config, ...) with extra setup arguments errors", {
  expect_error(
    preprocess(iris_train, setup_Preprocessor(scale = TRUE), center = TRUE)
  )
})

# impute meanMode ----
x <- iris
# Continuous
x[10:15, 1] <- NA
# Categorical
x[20:25, 5] <- NA
xp <- preprocess(
  x,
  setup_Preprocessor(impute = TRUE, impute_type = "meanMode")
)[["preprocessed"]]

test_that("impute meanMode works", {
  expect_false(anyNA(xp))
})

# Test one_hot ----
n <- 10
x <- rnormmat(n, 5, seed = 2025)
g <- factor(sample(c("A", "B"), n, replace = TRUE))
y <- x[, 3] + x[, 5] + ifelse(g == "A", 2, -1) + rnorm(n)
datr <- data.frame(x, g, y)
datr_onehot <- preprocess(
  datr,
  setup_Preprocessor(one_hot = TRUE)
)[["preprocessed"]]
test_that("one_hot.data.frame works", {
  expect_s3_class(datr_onehot, "data.frame")
})

# The factors deliberately do not lead the frame: a level map subscripted by a
# full-frame column index is correct only when they do.
oh_train <- data.frame(
  a = rnorm(6L),
  g = factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c")),
  h = factor(c("x", "y", "x", "y", "x", "y"))
)

test_that("one_hot encodes training data as it always has", {
  pre <- preprocess(
    oh_train,
    setup_Preprocessor(one_hot = TRUE),
    verbosity = 0L
  )
  out <- preprocessed(pre)
  expect_identical(names(out), c("a", "g_a", "g_b", "g_c", "h_x", "h_y"))
  expect_identical(out[["g_a"]], c(1, 0, 0, 1, 0, 0))
  expect_identical(out[["g_c"]], c(0, 0, 1, 0, 0, 1))
  expect_identical(out[["h_y"]], c(0, 1, 0, 1, 0, 1))

  # The levels are published, one entry per encoded feature.
  expect_identical(
    pre@values[["one_hot_levels"]],
    list(g = c("a", "b", "c"), h = c("x", "y"))
  )
})


test_that("one_hot indexes its level map by feature name", {
  x <- data.frame(
    a = rnorm(3L),
    g = factor(c("a", "b", "a"), levels = c("a", "b")),
    h = factor(c("x", "y", "x"), levels = c("x", "y", "z"))
  )
  out <- one_hot(
    x,
    factor_levels = list(g = c("a", "b"), h = c("x", "y", "z")),
    verbosity = 0L
  )
  expect_identical(names(out), c("a", "g_a", "g_b", "h_x", "h_y", "h_z"))
  expect_identical(out[["g_b"]], c(0, 1, 0))

  # A map carrying a key with no matching column is tolerated: `train()`
  # learns one on data that includes the outcome and applies it to features.
  out_extra <- one_hot(
    x,
    factor_levels = list(
      g = c("a", "b"),
      h = c("x", "y", "z"),
      y = c("neg", "pos")
    ),
    verbosity = 0L
  )
  expect_identical(out_extra, out)
})


test_that("remove_constants leaves a single surviving column a frame", {
  x <- data.frame(k = rep(1, 5L), a = c(1.5, 2.5, 3.5, 4.5, 5.5))
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(remove_constants = TRUE),
    verbosity = 0L
  ))
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), "a")
  expect_identical(out[["a"]], x[["a"]])
})


test_that("remove_features_thres leaves a single surviving column a frame", {
  x <- data.frame(k = c(NA_real_, NA_real_, NA_real_), a = c(1.5, 2.5, 3.5))
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(remove_features_thres = 0.5),
    verbosity = 0L
  ))
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), "a")
  expect_identical(out[["a"]], x[["a"]])
})


# %% The conversion boundary ----
# `preprocess()` runs on a data.table and restores the caller's structure at
# exit. These pin both halves: what goes in comes back out as itself, and the
# caller's own object is never written through -- which a data.table, alone
# among the three, would be by an unguarded `set()`.

pre_fixture <- data.frame(
  num = c(1.5, NA, 3.5, 4.5, 5.5, 3.5),
  int = c(10L, 20L, 30L, NA, 20L, 30L),
  lgl = c(TRUE, FALSE, NA, TRUE, FALSE, NA),
  chr = c("p", "q", "r", NA, "q", "r"),
  fct = factor(c("a", "b", "c", "a", NA, "c"), levels = c("a", "b", "c")),
  konst = rep(7, 6L),
  stringsAsFactors = FALSE
)

pre_full_config <- function() {
  setup_Preprocessor(
    remove_constants = TRUE,
    character2factor = TRUE,
    logical2factor = TRUE,
    factorNA2missing = TRUE,
    missingness = TRUE,
    impute = TRUE,
    impute_type = "meanMode",
    scale = TRUE,
    center = TRUE,
    one_hot = TRUE
  )
}


test_that("preprocess returns the structure it was given", {
  as_frame <- preprocessed(preprocess(
    pre_fixture,
    pre_full_config(),
    verbosity = 0L
  ))
  expect_s3_class(as_frame, "data.frame")
  expect_false(data.table::is.data.table(as_frame))

  as_table <- preprocessed(preprocess(
    data.table::as.data.table(pre_fixture),
    pre_full_config(),
    verbosity = 0L
  ))
  expect_s3_class(as_table, "data.table")

  # One pipeline, three structures: the values cannot depend on which one the
  # caller happened to hold the data in.
  expect_equal(as.data.frame(as_table), as_frame)

  skip_if_not_installed("tibble")
  # A tibble used to come back a data.frame, the one class change the old
  # data.frame-internal pipeline could not undo.
  as_tibble <- preprocessed(preprocess(
    tibble::as_tibble(pre_fixture),
    pre_full_config(),
    verbosity = 0L
  ))
  expect_s3_class(as_tibble, "tbl_df")
  expect_equal(as.data.frame(as_tibble), as_frame)
})


test_that("preprocess leaves the caller's object alone", {
  # A data.table is the one that can actually be written through: `set()` and
  # `:=` bypass copy-on-modify, so the entry copy is what stops
  # `preprocess(dt, ...)` from rewriting the caller's table.
  dt <- data.table::as.data.table(pre_fixture)
  dt_before <- data.table::copy(dt)
  invisible(preprocess(dt, pre_full_config(), verbosity = 0L))
  expect_identical(dt, dt_before)

  df <- pre_fixture
  df_before <- pre_fixture
  invisible(preprocess(df, pre_full_config(), verbosity = 0L))
  expect_identical(df, df_before)

  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(pre_fixture)
  tb_before <- tibble::as_tibble(pre_fixture)
  invisible(preprocess(tb, pre_full_config(), verbosity = 0L))
  expect_identical(tb, tb_before)
})


test_that("preprocess output carries no row names", {
  # Row names carry no data a run record can report: a case identifier that
  # matters belongs in a column. They are dropped for every structure and every
  # configuration, rather than surviving wherever no step rebuilt the frame.
  x <- pre_fixture
  rownames(x) <- paste0("case_", seq_len(nrow(x)))
  expect_identical(
    rownames(preprocessed(preprocess(
      x,
      setup_Preprocessor(scale = TRUE, center = TRUE),
      verbosity = 0L
    ))),
    as.character(seq_len(nrow(x)))
  )
  filtered <- preprocessed(preprocess(
    x,
    setup_Preprocessor(complete_cases = TRUE),
    verbosity = 0L
  ))
  expect_identical(nrow(filtered), sum(complete.cases(x)))
  expect_identical(rownames(filtered), as.character(seq_len(nrow(filtered))))
})


test_that("imputation widens a column rather than truncating into it", {
  # A partial data.table assignment coerces the value into the column's
  # existing type, so a mean imputed into an integer column would land as a
  # truncated integer with a warning. Whole columns are replaced instead.
  x <- data.frame(i = c(1L, 2L, 10L, NA))
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(
      impute = TRUE,
      impute_type = "meanMode",
      impute_discrete = "mean"
    ),
    verbosity = 0L
  ))
  expect_type(out[["i"]], "double")
  expect_equal(out[["i"]], c(1, 2, 10, 13 / 3))
})


test_that("excluded columns skip every step and are appended at the end", {
  x <- data.frame(
    a = c(1.5, 2.5, 3.5, 4.5),
    id = c("w", "x", "y", "z"),
    b = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(exclude = 2L, scale = TRUE, center = TRUE),
    verbosity = 0L
  ))
  # `id` held position 2 on entry and comes back last, untouched.
  expect_identical(names(out), c("a", "b", "id"))
  expect_identical(out[["id"]], x[["id"]])
  expect_equal(mean(out[["a"]]), 0)

  # An excluded column has to be filtered by every case-removing step, whether
  # or not that step removed anything. Both of these used to error: one
  # subscripted the excluded columns with `-integer(0)`, emptying them; the
  # other read a case index that is only created when the data has NAs.
  no_dups <- preprocessed(preprocess(
    x,
    setup_Preprocessor(exclude = 2L, remove_duplicates = TRUE),
    verbosity = 0L
  ))
  expect_identical(no_dups[["id"]], x[["id"]])

  no_nas <- preprocessed(preprocess(
    x,
    setup_Preprocessor(exclude = 2L, remove_cases_thres = 0.5),
    verbosity = 0L
  ))
  expect_identical(no_nas[["id"]], x[["id"]])

  dups <- data.frame(
    a = c(1.5, 1.5, 3.5),
    id = c("w", "x", "y"),
    stringsAsFactors = FALSE
  )
  deduped <- preprocessed(preprocess(
    dups,
    setup_Preprocessor(exclude = 2L, remove_duplicates = TRUE),
    verbosity = 0L
  ))
  expect_identical(deduped[["a"]], c(1.5, 3.5))
  expect_identical(deduped[["id"]], c("w", "y"))
})


test_that("an excluded column that a step would shadow is an error", {
  # `missingness` coins `<feature>_missing`; appending an excluded column of
  # that name over it would drop the indicator without saying so.
  x <- data.frame(
    a = c(1.5, NA, 3.5),
    a_missing = c("p", "q", "r"),
    stringsAsFactors = FALSE
  )
  expect_error(
    preprocess(
      x,
      setup_Preprocessor(exclude = 2L, missingness = TRUE),
      verbosity = 0L
    ),
    class = "rtemis_data_error"
  )
})


test_that("a step that removes every column is an error", {
  x <- data.frame(a = c(1.5, 2.5), b = c(3.5, 4.5))
  expect_error(
    preprocess(
      x,
      setup_Preprocessor(exclude = c(1L, 2L)),
      verbosity = 0L
    ),
    class = "rtemis_input_error"
  )
  expect_error(
    preprocess(
      x,
      setup_Preprocessor(remove_features = c("a", "b")),
      verbosity = 0L
    ),
    class = "rtemis_input_error"
  )
})


test_that("numeric_quant_n bins numeric features into quantiles", {
  x <- data.frame(
    a = c(1, 2, 3, 4, 5, 6, 7, 8),
    b = c(1, 2, NA, 4, 5, 6, 7, 8)
  )
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(numeric_quant_n = 3L),
    verbosity = 0L
  ))
  expect_s3_class(out[["a"]], "factor")
  expect_s3_class(out[["b"]], "factor")

  # `numeric_quant_NAonly` restricts it to the features that have NAs.
  out_na <- preprocessed(preprocess(
    x,
    setup_Preprocessor(numeric_quant_n = 3L, numeric_quant_NAonly = TRUE),
    verbosity = 0L
  ))
  expect_identical(out_na[["a"]], x[["a"]])
  expect_s3_class(out_na[["b"]], "factor")

  # One quantile is one break, which bounds no bin. Rejected where it is
  # supplied, rather than reaching `cut()` as "invalid number of intervals".
  expect_error(
    setup_Preprocessor(numeric_quant_n = 1L),
    class = "rtemis_input_error"
  )
  expect_identical(setup_Preprocessor(numeric_quant_n = 0L)@numeric_quant_n, 0L)
})


test_that("integer2numeric reads the data as it stands", {
  x <- data.frame(i = c(10L, 20L, 30L), a = c(1.5, 2.5, 3.5))
  expect_identical(
    preprocessed(preprocess(
      x,
      setup_Preprocessor(integer2numeric = TRUE),
      verbosity = 0L
    ))[["i"]],
    c(10, 20, 30)
  )
  # `integer2factor` leaves nothing integer behind, so `integer2numeric` has
  # nothing to do -- rather than converting the new factor to its level codes,
  # which replaced the values with 1, 2, 3.
  out_both <- preprocessed(preprocess(
    x,
    setup_Preprocessor(integer2factor = TRUE, integer2numeric = TRUE),
    verbosity = 0L
  ))
  expect_s3_class(out_both[["i"]], "factor")
  expect_identical(levels(out_both[["i"]]), c("10", "20", "30"))
})


test_that("derived date features are transformed like any other column", {
  skip_if_not_installed("timeDate")
  x <- data.frame(
    d = as.Date(c("2024-01-01", "2024-06-06", "2025-12-25", "2025-03-04")),
    v = c(1.5, 2.5, 3.5, 4.5)
  )
  # Date features are created before the conversions, not appended after them,
  # so the factors they derive are encoded and the year is scaled. Appended
  # last, weekday and month came back as the only factors a one-hot frame
  # did not encode, and the year as the only numeric scale did not touch.
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(
      add_date_features = TRUE,
      add_holidays = TRUE,
      one_hot = TRUE,
      scale = TRUE,
      center = TRUE
    ),
    verbosity = 0L
  ))
  expect_false(any(vapply(out, is.factor, logical(1L))))
  expect_true(all(c("d_weekday_Monday", "d_month_January") %in% names(out)))
  expect_equal(mean(out[["d_year"]]), 0)
  expect_equal(sd(out[["d_year"]]), 1)

  # `factor2integer` reaches them too, which is what the LightGBM family reads.
  coded <- preprocessed(preprocess(
    x,
    setup_Preprocessor(add_date_features = TRUE, factor2integer = TRUE),
    verbosity = 0L
  ))
  expect_type(coded[["d_weekday"]], "integer")
  expect_type(coded[["d_month"]], "integer")
})


test_that("add_holidays flags the holidays in a date column", {
  skip_if_not_installed("timeDate")
  x <- data.frame(
    d = as.Date(c("2024-01-01", "2024-06-06", "2024-12-25")),
    v = c(1.5, 2.5, 3.5)
  )
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(add_holidays = TRUE),
    verbosity = 0L
  ))
  expect_identical(names(out), c("d", "v", "d_holidays"))
  expect_identical(
    as.character(out[["d_holidays"]]),
    c("Holiday", "Not Holiday", "Holiday")
  )
})


test_that("holidays selects which holidays are flagged", {
  skip_if_not_installed("timeDate")
  x <- data.frame(
    d = as.Date(c("2024-09-02", "2024-05-01", "2024-11-28")),
    v = c(1.5, 2.5, 3.5)
  )
  # US Labor Day is the first Monday of September; the unprefixed "LaborDay" is
  # May 1, so the two sets flag different rows.
  us <- preprocessed(preprocess(
    x,
    setup_Preprocessor(add_holidays = TRUE, holidays = "USLaborDay"),
    verbosity = 0L
  ))
  expect_identical(
    as.character(us[["d_holidays"]]),
    c("Holiday", "Not Holiday", "Not Holiday")
  )
  intl <- preprocessed(preprocess(
    x,
    setup_Preprocessor(add_holidays = TRUE, holidays = "LaborDay"),
    verbosity = 0L
  ))
  expect_identical(
    as.character(intl[["d_holidays"]]),
    c("Not Holiday", "Holiday", "Not Holiday")
  )
  thanks <- preprocessed(preprocess(
    x,
    setup_Preprocessor(add_holidays = TRUE, holidays = "USThanksgivingDay"),
    verbosity = 0L
  ))
  expect_identical(
    as.character(thanks[["d_holidays"]]),
    c("Not Holiday", "Not Holiday", "Holiday")
  )
})


test_that("an unknown holiday name is rejected", {
  skip_if_not_installed("timeDate")
  x <- data.frame(d = as.Date("2024-09-02"), v = 1.5)
  expect_error(
    preprocess(
      x,
      setup_Preprocessor(add_holidays = TRUE, holidays = "NotAHoliday"),
      verbosity = 0L
    )
  )
})


test_that("add_holidays leaves a missing date unlabeled", {
  skip_if_not_installed("timeDate")
  x <- data.frame(
    d = as.Date(c("2024-01-01", NA, "2024-06-06")),
    v = c(1.5, 2.5, 3.5)
  )
  out <- preprocessed(preprocess(
    x,
    setup_Preprocessor(add_holidays = TRUE),
    verbosity = 0L
  ))
  expect_identical(
    as.character(out[["d_holidays"]]),
    c("Holiday", NA, "Not Holiday")
  )
})


oh_mixed <- data.frame(
  age = c(30L, 40L, 50L, 60L),
  grp = factor(c("a", "b", "c", "a"), levels = c("a", "b", "c")),
  score = c(1.5, 2.5, 3.5, 4.5),
  note = c("p", "q", "r", "s"),
  stringsAsFactors = FALSE
)

test_that("one_hot keeps the type of every column it does not encode", {
  x <- oh_mixed
  out <- one_hot(x, verbosity = 0L)

  # Names, column order and encoding are as they have always been; only the
  # types differ from a frame built by `cbind`, which coerced every column to
  # character to accommodate `note`. Row names are the default ones: they carry
  # no case identity here, and nothing downstream may start expecting them to.
  expect_identical(
    names(out),
    c("age", "grp_a", "grp_b", "grp_c", "score", "note")
  )
  expect_identical(rownames(out), as.character(1:4))
  expect_identical(out[["grp_a"]], c(1, 0, 0, 1))
  expect_identical(out[["grp_b"]], c(0, 1, 0, 0))
  expect_identical(out[["grp_c"]], c(0, 0, 1, 0))
  expect_identical(out[["age"]], x[["age"]])
  expect_identical(out[["score"]], x[["score"]])
  expect_identical(out[["note"]], x[["note"]])
})


test_that("one_hot encodes a data.table as it encodes a data.frame", {
  # One encoding, two assemblies: the methods share which levels a feature is
  # encoded against and what the columns are called, so the only difference
  # between their output is the structure it comes back in.
  out <- one_hot(data.table::as.data.table(oh_mixed), verbosity = 0L)
  expect_s3_class(out, "data.table")
  expect_identical(
    as.data.frame(out),
    one_hot(oh_mixed, verbosity = 0L)
  )
})


test_that("one_hot encodes a data.table against its pinned levels", {
  x <- data.table::data.table(
    a = c(1.5, 2.5),
    g = factor(c("a", "b"), levels = c("a", "b")),
    h = factor(c("x", NA), levels = c("x", "y"))
  )
  out <- one_hot(
    x,
    factor_levels = list(g = c("a", "b", "c"), h = c("x", "y")),
    verbosity = 0L
  )
  expect_identical(names(out), c("a", "g_a", "g_b", "g_c", "h_x", "h_y"))
  # A level absent from the pinned set, and an NA, each stay all-zero.
  expect_identical(out[["g_c"]], c(0, 0))
  expect_identical(out[["h_x"]], c(1, 0))
  expect_identical(out[["h_y"]], c(0, 0))
})


test_that("one_hot leaves its data.table input alone", {
  x <- data.table::data.table(
    a = c(1.5, 2.5),
    g = factor(c("a", "b"))
  )
  before <- data.table::copy(x)
  out <- one_hot(x, verbosity = 0L)
  # The result must share no column memory with the input: `:=` writes through
  # a shared column, which `setDT()` on the assembled list would have left.
  out[, a := 99]
  expect_identical(x, before)

  # A frame of nothing but factors is encoded, not emptied, and one with no
  # factor at all comes back a data.table.
  all_factors <- data.table::data.table(g = factor(c("a", "b")))
  expect_identical(
    names(one_hot(all_factors, verbosity = 0L)),
    c("g_a", "g_b")
  )
  no_factors <- data.table::data.table(a = c(1.5, 2.5))
  expect_identical(one_hot(no_factors, verbosity = 0L), no_factors)
})


test_that("preprocess(one_hot = TRUE) keeps a character column's frame typed", {
  types <- c(
    age = "integer",
    grp_a = "numeric",
    grp_b = "numeric",
    grp_c = "numeric",
    score = "numeric",
    note = "character"
  )
  out <- preprocessed(preprocess(
    oh_mixed,
    setup_Preprocessor(one_hot = TRUE),
    verbosity = 0L
  ))
  expect_identical(
    vapply(out, function(column) class(column)[[1L]], character(1L)),
    types
  )

  # A data.table in, a data.table out, holding the same types.
  out_dt <- preprocessed(preprocess(
    data.table::as.data.table(oh_mixed),
    setup_Preprocessor(one_hot = TRUE),
    verbosity = 0L
  ))
  expect_s3_class(out_dt, "data.table")
  expect_identical(
    vapply(out_dt, function(column) class(column)[[1L]], character(1L)),
    types
  )
})


test_that("apply_preprocessor encodes new data as the training data was encoded", {
  pre <- preprocess(
    oh_train,
    setup_Preprocessor(one_hot = TRUE),
    verbosity = 0L
  )

  # New data missing level "a" and holding the rest in a different order.
  # Read off its own levels, "c" would land in the first column here and the
  # third in training, and the frame would be one column narrower.
  reordered <- data.frame(
    a = rnorm(3L),
    g = factor(c("c", "b", "c"), levels = c("c", "b")),
    h = factor(c("y", "x", "y"))
  )
  out <- apply_preprocessor(pre, reordered, verbosity = 0L)
  expect_identical(names(out), c("a", "g_a", "g_b", "g_c", "h_x", "h_y"))
  expect_identical(out[["g_a"]], c(0, 0, 0))
  expect_identical(out[["g_c"]], c(1, 0, 1))
  expect_identical(out[["h_y"]], c(1, 0, 1))

  # A level unseen in training has no column to take, so the row is all-zero.
  # `NA` is encoded the same way; the preprocessor has dedicated steps for
  # missingness.
  unseen <- data.frame(
    a = rnorm(3L),
    g = factor(c("a", "d", NA), levels = c("a", "d")),
    h = factor(c("x", "x", "x"))
  )
  out_unseen <- apply_preprocessor(pre, unseen, verbosity = 0L)
  expect_identical(names(out_unseen), c("a", "g_a", "g_b", "g_c", "h_x", "h_y"))
  expect_identical(out_unseen[["g_a"]], c(1, 0, 0))
  expect_identical(out_unseen[["g_b"]], c(0, 0, 0))
  expect_identical(out_unseen[["g_c"]], c(0, 0, 0))
  expect_identical(out_unseen[["h_y"]], c(0, 0, 0))
})


# Test factor2integer ----
f2i_train <- data.frame(
  a = rnorm(6L),
  g = factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c")),
  h = factor(c("x", "y", "x", "y", "x", "y"))
)

test_that("factor2integer codes training data as it always has", {
  # Capturing the levels must not change what `preprocess()` returns: every
  # algorithm that converts factors reads this output.
  out <- preprocessed(preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE),
    verbosity = 0L
  ))
  # Codes are integer on both `startat0` branches: a category code indexes an
  # embedding table or a LightGBM category, and a double cannot.
  expect_identical(out[["g"]], as.integer(f2i_train[["g"]]) - 1L)
  expect_identical(out[["h"]], as.integer(f2i_train[["h"]]) - 1L)
  expect_identical(out[["a"]], f2i_train[["a"]])

  out_1based <- preprocessed(preprocess(
    f2i_train,
    setup_Preprocessor(
      factor2integer = TRUE,
      factor2integer_startat0 = FALSE
    ),
    verbosity = 0L
  ))
  expect_identical(out_1based[["g"]], as.integer(f2i_train[["g"]]))
  expect_identical(out_1based[["h"]], as.integer(f2i_train[["h"]]))

  # The levels are now published, one entry per converted feature.
  pre <- preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE),
    verbosity = 0L
  )
  expect_identical(
    pre@values[["factor2integer_levels"]],
    list(g = c("a", "b", "c"), h = c("x", "y"))
  )
})


test_that("apply_preprocessor codes new data as the training data was coded", {
  pre <- preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE),
    verbosity = 0L
  )

  # New data missing level "a" and holding the rest in a different order.
  # Read off its own levels, "c" would code as 0 here and 2 in training.
  reordered <- data.frame(
    a = rnorm(3L),
    g = factor(c("c", "b", "c"), levels = c("c", "b")),
    h = factor(c("y", "x", "y"))
  )
  out <- apply_preprocessor(pre, reordered, verbosity = 0L)
  expect_identical(out[["g"]], c(2L, 1L, 2L))
  expect_identical(out[["h"]], c(1L, 0L, 1L))

  # A level unseen in training takes the reserved index above the known ones,
  # so an embedding sized at length(levels) + 1 can index it. NA stays NA.
  unseen <- data.frame(
    a = rnorm(3L),
    g = factor(c("a", "d", NA), levels = c("a", "d")),
    h = factor(c("x", "x", "x"))
  )
  out_unseen <- apply_preprocessor(pre, unseen, verbosity = 0L)
  expect_identical(out_unseen[["g"]], c(0L, 3L, NA))
})


test_that("scale and center leave factor2integer codes alone", {
  pre <- preprocess(
    f2i_train,
    setup_Preprocessor(factor2integer = TRUE, scale = TRUE, center = TRUE),
    verbosity = 0L
  )
  out <- preprocessed(pre)

  # Standardizing a category code yields a fraction of an index, so the coded
  # columns are not numeric features as far as scale/center is concerned.
  expect_identical(out[["g"]], as.integer(f2i_train[["g"]]) - 1L)
  expect_identical(out[["h"]], as.integer(f2i_train[["h"]]) - 1L)
  expect_equal(mean(out[["a"]]), 0, tolerance = 1e-12)
  expect_equal(sd(out[["a"]]), 1, tolerance = 1e-12)

  # The learned values cover exactly the features that were scaled. `a` is the
  # only one, which is also the case that would drop a data.frame to a vector
  # were the name check written `names(x[, numeric_index])`.
  expect_identical(names(pre@values[["scale_centers"]]), "a")
  expect_identical(names(pre@values[["scale_coefficients"]]), "a")

  # Replaying the fit must agree with it on which columns are numeric.
  newdata <- data.frame(
    a = rnorm(3L),
    g = factor(c("c", "b", "c"), levels = c("a", "b", "c")),
    h = factor(c("y", "x", "y"), levels = c("x", "y"))
  )
  applied <- apply_preprocessor(pre, newdata, verbosity = 0L)
  expect_identical(applied[["g"]], c(2L, 1L, 2L))
  expect_identical(
    applied[["a"]],
    (newdata[["a"]] - pre@values[["scale_centers"]][["a"]]) /
      pre@values[["scale_coefficients"]][["a"]]
  )
})


# %% Preprocessing inside train() ----

test_that("train() preprocesses features, never the outcome", {
  x <- rnormmat(120L, 3L, seed = 3L)
  datr <- data.frame(x, y = 10 * x[, 1L] + 50 + rnorm(120L))
  plain <- train(datr, hyperparameters = setup_GLM(), verbosity = 0L)
  scaled <- train(
    datr,
    preprocessor_config = setup_SupervisedPreprocessor(
      scale = TRUE,
      center = TRUE
    ),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  # Scaling the outcome would silently report error metrics in scaled units --
  # and R-squared, being scale-invariant, would look identical either way.
  expect_identical(scaled@y_training, plain@y_training)
  expect_equal(
    scaled@metrics_training[["rmse"]],
    plain@metrics_training[["rmse"]],
    tolerance = 1e-8
  )
  # Predictions therefore stay in the outcome's units.
  expect_equal(
    mean(predict(scaled, features(datr))),
    mean(datr[["y"]]),
    tolerance = 1
  )
})


test_that("a fitted preprocessor returns one prediction per row", {
  # A case-removing step could not be replayed on new data: asked for n rows,
  # `predict()` must return n predictions.
  datr <- data.frame(a = rnorm(60L), b = rnorm(60L))
  datr[["y"]] <- datr[["a"]] + rnorm(60L)
  mod <- train(
    datr,
    preprocessor_config = setup_SupervisedPreprocessor(
      scale = TRUE,
      center = TRUE
    ),
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  newdata <- features(datr)
  expect_length(predict(mod, newdata), nrow(newdata))
})


test_that("train() rejects a preprocessor carrying an excluded operation", {
  # `SupervisedPreprocessorConfig` cannot express any of them, so what reaches
  # train() is a `PreprocessorConfig` and the answer is a type error naming the
  # operation that made it one.
  datr <- data.frame(a = rnorm(40L), b = rnorm(40L), y = rnorm(40L))
  values <- list(
    complete_cases = TRUE,
    remove_duplicates = TRUE,
    remove_cases_thres = 0.5,
    remove_features_thres = 0.5
  )
  for (op in PREPROCESSOR_TRAIN_EXCLUDED) {
    config <- do.call(setup_Preprocessor, values[op])
    expect_error(
      train(
        x = datr,
        preprocessor_config = config,
        hyperparameters = setup_GLM(),
        verbosity = 0L
      ),
      op,
      info = op
    )
  }
})


test_that("named and constant feature removal stay allowed in train()", {
  datr <- data.frame(a = rnorm(40L), b = rnorm(40L), y = rnorm(40L))
  datr[["const"]] <- 1
  for (config in list(
    setup_SupervisedPreprocessor(remove_features = "b"),
    setup_SupervisedPreprocessor(remove_constants = TRUE)
  )) {
    expect_no_error(
      train(
        x = datr[, c("a", "b", "const", "y")],
        preprocessor_config = config,
        hyperparameters = setup_GLM(),
        verbosity = 0L
      )
    )
  }
})


# SupervisedPreprocessorConfig ----
# The two classes are built from one property list, and the two `setup_*`
# functions cannot be: `supplied_origins()` reads its caller's formals, so each
# needs its own. These assert what that costs -- that the second stays the first
# minus `PREPROCESSOR_TRAIN_EXCLUDED`, in name, in order, and in default.
test_that("SupervisedPreprocessorConfig is PreprocessorConfig minus the excluded ops", {
  full <- names(props(setup_Preprocessor()))
  supervised <- names(props(setup_SupervisedPreprocessor()))
  expect_identical(supervised, setdiff(full, PREPROCESSOR_TRAIN_EXCLUDED))
  expect_true(all(PREPROCESSOR_TRAIN_EXCLUDED %in% full))
  expect_false(any(PREPROCESSOR_TRAIN_EXCLUDED %in% supervised))
})


test_that("setup_SupervisedPreprocessor formals track setup_Preprocessor", {
  full <- formals(setup_Preprocessor)
  supervised <- formals(setup_SupervisedPreprocessor)
  expect_identical(
    names(supervised),
    setdiff(names(full), PREPROCESSOR_TRAIN_EXCLUDED)
  )
  for (nm in names(supervised)) {
    expect_identical(supervised[[nm]], full[[nm]], info = nm)
  }
})


test_that("pp_opt reads an omitted option as its declared default", {
  supervised <- setup_SupervisedPreprocessor()
  full <- setup_Preprocessor()
  for (nm in PREPROCESSOR_TRAIN_EXCLUDED) {
    expect_identical(pp_opt(supervised, nm), prop(full, nm), info = nm)
  }
  # A property both carry is read from the object, not from the default.
  expect_true(pp_opt(setup_SupervisedPreprocessor(scale = TRUE), "scale"))
})


test_that("train() and SuperConfig refuse a PreprocessorConfig", {
  expect_error(
    setup_SuperConfig(
      dat_training_path = "t.csv",
      preprocessor_config = setup_Preprocessor(),
      hyperparameters = setup_LightRF()
    ),
    "SupervisedPreprocessorConfig"
  )
  # The corrective message names the operations that made it the wrong type.
  expect_error(
    train(
      iris,
      preprocessor_config = setup_Preprocessor(complete_cases = TRUE),
      verbosity = 0L
    ),
    "complete_cases"
  )
})


test_that("a supervised document cannot name an excluded operation", {
  expect_error(
    .list_to_SupervisedPreprocessorConfig(list(remove_duplicates = TRUE)),
    "remove_duplicates"
  )
})
