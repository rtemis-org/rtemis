# Declarative boundary corpus shared by the R and foreign-runtime checks.
schema_rule_cases <- function() {
  cases <- list()
  add <- function(class, arguments, expected = NULL) {
    cases[[length(cases) + 1L]] <<- list(
      class = class,
      arguments = arguments,
      expected = expected
    )
  }
  for (method in c("backward", "cv")) {
    for (folds in c(0L, 1L, 2L, 10L)) {
      add("MARSHyperparameters", list(pmethod = method, nfold = folds))
    }
  }
  for (ensembles in c(0L, 1L, 2L, 5L)) {
    for (chains in c(1L, 2L, 5L, 6L)) {
      add("BARTHyperparameters", list(num_gfr = ensembles, num_chains = chains))
    }
  }
  for (degree in list(1L, 2L, 3L, tune_over(1L, 2L))) {
    for (knots in list(
      NULL,
      1L,
      c(3L, 2L),
      c(2L, 3L),
      c(3L, 3L),
      c(3L, 2L, 1L)
    )) {
      add("HALHyperparameters", list(max_degree = degree, num_knots = knots))
    }
  }
  for (cls in c("LINADHyperparameters", "LINADForestHyperparameters")) {
    for (linear in list(NULL, "a", c("a", "b"))) {
      for (global in list(NULL, "a", "b", c("a", "b"))) {
        add(cls, list(linear_features = linear, global_features = global))
      }
    }
  }
  for (cls in c("LightGBMHyperparameters", "LightRuleFitHyperparameters")) {
    for (strategy in list("bagging", "goss")) {
      for (fraction in list(0.5, 1, tune_over(0.5, 1))) {
        for (top in list(0.2, 0.7, tune_over(0.2, 0.7))) {
          for (other in list(0.3, 0.8, tune_over(0.3, 0.8))) {
            # GOSS-only values are unset outside GOSS, as the property gates require.
            goss <- any(candidate_values(strategy) == "goss")
            add(
              cls,
              list(
                data_sample_strategy = strategy,
                bagging_fraction = fraction,
                top_rate = if (goss) top else NULL,
                other_rate = if (goss) other else NULL
              )
            )
          }
        }
      }
    }
  }
  for (ifw in c(FALSE, TRUE)) {
    for (lightgbm in list(FALSE, TRUE, tune_over(FALSE, TRUE))) {
      for (glmnet in list(FALSE, TRUE, tune_over(FALSE, TRUE))) {
        add(
          "LightRuleFitHyperparameters",
          list(ifw = ifw, ifw_lightgbm = lightgbm, ifw_glmnet = glmnet),
          expected = !ifw ||
            !(any(candidate_values(lightgbm)) || any(candidate_values(glmnet)))
        )
      }
    }
  }
  for (search in c("exhaustive", "randomized")) {
    for (fraction in list(NULL, 0.2, 0.9)) {
      add(
        "GridSearchConfig",
        list(search_type = search, randomize_p = fraction)
      )
    }
  }
  for (method in c("file", "object", "table")) {
    for (source in list(NULL, "data.csv")) {
      for (columns in list(NULL, "a", c("a", "b"))) {
        add(
          "DataFingerprint",
          list(
            method = method,
            source = source,
            column_names = columns,
            n_cols = 2L,
            hash = "aabb",
            encoding = "test",
            language = "R",
            data_structure = "data.frame"
          )
        )
      }
    }
  }
  for (field in c("hash", "encoding", "language", "data_structure")) {
    arguments <- list(
      hash = "aabb",
      encoding = "test",
      language = "R",
      data_structure = "data.frame"
    )
    arguments[[field]] <- ""
    add("DataFingerprint", arguments)
  }
  for (path in c("data.parquet", "")) {
    for (hash in c("aabb", "")) {
      add("DataRef", list(path = path, hash = hash))
    }
  }
  cases
}
