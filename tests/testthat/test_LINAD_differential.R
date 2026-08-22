# test_LINAD_differential.R
# ::rtemis::
# 2026- EDG rtemis.org

# The engine's fast paths checked against slow implementations of the same
# quantity. The fast paths exist because the obvious computation is too
# expensive to run at every candidate split; these tests run the obvious
# computation at a size where it is affordable, and require the two to agree.
#
# What this catches that the reduction tests cannot: the reductions pin the
# degenerate corner -- constant leaves, a hard partition, no shrinkage -- while
# the configuration the algorithm is actually used in has no external reference.
# Here the reference is written from the definition, so it covers linear leaves,
# soft weighting and the exhaustive search directly.

# %% Reference implementations ----

#' Every candidate split scored from the definition
#'
#' No incremental sufficient statistics, no subtraction of node totals, no
#' algebraic loss identity: each side's weights are formed explicitly over every
#' row, each side's model is fitted from those weights, and the loss is the
#' weighted sum of squared residuals of the fitted values.
reference_sweep <- function(state, r, w, member) {
  xm <- state[["xm"]]
  context <- state[["context"]]
  gamma <- state[["gamma"]]
  n <- nrow(xm)
  min_child <- rtemis:::linad_min_child_cases(state)
  total_members <- sum(member)

  side_loss <- function(weights) {
    if (sum(weights) <= 0) {
      return(Inf)
    }
    fitted <- if (identical(state[["node_model"]], "constant")) {
      rep(sum(weights * r) / sum(weights), n)
    } else {
      gram <- rtemis:::linad_gram(xm, r, weights, NULL)
      b <- rtemis:::linad_gram_solve(
        gram[["G"]],
        gram[["Xty"]],
        gram[["sw"]],
        state[["node_model"]],
        state[["lambda"]],
        state[["nvmax"]],
        syy = gram[["syy"]],
        forward_stop = state[["forward_stop"]],
        node_test = state[["node_test"]]
      )
      if (is.null(b) || !all(is.finite(b))) {
        return(Inf)
      }
      drop(xm %*% b)
    }
    sum(weights * (r - fitted)^2)
  }

  loss_of <- function(goes_left) {
    if (
      sum(member & goes_left) < min_child ||
        (total_members - sum(member & goes_left)) < min_child
    ) {
      return(Inf)
    }
    left <- w
    left[!goes_left] <- left[!goes_left] * gamma
    right <- w
    right[goes_left] <- right[goes_left] * gamma
    side_loss(left) + side_loss(right)
  }

  best <- list(
    loss = Inf,
    feature = NA_character_,
    levels = NULL,
    value = NA_real_
  )
  for (j in seq_along(context[["numeric_names"]])) {
    order_index <- context[["numeric_order"]][[j]]
    breaks <- context[["numeric_breaks"]][[j]]
    sorted <- context[["numeric_matrix"]][order_index, j]
    if (length(breaks) == 0L) {
      next
    }
    # Which cut points the search considers is a separate question from how it
    # scores them, and this reference is about the scoring.
    wanted <- rtemis:::linad_cut_positions(
      breaks,
      state[["n_cuts"]],
      sorted,
      state[["split_bin_type"]]
    )
    wanted <- wanted[wanted >= 1L & wanted < n]
    for (position in wanted) {
      goes_left <- logical(n)
      goes_left[order_index[seq_len(position)]] <- TRUE
      loss <- loss_of(goes_left)
      if (loss < best[["loss"]]) {
        best <- list(
          loss = loss,
          feature = context[["numeric_names"]][[j]],
          levels = NULL,
          value = (sorted[[position]] + sorted[[position + 1L]]) / 2
        )
      }
    }
  }
  for (j in seq_along(context[["factor_names"]])) {
    codes <- context[["factor_codes"]][[j]]
    levels_j <- context[["factor_levels"]][[j]]
    present <- sort(unique(codes))
    k <- length(present)
    if (k < 2L) {
      next
    }
    # Every partition, enumerated without regard to any ordering of the levels.
    for (mask in seq_len(2^(k - 1L) - 1L)) {
      chosen <- present[bitwAnd(mask, bitwShiftL(1L, seq_len(k) - 1L)) > 0L]
      loss <- loss_of(codes %in% chosen)
      if (loss < best[["loss"]]) {
        best <- list(
          loss = loss,
          feature = context[["factor_names"]][[j]],
          levels = levels_j[chosen],
          value = NA_real_
        )
      }
    }
  }
  best
} # /reference_sweep


#' A fit state carrying only what the split searches read
reference_state <- function(x, xm, settings = list()) {
  defaults <- list(
    context = rtemis:::linad_context(x),
    xm = xm,
    n = nrow(xm),
    gamma = 0,
    n_cuts = 20L,
    node_model = "ridge",
    lambda = 0.1,
    nvmax = 3L,
    forward_stop = "none",
    node_test = "none",
    split_search = "exhaustive",
    split_criterion = "mean",
    split_bin_type = "frequency",
    min_cases_leaf = 5L,
    min_cases_node_model = 5L
  )
  utils::modifyList(defaults, settings)
} # /reference_state


# %% The exhaustive search against the definition ----
test_that("linad_sweep() scores every candidate as the definition does", {
  set.seed(2026)
  n <- 220L
  x <- data.frame(
    a = rnorm(n),
    b = rnorm(n),
    g = factor(sample(c("p", "q", "r"), n, replace = TRUE)),
    h = factor(sample(c("u", "v"), n, replace = TRUE))
  )
  xm <- cbind(
    1,
    x[["a"]],
    x[["b"]],
    as.numeric(x[["g"]] == "q"),
    as.numeric(x[["g"]] == "r"),
    as.numeric(x[["h"]] == "v")
  )
  r <- 3 *
    x[["a"]] *
    ifelse(x[["b"]] > 0, 1, -1) +
    ifelse(x[["g"]] == "q", 2, 0) +
    rnorm(n, 0, 0.5)
  w <- runif(n, 0.5, 2)
  member <- rep(TRUE, n)

  grid <- expand.grid(
    node_model = c("constant", "ridge", "forward"),
    gamma = c(0, 0.2),
    stringsAsFactors = FALSE
  )
  for (row in seq_len(nrow(grid))) {
    settings <- list(
      node_model = grid[["node_model"]][[row]],
      gamma = grid[["gamma"]][[row]]
    )
    state <- reference_state(x, xm, settings)
    fast <- rtemis:::linad_sweep(state, r, w, member)
    slow <- reference_sweep(state, r, w, member)
    label <- paste(settings[["node_model"]], settings[["gamma"]])
    expect_equal(fast[["feature"]], slow[["feature"]], info = label)
    # `gain` is the negated loss, so the two are comparable directly.
    expect_equal(
      -fast[["gain"]],
      slow[["loss"]],
      tolerance = 1e-8,
      info = label
    )
    if (identical(fast[["kind"]], "factor")) {
      # Either side of a partition names it.
      expect_true(
        setequal(fast[["levels"]], slow[["levels"]]) ||
          setequal(
            fast[["levels"]],
            setdiff(
              state[["context"]][["factor_levels"]][[fast[["column"]]]],
              slow[["levels"]]
            )
          ),
        info = label
      )
    } else {
      expect_equal(
        fast[["value"]],
        slow[["value"]],
        tolerance = 1e-8,
        info = label
      )
    }
  }
})


test_that("linad_sweep() honors the child-size floor as the definition does", {
  # A floor that the search and the reference read differently would show as a
  # disagreement only on candidates near it, which is where they are placed.
  set.seed(11)
  n <- 120L
  x <- data.frame(a = rnorm(n), b = rnorm(n))
  xm <- cbind(1, x[["a"]], x[["b"]])
  r <- x[["a"]] + rnorm(n, 0, 0.5)
  w <- rep(1, n)
  member <- rep(TRUE, n)
  member[sample.int(n, 40L)] <- FALSE
  for (floor_size in c(5L, 20L, 45L)) {
    state <- reference_state(
      x,
      xm,
      list(min_cases_leaf = floor_size, min_cases_node_model = floor_size)
    )
    fast <- rtemis:::linad_sweep(state, r, w, member)
    slow <- reference_sweep(state, r, w, member)
    if (is.null(fast)) {
      expect_false(is.finite(slow[["loss"]]), info = floor_size)
    } else {
      expect_equal(
        -fast[["gain"]],
        slow[["loss"]],
        tolerance = 1e-8,
        info = floor_size
      )
    }
  }
})


test_that("linad_sweep() agrees with the definition on randomized problems", {
  # Twelve draws varying shape, weights, membership and hyperparameters, so a
  # defect that needs a particular configuration has somewhere to show.
  set.seed(4)
  for (rep in seq_len(12L)) {
    n <- sample(80:200, 1L)
    n_numeric <- sample(1:3, 1L)
    n_levels <- sample(2:4, 1L)
    x <- as.data.frame(matrix(rnorm(n * n_numeric), n, n_numeric))
    names(x) <- paste0("v", seq_len(n_numeric))
    x[["f"]] <- factor(sample(letters[seq_len(n_levels)], n, replace = TRUE))
    xm <- cbind(
      1,
      as.matrix(x[seq_len(n_numeric)]),
      stats::model.matrix(~f, x)[, -1L, drop = FALSE]
    )
    r <- rnorm(n) + as.numeric(x[["f"]])
    w <- if (rep %% 2L == 0L) rep(1, n) else runif(n, 0.2, 3)
    member <- rep(TRUE, n)
    if (rep %% 3L == 0L) {
      member[sample.int(n, n %/% 4L)] <- FALSE
    }
    state <- reference_state(
      x,
      xm,
      list(
        node_model = sample(c("constant", "ridge", "forward"), 1L),
        gamma = sample(c(0, 0.1, 0.5), 1L),
        lambda = sample(c(0.01, 0.5), 1L),
        nvmax = sample(1:3, 1L),
        min_cases_leaf = sample(3:10, 1L),
        min_cases_node_model = sample(3:10, 1L)
      )
    )
    fast <- rtemis:::linad_sweep(state, r, w, member)
    slow <- reference_sweep(state, r, w, member)
    expect_equal(
      -fast[["gain"]],
      slow[["loss"]],
      tolerance = 1e-8,
      info = paste("replicate", rep)
    )
    expect_equal(
      fast[["feature"]],
      slow[["feature"]],
      info = paste("replicate", rep)
    )
  }
})


# %% The two node-model solvers against each other ----
test_that("linad_solve() and linad_gram_solve() fit the same model", {
  # One is used by the commit and reads the data; the other is used by the
  # exhaustive search and reads sufficient statistics. They fit the same node
  # model by different routes -- one centers the design and carries the level
  # separately, the other solves jointly with an intercept column -- and a
  # divergence between them is a search optimizing a model that is never built.
  set.seed(3)
  for (rep in seq_len(10L)) {
    n <- sample(60:200, 1L)
    p <- sample(2:6, 1L)
    X <- matrix(rnorm(n * p), n, p)
    xm <- cbind(1, X)
    y <- drop(X %*% rnorm(p)) + rnorm(n)
    w <- if (rep %% 2L == 0L) rep(1, n) else runif(n, 0.3, 2)
    idx <- sort(sample.int(n, max(20L, n %/% 2L)))
    gram <- rtemis:::linad_gram(xm, y, w, idx)
    lambda <- sample(c(0, 0.05, 0.5), 1L)
    nvmax <- sample(seq_len(p), 1L)
    for (node_model in c("constant", "ridge", "forward")) {
      direct <- rtemis:::linad_solve(
        xm,
        y,
        w,
        idx,
        node_model,
        lambda = lambda,
        nvmax = nvmax,
        forward_stop = "none"
      )
      from_gram <- rtemis:::linad_gram_solve(
        gram[["G"]],
        gram[["Xty"]],
        gram[["sw"]],
        node_model,
        lambda,
        nvmax,
        syy = gram[["syy"]],
        forward_stop = "none"
      )
      expect_equal(
        direct[["coefficients"]],
        from_gram,
        tolerance = 1e-8,
        info = paste(node_model, rep)
      )
    }
  }
})


test_that("linad_gram_loss() equals the residual sum of squares it stands for", {
  # The identity syy - 2 b'Xty + b'Gb is what makes a candidate cost no data
  # access. It is used everywhere and asserted nowhere.
  set.seed(8)
  for (rep in seq_len(10L)) {
    n <- sample(50:150, 1L)
    p <- sample(2:5, 1L)
    xm <- cbind(1, matrix(rnorm(n * p), n, p))
    y <- rnorm(n)
    w <- runif(n, 0.1, 3)
    gram <- rtemis:::linad_gram(xm, y, w, NULL)
    b <- rnorm(p + 1L)
    expect_equal(
      rtemis:::linad_gram_loss(gram[["G"]], gram[["Xty"]], gram[["syy"]], b),
      sum(w * (y - drop(xm %*% b))^2),
      tolerance = 1e-8
    )
  }
})


test_that("linad_ridge_edf() equals the trace it stands for", {
  set.seed(5)
  for (d in c(4L, 12L, 30L)) {
    G <- crossprod(matrix(rnorm(5 * d * d), 5 * d, d))
    for (penalty in c(0, 0.5, 10, 1000)) {
      for (intercept in c(TRUE, FALSE)) {
        ridge <- rtemis:::linad_ridge_diagonal(G, penalty, intercept)
        # tr((G + D)^-1 G), computed directly.
        expect_equal(
          rtemis:::linad_ridge_edf(G, penalty, intercept),
          sum(diag(solve(G + diag(ridge, d)) %*% G)),
          tolerance = 1e-6,
          info = paste(d, penalty, intercept)
        )
      }
    }
  }
})


# %% The floor, as a tree-level invariant ----
test_that("linad_check_tree() catches a tree holding nodes below the floor", {
  # The failure this guards is a hyperparameter that moves the split floor
  # without naming it in its description: `node_test` did, dropping the floor
  # from `min_cases_node_model` to `min_cases_leaf`, which defaults to 1.
  set.seed(2026)
  n <- 400L
  X <- as.data.frame(matrix(rnorm(n * 4L), n, 4L))
  names(X) <- paste0("x", 1:4)
  X[["y"]] <- ifelse(X[["x1"]] > 0, 3 * X[["x2"]] - 2 * X[["x3"]], 5) +
    rnorm(n, 0, 1)
  fit <- function(...) {
    train(
      X,
      hyperparameters = setup_LINAD(
        max_leaves = 8L,
        node_model = "ridge",
        min_cases_node_model = 25L,
        force_max_leaves = TRUE,
        ...
      ),
      execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
      verbosity = 0L
    )
  }
  for (rule in c("none", "aic", "bic")) {
    expect_length(
      linad_check_tree(fit(node_test = rule)@model, min_cases_child = 25L),
      0L
    )
  }
  # And it reports rather than passing silently when the floor is not met.
  expect_length(
    linad_check_tree(fit()@model, min_cases_child = 10000L),
    1L
  )
})


# %% The selected tree is the model ----
test_that("Split-gain importance counts only splits the selected tree reaches", {
  # The frame keeps every node grown while validation selects a smaller size,
  # so anything describing the model must stop at the selected terminals.
  set.seed(6)
  n <- 400L
  dat <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  dat[["y"]] <- ifelse(dat[["b"]] < 0, -2, 2) +
    1.5 * dat[["a"]] +
    rnorm(n, 0, 0.7)
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = 7L, force_max_leaves = TRUE),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
  total_gain <- function(k) {
    shrunk <- mod
    shrunk@model@n_leaves <- k
    importance <- varimp_super(shrunk@model)
    sum(S7::prop(importance, S7::prop_names(importance)[[1L]])[["split_gain"]])
  }
  # One leaf reaches no split at all.
  expect_equal(total_gain(1L), 0)
  # And a larger selected tree never reaches less gain than a smaller one.
  gains <- vapply(seq_len(7L), total_gain, numeric(1L))
  expect_false(is.unsorted(gains))
})


test_that("A printed tree describes its selected size, not everything grown", {
  set.seed(6)
  n <- 300L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- ifelse(dat[["b"]] < 0, -2, 2) + dat[["a"]] + rnorm(n, 0, 0.7)
  mod <- train(
    dat,
    hyperparameters = setup_LINAD(max_leaves = 6L, force_max_leaves = TRUE),
    execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
    verbosity = 0L
  )
  shrunk <- mod
  shrunk@model@n_leaves <- 1L
  # A single-leaf tree is one node of depth 0, whatever the frame still holds.
  expect_match(
    paste(utils::capture.output(print(shrunk@model)), collapse = " "),
    "1 node"
  )
  expect_gt(nrow(mod@model@frame), 1L)
})


# %% Classification initialization ----
test_that("The classification root is the loss-minimizing constant at every rate", {
  set.seed(2)
  n <- 400L
  dat <- data.frame(x = rnorm(n))
  dat[["y"]] <- factor(
    ifelse(runif(n) < 0.8, "pos", "neg"),
    levels = c("neg", "pos")
  )
  prevalence <- mean(dat[["y"]] == "pos")
  baseline <- 0.5 * log(prevalence / (1 - prevalence))
  for (rate in c(0, 1e-12, 0.5, 1)) {
    mod <- train(
      dat,
      hyperparameters = setup_LINAD(
        max_leaves = 1L,
        node_model = "constant",
        root_learning_rate = rate
      ),
      execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
      verbosity = 0L
    )
    expect_equal(
      mod@model@frame[["node_value"]][[1L]],
      baseline,
      tolerance = 1e-8,
      info = rate
    )
    # An intercept-only classification predicts the prevalence.
    expect_equal(
      predict(mod, dat["x"])[[1L]],
      prevalence,
      tolerance = 1e-8,
      info = rate
    )
  }
})


# %% Forest boundaries ----
test_that("A forest predicts and reports a standard error for one case", {
  set.seed(1)
  n <- 150L
  dat <- data.frame(a = rnorm(n), b = rnorm(n))
  dat[["y"]] <- 2 * dat[["a"]] + rnorm(n)
  classification <- dat
  classification[["y"]] <- factor(
    ifelse(dat[["y"]] > 0, "hi", "lo"),
    levels = c("lo", "hi")
  )
  one_row <- dat[1L, c("a", "b")]
  for (n_trees in c(1L, 3L)) {
    for (outcome in list(dat, classification)) {
      mod <- train(
        outcome,
        hyperparameters = setup_LINADForest(n_trees = n_trees, max_leaves = 4L),
        execution_config = setup_ExecutionConfig(seed = 1L, backend = "none"),
        verbosity = 0L
      )
      expect_length(predict(mod, one_row), 1L)
      expect_equal(
        as.vector(predict(mod, one_row)),
        as.vector(predict(mod, dat[1:3, c("a", "b")]))[[1L]],
        tolerance = 1e-10
      )
      standard_error <- se(mod, one_row)
      expect_length(standard_error, 1L)
      if (n_trees == 1L) {
        # The infinitesimal jackknife is a covariance across bags and has
        # nothing to vary over with one tree.
        expect_true(is.na(standard_error))
      } else {
        expect_true(is.finite(standard_error) && standard_error >= 0)
      }
    }
  }
})
