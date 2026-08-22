# Extracted from test_LINAD_differential.R:562

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "rtemis", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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
      state[["n_cuts"]] - 1L,
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
}
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
}

# test -------------------------------------------------------------------------
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
      predict(mod, one_row),
      predict(mod, dat[1:3, c("a", "b")])[[1L]],
      tolerance = 1e-10
    )
    standard_error <- se(mod, one_row)
    expect_length(standard_error, 1L)
    expect_true(is.finite(standard_error) && standard_error >= 0)
  }
}
