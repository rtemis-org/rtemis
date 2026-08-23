# Extracted from test_LINAD_differential.R:229

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
    for (position in breaks) {
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
    min_cases_leaf = 5L,
    min_cases_node_model = 5L
  )
  utils::modifyList(defaults, settings)
}

# test -------------------------------------------------------------------------
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
