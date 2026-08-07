# benchmark_calibrators.R
# ::rtemis::
# 2026- EDG rtemis.org

# Compares the calibrators `calibrate()` accepts: Isotonic, which is the
# default, against MonotonicHAL. This is the evidence for that choice, so rerun
# it before changing the default.
#
# The comparison is made at the score level: each problem yields a vector of
# uncalibrated positive-class scores and the matching labels, split into a
# calibration set and an evaluation set. That isolates the calibration map from
# the base classifier's own variance, which is what is being compared.
#
# Run with: Rscript data-raw/benchmark_calibrators.R

suppressMessages(library(rtemis))
suppressMessages(library(data.table))

N_REPLICATES <- 10L
CAL_SIZES <- c(100L, 500L, 2000L)
EVAL_SIZE <- 4000L
EPS <- 1e-15
N_ECE_BINS <- 10L


# %% Metrics ----

brier <- function(p, y) mean((p - y)^2)


log_loss <- function(p, y) {
  # Isotonic returns exact 0 and 1, which make the unclipped log loss infinite.
  # Clipping keeps the table readable; `n_saturated` reports how often it bit.
  pc <- pmin(pmax(p, EPS), 1 - EPS)
  -mean(y * log(pc) + (1 - y) * log(1 - pc))
}


# Expected calibration error over equal-width bins.
ece <- function(p, y, n_bins = N_ECE_BINS) {
  bin <- cut(
    p,
    breaks = seq(0, 1, length.out = n_bins + 1L),
    include.lowest = TRUE
  )
  parts <- vapply(
    split(seq_along(p), bin),
    function(idx) {
      if (length(idx) == 0L) {
        return(0)
      }
      length(idx) / length(p) * abs(mean(p[idx]) - mean(y[idx]))
    },
    numeric(1L)
  )
  sum(parts)
}


auc <- function(p, y) {
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  if (n1 == 0L || n0 == 0L) {
    return(NA_real_)
  }
  r <- rank(p)
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}


n_saturated <- function(p) sum(p <= 0 | p >= 1)


# %% Problems ----
# Each returns list(score = numeric, y = integer 0/1) of at least
# max(CAL_SIZES) + EVAL_SIZE cases.

# A base classifier's scores, deliberately distorted so they rank well but are
# miscalibrated. `distort` maps a well-calibrated probability to a bad one.
synthetic_problem <- function(n, distort, seed) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  p_true <- plogis(1.2 * x1 + 0.8 * x2)
  y <- rbinom(n, 1L, p_true)
  list(score = pmin(pmax(distort(p_true), EPS), 1 - EPS), y = y)
}


# Scores from a real model fit on a real dataset, via a training split.
model_problem <- function(dat, outcome_name, n_needed, seed) {
  set.seed(seed)
  dat <- dat[sample(nrow(dat)), , drop = FALSE]
  n_train <- floor(nrow(dat) / 3)
  train_idx <- seq_len(n_train)
  mod <- train(
    dat[train_idx, , drop = FALSE],
    hyperparameters = setup_GLM(),
    verbosity = 0L
  )
  rest <- dat[-train_idx, , drop = FALSE]
  # Resample the held-out rows up to the size the benchmark needs.
  idx <- sample(nrow(rest), n_needed, replace = TRUE)
  rest <- rest[idx, , drop = FALSE]
  score <- rtemis:::positive_prob(
    predict(mod, rest[, setdiff(names(rest), outcome_name), drop = FALSE])
  )
  y <- as.integer(rest[[outcome_name]]) - 1L
  list(score = pmin(pmax(score, EPS), 1 - EPS), y = y)
}


# Sepal measurements only: the petal ones separate the two species almost
# perfectly, which makes the GLM diverge and leaves no miscalibration to fix.
iris_binary <- function() {
  d <- iris[51:150, c("Sepal.Length", "Sepal.Width", "Species")]
  d$Species <- factor(d$Species)
  d
}


pima <- function() {
  d <- rbind(MASS::Pima.tr, MASS::Pima.te)
  d
}


PROBLEMS <- list(
  list(
    name = "synthetic-overconfident",
    gen = function(n, seed) synthetic_problem(n, function(p) p^3, seed)
  ),
  list(
    name = "synthetic-underconfident",
    gen = function(n, seed) {
      synthetic_problem(n, function(p) 0.5 + 0.35 * (2 * p - 1), seed)
    }
  ),
  list(
    name = "synthetic-logit-shift",
    gen = function(n, seed) {
      synthetic_problem(n, function(p) plogis(2.2 * qlogis(p) - 1.1), seed)
    }
  ),
  list(
    name = "iris-binary-GLM",
    gen = function(n, seed) model_problem(iris_binary(), "Species", n, seed)
  ),
  list(
    name = "pima-GLM",
    gen = function(n, seed) model_problem(pima(), "type", n, seed)
  )
)


CALIBRATORS <- list(
  MonotonicHAL = function() setup_MonotonicHAL(),
  Isotonic = function() setup_Isotonic()
)


# %% Run ----

fit_and_score <- function(hyperparameters, cal, ev) {
  dat_cal <- data.table(
    predicted_probabilities = cal[["score"]],
    true_labels = factor(
      cal[["y"]],
      levels = c(0L, 1L),
      labels = c("neg", "pos")
    )
  )
  t0 <- proc.time()[["elapsed"]]
  mod <- train(dat_cal, hyperparameters = hyperparameters, verbosity = 0L)
  elapsed <- proc.time()[["elapsed"]] - t0
  p <- rtemis:::positive_prob(predict(
    mod,
    data.frame(predicted_probabilities = ev[["score"]])
  ))
  list(p = as.numeric(p), seconds = elapsed)
}


rows <- list()
for (problem in PROBLEMS) {
  for (n_cal in CAL_SIZES) {
    for (rep in seq_len(N_REPLICATES)) {
      seed <- 1000L * rep + n_cal
      dat <- problem[["gen"]](n_cal + EVAL_SIZE, seed)
      cal <- list(
        score = dat[["score"]][seq_len(n_cal)],
        y = dat[["y"]][seq_len(n_cal)]
      )
      ev_idx <- seq.int(n_cal + 1L, n_cal + EVAL_SIZE)
      ev <- list(score = dat[["score"]][ev_idx], y = dat[["y"]][ev_idx])
      base_auc <- auc(ev[["score"]], ev[["y"]])

      rows[[length(rows) + 1L]] <- data.table(
        problem = problem[["name"]],
        n_cal = n_cal,
        rep = rep,
        calibrator = "Uncalibrated",
        brier = brier(ev[["score"]], ev[["y"]]),
        log_loss = log_loss(ev[["score"]], ev[["y"]]),
        ece = ece(ev[["score"]], ev[["y"]]),
        auc_delta = 0,
        n_saturated = n_saturated(ev[["score"]]),
        seconds = 0
      )

      for (cname in names(CALIBRATORS)) {
        out <- tryCatch(
          fit_and_score(CALIBRATORS[[cname]](), cal, ev),
          error = function(e) NULL
        )
        if (is.null(out)) {
          next
        }
        rows[[length(rows) + 1L]] <- data.table(
          problem = problem[["name"]],
          n_cal = n_cal,
          rep = rep,
          calibrator = cname,
          brier = brier(out[["p"]], ev[["y"]]),
          log_loss = log_loss(out[["p"]], ev[["y"]]),
          ece = ece(out[["p"]], ev[["y"]]),
          auc_delta = auc(out[["p"]], ev[["y"]]) - base_auc,
          n_saturated = n_saturated(out[["p"]]),
          seconds = out[["seconds"]]
        )
      }
    }
  }
}

results <- rbindlist(rows)

summary_table <- results[,
  .(
    brier = mean(brier),
    log_loss = mean(log_loss),
    ece = mean(ece),
    auc_delta = mean(auc_delta),
    n_saturated = mean(n_saturated),
    seconds = mean(seconds)
  ),
  by = .(problem, n_cal, calibrator)
]

summary_table[,
  calibrator := factor(
    calibrator,
    levels = c("Uncalibrated", "Isotonic", "MonotonicHAL")
  )
]
setorder(summary_table, problem, n_cal, calibrator)

# One block per problem, so the columns stay on one line.
for (nm in unique(summary_table[["problem"]])) {
  cat("\n", nm, "\n", sep = "")
  block <- summary_table[problem == nm]
  cat(sprintf(
    "  %5s  %-13s %8s %9s %8s %10s %6s %8s\n",
    "n_cal",
    "calibrator",
    "brier",
    "logloss",
    "ece",
    "auc_delta",
    "sat",
    "secs"
  ))
  for (i in seq_len(nrow(block))) {
    cat(sprintf(
      "  %5d  %-13s %8.4f %9.4f %8.4f %+10.5f %6.0f %8.3f\n",
      block[["n_cal"]][i],
      as.character(block[["calibrator"]][i]),
      block[["brier"]][i],
      block[["log_loss"]][i],
      block[["ece"]][i],
      block[["auc_delta"]][i],
      block[["n_saturated"]][i],
      block[["seconds"]][i]
    ))
  }
}

out_file <- file.path(tempdir(), "calibrator_benchmark.rds")
saveRDS(results, out_file)
cat("\nRaw results saved to ", out_file, "\n", sep = "")
