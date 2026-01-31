# synth_reg_data.R
# ::rtemis::
# 2019 EDG rtemis.org

#' Synthesize Simple Regression Data
#'
#' @param nrow Integer: Number of rows.
#' @param ncol Integer: Number of columns.
#' @param noise_sd_factor Numeric: Add rnorm(nrow, sd = noise_sd_factor * sd(y)).
#' @param resampler_config Output of [setup_Resampler] defining training/test split. The first resulting resample
#' will be used to create `dat_training` and `dat_test` output; all resample output under `resamples`
#' @param seed Integer: Seed for random number generator.
#' @param verbosity Integer: Verbosity level.
#'
#' @author EDG
#' @return List with elements `dat, dat_training, dat_test, resamples, w, seed`
#' @export
synth_reg_data <- function(
  nrow = 500,
  ncol = 50,
  noise_sd_factor = 1,
  resampler_config = setup_Resampler(),
  seed = NULL,
  verbosity = 0L
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  x <- rnormmat(nrow, ncol)
  w <- rnorm(ncol)
  y <- c(x %*% w)
  y <- y + rnorm(nrow, sd = noise_sd_factor * sd(y))
  dat <- data.frame(x, y)
  colnames(dat)[seq(ncol)] <- paste0("Feature_", seq(ncol))

  res <- resample(y, config = resampler_config)
  dat_training <- dat[res[[1]], ]
  dat_test <- dat[-res[[1]], ]

  list(
    dat = dat,
    dat_training = dat_training,
    dat_test = dat_test,
    resamples = res,
    w = w,
    seed = seed
  )
} # /rtemis::synth_reg_data

#' Create "Multimodal" Synthetic Data
#'
#' Create "Multimodal" Synthetic Data using squares and arctangents
#'
#' There are no checks yet for compatibility among inputs and certain combinations may not work.
#'
#' @param n_cases Integer: Number of cases to create.
#' @param init_fn Character: "runifmat" or "rnormmat". Use the respective functions to
#' generate features as random uniform and random normal variables, respectively.
#' @param init_fn_params Named list with arguments "min", "max" for "runifmat" and
#' "mean", "sd" for "rnormmat".
#' @param n_groups Integer: Number of feature groups / modalities to create.
#' @param n_feat_per_group Integer, vector, length `n_groups`: Number of features per group to create.
#' @param contrib_p Float (0, 1]: Ratio of features contributing to outcome per group.
#' a third of the features in each group will be used to produce the outcome y
#' @param linear_p Float \[0, 1\]: Ratio of contributing features to be included linearly.
#' features in each group will be included
#' @param square_p Float \[0, 1\]: Ratio of contributing features to be squared.
#' in each group will be squared
#' @param atan_p Float \[0, 1\]: Ratio of contributing features whose `atan` will be used. These will be selected
#' from the features that were NOT sampled for squaring.
#' i.e. .1 of .33 of features in each group will be transformed using `atan`, but given these features were not
#' already picked to be squared (see `square_p`)
#' @param pair_multiply_p Float \[0, 1\] Ratio of features will be divided into pairs and multiplied.
#' @param pair_square_p Float \[0, 1\] Ratio of features which will be divided into pairs, multiplied and squared.
#' @param pair_atan_p Float \[0, 1\] Ratio of features which will be divided into pairs, multiplied and transformed using
#' `atan`.
#' @param verbosity Integer: Verbosity level.
#' @param seed Integer: If set, pass to `set.seed` for reproducibility
#' @param filename Character: Path to file to save output.
#'
#' @return List with elements x, y, index_square, index_atan, index_pair_square
#'
#' @author EDG
#' @export
#'
#' @examples
#' \dontrun{
#' xmm <- synth_multimodal(
#'   n_cases = 10000,
#'   init_fn = "runifmat",
#'   init_fn_params = list(min = -10, max = 10),
#'   n_groups = 5,
#'   n_feat_per_group = c(20, 50, 100, 200, 300),
#'   contrib_p = .33,
#'   linear_p = .66,
#'   square_p = .1,
#'   atan_p = .1,
#'   pair_multiply_p = .1,
#'   pair_square_p = .1,
#'   pair_atan_p = .1,
#'   seed = 2019
#' )
#' }
synth_multimodal <- function(
  n_cases = 10000,
  init_fn = "runifmat",
  init_fn_params = list(min = -10, max = 10),
  n_groups = 4,
  n_feat_per_group = round(seq(10, 300, length.out = n_groups)),
  contrib_p = .33,
  linear_p = .66,
  square_p = .1,
  atan_p = .1,
  pair_multiply_p = .05,
  pair_square_p = .05,
  pair_atan_p = .05,
  verbosity = 1L,
  seed = NULL,
  filename = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Synth features ----
  x <- lapply(seq(n_groups), function(i) {
    do.call(
      init_fn,
      c(
        list(
          nrow = n_cases,
          ncol = n_feat_per_group[i],
          return_df = FALSE
        ),
        init_fn_params
      )
    )
  })
  names(x) <- paste0("Group_", seq(x))

  # Indexes ----
  # '- contrib ----
  # index.contrib: The variables from each group contributing to the group's outcome
  index.contrib <- lapply(seq(n_groups), function(i) {
    sort(sample(seq(n_feat_per_group[i]), contrib_p * n_feat_per_group[i]))
  })
  names(index.contrib) <- names(x)
  if (verbosity > 0L) {
    cat("  Got index.contrib\n")
  }

  # '- linear ----
  # index_linear: The features within index.contrib that will be included linearly
  index_linear <- lapply(seq(n_groups), function(i) {
    sort(sample(index.contrib[[i]], linear_p * length(index.contrib[[i]])))
  })
  names(index_linear) <- names(x)
  if (verbosity > 0L) {
    cat("  Got index_square\n")
  }

  # '- square ----
  # index_square: The features within index.contrib that will be squared
  index_square <- lapply(seq(n_groups), function(i) {
    sort(sample(index.contrib[[i]], square_p * length(index.contrib[[i]])))
  })
  names(index_square) <- names(x)
  if (verbosity > 0L) {
    cat("  Got index_square\n")
  }

  # '- atan ----
  # index_atan: The features within index.contrib that will be arctanned
  index_atan <- lapply(seq(n_groups), function(i) {
    index.open <- index.contrib[[i]][!index.contrib[[i]] %in% index_square[[i]]]
    sort(sample(index.open, atan_p * length(index.contrib[[i]])))
  })
  names(index_atan) <- names(x)
  if (verbosity > 0L) {
    cat("  Got index_atan\n")
  }

  # '- pair.multiply ----
  # index_pair_multiply
  index_pair_multiply <- lapply(seq(n_groups), function(i) {
    n.pairs <- 2 * round(pair_multiply_p * length(index.contrib[[i]]) / 2)
    if (n.pairs == 0) {
      NULL
    } else {
      index <- sample(index.contrib[[i]], n.pairs * 2)
      t(apply(matrix(index, ncol = 2, byrow = TRUE), 1, sort))
    }
  })
  names(index_pair_multiply) <- names(x)
  if (verbosity > 0L) {
    cat("  Got index_pair_multiply\n")
  }

  # '- pair.square ----
  # index_pair_square
  index_pair_square <- lapply(seq(n_groups), function(i) {
    n.pairs <- 2 * round(pair_square_p * length(index.contrib[[i]]) / 2)
    if (n.pairs == 0) {
      NULL
    } else {
      index <- sample(index.contrib[[i]], n.pairs * 2)
      t(apply(matrix(index, ncol = 2, byrow = TRUE), 1, sort))
    }
  })
  names(index_pair_square) <- names(x)
  if (verbosity > 0L) {
    cat("  Got index_pair_square\n")
  }

  # index_pair_atan ----
  index_pair_atan <- lapply(seq(n_groups), function(i) {
    n.pairs <- 2 * round(pair_atan_p * length(index.contrib[[i]]) / 2)
    if (n.pairs == 0) {
      NULL
    } else {
      index <- sample(index.contrib[[i]], n.pairs * 2)
      t(apply(matrix(index, ncol = 2, byrow = TRUE), 1, sort))
    }
  })
  if (verbosity > 0L) {
    cat("  Got index_pair_atan\n")
  }

  # Outcome ----
  # '- linear, squares & atans ----
  if (verbosity > 0L) {
    cat("  Adding linear, square and atan terms...")
  }
  y1 <- lapply(seq(n_groups), function(i) {
    matrixStats::rowSums2(
      rnorm(1) * x[[i]][, index_linear[[i]], drop = FALSE]
    ) +
      matrixStats::rowSums2(
        rnorm(1) * x[[i]][, index_square[[i]], drop = FALSE]^2
      ) +
      matrixStats::rowSums2(
        rnorm(1) * atan(x[[i]][, index_atan[[i]], drop = FALSE])
      )
  })
  names(y1) <- names(x)
  if (verbosity > 0L) {
    cat(" Done\n")
  }

  # '- pair.multiply ----
  if (verbosity > 0L) {
    cat("  Getting pair products...")
  }
  y2 <- vector("list", n_groups)
  names(y2) <- names(x)
  for (i in seq_len(n_groups)) {
    y2[[i]] <- if (!is.null(index_pair_multiply[[i]])) {
      matrixStats::rowSums2(sapply(
        seq_len(NROW(index_pair_multiply[[i]])),
        function(k) {
          matrixStats::rowProds(
            rnorm(1) * x[[i]][, index_pair_multiply[[i]][k, ]]
          )^2
        }
      ))
    } else {
      rep(0, n_cases)
    }
  }
  if (verbosity > 0L) {
    cat(" Done\n")
  }

  # '- pair.square ----
  if (verbosity > 0L) {
    cat("  Squaring pair products...")
  }
  y3 <- vector("list", n_groups)
  names(y3) <- names(x)
  for (i in seq_len(n_groups)) {
    y3[[i]] <- if (!is.null(index_pair_square[[i]])) {
      matrixStats::rowSums2(sapply(
        seq_len(NROW(index_pair_square[[i]])),
        function(k) {
          matrixStats::rowProds(
            rnorm(1) * x[[i]][, index_pair_square[[i]][k, ]]
          )^2
        }
      ))
    } else {
      rep(0, n_cases)
    }
  }
  if (verbosity > 0L) {
    cat(" Done\n")
  }

  # '- pair.atan ----
  if (verbosity > 0L) {
    cat("  Atan of pair products...")
  }
  y4 <- vector("list", n_groups)
  names(y4) <- names(x)
  for (i in seq_len(n_groups)) {
    y4[[i]] <- if (!is.null(index_pair_atan[[i]])) {
      matrixStats::rowSums2(sapply(
        seq_len(NROW(index_pair_atan[[i]])),
        function(k) {
          matrixStats::rowProds(
            rnorm(1) * x[[i]][, index_pair_atan[[i]][k, ]]
          )^2
        }
      ))
    } else {
      rep(0, n_cases)
    }
  }
  if (verbosity > 0L) {
    cat(" Done\n")
  }

  y <- lapply(
    seq_len(n_groups),
    function(i) y1[[i]] + y2[[i]] + y3[[i]] + y4[[i]]
  )
  names(y) <- names(x)

  out <- list(
    x = x,
    y = y,
    index_linear = index_linear,
    index_square = index_square,
    index_atan = index_atan,
    index_pair_multiply = index_pair_multiply,
    index_pair_square = index_pair_square,
    index_pair_atan = index_pair_atan
  )

  # Save RDS ----
  if (!is.null(filename)) {
    if (verbosity > 0L) {
      cat("Saving data to file...\n")
    }
    filename <- paste0(gsub(".rds", "", filename), ".rds")
    saveRDS(out, filename)
    if (verbosity > 0L) msg("Saved", filename)
  }

  out
} # /rtemis::synth_multimodal
