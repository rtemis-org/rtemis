# generate_profile_fixture.R
# ::rtemis::
# 2026- EDG rtemis.org

# `profile/v1/fixture.json` and the Parquet files it names -- the documents that
# hold the three profile emitters to one answer.
#
# `checks/v1/corpus.json` cannot do this job. Profiles are *inputs* to the
# corpus, so a rule evaluated identically by every host still gives the wrong
# answer if the hosts described the dataset differently. Nothing else compares
# rtemis's `data_profile()`, the CLI's polars profiler and rtemislive's DuckDB
# one.
#
# **Parquet, not a delimited file.** A delimited file carries no types, so each
# reader infers them and the fixture would be testing three type inferences
# rather than three profilers -- and the inferences legitimately differ, which
# is why an ingest step exists to settle them once. Parquet *declares* its
# types, so every host reads the same declaration and a disagreement is a real
# disagreement.
#
# The data is published beside this document rather than inside it. A fixture
# that embedded its dataset would be a mechanism that only works while the
# dataset is small, and would invite someone to point it at one that is not.
# Each case names its file and records the sha256 a consumer must verify --
# the same shape as the wasm module's manifest.
#
# The Parquet files under `inst/profile/fixtures/` are the source of the
# fixture's bytes, and consumers verify those bytes by sha256. They are written
# by nanoparquet, which records no version of itself in the file and writes
# the same data to the same bytes; arrow records its version in the footer, so
# its files change with every arrow release. Generation copies the shipped
# files and verifies that they hold the declared cases; only `--refresh`
# writes them, and only where nanoparquet's output differs from the shipped
# bytes.
#
# Usage:
#   Rscript data-raw/generate_profile_fixture.R <out_dir>
#     Writes `<out_dir>/profile/v1/fixture.json` and copies the shipped Parquet
#     files beside it. Fails when a shipped file does not hold its case.
#   Rscript data-raw/generate_profile_fixture.R --refresh
#     Rewrites `inst/profile/fixture.json`, and the Parquet file of each case
#     whose shipped bytes differ from what nanoparquet writes for it.

suppressMessages(pkgload::load_all(
  rprojroot::find_package_root_file(),
  quiet = TRUE
))
check_dependencies("arrow")

args <- commandArgs(trailingOnly = TRUE)
refresh <- identical(args, "--refresh")
if (!refresh && length(args) != 1L) {
  stop("Usage: generate_profile_fixture.R <out_dir> | --refresh", call. = FALSE)
}
out_dir <- if (refresh) NULL else args[[1L]]
base_url <- "https://schema.rtemis.org"
rel_dir <- file.path("profile", "v1", "fixtures")
inst_dir <- rprojroot::find_package_root_file("inst", "profile")

# %% The cases ----

# Every dtype the vocabulary can hold, plus the shapes a check reads: a gap in a
# number and in an integer, a factor that carries level counts, a constant, and
# two rows that repeat an earlier one. A `factor` is what makes this Parquet
# carry a dictionary column, which is the one type a delimited file cannot say.
.mixed <- local({
  set.seed(2026L)
  n <- 24L
  d <- data.frame(
    x_num = c(NA, seq_len(n - 1L) * 1.25),
    x_int = c(NA_integer_, rep_len(0:6, n - 1L)),
    g = factor(rep_len(c("alpha", "beta", "gamma"), n)),
    flag = rep_len(c(TRUE, FALSE), n),
    when = as.Date("2026-01-01") + rep_len(0:27, n),
    note = rep_len(c("free text one", "free text two"), n),
    const = factor(rep_len("k", n))
  )
  rbind(d, d[1L, ], d[1L, ])
})

# `id` is above `PROFILE_MAX_LEVELS` and carries no level counts; `grp` beside it
# is below and does. The bound is what keeps the document a description rather
# than a copy of the column, and having both here shows it is applied per column
# rather than to the table.
.wide <- local({
  n <- PROFILE_MAX_LEVELS + 6L
  data.frame(
    id = factor(sprintf("id%03d", seq_len(n))),
    grp = factor(rep_len(c("a", "b"), n)),
    y = rep_len(0:2, n)
  )
})

cases <- list(
  list(
    id = "every dtype, with gaps, a constant and duplicate rows",
    file = "mixed.parquet",
    data = .mixed
  ),
  list(
    id = "a column above PROFILE_MAX_LEVELS carries no level counts",
    file = "wide.parquet",
    data = .wide
  )
)

# %% Shipped files ----

sha256 <- function(path) {
  paste(
    as.character(openssl::sha256(readBin(path, "raw", file.size(path)))),
    collapse = ""
  )
}

# The case as a reader sees it, without row names, which carry no data.
read_case <- function(path) {
  out <- as.data.frame(arrow::read_parquet(path))
  rownames(out) <- NULL
  out
}

holds_case <- function(path, case) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  declared <- case[["data"]]
  rownames(declared) <- NULL
  isTRUE(all.equal(read_case(path), declared))
}

shipped_path <- function(case) {
  file.path(inst_dir, "fixtures", case[["file"]])
}

if (refresh) {
  dir.create(
    file.path(inst_dir, "fixtures"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  for (case in cases) {
    written <- tempfile(fileext = ".parquet")
    nanoparquet::write_parquet(case[["data"]], written)
    shipped <- shipped_path(case)
    if (!file.exists(shipped) || !identical(sha256(written), sha256(shipped))) {
      file.copy(written, shipped, overwrite = TRUE)
      cat(sprintf("profile fixture  rewrote %s\n", case[["file"]]))
    }
    unlink(written)
  }
} else {
  for (case in cases) {
    if (!holds_case(shipped_path(case), case)) {
      stop(
        "inst/profile/fixtures/",
        case[["file"]],
        " does not hold the case declared for it. Run `just checks-refresh` ",
        "and review the diff.",
        call. = FALSE
      )
    }
  }
}


# %% Measure ----

measure <- function(case) {
  path <- shipped_path(case)
  list(
    id = case[["id"]],
    # Registry-relative, which is what a consumer knows a document by.
    data = paste(rel_dir, case[["file"]], sep = "/"),
    sha256 = sha256(path),
    profile = to_json(data_profile(read(path, verbosity = 0L)))
  )
}

doc <- list(
  `$id` = paste0(base_url, "/profile/v1/fixture.json"),
  title = "rtemis profile conformance fixture",
  description = paste0(
    "One Parquet file per case and the `profile/r/v1` document rtemis measures ",
    "for it. Every implementation of the profile reads the same file and must ",
    "produce the same document. Parquet because it declares its types: a ",
    "delimited file would make this a test of three type inferences rather ",
    "than of three profilers. Generated by rtemis; never edited downstream."
  ),
  rtemis_version = as.character(utils::packageVersion("rtemis")),
  profile_max_levels = PROFILE_MAX_LEVELS,
  cases = lapply(cases, measure)
)

json <- jsonlite::toJSON(
  doc,
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null",
  null = "null"
)


# %% Write ----

if (refresh) {
  # `inst/` carries the artifacts the package publishes: `data-raw/` is in
  # `.Rbuildignore`, and the test that reproduces the fixture reads them on
  # CRAN.
  writeLines(json, file.path(inst_dir, "fixture.json"))
  cat(sprintf(
    "profile fixture  %d cases -> %s\n",
    length(cases),
    file.path(inst_dir, "fixture.json")
  ))
} else {
  dir.create(
    file.path(out_dir, rel_dir),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(json, file.path(out_dir, "profile", "v1", "fixture.json"))
  for (case in cases) {
    file.copy(
      shipped_path(case),
      file.path(out_dir, rel_dir, case[["file"]]),
      overwrite = TRUE
    )
  }
  cat(sprintf(
    "profile fixture  %d cases + %d parquet -> %s\n",
    length(cases),
    length(cases),
    file.path(out_dir, "profile", "v1", "fixture.json")
  ))
}
