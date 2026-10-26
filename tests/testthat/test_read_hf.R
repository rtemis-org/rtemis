# test_read_hf.R
# ::rtemis::
# 2026- EDG rtemis.org

# library(testthat)

skip_if_not_installed("jsonlite")

.commit <- strrep("ab", 20L)
.csv_bytes <- charToRaw("a,b\n1,x\n2,y\n")

.git_blob_sha1 <- function(bytes) {
  as.vector(as.character(openssl::sha1(c(
    charToRaw(paste0("blob ", length(bytes))),
    as.raw(0L),
    bytes
  ))))
}

# A Hub that serves the revision API and `resolve/` downloads for `files`, a
# named list of raw contents keyed by repository path. Files named in `lfs` are
# listed with an LFS SHA-256, the others with a git blob SHA-1. `listed`
# overrides the hash the listing reports for a file. Returns an environment
# recording the URLs requested.
.mock_hub <- function(
  files,
  lfs = character(),
  gated = FALSE,
  listed = list(),
  env = parent.frame()
) {
  calls <- new.env(parent = emptyenv())
  calls[["urls"]] <- character()
  siblings <- lapply(names(files), function(f) {
    bytes <- files[[f]]
    s <- list(rfilename = f, size = length(bytes))
    if (f %in% lfs) {
      s[["blobId"]] <- strrep("0", 40L)
      s[["lfs"]] <- list(
        sha256 = listed[[f]] %||%
          as.vector(as.character(openssl::sha256(bytes))),
        size = length(bytes)
      )
    } else {
      s[["blobId"]] <- listed[[f]] %||% .git_blob_sha1(bytes)
    }
    s
  })
  testthat::local_mocked_bindings(
    download_file = function(url, destfile, quiet) {
      calls[["urls"]] <- c(calls[["urls"]], url)
      if (grepl("/api/datasets/", url, fixed = TRUE)) {
        jsonlite::write_json(
          list(sha = .commit, gated = gated, siblings = siblings),
          destfile,
          auto_unbox = TRUE
        )
      } else {
        f <- utils::URLdecode(sub("^.*/resolve/[0-9a-f]{40}/", "", url))
        writeBin(files[[f]], destfile)
      }
      0L
    },
    .env = env
  )
  calls
}


test_that("parse_hf() reads owner, name, revision, and file", {
  expect_identical(
    parse_hf("hf://datasets/rtemis/demo/data/x.parquet"),
    list(repo = "rtemis/demo", revision = "main", file = "data/x.parquet")
  )
  expect_identical(
    parse_hf("HF://datasets/rtemis/demo@refs%2Fconvert%2Fparquet/x.csv"),
    list(
      repo = "rtemis/demo",
      revision = "refs%2Fconvert%2Fparquet",
      file = "x.csv"
    )
  )
  expect_identical(
    parse_hf("hf://datasets/rtemis/demo@v1.0/"),
    list(repo = "rtemis/demo", revision = "v1.0", file = NULL)
  )
  expect_error(parse_hf("hf://rtemis/model/x.bin"), "datasets only")
  expect_error(parse_hf("hf://spaces/rtemis/app"), "datasets only")
  expect_error(parse_hf("hf://datasets/rtemis"), "Cannot parse")
  expect_error(parse_hf("hf://datasets/rtemis/demo/../x.csv"), "segment")
  expect_error(parse_hf("hf://datasets/rtemis/demo/a//x.csv"), "segment")
})


test_that("read() resolves a pinned hf:// path to a commit and verifies the git blob", {
  calls <- .mock_hub(list(
    README.md = charToRaw("# demo\n"),
    `data/small.csv` = .csv_bytes
  ))
  out <- read(
    "hf://datasets/rtemis/demo@v1/data/small.csv",
    verbosity = 0L,
    output = "data.frame"
  )
  expect_equal(out, data.frame(a = 1:2, b = c("x", "y")))
  expect_identical(
    calls[["urls"]],
    c(
      "https://huggingface.co/api/datasets/rtemis/demo/revision/v1?blobs=true",
      paste0(
        "https://huggingface.co/datasets/rtemis/demo/resolve/",
        .commit,
        "/data/small.csv"
      )
    )
  )
})


test_that("a repository-only hf:// path reads its one data file, verified by LFS SHA-256", {
  x <- data.frame(a = 1:3, b = c(0.5, 1.5, 2.5))
  src <- tempfile(fileext = ".rds")
  on.exit(unlink(src), add = TRUE)
  saveRDS(x, src)
  .mock_hub(
    list(
      .gitattributes = charToRaw("*.rds filter=lfs\n"),
      README.md = charToRaw("# demo\n"),
      `croissant.json` = charToRaw("{}"),
      `data/x.rds` = readBin(src, "raw", n = file.size(src))
    ),
    lfs = "data/x.rds"
  )
  out <- read(
    "hf://datasets/rtemis/demo",
    verbosity = 0L,
    output = "data.frame"
  )
  expect_equal(out, x)
})


test_that("a repository-only hf:// path with several data files names them", {
  .mock_hub(list(
    `train.csv` = .csv_bytes,
    `test.csv` = .csv_bytes
  ))
  expect_error(
    read("hf://datasets/rtemis/demo", verbosity = 0L),
    "has 2 data files: `train.csv`, `test.csv`. Name one",
    class = "rtemis_input_error"
  )
})


test_that("an hf:// file absent from the repository is rtemis_file_not_found", {
  .mock_hub(list(`data/small.csv` = .csv_bytes))
  expect_error(
    read("hf://datasets/rtemis/demo/data/other.csv", verbosity = 0L),
    "has no file `data/other.csv`. Its data files: `data/small.csv`",
    class = "rtemis_file_not_found"
  )
})


test_that("a gated dataset is refused before download", {
  calls <- .mock_hub(list(`x.csv` = .csv_bytes), gated = "auto")
  expect_error(
    read("hf://datasets/rtemis/demo/x.csv", verbosity = 0L),
    "is gated",
    class = "rtemis_unsupported_error"
  )
  expect_length(calls[["urls"]], 1L)
})


test_that("a downloaded file whose hash differs from the listing is refused", {
  .mock_hub(
    list(`x.csv` = .csv_bytes),
    lfs = "x.csv",
    listed = list(`x.csv` = strrep("0", 64L))
  )
  expect_error(
    read("hf://datasets/rtemis/demo/x.csv", verbosity = 0L),
    "has SHA-256",
    class = "rtemis_io_error"
  )
  .mock_hub(
    list(`x.csv` = .csv_bytes),
    listed = list(`x.csv` = strrep("0", 40L))
  )
  expect_error(
    read("hf://datasets/rtemis/demo/x.csv", verbosity = 0L),
    "has git blob SHA-1",
    class = "rtemis_io_error"
  )
})


test_that("HF_ENDPOINT sets the Hub endpoint", {
  old <- Sys.getenv("HF_ENDPOINT", unset = NA)
  on.exit(
    if (is.na(old)) {
      Sys.unsetenv("HF_ENDPOINT")
    } else {
      Sys.setenv(HF_ENDPOINT = old)
    },
    add = TRUE
  )
  Sys.setenv(HF_ENDPOINT = "https://hub.example.org/")
  calls <- .mock_hub(list(`x.csv` = .csv_bytes))
  read("hf://datasets/rtemis/demo/x.csv", verbosity = 0L)
  expect_true(all(startsWith(calls[["urls"]], "https://hub.example.org/")))
  Sys.setenv(HF_ENDPOINT = "hub.example.org")
  expect_error(hf_endpoint(), "HF_ENDPOINT", class = "rtemis_input_error")
})


test_that("ingest() records the resolved hf:// path and the input SHA-256", {
  skip_if_not_installed("arrow")
  .mock_hub(list(`data/small.csv` = .csv_bytes))
  out <- tempfile(fileext = ".parquet")
  on.exit(unlink(out), add = TRUE)
  m <- ingest("hf://datasets/rtemis/demo", out, verbosity = 0L)
  expect_identical(m[["input"]], "hf://datasets/rtemis/demo")
  expect_identical(
    m[["input_resolved"]],
    paste0("hf://datasets/rtemis/demo@", .commit, "/data/small.csv")
  )
  expect_identical(
    m[["input_sha256"]],
    as.vector(as.character(openssl::sha256(.csv_bytes)))
  )
  expect_match(m[["retrieved_at"]], "^\\d{4}-\\d{2}-\\d{2}T.*Z$")
  expect_identical(m[["format"]], "delimited")
})


test_that("resolve_hf() resolves a public rtemis dataset on the Hub", {
  skip_on_cran()
  skip_if_offline("huggingface.co")
  hf <- resolve_hf("hf://datasets/rtemis/tcga-pancan-hiseq", verbosity = 0L)
  expect_identical(hf[["file"]], "data/tcga-pancan-hiseq.parquet")
  expect_match(hf[["commit"]], "^[0-9a-f]{40}$")
  expect_match(hf[["lfs_sha256"]], "^[0-9a-f]{64}$")
})
