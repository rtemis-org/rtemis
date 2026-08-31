# test_SessionSidecar.R
# Tests for session_nodes(), DataRef, and the sidecar write_record() writes.

t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

make_event <- function(
  id,
  parent,
  kind,
  label = NULL,
  status = "ok",
  start_s = 0,
  end_s = 1,
  meta = list()
) {
  list(
    node_id = id,
    parent_id = parent,
    kind = kind,
    label = label,
    status = status,
    t_start = t0 + start_s,
    t_end = if (is.na(end_s)) as.POSIXct(NA) else t0 + end_s,
    meta = meta
  )
}

make_session <- function(events) {
  SupervisedSession(
    id = "s1",
    events = events,
    started = t0,
    finished = t0 + 10
  )
}


# %% session_nodes ----

test_that("session_nodes() returns one row per node with the durable fields", {
  s <- make_session(list(
    make_event("n1", NA_character_, "train", "LightRF"),
    make_event("n2", "n1", "outer_fold", "1/2", meta = list(fold = 1L))
  ))
  nodes <- session_nodes(s)
  expect_s3_class(nodes, "data.frame")
  expect_identical(nrow(nodes), 2L)
  expect_identical(
    names(nodes),
    c(
      "node_id",
      "parent_id",
      "kind",
      "label",
      "status",
      "t_start",
      "t_end",
      "meta"
    )
  )
  # Absolute timestamps, not offsets: the display projection is
  # session_timeline()'s job, and a stored table should not need to know when
  # the session it came from started.
  expect_s3_class(nodes[["t_start"]], "POSIXct")
  expect_identical(nodes[["t_start"]][[1L]], t0)
})


test_that("session_nodes() carries a root's absent parent as NA", {
  s <- make_session(list(make_event("n1", NA_character_, "train")))
  expect_true(is.na(session_nodes(s)[["parent_id"]][[1L]]))
})


test_that("session_nodes() writes meta as JSON, and NA when there is none", {
  s <- make_session(list(
    make_event("n1", NA_character_, "train"),
    make_event("n2", "n1", "grid_cell", meta = list(resample_id = 3L))
  ))
  nodes <- session_nodes(s)
  expect_true(is.na(nodes[["meta"]][[1L]]))
  # A string rather than flattened columns: the table's shape must not depend
  # on which node kinds a run happened to produce.
  expect_identical(nodes[["meta"]][[2L]], "{\"resample_id\":3}")
})


test_that("session_nodes() of an empty session is an empty table, not an error", {
  nodes <- session_nodes(make_session(list()))
  expect_identical(nrow(nodes), 0L)
  expect_identical(ncol(nodes), 8L)
})


# %% DataRef ----

test_that("DataRef requires a path and a hash", {
  expect_error(DataRef(path = "", hash = "abc"), "@path")
  expect_error(DataRef(path = "x.parquet", hash = ""), "@hash")
})


test_that("DataRef rejects an encoding it does not name", {
  expect_error(
    DataRef(path = "x.feather", hash = "abc", encoding = "feather")
  )
})


# %% the sidecar ----

test_that("write_record() writes the session beside the record and links it", {
  dir <- withr::local_tempdir()
  x <- train(
    iris[iris[["Species"]] != "setosa", ],
    hyperparameters = setup_CART(),
    verbosity = 0L
  )
  file <- file.path(dir, "train_CART.record.json")
  write_record(x, file, verbosity = 0L)

  sidecar <- file.path(dir, "train_CART.session.parquet")
  expect_true(file.exists(sidecar))

  rec <- jsonlite::fromJSON(file)
  # Relative, so a study is a directory that can be moved without rewriting
  # what it says about itself.
  expect_identical(rec[["session"]][["path"]], "train_CART.session.parquet")
  expect_identical(rec[["session"]][["encoding"]], "parquet")
  # The digest is over the bytes: one engine writes a session, so there is no
  # second writer to agree with a canonical form.
  expect_identical(
    rec[["session"]][["hash"]],
    rtemis:::.hash_file(sidecar, "sha256")
  )
  expect_identical(
    rec[["session"]][["n_rows"]],
    nrow(nanoparquet::read_parquet(sidecar))
  )
})


test_that("the sidecar round-trips the nodes the session holds", {
  dir <- withr::local_tempdir()
  x <- train(
    iris[iris[["Species"]] != "setosa", ],
    hyperparameters = setup_CART(),
    verbosity = 0L
  )
  file <- file.path(dir, "train_CART.record.json")
  write_record(x, file, verbosity = 0L)
  back <- as.data.frame(
    nanoparquet::read_parquet(file.path(dir, "train_CART.session.parquet"))
  )
  expect_identical(nrow(back), length(x@session@events))
  expect_true(all(back[["node_id"]] == session_nodes(x@session)[["node_id"]]))
})


test_that("a record built but not written states a null session", {
  x <- train(
    iris[iris[["Species"]] != "setosa", ],
    hyperparameters = setup_CART(),
    verbosity = 0L
  )
  # Present and null: the field exists whether or not there is a file to point
  # at, so a reader never has to distinguish "absent" from "none".
  rec <- record(x)
  expect_true("session" %in% names(rec))
  expect_null(rec[["session"]])
})


test_that("write_record() refuses to overwrite a sidecar unless told to", {
  dir <- withr::local_tempdir()
  x <- train(
    iris[iris[["Species"]] != "setosa", ],
    hyperparameters = setup_CART(),
    verbosity = 0L
  )
  file <- file.path(dir, "train_CART.record.json")
  write_record(x, file, verbosity = 0L)
  expect_error(write_record(x, file, verbosity = 0L), "overwrite")
})


test_that("the sidecar is named after the record, whatever the record is called", {
  # `.record.json` is the convention, but a record written to any other name
  # should still yield `<name>.session.parquet` rather than one that carries a
  # stray `.json` in the middle.
  expect_identical(
    rtemis:::session_sidecar_path("out/train_CART.record.json"),
    "out/train_CART.session.parquet"
  )
  expect_identical(
    rtemis:::session_sidecar_path("out/supervisedRecord.json"),
    "out/supervisedRecord.session.parquet"
  )
})


test_that("a result carrying no session writes no sidecar", {
  dir <- withr::local_tempdir()
  x <- decomp(iris[, 1:4], algorithm = "PCA", verbosity = 0L)
  file <- file.path(dir, "decomp_PCA.record.json")
  write_record(x, file, verbosity = 0L)
  expect_false(file.exists(file.path(dir, "decomp_PCA.session.parquet")))
  # A pipeline record has no `session` field at all -- only a supervised record
  # declares one, because only a supervised run records a graph.
  expect_false("session" %in% names(jsonlite::fromJSON(file)))
})
