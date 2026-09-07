test_that("the file format preserves typed values without R deserialization", {
  values <- list(
    integers = c(one = 1L, missing = NA_integer_),
    doubles = c(pi, 1 / 3, NA_real_, NaN, Inf, -Inf, -0),
    logicals = c(TRUE, FALSE, NA),
    strings = c("NA", NA_character_, "", "caf\u00e9", "\u6c34"),
    bytes = as.raw(0:255),
    complex = c(1 + 2i, NA_complex_),
    factor = factor(
      c("low", NA, "high"),
      levels = c("low", "high"),
      ordered = TRUE
    ),
    date = as.Date(c("2026-09-06", NA)),
    time = as.POSIXct(c("2026-09-06 12:34:56", NA), tz = "UTC"),
    matrix = matrix(1:4, 2),
    table = data.frame(x = 1:2, y = c("a", "b")),
    empty = list(integer(), character(), list(), NULL)
  )
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "typed",
      source_type = "manual",
      metadata = list(values)
    ),
    NULL,
    NULL
  )
  saved <- investigation_snapshot(bundle)
  path <- tempfile()
  write_investigation(saved, path)
  restored <- read_investigation(path)
  expect_identical(trajectory_info(restored$bundle)$metadata[[1]], values)
  expect_identical(restored, saved)
})

test_that("reading uses saved analysis even when scanners are unavailable", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  path <- tempfile()
  write_investigation(saved, path)
  local_mocked_bindings(
    assess_trajectory_scans = function(...) stop("Must not scan"),
    summarize_trajectories = function(...) stop("Must not summarize"),
    measure_trajectories = function(...) stop("Must not measure")
  )
  expect_identical(read_investigation(path), saved)
})

test_that("unsupported versions and edited evidence fail explicitly", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    application = "ORIGINAL_APPLICATION"
  )
  path <- tempfile()
  write_investigation(saved, path)
  envelope <- jsonlite::read_json(path)
  envelope$version <- 2L
  jsonlite::write_json(envelope, path, auto_unbox = TRUE)
  expect_snapshot(error = TRUE, read_investigation(path))
  write_investigation(saved, path, overwrite = TRUE)
  text <- readLines(path)
  writeLines(
    gsub("ORIGINAL_APPLICATION", "EDITED_APPLICATION", text, fixed = TRUE),
    path
  )
  expect_snapshot(error = TRUE, read_investigation(path))
})

test_that("the reader rejects executable types and the writer rejects custom objects", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  path <- tempfile()
  write_investigation(saved, path)
  envelope <- jsonlite::read_json(path)
  envelope$data$type <- "closure"
  jsonlite::write_json(envelope, path, auto_unbox = TRUE)
  expect_snapshot(error = TRUE, read_investigation(path))
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "custom",
      source_type = "manual",
      metadata = list(list(value = structure(1, class = "custom_object")))
    ),
    NULL,
    NULL
  )
  expect_snapshot(error = TRUE, investigation_snapshot(bundle))
})

test_that("size limits and overwrite defaults protect existing files", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  path <- tempfile()
  write_investigation(saved, path)
  before <- readBin(path, "raw", n = file.info(path)$size)
  expect_snapshot(error = TRUE, write_investigation(saved, path))
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  expect_snapshot(error = TRUE, read_investigation(path, max_bytes = 10))
  expect_identical(read_investigation(path, max_bytes = Inf), saved)
  writeLines("not JSON", path)
  expect_snapshot(error = TRUE, read_investigation(path))
})

test_that("opening saved scanner identities does not consult the installed registry", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    scans = "event_error",
    view = list(pattern = "event_error")
  )
  path <- tempfile()
  write_investigation(saved, path)
  local_mocked_bindings(scan_registry = function() {
    stop("Must not use the current registry")
  })
  expect_identical(read_investigation(path), saved)
})
