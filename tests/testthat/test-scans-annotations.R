test_that("an annotation round-trips through the store", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".jsonl")
  store <- scans_annotations(path = path)

  store$append("trajectory-1", label = "follow up", note = "Tool loop here.")
  records <- store$read("trajectory-1")

  expect_equal(nrow(records), 1L)
  expect_equal(records$label, "follow up")
  expect_equal(records$note, "Tool loop here.")
  expect_true(nzchar(records$author))
  expect_s3_class(records$created_at, "POSIXct")
})

test_that("the store is append-only and newest-first", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".jsonl")
  store <- scans_annotations(path = path)

  store$append("trajectory-1", label = "looks right", note = "First pass.")
  Sys.sleep(0.01)
  store$append("trajectory-1", label = "wrong answer", note = "On reflection.")

  records <- store$read("trajectory-1")
  expect_equal(nrow(records), 2L)
  # The earlier judgement is still there, and the later one leads.
  expect_equal(records$label, c("wrong answer", "looks right"))
  expect_length(readLines(path), 2L)
})

test_that("reads are scoped to one trajectory", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".jsonl")
  store <- scans_annotations(path = path)
  store$append("trajectory-1", label = "looks right", note = NULL)
  store$append("trajectory-2", label = "follow up", note = NULL)

  expect_equal(nrow(store$read("trajectory-1")), 1L)
  expect_equal(nrow(store$read()), 2L)
  expect_equal(nrow(store$read("missing")), 0L)
})

test_that("an empty annotation is refused", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".jsonl")
  store <- scans_annotations(path = path)

  expect_error(
    store$append("trajectory-1", label = NULL, note = NULL),
    class = "scans_error_annotation_record"
  )
  expect_error(
    store$append("", label = "looks right", note = "x"),
    class = "scans_error_annotation_record"
  )
  expect_false(file.exists(path))
})

test_that("a label or a note alone is enough", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".jsonl")
  store <- scans_annotations(path = path)

  store$append("trajectory-1", label = "looks right", note = NULL)
  store$append("trajectory-2", label = NULL, note = "Just a note.")
  records <- store$read()

  expect_equal(nrow(records), 2L)
  expect_true(anyNA(records$note))
  expect_true(anyNA(records$label))
})

test_that("a malformed line does not hide the annotations around it", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".jsonl")
  store <- scans_annotations(path = path)
  store$append("trajectory-1", label = "looks right", note = "Good.")
  cat("{not json at all\n", file = path, append = TRUE)
  store$append("trajectory-1", label = "follow up", note = "Also good.")

  records <- store$read("trajectory-1")
  expect_equal(nrow(records), 2L)
})

test_that("reading a store that does not exist yet returns no rows", {
  skip_if_not_installed("jsonlite")
  store <- scans_annotations(path = file.path(tempdir(), "absent.jsonl"))
  expect_equal(nrow(store$read()), 0L)
})

test_that("a long note is truncated rather than refused", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".jsonl")
  store <- scans_annotations(path = path)
  store$append("trajectory-1", label = NULL, note = strrep("x", 9000L))

  records <- store$read("trajectory-1")
  expect_equal(nchar(records$note), 4000L)
  expect_length(readLines(path), 1L)
})

test_that("labels are validated when the store is created", {
  skip_if_not_installed("jsonlite")
  expect_error(
    scans_annotations(path = tempfile(), labels = character()),
    class = "scans_error_annotation_labels"
  )
})

test_that("the app rejects an annotations argument that is not a store", {
  expect_error(
    scans_app_check_annotations("some/path.jsonl"),
    class = "scans_error_app_annotations"
  )
  expect_silent(scans_app_check_annotations(NULL))
})
