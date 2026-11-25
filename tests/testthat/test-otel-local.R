test_that("the commons trace fixture reads into an OTel trajectory", {
  skip_if_not_installed("jsonlite")
  conversations <- read_otel_traces(test_path("fixtures", "commons"))
  info <- attr(conversations, "read_info", exact = TRUE)

  expect_named(conversations, "conversation-001")
  expect_identical(info$files, 1L)
  expect_identical(info$conversations, 1L)
  expect_false(info$truncated)
  expect_match(attr(conversations, "source_uri"), "^file://.*/commons$")

  bundle <- as_trajectory_otel(conversations)
  events <- trajectory_events(bundle)
  expect_identical(
    events$text[events$event_type == "content"],
    c("What is revenue?", "Revenue excludes tax.")
  )
  # This older trace records provenance on the turn span itself.
  provenance <- events[events$event_type == "commons:provenance", ]
  expect_identical(provenance$value[[1L]]$provenance_tag, "B")
  expect_identical(
    provenance$value[[1L]]$citation_decisions[[1L]]$label,
    "Data dictionary"
  )
  expect_identical(
    read_otel_traces(test_path("fixtures", "commons", "trace-0.jsonl")),
    conversations,
    ignore_attr = TRUE
  )
})

test_that("a rotated set with a trace-latest duplicate reads each span once", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  fixture <- readLines(test_path("fixtures", "commons", "trace-0.jsonl"))
  later <- otel_test_conversation_line("later", 1787659300)
  writeLines(fixture, file.path(dir, "trace-0.jsonl"))
  writeLines(later, file.path(dir, "trace-1.jsonl"))
  writeLines(later, file.path(dir, "trace-latest.jsonl"))
  writeLines("not a trace", file.path(dir, "notes.jsonl"))

  conversations <- read_otel_traces(dir)
  info <- attr(conversations, "read_info", exact = TRUE)
  expect_named(conversations, c("conversation-001", "later"))
  expect_identical(info$files, 2L)
  expect_identical(info$spans_total, 3L)
  expect_identical(info$lines_dropped, 0L)
  expect_identical(nrow(trajectory_info(as_trajectory_otel(conversations))), 2L)

  # A copy taken mid-write differs from the file it copies, so its spans are
  # de-duplicated by identity rather than by file.
  writeLines(c(later, fixture), file.path(dir, "trace-latest.jsonl"))
  copied <- read_otel_traces(dir)
  expect_identical(attr(copied, "read_info")$files, 3L)
  expect_identical(attr(copied, "read_info")$spans_total, 3L)
  expect_identical(copied, conversations, ignore_attr = TRUE)

  unlink(file.path(dir, "trace-latest.jsonl"))
  linked <- file.symlink(
    file.path(dir, "trace-1.jsonl"),
    file.path(dir, "trace-latest.jsonl")
  )
  skip_if_not(linked, "Symbolic links are unavailable.")
  expect_identical(attr(read_otel_traces(dir), "read_info")$files, 2L)
})

test_that("several files are read together", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  writeLines(otel_test_conversation_line("a", 100), file.path(dir, "a.jsonl"))
  writeLines(otel_test_conversation_line("b", 200), file.path(dir, "b.jsonl"))

  conversations <- read_otel_traces(file.path(dir, c("b.jsonl", "a.jsonl")))
  expect_named(conversations, c("a", "b"))
  expect_null(attr(conversations, "source_uri"))
})

test_that("n, from, and to select conversations as the Connect reader does", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("httr2")
  dir <- withr::local_tempdir()
  lines <- c(
    otel_test_conversation_line("a", 100),
    otel_test_conversation_line("b", 200),
    otel_test_conversation_line("c", 300)
  )
  writeLines(lines, file.path(dir, "trace-0.jsonl"))
  testthat::local_mocked_bindings(
    connect_client = function(...) list(server = "https://connect.example.com"),
    connect_trace_lines = function(...) lines
  )
  guid <- "11111111-1111-4111-8111-111111111111"
  at <- function(seconds) as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC")
  cases <- list(
    list(n = NULL, from = NULL, to = NULL),
    list(n = 2L, from = NULL, to = NULL),
    list(n = NULL, from = at(150), to = NULL),
    list(n = NULL, from = NULL, to = at(250)),
    list(n = 1L, from = at(150), to = at(350))
  )
  for (case in cases) {
    local <- read_otel_traces(dir, n = case$n, from = case$from, to = case$to)
    connect <- read_connect_traces(
      guid,
      n = case$n,
      from = case$from,
      to = case$to
    )
    expect_identical(local, connect, ignore_attr = TRUE)
    keep <- c("from", "to", "n", "spans", "conversations_found")
    expect_identical(
      attr(local, "read_info")[keep],
      attr(connect, "read_info")[keep]
    )
  }
  expect_named(read_otel_traces(dir, n = 1L, to = at(250)), "b")
  expect_error(
    read_otel_traces(dir, from = "yesterday"),
    class = "scans_error_otel_window"
  )
})

test_that("the span budget keeps the most recent spans", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  writeLines(
    c(
      otel_test_conversation_line("a", 100),
      otel_test_conversation_line("b", 200),
      "{malformed"
    ),
    file.path(dir, "trace-0.jsonl")
  )

  expect_snapshot(conversations <- read_otel_traces(dir, max_spans = 1L))
  info <- attr(conversations, "read_info", exact = TRUE)
  expect_named(conversations, "b")
  expect_true(info$truncated)
  expect_true(info$incomplete)
  expect_identical(info$lines_dropped, 1L)
})

test_that("OTEL_EXPORTER_OTLP_TRACES_FILE locates and names the trace files", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  writeLines(
    otel_test_conversation_line("a", 100),
    file.path(dir, "spans-2026-08-03-0.jsonl")
  )
  writeLines(
    otel_test_conversation_line("b", 200),
    file.path(dir, "other.jsonl")
  )
  withr::local_envvar(
    OTEL_EXPORTER_OTLP_TRACES_FILE = file.path(dir, "spans-%F-%N.jsonl"),
    OTEL_EXPORTER_OTLP_FILE = NA
  )

  expect_named(read_otel_traces(), "a")

  withr::local_envvar(OTEL_EXPORTER_OTLP_TRACES_FILE = "stdout")
  expect_error(read_otel_traces(), class = "scans_error_otel_path")
})

test_that("missing paths and unreadable traces are reported", {
  skip_if_not_installed("jsonlite")
  withr::local_envvar(
    OTEL_EXPORTER_OTLP_TRACES_FILE = NA,
    OTEL_EXPORTER_OTLP_FILE = NA
  )
  dir <- withr::local_tempdir()
  empty <- file.path(dir, "trace-0.jsonl")
  writeLines("{malformed", empty)
  dir.create(file.path(dir, "no-traces"))

  expect_snapshot(
    error = TRUE,
    transform = function(x) gsub("'[^']*/([^/']+)'", "'<tmp>/\\1'", x),
    {
      read_otel_traces()
      read_otel_traces(file.path(dir, "absent.jsonl"))
      read_otel_traces(file.path(dir, "no-traces"))
      read_otel_traces(empty)
    }
  )
  expect_error(read_otel_traces(NA_character_), class = "scans_error_otel_path")
})
