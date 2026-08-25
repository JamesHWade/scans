test_that("scans_app() creates a read-only Shiny app", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- trajectory_fixture("simple_exchange")
  app <- scans_app(bundle)

  expect_s3_class(app, "shiny.appobj")
  expect_identical(trajectory_events(bundle)$text, c("Hello", "Hi there"))
})

test_that("scans app records retain source identity and deterministic findings", {
  data <- scans_app_data(trajectory_fixture("tool_error"))

  expect_identical(data$records$trajectory_id, "trajectory-error")
  expect_identical(data$records$title, "Find the record")
  expect_identical(data$records$n_findings, 2L)
  expect_identical(data$records$n_errors, 2L)
  expect_setequal(data$findings$event_id, c("error-event-3", "error-event-4"))
})

test_that("scans app filters compose without interpreting the query as a regex", {
  records <- scans_app_data(trajectory_fixture(
    "ellmerverse_correlation"
  ))$records
  records$n_findings[[2L]] <- 1L

  expect_identical(
    scans_app_filter_records(records, source = "deputy"),
    2L
  )
  expect_identical(
    scans_app_filter_records(records, query = "research-module"),
    3L
  )
  expect_identical(
    scans_app_filter_records(records, query = "[", findings_only = TRUE),
    integer()
  )
  expect_identical(
    scans_app_filter_records(records, findings_only = TRUE),
    2L
  )
})

test_that("scans app search covers the entire transcript", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  tables <- fixture_source(trajectory_fixture("simple_exchange"))
  tables$events$text[[2L]] <- paste(
    strrep("x", 100L),
    "late transcript token"
  )
  app <- scans_app(do.call(TrajectoryBundle, tables))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_query = "late transcript token",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "1 of 1 trajectory")
  })
})

test_that("scans app search ignores missing metadata", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "plain",
      source_type = "manual"
    ),
    data.frame(),
    data.frame()
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_query = "na",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "0 of 1 trajectory")
  })
})

test_that("scans app distinguishes a missing status from an unknown status", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("literal-missing", "unknown-status"),
      source_type = "manual",
      status = c("missing", NA_character_)
    ),
    data.frame(),
    data.frame()
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_status = "missing",
      scans_app_query = "",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "literal-missing", fixed = TRUE)
    expect_no_match(entries, "unknown-status", fixed = TRUE)
  })
})

test_that("scans app distinguishes literal all values from All filters", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("literal-all", "other-value"),
      source_type = c("all", "manual"),
      status = c("all", "complete")
    ),
    data.frame(),
    data.frame()
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_source = "all",
      scans_app_status = "all",
      scans_app_query = "",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "literal-all", fixed = TRUE)
    expect_no_match(entries, "other-value", fixed = TRUE)
  })
})

test_that("scans app keeps selection and empty filter states reactive", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  data <- scans_app_data(trajectory_fixture("delegated_agent"))
  shiny::testServer(scans_app_server(data), {
    session$setInputs(
      scans_app_query = "child",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "1 of 2 trajectories")
    expect_match(
      as.character(output$scans_app_entries)[[1L]],
      "trajectory-child"
    )

    session$setInputs(scans_app_query = "does not exist")
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "0 of 2 trajectories")
    expect_match(
      as.character(output$scans_app_entries)[[1L]],
      "No trajectories match these filters.",
      fixed = TRUE
    )
  })
})

test_that("scans app renders canonical text as text and links finding evidence", {
  skip_if_not_installed("htmltools")

  tables <- fixture_source(trajectory_fixture("tool_error"))
  tables$events$text[[1L]] <- "<script>alert('not markup')</script>"
  data <- scans_app_data(do.call(TrajectoryBundle, tables))
  transcript <- htmltools::renderTags(scans_app_transcript_ui(data, 1L))$html
  evidence <- htmltools::renderTags(scans_app_evidence_ui(data, 1L))$html

  expect_match(transcript, "&lt;script&gt;", fixed = TRUE)
  expect_no_match(transcript, "<script>", fixed = TRUE)
  expect_match(evidence, "#scans-app-event-3", fixed = TRUE)
  expect_match(evidence, "error-event-3", fixed = TRUE)
})

test_that("scans app renders transcript blocks in canonical event order", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "ordered-events",
      source_type = "manual"
    ),
    tibble::tibble(
      trajectory_id = "ordered-events",
      turn_id = "turn-1",
      turn_index = 1L,
      role = "assistant"
    ),
    tibble::tibble(
      trajectory_id = "ordered-events",
      event_id = c("turn-event", "run-event"),
      event_index = 1:2,
      event_type = "content",
      content_type = "text",
      turn_id = c("turn-1", NA_character_),
      text = c("Turn event first", "Run event second")
    )
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    transcript <- as.character(output$scans_app_transcript)[[1L]]

    expect_lt(
      regexpr("Turn event first", transcript, fixed = TRUE)[[1L]],
      regexpr("Run event second", transcript, fixed = TRUE)[[1L]]
    )
  })
})

test_that("scans app supports event-only and empty bundles", {
  skip_if_not_installed("htmltools")

  event_only <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "product-1",
      source_type = "tempest"
    ),
    data.frame(),
    tibble::tibble(
      trajectory_id = "product-1",
      event_id = "stage-1",
      event_index = 1L,
      event_type = "tempest:stage",
      name = "outline"
    )
  )
  data <- scans_app_data(event_only)
  html <- htmltools::renderTags(scans_app_transcript_ui(data, 1L))$html

  expect_match(html, "Event stream", fixed = TRUE)
  expect_match(html, "Tempest stage", fixed = TRUE)

  empty <- scans_app_data(TrajectoryBundle(
    data.frame(),
    data.frame(),
    data.frame()
  ))
  expect_identical(nrow(empty$records), 0L)
  expect_identical(scans_app_filter_records(empty$records), integer())
})
