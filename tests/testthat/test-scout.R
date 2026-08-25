test_that("scout() creates a read-only Shiny app", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- trajectory_fixture("simple_exchange")
  app <- scout(bundle)

  expect_s3_class(app, "shiny.appobj")
  expect_identical(trajectory_events(bundle)$text, c("Hello", "Hi there"))
})

test_that("Scout records retain source identity and deterministic findings", {
  data <- scout_data(trajectory_fixture("tool_error"))

  expect_identical(data$records$trajectory_id, "trajectory-error")
  expect_identical(data$records$title, "Find the record")
  expect_identical(data$records$n_findings, 2L)
  expect_identical(data$records$n_errors, 2L)
  expect_setequal(data$findings$event_id, c("error-event-3", "error-event-4"))
})

test_that("Scout filters compose without interpreting the query as a regex", {
  records <- scout_data(trajectory_fixture("ellmerverse_correlation"))$records
  records$n_findings[[2L]] <- 1L

  expect_identical(
    scout_filter_records(records, source = "deputy"),
    2L
  )
  expect_identical(
    scout_filter_records(records, query = "research-module"),
    3L
  )
  expect_identical(
    scout_filter_records(records, query = "[", findings_only = TRUE),
    integer()
  )
  expect_identical(
    scout_filter_records(records, findings_only = TRUE),
    2L
  )
})

test_that("Scout keeps selection and empty filter states reactive", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  data <- scout_data(trajectory_fixture("delegated_agent"))
  shiny::testServer(scout_server(data), {
    session$setInputs(
      scout_source = "all",
      scout_status = "all",
      scout_query = "child",
      scout_findings_only = FALSE
    )
    session$flushReact()

    expect_identical(output$scout_visible_count, "1 of 2 trajectories")
    expect_match(as.character(output$scout_entries)[[1L]], "trajectory-child")

    session$setInputs(scout_query = "does not exist")
    session$flushReact()

    expect_identical(output$scout_visible_count, "0 of 2 trajectories")
    expect_match(
      as.character(output$scout_entries)[[1L]],
      "No trajectories match these filters.",
      fixed = TRUE
    )
  })
})

test_that("Scout renders canonical text as text and links finding evidence", {
  skip_if_not_installed("htmltools")

  tables <- fixture_source(trajectory_fixture("tool_error"))
  tables$events$text[[1L]] <- "<script>alert('not markup')</script>"
  data <- scout_data(do.call(TrajectoryBundle, tables))
  transcript <- htmltools::renderTags(scout_transcript_ui(data, 1L))$html
  evidence <- htmltools::renderTags(scout_evidence_ui(data, 1L))$html

  expect_match(transcript, "&lt;script&gt;", fixed = TRUE)
  expect_no_match(transcript, "<script>", fixed = TRUE)
  expect_match(evidence, "#scout-event-3", fixed = TRUE)
  expect_match(evidence, "error-event-3", fixed = TRUE)
})

test_that("Scout supports event-only and empty bundles", {
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
  data <- scout_data(event_only)
  html <- htmltools::renderTags(scout_transcript_ui(data, 1L))$html

  expect_match(html, "Event stream", fixed = TRUE)
  expect_match(html, "Tempest stage", fixed = TRUE)

  empty <- scout_data(TrajectoryBundle(
    data.frame(),
    data.frame(),
    data.frame()
  ))
  expect_identical(nrow(empty$records), 0L)
  expect_identical(scout_filter_records(empty$records), integer())
})
