test_that("review controls are opt-in and decisions export selected content", {
  withr::local_options(sass.cache = FALSE)
  bundle <- investigation_bundle_fixture()
  html <- as.character(scans_app_ui(scans_app_sources(bundle)))
  expect_no_match(html, "scans_app_record_review", fixed = TRUE)
  app <- scans_app(bundle, reviews = TRUE)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_inspect = "otel/retry")
    session$setInputs(
      scans_app_review_finding_id = "",
      scans_app_review_scan = "",
      scans_app_review_author = "Reviewer",
      scans_app_review_judgment = "confirmed",
      scans_app_review_rationale = "Repeated retries",
      scans_app_review_expectation = "Bound retries",
      scans_app_review_adjudicate = FALSE,
      scans_app_record_review = 1L
    )
    expect_identical(review_status(review_state$current())$status, "confirmed")
    session$setInputs(
      scans_app_example_purpose = "application",
      scans_app_example_group = "order-1",
      scans_app_example_split = "development",
      scans_app_example_input_name = "question",
      scans_app_example_input = "Track my order",
      scans_app_example_target_name = "answer",
      scans_app_example_target = "Service unavailable",
      scans_app_preview_example = 1L
    )
    example <- review_state$pending()
    expect_identical(example$cases[[1L]]$trajectory_id, "otel/retry")
    downloaded <- output$scans_app_download_examples
    expect_identical(read_review_examples(downloaded), example)
    session$setInputs(scans_app_inspect = "otel/parallel")
    expect_null(review_state$current())
    expect_null(review_state$pending())
    session$setInputs(scans_app_inspect = "otel/retry")
    session$setInputs(scans_app_record_review = 2L)
    expect_length(review_state$current()$decisions, 2L)
    expect_identical(review_status(review_state$current())$status, "disputed")
    session$setInputs(
      scans_app_open_examples = data.frame(datapath = downloaded)
    )
    session$setInputs(
      scans_app_imported_case = "1",
      scans_app_inspect_imported_case = 1L
    )
    session$flushReact()
    investigation_ack_inputs(session)
    session$flushReact()
    expect_identical(
      review_state$current()$case_id,
      example$cases[[1L]]$case_id
    )
    expect_identical(
      review_state$current()$decisions[[1L]],
      example$cases[[1L]]$decisions[[1L]]
    )
    expect_identical(data()$info$trajectory_id, "otel/retry")
    session$setInputs(
      scans_app_review_finding_id = "",
      scans_app_review_scan = "",
      scans_app_review_judgment = "rejected",
      scans_app_review_adjudicate = TRUE,
      scans_app_record_review = 3L
    )
    expect_length(review_state$current()$decisions, 3L)
    expect_identical(review_status(review_state$current())$status, "rejected")
    session$setInputs(scans_app_close_examples = 1L)
    expect_null(review_state$current())
    expect_null(opened_investigation())
    expect_equal(nrow(data()$info), 4L)
  })
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    expect_null(review_state$current())
    expect_length(review_state$cases(), 0L)
  })
})

test_that("Connect review controls are optional without reading traces", {
  local_mocked_bindings(scans_app = function(
    x,
    annotations,
    investigations,
    reviews,
    chat_factory
  ) {
    reviews
  })
  source <- c(Deployment = "11111111-1111-4111-8111-111111111111")
  expect_false(scans_app_connect(source))
  expect_true(scans_app_connect(source, reviews = TRUE))
  expect_error(scans_app_connect(source, reviews = NA), class = "rlang_error")
})


test_that("importing a divergent review never replaces the current history", {
  case <- review_case(
    investigation_snapshot(investigation_bundle_fixture()),
    "otel/retry"
  )
  a <- review_decision(case, "confirmed", "A", "Repeated calls")
  b <- review_decision(case, "rejected", "B", "Different inputs")
  expect_error(scans_app_review_reconcile(a, b), class = "scans_error_review")
  expect_identical(scans_app_review_reconcile(a, case), a)
  expect_identical(scans_app_review_reconcile(case, a), a)
})
