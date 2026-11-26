test_that("select inputs and saved views name the same filters", {
  data <- scans_app_data(performance_fixture())
  choices <- scans_app_filter_choices(data)
  unknown <- scans_app_filter_sentinel(data$records$status, "status-unknown")

  all <- scans_app_input_view(data, choices$source_all, choices$status_all)
  expect_null(all$source_type)
  expect_null(all$status)
  expect_identical(scans_app_view_indices(data, all), 1:3)

  # A value the snapshot no longer offers selects everything, as it does
  # when a select input still holds a previous application's choice.
  stale <- scans_app_input_view(data, "otel", "cancelled")
  expect_null(stale$source_type)
  expect_null(stale$status)

  missing <- scans_app_input_view(data, "manual", unknown, query = " SLOW ")
  expect_identical(missing$source_type, "manual")
  expect_identical(missing$status, NA_character_)
  expect_identical(scans_app_view_status_input(data, missing$status), unknown)
  expect_identical(scans_app_view_indices(data, missing), integer())
  missing$query <- ""
  expect_identical(scans_app_view_indices(data, missing), 3L)

  failed <- scans_app_input_view(
    data,
    status = "failed",
    findings_only = TRUE,
    pattern = "event_error"
  )
  expect_identical(scans_app_view_indices(data, failed), 2L)
  expect_identical(
    scans_app_scope_view(scans_app_parse_scope(sub(
      "^.*\\?scope=",
      "",
      scans_app_scope_hash("App", scope = scans_app_view_scope(failed))
    ))),
    failed
  )
  expect_identical(
    scans_app_scope_view(scans_app_view_scope(missing))[names(missing)],
    missing
  )
})

test_that("the review filter keeps reviewed or labelled rows only with a queue", {
  data <- cohort_fixture_data()
  queue <- cohort_review_queue(
    data,
    data$info$trajectory_id,
    cohort_fixture_annotations()
  )
  expect_identical(
    scans_app_review_indices(1:5, list(state = "reviewed"), queue),
    2:3
  )
  expect_identical(
    scans_app_review_indices(1:5, list(state = "needs"), queue),
    c(1L, 4L, 5L)
  )
  expect_identical(
    scans_app_review_indices(1:5, list(label = "tool failure"), queue),
    3L
  )
  expect_identical(
    scans_app_review_indices(1:5, list(label = scans_app_no_label), queue),
    2L
  )
  expect_identical(
    scans_app_review_indices(1:5, list(state = "needs"), NULL),
    1:5
  )
})
