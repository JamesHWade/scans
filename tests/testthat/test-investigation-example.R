test_that("the installed offline investigation reproduces its documented conclusions", {
  skip_if_not_installed("jsonlite")
  path <- system.file(
    "extdata",
    "support-investigation.json",
    package = "scans"
  )
  bundle <- as_trajectory_otel(jsonlite::read_json(path))
  measures <- measure_trajectories(bundle)
  findings <- scan_trajectories(bundle)
  expect_equal(unique(findings$trajectory_id), "otel/retry")
  expect_equal(sum(findings$scan == "event_error"), 3L)
  loop <- findings[findings$scan == "suspicious_tool_loop", ]
  events <- trajectory_events(bundle)
  evidence <- events[events$event_id %in% loop$event_ids[[1L]], ]
  expect_equal(nrow(evidence), 3L)
  expect_equal(unique(evidence$error), "order_service_timeout")
  timing <- measures[
    measures$measure %in% c("elapsed", "recorded_work", "unattributed_elapsed"),
  ]
  expect_equal(
    timing$value[timing$trajectory_id == "otel/follow-up"],
    c(126, 6, 120)
  )
  expect_equal(
    timing$value[timing$trajectory_id == "otel/parallel"],
    c(10, 10, 0)
  )
  expect_equal(
    timing$value[timing$trajectory_id == "otel/limited-capture"],
    c(NA, 4, NA)
  )
  expect_equal(
    resource_row(bundle, "input_tokens")$value,
    c(1000, 240, 350, 75)
  )
  expect_equal(summarize_trajectories(bundle)$input_tokens[[1L]], 400)

  data <- scans_app_data(bundle)
  expect_identical(data$measures, measures)
  overview <- scans_app_performance_data(data, 1:4)
  expect_equal(overview$n_work, 3L)
  expect_equal(overview$n_work_partial, 1L)
  expect_equal(overview$median_work, 10)
  expect_equal(overview$n_timed, 3L)
  filtered <- scans_app_performance_data(data, 2L)
  expect_equal(filtered$median_work, 6)
  expect_equal(filtered$median_gap, 120)

  skip_if_not_installed("htmltools")
  html <- as.character(scans_app_resources_ui(data, 4L))
  expect_match(html, "partial; 1 / 2 records", fixed = TRUE)
  expect_match(
    html,
    "No supported per-request first-token measurement.",
    fixed = TRUE
  )
  expect_match(html, "as_trajectory_otel/1", fixed = TRUE)
})

test_that("resource details follow exact trajectory selection in the running app", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("shiny", "1.11.1")
  skip_if_not_installed("bslib", "0.11.0")
  path <- system.file(
    "extdata",
    "support-investigation.json",
    package = "scans"
  )
  app <- scans_app(as_trajectory_otel(jsonlite::read_json(path)))
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_inspect = "otel/limited-capture")
    expect_match(
      output$scans_app_resources$html,
      "partial; 1 / 2 records",
      fixed = TRUE
    )
    session$setInputs(scans_app_inspect = "otel/parallel")
    expect_match(
      output$scans_app_resources$html,
      "complete; 4 / 4 records",
      fixed = TRUE
    )
  })
})
