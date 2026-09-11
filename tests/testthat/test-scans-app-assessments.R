test_that("app uses the public assessment result and links findings to evidence", {
  bundle <- scan_loop_fixture()
  result <- assess_trajectory_scans(bundle)
  data <- scans_app_data(bundle)
  expect_identical(data$assessments, result$assessments)
  expect_identical(data$findings, result$findings)
  html <- as.character(scans_app_evidence_ui(data, 1L))
  expect_match(html, "Evidence coverage", fixed = TRUE)
  expect_match(html, "Assessed: findings", fixed = TRUE)
  expect_match(html, "Insufficient evidence", fixed = TRUE)
  expect_match(html, "Scanner version", fixed = TRUE)
  expect_match(html, "repeat_threshold", fixed = TRUE)
  expect_match(html, 'href="#scans-app-finding-1"', fixed = TRUE)
  expect_match(html, 'id="scans-app-finding-1"', fixed = TRUE)
  expect_match(
    html,
    paste0('href="#', scans_app_event_dom_id(data$events$event_id[[1L]]), '"'),
    fixed = TRUE
  )
})

test_that("app never describes missing evidence or execution failure as clean", {
  bare <- as_trajectory_otel(list(otel_chat_span()))
  data <- scans_app_data(bare)
  html <- as.character(scans_app_evidence_ui(data, 1L))
  expect_match(html, "Insufficient evidence", fixed = TRUE)
  expect_no_match(html, "found no diagnostic issues", fixed = TRUE)
  local_mocked_bindings(scan_tool_findings = function(...) {
    stop("PRIVATE condition")
  })
  data <- scans_app_data(scan_loop_fixture())
  html <- as.character(scans_app_evidence_ui(data, 1L))
  expect_match(html, "Execution failure", fixed = TRUE)
  expect_no_match(html, "PRIVATE", fixed = TRUE)
})

test_that("coverage uses the filtered cohort and scanner denominator", {
  bundle <- as_trajectory_otel(list(
    otel_chat_span(conversation = "bare"),
    otel_chat_span(
      "chat-2",
      conversation = "text",
      input = list(list(role = "user", parts = text_part("Hello")))
    )
  ))
  data <- scans_app_data(bundle)
  coverage <- scans_app_performance_data(data, 1:2)$assessment_coverage
  tool <- coverage[coverage$scan == "repeated_tool_call", ]
  expect_equal(tool$trajectories, c(0L, 0L, 1L, 1L, 0L))
  filtered <- scans_app_performance_data(
    data,
    which(data$info$trajectory_id == "otel/bare")
  )$assessment_coverage
  tool <- filtered[filtered$scan == "repeated_tool_call", ]
  expect_equal(tool$trajectories, c(0L, 0L, 1L, 0L, 0L))
  expect_identical(
    nrow(scans_app_performance_data(data, integer())$assessment_coverage),
    0L
  )
})

test_that("settings and selections recompute assessments without reloading", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  calls <- 0L
  app <- scans_app(list(Example = function() {
    calls <<- calls + 1L
    scan_loop_fixture()
  }))
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = "repeated_tool_call")
    expect_equal(
      performance()$assessment_coverage$trajectories,
      c(0L, 1L, 0L, 0L, 0L)
    )
    session$setInputs(scans_app_repeat_threshold = 4L)
    expect_equal(
      performance()$assessment_coverage$trajectories,
      c(1L, 0L, 0L, 0L, 0L)
    )
    session$setInputs(scans_app_scans = character())
    expect_identical(nrow(performance()$assessment_coverage), 0L)
    expect_identical(calls, 1L)
  })
})
