cohort_overview_html <- function(data, indices, scans = scan_registry()$scan) {
  scans_app_performance_ui(
    scans_app_performance_data(data, indices),
    "Cohort",
    "elapsed",
    scans
  )
}

test_that("tiles state each fact with its denominator", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  html <- as.character(cohort_overview_html(data, 1:5))

  expect_match(html, "of 5 captured", fixed = TRUE)
  expect_match(html, "of 4 assessed · 1 not assessed", fixed = TRUE)
  expect_match(html, "of 6 tool calls · in 2 conversations", fixed = TRUE)
  expect_match(html, "timed for 3 of 5", fixed = TRUE)
  expect_match(html, "recorded for 3 of 5", fixed = TRUE)
  expect_match(html, "5 of 5</strong>\\s+conversations")
  expect_match(html, "1 conversation has no recorded start time", fixed = TRUE)

  unknown <- as.character(cohort_overview_html(data, 5L))
  expect_match(unknown, "timed for 0 of 1", fixed = TRUE)
  expect_match(unknown, "No tool calls recorded", fixed = TRUE)
  expect_match(unknown, "activity over time is unknown", fixed = TRUE)
  expect_no_match(unknown, "NaN|Inf|>0 s<")

  off <- scans_app_data(
    cohort_fixture(),
    scans_app_scan_config(scans = character())
  )
  expect_match(
    as.character(cohort_overview_html(off, 1:5, scans = character())),
    "Scans off · no conversation was assessed",
    fixed = TRUE
  )
})

test_that("the activity chart has one labelled, focusable mark per bucket", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  performance <- scans_app_performance_data(data, 3:4, list(day = 1:5))
  chart <- htmltools::tagQuery(
    scans_app_activity_ui(performance$cohort$activity, "2026-10-03")
  )
  bars <- chart$find("button.scans-app-bar")$selectedTags()

  expect_length(bars, 3L)
  expect_identical(
    unname(vapply(bars, htmltools::tagGetAttribute, "", "data-scans-value")),
    c("2026-10-01", "2026-10-02", "2026-10-03")
  )
  expect_identical(
    unname(vapply(bars, htmltools::tagGetAttribute, "", "aria-pressed")),
    c("false", "false", "true")
  )
  expect_identical(
    htmltools::tagGetAttribute(bars[[3L]], "aria-label"),
    "Sat Oct 03: 2 conversations of 4. 1 error findings, 1 warnings only, 0 no findings, 0 not assessed."
  )
  expect_equal(chart$find("details tbody tr")$length(), 3L)
  expect_equal(chart$find("rect")$length(), 4L)

  one <- as.character(scans_app_activity_ui(
    cohort_activity(data, c("c", "d"), unit = "day")
  ))
  expect_match(
    one,
    "All 2 dated conversations started on Sat Oct 03 UTC: 1 with error findings, 1 with warnings only.",
    fixed = TRUE
  )
  expect_no_match(one, "<svg", fixed = TRUE)
})

test_that("tool and breakdown tables appear only when they inform", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  html <- as.character(cohort_overview_html(data, 1:5))
  expect_match(html, "<h2>Tools</h2>", fixed = TRUE)
  expect_match(html, "<h2>By model</h2>", fixed = TRUE)
  expect_match(html, "<h2>Most active users</h2>", fixed = TRUE)
  expect_match(
    html,
    "lookup: 1 of 4 calls failed, in 1 conversation",
    fixed = TRUE
  )
  expect_match(html, 'data-scans-facet="tool_failed"', fixed = TRUE)

  quiet <- as.character(cohort_overview_html(data, c(2L, 5L)))
  expect_no_match(quiet, "<h2>Tools</h2>", fixed = TRUE)
  expect_no_match(quiet, "Most active users", fixed = TRUE)
  expect_match(quiet, "<h2>By model</h2>", fixed = TRUE)

  one_model <- as.character(cohort_overview_html(data, 1:2))
  expect_match(one_model, "All 2 conversations used", fixed = TRUE)
  expect_no_match(one_model, 'data-scans-facet="model"', fixed = TRUE)

  anonymous <- as.character(cohort_overview_html(data, 5L))
  expect_no_match(anonymous, "By model|Most active users")
})

test_that("copy names conversations only for conversation sources", {
  skip_if_not_installed("htmltools")
  expect_identical(scans_app_noun(c("otel", "commons"))$many, "conversations")
  expect_identical(scans_app_noun(c("otel", "vitals"))$many, "trajectories")
  expect_identical(scans_app_noun(NA_character_)$one, "trajectory")

  data <- scans_app_data(performance_fixture())
  html <- as.character(cohort_overview_html(data, 1:3))
  expect_match(html, "3 of 3</strong>\\s+trajectories")
  expect_no_match(html, "onversation")
})

test_that("a faceted table keeps its sibling rows for comparison", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  performance <- scans_app_performance_data(
    data,
    c(1L, 4L),
    list(tool = 1:5, model = 1:5)
  )
  expect_identical(performance$cohort$tools$tool, c("refund", "lookup"))
  expect_identical(performance$cohort$models$n, c(2L, 2L, 1L))
  expect_identical(performance$cohort$users$n, c(1L, 1L))
  expect_identical(performance$cohort$headline$tool_calls, 4L)
})
