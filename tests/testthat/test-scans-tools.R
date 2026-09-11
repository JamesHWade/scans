test_that("tools count the population and retain canonical evidence", {
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- scans_support_bundle()
  tools <- scans_tools(bundle)
  summary <- scans_tool_result(tools$scans_summarize)
  expect_equal(summary$data[[1L]]$n, 4L)
  expect_equal(summary$data[[1L]]$n_findings, 1L)
  expect_equal(summary$data[[1L]]$median_elapsed, 33)
  expect_equal(
    vapply(summary$patterns, function(x) x$findings, numeric(1)),
    c(3, 1, 1)
  )
  events <- scans_tool_result(
    tools$scans_find_events,
    query = "TIMEOUT",
    limit = 2L
  )
  expect_equal(events$paging$total, 3L)
  expect_equal(events$paging$next_offset, 2L)
  expect_equal(events$data[[1L]]$event_id, "otel/retry/event-000002")
  expect_identical(events$evidence[[1L]]$snapshot_id, summary$scope$snapshot_id)
  expect_identical(events$evidence[[1L]]$event_id, events$data[[1L]]$event_id)
  last <- scans_tool_result(
    tools$scans_find_events,
    query = "timeout",
    offset = 2L
  )
  expect_equal(last$data[[1L]]$event_id, "otel/retry/event-000006")
  expect_null(last$paging$next_offset)
})

test_that("scope cannot broaden through any tool", {
  skip_if_not_installed("ellmer", "0.5.0")
  tools <- scans_tools(scans_support_bundle(), "otel/retry", max_rows = 2L)
  expect_equal(
    scans_tool_result(tools$scans_list_trajectories)$paging$total,
    1L
  )
  expect_error(
    tools$scans_read_trajectory("otel/follow-up"),
    class = "scans_error_tools_scope"
  )
  for (name in c(
    "scans_summarize",
    "scans_find_events",
    "scans_get_findings",
    "scans_measure"
  )) {
    expect_error(
      tools[[name]](trajectory_ids = "otel/follow-up"),
      class = "scans_error_tools_scope"
    )
    result <- scans_tool_result(tools[[name]], trajectory_ids = character())
    if (name == "scans_summarize") {
      expect_equal(result$data[[1L]]$n, 0L)
    } else {
      expect_length(result$data, 0L)
    }
  }
  expect_error(
    tools$scans_read_trajectory(
      "otel/retry",
      event_id = "otel/parallel/event-000001"
    ),
    class = "scans_error_tools_scope"
  )
  expect_error(
    tools$scans_find_events(limit = 3L),
    class = "scans_error_tools_limit"
  )
  expect_error(
    tools$scans_list_trajectories(offset = -1L),
    class = "scans_error_tools_limit"
  )
  expect_error(
    scans_tools(scans_support_bundle(), "missing"),
    class = "scans_error_tools_scope"
  )
})

test_that("saved analysis is retained and missing measurements stay unknown", {
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- scans_support_bundle()
  saved <- investigation_snapshot(
    bundle,
    scans = "repeated_tool_call",
    repeat_threshold = 4L
  )
  tools <- scans_tools(saved)
  findings <- scans_tool_result(tools$scans_get_findings)
  expect_length(findings$data, 0L)
  expect_equal(findings$settings$repeat_threshold, 4L)
  expect_identical(findings$scope$revision_id, saved$manifest$revision_id)
  expect_equal(findings$scope$analysis_source, "saved")
  measurements <- scans_tool_result(
    tools$scans_measure,
    trajectory_ids = "otel/follow-up",
    measures = c("elapsed", "recorded_work", "unattributed_elapsed")
  )
  expect_equal(
    vapply(measurements$data, function(x) x$value, numeric(1)),
    c(126, 6, 120)
  )
  missing <- scans_tool_result(
    tools$scans_measure,
    trajectory_ids = "otel/limited-capture",
    measures = "elapsed"
  )
  expect_null(missing$data[[1L]]$value)
  expect_null(missing$comparison[[1L]]$median_recorded)
  expect_null(missing$comparison[[1L]]$median_complete)
  expect_equal(missing$comparison[[1L]]$n_recorded, 0L)
  expect_equal(missing$comparison[[1L]]$n_total, 1L)
})

test_that("oversized fields have retrievable slices and copies are immutable", {
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- trajectory_fixture("simple_exchange")
  events <- trajectory_events(bundle)
  events$text[[1L]] <- paste(rep("abcdef", 100L), collapse = "")
  bundle <- TrajectoryBundle(
    trajectory_info(bundle),
    trajectory_turns(bundle),
    events
  )
  tools <- scans_tools(bundle, max_chars = 100L)
  original <- events$text[[1L]]
  bundle@events$text[[1L]] <- "changed after construction"
  first <- scans_tool_result(
    tools$scans_read_trajectory,
    "trajectory-simple",
    limit = 1L
  )
  expect_equal(nchar(first$data[[1L]]$text), 100L)
  expect_equal(first$data[[1L]]$content_slices$text$total_chars, 600L)
  expect_equal(first$data[[1L]]$content_slices$text$next_offset, 100L)
  next_slice <- scans_tool_result(
    tools$scans_read_trajectory,
    "trajectory-simple",
    event_id = first$data[[1L]]$event_id,
    content_offset = 100L
  )
  expect_equal(next_slice$data[[1L]]$text, substr(original, 101L, 200L))
  expect_identical(first$evidence[[1L]]$key, next_slice$evidence[[1L]]$key)
  found <- scans_tool_result(
    tools$scans_find_events,
    query = "abcdef",
    content_offset = 200L,
    limit = 1L
  )
  expect_equal(found$data[[1L]]$text, substr(original, 201L, 300L))
  beyond <- scans_tool_result(
    tools$scans_read_trajectory,
    "trajectory-simple",
    event_id = first$data[[1L]]$event_id,
    content_offset = .Machine$integer.max
  )
  expect_identical(beyond$data[[1L]]$text, "")
  expect_null(beyond$data[[1L]]$content_slices$text$next_offset)
  past_last <- scans_tool_result(
    tools$scans_find_events,
    offset = .Machine$integer.max
  )
  expect_length(past_last$data, 0L)
  expect_null(past_last$paging$next_offset)
})

test_that("measure comparisons distinguish partial and complete observations", {
  skip_if_not_installed("ellmer", "0.5.0")
  tools <- scans_tools(scans_support_bundle())
  result <- scans_tool_result(tools$scans_measure, measures = "recorded_work")
  comparison <- result$comparison[[1L]]
  expect_equal(comparison$n_total, 4L)
  expect_equal(comparison$n_recorded, 4L)
  expect_equal(comparison$n_complete, 3L)
  expect_equal(comparison$median_recorded, 8)
  expect_equal(comparison$median_complete, 10)
})

test_that("tool and app text filtering and ordering agree", {
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- scans_support_bundle()
  data <- scans_app_data(bundle)
  tools <- scans_tools(bundle)
  for (query in c("", "DEMO-104", "return", "does not occur")) {
    visible <- scans_app_filter_records(data$records, query = query)
    visible <- scans_app_order_records(data$records, visible, "findings")
    result <- scans_tool_result(
      tools$scans_list_trajectories,
      query = query,
      sort = "findings"
    )
    expect_equal(
      vapply(result$data, function(x) x$trajectory_id, character(1)),
      data$records$trajectory_id[visible]
    )
  }
})

test_that("absent event values remain JSON null rather than recorded text", {
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- trajectory_fixture("simple_exchange")
  events <- trajectory_events(bundle)
  events$value <- list(NULL, list(status = "null", optional = NULL))
  events$metadata <- list(list(recorded = NULL), list())
  bundle <- TrajectoryBundle(
    trajectory_info(bundle),
    trajectory_turns(bundle),
    events
  )
  tools <- scans_tools(bundle)
  result <- scans_tool_result(tools$scans_read_trajectory, "trajectory-simple")

  expect_contains(names(result$data[[1L]]), c("value", "metadata"))
  expect_null(result$data[[1L]]$value)
  expect_equal(
    jsonlite::fromJSON(result$data[[1L]]$metadata, simplifyVector = FALSE),
    list(recorded = NULL)
  )
  expect_equal(
    jsonlite::fromJSON(result$data[[2L]]$value, simplifyVector = FALSE),
    list(status = "null", optional = NULL)
  )
  expect_identical(result$data[[2L]]$metadata, "[]")
})

test_that("invalid measure names receive a measurement diagnostic", {
  skip_if_not_installed("ellmer", "0.5.0")
  tools <- scans_tools(scans_support_bundle())
  for (measures in list("elasped", c("elapsed", NA_character_), 1L)) {
    expect_error(
      tools$scans_measure(measures = measures),
      regexp = "measures.*resource measurements",
      class = "scans_error_tools_measure"
    )
  }
  result <- scans_tool_result(tools$scans_measure, measures = character())
  expect_length(result$data, 0L)
})

test_that("missing string arguments receive the tools input condition", {
  skip_if_not_installed("ellmer", "0.5.0")
  tools <- scans_tools(scans_support_bundle())
  expect_error(
    tools$scans_read_trajectory(NA_character_),
    class = "scans_error_tools_input"
  )
  expect_error(
    tools$scans_read_trajectory("otel/retry", event_id = NA_character_),
    class = "scans_error_tools_input"
  )
  for (name in c("query", "role", "event_type", "tool", "status")) {
    expect_error(
      do.call(
        tools$scans_find_events,
        stats::setNames(list(NA_character_), name)
      ),
      class = "scans_error_tools_input"
    )
  }
})

test_that("tools require newer ellmer without raising the adapter minimum", {
  check <- ellmer_check_installed
  local_mocked_bindings(ellmer_check_installed = function(...) {
    check(..., installed = function(pkg, version) {
      numeric_version("0.4.2") >= numeric_version(version)
    })
  })

  expect_no_error(ellmer_check_installed())
  expect_error(
    scans_tools(NULL),
    regexp = "ellmer.*0[.]5[.]0",
    class = "scans_error_missing_dependency"
  )
})
