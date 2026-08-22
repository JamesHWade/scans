test_that("filter_trajectory_events() composes event and turn filters", {
  bundle <- trajectory_fixture("multiple_tools")

  assistant <- filter_trajectory_events(bundle, role = "assistant")
  expect_identical(
    assistant$event_id,
    c("tools-event-2", "tools-event-3", "tools-event-6")
  )
  expect_identical(assistant$role, rep("assistant", 3L))

  weather <- filter_trajectory_events(
    bundle,
    event_type = c("tool_call", "tool_result"),
    tool = "weather"
  )
  expect_identical(
    weather$event_id,
    c("tools-event-2", "tools-event-4")
  )
  expect_identical(weather$call_id, rep("call-weather", 2L))
})

test_that("filter_trajectory_events() matches event metadata exactly", {
  bundle <- trajectory_fixture("evaluated")

  result <- filter_trajectory_events(
    bundle,
    metadata = list(kind = "result")
  )
  expect_identical(result$event_id, "evaluated-event-2")
  expect_identical(result$role, "tool")

  missing <- filter_trajectory_events(bundle, tool = "missing")
  expect_identical(nrow(missing), 0L)
  expect_named(missing, names(result))
})

test_that("filter_trajectory_events() applies every canonical filter", {
  bundle <- trajectory_fixture("tool_error")

  result <- filter_trajectory_events(
    bundle,
    trajectory_id = "trajectory-error",
    role = "tool",
    event_type = c("tool_result", "error"),
    content_type = "text",
    status = "failed"
  )

  expect_identical(result$event_id, "error-event-4")
  expect_identical(result$trajectory_id, "trajectory-error")
  expect_identical(result$role, "tool")
})

test_that("summarize_trajectories() reports activity, usage, and depth", {
  summary <- summarize_trajectories(trajectory_fixture("evaluated"))

  expect_identical(summary$trajectory_id, "trajectory-evaluated")
  expect_identical(summary$n_turns, 2L)
  expect_identical(summary$n_events, 2L)
  expect_identical(summary$n_tool_calls, 1L)
  expect_identical(summary$n_tool_results, 1L)
  expect_identical(summary$n_unresolved_tool_calls, 0L)
  expect_identical(summary$n_unmatched_tool_results, 0L)
  expect_identical(summary$n_losses, 1L)
  expect_identical(summary$input_tokens, 12)
  expect_identical(summary$output_tokens, 4)
  expect_identical(summary$cached_input_tokens, 2)
  expect_identical(summary$cost, 0.001)
  expect_equal(summary$turn_duration, 1.7)
  expect_identical(summary$elapsed, 2)
  expect_identical(summary$trajectory_depth, 0L)
  expect_identical(summary$max_event_depth, 1L)
})

test_that("summarize_trajectories() reports delegated trajectory depth", {
  summary <- summarize_trajectories(trajectory_fixture("delegated_agent"))

  expect_identical(
    summary$trajectory_id,
    c("trajectory-parent", "trajectory-child")
  )
  expect_identical(summary$trajectory_depth, c(0L, 1L))
  expect_identical(summary$max_event_depth, c(0L, 0L))
})

test_that("summarize_trajectories() counts distinct failure records", {
  summary <- summarize_trajectories(trajectory_fixture("tool_error"))

  expect_identical(summary$n_error_events, 2L)
  expect_identical(summary$n_failed_turns, 1L)
  expect_identical(summary$n_unresolved_tool_calls, 0L)
  expect_identical(summary$n_unmatched_tool_results, 0L)
})

test_that("scan_trajectories() finds repeated tool calls", {
  findings <- scan_trajectories(
    trajectory_fixture("repeated_tools"),
    scan_id = "scan-repeat"
  )

  expect_identical(findings$scan, "repeated_tool_call")
  expect_identical(findings$finding_id, "scan-repeat/finding-000001")
  expect_identical(
    findings$event_ids[[1L]],
    c("repeated-event-2", "repeated-event-4")
  )
  expect_identical(findings$value[[1L]]$count, 2L)
})

test_that("scan_trajectories() distinguishes repeated calls from loops", {
  bundle <- scan_loop_fixture()
  findings <- scan_trajectories(bundle, scan_id = "scan-loop")

  expect_identical(
    findings$scan,
    c("repeated_tool_call", "suspicious_tool_loop")
  )
  expect_identical(
    findings$event_ids[[2L]],
    c("loop-event-1", "loop-event-3", "loop-event-5")
  )
  expect_identical(findings$value[[2L]]$count, 3L)
})

test_that("scan_trajectories() compares tool arguments, not names alone", {
  tables <- fixture_source(trajectory_fixture("repeated_tools"))
  tables$events$value[[2L]] <- list(query = "first")
  tables$events$value[[4L]] <- list(query = "second")
  bundle <- do.call(TrajectoryBundle, tables)

  findings <- scan_trajectories(bundle)

  expect_identical(findings, scan_empty_findings())
})

test_that("scan thresholds control repeated and loop findings", {
  repeated <- scan_trajectories(
    trajectory_fixture("repeated_tools"),
    repeat_threshold = 3L,
    loop_threshold = 2L
  )

  expect_identical(repeated$scan, "suspicious_tool_loop")
  expect_identical(repeated$value[[1L]]$count, 2L)
})

test_that("non-tool content breaks a suspicious tool loop", {
  tables <- fixture_source(scan_loop_fixture())
  tables$events$event_type[[4L]] <- "content"
  bundle <- do.call(TrajectoryBundle, tables)

  findings <- scan_trajectories(bundle)

  expect_identical(
    findings$scan[findings$scan == "suspicious_tool_loop"],
    character()
  )
  expect_in("repeated_tool_call", findings$scan)
})

test_that("scan_trajectories() preserves unresolved correlation evidence", {
  bundle <- scan_correlation_fixture()
  findings <- scan_trajectories(bundle)

  expect_identical(
    findings$scan,
    c("unresolved_tool_call", "unmatched_tool_result")
  )
  expect_identical(
    findings$event_id,
    c("correlation-event-1", "correlation-event-2")
  )
  expect_identical(findings$value[[1L]]$call_id, "call-missing")
  expect_identical(findings$value[[2L]]$call_id, "call-orphan")
})

test_that("scan_trajectories() finds event errors and causal chains", {
  findings <- scan_trajectories(scan_error_chain_fixture())

  expect_identical(
    findings$scan,
    c("event_error", "error_chain", "event_error")
  )
  expect_identical(
    findings$event_ids[[2L]],
    c("chain-event-1", "chain-event-2")
  )
  expect_identical(findings$value[[2L]]$length, 2L)
})

test_that("scan primitives return typed empty results", {
  bundle <- TrajectoryBundle(data.frame(), data.frame(), data.frame())

  expect_identical(nrow(filter_trajectory_events(bundle)), 0L)
  expect_identical(summarize_trajectories(bundle), scan_empty_summaries())
  expect_identical(scan_trajectories(bundle), scan_empty_findings())
})

test_that("ordinary conversations do not produce findings", {
  findings <- scan_trajectories(trajectory_fixture("simple_exchange"))

  expect_identical(findings, scan_empty_findings())
})

test_that("scan primitive arguments fail with stable conditions", {
  bundle <- minimal_trajectory_bundle()

  expect_snapshot(error = TRUE, {
    filter_trajectory_events(bundle, role = NA_character_)
  })
  expect_snapshot(error = TRUE, {
    filter_trajectory_events(bundle, metadata = list("user"))
  })
  expect_snapshot(error = TRUE, {
    filter_trajectory_events(bundle, metadata = list(value = globalenv()))
  })
  expect_snapshot(error = TRUE, {
    scan_trajectories(bundle, scan_id = "")
  })
  expect_snapshot(error = TRUE, {
    scan_trajectories(bundle, repeat_threshold = 1L)
  })
  expect_snapshot(error = TRUE, {
    summarize_trajectories(data.frame())
  })
})
