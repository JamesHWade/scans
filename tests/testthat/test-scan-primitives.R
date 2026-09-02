test_that("filter_trajectory_events() composes event and turn filters", {
  bundle <- trajectory_fixture("multiple_tools")

  assistant <- filter_trajectory_events(bundle, role = "assistant")
  expect_identical(
    assistant$event_id,
    c("tools-event-2", "tools-event-3", "tools-event-6")
  )
  expect_identical(assistant[[".turn_role"]], rep("assistant", 3L))

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
  expect_identical(result[[".turn_role"]], "tool")

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
  expect_identical(result[[".turn_role"]], "tool")
})

test_that("filter_trajectory_events() preserves source context columns", {
  tables <- minimal_trajectory_tables()
  tables$events$role <- "source-role"
  tables$events$turn_index <- 99L
  tables$events$round_index <- 98L
  bundle <- do.call(TrajectoryBundle, tables)

  result <- filter_trajectory_events(bundle)

  expect_identical(result$role, "source-role")
  expect_identical(result$turn_index, 99L)
  expect_identical(result$round_index, 98L)
  expect_identical(result[[".turn_role"]], "user")
  expect_identical(result[[".turn_index"]], 1L)
  expect_identical(result[[".round_index"]], NA_integer_)
})

test_that("filter_trajectory_events() rejects reserved source columns", {
  tables <- minimal_trajectory_tables()
  tables$events$.turn_role <- "source-role"
  bundle <- do.call(TrajectoryBundle, tables)

  condition <- rlang::catch_cnd(filter_trajectory_events(bundle))

  expect_s3_class(condition, "scans_error_scan_column_collision")
  expect_snapshot(error = TRUE, filter_trajectory_events(bundle))
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

test_that("assistant narration between identical calls does not hide a loop", {
  tables <- fixture_source(scan_loop_fixture())
  tables$events$event_type[[4L]] <- "content"
  tables$turns$role[[4L]] <- "assistant"
  bundle <- do.call(TrajectoryBundle, tables)

  findings <- scan_trajectories(bundle)

  expect_in("suspicious_tool_loop", findings$scan)
  expect_in("repeated_tool_call", findings$scan)
})

test_that("a user's repeated request is not a tool loop", {
  tables <- fixture_source(scan_loop_fixture())
  tables$events$event_type[[4L]] <- "content"
  tables$turns$role[[4L]] <- "user"
  bundle <- do.call(TrajectoryBundle, tables)

  findings <- scan_trajectories(bundle)

  expect_false("suspicious_tool_loop" %in% findings$scan)
  expect_in("repeated_tool_call", findings$scan)
})

test_that("a lifecycle event between identical calls breaks a tool loop", {
  tables <- fixture_source(scan_loop_fixture())
  tables$events$event_type[[4L]] <- "custom"
  bundle <- do.call(TrajectoryBundle, tables)

  findings <- scan_trajectories(bundle)

  expect_false("suspicious_tool_loop" %in% findings$scan)
  expect_in("repeated_tool_call", findings$scan)
})

test_that("failed trajectories and turns produce findings without events", {
  bundle <- TrajectoryBundle(
    data.frame(
      trajectory_id = "run-1",
      source_type = "manual",
      status = "failed",
      error = "boom"
    ),
    data.frame(
      trajectory_id = "run-1",
      turn_id = c("turn-1", "turn-2"),
      turn_index = 1:2,
      role = c("user", "assistant"),
      status = c("completed", "failed"),
      finish_reason = c(NA, "length")
    ),
    data.frame()
  )
  findings <- scan_trajectories(bundle)
  expect_identical(findings$scan, c("trajectory_error", "turn_error"))
  expect_identical(findings$severity, c("error", "error"))
  expect_identical(findings$turn_id, c(NA, "turn-2"))
  expect_true(all(is.na(findings$event_id)))
  expect_identical(findings$event_ids[[1L]], character())
  expect_identical(findings$value[[1L]]$error, "boom")
  expect_identical(findings$value[[2L]]$finish_reason, "length")

  only_turns <- scan_trajectories(bundle, scans = "turn_error")
  expect_identical(only_turns$finding_id, findings$finding_id[[2L]])
})

test_that("tool correlation and cycle checks handle large bundles", {
  size <- 20000L
  ids <- sprintf("event-%06d", seq_len(size))
  events <- tibble::tibble(
    trajectory_id = "run-1",
    turn_id = "turn-1",
    event_id = ids,
    event_index = seq_len(size),
    event_type = rep(c("tool_call", "tool_result"), size / 2L),
    name = "search",
    call_id = rep(sprintf("call-%06d", seq_len(size / 2L)), each = 2L),
    parent_event_id = c(NA_character_, ids[-size])
  )
  bundle <- TrajectoryBundle(
    data.frame(trajectory_id = "run-1", source_type = "manual"),
    data.frame(
      trajectory_id = "run-1",
      turn_id = "turn-1",
      turn_index = 1L,
      role = "assistant"
    ),
    events
  )
  summary <- summarize_trajectories(bundle)
  expect_identical(summary$n_unresolved_tool_calls, 0L)
  expect_identical(summary$n_unmatched_tool_results, 0L)
  expect_identical(summary$max_event_depth, size - 1L)

  # Many calls sharing one id: results resolve them in order.
  shared <- scan_tool_relation_indices(
    rep(c("tool_call", "tool_result"), c(3L, 2L)),
    rep("call-1", 5L)
  )
  expect_identical(shared$unresolved_calls, 3L)
  expect_identical(shared$unmatched_results, integer())
  expect_length(shared$ambiguous, 1L)

  expect_true(trajectory_reference_has_cycle(c("a", "b"), c("b", "a")))
  expect_false(trajectory_reference_has_cycle(c("a", "b"), c(NA, "a")))
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

test_that("tool results cannot resolve calls that occur later", {
  tables <- minimal_trajectory_tables()
  tables$turns <- tables$turns[rep(1L, 2L), , drop = FALSE]
  tables$turns$turn_id <- paste0("turn-", 1:2)
  tables$turns$turn_index <- 1:2
  tables$events <- tables$events[rep(1L, 2L), , drop = FALSE]
  tables$events$event_id <- paste0("event-", 1:2)
  tables$events$event_index <- c(2L, 1L)
  tables$events$turn_id <- tables$turns$turn_id
  tables$events$event_type <- c("tool_call", "tool_result")
  tables$events$name <- "lookup"
  tables$events$call_id <- "out-of-order"
  bundle <- do.call(TrajectoryBundle, tables)

  summary <- summarize_trajectories(bundle)
  findings <- scan_trajectories(bundle)

  expect_identical(summary$n_unresolved_tool_calls, 1L)
  expect_identical(summary$n_unmatched_tool_results, 1L)
  expect_identical(
    findings$scan,
    c("unmatched_tool_result", "unresolved_tool_call")
  )
  expect_identical(findings$event_id, c("event-2", "event-1"))
  expect_identical(
    findings$explanation,
    c(
      "No preceding tool call can be paired with this tool result.",
      "No subsequent tool result can be paired with this tool call."
    )
  )
})

test_that("scan_trajectories() reports ambiguous call identifiers", {
  tables <- minimal_trajectory_tables()
  tables$turns <- tables$turns[rep(1L, 3L), , drop = FALSE]
  tables$turns$turn_id <- paste0("turn-", 1:3)
  tables$turns$turn_index <- 1:3
  tables$events <- tables$events[rep(1L, 3L), , drop = FALSE]
  tables$events$event_id <- paste0("event-", 1:3)
  tables$events$event_index <- 1:3
  tables$events$turn_id <- tables$turns$turn_id
  tables$events$event_type <- c("tool_call", "tool_call", "tool_result")
  tables$events$name <- "lookup"
  tables$events$call_id <- "duplicate-call"
  tables$events$value <- list(list(id = 1L), list(id = 2L), "done")
  bundle <- do.call(TrajectoryBundle, tables)

  summary <- summarize_trajectories(bundle)
  findings <- scan_trajectories(bundle)

  expect_identical(summary$n_unresolved_tool_calls, 1L)
  expect_identical(summary$n_unmatched_tool_results, 0L)
  expect_identical(summary$n_ambiguous_tool_correlations, 1L)
  expect_setequal(
    findings$scan,
    c("ambiguous_tool_correlation", "unresolved_tool_call")
  )
})

test_that("scan_trajectories() canonicalizes named tool arguments", {
  tables <- fixture_source(trajectory_fixture("repeated_tools"))
  tables$events$value[[2L]] <- list(
    options = list(query = "same", limit = 10L)
  )
  tables$events$value[[4L]] <- list(
    options = list(limit = 10L, query = "same")
  )
  bundle <- do.call(TrajectoryBundle, tables)

  findings <- scan_trajectories(bundle)

  expect_in("repeated_tool_call", findings$scan)
})

test_that("scan_trajectories() finds event errors and causal chains", {
  findings <- scan_trajectories(scan_error_chain_fixture())
  # The fixture's failed trajectory and turns are reported first.
  expect_in(c("trajectory_error", "turn_error"), findings$scan)
  findings <- findings[
    !findings$scan %in% c("trajectory_error", "turn_error"),
  ]

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

  summary <- summarize_trajectories(bundle)
  findings <- scan_trajectories(bundle)

  expect_identical(nrow(filter_trajectory_events(bundle)), 0L)
  expect_named(
    summary,
    c(
      "trajectory_id",
      "run_id",
      "parent_trajectory_id",
      "source_type",
      "status",
      "model",
      "agent",
      "task_id",
      "sample_id",
      "epoch",
      "started_at",
      "completed_at",
      "trajectory_depth",
      "max_event_depth",
      "n_turns",
      "n_rounds",
      "n_events",
      "n_tool_calls",
      "n_tool_results",
      "n_unresolved_tool_calls",
      "n_unmatched_tool_results",
      "n_ambiguous_tool_correlations",
      "n_error_events",
      "n_failed_turns",
      "n_losses",
      "input_tokens",
      "output_tokens",
      "cached_input_tokens",
      "cost",
      "turn_duration",
      "elapsed"
    )
  )
  expect_identical(
    unname(vapply(summary, typeof, character(1))),
    c(
      rep("character", 9L),
      "integer",
      rep("double", 2L),
      rep("integer", 13L),
      rep("double", 6L)
    )
  )
  expect_named(
    findings,
    c(
      "finding_id",
      "scan_id",
      "scan",
      "scan_version",
      "trajectory_id",
      "turn_id",
      "event_id",
      "event_ids",
      "severity",
      "label",
      "value",
      "explanation",
      "metadata"
    )
  )
  expect_identical(
    unname(vapply(findings, typeof, character(1))),
    c(
      rep("character", 7L),
      "list",
      rep("character", 2L),
      "list",
      "character",
      "list"
    )
  )
})

test_that("summarize_trajectories() handles many independent trajectories", {
  size <- 1000L
  ids <- sprintf("trajectory-%04d", seq_len(size))
  bundle <- TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = ids,
      source_type = "synthetic"
    ),
    turns = tibble::tibble(
      trajectory_id = ids,
      turn_id = sprintf("turn-%04d", seq_len(size)),
      turn_index = 1L,
      role = "assistant"
    ),
    events = tibble::tibble(
      trajectory_id = ids,
      event_id = sprintf("event-%04d", seq_len(size)),
      event_index = 1L,
      turn_id = sprintf("turn-%04d", seq_len(size)),
      event_type = "content",
      content_type = "text"
    )
  )

  summary <- summarize_trajectories(bundle)

  expect_identical(summary$trajectory_id, ids)
  expect_identical(summary$n_turns, rep(1L, size))
  expect_identical(summary$n_events, rep(1L, size))
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
