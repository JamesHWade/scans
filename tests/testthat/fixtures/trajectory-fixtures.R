trajectory_fixtures <- function() {
  list(
    simple_exchange = fixture_simple_exchange(),
    multiple_tools = fixture_multiple_tools(),
    tool_error = fixture_tool_error(),
    repeated_tools = fixture_repeated_tools(),
    delegated_agent = fixture_delegated_agent(),
    missing_data = fixture_missing_data(),
    evaluated = fixture_evaluated()
  )
}

malformed_trajectory_fixtures <- function() {
  valid <- fixture_simple_exchange()
  valid_tables <- fixture_source(valid)

  missing_id <- valid_tables
  missing_id$trajectories$trajectory_id <- NULL

  unknown_reference <- valid_tables
  unknown_reference$events$trajectory_id <- "trajectory-missing"

  unsafe_payload <- valid_tables
  unsafe_payload$events$value[[1L]] <- new.env(parent = emptyenv())

  list(
    missing_id = list(
      source = missing_id,
      condition = "scans_error_trajectory_schema"
    ),
    unknown_reference = list(
      source = unknown_reference,
      condition = "scans_error_trajectory_validation"
    ),
    unsafe_payload = list(
      source = unsafe_payload,
      condition = "scans_error_trajectory_validation"
    )
  )
}

fixture_simple_exchange <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-simple",
      source_type = "synthetic",
      source_id = "source-simple"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-simple",
      turn_id = c("simple-turn-1", "simple-turn-2"),
      turn_index = 1:2,
      role = c("user", "assistant")
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-simple",
      event_id = c("simple-event-1", "simple-event-2"),
      event_index = 1:2,
      turn_id = c("simple-turn-1", "simple-turn-2"),
      content_index = c(1L, 1L),
      event_type = c("content", "content"),
      content_type = c("text", "text"),
      text = c("Hello", "Hi there")
    )
  )
}

fixture_multiple_tools <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-tools",
      run_id = "run-tools",
      source_type = "synthetic",
      source_id = "source-tools"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-tools",
      turn_id = paste0("tools-turn-", 1:4),
      turn_index = 1:4,
      role = c("user", "assistant", "tool", "assistant")
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-tools",
      event_id = paste0("tools-event-", 1:6),
      event_index = 1:6,
      turn_id = c(
        "tools-turn-1",
        "tools-turn-2",
        "tools-turn-2",
        "tools-turn-3",
        "tools-turn-3",
        "tools-turn-4"
      ),
      content_index = c(1L, 1L, 2L, 1L, 2L, 1L),
      event_type = c(
        "content",
        "tool_call",
        "tool_call",
        "tool_result",
        "tool_result",
        "content"
      ),
      content_type = c("text", NA, NA, NA, NA, "text"),
      name = c(NA, "weather", "clock", "weather", "clock", NA),
      call_id = c(
        NA,
        "call-weather",
        "call-clock",
        "call-weather",
        "call-clock",
        NA
      ),
      text = c("Plan my walk", NA, NA, "Sunny", "14:00", "Leave at two"),
      value = list(
        NULL,
        list(city = "Detroit"),
        list(zone = "America/Detroit"),
        "Sunny",
        "14:00",
        NULL
      )
    )
  )
}

fixture_tool_error <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-error",
      source_type = "synthetic",
      source_id = "source-error",
      status = "failed",
      error = "The lookup tool failed"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-error",
      turn_id = paste0("error-turn-", 1:3),
      turn_index = 1:3,
      role = c("user", "assistant", "tool"),
      status = c("completed", "completed", "failed"),
      error = c(NA, NA, "Service unavailable")
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-error",
      event_id = paste0("error-event-", 1:4),
      event_index = 1:4,
      turn_id = c(
        "error-turn-1",
        "error-turn-2",
        "error-turn-3",
        "error-turn-3"
      ),
      content_index = c(1L, 1L, 1L, 2L),
      event_type = c("content", "tool_call", "tool_result", "error"),
      content_type = c("text", NA, NA, "text"),
      name = c(NA, "lookup", "lookup", NA),
      call_id = c(NA, "call-failed", "call-failed", NA),
      text = c("Find the record", NA, NA, "The lookup failed"),
      value = list(NULL, list(id = 42L), NULL, NULL),
      status = c("completed", "completed", "failed", "failed"),
      error = c(NA, NA, "Service unavailable", "Service unavailable")
    ),
    losses = tibble::tibble(
      trajectory_id = "trajectory-error",
      turn_id = "error-turn-3",
      event_id = "error-event-3",
      field = "provider_response",
      reason = "redacted",
      detail = "Credentials were removed"
    )
  )
}

fixture_repeated_tools <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-repeated",
      source_type = "synthetic",
      source_id = "source-repeated"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-repeated",
      turn_id = paste0("repeated-turn-", 1:5),
      turn_index = 1:5,
      round_index = c(1L, 1L, 1L, 2L, 2L),
      role = c("user", "assistant", "tool", "assistant", "tool")
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-repeated",
      event_id = paste0("repeated-event-", 1:5),
      event_index = 1:5,
      turn_id = paste0("repeated-turn-", 1:5),
      content_index = rep(1L, 5L),
      event_type = c(
        "content",
        "tool_call",
        "tool_result",
        "tool_call",
        "tool_result"
      ),
      content_type = c("text", NA, NA, NA, NA),
      name = c(NA, "search", "search", "search", "search"),
      call_id = c(
        NA,
        "call-search-1",
        "call-search-1",
        "call-search-2",
        "call-search-2"
      ),
      text = c("Search twice", NA, "first", NA, "second")
    ),
    losses = tibble::tibble(
      trajectory_id = "trajectory-repeated",
      turn_id = "repeated-turn-5",
      event_id = "repeated-event-5",
      field = "events$value",
      reason = "truncated",
      detail = "The source result exceeded the fixture limit"
    )
  )
}

fixture_delegated_agent <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = c("trajectory-parent", "trajectory-child"),
      run_id = c("run-delegated", "run-delegated"),
      parent_trajectory_id = c(NA, "trajectory-parent"),
      source_type = c("synthetic", "synthetic"),
      source_id = c("source-parent", "source-child"),
      agent = c("coordinator", "researcher")
    ),
    turns = tibble::tibble(
      trajectory_id = c("trajectory-parent", "trajectory-child"),
      turn_id = c("parent-turn-1", "child-turn-1"),
      turn_index = c(1L, 1L),
      role = c("assistant", "synthetic:researcher")
    ),
    events = tibble::tibble(
      trajectory_id = c("trajectory-parent", "trajectory-child"),
      event_id = c("parent-event-1", "child-event-1"),
      event_index = c(1L, 1L),
      turn_id = c("parent-turn-1", "child-turn-1"),
      content_index = c(1L, 1L),
      event_type = c("synthetic:delegation", "content"),
      content_type = c(NA, "text"),
      text = c("Delegate research", "Research complete")
    )
  )
}

fixture_missing_data <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-missing",
      source_type = "synthetic",
      source_id = "source-missing",
      metadata = list(list(source_field = "preserved"))
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-missing",
      turn_id = "missing-turn-1",
      turn_index = 1L,
      role = "assistant",
      metadata = list(list(unknown_field = 17L))
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-missing",
      event_id = "missing-event-1",
      event_index = 1L,
      turn_id = "missing-turn-1",
      content_index = 1L,
      event_type = "content",
      content_type = "synthetic:opaque",
      metadata = list(list(source_kind = "opaque"))
    ),
    losses = tibble::tibble(
      trajectory_id = "trajectory-missing",
      turn_id = "missing-turn-1",
      event_id = "missing-event-1",
      field = "content",
      reason = "unsupported",
      detail = "The opaque content has no canonical representation",
      metadata = list(list(source_kind = "opaque"))
    )
  )
}

fixture_evaluated <- function() {
  started_at <- as.POSIXct("2026-08-22 12:00:00", tz = "UTC")

  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-evaluated",
      run_id = "run-evaluated",
      source_type = "synthetic",
      source_id = "source-evaluated",
      source_uri = "fixture://evaluated",
      task_id = "task-1",
      sample_id = "sample-1",
      epoch = 1L,
      agent = "fixture-agent",
      model = "fixture-model",
      started_at = started_at,
      completed_at = started_at + 2,
      status = "completed",
      metadata = list(list(dataset = "synthetic"))
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-evaluated",
      turn_id = c("evaluated-turn-1", "evaluated-turn-2"),
      turn_index = 1:2,
      round_index = c(1L, 1L),
      role = c("assistant", "tool"),
      input_tokens = c(12, NA),
      output_tokens = c(4, NA),
      cached_input_tokens = c(2, NA),
      cost = c(0.001, NA),
      duration = c(1.2, 0.5),
      finish_reason = c("tool_call", "completed"),
      status = c("completed", "completed"),
      metadata = list(list(provider = "fixture"), list(tool = "score"))
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-evaluated",
      event_id = c("evaluated-event-1", "evaluated-event-2"),
      event_index = 1:2,
      turn_id = c("evaluated-turn-1", "evaluated-turn-2"),
      content_index = c(1L, 1L),
      parent_event_id = c(NA, "evaluated-event-1"),
      event_type = c("tool_call", "tool_result"),
      content_type = c("json", "json"),
      name = c("score", "score"),
      call_id = c("call-score", "call-score"),
      text = c("Score the answer", "0.9"),
      value = list(list(answer = 42L), list(score = 0.9)),
      timestamp = started_at + c(0.5, 1.5),
      duration = c(0.1, 0.4),
      status = c("completed", "completed"),
      metadata = list(list(kind = "request"), list(kind = "result"))
    ),
    evaluations = tibble::tibble(
      trajectory_id = "trajectory-evaluated",
      evaluation_id = "evaluation-1",
      task_id = "task-1",
      sample_id = "sample-1",
      epoch = 1L,
      scorer = "accuracy",
      value = list(0.9),
      target = list(1),
      explanation = "The answer matches the target",
      metadata = list(list(source = "vitals-fixture"))
    ),
    losses = tibble::tibble(
      trajectory_id = "trajectory-evaluated",
      turn_id = "evaluated-turn-2",
      event_id = "evaluated-event-2",
      field = "binary_attachment",
      reason = "externalized",
      detail = "Binary fixture content is represented by a safe reference",
      metadata = list(list(uri = "fixture://attachment"))
    )
  )
}

fixture_source <- function(bundle) {
  properties <- S7::props(bundle)
  properties[c("trajectories", "turns", "events", "evaluations", "losses")]
}
