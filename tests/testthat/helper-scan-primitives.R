scan_loop_fixture <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-loop",
      source_type = "synthetic"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-loop",
      turn_id = paste0("loop-turn-", 1:6),
      turn_index = 1:6,
      role = rep(c("assistant", "tool"), 3L)
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-loop",
      event_id = paste0("loop-event-", 1:6),
      event_index = 1:6,
      turn_id = paste0("loop-turn-", 1:6),
      content_index = 1L,
      parent_event_id = c(
        NA,
        "loop-event-1",
        NA,
        "loop-event-3",
        NA,
        "loop-event-5"
      ),
      event_type = rep(c("tool_call", "tool_result"), 3L),
      name = "search",
      call_id = rep(paste0("loop-call-", 1:3), each = 2L),
      value = rep(list(list(query = "same"), "result"), 3L)
    )
  )
}

scan_correlation_fixture <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-correlation",
      source_type = "synthetic"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-correlation",
      turn_id = c("correlation-turn-1", "correlation-turn-2"),
      turn_index = 1:2,
      role = c("assistant", "tool")
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-correlation",
      event_id = c("correlation-event-1", "correlation-event-2"),
      event_index = 1:2,
      turn_id = c("correlation-turn-1", "correlation-turn-2"),
      content_index = 1L,
      event_type = c("tool_call", "tool_result"),
      name = "lookup",
      call_id = c("call-missing", "call-orphan")
    )
  )
}

scan_error_chain_fixture <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-chain",
      source_type = "synthetic",
      status = "failed"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-chain",
      turn_id = c("chain-turn-1", "chain-turn-2"),
      turn_index = 1:2,
      role = "assistant",
      status = "failed"
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-chain",
      event_id = c("chain-event-1", "chain-event-2"),
      event_index = 1:2,
      turn_id = c("chain-turn-1", "chain-turn-2"),
      content_index = 1L,
      parent_event_id = c(NA, "chain-event-1"),
      event_type = c("custom", "error"),
      status = "failed",
      error = c("Tool execution failed", "Recovery failed")
    )
  )
}
