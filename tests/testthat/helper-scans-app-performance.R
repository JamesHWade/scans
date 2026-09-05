performance_fixture <- function() {
  start <- as.POSIXct("2026-09-01 12:00:00", tz = "UTC")
  TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("fast", "slow", "unknown"),
      source_type = "manual",
      started_at = start + c(0, 3600, NA),
      completed_at = start + c(10, 3700, NA),
      status = c("completed", "failed", NA),
      model = c("small", "large", NA)
    ),
    tibble::tibble(
      trajectory_id = c("fast", "slow", "slow"),
      turn_id = c("fast-1", "slow-1", "slow-2"),
      turn_index = c(1L, 1L, 2L),
      role = "assistant",
      input_tokens = c(10, 100, NA),
      output_tokens = c(5, 20, NA)
    ),
    tibble::tibble(
      trajectory_id = "slow",
      event_id = c("error-1", "error-2"),
      event_index = 1:2,
      event_type = "error",
      error = c("First failure", "Second failure")
    ),
    evaluations = tibble::tibble(
      trajectory_id = "fast",
      evaluation_id = "score-1",
      value = list(1)
    ),
    losses = tibble::tibble(
      turn_id = c("slow-2", NA),
      field = "usage",
      reason = "unsupported",
      detail = "Usage unavailable"
    )
  )
}
