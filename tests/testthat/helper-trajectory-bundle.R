minimal_trajectory_tables <- function() {
  list(
    trajectories = tibble::tibble(
      trajectory_id = "trajectory-1",
      source_type = "manual"
    ),
    turns = tibble::tibble(
      trajectory_id = "trajectory-1",
      turn_id = "turn-1",
      turn_index = 1L,
      role = "user"
    ),
    events = tibble::tibble(
      trajectory_id = "trajectory-1",
      event_id = "event-1",
      event_index = 1L,
      turn_id = "turn-1",
      content_index = 1L,
      event_type = "content",
      content_type = "text",
      text = "Hello"
    )
  )
}

minimal_trajectory_bundle <- function() {
  tables <- minimal_trajectory_tables()
  TrajectoryBundle(
    tables$trajectories,
    tables$turns,
    tables$events
  )
}
