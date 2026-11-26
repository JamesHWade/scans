# Five trajectories over three UTC days with an empty day between them:
# a paired failing call, a call that failed without a result, a result no
# call claims, a repeated call, a plain answer, and a trajectory with neither
# a start time nor any evidence a scan could assess.
cohort_fixture <- function() {
  start <- as.POSIXct("2026-10-01 10:00:00", tz = "UTC")
  call <- function(trajectory, index, type, name, call_id, ...) {
    tibble::tibble(
      trajectory_id = trajectory,
      event_id = paste0(trajectory, "-", index),
      event_index = index,
      turn_id = paste0(trajectory, "-turn"),
      event_type = type,
      name = name,
      call_id = call_id,
      value = list(list(order = "A-1")),
      ...
    )
  }
  TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("a", "b", "c", "d", "e"),
      source_type = "otel",
      started_at = start + c(0, 5 * 3600, 2 * 86400, 2 * 86400 + 3600, NA),
      completed_at = start +
        c(10, 5 * 3600 + 20, 2 * 86400 + 40, NA, NA),
      model = c("small", "small", "large", "large", NA),
      metadata = list(
        list(otel = list(user = "ada")),
        list(otel = list(user = "ada")),
        list(otel = list(user = "bo")),
        list(),
        list()
      )
    ),
    tibble::tibble(
      trajectory_id = c("a", "b", "c", "d", "e"),
      turn_id = paste0(c("a", "b", "c", "d", "e"), "-turn"),
      turn_index = 1L,
      role = "assistant",
      input_tokens = c(10, 20, 80, NA, NA),
      output_tokens = c(5, 10, 20, NA, NA)
    ),
    vctrs::vec_rbind(
      call("a", 1L, "tool_call", "lookup", "a-1", duration = 1),
      call("a", 2L, "tool_result", NA, "a-1"),
      call("a", 3L, "tool_call", "lookup", "a-2", duration = 4),
      call("a", 4L, "tool_result", NA, "a-2", error = "timeout"),
      call(
        "b",
        1L,
        "content",
        NA,
        NA,
        text = "Unopened items can be returned.",
        status = "completed"
      ),
      call("c", 1L, "tool_call", "refund", "c-1", status = "failed"),
      call("c", 2L, "tool_result", "refund", "ghost", error = "late"),
      call("d", 1L, "tool_call", "lookup", "d-1", duration = 2),
      call("d", 2L, "tool_result", NA, "d-1"),
      call("d", 3L, "tool_call", "lookup", "d-2", duration = 3),
      call("d", 4L, "tool_result", NA, "d-2")
    )
  )
}

cohort_fixture_data <- function() {
  scans_app_data(cohort_fixture())
}

# c has a labelled record followed by a note; b has a note only.
cohort_fixture_annotations <- function() {
  tibble::tibble(
    application = "Cohort",
    trajectory_id = c("c", "c", "b"),
    label = c("tool failure", NA, NA),
    note = c(NA, "Checked the retry", "Fine"),
    author = c("ada", "bo", "ada"),
    created_at = as.POSIXct("2026-10-05", tz = "UTC") + c(0, 60, 120)
  )
}
