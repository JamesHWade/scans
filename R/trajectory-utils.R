trajectory_bind_rows <- function(rows) {
  rows <- Filter(\(row) !is.null(row) && nrow(row) > 0L, rows)
  if (length(rows) == 0L) {
    return(tibble::tibble())
  }
  tibble::as_tibble(do.call(rbind, rows))
}

trajectory_ids <- function(
  trajectory_id,
  turn_id = NA_character_,
  event_id = NA_character_
) {
  list(
    trajectory_id = trajectory_id,
    turn_id = turn_id,
    event_id = event_id
  )
}

trajectory_event_id <- function(trajectory_id, index) {
  sprintf("%s/event-%06d", trajectory_id, index)
}
