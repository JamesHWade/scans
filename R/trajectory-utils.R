trajectory_bind_rows <- function(rows) {
  rows <- Filter(\(row) !is.null(row) && nrow(row) > 0L, rows)
  if (length(rows) == 0L) {
    return(tibble::tibble())
  }
  tibble::as_tibble(vctrs::vec_rbind(!!!rows))
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

# One status vocabulary for every adapter: completed, failed, interrupted
# (a budget, stop request, or abandoned stream ended the run), or cancelled.
# Anything the source
# reports that is not recognised counts as interrupted rather than inventing
# a new status value.
trajectory_canonical_status <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return("interrupted")
  }
  switch(
    tolower(x),
    complete = ,
    completed = ,
    succeeded = ,
    success = "completed",
    error = ,
    failed = ,
    failure = ,
    provider_error = ,
    file_checkpoint_error = ,
    delegation_error = ,
    batch_error = ,
    setup_error = ,
    setup_failed = "failed",
    cancelled = ,
    canceled = "cancelled",
    "interrupted"
  )
}
