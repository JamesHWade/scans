#' Filter trajectory events
#'
#' `filter_trajectory_events()` selects canonical events using event fields,
#' containing-turn context, or exact top-level metadata values. Filters compose
#' with AND across arguments and OR within each character vector.
#'
#' The returned event columns are not modified. Three derived columns are added
#' after `turn_id`: `.turn_role`, `.turn_index`, and `.round_index`. These names
#' are reserved so source columns are never overwritten; a source event table
#' that already uses one of them produces a
#' `scans_error_scan_column_collision` error.
#'
#' @param x A [TrajectoryBundle].
#' @param trajectory_id,role,event_type,content_type,tool,status Optional
#'   character vectors. An event must match one value in every supplied filter.
#' @param metadata An optional uniquely named list of top-level event metadata
#'   values to match with exact R equality.
#'
#' @returns A tibble containing the selected canonical event rows and any extra
#'   source columns, followed by these derived turn-context columns:
#'
#' - Identity and order: character `trajectory_id`, `event_id`, and optional
#'   `turn_id` and `parent_event_id`; integer `event_index` and optional
#'   `content_index`.
#' - Event description: character `event_type`, optional `content_type`, `name`,
#'   `call_id`, and `text`; list-column `value` for structured payloads.
#' - Outcome and extensions: optional character `status` and `error`, plus the
#'   named-list `metadata` column.
#' - `.turn_role`: character role of the containing turn.
#' - `.turn_index`: integer position of the containing turn.
#' - `.round_index`: optional integer round of the containing turn.
#'
#' Rows follow trajectory order and then `event_index`. A valid filter with no
#' matches returns a typed zero-row tibble with the same columns.
#'
#' @export
#'
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "trajectory-1", source_type = "manual"),
#'   data.frame(
#'     trajectory_id = "trajectory-1",
#'     turn_id = "turn-1",
#'     turn_index = 1L,
#'     role = "assistant"
#'   ),
#'   data.frame(
#'     trajectory_id = "trajectory-1",
#'     event_id = "event-1",
#'     event_index = 1L,
#'     turn_id = "turn-1",
#'     event_type = "content",
#'     content_type = "text",
#'     text = "Done"
#'   )
#' )
#'
#' filter_trajectory_events(bundle, role = "assistant")
filter_trajectory_events <- function(
  x,
  trajectory_id = NULL,
  role = NULL,
  event_type = NULL,
  content_type = NULL,
  tool = NULL,
  status = NULL,
  metadata = NULL
) {
  check_trajectory_bundle(x)
  call <- rlang::caller_env()
  filters <- list(
    trajectory_id = trajectory_id,
    role = role,
    event_type = event_type,
    content_type = content_type,
    tool = tool,
    status = status
  )
  for (name in names(filters)) {
    scan_check_character_filter(filters[[name]], name, call)
  }
  scan_check_metadata_filter(metadata, call)

  events <- trajectory_events(x)
  scan_check_turn_context_columns(events, call)
  turns <- trajectory_turns(x)
  turn_match <- match(events$turn_id, turns$turn_id)
  event_roles <- turns$role[turn_match]
  event_turn_indices <- turns$turn_index[turn_match]
  event_round_indices <- turns$round_index[turn_match]

  keep <- rep(TRUE, nrow(events))
  keep <- keep & scan_filter_match(events$trajectory_id, trajectory_id)
  keep <- keep & scan_filter_match(event_roles, role)
  keep <- keep & scan_filter_match(events$event_type, event_type)
  keep <- keep & scan_filter_match(events$content_type, content_type)
  keep <- keep & scan_filter_match(events$status, status)
  if (!is.null(tool)) {
    keep <- keep &
      events$event_type %in% c("tool_call", "tool_result") &
      events$name %in% tool
  }
  if (!is.null(metadata)) {
    keep <- keep &
      vapply(
        events$metadata,
        scan_metadata_matches,
        logical(1),
        expected = metadata
      )
  }

  out <- events[keep, , drop = FALSE]
  out$.turn_role <- event_roles[keep]
  out$.turn_index <- event_turn_indices[keep]
  out$.round_index <- event_round_indices[keep]
  out <- scan_reorder_event_columns(out)

  trajectory_order <- match(out$trajectory_id, trajectory_info(x)$trajectory_id)
  row_order <- order(
    trajectory_order,
    out$event_index,
    out$event_id,
    na.last = TRUE,
    method = "radix"
  )
  out[row_order, , drop = FALSE]
}
