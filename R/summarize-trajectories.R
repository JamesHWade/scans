#' Summarize trajectories
#'
#' `summarize_trajectories()` reduces each trajectory in a [TrajectoryBundle]
#' to one row of identity, structure, diagnostic, and usage measures. Counts are
#' zero when no matching records exist. Usage totals are `NA` when every source
#' value contributing to that total is unknown.
#'
#' @param x A [TrajectoryBundle].
#'
#' @returns A tibble with one row per trajectory and these columns:
#'
#' - Identity: `trajectory_id`, `run_id`, `parent_trajectory_id`, `source_type`,
#'   `status`, `model`, `agent`, `task_id`, `sample_id`, `epoch`, `started_at`,
#'   and `completed_at` retain the corresponding trajectory fields, so
#'   summaries can be grouped by model, agent, or task without a join back to
#'   [trajectory_info()].
#' - Structure: `trajectory_depth` counts parent trajectories from a root at
#'   zero; `max_event_depth` counts the longest event-parent path;
#'   `n_turns`, `n_rounds`, and `n_events` count semantic records.
#' - Tool correlation: `n_tool_calls`, `n_tool_results`,
#'   `n_unresolved_tool_calls`, `n_unmatched_tool_results`, and
#'   `n_ambiguous_tool_correlations` count tool activity and correlation gaps.
#' - Failures and losses: `n_error_events`, `n_failed_turns`, and `n_losses`.
#' - Usage: `input_tokens`, `output_tokens`, and `cached_input_tokens` sum known
#'   token counts; `cost` sums known source costs without currency conversion;
#'   `turn_duration` sums known turn durations in seconds; `elapsed` is wall
#'   time in seconds from `started_at` through `completed_at`.
#'
#' Depths, counts, and `epoch` are integers. Usage, cost, and duration
#' columns are doubles; `started_at` and `completed_at` are `POSIXct`.
#'
#' @export
#'
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "trajectory-1", source_type = "manual"),
#'   data.frame(),
#'   data.frame()
#' )
#'
#' summarize_trajectories(bundle)
summarize_trajectories <- function(x) {
  check_trajectory_bundle(x)
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  losses <- trajectory_losses(x)

  if (nrow(info) == 0L) {
    return(scan_empty_summaries())
  }

  size <- nrow(info)
  trajectory_ids <- info$trajectory_id
  turn_groups <- match(turns$trajectory_id, trajectory_ids)
  event_groups <- match(events$trajectory_id, trajectory_ids)
  loss_groups <- match(losses$trajectory_id, trajectory_ids)
  trajectory_depths <- scan_parent_depths(
    info$trajectory_id,
    info$parent_trajectory_id
  )
  event_depths <- scan_parent_depths(
    events$event_id,
    events$parent_event_id
  )
  relation_counts <- scan_tool_relation_counts(
    events$event_type,
    events$call_id,
    event_groups,
    size,
    events$event_index
  )

  tibble::tibble(
    trajectory_id = trajectory_ids,
    run_id = info$run_id,
    parent_trajectory_id = info$parent_trajectory_id,
    source_type = info$source_type,
    status = info$status,
    model = info$model,
    agent = info$agent,
    task_id = info$task_id,
    sample_id = info$sample_id,
    epoch = info$epoch,
    started_at = info$started_at,
    completed_at = info$completed_at,
    trajectory_depth = trajectory_depths,
    max_event_depth = scan_group_max(event_depths, event_groups, size),
    n_turns = tabulate(turn_groups, nbins = size),
    n_rounds = scan_group_distinct_count(
      turns$round_index,
      turn_groups,
      size
    ),
    n_events = tabulate(event_groups, nbins = size),
    n_tool_calls = scan_group_count(
      events$event_type == "tool_call",
      event_groups,
      size
    ),
    n_tool_results = scan_group_count(
      events$event_type == "tool_result",
      event_groups,
      size
    ),
    n_unresolved_tool_calls = relation_counts$unresolved,
    n_unmatched_tool_results = relation_counts$unmatched,
    n_ambiguous_tool_correlations = relation_counts$ambiguous,
    n_error_events = scan_group_count(
      scan_event_is_error(events),
      event_groups,
      size
    ),
    n_failed_turns = scan_group_count(
      scan_turn_is_error(turns),
      turn_groups,
      size
    ),
    n_losses = tabulate(loss_groups, nbins = size),
    input_tokens = scan_group_sum(turns$input_tokens, turn_groups, size),
    output_tokens = scan_group_sum(turns$output_tokens, turn_groups, size),
    cached_input_tokens = scan_group_sum(
      turns$cached_input_tokens,
      turn_groups,
      size
    ),
    cost = scan_group_sum(turns$cost, turn_groups, size),
    turn_duration = scan_group_sum(turns$duration, turn_groups, size),
    elapsed = as.numeric(difftime(
      info$completed_at,
      info$started_at,
      units = "secs"
    ))
  )
}
