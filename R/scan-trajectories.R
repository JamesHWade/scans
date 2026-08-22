#' Scan trajectories for diagnostic findings
#'
#' `scan_trajectories()` analyzes a completed [TrajectoryBundle] without calling
#' a model or tool. It detects ambiguous, unresolved, and unmatched tool
#' correlation; repeated calls and suspicious consecutive loops; failed events;
#' and causal error chains. Named tool-argument mappings are compared without
#' regard to key order.
#'
#' Finding identifiers are deterministic for an unchanged bundle, argument set,
#' and `scan_id`. The default `scan_id` is snapshot-local and is not intended as
#' a globally unique persistent identifier.
#'
#' @param x A [TrajectoryBundle].
#' @param scan_id A non-empty identifier for this diagnostic run.
#' @param repeat_threshold The minimum number of calls with the same tool name
#'   and arguments that produces a `repeated_tool_call` finding.
#' @param loop_threshold The minimum consecutive calls with the same tool name
#'   and arguments that produces a `suspicious_tool_loop` finding. Tool results
#'   between calls do not break a call sequence.
#'
#' @returns A tibble with one row per finding and these columns:
#'
#' - `finding_id`: deterministic identifier within `scan_id`; `scan_id`: the
#'   caller-supplied diagnostic-run identifier.
#' - `scan`: one of `ambiguous_tool_correlation`, `unresolved_tool_call`,
#'   `unmatched_tool_result`, `repeated_tool_call`, `suspicious_tool_loop`,
#'   `event_error`, or `error_chain`; `scan_version`: detector version.
#' - `trajectory_id`, `turn_id`, and `event_id`: primary evidence identities;
#'   `event_ids`: character vector of every supporting event identifier.
#' - `severity`: `warning` for tool-pattern findings or `error` for failures;
#'   `label` and `explanation`: concise human-readable descriptions.
#' - `value`: scan-specific structured details; `metadata`: a named extension
#'   list, currently empty for built-in scans.
#'
#' No findings returns a typed zero-row tibble with the same columns.
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
#' scan_trajectories(bundle)
scan_trajectories <- function(
  x,
  scan_id = "scan-000001",
  repeat_threshold = 2L,
  loop_threshold = 3L
) {
  check_trajectory_bundle(x)
  call <- rlang::caller_env()
  scan_check_id(scan_id, call)
  repeat_threshold <- scan_check_threshold(
    repeat_threshold,
    "repeat_threshold",
    call
  )
  loop_threshold <- scan_check_threshold(
    loop_threshold,
    "loop_threshold",
    call
  )

  info <- trajectory_info(x)
  events <- trajectory_events(x)
  event_groups <- scan_split_trajectory_rows(events, info$trajectory_id)
  findings <- list()

  for (index in seq_len(nrow(info))) {
    trajectory_events <- event_groups[[index]]
    trajectory_events <- trajectory_events[
      order(
        trajectory_events$event_index,
        trajectory_events$event_id,
        method = "radix"
      ),
      ,
      drop = FALSE
    ]
    findings <- c(
      findings,
      scan_tool_findings(
        trajectory_events,
        scan_id,
        repeat_threshold,
        loop_threshold
      ),
      scan_error_findings(trajectory_events, scan_id)
    )
  }

  scan_bind_findings(findings, scan_id, info)
}
