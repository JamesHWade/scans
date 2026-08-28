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
#' @param scans Which detectors to run, as a character vector of names from
#'   [scan_registry()]. `NULL` (the default) runs all of them. Selecting a
#'   subset narrows the findings without changing how any of them are
#'   computed, so a finding is the same whether or not its neighbours ran.
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
  loop_threshold = 3L,
  scans = NULL
) {
  check_trajectory_bundle(x)
  call <- rlang::caller_env()
  scan_check_id(scan_id, call)
  scans <- scan_check_selection(scans, call)
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

  # Selection filters the generated findings rather than skipping
  # detectors, so identifiers stay deterministic for a bundle and
  # `scan_id` however the panel is configured: a finding does not change
  # its id because a neighbouring detector was switched off.
  findings <- scan_bind_findings(findings, scan_id, info)
  if (is.null(scans)) {
    return(findings)
  }
  findings[findings$scan %in% scans, , drop = FALSE]
}

#' Available trajectory scans
#'
#' `scan_registry()` lists the detectors [scan_trajectories()] can run,
#' with the severity each produces and a one-line description. Use it to
#' build a scan selection rather than hard-coding names.
#'
#' @returns A tibble with `scan`, `severity`, and `description` columns.
#' @export
#'
#' @examples
#' scan_registry()
scan_registry <- function() {
  tibble::tibble(
    scan = c(
      "ambiguous_tool_correlation",
      "unresolved_tool_call",
      "unmatched_tool_result",
      "repeated_tool_call",
      "suspicious_tool_loop",
      "event_error",
      "error_chain"
    ),
    severity = c(
      "warning",
      "warning",
      "warning",
      "warning",
      "warning",
      "error",
      "error"
    ),
    description = c(
      "A call identifier matches more than one call or result.",
      "A tool was called and no result came back.",
      "A tool result arrived with no call to match it.",
      "The same tool was called with the same arguments repeatedly.",
      "Consecutive identical tool calls suggest the agent is looping.",
      "An event recorded a failure.",
      "A failure followed an earlier one within the same trajectory."
    )
  )
}

scan_check_selection <- function(scans, call) {
  if (is.null(scans)) {
    return(NULL)
  }
  if (!is.character(scans) || anyNA(scans)) {
    scans_abort(
      "{.arg scans} must be a character vector of scan names.",
      class = "scans_error_scan_selection",
      call = call
    )
  }
  known <- scan_registry()$scan
  unknown <- setdiff(scans, known)
  if (length(unknown) > 0L) {
    scans_abort(
      c(
        "Unknown scan{?s} in {.arg scans}: {.val {unknown}}.",
        i = "See {.fn scan_registry} for the available scans."
      ),
      class = "scans_error_scan_selection",
      call = call
    )
  }
  unique(scans)
}
