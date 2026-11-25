#' Scan trajectories for diagnostic findings
#'
#' `scan_trajectories()` analyzes a completed [TrajectoryBundle] without calling
#' a model or tool. It detects ambiguous, unresolved, and unmatched tool
#' correlation; repeated calls and suspicious consecutive loops; failed events;
#' and causal error chains. Named tool-argument mappings are compared without
#' regard to key order.
#'
#' Two scans read Deputy run evidence: the run ending in
#' `metadata$ending` and `deputy:*` lifecycle events, from any adapter that
#' records them. `cut_off_answer` reports a final answer the provider cut
#' short (`cut_off`, or a `max_tokens`, `context_window`, or `content_filter`
#' finish reason), which a `"completed"` status does not reveal.
#' `run_intervention` reports, with a `kind` in `value`: `limit_wrap_up` (a
#' usage limit made the model answer without tools), `provider_fallback`,
#' `compaction_failure`, `repeated_compaction` (at least two `deputy:compaction`
#' events in one run), `permission_denied`, and `approval_pending` (an approval
#' with no later decision or tool execution in the run). A failed compaction
#' or denied permission is also a failed event, so `event_error` reports the
#' same event: one finding records the failure, the other what it means for
#' the run.
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
#'   `event_error`, `error_chain`, `turn_error`, `trajectory_error`,
#'   `cut_off_answer`, or `run_intervention`; `scan_version`: detector version.
#' - `trajectory_id`, `turn_id`, and `event_id`: primary evidence identities;
#'   `event_ids`: character vector of every supporting event identifier. A
#'   `trajectory_error` or `cut_off_answer` finding has no turn or event, and a
#'   `turn_error` finding no event, because the evidence is recorded on the
#'   record itself. A limit wrap-up known only from the run ending also has no
#'   event.
#' - `severity`: `warning` for tool-pattern findings and run interventions, or
#'   `error` for failures and cut-off answers;
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

  collected <- scan_collect_findings(
    x,
    scan_id,
    repeat_threshold,
    loop_threshold
  )
  info <- trajectory_info(x)
  findings <- collected$findings

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
      "error_chain",
      "turn_error",
      "trajectory_error",
      "cut_off_answer",
      "run_intervention"
    ),
    severity = c(
      "warning",
      "warning",
      "warning",
      "warning",
      "warning",
      "error",
      "error",
      "error",
      "error",
      "error",
      "warning"
    ),
    description = c(
      "A call identifier matches more than one call or result.",
      "A tool was called and no result came back.",
      "A tool result arrived with no call to match it.",
      "The same tool was called with the same arguments repeatedly.",
      "Consecutive identical tool calls suggest the agent is looping.",
      "An event recorded a failure.",
      "A failure followed an earlier one within the same trajectory.",
      paste(
        "A turn ended with a failed status, an error, or a truncating",
        "finish reason."
      ),
      "The trajectory ended with a failed status or an error.",
      "The provider cut the run's final answer short.",
      paste(
        "The agent runtime intervened: a limit wrap-up, provider fallback,",
        "failed or repeated compaction, denied permission, or pending approval."
      )
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
