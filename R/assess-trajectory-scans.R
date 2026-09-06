#' Assess built-in trajectory scans and their evidence coverage
#'
#' `assess_trajectory_scans()` runs the built-in diagnostics and explains whether
#' each selected scanner could assess each trajectory. It uses the same
#' detectors as [scan_trajectories()] and never calls a model or tool.
#'
#' @inheritParams scan_trajectories
#'
#' @returns A list with two tibbles:
#' - `findings`: the findings from assessable scanners, with the same columns
#'   and identities as [scan_trajectories()]. Candidates from scanners with
#'   insufficient evidence are excluded. The findings-only API still returns
#'   those raw candidates.
#' - `assessments`: one row per selected scanner and trajectory, with `scan_id`,
#'   `trajectory_id`, `scan`, `scan_version`, `status`, `required_evidence`,
#'   `reason`, and list columns `settings`, `finding_ids`, `limitations`, and
#'   `loss_rows`. `loss_rows` indexes [trajectory_losses()] in the input bundle,
#'   including unassigned losses that may affect any trajectory.
#'
#' Status is one of `assessed_no_findings`, `assessed_with_findings`,
#' `insufficient_evidence`, `not_applicable`, or `execution_failure`.
#' An assessment describes only the retained records, never complete capture
#' or overall success. A positive error observation remains assessable when
#' other records lack status; limitations explain that negative conclusions
#' are unavailable. Missing semantic records do not establish inapplicability.
#'
#' Tool comparisons require recorded identities or arguments as appropriate.
#' Known losses in required fields prevent assessment. Empty argument lists
#' are valid; missing arguments are not. Recorded text conversations without
#' tool activity make tool scans inapplicable to that capture. Status scans
#' require explicit status or failure evidence. Recognized status labels are
#' `completed`, `failed`, `cancelled`, `interrupted`, `running`, and `pending`;
#' other labels do not establish status coverage. Multiple failed events without
#' recorded parent links cannot establish the absence of an error chain.
#'
#' Built-ins execute in tool, event, and record groups. A group failure affects
#' its selected scanners on that trajectory; other groups continue. Failure
#' reasons omit arbitrary condition messages. Finding identifiers are local to
#' the run and can change if a previously failed group later succeeds.
#'
#' @export
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "span-1", source_type = "manual",
#'              status = "completed"),
#'   data.frame(), data.frame()
#' )
#' result <- assess_trajectory_scans(bundle)
#' result$assessments[c("scan", "status", "reason")]
assess_trajectory_scans <- function(
  x,
  scan_id = "scan-000001",
  repeat_threshold = 2L,
  loop_threshold = 3L,
  scans = NULL
) {
  check_trajectory_bundle(x)
  call <- rlang::caller_env()
  scan_check_id(scan_id, call)
  scans <- scan_check_selection(scans, call) %||% scan_registry()$scan
  repeat_threshold <- scan_check_threshold(
    repeat_threshold,
    "repeat_threshold",
    call
  )
  loop_threshold <- scan_check_threshold(loop_threshold, "loop_threshold", call)
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  losses <- trajectory_losses(x)
  owners <- scan_loss_trajectory_ids(losses, turns, events)
  collected <- scan_collect_findings(
    x,
    scan_id,
    repeat_threshold,
    loop_threshold,
    capture_errors = TRUE
  )
  findings <- scan_bind_findings(collected$findings, scan_id, info)
  event_groups <- scan_split_trajectory_rows(events, info$trajectory_id)
  turn_groups <- scan_split_trajectory_rows(turns, info$trajectory_id)
  assessments <- list()
  retained <- rep(FALSE, nrow(findings))
  for (i in seq_len(nrow(info))) {
    id <- info$trajectory_id[[i]]
    loss_rows <- which(is.na(owners) | owners == id)
    scoped_losses <- losses[loss_rows, , drop = FALSE]
    for (scan in scans) {
      rows <- which(findings$trajectory_id == id & findings$scan == scan)
      evidence <- scan_assess_evidence(
        scan,
        info[i, , drop = FALSE],
        turn_groups[[i]],
        event_groups[[i]],
        scoped_losses,
        length(rows) > 0L
      )
      status <- evidence$status
      reason <- evidence$reason
      if (scan_assessment_group(scan) %in% collected$failures[[i]]) {
        status <- "execution_failure"
        reason <- "The built-in detector group failed. Other groups were retained; reload or retry the scan."
      }
      assessed <- status %in%
        c("assessed_no_findings", "assessed_with_findings")
      if (assessed) {
        retained[rows] <- TRUE
      }
      settings <- switch(
        scan,
        repeated_tool_call = list(repeat_threshold = repeat_threshold),
        suspicious_tool_loop = list(loop_threshold = loop_threshold),
        list()
      )
      assessments[[length(assessments) + 1L]] <- tibble::tibble(
        scan_id = scan_id,
        trajectory_id = id,
        scan = scan,
        scan_version = "1",
        status = status,
        required_evidence = scan_required_evidence(scan),
        reason = reason,
        settings = list(settings),
        finding_ids = list(
          if (assessed) findings$finding_id[rows] else character()
        ),
        limitations = list(unique(c(
          "Assessment covers retained records only; complete capture is not established.",
          evidence$limitations,
          if (nrow(scoped_losses)) {
            paste0(
              scoped_losses$field,
              ": ",
              scoped_losses$reason,
              " - ",
              scoped_losses$detail
            )
          }
        ))),
        loss_rows = list(loss_rows)
      )
    }
  }
  list(
    findings = findings[retained, , drop = FALSE],
    assessments = if (length(assessments)) {
      trajectory_bind_rows(assessments)
    } else {
      scan_empty_assessments()
    }
  )
}

scan_empty_assessments <- function() {
  tibble::tibble(
    scan_id = character(),
    trajectory_id = character(),
    scan = character(),
    scan_version = character(),
    status = character(),
    required_evidence = character(),
    reason = character(),
    settings = list(),
    finding_ids = list(),
    limitations = list(),
    loss_rows = list()
  )
}

scan_assessment_group <- function(scan) {
  if (scan %in% c("trajectory_error", "turn_error")) {
    return("record")
  }
  if (scan %in% c("event_error", "error_chain")) {
    return("event")
  }
  "tool"
}

scan_required_evidence <- function(scan) {
  switch(
    scan,
    ambiguous_tool_correlation = "Tool calls/results with recorded call identities.",
    unresolved_tool_call = "Ordered tool calls/results with recorded call identities.",
    unmatched_tool_result = "Ordered tool calls/results with recorded call identities.",
    repeated_tool_call = "Tool calls with recorded names and complete arguments.",
    suspicious_tool_loop = "Ordered tool calls with names and complete arguments; roles for intervening content.",
    event_error = "Recorded event status or explicit failure evidence.",
    error_chain = "Event failure evidence and recorded causal parent relationships.",
    turn_error = "Recorded semantic turn status, finish reason, or explicit error.",
    trajectory_error = "Recorded trajectory status or explicit error."
  )
}

scan_has_value <- function(x) !is.na(x) & nzchar(trimws(x))

scan_status_recorded <- function(x) {
  x %in%
    c("completed", "failed", "cancelled", "interrupted", "running", "pending")
}

scan_assess_evidence <- function(scan, info, turns, events, losses, positive) {
  status <- if (positive) "assessed_with_findings" else "assessed_no_findings"
  reason <- if (positive) {
    "Findings are supported by retained evidence."
  } else {
    "Required evidence is recorded; no findings in this capture."
  }
  limitations <- character()
  insufficient <- function(reason) {
    list(
      status = "insufficient_evidence",
      reason = reason,
      limitations = limitations
    )
  }
  fields <- losses$field
  # Loss paths come from public adapters. Credential or text redaction does not
  # invalidate a status scan, but argument loss invalidates tool comparison.
  structure_loss <- any(grepl(
    "^(turns|events|messages|parts)$|^(read_info|capture)($|[.$])",
    fields
  ))
  group <- scan_assessment_group(scan)
  if (group == "tool") {
    tools <- events$event_type %in% c("tool_call", "tool_result")
    calls <- events$event_type %in% "tool_call"
    if (structure_loss) {
      return(insufficient("Capture losses affect the semantic tool sequence."))
    }
    if (!any(tools)) {
      conversation <- events[events$event_type != "commons:provenance", ]
      semantic <- nrow(turns) > 0L &&
        nrow(conversation) > 0L &&
        all(conversation$event_type == "content") &&
        all(conversation$turn_id %in% turns$turn_id) &&
        all(turns$role %in% c("user", "assistant", "system")) &&
        any(scan_has_value(conversation$text))
      if (!semantic) {
        return(insufficient(
          "No tool exchange or fully interpretable text conversation is recorded."
        ))
      }
      return(list(
        status = "not_applicable",
        reason = "The retained text conversation contains no tool activity.",
        limitations = limitations
      ))
    }
    if (
      scan %in%
        c(
          "ambiguous_tool_correlation",
          "unresolved_tool_call",
          "unmatched_tool_result"
        )
    ) {
      if (
        !all(scan_has_value(events$call_id[tools])) ||
          any(grepl(
            "call_id|tool_call_id|^contents\\$(request\\$)?id$",
            fields
          ))
      ) {
        return(insufficient(
          "Tool call identities are missing or lost; pairing cannot be assessed."
        ))
      }
    } else {
      if (!any(calls)) {
        return(insufficient(
          "Tool results are recorded without the requests needed for argument comparison."
        ))
      }
      missing_args <- vapply(events$value[calls], is.null, logical(1))
      call_losses <- is.na(losses$event_id) |
        losses$event_id %in% events$event_id[calls]
      argument_loss <- any(
        call_losses & grepl("value|arguments|(^|[.$])name($|[.$])", fields)
      )
      if (
        !all(scan_has_value(events$name[calls])) ||
          any(missing_args) ||
          argument_loss
      ) {
        return(insufficient(
          "Tool names or arguments are missing, redacted, or truncated."
        ))
      }
      if (scan == "suspicious_tool_loop") {
        ordered <- order(events$event_index, events$event_id, method = "radix")
        ordered_events <- events[ordered, , drop = FALSE]
        call_positions <- which(ordered_events$event_type == "tool_call")
        between <- seq.int(min(call_positions), max(call_positions))
        content <- between[ordered_events$event_type[between] == "content"]
        roles <- turns$role[match(
          ordered_events$turn_id[content],
          turns$turn_id
        )]
        if (!all(scan_has_value(roles))) {
          return(insufficient(
            "Roles of content between tool calls are not recorded."
          ))
        }
      }
    }
  } else {
    if (scan == "trajectory_error") {
      complete <- scan_status_recorded(info$status) | scan_has_value(info$error)
      relevant_loss <- any(
        grepl("(^|[.$])(status|error)($|[.$])", fields) &
          is.na(losses$event_id) &
          is.na(losses$turn_id)
      )
    } else if (scan == "turn_error") {
      if (!nrow(turns)) {
        return(insufficient("Semantic turns are not recorded."))
      }
      complete <- scan_status_recorded(turns$status) |
        scan_has_value(turns$finish_reason) |
        scan_has_value(turns$error)
      turn_path <- grepl(
        "^turns\\[\\[[0-9]+\\]\\]\\$(status|finish_reason|error)$",
        fields
      )
      turn_owner <- losses$turn_id %in%
        turns$turn_id |
        (is.na(losses$turn_id) & turn_path) |
        (is.na(losses$turn_id) & is.na(losses$trajectory_id))
      turn_field <- fields %in%
        c("status", "finish_reason", "error", "contents$error")
      relevant_loss <- structure_loss ||
        any(
          is.na(losses$event_id) & turn_owner & (turn_path | turn_field)
        )
    } else {
      if (!nrow(events)) {
        return(insufficient("Events with diagnostic status are not recorded."))
      }
      complete <- scan_status_recorded(events$status) |
        scan_event_is_error(events)
      relevant_loss <- structure_loss || any(grepl("status|error", fields))
    }
    if (!all(complete) || relevant_loss) {
      if (!positive) {
        return(insufficient(
          "Required status or failure evidence is missing or affected by capture loss."
        ))
      }
      limitations <- c(
        limitations,
        "Positive failures are recorded, but missing status or capture losses prevent ruling out other failures."
      )
    }
    if (scan == "error_chain") {
      failed <- which(scan_event_is_error(events))
      if (length(failed) < 2L && !positive) {
        return(list(
          status = "not_applicable",
          reason = "Fewer than two failed events are recorded in the assessed capture.",
          limitations = limitations
        ))
      }
      if (
        length(failed) >= 2L &&
          !positive &&
          !all(scan_has_value(events$parent_event_id[failed]))
      ) {
        return(insufficient(
          "Causal parent relationships are missing for one or more failed events."
        ))
      }
      if (any(grepl("parent_event_id", fields))) {
        if (!positive) {
          return(insufficient(
            "Capture losses affect causal parent relationships."
          ))
        }
        limitations <- c(
          limitations,
          "Some parent relationships were lost; additional chains may be unavailable."
        )
      }
    }
  }
  list(status = status, reason = reason, limitations = limitations)
}
