# Deputy run diagnostics read only the canonical bundle: the run ending in
# `metadata$ending` and `deputy:*` lifecycle events. Any adapter that projects
# that shape, such as OTel `deputy.run` spans, is assessed the same way.

scan_compaction_threshold <- 2L

scan_cut_off_finish_reasons <- c(
  "max_tokens",
  "context_window",
  "content_filter"
)

scan_run_ending <- function(metadata) {
  ending <- if (is.list(metadata)) metadata$ending
  if (!is.list(ending) || is.null(names(ending))) {
    return(NULL)
  }
  ending
}

scan_ending_string <- function(ending, name) {
  value <- ending[[name]]
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    value
  } else {
    NA_character_
  }
}

scan_ending_flag <- function(ending, name) {
  value <- ending[[name]]
  if (rlang::is_bool(value)) value else NA
}

scan_ending_number <- function(ending, name) {
  value <- ending[[name]]
  if (is.numeric(value) && length(value) == 1L) as.double(value) else NA_real_
}

scan_is_cut_off <- function(ending) {
  finish_reason <- scan_ending_string(ending, "finish_reason")
  isTRUE(scan_ending_flag(ending, "cut_off")) ||
    scan_turn_is_truncated(finish_reason) ||
    tolower(trimws(finish_reason)) %in% scan_cut_off_finish_reasons
}

scan_is_deputy_event <- function(event_type) {
  !is.na(event_type) & startsWith(event_type, "deputy:")
}

scan_has_deputy_evidence <- function(info, events) {
  identical(info$source_type, "deputy") ||
    !is.null(scan_run_ending(info$metadata[[1L]])) ||
    !is.null(scan_delegation(info$metadata[[1L]])) ||
    any(scan_is_deputy_event(events$event_type))
}

# The delegation record on a child trajectory converted from a Deputy
# delegation view, or NULL for any other trajectory.
scan_delegation <- function(metadata) {
  delegation <- if (is.list(metadata)) metadata$delegation
  if (!is.list(delegation) || is.null(names(delegation))) {
    return(NULL)
  }
  delegation
}

scan_delegation_claims <- c(
  delegation_missing_evidence = "missing_evidence",
  delegation_unresolved_work = "unresolved_work"
)

# Each claim is Deputy's newline-joined string: "" when the subagent reported
# none, NA when it did not report.
scan_delegation_claim <- function(delegation, name) {
  claims <- delegation$claims
  if (!is.list(claims)) NA_character_ else scan_ending_string(claims, name)
}

scan_delegation_findings <- function(info_row, scan_id) {
  metadata <- info_row$metadata[[1L]]
  delegation <- scan_delegation(metadata)
  if (is.null(delegation)) {
    return(list())
  }
  labels <- c(
    delegation_missing_evidence = "Delegation missing evidence",
    delegation_unresolved_work = "Delegation unresolved work"
  )
  explanations <- c(
    delegation_missing_evidence = "The subagent reported evidence it could not find.",
    delegation_unresolved_work = "The subagent reported work it left unfinished."
  )
  findings <- list()
  for (kind in names(scan_delegation_claims)) {
    claim <- scan_delegation_claim(delegation, scan_delegation_claims[[kind]])
    if (!scan_has_value(claim)) {
      next
    }
    findings <- c(
      findings,
      list(scan_intervention_record_finding(
        scan_id,
        info_row$trajectory_id[[1L]],
        labels[[kind]],
        explanations[[kind]],
        list(
          kind = kind,
          parent_trajectory_id = info_row$parent_trajectory_id[[1L]] %||%
            NA_character_,
          delegation_id = scan_ending_string(metadata, "delegation_id"),
          claim = claim
        ),
        scan_order = 4L
      ))
    )
  }
  findings
}

scan_deputy_findings <- function(info_row, events, scan_id) {
  ending <- scan_run_ending(info_row$metadata[[1L]])
  trajectory_id <- info_row$trajectory_id[[1L]]
  findings <- list()
  if (!is.null(ending) && scan_is_cut_off(ending)) {
    findings[[1L]] <- scan_new_record_finding(
      scan_id = scan_id,
      scan = "cut_off_answer",
      trajectory_id = trajectory_id,
      turn_id = NA_character_,
      severity = "error",
      label = "Cut-off answer",
      explanation = paste(
        "The provider cut the final answer short; a completed status does",
        "not mean the answer is whole."
      ),
      value = list(
        status = info_row$status[[1L]],
        ending_class = scan_ending_string(ending, "class"),
        finish_reason = scan_ending_string(ending, "finish_reason"),
        cut_off = scan_ending_flag(ending, "cut_off")
      ),
      scan_order = 2L
    )
  }
  c(
    findings,
    scan_delegation_findings(info_row, scan_id),
    scan_intervention_findings(events, ending, trajectory_id, scan_id)
  )
}

# The run ID Deputy stamps on each lifecycle event, or NA when not recorded.
scan_event_run <- function(events) {
  vapply(
    events$metadata,
    function(metadata) {
      if (is.list(metadata)) {
        scan_ending_string(metadata, "run_id")
      } else {
        NA_character_
      }
    },
    character(1)
  )
}

# Compaction events grouped by the run they belong to. A trajectory can hold
# several runs (the root runs of one traced conversation), so its compactions
# count toward the threshold only within a run. When several runs are
# recorded, a compaction without a run ID belongs to none of them; `unknown`
# says such events could still complete a repeat.
scan_compaction_runs <- function(events) {
  type <- events$event_type
  compactions <- which(type %in% "deputy:compaction")
  run <- scan_event_run(events)
  several <- length(unique(stats::na.omit(run[scan_is_deputy_event(type)]))) >
    1L ||
    sum(type %in% "deputy:start") > 1L ||
    sum(type %in% "deputy:stop") > 1L
  if (!several) {
    return(list(runs = list(compactions), unknown = FALSE))
  }
  known <- compactions[!is.na(run[compactions])]
  list(
    runs = unname(split(known, factor(run[known], unique(run[known])))),
    unknown = length(known) < length(compactions) &&
      length(compactions) >= scan_compaction_threshold
  )
}

scan_intervention_record_finding <- function(
  scan_id,
  trajectory_id,
  label,
  explanation,
  value,
  scan_order
) {
  scan_new_record_finding(
    scan_id = scan_id,
    scan = "run_intervention",
    trajectory_id = trajectory_id,
    turn_id = NA_character_,
    severity = "warning",
    label = label,
    explanation = explanation,
    value = value,
    scan_order = scan_order
  )
}

scan_intervention_finding <- function(
  scan_id,
  events,
  indices,
  label,
  explanation,
  value
) {
  scan_new_finding(
    scan_id = scan_id,
    scan = "run_intervention",
    events = events,
    indices = indices,
    severity = "warning",
    label = label,
    explanation = explanation,
    value = value,
    scan_order = 8L
  )
}

scan_intervention_findings <- function(events, ending, trajectory_id, scan_id) {
  type <- events$event_type
  status <- events$status
  meta <- function(index, name) {
    value <- events$metadata[[index]][[name]]
    if (is.null(value) || length(value) != 1L || is.list(value)) NA else value
  }
  findings <- list()

  wrap_up <- which(type == "deputy:limit_wrap_up")
  limited <- !is.null(ending) &&
    identical(scan_ending_string(ending, "class"), "RunEndingLimit")
  wrapped_up <- if (limited) scan_ending_flag(ending, "wrapped_up") else NA
  limit_value <- function(kind) {
    list(
      kind = kind,
      reason = scan_ending_string(ending, "reason"),
      field = scan_ending_string(ending, "field"),
      actual = scan_ending_number(ending, "actual"),
      limit = scan_ending_number(ending, "limit"),
      wrapped_up = scan_ending_flag(ending, "wrapped_up")
    )
  }
  if (length(wrap_up) == 0L && isFALSE(wrapped_up)) {
    findings <- c(
      findings,
      list(scan_intervention_record_finding(
        scan_id,
        trajectory_id,
        "Limit stop",
        "A usage limit stopped the run before the model gave an answer.",
        limit_value("limit_stop"),
        scan_order = 3L
      ))
    )
  }
  if (length(wrap_up) > 0L || isTRUE(wrapped_up)) {
    explanation <- paste(
      "A usage limit was reached, so the model gave its answer in one more",
      "request without tools."
    )
    value <- limit_value("limit_wrap_up")
    if (length(wrap_up) > 0L) {
      if (is.na(value$reason)) {
        value$reason <- as.character(meta(wrap_up[[1L]], "reason"))
      }
      findings <- c(
        findings,
        list(scan_intervention_finding(
          scan_id,
          events,
          wrap_up,
          "Limit wrap-up",
          explanation,
          value
        ))
      )
    } else {
      findings <- c(
        findings,
        list(scan_intervention_record_finding(
          scan_id,
          trajectory_id,
          "Limit wrap-up",
          explanation,
          value,
          scan_order = 3L
        ))
      )
    }
  }

  for (index in which(type == "deputy:fallback")) {
    condition <- events$metadata[[index]]$condition
    findings <- c(
      findings,
      list(scan_intervention_finding(
        scan_id,
        events,
        index,
        "Provider fallback",
        "A provider error made a fallback chat take over the run.",
        list(
          kind = "provider_fallback",
          fallback_index = meta(index, "fallback_index"),
          model = meta(index, "model"),
          error = if (is.list(condition)) {
            scan_ending_string(condition, "message")
          } else {
            NA_character_
          }
        )
      ))
    )
  }

  for (index in which(type == "deputy:compaction_error")) {
    findings <- c(
      findings,
      list(scan_intervention_finding(
        scan_id,
        events,
        index,
        "Compaction failed",
        "Automatic context compaction failed, so context may have been lost.",
        list(
          kind = "compaction_failure",
          phase = meta(index, "phase"),
          error = events$error[[index]]
        )
      ))
    )
  }

  for (run in scan_compaction_runs(events)$runs) {
    if (length(run) < scan_compaction_threshold) {
      next
    }
    findings <- c(
      findings,
      list(scan_intervention_finding(
        scan_id,
        events,
        run,
        "Repeated compaction",
        "Context was compacted repeatedly in one run, a sign of sustained context pressure.",
        list(kind = "repeated_compaction", count = length(run))
      ))
    )
  }

  denied <- which(type == "deputy:permission" & status %in% "failed")
  for (index in denied) {
    findings <- c(
      findings,
      list(scan_intervention_finding(
        scan_id,
        events,
        index,
        "Tool permission denied",
        "A permission policy or hook denied a tool call.",
        list(
          kind = "permission_denied",
          call_id = events$call_id[[index]]
        )
      ))
    )
  }

  pending <- which(type == "deputy:approval" & status %in% "pending")
  for (index in pending) {
    if (scan_approval_resolved(events, index)) {
      next
    }
    findings <- c(
      findings,
      list(scan_intervention_finding(
        scan_id,
        events,
        index,
        "Approval pending",
        "The run suspended for a tool approval that this run never resolved.",
        list(
          kind = "approval_pending",
          tool = events$name[[index]],
          call_id = events$call_id[[index]],
          approval_id = as.character(meta(index, "approval_id"))
        )
      ))
    )
  }
  findings
}

# Deputy records an approval event only when a run suspends, always as
# pending; `as_trajectory_deputy()` gives any other approval or permission
# event a completed or failed status.
scan_decided_statuses <- c("completed", "failed", "cancelled")

# Only later lifecycle events count: the adapter places content events before
# lifecycle events, so their order relative to an approval is not recorded.
# The call starting or ending shows it was decided; a later approval or
# permission event does only with a recorded decision, not an unrecorded,
# running, or pending status.
scan_approval_resolved <- function(events, index) {
  call_id <- events$call_id[[index]]
  if (is.na(call_id)) {
    return(FALSE)
  }
  later <- seq_len(nrow(events)) > index &
    events$call_id %in% call_id &
    (events$event_type %in%
      c("deputy:tool_start", "deputy:tool_end") |
      (events$event_type %in%
        c("deputy:permission", "deputy:approval") &
        events$status %in% scan_decided_statuses))
  any(later)
}
