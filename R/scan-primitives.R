#' Filter, summarize, and scan trajectory bundles
#'
#' These functions provide the first composable post-run diagnostics for a
#' [TrajectoryBundle]. `filter_trajectory_events()` selects canonical events,
#' `summarize_trajectories()` returns one summary row per trajectory, and
#' `scan_trajectories()` emits deterministic findings for common tool and
#' error patterns.
#'
#' `filter_trajectory_events()` joins each event to its containing turn for
#' role-based filtering and returns `role`, `turn_index`, and `round_index`
#' alongside the canonical event columns. A named `metadata` list matches
#' top-level event metadata entries using exact R equality.
#'
#' `scan_trajectories()` detects unresolved tool calls, unmatched tool results,
#' repeated tool calls, suspicious consecutive tool-call loops, failed events,
#' and causal error chains. It analyzes a completed snapshot and never calls a
#' model or tool. Finding identifiers are deterministic for an unchanged
#' bundle, argument set, and `scan_id`.
#'
#' @param x A [TrajectoryBundle].
#' @param trajectory_id,role,event_type,content_type,tool,status Optional
#'   character vectors. When supplied, an event must match one of the values
#'   in every supplied filter.
#' @param metadata An optional named list of top-level event metadata values to
#'   match exactly.
#' @param scan_id A non-empty identifier for this diagnostic run.
#' @param repeat_threshold The minimum number of calls with the same tool name
#'   and arguments that produces a `repeated_tool_call` finding.
#' @param loop_threshold The minimum consecutive calls with the same tool name
#'   and arguments that produces a `suspicious_tool_loop` finding. Tool results
#'   between calls do not break a call sequence.
#'
#' @returns
#' - `filter_trajectory_events()` returns a tibble of selected events.
#' - `summarize_trajectories()` returns one tibble row per trajectory.
#' - `scan_trajectories()` returns a tibble with one row per finding and the
#'   event identifiers supporting that finding.
#'
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(
#'     trajectory_id = "trajectory-1",
#'     source_type = "manual"
#'   ),
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
#' summarize_trajectories(bundle)
#' scan_trajectories(bundle)
#'
#' @name trajectory_scan_primitives
NULL

#' @rdname trajectory_scan_primitives
#' @export
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
  out$role <- event_roles[keep]
  out$turn_index <- event_turn_indices[keep]
  out$round_index <- event_round_indices[keep]
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

#' @rdname trajectory_scan_primitives
#' @export
summarize_trajectories <- function(x) {
  check_trajectory_bundle(x)
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  losses <- trajectory_losses(x)

  if (nrow(info) == 0L) {
    return(scan_empty_summaries())
  }

  rows <- lapply(seq_len(nrow(info)), function(index) {
    trajectory <- info[index, , drop = FALSE]
    trajectory_id <- trajectory$trajectory_id[[1L]]
    trajectory_turns <- turns[
      turns$trajectory_id == trajectory_id,
      ,
      drop = FALSE
    ]
    trajectory_events <- events[
      events$trajectory_id == trajectory_id,
      ,
      drop = FALSE
    ]
    trajectory_losses <- losses[
      !is.na(losses$trajectory_id) & losses$trajectory_id == trajectory_id,
      ,
      drop = FALSE
    ]
    relations <- scan_tool_relations(trajectory_events)
    error_events <- scan_event_is_error(trajectory_events)

    tibble::tibble(
      trajectory_id = trajectory_id,
      run_id = trajectory$run_id[[1L]],
      parent_trajectory_id = trajectory$parent_trajectory_id[[1L]],
      source_type = trajectory$source_type[[1L]],
      status = trajectory$status[[1L]],
      trajectory_depth = scan_trajectory_depth(trajectory_id, info),
      max_event_depth = scan_max_event_depth(trajectory_events),
      n_turns = nrow(trajectory_turns),
      n_rounds = length(unique(stats::na.omit(trajectory_turns$round_index))),
      n_events = nrow(trajectory_events),
      n_tool_calls = sum(trajectory_events$event_type == "tool_call"),
      n_tool_results = sum(trajectory_events$event_type == "tool_result"),
      n_unresolved_tool_calls = length(relations$unresolved_calls),
      n_unmatched_tool_results = length(relations$unmatched_results),
      n_error_events = sum(error_events),
      n_failed_turns = sum(
        !is.na(trajectory_turns$status) & trajectory_turns$status == "failed"
      ),
      n_losses = nrow(trajectory_losses),
      input_tokens = scan_sum_known(trajectory_turns$input_tokens),
      output_tokens = scan_sum_known(trajectory_turns$output_tokens),
      cached_input_tokens = scan_sum_known(
        trajectory_turns$cached_input_tokens
      ),
      cost = scan_sum_known(trajectory_turns$cost),
      turn_duration = scan_sum_known(trajectory_turns$duration),
      elapsed = scan_trajectory_elapsed(trajectory)
    )
  })

  do.call(rbind, rows)
}

#' @rdname trajectory_scan_primitives
#' @export
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
  findings <- list()

  for (trajectory_id in info$trajectory_id) {
    trajectory_events <- events[
      events$trajectory_id == trajectory_id,
      ,
      drop = FALSE
    ]
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

scan_check_character_filter <- function(x, arg, call) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (
    !is.character(x) ||
      length(x) == 0L ||
      anyNA(x) ||
      !all(nzchar(x))
  ) {
    scans_abort(
      c(
        "{.arg {arg}} must be a non-empty character vector or {.code NULL}.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_scan_argument",
      call = call,
      .envir = environment()
    )
  }
  invisible(x)
}

scan_check_metadata_filter <- function(x, call) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (
    !is.list(x) ||
      is.null(names(x)) ||
      length(x) == 0L ||
      anyNA(names(x)) ||
      !all(nzchar(names(x))) ||
      anyDuplicated(names(x))
  ) {
    scans_abort(
      "{.arg metadata} must be a non-empty, uniquely named list or {.code NULL}.",
      class = "scans_error_scan_argument",
      call = call
    )
  }
  if (!trajectory_value_is_safe(x)) {
    scans_abort(
      "{.arg metadata} must contain only bounded, serializable values.",
      class = "scans_error_scan_argument",
      call = call
    )
  }
  invisible(x)
}

scan_check_id <- function(x, call) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    scans_abort(
      "{.arg scan_id} must be one non-empty string.",
      class = "scans_error_scan_argument",
      call = call
    )
  }
  invisible(x)
}

scan_check_threshold <- function(x, arg, call) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x != floor(x) ||
      x < 2L ||
      x > .Machine$integer.max
  ) {
    scans_abort(
      "{.arg {arg}} must be one integer greater than or equal to 2.",
      class = "scans_error_scan_argument",
      call = call,
      .envir = environment()
    )
  }
  as.integer(x)
}

scan_filter_match <- function(x, expected) {
  if (is.null(expected)) {
    return(rep(TRUE, length(x)))
  }
  x %in% expected
}

scan_metadata_matches <- function(x, expected) {
  if (!is.list(x) || is.null(names(x))) {
    return(FALSE)
  }
  all(vapply(
    names(expected),
    function(name) name %in% names(x) && identical(x[[name]], expected[[name]]),
    logical(1)
  ))
}

scan_reorder_event_columns <- function(x) {
  derived <- c("role", "turn_index", "round_index")
  canonical <- setdiff(names(x), derived)
  turn_position <- match("turn_id", canonical)
  before <- canonical[seq_len(turn_position)]
  after <- canonical[-seq_len(turn_position)]
  x[c(before, derived, after)]
}

scan_empty_summaries <- function() {
  tibble::tibble(
    trajectory_id = character(),
    run_id = character(),
    parent_trajectory_id = character(),
    source_type = character(),
    status = character(),
    trajectory_depth = integer(),
    max_event_depth = integer(),
    n_turns = integer(),
    n_rounds = integer(),
    n_events = integer(),
    n_tool_calls = integer(),
    n_tool_results = integer(),
    n_unresolved_tool_calls = integer(),
    n_unmatched_tool_results = integer(),
    n_error_events = integer(),
    n_failed_turns = integer(),
    n_losses = integer(),
    input_tokens = double(),
    output_tokens = double(),
    cached_input_tokens = double(),
    cost = double(),
    turn_duration = double(),
    elapsed = double()
  )
}

scan_sum_known <- function(x) {
  if (length(x) == 0L || all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

scan_trajectory_elapsed <- function(trajectory) {
  if (
    is.na(trajectory$started_at[[1L]]) || is.na(trajectory$completed_at[[1L]])
  ) {
    return(NA_real_)
  }
  as.numeric(
    difftime(
      trajectory$completed_at[[1L]],
      trajectory$started_at[[1L]],
      units = "secs"
    )
  )
}

scan_trajectory_depth <- function(trajectory_id, trajectories) {
  depth <- 0L
  current <- trajectory_id
  repeat {
    index <- match(current, trajectories$trajectory_id)
    parent <- trajectories$parent_trajectory_id[[index]]
    if (is.na(parent)) {
      return(depth)
    }
    depth <- depth + 1L
    current <- parent
  }
}

scan_event_depths <- function(events) {
  depths <- integer(nrow(events))
  names(depths) <- events$event_id
  for (event_id in events$event_id) {
    depth <- 0L
    current <- event_id
    repeat {
      index <- match(current, events$event_id)
      parent <- events$parent_event_id[[index]]
      if (is.na(parent)) {
        break
      }
      depth <- depth + 1L
      current <- parent
    }
    depths[[event_id]] <- depth
  }
  unname(depths)
}

scan_max_event_depth <- function(events) {
  if (nrow(events) == 0L) {
    return(0L)
  }
  max(scan_event_depths(events))
}

scan_tool_relations <- function(events) {
  calls <- which(events$event_type == "tool_call")
  results <- which(events$event_type == "tool_result")
  call_ids <- events$call_id[calls]
  result_ids <- events$call_id[results]

  unresolved_calls <- calls[
    is.na(call_ids) | !call_ids %in% result_ids[!is.na(result_ids)]
  ]
  unmatched_results <- results[
    is.na(result_ids) | !result_ids %in% call_ids[!is.na(call_ids)]
  ]
  list(
    calls = calls,
    results = results,
    unresolved_calls = unresolved_calls,
    unmatched_results = unmatched_results
  )
}

scan_event_is_error <- function(events) {
  events$event_type == "error" |
    (!is.na(events$status) & events$status == "failed") |
    (!is.na(events$error) & nzchar(events$error))
}

scan_tool_signature <- function(name, value) {
  name <- if (is.na(name)) "<missing>" else name
  serialized <- serialize(value, NULL, version = 3L)
  paste0(name, "\r", paste(format(serialized), collapse = ""))
}

scan_tool_findings <- function(
  events,
  scan_id,
  repeat_threshold,
  loop_threshold
) {
  relations <- scan_tool_relations(events)
  findings <- list()

  for (index in relations$unresolved_calls) {
    findings[[length(findings) + 1L]] <- scan_new_finding(
      scan_id = scan_id,
      scan = "unresolved_tool_call",
      events = events,
      indices = index,
      severity = "warning",
      label = "Unresolved tool call",
      explanation = "No tool result has the same call identifier.",
      value = list(
        tool = events$name[[index]],
        call_id = events$call_id[[index]]
      ),
      scan_order = 1L
    )
  }
  for (index in relations$unmatched_results) {
    findings[[length(findings) + 1L]] <- scan_new_finding(
      scan_id = scan_id,
      scan = "unmatched_tool_result",
      events = events,
      indices = index,
      severity = "warning",
      label = "Unmatched tool result",
      explanation = "No tool call has the same call identifier.",
      value = list(
        tool = events$name[[index]],
        call_id = events$call_id[[index]]
      ),
      scan_order = 2L
    )
  }

  calls <- relations$calls
  if (length(calls) == 0L) {
    return(findings)
  }
  signatures <- vapply(
    calls,
    \(index) scan_tool_signature(events$name[[index]], events$value[[index]]),
    character(1)
  )

  for (signature in unique(signatures)) {
    indices <- calls[signatures == signature]
    if (length(indices) >= repeat_threshold) {
      findings[[length(findings) + 1L]] <- scan_new_finding(
        scan_id = scan_id,
        scan = "repeated_tool_call",
        events = events,
        indices = indices,
        severity = "warning",
        label = "Repeated tool call",
        explanation = "The same tool and arguments were requested repeatedly.",
        value = list(
          tool = events$name[[indices[[1L]]]],
          count = length(indices),
          call_ids = events$call_id[indices]
        ),
        scan_order = 3L
      )
    }
  }

  run_groups <- scan_tool_run_groups(events, calls, signatures)
  for (positions in run_groups) {
    if (length(positions) < loop_threshold) {
      next
    }
    indices <- calls[positions]
    findings[[length(findings) + 1L]] <- scan_new_finding(
      scan_id = scan_id,
      scan = "suspicious_tool_loop",
      events = events,
      indices = indices,
      severity = "warning",
      label = "Suspicious tool loop",
      explanation = "Consecutive tool calls repeated the same tool and arguments.",
      value = list(
        tool = events$name[[indices[[1L]]]],
        count = length(indices),
        call_ids = events$call_id[indices]
      ),
      scan_order = 4L
    )
  }

  findings
}

scan_tool_run_groups <- function(events, calls, signatures) {
  groups <- integer(length(calls))
  groups[[1L]] <- 1L
  if (length(calls) == 1L) {
    return(list(1L))
  }

  for (index in seq.int(2L, length(calls))) {
    previous <- calls[[index - 1L]]
    current <- calls[[index]]
    between <- if (current - previous > 1L) {
      seq.int(previous + 1L, current - 1L)
    } else {
      integer()
    }
    allowed_between <- length(between) == 0L ||
      all(events$event_type[between] == "tool_result")
    same_signature <- signatures[[index]] == signatures[[index - 1L]]
    groups[[index]] <- groups[[index - 1L]] +
      as.integer(!allowed_between || !same_signature)
  }
  unname(split(seq_along(calls), groups))
}

scan_error_findings <- function(events, scan_id) {
  error_indices <- which(scan_event_is_error(events))
  findings <- lapply(error_indices, function(index) {
    scan_new_finding(
      scan_id = scan_id,
      scan = "event_error",
      events = events,
      indices = index,
      severity = "error",
      label = "Event error",
      explanation = "The event reports an error or failed status.",
      value = list(
        event_type = events$event_type[[index]],
        status = events$status[[index]],
        error = events$error[[index]]
      ),
      scan_order = 5L
    )
  })
  if (length(error_indices) < 2L) {
    return(findings)
  }

  error_ids <- events$event_id[error_indices]
  error_parents <- events$parent_event_id[error_indices]
  leaves <- error_indices[!error_ids %in% error_parents[!is.na(error_parents)]]
  for (leaf in leaves) {
    chain <- leaf
    parent <- events$parent_event_id[[leaf]]
    while (!is.na(parent) && parent %in% error_ids) {
      parent_index <- match(parent, events$event_id)
      chain <- c(parent_index, chain)
      parent <- events$parent_event_id[[parent_index]]
    }
    if (length(chain) < 2L) {
      next
    }
    findings[[length(findings) + 1L]] <- scan_new_finding(
      scan_id = scan_id,
      scan = "error_chain",
      events = events,
      indices = chain,
      severity = "error",
      label = "Causal error chain",
      explanation = "Failed events form a causal parent chain.",
      value = list(length = length(chain)),
      scan_order = 6L
    )
  }
  findings
}

scan_new_finding <- function(
  scan_id,
  scan,
  events,
  indices,
  severity,
  label,
  explanation,
  value,
  scan_order
) {
  primary <- indices[[1L]]
  list(
    scan_id = scan_id,
    scan = scan,
    scan_version = "1",
    trajectory_id = events$trajectory_id[[primary]],
    turn_id = events$turn_id[[primary]],
    event_id = events$event_id[[primary]],
    event_ids = events$event_id[indices],
    severity = severity,
    label = label,
    value = value,
    explanation = explanation,
    metadata = list(),
    .event_index = events$event_index[[primary]],
    .scan_order = scan_order
  )
}

scan_bind_findings <- function(findings, scan_id, trajectories) {
  if (length(findings) == 0L) {
    return(scan_empty_findings())
  }
  trajectory_order <- stats::setNames(
    seq_len(nrow(trajectories)),
    trajectories$trajectory_id
  )
  finding_order <- order(
    unname(trajectory_order[vapply(
      findings,
      `[[`,
      character(1),
      "trajectory_id"
    )]),
    vapply(findings, `[[`, integer(1), ".event_index"),
    vapply(findings, `[[`, integer(1), ".scan_order"),
    method = "radix"
  )
  findings <- findings[finding_order]
  size <- length(findings)

  tibble::tibble(
    finding_id = sprintf("%s/finding-%06d", scan_id, seq_len(size)),
    scan_id = vapply(findings, `[[`, character(1), "scan_id"),
    scan = vapply(findings, `[[`, character(1), "scan"),
    scan_version = vapply(findings, `[[`, character(1), "scan_version"),
    trajectory_id = vapply(findings, `[[`, character(1), "trajectory_id"),
    turn_id = vapply(findings, `[[`, character(1), "turn_id"),
    event_id = vapply(findings, `[[`, character(1), "event_id"),
    event_ids = lapply(findings, `[[`, "event_ids"),
    severity = vapply(findings, `[[`, character(1), "severity"),
    label = vapply(findings, `[[`, character(1), "label"),
    value = lapply(findings, `[[`, "value"),
    explanation = vapply(findings, `[[`, character(1), "explanation"),
    metadata = lapply(findings, `[[`, "metadata")
  )
}

scan_empty_findings <- function() {
  tibble::tibble(
    finding_id = character(),
    scan_id = character(),
    scan = character(),
    scan_version = character(),
    trajectory_id = character(),
    turn_id = character(),
    event_id = character(),
    event_ids = list(),
    severity = character(),
    label = character(),
    value = list(),
    explanation = character(),
    metadata = list()
  )
}
