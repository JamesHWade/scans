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

scan_check_turn_context_columns <- function(events, call) {
  reserved <- c(".turn_role", ".turn_index", ".round_index")
  collisions <- intersect(names(events), reserved)
  if (length(collisions) > 0L) {
    scans_abort(
      c(
        "Can't add derived turn context to the filtered events.",
        "x" = "Source event columns use reserved names: {.field {collisions}}."
      ),
      class = "scans_error_scan_column_collision",
      call = call
    )
  }
  invisible(events)
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
  derived <- c(".turn_role", ".turn_index", ".round_index")
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
    n_ambiguous_tool_correlations = integer(),
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

scan_group_count <- function(include, groups, size) {
  tabulate(groups[include], nbins = size)
}

scan_group_distinct_count <- function(x, groups, size) {
  known <- !is.na(x) & !is.na(groups)
  if (!any(known)) {
    return(integer(size))
  }
  pairs <- data.frame(
    group = groups[known],
    value = x[known]
  )
  first <- !duplicated(pairs)
  tabulate(pairs$group[first], nbins = size)
}

scan_group_max <- function(x, groups, size) {
  out <- integer(size)
  if (length(x) == 0L) {
    return(out)
  }
  maxima <- tapply(x, groups, max)
  out[as.integer(names(maxima))] <- as.integer(maxima)
  out
}

scan_group_sum <- function(x, groups, size) {
  out <- rep(NA_real_, size)
  known <- !is.na(x) & !is.na(groups)
  if (!any(known)) {
    return(out)
  }
  sums <- rowsum(x[known], groups[known], reorder = FALSE)
  out[as.integer(rownames(sums))] <- sums[, 1L]
  out
}

scan_split_trajectory_rows <- function(data, trajectory_ids) {
  positions <- split(
    seq_len(nrow(data)),
    match(data$trajectory_id, trajectory_ids)
  )
  lapply(seq_along(trajectory_ids), function(index) {
    rows <- positions[[as.character(index)]]
    if (is.null(rows)) {
      rows <- integer()
    }
    data[rows, , drop = FALSE]
  })
}

scan_parent_depths <- function(ids, parent_ids) {
  size <- length(ids)
  depths <- rep(NA_integer_, size)
  parents <- match(parent_ids, ids)
  path <- integer(size)

  for (start in seq_len(size)) {
    if (!is.na(depths[[start]])) {
      next
    }
    path_size <- 0L
    current <- start
    while (!is.na(current) && is.na(depths[[current]])) {
      path_size <- path_size + 1L
      path[[path_size]] <- current
      current <- parents[[current]]
    }

    next_depth <- if (is.na(current)) 0L else depths[[current]] + 1L
    for (position in rev(seq_len(path_size))) {
      depths[[path[[position]]]] <- next_depth
      next_depth <- next_depth + 1L
    }
  }
  depths
}

scan_tool_relations <- function(events) {
  scan_tool_relation_indices(events$event_type, events$call_id)
}

scan_tool_relation_counts <- function(
  event_type,
  call_id,
  groups,
  size,
  event_index
) {
  out <- list(
    unresolved = integer(size),
    unmatched = integer(size),
    ambiguous = integer(size)
  )
  tool_rows <- which(event_type %in% c("tool_call", "tool_result"))
  positions <- split(tool_rows, groups[tool_rows])
  for (group in names(positions)) {
    rows <- positions[[group]]
    rows <- rows[order(event_index[rows], method = "radix")]
    relations <- scan_tool_relation_indices(event_type[rows], call_id[rows])
    index <- as.integer(group)
    out$unresolved[[index]] <- length(relations$unresolved_calls)
    out$unmatched[[index]] <- length(relations$unmatched_results)
    out$ambiguous[[index]] <- length(relations$ambiguous)
  }
  out
}

scan_tool_relation_indices <- function(event_type, call_id) {
  calls <- which(event_type == "tool_call")
  results <- which(event_type == "tool_result")
  call_ids <- call_id[calls]
  result_ids <- call_id[results]

  unresolved_calls <- calls[is.na(call_ids)]
  unmatched_results <- results[is.na(result_ids)]
  ambiguous <- list()
  ids <- unique(c(call_ids[!is.na(call_ids)], result_ids[!is.na(result_ids)]))

  for (call_id in ids) {
    matching_calls <- calls[!is.na(call_ids) & call_ids == call_id]
    matching_results <- results[!is.na(result_ids) & result_ids == call_id]
    pending_calls <- integer()
    positions <- sort(c(matching_calls, matching_results))
    for (position in positions) {
      if (event_type[[position]] == "tool_call") {
        pending_calls <- c(pending_calls, position)
      } else if (length(pending_calls) == 0L) {
        unmatched_results <- c(unmatched_results, position)
      } else {
        pending_calls <- pending_calls[-1L]
      }
    }
    unresolved_calls <- c(unresolved_calls, pending_calls)
    if (length(matching_calls) > 1L || length(matching_results) > 1L) {
      ambiguous[[length(ambiguous) + 1L]] <- list(
        call_id = call_id,
        indices = sort(c(matching_calls, matching_results)),
        n_calls = length(matching_calls),
        n_results = length(matching_results)
      )
    }
  }
  list(
    calls = calls,
    results = results,
    unresolved_calls = sort(unresolved_calls),
    unmatched_results = sort(unmatched_results),
    ambiguous = ambiguous
  )
}

scan_event_is_error <- function(events) {
  events$event_type == "error" |
    (!is.na(events$status) & events$status == "failed") |
    (!is.na(events$error) & nzchar(events$error))
}

scan_tool_signature <- function(name, value) {
  name <- if (is.na(name)) "<missing>" else name
  value <- scan_canonicalize_value(value)
  serialized <- serialize(value, NULL, version = 3L)
  paste0(name, "\r", paste(format(serialized), collapse = ""))
}

scan_canonicalize_value <- function(x) {
  if (is.list(x) && !is.data.frame(x) && !is.object(x)) {
    x <- lapply(x, scan_canonicalize_value)
  }

  element_names <- names(x)
  mapping <- !is.null(element_names) &&
    length(element_names) > 0L &&
    !anyNA(element_names) &&
    all(nzchar(element_names)) &&
    !anyDuplicated(element_names)
  if (mapping && !is.object(x)) {
    x <- x[order(element_names, method = "radix")]
  }
  x
}

scan_tool_findings <- function(
  events,
  scan_id,
  repeat_threshold,
  loop_threshold
) {
  relations <- scan_tool_relations(events)
  findings <- list()

  for (ambiguous in relations$ambiguous) {
    findings[[length(findings) + 1L]] <- scan_new_finding(
      scan_id = scan_id,
      scan = "ambiguous_tool_correlation",
      events = events,
      indices = ambiguous$indices,
      severity = "warning",
      label = "Ambiguous tool correlation",
      explanation = paste(
        "The call identifier is reused, so requests and results cannot be",
        "paired uniquely."
      ),
      value = list(
        call_id = ambiguous$call_id,
        n_calls = ambiguous$n_calls,
        n_results = ambiguous$n_results
      ),
      scan_order = 1L
    )
  }

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
      scan_order = 2L
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
      scan_order = 3L
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
        scan_order = 4L
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
      scan_order = 5L
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
      scan_order = 6L
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
      scan_order = 7L
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
