# Deputy wraps each agent run in a `deputy.run` span (R/agent-tracing.R in
# deputy). Its attributes identify the run and say how it ended, and its span
# events record the run's governance decisions. A subagent's run is filed
# under its lead's conversation id, so a conversation can hold several runs
# whose model calls must not be read as one transcript.

otel_is_deputy_run <- function(span) {
  identical(span$name, "deputy.run")
}

# Mark each span of a conversation that holds a Deputy run with the run it
# belongs to: the nearest `deputy.run` ancestor, found through every span the
# read retained, including ancestry kept only for context beyond the span
# budget. A conversation without a run is returned unchanged.
otel_deputy_assign_runs <- function(spans, index) {
  runs <- vapply(spans, otel_is_deputy_run, logical(1))
  if (!any(runs)) {
    return(spans)
  }
  run_keys <- vapply(
    spans[runs],
    function(span) paste(span$trace_id, span$span_id),
    character(1)
  )
  for (position in seq_along(spans)) {
    key <- otel_deputy_run_key(spans[[position]], index)
    attr(spans[[position]], "otel_deputy_run") <- if (key %in% run_keys) {
      key
    } else {
      NA_character_
    }
  }
  spans
}

otel_deputy_run_key <- function(span, index) {
  current <- span
  for (step in seq_len(64L)) {
    if (otel_is_deputy_run(current)) {
      return(paste(current$trace_id, current$span_id))
    }
    parent <- current$parent_span_id
    if (is.null(parent) || is.na(parent) || !nzchar(parent)) {
      return(NA_character_)
    }
    key <- paste(span$trace_id, parent)
    if (!exists(key, envir = index, inherits = FALSE)) {
      return(NA_character_)
    }
    current <- get(key, envir = index, inherits = FALSE)
  }
  NA_character_
}

# Split a conversation into the lead's trajectory and one per subagent run.
# Root runs (no parent run) are the turns of one conversation and stay
# together: each later model call repeats the history. A run with a parent run
# is a delegated subagent and becomes its own trajectory with its model and
# tool calls. A span no run claims stays with the lead, and is counted so the
# lead's trajectory can report it.
otel_deputy_parts <- function(spans) {
  runs <- Filter(otel_is_deputy_run, spans)
  child <- vapply(
    runs,
    function(run) !is.na(otel_attribute(run, "deputy.parent.run.id")),
    logical(1)
  )
  if (!any(child)) {
    return(list(list(spans = spans, runs = runs, run_id = NULL)))
  }
  context <- attr(spans, "otel_context", exact = TRUE)
  index <- otel_span_index(spans)
  child_keys <- vapply(
    runs[child],
    function(run) paste(run$trace_id, run$span_id),
    character(1)
  )
  owner <- vapply(
    spans,
    function(span) {
      key <- attr(span, "otel_deputy_run", exact = TRUE) %||%
        otel_deputy_run_key(span, index)
      if (is.na(key)) NA_character_ else key
    },
    character(1)
  )
  owner[!owner %in% child_keys & !is.na(owner)] <- "lead"
  unassigned <- is.na(owner) &
    vapply(
      spans,
      function(span) otel_is_chat_span(span) || otel_is_tool_span(span),
      logical(1)
    )
  owner[is.na(owner)] <- "lead"

  lead <- spans[owner == "lead"]
  attr(lead, "otel_context") <- context
  # A read holding only delegated runs has no lead to describe.
  parts <- if (length(lead) > 0L) {
    list(list(
      spans = lead,
      runs = runs[!child],
      run_id = NULL,
      unassigned = sum(unassigned)
    ))
  }
  for (position in seq_along(child_keys)) {
    part <- spans[owner == child_keys[[position]]]
    attr(part, "otel_context") <- context
    run <- runs[child][[position]]
    parts[[length(parts) + 1L]] <- list(
      spans = part,
      runs = list(run),
      run_id = otel_attribute(run, "deputy.run.id") %|na|% run$span_id
    )
  }
  parts
}

# The run reasons deputy maps to each ending (R/run-ending.R in deputy).
otel_deputy_limit_reasons <- c(
  "request_limit",
  "tool_call_limit",
  "input_token_limit",
  "output_token_limit",
  "total_token_limit",
  "cost_limit",
  "cost_unavailable",
  "tool_loop"
)
otel_deputy_failure_reasons <- c(
  "error",
  "provider_error",
  "file_checkpoint_error",
  "delegation_error",
  "batch_error",
  "setup_error",
  "setup_failed"
)
otel_deputy_cut_off_reasons <- c(
  "max_tokens",
  "context_window",
  "content_filter"
)

# The run's ending in the ten-field shape `as_trajectory_deputy()` records.
# The span carries the stop reason, the final finish reason, and whether the
# answer was cut off. Deputy ends a run with reason "complete" only as
# answered or cut off, and with any other reason it exports by name (other
# than a limit or failure) only as stopped. A limit or failure reason may
# also be a caller's or hook's stop that reused the word, and "custom" hides
# the reason, so those leave `class` unknown. The span records no limit
# figures, wrap-up, failure phase, or who stopped the run.
otel_deputy_ending <- function(run) {
  stop_reason <- otel_attribute(run, "deputy.stop_reason")
  finish_reason <- otel_attribute(run, "deputy.finish_reason")
  cut_off <- otel_logical(run$attributes[["deputy.cut_off"]])
  class <- NA_character_
  reason <- stop_reason
  if (identical(stop_reason, "complete")) {
    cut <- isTRUE(cut_off) || finish_reason %in% otel_deputy_cut_off_reasons
    class <- if (cut) "RunEndingCutOff" else "RunEndingAnswered"
    cut_off <- if (cut) TRUE else cut_off
    reason <- NA_character_
  } else if (
    !is.na(stop_reason) &&
      !stop_reason %in%
        c("custom", otel_deputy_limit_reasons, otel_deputy_failure_reasons)
  ) {
    class <- "RunEndingStopped"
  }
  if (identical(stop_reason, "custom")) {
    reason <- NA_character_
  }
  ending <- deputy_ending_metadata(NULL, finish_reason, cut_off)
  ending$class <- class
  ending$reason <- reason
  ending
}

otel_logical <- function(x) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    return(x)
  }
  if (
    is.character(x) && length(x) == 1L && tolower(x) %in% c("true", "false")
  ) {
    return(identical(tolower(x), "true"))
  }
  NA
}

# Run identity, ending, status, governance events, and lineage losses for a
# trajectory's Deputy runs; NULL when it has none. The latest run speaks for
# the trajectory, as the latest model call does for its transcript.
otel_deputy_tables <- function(runs, trajectory_id, offset, unassigned = 0L) {
  if (length(runs) == 0L) {
    return(NULL)
  }
  starts <- vapply(runs, function(run) otel_nanos(run$start_time), numeric(1))
  starts[!is.finite(starts)] <- -Inf
  runs <- runs[order(starts)]
  run <- runs[[length(runs)]]
  ending <- otel_deputy_ending(run)
  stop_reason <- otel_attribute(run, "deputy.stop_reason")
  ids <- trajectory_ids(trajectory_id)
  string <- function(key) {
    value <- otel_attribute(run, key)
    if (is.na(value)) NULL else value
  }
  metadata <- list(
    session_id = string("deputy.session.id"),
    agent_id = string("deputy.agent.id"),
    parent_run_id = string("deputy.parent.run.id"),
    delegation_id = string("deputy.delegation.id"),
    stop_reason = string("deputy.stop_reason"),
    ending = ending
  )
  losses <- list()
  if (!is.null(metadata$parent_run_id)) {
    losses[[length(losses) + 1L]] <- trajectory_new_loss(
      ids,
      "parent_run_id",
      "unsupported",
      paste(
        "The parent run ID was retained as metadata because the parent run",
        "was not in the same read"
      )
    )
  }
  if (unassigned > 0L) {
    losses[[length(losses) + 1L]] <- trajectory_new_loss(
      ids,
      "deputy.run",
      "unsupported",
      paste(
        "Model or tool spans whose ancestry reaches no Deputy run were kept",
        "with the lead run"
      ),
      metadata = list(spans = as.integer(unassigned))
    )
  }
  events <- otel_deputy_events(runs, trajectory_id, offset)
  list(
    run_id = otel_attribute(run, "deputy.run.id"),
    run_ids = vapply(
      runs,
      function(r) otel_attribute(r, "deputy.run.id"),
      character(1)
    ),
    status = if (is.na(stop_reason)) {
      NULL
    } else {
      deputy_ending_status(ending$class, stop_reason)
    },
    metadata = metadata[!vapply(metadata, is.null, logical(1))],
    events = events$events,
    losses = c(losses, events$losses)
  )
}

# Deputy exports nine of its event types as `deputy.<type>` span events, with
# only identifiers and decisions as attributes. Each becomes a `deputy:<type>`
# event as `as_trajectory_deputy()` names it; `tool_call_id` is the call ID,
# the other attributes stay in `metadata`, and the status follows the same
# rule (a denied permission or an invalid structured attempt failed).
otel_deputy_events <- function(runs, trajectory_id, offset) {
  events <- unlist(
    lapply(runs, function(run) {
      Filter(function(event) startsWith(event$name, "deputy."), run$events)
    }),
    recursive = FALSE
  )
  if (length(events) == 0L) {
    return(list(events = NULL, losses = list()))
  }
  times <- vapply(events, function(event) otel_nanos(event$time), numeric(1))
  events <- events[order(times)]
  rows <- vector("list", length(events))
  losses <- list()
  for (position in seq_along(events)) {
    event <- events[[position]]
    index <- offset + position
    event_id <- trajectory_event_id(trajectory_id, index)
    attributes <- event$attributes
    # OTLP carries integers as strings and booleans as JSON booleans.
    attributes$valid <- if (!is.null(attributes$valid)) {
      otel_logical(attributes$valid)
    }
    for (key in intersect(c("attempt", "fallback_index"), names(attributes))) {
      attributes[[key]] <- suppressWarnings(as.integer(attributes[[key]]))
    }
    type <- sub("^deputy[.]", "", event$name)
    safe <- trajectory_sanitize_metadata(
      attributes[setdiff(names(attributes), "tool_call_id")],
      paste0("events[[", position, "]]$metadata"),
      trajectory_ids(trajectory_id, event_id = event_id)
    )
    losses <- c(losses, safe$losses)
    row <- otel_event_row(trajectory_id, NA_character_, event_id, index, NA)
    row$event_type <- paste0("deputy:", deputy_event_type(type, position))
    row$call_id <- deputy_canonical_string(attributes$tool_call_id)
    row$timestamp <- otel_nanos(event$time) / 1e9
    row$status <- deputy_event_status(c(list(type = type), attributes))
    row$metadata <- safe$value
    rows[[position]] <- row
  }
  list(events = otel_rows_table(rows, "events"), losses = losses)
}
