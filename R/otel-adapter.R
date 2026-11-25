#' Convert OpenTelemetry GenAI spans
#'
#' @description
#' `as_trajectory_otel()` builds a [TrajectoryBundle] from OpenTelemetry spans
#' that follow the GenAI semantic conventions, as emitted by \pkg{ellmer} and
#' recorded by Posit Connect's content observability store or written to local
#' OTLP files. Read those spans with [read_connect_traces()] or
#' [read_otel_traces()].
#'
#' Each conversation is reconstructed from its most recent `chat` span:
#' provider instrumentation records the whole message history on every model
#' call, so the last call carries the complete exchange. `execute_tool` spans
#' contribute the timing and failure of each tool call, which the message
#' history alone does not record.
#'
#' A shinychat conversation-titling call (system instructions beginning
#' "You title chat conversations.") is never chosen over another model call,
#' even when it started last. Its usage stays in the conversation's totals in
#' `metadata$otel`, and `metadata$otel$title_calls` reports how many title
#' calls there were and their tokens.
#'
#' When the chosen span records its output but omits `gen_ai.input.messages`,
#' as ellmer does when a turn holds content it cannot serialize, the history
#' is recovered from the latest earlier model call in the conversation that
#' recorded its input, followed by the output of each later call. A span with
#' neither attribute had message capture turned off and is left contentless.
#' Messages that no span recorded
#' are reported as `capture$input` or `capture$tool_result` losses, so
#' [assess_trajectory_scans()] does not treat the transcript as complete.
#'
#' @section commons agents:
#' commons wraps each turn in a `commons_conversation_turn` span and records
#' each answer's provenance in a `commons_provenance` span. These spans are kept
#' with their conversation, never counted against a reader's span budget, and
#' never used as a transcript, so token totals, model calls, and resource
#' measures are unchanged by them. Each exchange of the transcript becomes a
#' `"commons:provenance"` event in the form [as_trajectory_commons()] gives:
#' `value` holds `provenance_tag` and the full `citation_decisions` parsed
#' from `commons.citation.candidates` (sanitized and bounded by the payload
#' limit, with a loss when truncated), `metadata$exchange_index` numbers the
#' exchange, and each turn's `round_index` is its exchange. As in commons, a
#' user turn opens an exchange unless it returns tool results or is an
#' internal reminder; reminder turns carry `commons_internal = TRUE` in their
#' metadata. Unlike the commons reader, `timestamp` is the time the record's
#' span ended. A record attaches to an exchange only when the history its turn
#' recorded matches the transcript, and conflicting records for one exchange
#' leave it unclassified with a loss. The Connect user commons records
#' (`commons.connect.user`) is kept as `metadata$otel$user`. commons setup
#' spans such as `commons_agent_create` belong to no conversation and are
#' ignored.
#'
#' @section Deputy agents:
#' Deputy wraps each agent run in a `deputy.run` span. Like commons' spans it
#' is kept with its conversation, never counted against a span budget, and
#' never used as a transcript or a measure. A conversation's root runs, the
#' turns of one chat, stay one trajectory. Each subagent run (one with
#' `deputy.parent.run.id`) becomes its own trajectory, holding the model and
#' tool calls under its run span, with the ID `otel/<conversation>/<run ID>`
#' and `parent_trajectory_id` set when its parent run is in the same input.
#' Otherwise the parent run ID stays in metadata with a `parent_run_id` loss.
#' A model or tool call that no run's ancestry reaches stays with the lead and
#' is reported with a `deputy.run` loss.
#'
#' The latest run speaks for its trajectory. `run_id` is its `deputy.run.id`,
#' and metadata carries `session_id`, `agent_id`, `parent_run_id`,
#' `delegation_id`, `stop_reason`, and the ten-field `ending` described in
#' [as_trajectory_deputy()]. The span records only the stop reason, the final
#' finish reason, and whether the answer was cut off, so `by`, `field`,
#' `actual`, `limit`, `wrapped_up`, and `phase` are `NA`. `class` is
#' `"RunEndingAnswered"` or `"RunEndingCutOff"` for stop reason `"complete"`,
#' `"RunEndingStopped"` for any other reason Deputy exports by name except a
#' limit or failure reason, and `NA` for those and for `"custom"`, because a
#' caller or hook can stop a run with a limit's or failure's own word. The
#' trajectory `status` then follows the Deputy adapter's mapping from `class`,
#' or from the stop reason when `class` is `NA`.
#'
#' Deputy exports nine event types as span events, each with only
#' identifiers and decisions. They become events named as the Deputy adapter
#' names the same events, timestamped from the span event:
#'
#' | Span event | Bundle event | Notes |
#' | --- | --- | --- |
#' | `deputy.permission` | `deputy:permission` | `call_id` from `tool_call_id`; `"failed"` when `decision` is `"deny"` |
#' | `deputy.structured_attempt` | `deputy:structured_attempt` | `"failed"` when `valid` is false; no attempted value |
#' | `deputy.fallback` | `deputy:fallback` | `fallback_index` |
#' | `deputy.compaction` | `deputy:compaction` | successful compactions only |
#' | `deputy.hook` | `deputy:hook` | `hook`, `decision` |
#' | `deputy.file_checkpoint` | `deputy:file_checkpoint` | `checkpoint_id` |
#' | `deputy.delegation` | `deputy:delegation` | `delegation_id` |
#' | `deputy.warning` | `deputy:warning` | `code`; no message |
#' | `deputy.stop` | `deputy:stop` | |
#'
#' Other attributes stay in event `metadata`. Deputy does not export its other
#' event types, so OTel bundles have no `deputy:approval`,
#' `deputy:limit_wrap_up`, `deputy:compaction_error`, tool, request, or
#' content events, and no tool names, inputs, results, or error messages.
#' Conversations without a `deputy.run` span are unaffected.
#'
#' Spans are grouped into conversations by the `gen_ai.conversation.id`
#' attribute, walking up the span's ancestry to find it. A conversation
#' without that attribute anywhere in its ancestry falls back to its trace id,
#' which groups a single model call rather than a session.
#'
#' @param x A list of conversations, as returned by [read_connect_traces()]
#'   or [read_otel_traces()], or a flat list of spans to be grouped.
#' @param trajectory_id Optional trajectory identifier. Must be `NULL` when
#'   `x` holds more than one conversation.
#' @param source_uri Optional URI recording where the spans were read from.
#' @param metadata Optional named list attached to every trajectory.
#'
#' @returns A [TrajectoryBundle].
#'
#' @seealso [read_connect_traces()] to fetch spans from Posit Connect, and
#'   [read_otel_traces()] to read local trace files.
#' @export
as_trajectory_otel <- function(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
) {
  call <- rlang::caller_env()
  otel_check_jsonlite(call)
  if (!is.list(x)) {
    scans_abort(
      "{.arg x} must be a list of OpenTelemetry spans or conversations.",
      class = "scans_error_otel_source",
      call = call
    )
  }
  conversations <- if (otel_is_span_list(x)) {
    otel_group_conversations(x)
  } else {
    x
  }
  if (length(conversations) == 0L) {
    return(TrajectoryBundle(
      otel_empty_table("trajectories"),
      otel_empty_table("turns"),
      otel_empty_table("events")
    ))
  }
  ids <- names(conversations) %||% seq_along(conversations)
  parts <- unlist(
    Map(
      function(spans, id) {
        lapply(otel_deputy_parts(spans), function(part) c(part, list(id = id)))
      },
      conversations,
      ids
    ),
    recursive = FALSE
  )
  if (!is.null(trajectory_id) && length(parts) > 1L) {
    scans_abort(
      "{.arg trajectory_id} must be {.code NULL} for multiple conversations.",
      class = "scans_error_otel_source",
      call = call
    )
  }

  tables <- lapply(parts, function(part) {
    otel_conversation_tables(
      part$spans,
      conversation_id = part$id,
      trajectory_id = trajectory_id,
      source_uri = source_uri,
      metadata = metadata,
      deputy = part
    )
  })
  otel_combine_tables(otel_deputy_resolve_parents(tables))
}

otel_is_span_list <- function(x) {
  length(x) > 0L &&
    is.list(x[[1L]]) &&
    all(c("trace_id", "span_id", "attributes") %in% names(x[[1L]]))
}

otel_empty_table <- function(table) {
  tibble::as_tibble(trajectory_table_schemas()[[table]])
}

# A subagent's trajectory links to the trajectory holding its parent run when
# both are in the read; the unresolved-parent loss then no longer applies.
otel_deputy_resolve_parents <- function(tables) {
  owners <- character()
  for (table in tables) {
    owners[table$run_ids %||% character()] <- table$info$trajectory_id
  }
  lapply(tables, function(table) {
    parent <- table$info$metadata[[1L]]$parent_run_id
    if (
      is.character(parent) && length(parent) == 1L && parent %in% names(owners)
    ) {
      table$info$parent_trajectory_id <- owners[[parent]]
      if (!is.null(table$losses)) {
        table$losses <- table$losses[
          table$losses$field != "parent_run_id",
          ,
          drop = FALSE
        ]
      }
    }
    table
  })
}

# Combine conversation tables before validating the bundle to avoid
# validating every conversation twice.
otel_combine_tables <- function(tables) {
  TrajectoryBundle(
    trajectory_bind_rows(lapply(tables, `[[`, "info")),
    trajectory_bind_rows(lapply(tables, `[[`, "turns")),
    trajectory_bind_rows(lapply(tables, `[[`, "events")),
    losses = trajectory_bind_rows(lapply(tables, `[[`, "losses"))
  )
}

# ---- conversation -> bundle -------------------------------------------------

otel_conversation_tables <- function(
  spans,
  conversation_id,
  trajectory_id,
  source_uri,
  metadata,
  deputy = list()
) {
  context <- attr(spans, "otel_context", exact = TRUE)
  spans <- otel_unique_spans(spans)
  attr(spans, "otel_context") <- context
  if (is.null(trajectory_id)) {
    trajectory_id <- paste0(
      "otel/",
      utils::URLencode(as.character(conversation_id), reserved = TRUE)
    )
    if (!is.null(deputy$run_id)) {
      trajectory_id <- paste0(
        trajectory_id,
        "/",
        utils::URLencode(deputy$run_id, reserved = TRUE)
      )
    }
  }
  # Companion spans contribute provenance and context only: counts, timing,
  # and status describe the model and tool activity.
  companion <- vapply(spans, otel_is_companion_span, logical(1))
  activity <- spans[!companion]
  attr(activity, "otel_context") <- context %||% otel_context(spans)
  chat_spans <- Filter(otel_is_chat_span, activity)
  latest <- otel_latest_span(chat_spans)
  tool_spans <- Filter(otel_is_tool_span, activity)

  transcript <- otel_transcript(latest, chat_spans, trajectory_id)
  tables <- otel_messages_tables(
    transcript$messages,
    trajectory_id = trajectory_id,
    turn_spans = transcript$turn_spans,
    tool_spans = tool_spans
  )
  tables$losses <- trajectory_bind_rows(list(
    trajectory_loss_table(transcript$losses),
    tables$losses
  ))
  commons <- otel_commons_tables(
    spans,
    chat_spans,
    transcript$messages,
    trajectory_id,
    nrow(tables$events),
    ancestry = context$ancestry
  )
  if (!is.null(commons)) {
    tables$turns$round_index <- commons$rounds
    tables$turns$metadata[commons$internal] <- list(list(
      commons_internal = TRUE
    ))
    tables$events <- trajectory_bind_rows(list(tables$events, commons$events))
    tables$losses <- trajectory_bind_rows(list(
      tables$losses,
      trajectory_loss_table(commons$losses)
    ))
  }

  runs <- otel_deputy_tables(
    deputy$runs %||% list(),
    trajectory_id,
    nrow(tables$events),
    deputy$unassigned %||% 0L
  )
  if (!is.null(runs)) {
    tables$events <- trajectory_bind_rows(list(tables$events, runs$events))
    tables$losses <- trajectory_bind_rows(list(
      tables$losses,
      trajectory_loss_table(runs$losses)
    ))
    metadata <- c(
      metadata[setdiff(names(metadata), names(runs$metadata))],
      runs$metadata
    )
  }

  times <- otel_span_times(activity)
  # The conversation's status is that of its latest model call: an early
  # rate-limited call that was retried successfully is a failed event, not a
  # failed conversation. Earlier failures stay countable in the metadata.
  failed_spans <- Filter(otel_span_failed, activity)
  failed <- otel_span_failed(latest)
  error <- if (failed) otel_span_error(latest) else NA_character_
  metadata$otel_failed_spans <- length(failed_spans)
  ids <- trajectory_ids(trajectory_id)
  safe_source_uri <- trajectory_sanitize_uri(source_uri, "source_uri", ids)
  safe_metadata <- trajectory_sanitize_metadata(
    otel_info_metadata(activity, chat_spans, metadata),
    "metadata",
    ids
  )
  info <- tibble::tibble(
    trajectory_id = trajectory_id,
    run_id = runs$run_id %||% as.character(conversation_id),
    parent_trajectory_id = NA_character_,
    source_type = "otel",
    source_id = as.character(conversation_id),
    source_uri = safe_source_uri$value,
    task_id = NA_character_,
    sample_id = NA_character_,
    epoch = NA_integer_,
    agent = otel_attribute(latest, "gen_ai.provider.name"),
    model = otel_attribute(latest, "gen_ai.response.model") %|na|%
      otel_attribute(latest, "gen_ai.request.model"),
    started_at = times$started_at,
    completed_at = times$completed_at,
    status = runs$status %||% if (failed) "failed" else "completed",
    error = error,
    metadata = list(safe_metadata$value)
  )

  list(
    info = info,
    run_ids = runs$run_ids,
    turns = tables$turns,
    events = tables$events,
    losses = trajectory_bind_rows(list(
      tables$losses,
      trajectory_loss_table(c(
        safe_source_uri$losses,
        safe_metadata$losses
      ))
    ))
  )
}

# Sum usage across distinct captured model calls, including calls whose
# turns are absent from the reconstructed history. Conversation-titling calls
# are work the application did for this conversation, so they stay in the
# totals; `title_calls` reports their share so it is neither hidden nor
# mistaken for transcript activity.
otel_info_metadata <- function(spans, chat_spans, metadata) {
  context <- attr(spans, "otel_context", exact = TRUE) %||% list()
  usage <- list(
    input_tokens = otel_sum_attribute(chat_spans, "gen_ai.usage.input_tokens"),
    output_tokens = otel_sum_attribute(
      chat_spans,
      "gen_ai.usage.output_tokens"
    ),
    model_calls = length(chat_spans),
    spans = length(spans)
  )
  title_spans <- Filter(otel_is_title_span, chat_spans)
  if (length(title_spans) > 0L) {
    usage$title_calls <- list(
      model_calls = length(title_spans),
      input_tokens = otel_sum_attribute(
        title_spans,
        "gen_ai.usage.input_tokens"
      ),
      output_tokens = otel_sum_attribute(
        title_spans,
        "gen_ai.usage.output_tokens"
      )
    )
  }
  usage$user <- context$user %||% otel_user_id(spans)
  usage$attributes <- context$attributes %||% otel_extra_attributes(spans)
  usage$resource <- context$resource %||% otel_resource_attributes(spans)
  usage$failed_spans <- metadata$otel_failed_spans
  usage$measures <- otel_resource_measures(spans, chat_spans)
  metadata$otel_failed_spans <- NULL
  metadata$otel <- usage[!vapply(usage, is.null, logical(1))]
  metadata
}

# Retain bounded source attributes for user, session, and deployment context.
otel_extra_attributes <- function(spans, max_chars = 200L) {
  values <- list()
  for (span in spans) {
    attributes <- span$attributes
    keys <- names(attributes)
    keep <- !startsWith(keys, "gen_ai.") &
      !keys %in% c("error.type", otel_commons_provenance_keys) &
      !otel_transport_attribute(keys)
    for (key in keys[keep]) {
      value <- otel_string(attributes[[key]])
      if (is.na(value)) {
        next
      }
      value <- otel_scrub_attribute_value(value)
      values[[key]] <- unique(c(values[[key]], otel_clip(value, max_chars)))
    }
  }
  if (length(values) == 0L) NULL else values[order(names(values))]
}

# HTTP client spans under a model call describe the provider request, not the
# conversation, and their URLs can carry credentials in the query string.
otel_transport_attribute <- function(keys) {
  prefixes <- c(
    "http.",
    "url.",
    "user_agent.",
    "server.",
    "client.",
    "network.",
    "net.",
    "peer.",
    "rpc."
  )
  Reduce(
    `|`,
    lapply(prefixes, function(prefix) startsWith(keys, prefix)),
    rep(FALSE, length(keys))
  )
}

# A URL-shaped value keeps only its scheme, host, and path.
otel_scrub_attribute_value <- function(value) {
  if (!grepl("^[A-Za-z][A-Za-z0-9+.-]*://", value)) {
    return(value)
  }
  value <- sub(
    "^([A-Za-z][A-Za-z0-9+.-]*://)[^/@[:space:]]+@",
    "\\1",
    value,
    perl = TRUE
  )
  sub("[?#].*$", "", value)
}

otel_resource_attributes <- function(spans, max_chars = 200L) {
  values <- list()
  for (span in spans) {
    resource <- span$resource
    if (is.list(resource) && length(resource) > 0L) {
      for (key in names(resource)) {
        value <- otel_string(resource[[key]])
        if (!is.na(value)) {
          values[[key]] <- unique(c(values[[key]], otel_clip(value, max_chars)))
        }
      }
    }
  }
  if (length(values) == 0L) NULL else values[order(names(values))]
}

otel_clip <- function(value, max_chars) {
  if (nchar(value) <= max_chars) {
    return(value)
  }
  paste0(substr(value, 1L, max_chars - 1L), "\u2026")
}

# The user behind a conversation, when any span names one. The semantic
# conventions say `enduser.id`; older and framework-specific keys are tried
# after it. commons records the Connect session user on its turn span.
otel_user_keys <- c(
  "enduser.id",
  "commons.connect.user",
  "user.id",
  "user.name",
  "user.email",
  "user.full_name",
  "session.user",
  "shiny.user",
  "connect.user",
  "rsconnect.user"
)

otel_user_id <- function(spans) {
  for (key in otel_user_keys) {
    for (span in spans) {
      for (attributes in list(span$attributes, span$resource)) {
        value <- otel_string(attributes[[key]])
        if (!is.na(value)) {
          return(value)
        }
      }
    }
  }
  NULL
}

otel_sum_attribute <- function(spans, key) {
  values <- vapply(
    spans,
    function(span) suppressWarnings(as.numeric(otel_attribute(span, key))),
    numeric(1)
  )
  if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
}

# Leave completion unavailable when recorded bounds are reversed.
otel_span_times <- function(spans) {
  starts <- vapply(spans, function(s) otel_nanos(s$start_time), numeric(1))
  ends <- vapply(spans, function(s) otel_nanos(s$end_time), numeric(1))
  started <- suppressWarnings(min(starts, na.rm = TRUE))
  completed <- suppressWarnings(max(ends, na.rm = TRUE))
  if (is.finite(started) && is.finite(completed) && completed < started) {
    completed <- NA_real_
  }
  list(
    started_at = otel_posixct(started),
    completed_at = otel_posixct(completed)
  )
}

# ---- messages -> turns and events -------------------------------------------

# The system prompt travels in its own attribute rather than the message
# list, so it is prepended as a system turn to match every other adapter.
otel_system_messages <- function(span) {
  parts <- otel_parse_json(otel_attribute(span, "gen_ai.system_instructions"))
  if (length(parts) == 0L) {
    return(list())
  }
  list(list(role = "system", parts = parts))
}

otel_parse_messages <- function(span, key) {
  messages <- otel_parse_json(otel_attribute(span, key))
  Filter(function(message) is.list(message) && !is.null(message$role), messages)
}

otel_parse_json <- function(text) {
  if (is.na(text) || !nzchar(text)) {
    return(list())
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || !is.list(parsed)) {
    return(list())
  }
  parsed
}

# `gen_ai.input.messages` is present and parses, even to an empty list.
otel_has_input_messages <- function(span) {
  text <- otel_attribute(span, "gen_ai.input.messages")
  if (is.na(text) || !nzchar(text)) {
    return(FALSE)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  is.list(parsed)
}

# The messages to reconstruct, the span whose usage each turn carries, and
# losses for history the spans did not record. Normally this is the selected
# span's own history and output. ellmer omits `gen_ai.input.messages` when a
# turn holds content it cannot serialize; the history is then recovered from
# the latest earlier call that recorded input, followed by the output of each
# call from that one to the selected one. Messages sent between those outputs
# are lost and reported, not replaced with placeholders. After commons'
# `recover_missing_input()`. A span with neither attribute had message
# capture turned off; it is contentless rather than partially captured.
otel_transcript <- function(latest, chat_spans, trajectory_id) {
  system <- otel_system_messages(latest)
  if (
    is.null(latest) ||
      otel_has_input_messages(latest) ||
      is.na(otel_attribute(latest, "gen_ai.output.messages"))
  ) {
    input <- otel_parse_messages(latest, "gen_ai.input.messages")
    return(otel_transcript_segments(system, input, list(latest)))
  }

  ids <- trajectory_ids(trajectory_id)
  candidates <- Filter(
    function(span) !otel_is_title_span(span) || identical(span, latest),
    chat_spans
  )
  starts <- vapply(candidates, function(s) otel_nanos(s$start_time), numeric(1))
  starts[!is.finite(starts)] <- -Inf
  candidates <- candidates[order(starts)]
  target <- Position(function(span) identical(span, latest), candidates)
  prior <- candidates[seq_len(target - 1L)]
  valid <- which(vapply(prior, otel_has_input_messages, logical(1)))

  if (length(valid) == 0L) {
    transcript <- otel_transcript_segments(system, list(), list(latest))
    transcript$losses <- list(trajectory_new_loss(
      ids,
      "capture$input",
      "unsupported",
      paste(
        "The latest model call omitted gen_ai.input.messages and no earlier",
        "call in the conversation recorded its input; earlier turns are missing"
      ),
      metadata = list(span_id = latest$span_id)
    ))
    return(transcript)
  }

  history <- prior[[utils::tail(valid, 1L)]]
  if (length(system) == 0L) {
    system <- otel_system_messages(history)
  }
  input <- otel_parse_messages(history, "gen_ai.input.messages")
  segments <- candidates[seq.int(utils::tail(valid, 1L), target)]
  transcript <- otel_transcript_segments(system, input, segments)
  transcript$losses <- lapply(seq_along(segments)[-1L], function(index) {
    previous <- otel_parse_messages(
      segments[[index - 1L]],
      "gen_ai.output.messages"
    )
    span_id <- segments[[index]]$span_id
    if (otel_messages_request_tools(previous)) {
      trajectory_new_loss(
        ids,
        "capture$tool_result",
        "unsupported",
        paste(
          "A model call omitted gen_ai.input.messages; the results of the",
          "tool calls requested before it are missing"
        ),
        metadata = list(span_id = span_id)
      )
    } else {
      trajectory_new_loss(
        ids,
        "capture$input",
        "unsupported",
        paste(
          "A model call omitted gen_ai.input.messages; the messages sent to",
          "it after the previous recorded output are missing"
        ),
        metadata = list(span_id = span_id)
      )
    }
  })
  transcript
}

# Each segment's output follows the history in order, and its last assistant
# message is the turn that carries that call's usage and timing.
otel_transcript_segments <- function(system, input, segments) {
  messages <- c(system, input)
  turn_spans <- vector("list", length(messages))
  for (span in segments) {
    output <- otel_parse_messages(span, "gen_ai.output.messages")
    assistant <- which(vapply(
      output,
      function(message) identical(otel_message_role(message), "assistant"),
      logical(1)
    ))
    spans <- vector("list", length(output))
    if (length(assistant) > 0L) {
      spans[utils::tail(assistant, 1L)] <- list(span)
    }
    messages <- c(messages, output)
    turn_spans <- c(turn_spans, spans)
  }
  list(messages = messages, turn_spans = turn_spans, losses = list())
}

otel_messages_request_tools <- function(messages) {
  any(vapply(
    messages,
    function(message) {
      any(vapply(
        message$parts %||% list(),
        function(part) identical(otel_string(part$type), "tool_call"),
        logical(1)
      ))
    },
    logical(1)
  ))
}

otel_messages_tables <- function(
  messages,
  trajectory_id,
  turn_spans,
  tool_spans
) {
  turns <- list()
  events <- list()
  losses <- list()
  event_index <- 0L
  tool_index <- otel_tool_index(tool_spans)

  for (index in seq_along(messages)) {
    message <- messages[[index]]
    role <- otel_message_role(message)
    turn_id <- sprintf("%s/turn-%06d", trajectory_id, index)
    parts <- message$parts %||% list()
    # A role-only message with no parts still marks a turn taking place.
    for (position in seq_along(parts)) {
      event_index <- event_index + 1L
      mapped <- otel_part_event(
        parts[[position]],
        trajectory_id = trajectory_id,
        turn_id = turn_id,
        event_index = event_index,
        content_index = position,
        tool_index = tool_index
      )
      events[[length(events) + 1L]] <- mapped$event
      if (!is.null(mapped$loss)) {
        losses[[length(losses) + 1L]] <- mapped$loss
      }
    }
    turns[[length(turns) + 1L]] <- otel_turn_row(
      trajectory_id = trajectory_id,
      turn_id = turn_id,
      index = index,
      role = role,
      span = turn_spans[[index]]
    )
  }

  list(
    turns = otel_rows_table(turns, "turns"),
    events = ellmer_link_tool_results(otel_rows_table(events, "events")),
    losses = trajectory_bind_rows(losses)
  )
}

# Roles outside the canonical vocabulary are namespaced rather than dropped,
# so an unfamiliar provider role still validates.
otel_message_role <- function(message) {
  role <- otel_string(message$role)
  if (is.na(role)) {
    return("assistant")
  }
  if (role %in% c("system", "user", "assistant", "tool")) {
    return(role)
  }
  paste0("otel:", gsub("[^a-z0-9._-]+", "-", tolower(role)))
}

otel_turn_row <- function(trajectory_id, turn_id, index, role, span) {
  failed <- !is.null(span) && otel_span_failed(span)
  list(
    trajectory_id = trajectory_id,
    turn_id = turn_id,
    turn_index = as.integer(index),
    round_index = NA_integer_,
    role = role,
    input_tokens = otel_numeric(span, "gen_ai.usage.input_tokens"),
    output_tokens = otel_numeric(span, "gen_ai.usage.output_tokens"),
    cached_input_tokens = NA_real_,
    cost = NA_real_,
    duration = if (is.null(span)) NA_real_ else otel_span_duration(span),
    finish_reason = NA_character_,
    status = if (failed) "failed" else "completed",
    error = if (failed) otel_span_error(span) else NA_character_,
    metadata = list()
  )
}

# Each part of a message becomes one event. Tool calls are enriched from the
# matching `execute_tool` span, which is the only place the call's duration
# and failure are recorded.
otel_part_event <- function(
  part,
  trajectory_id,
  turn_id,
  event_index,
  content_index,
  tool_index
) {
  type <- otel_string(part$type)
  event_id <- trajectory_event_id(trajectory_id, event_index)
  row <- otel_event_row(
    trajectory_id,
    turn_id,
    event_id,
    event_index,
    content_index
  )
  loss <- NULL

  if (identical(type, "text")) {
    row$event_type <- "content"
    row$content_type <- "text"
    trimmed <- otel_truncate_text(otel_string(part$content))
    row$text <- trimmed$text
    loss <- otel_truncation_loss(
      trimmed,
      trajectory_id,
      turn_id,
      event_id,
      "content"
    )
  } else if (identical(type, "tool_call")) {
    row$event_type <- "tool_call"
    row$name <- otel_string(part$name)
    row$call_id <- otel_string(part$id)
    safe <- otel_safe_value(
      part$arguments,
      trajectory_ids(trajectory_id, turn_id, event_id),
      "arguments"
    )
    row$value <- safe$value
    loss <- safe$loss
    row <- otel_apply_tool_span(row, tool_index)
  } else if (identical(type, "tool_call_response")) {
    row$event_type <- "tool_result"
    row$call_id <- otel_string(part$id)
    response <- part$response
    if (is.character(response) && length(response) == 1L) {
      trimmed <- otel_truncate_text(response)
      row$text <- trimmed$text
      loss <- otel_truncation_loss(
        trimmed,
        trajectory_id,
        turn_id,
        event_id,
        "response"
      )
    } else {
      safe <- otel_safe_value(
        response,
        trajectory_ids(trajectory_id, turn_id, event_id),
        "response"
      )
      row$value <- safe$value
      loss <- safe$loss
    }
  } else {
    # `generic` parts name a provider class the semantic conventions do not
    # describe. Recording the class keeps the turn's shape honest, and the
    # loss says plainly that its payload never reached the span.
    row$event_type <- "custom"
    row$name <- otel_string(part$class) %|na|%
      otel_string(type) %|na|%
      "unknown"
    loss <- tibble::tibble(
      trajectory_id = trajectory_id,
      turn_id = turn_id,
      event_id = event_id,
      field = "parts",
      reason = "unsupported",
      detail = paste0(
        "OpenTelemetry part type ",
        otel_string(type) %|na|% "<missing>",
        " carries no readable content"
      ),
      metadata = list(list())
    )
  }
  list(event = row, loss = loss)
}

# Return truncated text and its original byte count for loss reporting.
otel_truncate_text <- function(text, max_bytes = trajectory_payload_max_bytes) {
  if (is.na(text) || nchar(text, type = "bytes") <= max_bytes) {
    return(list(text = text, truncated = FALSE, bytes = NA_integer_))
  }
  bytes <- nchar(text, type = "bytes")
  # Cut by characters until the byte budget fits: a multibyte character must
  # not be split, and the ellipsis costs bytes of its own.
  keep <- substr(text, 1L, max_bytes - 16L)
  while (nchar(keep, type = "bytes") > max_bytes - 16L && nchar(keep) > 0L) {
    keep <- substr(keep, 1L, nchar(keep) - 8L)
  }
  list(
    text = paste0(keep, "\u2026"),
    truncated = TRUE,
    bytes = bytes
  )
}

otel_truncation_loss <- function(
  trimmed,
  trajectory_id,
  turn_id,
  event_id,
  field
) {
  if (!isTRUE(trimmed$truncated)) {
    return(NULL)
  }
  tibble::tibble(
    trajectory_id = trajectory_id,
    turn_id = turn_id,
    event_id = event_id,
    field = field,
    reason = "truncated",
    detail = sprintf(
      "Text of %s bytes was truncated to fit the %s byte field limit",
      format(trimmed$bytes, big.mark = ","),
      format(trajectory_payload_max_bytes, big.mark = ",")
    ),
    metadata = list(list())
  )
}

# Construct one tibble per table after assembling event rows.
otel_event_row <- function(
  trajectory_id,
  turn_id,
  event_id,
  event_index,
  content_index
) {
  list(
    trajectory_id = trajectory_id,
    event_id = event_id,
    event_index = as.integer(event_index),
    turn_id = turn_id,
    content_index = as.integer(content_index),
    parent_event_id = NA_character_,
    event_type = NA_character_,
    content_type = NA_character_,
    name = NA_character_,
    call_id = NA_character_,
    text = NA_character_,
    value = NULL,
    timestamp = NA_real_,
    duration = NA_real_,
    status = "completed",
    error = NA_character_,
    metadata = list()
  )
}

otel_rows_table <- function(rows, table) {
  columns <- trajectory_table_schemas()[[table]]
  if (length(rows) == 0L) {
    return(otel_empty_table(table))
  }
  values <- Map(
    function(column, prototype) {
      cells <- lapply(rows, function(row) row[[column]])
      type <- trajectory_prototype_type(prototype)
      switch(
        type,
        list = lapply(cells, function(cell) cell),
        POSIXct = as.POSIXct(
          vapply(cells, function(cell) cell %||% NA_real_, numeric(1)),
          origin = "1970-01-01",
          tz = "UTC"
        ),
        integer = vapply(
          cells,
          function(cell) cell %||% NA_integer_,
          integer(1)
        ),
        double = vapply(cells, function(cell) cell %||% NA_real_, numeric(1)),
        vapply(cells, function(cell) cell %||% NA_character_, character(1))
      )
    },
    names(columns),
    columns
  )
  tibble::new_tibble(values, nrow = length(rows))
}

otel_apply_tool_span <- function(row, tool_index) {
  call_id <- row$call_id[[1L]]
  if (is.na(call_id) || is.null(tool_index[[call_id]])) {
    return(row)
  }
  span <- tool_index[[call_id]]
  row$timestamp <- otel_nanos(span$start_time) / 1e9
  row$duration <- otel_span_duration(span)
  if (is.na(row$name[[1L]])) {
    row$name <- otel_attribute(span, "gen_ai.tool.name")
  }
  if (otel_span_failed(span)) {
    row$status <- "failed"
    row$error <- otel_span_error(span)
  }
  row
}

otel_tool_index <- function(tool_spans) {
  index <- list()
  for (span in tool_spans) {
    call_id <- otel_attribute(span, "gen_ai.tool.call.id")
    if (!is.na(call_id)) {
      index[[call_id]] <- span
    }
  }
  index
}

# Tool arguments and responses are provider-shaped JSON. Apply the same
# bounded, serializable-value policy as every other adapter before placing a
# value in the bundle. In particular, a structurally safe but oversized list
# must become a recorded truncation instead of invalidating the conversation.
otel_safe_value <- function(value, ids, field) {
  sanitized <- trajectory_sanitize_value(value, field, ids)
  list(
    value = sanitized$value,
    loss = trajectory_loss_table(sanitized$losses)
  )
}

# ---- span helpers -----------------------------------------------------------

otel_is_chat_span <- function(span) {
  identical(otel_attribute(span, "gen_ai.operation.name"), "chat")
}

otel_title_prompt <- "You title chat conversations."

# After commons' `is_conversation_title_span()`: the system instructions'
# text begins with shinychat's titling prompt.
otel_is_title_span <- function(span) {
  parts <- otel_parse_json(otel_attribute(span, "gen_ai.system_instructions"))
  texts <- vapply(
    parts,
    function(part) {
      if (is.list(part) && identical(otel_string(part$type), "text")) {
        otel_string(part$content)
      } else {
        NA_character_
      }
    },
    character(1)
  )
  texts <- texts[!is.na(texts)]
  length(texts) > 0L &&
    startsWith(paste(texts, collapse = ""), otel_title_prompt)
}

otel_is_tool_span <- function(span) {
  identical(otel_attribute(span, "gen_ai.operation.name"), "execute_tool")
}

otel_span_failed <- function(span) {
  !is.na(otel_attribute(span, "error.type")) || otel_status_failed(span$status)
}

otel_status_failed <- function(status) {
  if (!is.list(status)) {
    return(FALSE)
  }
  code <- otel_string(status$code)
  !is.na(code) && toupper(code) %in% c("2", "ERROR", "STATUS_CODE_ERROR")
}

otel_span_error <- function(span) {
  error <- otel_attribute(span, "error.type")
  if (!is.na(error)) {
    return(error)
  }
  message <- otel_string(span$status$message)
  if (!is.na(message)) {
    return(message)
  }
  if (otel_status_failed(span$status)) "OTLP status: ERROR" else NA_character_
}

otel_attribute <- function(span, key) {
  if (is.null(span)) {
    return(NA_character_)
  }
  value <- span$attributes[[key]]
  if (is.null(value)) {
    return(NA_character_)
  }
  as.character(value)
}

otel_numeric <- function(span, key) {
  value <- otel_attribute(span, key)
  if (is.na(value)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(value))
}

otel_string <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    return(NA_character_)
  }
  as.character(x)
}

otel_nanos <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x))
}

otel_posixct <- function(nanos) {
  if (!is.finite(nanos)) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  as.POSIXct(nanos / 1e9, origin = "1970-01-01", tz = "UTC")
}

# Leave missing or reversed span bounds unavailable; durations must be
# nonnegative for bundle validation.
otel_span_duration <- function(span) {
  start <- otel_nanos(span$start_time)
  end <- otel_nanos(span$end_time)
  if (!is.finite(start) || !is.finite(end) || end < start) {
    return(NA_real_)
  }
  (end - start) / 1e9
}

# The latest chat span is the transcript source, except that a shinychat
# conversation-titling call never displaces a real one: it shares the
# conversation id and often starts last, but its history is the title prompt.
# After commons' `chat_span_is_preferred()`.
otel_latest_span <- function(spans) {
  if (length(spans) == 0L) {
    return(NULL)
  }
  transcript <- Filter(Negate(otel_is_title_span), spans)
  if (length(transcript) > 0L) {
    spans <- transcript
  }
  times <- vapply(spans, function(s) otel_nanos(s$start_time), numeric(1))
  times[!is.finite(times)] <- -Inf
  spans[[which.max(times)]]
}

# ---- grouping ---------------------------------------------------------------

#' @rdname as_trajectory_otel
#' @param spans A flat list of parsed spans.
#' @export
otel_group_conversations <- function(spans) {
  spans <- otel_unique_spans(spans)
  index <- otel_span_index(spans)
  chat_spans <- Filter(otel_is_selected_chat_span, spans)
  if (length(chat_spans) == 0L) {
    return(list())
  }
  ids <- vapply(
    spans,
    function(span) otel_conversation_id(span, index),
    character(1)
  )
  ids <- otel_infer_trace_conversations(spans, ids)
  keep <- !is.na(ids)
  groups <- split(spans[keep], ids[keep])
  # A group with no chat span holds only tool or framework activity and has
  # no conversation to show.
  groups <- Filter(
    function(group) {
      any(vapply(group, otel_is_selected_chat_span, logical(1)))
    },
    groups
  )
  groups <- lapply(groups, function(group) {
    context_spans <- otel_group_context_spans(group, index)
    members <- seq_along(context_spans) <= length(group)
    # A companion found only as an ancestor belongs to another conversation:
    # Deputy files a compaction summary under its session, not the run's.
    keep <- vapply(
      seq_along(context_spans),
      function(i) {
        otel_is_conversation_span(context_spans[[i]]) &&
          (members[[i]] || !otel_is_companion_span(context_spans[[i]]))
      },
      logical(1)
    )
    conversation <- otel_deputy_assign_runs(context_spans[keep], index)
    context <- otel_context(context_spans)
    context$ancestry <- otel_unexposed_ancestry(context_spans)
    attr(conversation, "otel_context") <- context
    conversation
  })
  otel_order_conversations(groups)
}

# Include ancestors while extracting conversation context, even when a wrapper
# has no GenAI attributes of its own. The returned conversation later exposes
# only GenAI spans, while its bounded context summary retains wrapper metadata.
otel_context <- function(spans) {
  list(
    user = otel_user_id(spans),
    attributes = otel_extra_attributes(spans),
    resource = otel_resource_attributes(spans)
  )
}

# Parent links of the context spans a conversation does not expose: framework
# wrappers, and ancestors the span budget kept as context only. They let a
# retained span reach its commons turn without counting as activity.
otel_unexposed_ancestry <- function(context_spans) {
  lapply(
    Filter(Negate(otel_is_conversation_span), context_spans),
    function(span) span[c("trace_id", "span_id", "parent_span_id", "name")]
  )
}

otel_group_context_spans <- function(group, index) {
  context_spans <- group
  included <- new.env(parent = emptyenv())
  for (span in group) {
    assign(paste(span$trace_id, span$span_id), TRUE, envir = included)
  }
  for (span in group) {
    current <- span
    for (step in seq_len(64L)) {
      parent <- current$parent_span_id
      if (is.null(parent) || is.na(parent) || !nzchar(parent)) {
        break
      }
      key <- paste(current$trace_id, parent)
      if (!exists(key, envir = index, inherits = FALSE)) {
        break
      }
      current <- get(key, envir = index, inherits = FALSE)
      if (!exists(key, envir = included, inherits = FALSE)) {
        context_spans[[length(context_spans) + 1L]] <- current
        assign(key, TRUE, envir = included)
      }
    }
  }
  context_spans
}

otel_order_conversations <- function(groups) {
  if (length(groups) == 0L) {
    return(groups)
  }
  latest <- vapply(
    groups,
    function(group) {
      starts <- vapply(group, function(s) otel_nanos(s$start_time), numeric(1))
      if (any(is.finite(starts))) max(starts[is.finite(starts)]) else -Inf
    },
    numeric(1)
  )
  groups[order(latest)]
}

otel_span_index <- function(spans) {
  index <- new.env(parent = emptyenv())
  for (span in spans) {
    assign(paste(span$trace_id, span$span_id), span, envir = index)
  }
  index
}

# The same span can arrive twice: in the content-wide store and a retained
# per-job store, or on two offset pages after new spans shifted the paging.
# Counting it twice doubles token totals, so identity is trace id + span id.
otel_unique_spans <- function(spans) {
  if (length(spans) == 0L) {
    return(spans)
  }
  keys <- vapply(
    spans,
    function(span) paste(span$trace_id, span$span_id),
    character(1)
  )
  spans[!duplicated(keys)]
}

# Walk up the ancestry for a conversation id, as commons does: ellmer records
# it on the spans it owns, and a framework wrapper may set it on a parent.
# `NA` says no ancestor carries one.
otel_conversation_id <- function(span, index) {
  current <- span
  for (step in seq_len(64L)) {
    id <- current$attributes[["gen_ai.conversation.id"]]
    if (!is.null(id) && nzchar(as.character(id))) {
      return(as.character(id))
    }
    parent <- current$parent_span_id
    if (is.null(parent) || is.na(parent) || !nzchar(parent)) {
      break
    }
    key <- paste(span$trace_id, parent)
    if (!exists(key, envir = index, inherits = FALSE)) {
      break
    }
    current <- get(key, envir = index, inherits = FALSE)
  }
  NA_character_
}

# ellmer stamps the conversation id on its chat spans only; a tool span is a
# sibling, not a child, so its own ancestry has none. A span without an id
# joins the one conversation its trace belongs to, when the trace has exactly
# one. Without any, the trace id groups a single model call.
otel_infer_trace_conversations <- function(spans, ids) {
  traces <- vapply(spans, function(span) span$trace_id, character(1))
  missing <- is.na(ids)
  if (any(missing) && !all(missing)) {
    known <- split(ids[!missing], traces[!missing])
    unique_ids <- vapply(
      known,
      function(x) {
        x <- unique(x)
        if (length(x) == 1L) x else NA_character_
      },
      character(1)
    )
    inferred <- unname(unique_ids[traces[missing]])
    ids[missing] <- inferred
    missing <- is.na(ids)
  }
  ids[missing] <- traces[missing]
  ids
}

`%|na|%` <- function(x, y) {
  if (length(x) == 1L && !is.na(x)) x else y
}
