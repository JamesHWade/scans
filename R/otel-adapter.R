#' Convert OpenTelemetry GenAI spans
#'
#' @description
#' `as_trajectory_otel()` builds a [TrajectoryBundle] from OpenTelemetry spans
#' that follow the GenAI semantic conventions, as emitted by \pkg{ellmer} and
#' recorded by Posit Connect's content observability store. Read those spans
#' with [read_connect_traces()].
#'
#' Each conversation is reconstructed from its most recent `chat` span:
#' provider instrumentation records the whole message history on every model
#' call, so the last call carries the complete exchange. `execute_tool` spans
#' contribute the timing and failure of each tool call, which the message
#' history alone does not record.
#'
#' Spans are grouped into conversations by the `gen_ai.conversation.id`
#' attribute, walking up the span's ancestry to find it. A conversation
#' without that attribute anywhere in its ancestry falls back to its trace id,
#' which groups a single model call rather than a session.
#'
#' @param x A list of conversations, as returned by [read_connect_traces()],
#'   or a flat list of spans to be grouped.
#' @param trajectory_id Optional trajectory identifier. Must be `NULL` when
#'   `x` holds more than one conversation.
#' @param source_uri Optional URI recording where the spans were read from.
#' @param metadata Optional named list attached to every trajectory.
#'
#' @returns A [TrajectoryBundle].
#'
#' @seealso [read_connect_traces()] to fetch spans from Posit Connect.
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
  if (!is.null(trajectory_id) && length(conversations) > 1L) {
    scans_abort(
      "{.arg trajectory_id} must be {.code NULL} for multiple conversations.",
      class = "scans_error_otel_source",
      call = call
    )
  }

  tables <- Map(
    function(spans, id) {
      otel_conversation_tables(
        spans,
        conversation_id = id,
        trajectory_id = trajectory_id,
        source_uri = source_uri,
        metadata = metadata
      )
    },
    conversations,
    names(conversations) %||% seq_along(conversations)
  )
  otel_combine_tables(tables)
}

otel_is_span_list <- function(x) {
  length(x) > 0L &&
    is.list(x[[1L]]) &&
    all(c("trace_id", "span_id", "attributes") %in% names(x[[1L]]))
}

otel_empty_table <- function(table) {
  tibble::as_tibble(trajectory_table_schemas()[[table]])
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
  metadata
) {
  context <- attr(spans, "otel_context", exact = TRUE)
  spans <- otel_unique_spans(spans)
  attr(spans, "otel_context") <- context
  if (is.null(trajectory_id)) {
    trajectory_id <- paste0(
      "otel/",
      utils::URLencode(as.character(conversation_id), reserved = TRUE)
    )
  }
  chat_spans <- Filter(otel_is_chat_span, spans)
  latest <- otel_latest_span(chat_spans)
  tool_spans <- Filter(otel_is_tool_span, spans)

  system_messages <- otel_system_messages(latest)
  input_messages <- otel_parse_messages(latest, "gen_ai.input.messages")
  output_messages <- otel_parse_messages(latest, "gen_ai.output.messages")
  messages <- c(system_messages, input_messages, output_messages)
  model_output_index <- otel_model_output_index(
    output_messages,
    length(system_messages) + length(input_messages)
  )
  tables <- otel_messages_tables(
    messages,
    trajectory_id = trajectory_id,
    latest = latest,
    model_output_index = model_output_index,
    tool_spans = tool_spans
  )

  times <- otel_span_times(spans)
  # The conversation's status is that of its latest model call: an early
  # rate-limited call that was retried successfully is a failed event, not a
  # failed conversation. Earlier failures stay countable in the metadata.
  failed_spans <- Filter(otel_span_failed, spans)
  failed <- otel_span_failed(latest)
  error <- if (failed) otel_span_error(latest) else NA_character_
  metadata$otel_failed_spans <- length(failed_spans)
  ids <- trajectory_ids(trajectory_id)
  safe_source_uri <- trajectory_sanitize_uri(source_uri, "source_uri", ids)
  safe_metadata <- trajectory_sanitize_metadata(
    otel_info_metadata(spans, chat_spans, metadata),
    "metadata",
    ids
  )
  info <- tibble::tibble(
    trajectory_id = trajectory_id,
    run_id = as.character(conversation_id),
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
    status = if (failed) "failed" else "completed",
    error = error,
    metadata = list(safe_metadata$value)
  )

  list(
    info = info,
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
# turns are absent from the reconstructed history.
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
      !keys %in% c("error.type") &
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
# after it.
otel_user_keys <- c(
  "enduser.id",
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

otel_model_output_index <- function(messages, offset) {
  indices <- which(vapply(
    messages,
    function(message) identical(otel_message_role(message), "assistant"),
    logical(1)
  ))
  if (length(indices) == 0L) {
    return(NA_integer_)
  }
  as.integer(offset + utils::tail(indices, 1L))
}

otel_messages_tables <- function(
  messages,
  trajectory_id,
  latest,
  model_output_index,
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
      span = if (!is.na(model_output_index) && index == model_output_index) {
        latest
      } else {
        NULL
      }
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

otel_latest_span <- function(spans) {
  if (length(spans) == 0L) {
    return(NULL)
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
    group <- Filter(otel_is_selected_genai_span, context_spans)
    attr(group, "otel_context") <- list(
      user = otel_user_id(context_spans),
      attributes = otel_extra_attributes(context_spans),
      resource = otel_resource_attributes(context_spans)
    )
    group
  })
  otel_order_conversations(groups)
}

# Include ancestors while extracting conversation context, even when a wrapper
# has no GenAI attributes of its own. The returned conversation later exposes
# only GenAI spans, while its bounded context summary retains wrapper metadata.
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
