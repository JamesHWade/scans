# commons records each answer's provenance on OpenTelemetry spans
# (pkg-r/R/tracing.R): a `commons_conversation_turn` span wraps every turn,
# and a `commons_provenance` span under it carries `commons.provenance.tag`
# and the `commons.citation.candidates` JSON. Older traces put both
# attributes on the turn span itself. These attributes become
# `commons:provenance` events, so they are left out of generic metadata.
otel_commons_provenance_keys <- c(
  "commons.provenance.tag",
  "commons.citation.candidates"
)

# Provenance events, round indices, and internal-turn flags for a commons
# conversation, or NULL when no commons turn or provenance span was captured. Each exchange of
# the reconstructed transcript gets one event, in the shape
# `as_trajectory_commons()` gives the same record. A record attaches to an
# exchange as in commons' `build_trajectories()`: the latest model call of
# each turn proposes the latest provenance recorded for that turn, and the
# proposal holds when that call's history, split into exchanges, is a prefix
# of the transcript's and ends in a complete exchange. Matching content
# rather than position keeps an edited or retried history from borrowing a
# record. Different records for one exchange leave it unclassified, with a
# loss, rather than choosing one. `ancestry` holds the parent links of spans
# the conversation does not expose, so a call still reaches its turn through
# them.
otel_commons_tables <- function(
  spans,
  chat_spans,
  messages,
  trajectory_id,
  offset,
  ancestry = NULL
) {
  commons <- c("commons_conversation_turn", "commons_provenance")
  if (
    !any(vapply(
      spans,
      function(span) isTRUE(span$name %in% commons),
      logical(1)
    ))
  ) {
    return(NULL)
  }
  index <- otel_span_index(c(ancestry, spans))
  turn_key <- function(span) {
    turn <- otel_commons_turn_ancestor(span, index)
    if (is.null(turn)) NA_character_ else paste(turn$trace_id, turn$span_id)
  }

  provenance <- list()
  for (span in spans) {
    if (!identical(span$name, "commons_provenance")) {
      next
    }
    key <- turn_key(span)
    if (is.na(key)) {
      next
    }
    previous <- provenance[[key]]
    if (is.null(previous) || otel_span_is_later(span, previous)) {
      provenance[[key]] <- span
    }
  }
  calls <- list()
  for (span in Filter(Negate(otel_is_title_span), chat_spans)) {
    key <- turn_key(span)
    if (is.na(key)) {
      next
    }
    previous <- calls[[key]]$chat
    if (is.null(previous) || otel_span_is_later(span, previous)) {
      calls[[key]] <- list(
        chat = span,
        record = provenance[[key]] %||% get(key, envir = index)
      )
    }
  }

  rounds <- otel_exchange_rounds(messages)
  canonical <- otel_exchange_signatures(messages, rounds)
  claims <- vector("list", length(canonical))
  for (call in calls) {
    history <- otel_transcript(call$chat, chat_spans, trajectory_id)$messages
    history_rounds <- otel_exchange_rounds(history)
    signature <- otel_exchange_signatures(history, history_rounds)
    last <- which(history_rounds == length(signature))
    complete <- length(signature) > 0L &&
      otel_exchange_complete(history[last])
    matches <- complete &&
      length(signature) <= length(canonical) &&
      identical(signature, canonical[seq_along(signature)])
    if (matches) {
      claims[[length(signature)]] <- c(
        claims[[length(signature)]],
        list(call$record)
      )
    }
  }

  ids <- trajectory_ids(trajectory_id)
  records <- vector("list", length(canonical))
  times <- rep(NA_real_, length(canonical))
  losses <- list()
  for (exchange in seq_along(canonical)) {
    claimed <- lapply(claims[[exchange]], otel_commons_record)
    distinct <- unique(claimed)
    records[[exchange]] <- list(
      provenance_tag = NA_character_,
      citation_decisions = list()
    )
    if (length(distinct) == 1L) {
      records[[exchange]] <- distinct[[1L]]
      times[[exchange]] <- max(vapply(
        claims[[exchange]],
        otel_span_time,
        numeric(1)
      ))
    } else if (length(distinct) > 1L) {
      losses[[length(losses) + 1L]] <- trajectory_new_loss(
        ids,
        paste0("provenance[[", exchange, "]]"),
        "unsupported",
        paste(
          "Conflicting commons provenance records were captured for this",
          "exchange; it is left unclassified"
        ),
        metadata = list(records = length(distinct))
      )
    }
  }

  tables <- commons_provenance_events(records, trajectory_id, offset)
  events <- tables$events
  if (!is.null(events) && nrow(events) > 0L) {
    events$timestamp <- as.POSIXct(
      times / 1e9,
      origin = "1970-01-01",
      tz = "UTC"
    )
  }
  list(
    events = events,
    losses = c(tables$losses, losses),
    rounds = rounds,
    internal = vapply(messages, otel_is_internal_message, logical(1))
  )
}

# After commons' `turn_span_provenance()`. Candidates that are not valid JSON
# leave `citation_decisions` NULL, which the commons adapter's record checks
# report as malformed.
otel_commons_record <- function(span) {
  tag <- otel_attribute(span, "commons.provenance.tag")
  candidates <- otel_attribute(span, "commons.citation.candidates")
  decisions <- if (is.na(candidates)) {
    list()
  } else {
    tryCatch(
      jsonlite::fromJSON(candidates, simplifyVector = FALSE) %||% list(),
      error = function(e) NULL
    )
  }
  list(provenance_tag = tag, citation_decisions = decisions)
}

otel_commons_turn_ancestor <- function(span, index) {
  current <- span
  for (step in seq_len(64L)) {
    if (identical(current$name, "commons_conversation_turn")) {
      return(current)
    }
    parent <- current$parent_span_id
    if (is.null(parent) || is.na(parent) || !nzchar(parent)) {
      return(NULL)
    }
    key <- paste(span$trace_id, parent)
    if (!exists(key, envir = index, inherits = FALSE)) {
      return(NULL)
    }
    current <- get(key, envir = index, inherits = FALSE)
  }
  NULL
}

# commons compares span times as end time, else start time.
otel_span_time <- function(span) {
  end <- otel_nanos(span$end_time)
  if (is.finite(end)) end else otel_nanos(span$start_time)
}

# A span without a usable time never displaces one already chosen, as in
# `otel_latest_span()`.
otel_span_is_later <- function(span, previous) {
  times <- c(otel_span_time(span), otel_span_time(previous))
  times[!is.finite(times)] <- -Inf
  times[[1L]] > times[[2L]]
}

# Exchange (round) of each message, after commons' `split_exchanges()`: a user
# message opens an exchange unless it returns tool results or is an internal
# reminder. Messages before the first exchange have none.
otel_exchange_rounds <- function(messages) {
  commons_exchange_rounds(vapply(
    messages,
    function(message) {
      identical(otel_message_role(message), "user") &&
        !any(vapply(
          message$parts %||% list(),
          function(part) {
            identical(otel_string(part$type), "tool_call_response")
          },
          logical(1)
        )) &&
        !otel_is_internal_message(message)
    },
    logical(1)
  ))
}

# commons' `is_internal_turn()`: a user turn whose every part is text wrapped
# in one <system-instruction> or legacy <reminder> element.
otel_is_internal_message <- function(message) {
  identical(otel_message_role(message), "user") &&
    all(vapply(
      message$parts %||% list(),
      function(part) {
        identical(otel_string(part$type), "text") &&
          commons_is_internal_text(otel_string(part$content) %|na|% "")
      },
      logical(1)
    ))
}

# An exchange is complete when it has a reply that requests no tools.
otel_exchange_complete <- function(messages) {
  length(messages) >= 2L &&
    identical(otel_message_role(messages[[length(messages)]]), "assistant") &&
    !otel_messages_request_tools(messages[length(messages)])
}

otel_exchange_signatures <- function(messages, rounds) {
  lapply(seq_len(max(c(0L, rounds), na.rm = TRUE)), function(round) {
    lapply(messages[which(rounds == round)], function(message) {
      list(
        role = otel_message_role(message),
        parts = lapply(message$parts %||% list(), otel_part_signature)
      )
    })
  })
}

otel_part_signature <- function(part) {
  type <- otel_string(part$type)
  switch(
    type %|na|% "",
    text = list(type = type, text = part$content),
    tool_call = list(
      type = type,
      id = part$id,
      name = part$name,
      arguments = otel_canonical_value(part$arguments)
    ),
    tool_call_response = list(
      type = type,
      id = part$id,
      response = otel_canonical_value(part$response)
    ),
    otel_canonical_value(part)
  )
}

otel_canonical_value <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value <- lapply(value, otel_canonical_value)
  if (!is.null(names(value))) {
    value <- value[order(names(value))]
  }
  value
}
