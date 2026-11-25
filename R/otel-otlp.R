# Each line is one OTLP envelope: {"resourceSpans":[{"scopeSpans":[{"spans":
# [...]}]}]}. A malformed line is skipped rather than failing the read; one
# bad record should not cost the whole history.
otel_parse_otlp_lines <- function(lines, max_spans = Inf, to = NULL) {
  spans <- list()
  genai <- 0L
  for (envelope in otel_parse_envelopes(lines)) {
    for (resource in envelope$resourceSpans %||% list()) {
      for (scope in resource$scopeSpans %||% list()) {
        for (span in scope$spans %||% list()) {
          parsed_span <- otel_span(span, scope, resource)
          if (!otel_span_in_window(parsed_span, from = NULL, to = to)) {
            next
          }
          if (otel_is_budget_span(parsed_span)) {
            if (genai >= max_spans) {
              return(spans)
            }
            genai <- genai + 1L
          }
          spans[[length(spans) + 1L]] <- parsed_span
        }
      }
    }
  }
  spans
}

# Spans from a set of lines, plus how many lines could not be parsed at all.
otel_parse_otlp_lines_counted <- function(lines, to = NULL) {
  envelopes <- otel_parse_envelopes(lines)
  spans <- list()
  for (envelope in envelopes) {
    for (resource in envelope$resourceSpans %||% list()) {
      for (scope in resource$scopeSpans %||% list()) {
        for (span in scope$spans %||% list()) {
          spans[[length(spans) + 1L]] <- otel_span(span, scope, resource)
        }
      }
    }
  }
  list(spans = spans, dropped = attr(envelopes, "dropped") %||% 0L)
}

# Lines are parsed as one JSON array: a call per line spent most of a
# store's read time in jsonlite's setup. If any line is malformed the array
# fails to parse and the lines are parsed one by one, skipping the bad ones.
otel_parse_envelopes <- function(lines) {
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return(list())
  }
  batch <- tryCatch(
    jsonlite::fromJSON(
      paste0("[", paste(lines, collapse = ","), "]"),
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  if (is.list(batch) && length(batch) == length(lines)) {
    attr(batch, "dropped") <- 0L
    return(batch)
  }
  envelopes <- lapply(lines, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })
  kept <- envelopes[!vapply(envelopes, is.null, logical(1))]
  attr(kept, "dropped") <- length(lines) - length(kept)
  kept
}

# One envelope per line, NULL where a line is malformed (so positions line
# up with the input).
otel_parse_envelopes_each <- function(lines) {
  batch <- tryCatch(
    jsonlite::fromJSON(
      paste0("[", paste(lines, collapse = ","), "]"),
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  if (is.list(batch) && length(batch) == length(lines)) {
    return(batch)
  }
  lapply(lines, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })
}

otel_envelope_spans <- function(envelope) {
  spans <- list()
  for (resource in envelope$resourceSpans %||% list()) {
    for (scope in resource$scopeSpans %||% list()) {
      for (span in scope$spans %||% list()) {
        spans[[length(spans) + 1L]] <- otel_span(span, scope, resource)
      }
    }
  }
  spans
}

otel_span <- function(span, scope = NULL, resource = NULL) {
  parsed <- list(
    trace_id = span$traceId %||% "",
    span_id = span$spanId %||% "",
    parent_span_id = span$parentSpanId %||% "",
    name = span$name %||% "",
    scope = scope$scope$name %||% NA_character_,
    start_time = as.character(span$startTimeUnixNano %||% NA),
    end_time = as.character(span$endTimeUnixNano %||% NA),
    status = span$status %||% list(),
    attributes = otel_attributes(span$attributes),
    resource = otel_attributes(resource$resource$attributes)
  )
  # Span events are read only where they are projected, so a trace full of
  # exception or framework events does not grow every parsed span.
  if (otel_is_companion_span(parsed) && length(span$events) > 0L) {
    parsed$events <- otel_span_events(span$events)
  }
  parsed
}

# At most `max_events` events per span; the count left out is kept.
otel_span_events <- function(events, max_events = 1000L) {
  kept <- utils::head(events, max_events)
  out <- lapply(kept, function(event) {
    list(
      name = event$name %||% "",
      time = as.character(event$timeUnixNano %||% NA),
      attributes = otel_attributes(event$attributes)
    )
  })
  attr(out, "dropped") <- length(events) - length(kept)
  out
}

otel_attributes <- function(attributes) {
  values <- lapply(attributes %||% list(), function(attribute) {
    otel_attribute_value(attribute$value)
  })
  names(values) <- vapply(
    attributes %||% list(),
    function(attribute) attribute$key %||% "",
    character(1)
  )
  values[nzchar(names(values))]
}

otel_attribute_value <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }
  for (field in c("stringValue", "intValue", "doubleValue", "boolValue")) {
    if (!is.null(value[[field]])) {
      return(value[[field]])
    }
  }
  if (!is.null(value$arrayValue)) {
    return(paste(
      vapply(
        value$arrayValue$values %||% list(),
        function(x) as.character(otel_attribute_value(x)),
        character(1)
      ),
      collapse = ", "
    ))
  }
  NA_character_
}

# A span without a GenAI semantic-convention attribute is framework activity
# (a Shiny reactive, an HTTP handler) and does not consume the GenAI budget.
otel_is_genai_span <- function(span) {
  keys <- names(span$attributes)
  any(startsWith(keys, "gen_ai."))
}

# Producer spans that belong with a conversation but are not model or tool
# activity. They are kept with the conversation their ancestry or trace
# assigns them to, never count against the span budget, and never define a
# conversation on their own. commons wraps each turn in
# `commons_conversation_turn` and records each answer's provenance in
# `commons_provenance` (pkg-r/R/tracing.R). Deputy wraps each agent run in
# `deputy.run`, with its governance events on it (R/agent-tracing.R).
otel_companion_span_names <- c(
  "commons_conversation_turn",
  "commons_provenance",
  "deputy.run"
)

otel_is_companion_span <- function(span) {
  isTRUE(span$name %in% otel_companion_span_names)
}

# The spans the `max_spans` budget counts.
otel_is_budget_span <- function(span) {
  otel_is_genai_span(span) && !otel_is_companion_span(span)
}

otel_is_selected_budget_span <- function(span) {
  otel_is_budget_span(span) &&
    !isTRUE(attr(span, "otel_context_only", exact = TRUE))
}

# The spans a grouped conversation keeps.
otel_is_conversation_span <- function(span) {
  otel_is_selected_budget_span(span) || otel_is_companion_span(span)
}

otel_is_selected_chat_span <- function(span) {
  otel_is_chat_span(span) &&
    !isTRUE(attr(span, "otel_context_only", exact = TRUE))
}

otel_jsonlite_available <- function() {
  requireNamespace("jsonlite", quietly = TRUE)
}

otel_check_jsonlite <- function(call = rlang::caller_env()) {
  if (otel_jsonlite_available()) {
    return(invisible(TRUE))
  }
  scans_abort(
    c(
      "Converting OpenTelemetry spans requires {.pkg jsonlite}.",
      i = "Install {.pkg jsonlite} to enable it."
    ),
    class = "scans_error_otel_dependency",
    call = call
  )
}
