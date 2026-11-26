otel_test_span <- function(
  span_id,
  attributes,
  parent = "",
  trace = "trace-1",
  start = 1e9,
  end = 2e9,
  name = "span"
) {
  list(
    trace_id = trace,
    span_id = span_id,
    parent_span_id = parent,
    name = name,
    scope = "test",
    start_time = as.character(start),
    end_time = as.character(end),
    attributes = attributes
  )
}

otel_chat_span <- function(
  span_id = "chat-1",
  conversation = "conv-1",
  parent = "",
  input = NULL,
  output = NULL,
  system = NULL,
  extra = list()
) {
  attributes <- c(
    list(
      "gen_ai.operation.name" = "chat",
      "gen_ai.provider.name" = "openai",
      "gen_ai.request.model" = "gpt-test",
      "gen_ai.usage.input_tokens" = 100,
      "gen_ai.usage.output_tokens" = 20
    ),
    if (!is.null(conversation)) list("gen_ai.conversation.id" = conversation),
    if (!is.null(system)) {
      list(
        "gen_ai.system_instructions" = jsonlite::toJSON(
          system,
          auto_unbox = TRUE
        )
      )
    },
    if (!is.null(input)) {
      list("gen_ai.input.messages" = jsonlite::toJSON(input, auto_unbox = TRUE))
    },
    if (!is.null(output)) {
      list(
        "gen_ai.output.messages" = jsonlite::toJSON(
          output,
          auto_unbox = TRUE
        )
      )
    },
    extra
  )
  otel_test_span(span_id, attributes, parent = parent)
}

text_part <- function(content) list(list(type = "text", content = content))

otel_test_envelope <- function(span_ids, start_times = NULL) {
  spans <- Map(
    function(span_id, start_time) {
      span <- list(
        spanId = span_id,
        attributes = list(list(
          key = "gen_ai.operation.name",
          value = list(stringValue = "chat")
        ))
      )
      if (!is.na(start_time)) {
        span$startTimeUnixNano <- as.character(start_time)
      }
      span
    },
    span_ids,
    start_times %||% rep(NA_real_, length(span_ids))
  )
  jsonlite::toJSON(
    list(
      resourceSpans = list(list(
        scopeSpans = list(list(spans = spans))
      ))
    ),
    auto_unbox = TRUE
  )
}

otel_test_framework_envelope <- function(span_id) {
  jsonlite::toJSON(
    list(
      resourceSpans = list(list(
        scopeSpans = list(list(
          spans = list(list(
            spanId = span_id,
            attributes = list(list(
              key = "shiny.reactive",
              value = list(stringValue = "x")
            ))
          ))
        ))
      ))
    ),
    auto_unbox = TRUE
  )
}

# Serialise parsed test spans back into one OTLP NDJSON line.
otel_test_otlp_line <- function(spans) {
  otlp <- lapply(spans, function(span) {
    list(
      traceId = span$trace_id,
      spanId = span$span_id,
      parentSpanId = span$parent_span_id,
      name = span$name,
      startTimeUnixNano = span$start_time,
      endTimeUnixNano = span$end_time,
      attributes = unname(Map(
        function(key, value) {
          list(
            key = key,
            value = if (is.numeric(value)) {
              list(intValue = as.character(value))
            } else {
              list(stringValue = as.character(value))
            }
          )
        },
        names(span$attributes),
        span$attributes
      ))
    )
  })
  as.character(jsonlite::toJSON(
    list(resourceSpans = list(list(scopeSpans = list(list(spans = otlp))))),
    auto_unbox = TRUE
  ))
}

# One single-call conversation per id, starting `start` seconds after epoch.
otel_test_conversation_line <- function(conversation, start) {
  span <- otel_chat_span(
    span_id = paste0("chat-", conversation),
    conversation = conversation,
    input = list(list(role = "user", parts = text_part(conversation))),
    output = list(list(role = "assistant", parts = text_part("ok")))
  )
  span$trace_id <- paste0("trace-", conversation)
  span$start_time <- format(start * 1e9, scientific = FALSE)
  span$end_time <- format((start + 1) * 1e9, scientific = FALSE)
  otel_test_otlp_line(list(span))
}
