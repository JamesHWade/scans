# Authored, provider-free support conversations. Regenerate from the repo root.
# No production messages, provider responses, or API calls are used.
message <- function(role, content) {
  list(role = role, parts = list(list(type = "text", content = content)))
}
request <- function(id, name, arguments) {
  list(type = "tool_call", id = id, name = name, arguments = arguments)
}
response <- function(id, text) {
  list(
    role = "tool",
    parts = list(list(type = "tool_call_response", id = id, response = text))
  )
}
span <- function(conversation, id, start, end, attributes) {
  origin <- as.numeric(as.POSIXct("2026-09-01 12:00:00", tz = "UTC"))
  list(
    trace_id = paste0("trace-", conversation),
    span_id = id,
    parent_span_id = "",
    name = id,
    scope = "scans.offline-example",
    start_time = sprintf("%.0f", (origin + start) * 1e9),
    end_time = if (is.na(end)) NULL else sprintf("%.0f", (origin + end) * 1e9),
    attributes = c(list("gen_ai.conversation.id" = conversation), attributes)
  )
}
chat <- function(
  conversation,
  id,
  start,
  end,
  input,
  output,
  input_tokens,
  output_tokens
) {
  span(
    conversation,
    id,
    start,
    end,
    list(
      "gen_ai.operation.name" = "chat",
      "gen_ai.request.model" = "example-support-model",
      "gen_ai.input.messages" = jsonlite::toJSON(input, auto_unbox = TRUE),
      "gen_ai.output.messages" = jsonlite::toJSON(output, auto_unbox = TRUE),
      "gen_ai.usage.input_tokens" = input_tokens,
      "gen_ai.usage.output_tokens" = output_tokens
    )
  )
}
tool <- function(conversation, id, start, end, name, error = NULL) {
  span(
    conversation,
    id,
    start,
    end,
    c(
      list(
        "gen_ai.operation.name" = "execute_tool",
        "gen_ai.tool.call.id" = id,
        "gen_ai.tool.name" = name
      ),
      if (!is.null(error)) list("error.type" = error)
    )
  )
}

spans <- list()
history <- list(message(
  "user",
  "Where is order DEMO-104? The tracking page has not changed."
))
starts <- c(0, 8, 17)
ends <- c(3, 11, 20)
tool_ends <- c(8, 17, 29)
for (i in 1:3) {
  id <- paste0("lookup-", i)
  output <- list(list(
    role = "assistant",
    parts = list(request(id, "lookup_order", list(order = "DEMO-104")))
  ))
  spans <- c(
    spans,
    list(
      chat(
        "retry",
        paste0("retry-chat-", i),
        starts[[i]],
        ends[[i]],
        history,
        output,
        100 * i,
        20
      ),
      tool(
        "retry",
        id,
        ends[[i]],
        tool_ends[[i]],
        "lookup_order",
        "order_service_timeout"
      )
    )
  )
  history <- c(
    history,
    output,
    list(response(
      id,
      "The order service timed out. No shipment data was returned."
    ))
  )
}
spans <- c(
  spans,
  list(chat(
    "retry",
    "retry-final",
    29,
    33,
    history,
    list(message(
      "assistant",
      "I could not retrieve this order. Please contact the support desk; I cannot confirm its shipping status."
    )),
    400,
    35
  ))
)

first <- list(message("user", "Can I return an unopened item?"))
answer <- list(message(
  "assistant",
  "Unopened items can be returned within the example store's 30-day window."
))
spans <- c(
  spans,
  list(
    chat("follow-up", "policy-first", 200, 202, first, answer, 90, 25),
    chat(
      "follow-up",
      "policy-second",
      322,
      326,
      c(first, answer, list(message("user", "Does that include gifts?"))),
      list(message(
        "assistant",
        "Yes. Keep the gift receipt so the support desk can find the order."
      )),
      150,
      30
    )
  )
)

first <- list(message(
  "user",
  "Is the blue jacket in stock, and what is its return window?"
))
calls <- list(list(
  role = "assistant",
  parts = list(
    request("stock", "check_stock", list(item = "blue-jacket")),
    request("policy", "return_policy", list(item = "blue-jacket"))
  )
))
history <- c(
  first,
  calls,
  list(
    response("stock", "The blue jacket is in stock."),
    response("policy", "Unworn jackets have a 30-day return window.")
  )
)
spans <- c(
  spans,
  list(
    chat("parallel", "parallel-first", 400, 402, first, calls, 120, 40),
    tool("parallel", "stock", 402, 407, "check_stock"),
    tool("parallel", "policy", 403, 408, "return_policy"),
    chat(
      "parallel",
      "parallel-final",
      408,
      410,
      history,
      list(message(
        "assistant",
        "The blue jacket is in stock. You have 30 days to return it unworn."
      )),
      230,
      35
    )
  )
)

# Content capture is off for these calls and the second end timestamp is absent.
spans <- c(
  spans,
  list(
    span(
      "limited-capture",
      "limited-first",
      500,
      504,
      list(
        "gen_ai.operation.name" = "chat",
        "gen_ai.usage.input_tokens" = 75
      )
    ),
    span(
      "limited-capture",
      "limited-second",
      510,
      NA,
      list("gen_ai.operation.name" = "chat")
    )
  )
)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  spans,
  "inst/extdata/support-investigation.json",
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
