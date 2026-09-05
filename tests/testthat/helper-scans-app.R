scans_app_connect_trace_line <- function(prompt) {
  start_time <- sprintf("%.0f", (as.numeric(Sys.time()) - 60) * 1e9)
  end_time <- sprintf("%.0f", (as.numeric(Sys.time()) - 59) * 1e9)
  messages <- paste0(
    '[{\\"role\\":\\"user\\",\\"parts\\":[{\\"type\\":\\"text\\",',
    '\\"content\\":\\"',
    prompt,
    '\\"}]}]'
  )
  paste0(
    '{"resourceSpans":[{"scopeSpans":[{"spans":[{',
    '"traceId":"0123456789abcdef","spanId":"0123456789abcdef",',
    '"name":"chat test-model","startTimeUnixNano":"',
    start_time,
    '","endTimeUnixNano":"',
    end_time,
    '","attributes":[',
    '{"key":"gen_ai.operation.name","value":{"stringValue":"chat"}},',
    '{"key":"gen_ai.input.messages","value":{"stringValue":"',
    messages,
    '"}}]}]}]}]}'
  )
}
