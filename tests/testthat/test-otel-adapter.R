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

otel_test_envelope <- function(span_ids) {
  spans <- lapply(span_ids, function(span_id) list(spanId = span_id))
  jsonlite::toJSON(
    list(
      resourceSpans = list(list(
        scopeSpans = list(list(spans = spans))
      ))
    ),
    auto_unbox = TRUE
  )
}

test_that("OTEL conversion requires jsonlite", {
  testthat::local_mocked_bindings(
    otel_jsonlite_available = function() FALSE
  )

  expect_snapshot(error = TRUE, as_trajectory_otel(list()))
})

test_that("a chat span becomes turns and events", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    system = list(list(type = "text", content = "You are helpful.")),
    input = list(list(role = "user", parts = text_part("Hello"))),
    output = list(list(role = "assistant", parts = text_part("Hi there")))
  )
  bundle <- as_trajectory_otel(list(span))

  turns <- trajectory_turns(bundle)
  events <- trajectory_events(bundle)
  expect_equal(turns$role, c("system", "user", "assistant"))
  expect_equal(events$event_type, rep("content", 3L))
  expect_equal(events$text, c("You are helpful.", "Hello", "Hi there"))
  expect_equal(trajectory_info(bundle)$model, "gpt-test")
  expect_equal(trajectory_info(bundle)$agent, "openai")
})

test_that("tool calls and results carry their identifiers", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(
      list(role = "user", parts = text_part("Search")),
      list(
        role = "assistant",
        parts = list(list(
          type = "tool_call",
          id = "call-1",
          name = "search",
          arguments = list(query = "polymers")
        ))
      ),
      list(
        role = "tool",
        parts = list(list(
          type = "tool_call_response",
          id = "call-1",
          response = "3 results"
        ))
      )
    ),
    output = list(list(role = "assistant", parts = text_part("Done")))
  )
  bundle <- as_trajectory_otel(list(span))
  events <- trajectory_events(bundle)

  call <- events[events$event_type == "tool_call", ]
  result <- events[events$event_type == "tool_result", ]
  expect_equal(call$name, "search")
  expect_equal(call$call_id, "call-1")
  expect_equal(call$value[[1L]]$query, "polymers")
  expect_equal(result$call_id, "call-1")
  expect_equal(result$text, "3 results")
})

test_that("an execute_tool span supplies timing and failure", {
  skip_if_not_installed("jsonlite")
  chat <- otel_chat_span(
    input = list(
      list(
        role = "assistant",
        parts = list(list(
          type = "tool_call",
          id = "call-9",
          name = "search"
        ))
      ),
      list(
        role = "tool",
        parts = list(list(
          type = "tool_call_response",
          id = "call-9",
          response = "failed"
        ))
      )
    ),
    output = list(list(role = "assistant", parts = text_part("Sorry")))
  )
  tool <- otel_test_span(
    "tool-1",
    list(
      "gen_ai.operation.name" = "execute_tool",
      "gen_ai.tool.name" = "search",
      "gen_ai.tool.call.id" = "call-9",
      "error.type" = "simpleError",
      "gen_ai.conversation.id" = "conv-1"
    ),
    start = 1e9,
    end = 3.5e9
  )
  bundle <- as_trajectory_otel(list(chat, tool))
  events <- trajectory_events(bundle)
  call <- events[events$event_type == "tool_call", ]
  result <- events[events$event_type == "tool_result", ]

  expect_equal(call$status, "failed")
  expect_equal(call$error, "simpleError")
  expect_equal(call$duration, 2.5)
  expect_identical(result$status, "completed")
  expect_identical(result$error, NA_character_)
  expect_identical(result$duration, NA_real_)
  expect_identical(sum(events$status == "failed"), 1L)
  expect_equal(trajectory_info(bundle)$status, "failed")
})

test_that("standard OTLP status marks model and tool spans as failed", {
  skip_if_not_installed("jsonlite")
  chat <- otel_chat_span(
    input = list(
      list(
        role = "assistant",
        parts = list(list(
          type = "tool_call",
          id = "call-status",
          name = "search"
        ))
      )
    ),
    output = list(list(role = "assistant", parts = text_part("Partial")))
  )
  chat$status <- list(code = "STATUS_CODE_ERROR", message = "Model failed")
  tool <- otel_test_span(
    "tool-status",
    list(
      "gen_ai.operation.name" = "execute_tool",
      "gen_ai.tool.call.id" = "call-status",
      "gen_ai.conversation.id" = "conv-1"
    )
  )
  tool$status <- list(code = 2L, message = "Tool timed out")

  bundle <- as_trajectory_otel(list(chat, tool))
  output <- trajectory_turns(bundle)
  output <- utils::tail(output[output$role == "assistant", ], 1L)
  call <- trajectory_events(bundle)
  call <- call[call$event_type == "tool_call", ]

  expect_identical(output$status, "failed")
  expect_identical(output$error, "Model failed")
  expect_identical(call$status, "failed")
  expect_identical(call$error, "Tool timed out")
  expect_identical(trajectory_info(bundle)$status, "failed")
  expect_identical(trajectory_info(bundle)$error, "Model failed")
})

test_that("OTLP parsing retains the standard span status", {
  span <- otel_span(list(
    status = list(code = "ERROR", message = "Provider refused the request")
  ))

  expect_identical(
    span$status,
    list(code = "ERROR", message = "Provider refused the request")
  )
  expect_true(otel_span_failed(span))
  expect_identical(otel_span_error(span), "Provider refused the request")
})

test_that("failed chats do not assign model metrics to input turns", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(role = "user", parts = text_part("Hello")))
  )
  span$attributes[["error.type"]] <- "provider_error"

  bundle <- as_trajectory_otel(list(span))
  turns <- trajectory_turns(bundle)

  expect_identical(turns$role, "user")
  expect_identical(turns$input_tokens, NA_real_)
  expect_identical(turns$output_tokens, NA_real_)
  expect_identical(turns$duration, NA_real_)
  expect_identical(turns$status, "completed")
  info <- trajectory_info(bundle)
  expect_identical(info$status, "failed")
  expect_identical(info$error, "provider_error")
})

test_that("failed model output carries its own metrics and status", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(role = "user", parts = text_part("Hello"))),
    output = list(list(role = "assistant", parts = text_part("Partial")))
  )
  span$attributes[["error.type"]] <- "provider_error"

  bundle <- as_trajectory_otel(list(span))
  turns <- trajectory_turns(bundle)
  output <- turns[turns$role == "assistant", ]

  expect_identical(output$input_tokens, 100)
  expect_identical(output$output_tokens, 20)
  expect_identical(output$status, "failed")
  expect_identical(output$error, "provider_error")
})

test_that("parts with no readable content are recorded as losses", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(
      role = "assistant",
      parts = list(list(
        type = "generic",
        class = "ContentToolRequestSearch"
      ))
    )),
    output = list(list(role = "assistant", parts = text_part("Done")))
  )
  bundle <- as_trajectory_otel(list(span))
  events <- trajectory_events(bundle)
  losses <- trajectory_losses(bundle)

  expect_true("custom" %in% events$event_type)
  expect_equal(
    events$name[events$event_type == "custom"],
    "ContentToolRequestSearch"
  )
  expect_equal(nrow(losses), 1L)
  expect_equal(losses$reason, "unsupported")
})

test_that("spans group by conversation id, walking up to a parent", {
  skip_if_not_installed("jsonlite")
  parent <- otel_test_span(
    "root",
    list("gen_ai.conversation.id" = "conv-A"),
    name = "wrapper"
  )
  child <- otel_chat_span(
    span_id = "chat-A",
    conversation = NULL,
    parent = "root",
    input = list(list(role = "user", parts = text_part("A"))),
    output = list(list(role = "assistant", parts = text_part("a")))
  )
  other <- otel_chat_span(
    span_id = "chat-B",
    conversation = "conv-B",
    input = list(list(role = "user", parts = text_part("B"))),
    output = list(list(role = "assistant", parts = text_part("b")))
  )

  groups <- otel_group_conversations(list(parent, child, other))
  expect_setequal(names(groups), c("conv-A", "conv-B"))
  expect_equal(nrow(trajectory_info(as_trajectory_otel(groups))), 2L)
})

test_that("time filtering retains padded ancestors for conversation grouping", {
  parent <- otel_test_span(
    "root",
    list("gen_ai.conversation.id" = "conv-A"),
    start = 1e9,
    end = 2e9
  )
  child <- otel_chat_span(
    span_id = "chat-A",
    conversation = NULL,
    parent = "root"
  )
  child$start_time <- as.character(3e9)
  child$end_time <- as.character(4e9)

  groups <- otel_group_conversations_in_window(
    list(parent, child),
    from = as.POSIXct(2, origin = "1970-01-01", tz = "UTC"),
    to = as.POSIXct(5, origin = "1970-01-01", tz = "UTC")
  )

  expect_named(groups, "conv-A")
  expect_equal(vapply(groups[[1L]], `[[`, character(1), "span_id"), "chat-A")
})

test_that("a conversation with no id falls back to its trace", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    conversation = NULL,
    input = list(list(role = "user", parts = text_part("Hi"))),
    output = list(list(role = "assistant", parts = text_part("Hello")))
  )
  groups <- otel_group_conversations(list(span))
  expect_named(groups, "trace-1")
})

test_that("only the most recent chat span rebuilds the transcript", {
  skip_if_not_installed("jsonlite")
  early <- otel_chat_span(
    span_id = "chat-early",
    input = list(list(role = "user", parts = text_part("First"))),
    output = list(list(role = "assistant", parts = text_part("one"))),
    extra = list()
  )
  late <- otel_chat_span(
    span_id = "chat-late",
    input = list(
      list(role = "user", parts = text_part("First")),
      list(role = "assistant", parts = text_part("one")),
      list(role = "user", parts = text_part("Second"))
    ),
    output = list(list(role = "assistant", parts = text_part("two")))
  )
  late$start_time <- as.character(5e9)
  late$end_time <- as.character(6e9)

  bundle <- as_trajectory_otel(list(early, late))
  expect_equal(nrow(trajectory_turns(bundle)), 4L)
  expect_true("Second" %in% trajectory_events(bundle)$text)
  # Both calls are counted even though one rebuilt the transcript.
  expect_equal(trajectory_info(bundle)$metadata[[1L]]$otel$model_calls, 2L)
  expect_equal(trajectory_info(bundle)$metadata[[1L]]$otel$input_tokens, 200)
})

test_that("trajectory metadata is sanitized and losses are retained", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(role = "user", parts = text_part("Hello"))),
    output = list(list(role = "assistant", parts = text_part("Hi")))
  )
  bundle <- as_trajectory_otel(
    list(span),
    metadata = list(
      api_key = "secret",
      live = globalenv(),
      provider = "fixture"
    )
  )
  metadata <- trajectory_info(bundle)$metadata[[1L]]
  losses <- trajectory_losses(bundle)

  expect_identical(metadata$api_key, "<redacted>")
  expect_identical(metadata$live, "<unsupported>")
  expect_identical(metadata$provider, "fixture")
  expect_identical(metadata$otel$model_calls, 1L)
  expect_setequal(losses$field, c("metadata$api_key", "metadata$live"))
})

test_that("source URI credentials and parameters are not retained", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(role = "user", parts = text_part("Hello"))),
    output = list(list(role = "assistant", parts = text_part("Hi")))
  )
  bundle <- as_trajectory_otel(
    list(span),
    source_uri = paste0(
      "https://user:secret@example.com/traces",
      "?access_token=secret#fragment"
    )
  )

  expect_identical(
    trajectory_info(bundle)$source_uri,
    "https://example.com/traces"
  )
  expect_identical(trajectory_losses(bundle)$field, "source_uri")
  expect_identical(trajectory_losses(bundle)$reason, "redacted")
})

test_that("an unfamiliar role is namespaced rather than dropped", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(role = "developer", parts = text_part("Note"))),
    output = list(list(role = "assistant", parts = text_part("ok")))
  )
  bundle <- as_trajectory_otel(list(span))
  expect_true("otel:developer" %in% trajectory_turns(bundle)$role)
})

test_that("a span whose clock ran backwards yields no duration", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(role = "user", parts = text_part("Hi"))),
    output = list(list(role = "assistant", parts = text_part("Hello")))
  )
  span$start_time <- as.character(9e9)
  span$end_time <- as.character(1e9)
  bundle <- as_trajectory_otel(list(span))
  expect_true(all(is.na(trajectory_turns(bundle)$duration)))
})

test_that("empty input produces an empty bundle, not an error", {
  expect_equal(nrow(trajectory_info(as_trajectory_otel(list()))), 0L)
})

test_that("a content GUID is recognised in a guid, content URL, or dashboard URL", {
  guid <- "11111111-1111-4111-8111-111111111111"
  expect_equal(connect_source_guid(guid), guid)
  expect_equal(
    connect_source_guid(paste0(
      "https://connect.example.com/content/",
      guid,
      "/"
    )),
    guid
  )
  expect_equal(
    connect_source_guid(paste0(
      "https://connect.example.com/connect/#/apps/",
      guid
    )),
    guid
  )
  expect_error(
    connect_source_guid("not-a-guid"),
    class = "scans_error_connect_source"
  )
})

test_that("missing Connect credentials are reported before any request", {
  withr::local_envvar(CONNECT_SERVER = NA, CONNECT_API_KEY = NA)
  expect_error(
    connect_client(NULL, NULL),
    class = "scans_error_connect_credentials"
  )
})

test_that("explicit NULL Connect bounds remain open", {
  bounds <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    connect_client = function(...) list(server = "https://connect.example.com"),
    connect_trace_lines = function(client, guid, from, to, ...) {
      bounds$from <- from
      bounds$to <- to
      character()
    }
  )
  guid <- "11111111-1111-4111-8111-111111111111"
  historical <- as.POSIXct("2026-01-01", tz = "UTC")

  read_connect_traces(guid, from = NULL, to = historical)
  expect_null(bounds$from)
  expect_identical(bounds$to, historical)

  read_connect_traces(guid, from = historical, to = NULL)
  expect_identical(bounds$from, historical)
  expect_null(bounds$to)
})

test_that("fallback conversation counts respect the requested time window", {
  inside <- otel_chat_span(span_id = "inside", conversation = "inside")
  inside$start_time <- as.character(3e9)
  after <- otel_chat_span(span_id = "after", conversation = "after")
  after$start_time <- as.character(7e9)
  testthat::local_mocked_bindings(
    otel_parse_otlp_lines = function(lines) list(inside, after)
  )

  expect_equal(
    connect_conversation_count(
      "lines",
      from = as.POSIXct(2, origin = "1970-01-01", tz = "UTC"),
      to = as.POSIXct(5, origin = "1970-01-01", tz = "UTC")
    ),
    1L
  )
})

test_that("content-wide and retained per-job traces are merged", {
  skip_if_not_installed("httr2")
  aggregate_response <- httr2::response(
    headers = list(`X-Total-Count` = "1"),
    body = charToRaw("aggregate\n")
  )
  jobs_response <- httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw('[{"key":"job-1","start_time":"2026-01-01"}]')
  )
  legacy_response <- httr2::response(
    headers = list(`X-Total-Count` = "2"),
    body = charToRaw("aggregate\nlegacy\n")
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      path <- sub("\\?.*$", "", request$url)
      if (endsWith(path, "/jobs")) {
        return(jobs_response)
      }
      if (grepl("/jobs/[^/]+/traces$", path)) {
        return(legacy_response)
      }
      aggregate_response
    }
  )

  lines <- connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 10L,
    n = NULL,
    call = rlang::caller_env(),
    page_size = 10L
  )

  expect_identical(lines, c("aggregate", "legacy"))
})

test_that("post-window legacy spans do not consume max_spans", {
  skip_if_not_installed("httr2")
  urls <- character()
  page_number <- 0L
  jobs_response <- httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw('[{"key":"job-1","start_time":"2026-01-01"}]')
  )
  trace_response <- httr2::response(
    headers = list(`X-Total-Count` = "3")
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      if (endsWith(sub("\\?.*$", "", request$url), "/jobs")) {
        return(jobs_response)
      }
      urls <<- c(urls, request$url)
      page_number <<- page_number + 1L
      trace_response
    },
    connect_trace_page = function(response) {
      if (page_number == 1L) c("after-1", "after-2") else "inside"
    },
    connect_trace_lines_before = function(lines, to) {
      if (identical(lines, c("after-1", "after-2"))) character() else lines
    }
  )

  lines <- connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = as.POSIXct("2026-01-01", tz = "UTC"),
    max_spans = 2L,
    n = NULL,
    call = rlang::caller_env(),
    page_size = 2L
  )

  expect_identical(lines, "inside")
  expect_length(urls, 2L)
  expect_match(urls[[1L]], "limit=2", fixed = TRUE)
  expect_match(urls[[2L]], "limit=2", fixed = TRUE)
})

test_that("legacy trace filtering keeps only relevant or unclassified lines", {
  skip_if_not_installed("jsonlite")
  before <- paste0(
    '{"resourceSpans":[{"scopeSpans":[{"spans":[',
    '{"spanId":"before","startTimeUnixNano":"1000000000"}',
    "]}]}]}"
  )
  after <- paste0(
    '{"resourceSpans":[{"scopeSpans":[{"spans":[',
    '{"spanId":"after","startTimeUnixNano":"3000000000"}',
    "]}]}]}"
  )

  lines <- connect_trace_lines_before(
    c(before, after, "malformed"),
    as.POSIXct(2, origin = "1970-01-01", tz = "UTC")
  )

  expect_identical(lines, c(before, "malformed"))
})

test_that("per-job pagination never requests beyond max_spans", {
  skip_if_not_installed("httr2")
  urls <- character()
  page_number <- 0L
  jobs_response <- httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw('[{"key":"job-1","start_time":"2026-01-01"}]')
  )
  trace_response <- httr2::response(
    headers = list(`X-Total-Count` = "100")
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      if (endsWith(sub("\\?.*$", "", request$url), "/jobs")) {
        return(jobs_response)
      }
      urls <<- c(urls, request$url)
      page_number <<- page_number + 1L
      trace_response
    },
    connect_trace_page = function(response) {
      if (page_number == 1L) c("span-1", "span-2") else "span-3"
    }
  )

  lines <- connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 3L,
    n = NULL,
    call = rlang::caller_env(),
    page_size = 2L
  )

  expect_length(lines, 3L)
  expect_length(urls, 2L)
  expect_match(urls[[1L]], "limit=2", fixed = TRUE)
  expect_match(urls[[2L]], "limit=1", fixed = TRUE)
})

test_that("nested OTLP spans respect the aggregate span budget", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  requests <- 0L
  batch <- otel_test_envelope(c("one", "two", "three"))
  response <- httr2::response(
    headers = list(`X-Total-Count` = "1"),
    body = charToRaw(batch)
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      requests <<- requests + 1L
      response
    }
  )

  lines <- suppressWarnings(connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 2L,
    n = NULL,
    call = rlang::caller_env(),
    page_size = 10L
  ))
  spans <- otel_parse_otlp_lines(lines, max_spans = 2L)

  expect_identical(requests, 1L)
  expect_identical(
    vapply(spans, `[[`, character(1), "span_id"),
    c("one", "two")
  )
})

test_that("nested OTLP spans respect the per-job span budget", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  trace_requests <- 0L
  jobs_response <- httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw('[{"key":"job-1","start_time":"2026-01-01"}]')
  )
  trace_response <- httr2::response(
    headers = list(`X-Total-Count` = "10"),
    body = charToRaw(otel_test_envelope(c("one", "two", "three")))
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      if (endsWith(sub("\\?.*$", "", request$url), "/jobs")) {
        return(jobs_response)
      }
      trace_requests <<- trace_requests + 1L
      trace_response
    }
  )

  lines <- connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 2L,
    n = NULL,
    call = rlang::caller_env(),
    page_size = 10L
  )
  spans <- otel_parse_otlp_lines(lines, max_spans = 2L)

  expect_identical(trace_requests, 1L)
  expect_identical(
    vapply(spans, `[[`, character(1), "span_id"),
    c("one", "two")
  )
})

test_that("oversized text is truncated and recorded as a loss", {
  skip_if_not_installed("jsonlite")
  big <- strrep("x", 70000L)
  span <- otel_chat_span(
    input = list(list(
      role = "tool",
      parts = list(list(
        type = "tool_call_response",
        id = "call-1",
        response = big
      ))
    )),
    output = list(list(role = "assistant", parts = text_part("Summarised")))
  )
  bundle <- as_trajectory_otel(list(span))
  events <- trajectory_events(bundle)
  losses <- trajectory_losses(bundle)

  result <- events[events$event_type == "tool_result", ]
  expect_lte(nchar(result$text, type = "bytes"), 65536L)
  expect_true(endsWith(result$text, "…"))
  expect_equal(losses$reason, "truncated")
  expect_equal(losses$field, "response")
  # The rest of the conversation survives the oversized part.
  expect_true("Summarised" %in% events$text)
})

test_that("oversized structured tool values are truncated with losses", {
  skip_if_not_installed("jsonlite")
  big <- list(document = strrep("x", 70000L))
  span <- otel_chat_span(
    input = list(
      list(
        role = "assistant",
        parts = list(list(
          type = "tool_call",
          id = "call-1",
          name = "search",
          arguments = big
        ))
      ),
      list(
        role = "tool",
        parts = list(list(
          type = "tool_call_response",
          id = "call-1",
          response = big
        ))
      )
    ),
    output = list(list(role = "assistant", parts = text_part("Summarised")))
  )

  bundle <- as_trajectory_otel(list(span))
  events <- trajectory_events(bundle)
  losses <- trajectory_losses(bundle)

  tool_values <- events$value[
    events$event_type %in% c("tool_call", "tool_result")
  ]
  documents <- vapply(tool_values, `[[`, character(1), "document")
  expect_true(all(nchar(documents, type = "bytes") < 65536L))
  expect_true(all(endsWith(documents, "...")))
  expect_equal(losses$reason, rep("truncated", 2L))
  expect_setequal(losses$field, c("arguments$document", "response$document"))
})

test_that("truncation does not split a multibyte character", {
  skip_if_not_installed("jsonlite")
  span <- otel_chat_span(
    input = list(list(role = "user", parts = text_part(strrep("é", 40000L)))),
    output = list(list(role = "assistant", parts = text_part("ok")))
  )
  bundle <- as_trajectory_otel(list(span))
  text <- trajectory_events(bundle)$text[[1L]]
  expect_lte(nchar(text, type = "bytes"), 65536L)
  expect_false(anyNA(utf8ToInt(substr(text, 1L, nchar(text) - 1L))))
})
