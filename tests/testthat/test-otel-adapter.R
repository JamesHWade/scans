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
  # The model answered after the tool failed, so the conversation completed;
  # the failure is on the tool event and counted in the metadata.
  expect_equal(trajectory_info(bundle)$status, "completed")
  expect_identical(
    trajectory_info(bundle)$metadata[[1L]]$otel$failed_spans,
    1L
  )
  expect_identical(result$parent_event_id, call$event_id)
})

test_that("a tool span without a conversation id joins its trace's conversation", {
  skip_if_not_installed("jsonlite")
  chat <- otel_chat_span(
    parent = "agent",
    input = list(
      list(
        role = "assistant",
        parts = list(list(type = "tool_call", id = "call-9", name = "search"))
      ),
      list(
        role = "tool",
        parts = list(list(
          type = "tool_call_response",
          id = "call-9",
          response = "ok"
        ))
      )
    ),
    output = list(list(role = "assistant", parts = text_part("Done")))
  )
  # ellmer's execute_tool span is a sibling of the chat span under the agent
  # span and carries no conversation id of its own.
  tool <- otel_test_span(
    "tool-1",
    list(
      "gen_ai.operation.name" = "execute_tool",
      "gen_ai.tool.name" = "search",
      "gen_ai.tool.call.id" = "call-9"
    ),
    parent = "agent",
    start = 1e9,
    end = 3e9
  )
  groups <- otel_group_conversations(list(chat, tool))
  expect_named(groups, "conv-1")
  expect_length(groups[[1L]], 2L)

  events <- trajectory_events(as_trajectory_otel(list(chat, tool)))
  expect_equal(events$duration[events$event_type == "tool_call"], 2)
})

test_that("duplicate spans are counted once", {
  skip_if_not_installed("jsonlite")
  chat <- otel_chat_span(
    output = list(list(role = "assistant", parts = text_part("Hi")))
  )
  bundle <- as_trajectory_otel(list(chat, chat, chat))
  otel <- trajectory_info(bundle)$metadata[[1L]]$otel
  expect_identical(otel$model_calls, 1L)
  expect_equal(otel$input_tokens, 100)
})

test_that("conversation status follows the latest model call", {
  skip_if_not_installed("jsonlite")
  first <- otel_chat_span(
    span_id = "chat-1",
    output = list(list(role = "assistant", parts = text_part("x"))),
    extra = list("error.type" = "rate_limit")
  )
  second <- otel_chat_span(
    span_id = "chat-2",
    output = list(list(role = "assistant", parts = text_part("Hello")))
  )
  second$start_time <- as.character(5e9)
  second$end_time <- as.character(6e9)
  info <- trajectory_info(as_trajectory_otel(list(first, second)))
  expect_identical(info$status, "completed")
  expect_identical(info$error, NA_character_)
  expect_identical(info$metadata[[1L]]$otel$failed_spans, 1L)
})

test_that("transport attributes and URL secrets stay out of metadata", {
  skip_if_not_installed("jsonlite")
  chat <- otel_chat_span(
    output = list(list(role = "assistant", parts = text_part("Hi"))),
    extra = list(
      "url.full" = "https://api.example.com/v1?key=SECRET",
      "http.request.method" = "POST",
      "user_agent.original" = "httr2",
      "custom.link" = "https://user:pw@files.example.com/p?tok=SECRET",
      "shiny.session" = "s-1"
    )
  )
  attributes <- trajectory_info(as_trajectory_otel(list(chat)))$metadata[[
    1L
  ]]$otel$attributes
  expect_false(any(
    c("url.full", "http.request.method", "user_agent.original") %in%
      names(attributes)
  ))
  expect_identical(attributes$custom.link, "https://files.example.com/p")
  expect_identical(attributes$shiny.session, "s-1")
  expect_no_match(
    paste(unlist(attributes), collapse = " "),
    "SECRET",
    fixed = TRUE
  )
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
  # The window selects the conversation through its model call; the earlier
  # parent stays as context because it precedes `to`.
  expect_setequal(
    vapply(groups[[1L]], `[[`, character(1), "span_id"),
    c("root", "chat-A")
  )
})

test_that("the window selects conversations but keeps their earlier history", {
  skip_if_not_installed("jsonlite")
  early <- otel_chat_span(
    span_id = "chat-early",
    output = list(list(role = "assistant", parts = text_part("first")))
  )
  tool <- otel_test_span(
    "tool-1",
    list(
      "gen_ai.operation.name" = "execute_tool",
      "gen_ai.tool.name" = "search",
      "gen_ai.tool.call.id" = "call-1",
      "gen_ai.conversation.id" = "conv-1",
      "error.type" = "simpleError"
    ),
    start = 1.5e9,
    end = 2e9
  )
  late <- otel_chat_span(
    span_id = "chat-late",
    output = list(list(role = "assistant", parts = text_part("second")))
  )
  late$start_time <- as.character(6e9)
  late$end_time <- as.character(7e9)
  after <- otel_chat_span(
    span_id = "chat-after",
    output = list(list(role = "assistant", parts = text_part("third")))
  )
  after$start_time <- as.character(9e9)
  after$end_time <- as.character(9.5e9)

  groups <- otel_group_conversations_in_window(
    list(early, tool, late, after),
    from = as.POSIXct(5, origin = "1970-01-01", tz = "UTC"),
    to = as.POSIXct(8, origin = "1970-01-01", tz = "UTC")
  )
  ids <- vapply(groups[[1L]], `[[`, character(1), "span_id")
  expect_setequal(ids, c("chat-early", "tool-1", "chat-late"))

  # No model call inside the window: the conversation is not selected.
  expect_length(
    otel_group_conversations_in_window(
      list(early, tool),
      from = as.POSIXct(5, origin = "1970-01-01", tz = "UTC"),
      to = as.POSIXct(8, origin = "1970-01-01", tz = "UTC")
    ),
    0L
  )
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
    call = rlang::caller_env(),
    page_size = 10L
  )

  expect_identical(as.vector(lines), c("aggregate", "legacy"))
})

test_that("post-window legacy spans do not consume max_spans", {
  skip_if_not_installed("httr2")
  urls <- character()
  page_number <- 0L
  to <- as.POSIXct("2026-01-01", tz = "UTC")
  jobs_response <- httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw('[{"key":"job-1","start_time":"2026-01-01"}]')
  )
  trace_response <- httr2::response(headers = list(`X-Total-Count` = "2"))
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
      if (page_number == 1L) {
        otel_test_envelope(
          c("after-1", "after-2"),
          (as.numeric(to) + c(1, 2)) * 1e9
        )
      } else {
        otel_test_envelope("inside", (as.numeric(to) - 1) * 1e9)
      }
    }
  )

  lines <- connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = to,
    max_spans = 2L,
    call = rlang::caller_env(),
    page_size = 2L
  )

  expect_length(lines, 1L)
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

test_that("per-job pagination uses full transport pages", {
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
      if (page_number == 1L) {
        otel_test_envelope(c("span-1", "span-2"))
      } else {
        otel_test_envelope("span-3")
      }
    }
  )

  lines <- connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 3L,
    call = rlang::caller_env(),
    page_size = 2L
  )

  expect_length(lines, 2L)
  expect_length(attr(lines, "spans", exact = TRUE), 3L)
  expect_length(urls, 2L)
  expect_match(urls[[1L]], "limit=2", fixed = TRUE)
  expect_match(urls[[2L]], "limit=2", fixed = TRUE)
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

test_that("post-window nested spans do not consume the legacy span budget", {
  skip_if_not_installed("jsonlite")
  line <- otel_test_envelope(
    c("after", "inside"),
    c(3e9, 1e9)
  )
  to <- as.POSIXct(2, origin = "1970-01-01", tz = "UTC")

  spans <- otel_parse_otlp_lines(line, max_spans = 1L, to = to)

  expect_identical(connect_trace_lines_summary(line, to)$span_count, 1L)
  expect_identical(vapply(spans, `[[`, character(1), "span_id"), "inside")
})

test_that("legacy reader checks every overlapping job", {
  skip_if_not_installed("httr2")
  trace_requests <- character()
  jobs_response <- httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw(paste0(
      '[{"key":"newer-start","start_time":"2026-02-01"},',
      '{"key":"older-start","start_time":"2026-01-01"}]'
    ))
  )
  trace_response <- httr2::response(
    headers = list(`X-Total-Count` = "1")
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      path <- sub("\\?.*$", "", request$url)
      if (endsWith(path, "/jobs")) {
        return(jobs_response)
      }
      trace_requests <<- c(trace_requests, path)
      trace_response
    },
    connect_perform_batch = function(requests, call, ...) {
      lapply(requests, connect_perform, call = call)
    },
    connect_trace_page = function(response) "one-conversation"
  )

  connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 10L,
    call = rlang::caller_env(),
    page_size = 10L
  )

  expect_length(trace_requests, 2L)
  expect_match(trace_requests[[1L]], "newer-start", fixed = TRUE)
  expect_match(trace_requests[[2L]], "older-start", fixed = TRUE)
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

test_that("aggregate trace pages after the first are fetched in batches", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  urls <- character()
  page_lines <- function(offset, limit) {
    ids <- paste0("span-", seq.int(offset + 1L, length.out = limit))
    paste(vapply(ids, otel_test_envelope, character(1)), collapse = "\n")
  }
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    query <- httr2::url_parse(req$url)$query
    offset <- as.integer(query$offset)
    limit <- as.integer(query$limit)
    if (grepl("/jobs$", sub("\\?.*$", "", req$url))) {
      return(httr2::response_json(body = list()))
    }
    httr2::response(
      headers = list(`X-Total-Count` = "7"),
      body = charToRaw(page_lines(offset, min(limit, 7L - offset)))
    )
  })

  lines <- connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 100L,
    call = rlang::caller_env(),
    page_size = 2L
  )

  expect_length(lines, 7L)
  trace_urls <- urls[grepl("/traces", urls, fixed = TRUE)]
  expect_length(trace_urls, 4L)
  offsets <- vapply(
    trace_urls,
    function(url) httr2::url_parse(url)$query$offset,
    character(1),
    USE.NAMES = FALSE
  )
  expect_identical(offsets, c("0", "2", "4", "6"))
  expect_null(attr(lines, "truncated", exact = TRUE))
})

test_that("aggregate transport pages are independent of the GenAI budget", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  urls <- character()
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    query <- httr2::url_parse(req$url)$query
    offset <- as.integer(query$offset)
    limit <- min(as.integer(query$limit), 6L - offset)
    indices <- seq.int(offset + 1L, length.out = limit)
    body <- vapply(
      indices,
      function(index) {
        if (index == 6L) {
          otel_test_envelope("chat")
        } else {
          otel_test_framework_envelope(paste0("framework-", index))
        }
      },
      character(1)
    )
    httr2::response(
      headers = list(`X-Total-Count` = "6"),
      body = charToRaw(paste(body, collapse = "\n"))
    )
  })

  lines <- connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 1L,
    call = rlang::caller_env(),
    page_size = 3L,
    jobs = FALSE
  )

  limits <- vapply(
    urls,
    \(url) httr2::url_parse(url)$query$limit,
    character(1),
    USE.NAMES = FALSE
  )
  expect_identical(limits, c("3", "3"))
  expect_length(lines, 6L)
})

test_that("a read that hits the span ceiling is marked truncated", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  batch <- otel_test_envelope(c("one", "two", "three"))
  response <- httr2::response(
    headers = list(`X-Total-Count` = "1"),
    body = charToRaw(batch)
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) response
  )

  lines <- suppressWarnings(connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 2L,
    call = rlang::caller_env(),
    page_size = 10L
  ))

  expect_true(attr(lines, "truncated", exact = TRUE))
})

test_that("jobs that ended before the window are not read", {
  from <- as.POSIXct("2026-03-10 00:00:00", tz = "UTC")
  jobs <- list(
    list(
      key = "old",
      start_time = "2026-03-01T00:00:00Z",
      end_time = "2026-03-02T00:00:00Z"
    ),
    list(
      key = "edge",
      start_time = "2026-03-09T00:00:00Z",
      end_time = "2026-03-09T23:30:00Z"
    ),
    list(key = "running", start_time = "2026-03-08T00:00:00Z"),
    list(
      key = "recent",
      start_time = "2026-03-11T00:00:00.123456Z",
      end_time = "2026-03-11T02:00:00.5Z"
    ),
    list(start_time = "2026-03-12T00:00:00Z")
  )

  expect_identical(connect_job_keys(jobs, from), c("recent", "edge", "running"))
  expect_identical(
    connect_job_keys(jobs, NULL),
    c("recent", "edge", "running", "old")
  )
})

test_that("job timestamps preserve numeric timezone offsets", {
  midnight <- as.numeric(as.POSIXct("2026-03-10 00:00:00", tz = "UTC"))
  expect_equal(
    connect_parse_time(c(
      "2026-03-10T00:00:00Z",
      "2026-03-10T01:00:00+01:00",
      "2026-03-09T19:00:00-05:00"
    )),
    rep(midnight, 3L)
  )

  jobs <- list(
    list(key = "newest", start_time = "2026-03-09T20:00:00-05:00"),
    list(key = "middle", start_time = "2026-03-10T00:30:00Z"),
    list(key = "oldest", start_time = "2026-03-10T01:00:00+01:00")
  )
  expect_identical(
    connect_job_keys(jobs, NULL),
    c("newest", "middle", "oldest")
  )
  expect_identical(
    connect_job_keys(
      jobs,
      NULL,
      to = as.POSIXct("2026-03-10 00:15:00", tz = "UTC")
    ),
    "oldest"
  )

  overlap <- list(list(
    key = "overlap",
    start_time = "2026-03-09T17:00:00-05:00",
    end_time = "2026-03-09T18:30:00-05:00"
  ))
  expect_identical(
    connect_job_keys(
      overlap,
      as.POSIXct("2026-03-10 00:00:00", tz = "UTC")
    ),
    "overlap"
  )
})

test_that("span counting parses all lines in one batch and respects the window", {
  skip_if_not_installed("jsonlite")
  parses <- 0L
  original <- otel_parse_envelopes
  testthat::local_mocked_bindings(
    otel_parse_envelopes = function(...) {
      parses <<- parses + 1L
      original(...)
    }
  )
  line <- otel_test_envelope(c("a", "b", "c"), c(1e9, 2e9, 3e9))
  to <- as.POSIXct(2.5, origin = "1970-01-01", tz = "UTC")

  expect_identical(
    connect_trace_lines_summary(c(line, "malformed"), to)$span_count,
    2L
  )
  expect_identical(parses, 1L)
})

test_that("spans parsed while paging are reused rather than parsed again", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  historical <- as.POSIXct("2026-01-01", tz = "UTC")
  parses <- 0L
  original <- otel_parse_envelopes_each
  testthat::local_mocked_bindings(
    otel_parse_envelopes_each = function(...) {
      parses <<- parses + 1L
      original(...)
    },
    connect_client = function(...) list(server = "https://connect.example.com"),
    connect_perform = function(request, call) {
      path <- sub("\\?.*$", "", request$url)
      if (endsWith(path, "/jobs")) {
        return(httr2::response_json(body = list()))
      }
      httr2::response(
        headers = list(`X-Total-Count` = "1"),
        body = charToRaw(otel_test_envelope(
          c("chat", "later"),
          c(
            (as.numeric(historical) - 60) * 1e9,
            (as.numeric(historical) + 60) * 1e9
          )
        ))
      )
    }
  )

  conversations <- read_connect_traces(
    "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = historical
  )

  expect_identical(parses, 1L)
  expect_identical(attr(conversations, "read_info")$spans_total, 1L)
})

test_that("read_connect_traces reports how the read went", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  historical <- as.POSIXct("2026-01-01", tz = "UTC")
  testthat::local_mocked_bindings(
    connect_client = function(...) list(server = "https://connect.example.com"),
    connect_trace_lines = function(client, guid, from, to, ...) {
      lines <- otel_test_envelope("chat", (as.numeric(historical) - 60) * 1e9)
      attr(lines, "truncated") <- TRUE
      lines
    },
    otel_group_conversations_in_window = function(spans, from, to) {
      list(a = spans, b = spans, c = spans)
    }
  )

  conversations <- suppressWarnings(read_connect_traces(
    "11111111-1111-4111-8111-111111111111",
    n = 2L,
    from = NULL,
    to = historical
  ))
  info <- attr(conversations, "read_info", exact = TRUE)

  expect_length(conversations, 2L)
  expect_true(info$truncated)
  expect_identical(info$conversations_found, 3L)
  expect_identical(info$conversations, 2L)
  expect_identical(info$spans, 1L)
  expect_identical(info$n, 2L)
  expect_null(info$from)
  expect_identical(info$to, historical)
  expect_s3_class(info$read_at, "POSIXct")
})

test_that("read_connect_traces preserves the positional max_spans slot", {
  expect_identical(
    names(formals(read_connect_traces))[7:8],
    c("max_spans", "jobs")
  )
})

test_that("non-GenAI span and resource attributes are kept as metadata", {
  skip_if_not_installed("jsonlite")
  envelope <- jsonlite::toJSON(
    list(
      resourceSpans = list(list(
        resource = list(
          attributes = list(
            list(key = "service.name", value = list(stringValue = "assistant"))
          )
        ),
        scopeSpans = list(list(
          spans = list(
            list(
              traceId = "t1",
              spanId = "parent",
              name = "session",
              startTimeUnixNano = "1000000000",
              endTimeUnixNano = "3000000000",
              attributes = list(
                list(key = "enduser.id", value = list(stringValue = "ada")),
                list(
                  key = "gen_ai.conversation.id",
                  value = list(stringValue = "c1")
                ),
                list(key = "shiny.session", value = list(stringValue = "abc"))
              )
            ),
            list(
              traceId = "t1",
              spanId = "chat",
              parentSpanId = "parent",
              name = "chat",
              startTimeUnixNano = "2000000000",
              endTimeUnixNano = "2500000000",
              attributes = list(
                list(
                  key = "gen_ai.operation.name",
                  value = list(stringValue = "chat")
                ),
                list(
                  key = "gen_ai.request.model",
                  value = list(stringValue = "m")
                ),
                list(
                  key = "gen_ai.input.messages",
                  value = list(
                    stringValue = jsonlite::toJSON(
                      list(list(
                        role = "user",
                        parts = list(list(type = "text", content = "hi"))
                      )),
                      auto_unbox = TRUE
                    )
                  )
                )
              )
            )
          )
        ))
      ))
    ),
    auto_unbox = TRUE
  )

  spans <- otel_parse_otlp_lines(envelope)
  bundle <- as_trajectory_otel(spans)
  otel <- trajectory_info(bundle)$metadata[[1L]]$otel

  expect_identical(otel$user, "ada")
  expect_identical(otel$attributes$enduser.id, "ada")
  expect_identical(otel$attributes$shiny.session, "abc")
  expect_false("gen_ai.input.messages" %in% names(otel$attributes))
  expect_identical(otel$resource$service.name, "assistant")
  expect_identical(
    scans_app_metadata_user(trajectory_info(bundle)$metadata[[1L]]),
    "ada"
  )
})

test_that("framework spans do not consume the GenAI span budget", {
  skip_if_not_installed("jsonlite")
  span <- function(id, attributes) {
    list(spanId = id, startTimeUnixNano = "1000000000", attributes = attributes)
  }
  reactive <- lapply(1:5, function(i) {
    span(
      paste0("shiny-", i),
      list(list(key = "shiny.reactive", value = list(stringValue = "x")))
    )
  })
  chats <- lapply(1:2, function(i) {
    span(
      paste0("chat-", i),
      list(list(
        key = "gen_ai.operation.name",
        value = list(stringValue = "chat")
      ))
    )
  })
  line <- jsonlite::toJSON(
    list(
      resourceSpans = list(list(
        scopeSpans = list(list(spans = c(reactive, chats)))
      ))
    ),
    auto_unbox = TRUE
  )

  expect_identical(connect_trace_lines_summary(line)$span_count, 2L)
  spans <- otel_parse_otlp_lines(line, max_spans = 1L)
  ids <- vapply(spans, `[[`, character(1), "span_id")
  expect_identical(ids, c(paste0("shiny-", 1:5), "chat-1"))
})

test_that("span budget retains late ancestors as grouping context", {
  child <- otel_chat_span(
    span_id = "chat-A",
    conversation = NULL,
    parent = "root"
  )
  extra <- otel_chat_span(span_id = "chat-B", conversation = "conv-B")
  parent <- otel_test_span(
    "root",
    list(
      "gen_ai.conversation.id" = "conv-A",
      "enduser.id" = "ada"
    ),
    name = "wrapper"
  )

  spans <- otel_limit_spans(list(child, extra, parent), max_spans = 1L)
  groups <- otel_group_conversations(spans)

  expect_identical(
    vapply(spans, `[[`, character(1), "span_id"),
    c("chat-A", "root")
  )
  expect_identical(
    sum(vapply(spans, otel_is_selected_genai_span, logical(1))),
    1L
  )
  expect_named(groups, "conv-A")
  expect_identical(
    vapply(groups[[1L]], `[[`, character(1), "span_id"),
    "chat-A"
  )
  expect_identical(attr(groups[[1L]], "otel_context")$user, "ada")
})

test_that("jobs already covered by the content-wide store are not read", {
  from <- as.POSIXct("2026-03-01", tz = "UTC")
  earliest <- as.numeric(as.POSIXct("2026-03-10", tz = "UTC"))
  jobs <- list(
    list(key = "legacy", start_time = "2026-03-05T00:00:00Z"),
    list(key = "covered", start_time = "2026-03-12T00:00:00Z"),
    list(key = "undated")
  )
  expect_identical(
    connect_job_keys(jobs, from, before = earliest),
    c("legacy", "undated")
  )
  expect_identical(
    connect_job_keys(jobs, from),
    c("covered", "legacy", "undated")
  )
})

test_that("a line summary reports GenAI count and earliest start", {
  skip_if_not_installed("jsonlite")
  line <- otel_test_envelope(c("a", "b"), c(5e9, 2e9))
  summary <- connect_trace_lines_summary(c(line, "malformed"))
  expect_identical(summary$span_count, 2L)
  expect_identical(summary$earliest, 2)
  expect_null(connect_trace_lines_summary(character())$earliest)
})

test_that("jobs = FALSE skips the retained job stores", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  urls <- character()
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      urls <<- c(urls, request$url)
      httr2::response(
        headers = list(`X-Total-Count` = "1"),
        body = charToRaw(otel_test_envelope("one"))
      )
    }
  )

  lines <- connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 10L,
    call = rlang::caller_env(),
    jobs = FALSE
  )

  expect_length(urls, 1L)
  expect_false(any(grepl("/jobs", urls, fixed = TRUE)))
  expect_length(attr(lines, "spans", exact = TRUE), 1L)
})

test_that("jobs = FALSE also skips fallback when the aggregate store is absent", {
  skip_if_not_installed("httr2")
  requests <- 0L
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      requests <<- requests + 1L
      NULL
    }
  )

  lines <- connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 10L,
    call = rlang::caller_env(),
    jobs = FALSE
  )

  expect_identical(requests, 1L)
  expect_length(lines, 0L)
  expect_identical(attr(lines, "spans", exact = TRUE), list())
})

test_that("parallel requests preserve unexpected HTTP failures", {
  rate_limit <- structure(
    list(message = "rate limited", call = NULL),
    class = c("httr2_http_429", "httr2_http", "error", "condition")
  )
  unavailable <- structure(
    list(message = "unavailable", call = NULL),
    class = c("httr2_http_503", "httr2_http", "error", "condition")
  )

  expect_error(
    connect_response_result(rate_limit, rlang::caller_env()),
    class = "httr2_http_429"
  )
  expect_null(connect_response_result(unavailable, rlang::caller_env()))
})

test_that("a failed first retained-job page aborts the read", {
  skip_if_not_installed("httr2")
  jobs_response <- httr2::response_json(
    body = list(list(
      key = "job-1",
      start_time = "2026-01-01T00:00:00Z"
    ))
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) jobs_response,
    connect_perform_batch = function(requests, call, ...) list(NULL)
  )

  expect_error(
    connect_job_trace_lines(
      client = list(server = "https://connect.example.com", api_key = "secret"),
      guid = "11111111-1111-4111-8111-111111111111",
      from = NULL,
      to = NULL,
      max_spans = 10L,
      call = rlang::caller_env(),
      page_size = 1L
    ),
    class = "scans_error_connect_traces"
  )
})

test_that("a failed later retained-job page aborts the read", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  jobs_response <- httr2::response_json(
    body = list(list(
      key = "job-1",
      start_time = "2026-01-01T00:00:00Z"
    ))
  )
  trace_response <- httr2::response(
    headers = list(`X-Total-Count` = "2"),
    body = charToRaw(otel_test_envelope("one"))
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      if (endsWith(sub("\\?.*$", "", request$url), "/jobs")) {
        return(jobs_response)
      }
      NULL
    },
    connect_perform_batch = function(requests, call, ...) list(trace_response)
  )

  expect_error(
    connect_job_trace_lines(
      client = list(server = "https://connect.example.com", api_key = "secret"),
      guid = "11111111-1111-4111-8111-111111111111",
      from = NULL,
      to = NULL,
      max_spans = 10L,
      call = rlang::caller_env(),
      page_size = 1L
    ),
    class = "scans_error_connect_traces"
  )
})

test_that("retained jobs are requested in bounded waves", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  batch_sizes <- integer()
  request_limits <- character()
  jobs <- lapply(5:1, function(index) {
    list(
      key = paste0("job-", index),
      start_time = sprintf("2026-01-%02dT00:00:00Z", index)
    )
  })
  jobs_response <- httr2::response_json(body = jobs)
  trace_response <- httr2::response(
    headers = list(`X-Total-Count` = "1"),
    body = charToRaw(otel_test_envelope(c("one", "two")))
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) jobs_response,
    connect_perform_batch = function(requests, call, ...) {
      batch_sizes <<- c(batch_sizes, length(requests))
      request_limits <<- c(
        request_limits,
        vapply(
          requests,
          \(request) httr2::url_parse(request$url)$query$limit,
          character(1)
        )
      )
      rep(list(trace_response), length(requests))
    }
  )

  lines <- connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 1L,
    call = rlang::caller_env(),
    page_size = 10L,
    wave_size = 2L
  )

  expect_identical(batch_sizes, 2L)
  expect_identical(request_limits, c("10", "10"))
  expect_true(attr(lines, "truncated", exact = TRUE))
})

test_that("an exact final retained page is not marked truncated", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  parses <- 0L
  progress <- character()
  original <- otel_parse_envelopes_each
  jobs_response <- httr2::response_json(
    body = list(list(
      key = "job-1",
      start_time = "2026-01-01T00:00:00Z"
    ))
  )
  trace_response <- httr2::response(
    headers = list(`X-Total-Count` = "1"),
    body = charToRaw(otel_test_envelope(c("one", "two")))
  )
  withr::local_options(list(scans.progress = function(message) {
    progress <<- c(progress, message)
  }))
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) {
      if (endsWith(sub("\\?.*$", "", request$url), "/jobs")) {
        jobs_response
      } else {
        trace_response
      }
    },
    otel_parse_envelopes_each = function(...) {
      parses <<- parses + 1L
      original(...)
    }
  )

  lines <- connect_job_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 2L,
    call = rlang::caller_env(),
    page_size = 10L
  )

  expect_null(attr(lines, "truncated", exact = TRUE))
  expect_identical(parses, 1L)
  expect_true(any(grepl("job-1 page 1", progress, fixed = TRUE)))
})

test_that("retained jobs must start before the upper window bound", {
  to <- as.POSIXct("2026-03-10", tz = "UTC")
  jobs <- list(
    list(key = "inside", start_time = "2026-03-09T23:59:59Z"),
    list(key = "edge", start_time = "2026-03-10T00:00:00Z"),
    list(key = "later", start_time = "2026-03-11T00:00:00Z"),
    list(key = "undated")
  )

  expect_identical(connect_job_keys(jobs, NULL, to), c("inside", "undated"))
})

test_that("malformed and attribute-less spans do not consume GenAI budget", {
  skip_if_not_installed("jsonlite")
  line <- jsonlite::toJSON(
    list(
      resourceSpans = list(list(
        scopeSpans = list(list(
          spans = list(list(spanId = "framework"))
        ))
      ))
    ),
    auto_unbox = TRUE
  )

  expect_identical(
    connect_trace_lines_summary(c(line, "malformed"))$span_count,
    0L
  )
})

test_that("framework spans are dropped after conversation grouping", {
  chat <- otel_chat_span()
  framework <- otel_test_span(
    "reactive",
    list("shiny.reactive" = "x"),
    parent = chat$span_id
  )

  groups <- otel_group_conversations(list(chat, framework))

  expect_identical(
    vapply(groups[[1L]], `[[`, character(1), "span_id"),
    chat$span_id
  )
})

test_that("OTel metadata keeps distinct values and resource users", {
  spans <- list(
    list(
      attributes = list(session = "a"),
      resource = list("enduser.id" = "ada", service = "one")
    ),
    list(
      attributes = list(session = "b"),
      resource = list(service = "two")
    )
  )

  expect_identical(otel_extra_attributes(spans)$session, c("a", "b"))
  expect_identical(otel_resource_attributes(spans)$service, c("one", "two"))
  expect_identical(otel_user_id(spans), "ada")
})

test_that("a failed later aggregate page keeps what was read and warns", {
  skip_if_not_installed("httr2")
  skip_if_not_installed("jsonlite")
  fallback_lines <- NULL
  aggregate_response <- httr2::response(
    headers = list(`X-Total-Count` = "2"),
    body = charToRaw(otel_test_envelope("first"))
  )
  testthat::local_mocked_bindings(
    connect_perform = function(request, call) aggregate_response,
    connect_perform_batch = function(requests, call, ...) list(NULL),
    connect_job_trace_lines = function(
      client,
      guid,
      from,
      to,
      max_spans,
      call,
      page_size,
      lines = character(),
      ...
    ) {
      fallback_lines <<- lines
      result <- "fallback"
      attr(result, "spans") <- list()
      result
    }
  )

  expect_warning(
    lines <- connect_trace_lines(
      client = list(server = "https://connect.example.com", api_key = "secret"),
      guid = "11111111-1111-4111-8111-111111111111",
      from = NULL,
      to = NULL,
      max_spans = 10L,
      call = rlang::caller_env(),
      page_size = 1L,
      wave_size = 1L
    ),
    "could not be read"
  )

  expect_identical(as.vector(lines), "fallback")
  # The page already read travels to the per-job fallback.
  expect_length(fallback_lines, 1L)
  expect_true(attr(fallback_lines, "truncated"))
  expect_error(
    connect_trace_lines(
      client = list(server = "https://connect.example.com", api_key = "secret"),
      guid = "11111111-1111-4111-8111-111111111111",
      from = NULL,
      to = NULL,
      max_spans = 10L,
      call = rlang::caller_env(),
      page_size = 1L,
      wave_size = 1L,
      jobs = FALSE
    ),
    class = "scans_error_connect_traces"
  )
})

test_that("framework-wrapper metadata survives GenAI span filtering", {
  skip_if_not_installed("jsonlite")
  parent <- otel_test_span(
    "root",
    list("enduser.id" = "ada", "shiny.session" = "abc")
  )
  parent$resource <- list("service.name" = "assistant")
  child <- otel_chat_span(
    parent = "root",
    input = list(list(role = "user", parts = text_part("Hello"))),
    output = list(list(role = "assistant", parts = text_part("Hi")))
  )

  groups <- otel_group_conversations_in_window(
    list(parent, child),
    from = NULL,
    to = NULL
  )
  bundle <- as_trajectory_otel(groups)
  metadata <- trajectory_info(bundle)$metadata[[1L]]$otel

  expect_identical(
    vapply(groups[[1L]], `[[`, character(1), "span_id"),
    child$span_id
  )
  expect_identical(metadata$user, "ada")
  expect_identical(metadata$attributes$shiny.session, "abc")
  expect_identical(metadata$resource$service.name, "assistant")
})

test_that("a missing X-Total-Count header pages until a short page", {
  skip_if_not_installed("httr2")
  expect_identical(connect_total_count(httr2::response()), NA_integer_)

  pages <- list(
    otel_test_envelope("a"),
    otel_test_envelope("b"),
    ""
  )
  served <- 0L
  serve <- function(request, ...) {
    served <<- served + 1L
    body <- if (served <= length(pages)) pages[[served]] else ""
    httr2::response(body = charToRaw(body))
  }
  testthat::local_mocked_bindings(
    connect_perform = serve,
    connect_perform_batch = function(requests, call, ...) {
      lapply(requests, serve)
    }
  )
  lines <- connect_trace_lines(
    client = list(server = "https://connect.example.com", api_key = "secret"),
    guid = "11111111-1111-4111-8111-111111111111",
    from = NULL,
    to = NULL,
    max_spans = 10L,
    call = rlang::caller_env(),
    page_size = 1L,
    wave_size = 1L,
    jobs = FALSE
  )
  expect_length(as.vector(lines), 2L)
  expect_false(isTRUE(attr(lines, "truncated")))
})

test_that("Date and string window bounds become UTC times", {
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
  read_connect_traces(
    guid,
    from = as.Date("2026-08-20"),
    to = "2026-08-27T10:30:00"
  )
  expect_identical(bounds$from, as.POSIXct("2026-08-20", tz = "UTC"))
  expect_identical(bounds$to, as.POSIXct("2026-08-27 10:30:00", tz = "UTC"))

  expect_error(
    read_connect_traces(guid, from = "yesterday"),
    class = "scans_error_connect_window"
  )
  expect_error(
    scans_app_connect(c(App = guid), to = 12345),
    class = "scans_error_connect_window"
  )
})
