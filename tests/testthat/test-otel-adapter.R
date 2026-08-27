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
    input = list(list(
      role = "assistant",
      parts = list(list(
        type = "tool_call",
        id = "call-9",
        name = "search"
      ))
    )),
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

  expect_equal(call$status, "failed")
  expect_equal(call$error, "simpleError")
  expect_equal(call$duration, 2.5)
  expect_equal(trajectory_info(bundle)$status, "failed")
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
