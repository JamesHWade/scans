# A turn recorded without a conversation id is its own trace. These spans
# model a Shiny app: each turn's chat span sits under a task span carrying the
# Shiny session id, as Shiny's OpenTelemetry spans do.
stitch_turn <- function(
  trace,
  history,
  conversation = NULL,
  session = "session-1",
  start = 1
) {
  task <- otel_test_span(
    "task",
    c(
      list(label = "task"),
      if (!is.null(session)) list("session.id" = session)
    ),
    trace = trace,
    start = start * 1e9,
    end = (start + 1) * 1e9,
    name = "ExtendedTask agent:<anonymous>"
  )
  chat <- otel_chat_span(
    span_id = "chat",
    conversation = conversation,
    parent = "task",
    input = history,
    output = list(list(role = "assistant", parts = text_part("ok")))
  )
  chat$trace_id <- trace
  chat$start_time <- as.character(start * 1e9)
  chat$end_time <- as.character((start + 1) * 1e9)
  list(task, chat)
}

user_says <- function(text) list(role = "user", parts = text_part(text))
model_says <- function(text) list(role = "assistant", parts = text_part(text))

first_history <- list(user_says("Draft a report"))
second_history <- list(
  user_says("Draft a report"),
  model_says("ok"),
  user_says("Shorter, please")
)

test_that("a first turn without an id joins the conversation it continues", {
  skip_if_not_installed("jsonlite")
  spans <- c(
    stitch_turn("trace-a", first_history, start = 1),
    stitch_turn("trace-b", second_history, conversation = "conv-1", start = 5)
  )
  groups <- otel_group_conversations(spans)
  expect_named(groups, "conv-1")
  expect_identical(attr(groups[[1]], "otel_context")$stitched, "trace-a")

  info <- trajectory_info(as_trajectory_otel(spans))
  expect_identical(info$metadata[[1]]$otel$stitched, "trace-a")
  expect_identical(info$metadata[[1]]$otel$model_calls, 2L)
  expect_equal(
    info$started_at,
    as.POSIXct(1, origin = "1970-01-01", tz = "UTC")
  )
})

test_that("a turn is only joined within its own Shiny session", {
  skip_if_not_installed("jsonlite")
  spans <- c(
    stitch_turn("trace-a", first_history, start = 1),
    stitch_turn(
      "trace-b",
      second_history,
      conversation = "conv-1",
      session = "session-2",
      start = 5
    )
  )
  groups <- otel_group_conversations(spans)
  expect_setequal(names(groups), c("trace-a", "conv-1"))
  expect_null(attr(groups[["conv-1"]], "otel_context")$stitched)

  # Without a recorded session there is nothing to match on.
  spans <- c(
    stitch_turn("trace-a", first_history, session = NULL, start = 1),
    stitch_turn(
      "trace-b",
      second_history,
      conversation = "conv-1",
      session = NULL,
      start = 5
    )
  )
  expect_setequal(
    names(otel_group_conversations(spans)),
    c("trace-a", "conv-1")
  )
})

test_that("a turn is not joined to a conversation that does not continue it", {
  skip_if_not_installed("jsonlite")
  # A new conversation opening with the same message, answered differently.
  other <- list(
    user_says("Draft a report"),
    model_says("Which template?"),
    user_says("The CRI one")
  )
  spans <- c(
    stitch_turn("trace-a", first_history, start = 1),
    stitch_turn("trace-b", other, conversation = "conv-1", start = 5)
  )
  expect_setequal(
    names(otel_group_conversations(spans)),
    c("trace-a", "conv-1")
  )

  # Nor to one that started before it.
  spans <- c(
    stitch_turn("trace-a", first_history, start = 5),
    stitch_turn("trace-b", second_history, conversation = "conv-1", start = 1)
  )
  expect_setequal(
    names(otel_group_conversations(spans)),
    c("trace-a", "conv-1")
  )
})

test_that("turns recorded without any id chain into one conversation", {
  skip_if_not_installed("jsonlite")
  third_history <- c(
    second_history,
    list(model_says("ok"), user_says("Add a table"))
  )
  spans <- c(
    stitch_turn("trace-a", first_history, start = 1),
    stitch_turn("trace-b", second_history, start = 5),
    stitch_turn("trace-c", third_history, start = 9)
  )
  groups <- otel_group_conversations(spans)
  expect_named(groups, "trace-c")
  expect_identical(
    attr(groups[[1]], "otel_context")$stitched,
    c("trace-a", "trace-b")
  )

  # The whole history is one trajectory, three user turns long.
  turns <- trajectory_turns(as_trajectory_otel(spans))
  expect_identical(sum(turns$role == "user"), 3L)
})
