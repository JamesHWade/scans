commons_otel_fixture <- function(writer) {
  test_path("fixtures", "commons", "otel", writer)
}

# One commons turn in its own trace: the turn wrapper, a chat span under it,
# and, when `tag` or `candidates` is given, a provenance span.
commons_otel_turn <- function(
  turn,
  input,
  output,
  start,
  tag = NULL,
  candidates = NULL,
  user = NULL
) {
  trace <- paste0("trace-", turn)
  wrapper <- otel_test_span(
    paste0("turn-", turn),
    if (is.null(user)) {
      stats::setNames(list(), character())
    } else {
      list("commons.connect.user" = user)
    },
    trace = trace,
    start = start * 1e9,
    end = (start + 3) * 1e9,
    name = "commons_conversation_turn"
  )
  chat <- otel_chat_span(
    span_id = paste0("chat-", turn),
    parent = wrapper$span_id,
    input = input,
    output = output
  )
  chat$trace_id <- trace
  chat$start_time <- format(start * 1e9 + 1e8, scientific = FALSE)
  chat$end_time <- format((start + 2) * 1e9, scientific = FALSE)
  spans <- list(wrapper, chat)
  if (!is.null(tag) || !is.null(candidates)) {
    attributes <- list(
      "commons.citation.candidates" = candidates %||% "[]"
    )
    if (!is.null(tag)) {
      attributes[["commons.provenance.tag"]] <- tag
    }
    spans[[3L]] <- otel_test_span(
      paste0("provenance-", turn),
      attributes,
      parent = wrapper$span_id,
      trace = trace,
      start = (start + 2.5) * 1e9,
      end = (start + 2.5) * 1e9,
      name = "commons_provenance"
    )
  }
  spans
}

user_message <- function(text) list(role = "user", parts = text_part(text))
reply_message <- function(text) {
  list(role = "assistant", parts = text_part(text))
}

provenance_events <- function(bundle) {
  events <- trajectory_events(bundle)
  events[events$event_type == "commons:provenance", ]
}

test_that("shared commons traces keep provenance, timing, and transcripts", {
  skip_if_not_installed("jsonlite")
  for (writer in c("r", "python")) {
    path <- commons_otel_fixture(writer)
    expected <- jsonlite::read_json(file.path(path, "expected.json"))
    conversations <- read_otel_traces(path)
    bundle <- as_trajectory_otel(conversations)
    info <- trajectory_info(bundle)
    expect_length(conversations, length(expected$conversations))

    for (index in seq_along(expected$conversations)) {
      want <- expected$conversations[[index]]
      id <- info$trajectory_id[[index]]
      turns <- trajectory_turns(bundle)
      # commons reads tool results as user turns; OTel keeps the tool role.
      roles <- turns$role[turns$trajectory_id == id]
      roles[roles == "tool"] <- "user"
      expect_identical(roles, unlist(want$roles))
      provenance <- provenance_events(bundle)
      provenance <- provenance[provenance$trajectory_id == id, ]
      expect_identical(
        vapply(provenance$value, `[[`, character(1), "provenance_tag"),
        vapply(
          want$exchanges,
          function(x) x$provenance_tag %||% NA_character_,
          character(1)
        )
      )
      expect_identical(
        lapply(provenance$value, function(value) {
          vapply(value$citation_decisions, `[[`, character(1), "status")
        }),
        lapply(want$exchanges, function(x) {
          vapply(x$decision_statuses, as.character, character(1))
        })
      )
      expect_identical(
        vapply(provenance$metadata, `[[`, integer(1), "exchange_index"),
        seq_along(want$exchanges)
      )
      expect_false(anyNA(provenance$timestamp))
    }

    accepted <- provenance_events(bundle)$value[[1L]]$citation_decisions[[1L]]
    expect_identical(
      accepted,
      list(
        quote = "Orders are one row per placed order.",
        status = "accepted",
        label = "documentation",
        kind = "prose"
      )
    )

    # Companion spans add provenance without changing a resource measure.
    spans <- otel_parse_otlp_lines(
      unlist(lapply(
        list.files(path, "[.]jsonl$", full.names = TRUE),
        readLines
      ))
    )
    from_spans <- as_trajectory_otel(spans)
    without <- as_trajectory_otel(Filter(Negate(otel_is_companion_span), spans))
    expect_identical(
      measure_trajectories(from_spans),
      measure_trajectories(without)
    )
    expect_identical(
      measure_trajectories(bundle),
      measure_trajectories(without)
    )
    measures <- measure_trajectories(bundle)
    first <- measures[measures$trajectory_id == info$trajectory_id[[1L]], ]
    expect_identical(
      first$coverage[match(
        c("elapsed", "model_duration", "tool_duration", "recorded_work"),
        first$measure
      )],
      rep("complete", 4L)
    )
    expect_identical(
      trajectory_info(from_spans)$metadata[[1L]]$otel$model_calls,
      3L
    )
    expect_identical(nrow(provenance_events(without)), 0L)
  }
})

test_that("OTel provenance matches the commons adapter on the same trace", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("commons")
  skip_if_not_installed("ellmer")
  for (writer in c("r", "python")) {
    path <- commons_otel_fixture(writer)
    otel <- as_trajectory_otel(read_otel_traces(path))
    commons <- as_trajectory_commons(commons::trajectory_read(path))
    columns <- c("event_type", "name", "turn_id", "value", "metadata")
    expect_identical(
      provenance_events(otel)[columns],
      provenance_events(commons)[columns]
    )
    expect_identical(
      trajectory_turns(otel)$round_index,
      trajectory_turns(commons)$round_index
    )
  }
})

test_that("setup spans stay out of conversations and the span budget", {
  skip_if_not_installed("jsonlite")
  conversations <- read_otel_traces(commons_otel_fixture("r"))
  setup <- c(
    "commons_agent_create",
    "commons_data_source_create",
    "commons_data_source_load_frames"
  )
  names <- unlist(lapply(conversations, function(spans) {
    vapply(spans, `[[`, character(1), "name")
  }))
  expect_false(any(names %in% setup))
  expect_identical(sum(names == "commons_provenance"), 3L)
  expect_identical(attr(conversations, "read_info")$spans, 8L)
})

test_that("companion spans do not consume the span budget", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  first <- commons_otel_turn(
    "1",
    list(user_message("Q1")),
    list(reply_message("A1")),
    start = 100,
    tag = "A"
  )
  second <- commons_otel_turn(
    "2",
    list(user_message("Q2")),
    list(reply_message("A2")),
    start = 200,
    tag = "B"
  )
  second[[2L]]$attributes[["gen_ai.conversation.id"]] <- "conv-2"
  writeLines(
    c(otel_test_otlp_line(first), otel_test_otlp_line(second)),
    file.path(dir, "trace-0.jsonl")
  )

  conversations <- read_otel_traces(dir, max_spans = 2L)
  info <- attr(conversations, "read_info", exact = TRUE)
  expect_false(info$truncated)
  expect_identical(info$spans, 2L)
  expect_identical(
    vapply(
      provenance_events(as_trajectory_otel(conversations))$value,
      `[[`,
      character(1),
      "provenance_tag"
    ),
    c("A", "B")
  )

  # At the ceiling the older conversation's model call is cut; its companion
  # spans alone do not make a conversation.
  expect_warning(
    kept <- read_otel_traces(dir, max_spans = 1L),
    "most recent 1 span"
  )
  expect_named(kept, "conv-2")
  expect_identical(
    as_trajectory_otel(first[-2L])@trajectories,
    as_trajectory_otel(list())@trajectories
  )
})

test_that("the commons user and full citation candidates are kept", {
  skip_if_not_installed("jsonlite")
  long <- strrep("Revenue excludes tax and refunds. ", 20L)
  candidates <- jsonlite::toJSON(
    list(list(quote = long, status = "accepted", label = "Dictionary")),
    auto_unbox = TRUE
  )
  spans <- commons_otel_turn(
    "1",
    list(user_message("What is revenue?")),
    list(reply_message("Revenue excludes tax.")),
    start = 100,
    tag = "B",
    candidates = as.character(candidates),
    user = "ada"
  )
  bundle <- as_trajectory_otel(spans)
  otel <- trajectory_info(bundle)$metadata[[1L]]$otel
  provenance <- provenance_events(bundle)

  expect_identical(otel$user, "ada")
  expect_null(otel$attributes[["commons.citation.candidates"]])
  expect_identical(
    provenance$value[[1L]]$citation_decisions[[1L]]$quote,
    long
  )
  expect_identical(
    provenance$timestamp,
    as.POSIXct(102.5, origin = "1970-01-01", tz = "UTC")
  )
  expect_identical(nrow(trajectory_losses(bundle)), 0L)

  oversized <- spans
  oversized[[3L]]$attributes[["commons.citation.candidates"]] <-
    as.character(jsonlite::toJSON(
      list(list(quote = strrep("x", 70000L), status = "accepted")),
      auto_unbox = TRUE
    ))
  losses <- trajectory_losses(as_trajectory_otel(oversized))
  expect_identical(losses$reason, "truncated")
  expect_match(losses$field, "^provenance\\[\\[1\\]\\]")

  malformed <- spans
  malformed[[3L]]$attributes[["commons.citation.candidates"]] <- "[{"
  expect_identical(
    trajectory_losses(as_trajectory_otel(malformed))$field,
    "provenance[[1]]$citation_decisions"
  )
})

test_that("conflicting records leave an exchange unclassified", {
  skip_if_not_installed("jsonlite")
  q1 <- user_message("Q1")
  a1 <- reply_message("A1")
  first <- commons_otel_turn("1", list(q1), list(a1), start = 100, tag = "A")
  # The same exchange recorded again, as a restored session replays it, with
  # a different record.
  replay <- commons_otel_turn("2", list(q1), list(a1), start = 200, tag = "C")
  latest <- commons_otel_turn(
    "3",
    list(q1, a1, user_message("Q2")),
    list(reply_message("A2")),
    start = 300,
    tag = "B"
  )
  bundle <- as_trajectory_otel(c(first, replay, latest))
  provenance <- provenance_events(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(
    vapply(provenance$value, `[[`, character(1), "provenance_tag"),
    c(NA, "B")
  )
  expect_true(is.na(provenance$timestamp[[1L]]))
  expect_setequal(
    losses$field,
    c("provenance[[1]]", "provenance[[1]]$provenance_tag")
  )

  # An edited first question no longer matches, so its record is not reused.
  edited <- commons_otel_turn(
    "4",
    list(user_message("Q1, edited")),
    list(reply_message("A1'")),
    start = 400,
    tag = "B"
  )
  bundle <- as_trajectory_otel(c(first, edited))
  expect_identical(provenance_events(bundle)$value[[1L]]$provenance_tag, "B")
  expect_identical(nrow(trajectory_losses(bundle)), 0L)
})

test_that("internal reminder turns do not open an exchange", {
  skip_if_not_installed("jsonlite")
  reminder <- user_message(
    "<system-instruction>Cite sources.</system-instruction>"
  )
  spans <- commons_otel_turn(
    "1",
    list(user_message("Q1"), reply_message("Draft"), reminder),
    list(reply_message("A1")),
    start = 100,
    tag = "A"
  )
  bundle <- as_trajectory_otel(spans)
  turns <- trajectory_turns(bundle)

  expect_identical(turns$round_index, rep(1L, 4L))
  expect_identical(turns$metadata[[3L]], list(commons_internal = TRUE))
  expect_identical(provenance_events(bundle)$value[[1L]]$provenance_tag, "A")
})

test_that("spans without timestamps do not abort record selection", {
  skip_if_not_installed("jsonlite")
  untimed <- function(span) {
    span$start_time <- NA_character_
    span$end_time <- NA_character_
    span
  }
  spans <- commons_otel_turn(
    "1",
    list(user_message("Q1")),
    list(reply_message("A1")),
    start = 100,
    tag = "A"
  )
  # A second model call and a second record in the same turn, neither with a
  # usable time: the timed ones stay the latest wherever the others arrive.
  call <- spans[[2L]]
  call$span_id <- "chat-untimed"
  call$attributes[["gen_ai.output.messages"]] <- NULL
  record <- spans[[3L]]
  record$span_id <- "provenance-untimed"
  record$attributes[["commons.provenance.tag"]] <- "C"
  extra <- list(untimed(call), untimed(record))

  for (ordered in list(c(spans, extra), c(spans[1L], extra, spans[-1L]))) {
    provenance <- provenance_events(as_trajectory_otel(ordered))
    expect_identical(provenance$value[[1L]]$provenance_tag, "A")
    expect_false(is.na(provenance$timestamp))
  }
})

test_that("a turn reached through unexposed ancestors keeps its record", {
  skip_if_not_installed("jsonlite")
  # The budget cuts through the first turn: its `invoke_agent` span is kept
  # only as ancestry of the surviving model call.
  expect_warning(
    conversations <- read_otel_traces(commons_otel_fixture("r"), max_spans = 5L)
  )
  first <- conversations[[1L]]
  expect_identical(
    sum(vapply(first, `[[`, character(1), "name") == "invoke_agent"),
    1L
  )
  bundle <- as_trajectory_otel(conversations)
  record <- provenance_events(bundle)$value[[1L]]
  expect_identical(record$provenance_tag, "B")
  expect_identical(record$citation_decisions[[1L]]$status, "accepted")
  expect_identical(trajectory_info(bundle)$metadata[[1L]]$otel$model_calls, 2L)

  # A framework span between the turn and its model call is not exposed
  # either.
  spans <- commons_otel_turn(
    "1",
    list(user_message("Q1")),
    list(reply_message("A1")),
    start = 100,
    tag = "A"
  )
  spans[[2L]]$parent_span_id <- "reactive"
  wrapper <- otel_test_span(
    "reactive",
    stats::setNames(list(), character()),
    parent = spans[[1L]]$span_id,
    trace = "trace-1",
    name = "reactive"
  )
  bundle <- as_trajectory_otel(c(spans, list(wrapper)))
  expect_identical(provenance_events(bundle)$value[[1L]]$provenance_tag, "A")
})
