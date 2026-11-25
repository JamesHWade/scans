deputy_otel_fixture <- function() {
  test_path("fixtures", "deputy", "otel")
}

deputy_otel_bundle <- function(...) {
  as_trajectory_otel(read_otel_traces(deputy_otel_fixture(), ...))
}

trajectory_row <- function(bundle, id) {
  info <- trajectory_info(bundle)
  info[info$trajectory_id == id, ]
}

deputy_events <- function(bundle, id) {
  events <- trajectory_events(bundle)
  events[
    events$trajectory_id == id & startsWith(events$event_type, "deputy:"),
  ]
}

ending_fields <- c(
  "class",
  "reason",
  "by",
  "field",
  "actual",
  "limit",
  "wrapped_up",
  "phase",
  "finish_reason",
  "cut_off"
)

# A run span as deputy's agent-tracing.R writes it, with one model call.
deputy_run_spans <- function(
  run,
  attributes = list(),
  events = list(),
  parent = "",
  conversation = "conv-1"
) {
  span <- otel_test_span(
    paste0("span-", run),
    c(
      list(
        "deputy.run.id" = run,
        "deputy.agent.id" = paste0("agent-", run),
        "gen_ai.conversation.id" = conversation
      ),
      attributes
    ),
    parent = parent,
    start = 1e9,
    end = 5e9,
    name = "deputy.run"
  )
  span$events <- events
  chat <- otel_chat_span(
    span_id = paste0("chat-", run),
    conversation = conversation,
    parent = span$span_id,
    input = list(list(role = "user", parts = text_part(run))),
    output = list(list(role = "assistant", parts = text_part("ok")))
  )
  list(span, chat)
}

test_that("captured Deputy runs keep transcripts, endings, and events", {
  skip_if_not_installed("jsonlite")
  bundle <- deputy_otel_bundle()
  info <- trajectory_info(bundle)
  deputy <- info[startsWith(info$trajectory_id, "otel/c_"), ]

  for (index in seq_len(nrow(deputy))) {
    ending <- deputy$metadata[[index]]$ending
    expect_named(ending, ending_fields)
    expect_type(ending$actual, "double")
    expect_type(ending$wrapped_up, "logical")
    expect_type(ending$cut_off, "logical")
    expect_match(deputy$run_id[[index]], "^run_")
    expect_match(deputy$metadata[[index]]$session_id, "^session_")
    expect_match(deputy$metadata[[index]]$agent_id, "^agent_")
  }

  cutoff <- trajectory_row(bundle, "otel/c_cutoff")
  expect_identical(cutoff$status, "completed")
  expect_identical(cutoff$metadata[[1L]]$ending$class, "RunEndingCutOff")
  expect_identical(cutoff$metadata[[1L]]$ending$finish_reason, "max_tokens")
  expect_true(cutoff$metadata[[1L]]$ending$cut_off)

  review <- deputy_events(bundle, "otel/c_review")
  expect_identical(
    review$event_type,
    c("deputy:fallback", "deputy:compaction", "deputy:fallback", "deputy:stop")
  )
  expect_identical(review$metadata[[1L]]$fallback_index, 1L)
  expect_false(anyNA(review$timestamp))
  expect_identical(review$status, rep("completed", 4L))

  denied <- deputy_events(bundle, "otel/c_denied")
  permission <- denied[denied$event_type == "deputy:permission", ]
  expect_identical(permission$status, "failed")
  expect_identical(permission$call_id, "call_fixture")
  expect_identical(permission$metadata[[1L]]$decision, "deny")
  expect_null(permission$metadata[[1L]]$tool_call_id)

  # Deputy files the compaction summary under the run's session, so it is its
  # own conversation, without the run.
  summary <- info[startsWith(info$trajectory_id, "otel/session_"), ]
  expect_null(summary$metadata[[1L]]$ending)
  expect_identical(nrow(deputy_events(bundle, summary$trajectory_id)), 0L)
})

test_that("a delegated subagent gets its own trajectory linked to its lead", {
  skip_if_not_installed("jsonlite")
  bundle <- deputy_otel_bundle()
  info <- trajectory_info(bundle)
  lead <- trajectory_row(bundle, "otel/c_team")
  child <- info[startsWith(info$trajectory_id, "otel/c_team/"), ]
  turns <- trajectory_turns(bundle)
  events <- trajectory_events(bundle)
  text <- function(id) {
    events$text[events$trajectory_id == id & events$event_type == "content"]
  }

  expect_identical(nrow(child), 1L)
  expect_identical(
    child$trajectory_id,
    paste0("otel/c_team/", utils::URLencode(child$run_id, reserved = TRUE))
  )
  expect_identical(child$parent_trajectory_id, "otel/c_team")
  expect_identical(child$source_id, "c_team")
  expect_identical(child$metadata[[1L]]$parent_run_id, lead$run_id)
  expect_match(child$metadata[[1L]]$delegation_id, "^delegation_")
  expect_identical(
    utils::tail(text(child$trajectory_id), 1L),
    "There are 84 rows."
  )
  expect_identical(
    utils::tail(text("otel/c_team"), 1L),
    "The worker counted 84 rows."
  )
  expect_identical(lead$metadata[[1L]]$otel$model_calls, 2L)
  expect_identical(child$metadata[[1L]]$otel$model_calls, 1L)
  expect_false(any(trajectory_losses(bundle)$field == "parent_run_id"))

  measures <- measure_trajectories(bundle)
  tokens <- measures[measures$measure == "input_tokens", ]
  expect_identical(
    tokens$value[match(
      c("otel/c_team", child$trajectory_id),
      tokens$trajectory_id
    )],
    c(20, 10)
  )
  expect_identical(
    unique(tokens$coverage[tokens$trajectory_id %in% child$trajectory_id]),
    "complete"
  )

  # Without the lead's run in the read, the parent stays a source identifier.
  conversations <- read_otel_traces(deputy_otel_fixture())
  lead_run <- vapply(
    conversations$c_team,
    function(span) {
      otel_is_deputy_run(span) &&
        is.na(otel_attribute(span, "deputy.parent.run.id"))
    },
    logical(1)
  )
  partial <- as_trajectory_otel(list(c_team = conversations$c_team[!lead_run]))
  orphan <- trajectory_info(partial)
  orphan <- orphan[orphan$trajectory_id != "otel/c_team", ]
  expect_true(is.na(orphan$parent_trajectory_id))
  expect_identical(
    trajectory_losses(partial)$field[
      trajectory_losses(partial)$trajectory_id == orphan$trajectory_id
    ],
    "parent_run_id"
  )
})

test_that("Deputy runs do not consume the span budget", {
  skip_if_not_installed("jsonlite")
  conversations <- read_otel_traces(deputy_otel_fixture())
  budget <- attr(conversations, "read_info")$spans
  spans <- unlist(conversations, recursive = FALSE)
  expect_identical(
    sum(vapply(spans, otel_is_deputy_run, logical(1))),
    5L
  )
  expect_false(any(
    vapply(spans, otel_is_budget_span, logical(1)) &
      vapply(spans, otel_is_deputy_run, logical(1))
  ))

  exact <- read_otel_traces(deputy_otel_fixture(), max_spans = budget)
  expect_false(attr(exact, "read_info")$truncated)
  expect_identical(exact, conversations, ignore_attr = TRUE)
})

test_that("an ending's class is set only when the span determines it", {
  skip_if_not_installed("jsonlite")
  ending <- function(stop_reason, ...) {
    spans <- deputy_run_spans(
      "run-1",
      list("deputy.stop_reason" = stop_reason, ...)
    )
    info <- trajectory_info(as_trajectory_otel(spans))
    list(status = info$status, ending = info$metadata[[1L]]$ending)
  }

  answered <- ending("complete", "deputy.finish_reason" = "stop")
  expect_identical(answered$ending$class, "RunEndingAnswered")
  expect_identical(answered$status, "completed")
  stopped <- ending("interrupted")
  expect_identical(stopped$ending$class, "RunEndingStopped")
  expect_identical(stopped$ending$reason, "interrupted")
  expect_identical(stopped$ending$by, NA_character_)
  expect_identical(stopped$status, "interrupted")
  # A caller may stop a run with a limit's or failure's own word.
  limit <- ending("request_limit")
  expect_identical(limit$ending$class, NA_character_)
  expect_identical(limit$ending$reason, "request_limit")
  expect_identical(limit$status, "interrupted")
  failed <- ending("provider_error")
  expect_identical(failed$ending$class, NA_character_)
  expect_identical(failed$status, "failed")
  custom <- ending("custom")
  expect_identical(custom$ending$reason, NA_character_)
  expect_identical(custom$status, "interrupted")
})

test_that("event statuses follow the Deputy adapter", {
  skip_if_not_installed("jsonlite")
  event <- function(name, ...) {
    list(name = name, time = "2000000000", attributes = list(...))
  }
  spans <- deputy_run_spans(
    "run-1",
    list("deputy.stop_reason" = "complete"),
    events = list(
      event("deputy.structured_attempt", attempt = "1", valid = FALSE),
      event("deputy.structured_attempt", attempt = "2", valid = TRUE),
      event("deputy.permission", tool_call_id = "call-1", decision = "allow"),
      event("exception")
    )
  )
  events <- deputy_events(as_trajectory_otel(spans), "otel/conv-1")

  expect_identical(
    events$event_type,
    c(
      "deputy:structured_attempt",
      "deputy:structured_attempt",
      "deputy:permission"
    )
  )
  expect_identical(events$status, c("failed", "completed", "completed"))
  expect_identical(events$metadata[[1L]], list(attempt = 1L, valid = FALSE))
})

test_that("spans no run claims stay with the lead and are reported", {
  skip_if_not_installed("jsonlite")
  lead <- deputy_run_spans("lead", list("deputy.stop_reason" = "complete"))
  child <- deputy_run_spans(
    "child",
    list(
      "deputy.stop_reason" = "complete",
      "deputy.parent.run.id" = "lead"
    ),
    parent = "chat-lead"
  )
  stray <- otel_chat_span(span_id = "stray", conversation = "conv-1")
  bundle <- as_trajectory_otel(c(lead, child, list(stray)))
  losses <- trajectory_losses(bundle)

  expect_identical(
    trajectory_info(bundle)$trajectory_id,
    c("otel/conv-1", "otel/conv-1/child")
  )
  expect_identical(losses$trajectory_id, "otel/conv-1")
  expect_identical(losses$field, "deputy.run")
  expect_identical(losses$metadata[[1L]], list(spans = 1L))
  expect_error(
    as_trajectory_otel(c(lead, child), trajectory_id = "one"),
    class = "scans_error_otel_source"
  )
})

test_that("conversations without a Deputy run take no Deputy path", {
  skip_if_not_installed("jsonlite")
  sources <- list(
    jsonlite::read_json(
      system.file("extdata", "support-investigation.json", package = "scans")
    ),
    read_otel_traces(test_path("fixtures", "commons", "otel", "r"))
  )
  for (source in sources) {
    actual <- as_trajectory_otel(source)
    local_mocked_bindings(
      otel_deputy_assign_runs = function(spans, index) spans,
      otel_deputy_parts = function(spans) list(list(spans = spans))
    )
    expect_identical(as_trajectory_otel(source), actual)
  }
})

test_that("run assignment survives ancestry kept only for context", {
  skip_if_not_installed("jsonlite")
  lead <- deputy_run_spans("lead", list("deputy.stop_reason" = "complete"))
  child <- deputy_run_spans(
    "child",
    list(
      "deputy.stop_reason" = "complete",
      "deputy.parent.run.id" = "lead"
    ),
    parent = "chat-lead"
  )
  # The subagent's chat sits under an agent span that falls past the budget.
  invoke <- otel_test_span(
    "invoke-child",
    list("gen_ai.operation.name" = "invoke_agent"),
    parent = child[[1L]]$span_id,
    name = "invoke_agent"
  )
  child[[2L]]$parent_span_id <- invoke$span_id
  spans <- otel_limit_spans(
    list(child[[2L]], lead[[2L]], invoke, lead[[1L]], child[[1L]]),
    max_spans = 2L
  )
  expect_true(isTRUE(attr(spans[[3L]], "otel_context_only")))

  bundle <- as_trajectory_otel(otel_group_conversations(spans))
  events <- trajectory_events(bundle)
  expect_identical(
    events$text[events$trajectory_id == "otel/conv-1/child"],
    c("child", "ok")
  )
  expect_identical(
    events$text[events$trajectory_id == "otel/conv-1"],
    c("lead", "ok")
  )
  expect_identical(nrow(trajectory_losses(bundle)), 0L)
})

test_that("a subagent read without its lead adds no lead trajectory", {
  skip_if_not_installed("jsonlite")
  child <- deputy_run_spans(
    "child",
    list(
      "deputy.stop_reason" = "complete",
      "deputy.parent.run.id" = "lead"
    )
  )
  info <- trajectory_info(as_trajectory_otel(child))

  expect_identical(info$trajectory_id, "otel/conv-1/child")
  expect_identical(info$run_id, "child")
  expect_identical(
    trajectory_info(as_trajectory_otel(child, trajectory_id = "one"))$run_id,
    "child"
  )
})
