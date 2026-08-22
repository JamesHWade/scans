test_that("completed Deputy results become canonical trajectories", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  bundle <- as_trajectory_deputy(deputy_result_fixture())
  info <- trajectory_info(bundle)
  events <- trajectory_events(bundle)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(info$source_type, "deputy")
  expect_identical(info$run_id, "run-001")
  expect_identical(info$source_id, "run-001")
  expect_identical(info$agent, "researcher")
  expect_identical(info$status, "completed")
  expect_identical(
    info$started_at,
    as.POSIXct("2026-08-22 17:00:00", tz = "UTC")
  )
  expect_identical(
    info$completed_at,
    as.POSIXct("2026-08-22 17:00:02", tz = "UTC")
  )
  expect_identical(info$metadata[[1L]]$session_id, "session-001")
  expect_identical(
    info$metadata[[1L]]$run_context$research_run_id,
    "research-run-001"
  )
  expect_identical(trajectory_turns(bundle)$role, c("user", "assistant"))
  expect_identical(
    events$event_type[grepl("^deputy:", events$event_type)],
    c("deputy:start", "deputy:stop")
  )
})

test_that("as_trajectory dispatches Deputy results", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  bundle <- as_trajectory(deputy_result_fixture())

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(trajectory_info(bundle)$source_type, "deputy")
})

test_that("as_trajectory does not claim unrelated AgentResult classes", {
  skip_if_not_installed("deputy", "0.0.0.9000")

  fake_result <- new.env(parent = emptyenv())
  fake_result$n_turns <- length
  fake_result$is_success <- function() TRUE
  class(fake_result) <- c("AgentResult", "R6")

  condition <- rlang::catch_cnd(as_trajectory(fake_result))

  expect_s3_class(condition, "scans_error_unsupported_source")
})

test_that("Deputy adapter arguments use source-specific conditions", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  condition <- rlang::catch_cnd(
    as_trajectory_deputy(deputy_result_fixture(), metadata = list("unnamed"))
  )

  expect_s3_class(condition, "scans_error_deputy_argument")
})

test_that("Deputy tool events preserve lifecycle correlation and failures", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  started_at <- as.POSIXct("2026-08-22 17:00:00", tz = "UTC")
  events <- list(
    deputy::AgentEvent("start", task = "Search"),
    deputy::AgentEvent(
      "tool_start",
      tool_call_id = "call-001",
      tool_name = "search",
      tool_input = list(query = "scans", api_key = "secret")
    ),
    deputy::AgentEvent(
      "tool_end",
      tool_call_id = "call-001",
      tool_name = "search",
      tool_result = list(matches = 0L),
      tool_error = simpleError("Search failed"),
      duration = 0.5
    ),
    deputy::AgentEvent("stop", reason = "complete")
  )
  for (index in seq_along(events)) {
    events[[index]]$timestamp <- started_at + index - 1L
  }

  bundle <- as_trajectory_deputy(deputy_result_fixture(events))
  deputy_events <- trajectory_events(bundle)
  deputy_events <- deputy_events[
    grepl("^deputy:", deputy_events$event_type),
    ,
    drop = FALSE
  ]
  losses <- trajectory_losses(bundle)

  expect_identical(
    deputy_events$event_type,
    c(
      "deputy:start",
      "deputy:tool_start",
      "deputy:tool_end",
      "deputy:stop"
    )
  )
  expect_identical(deputy_events$call_id[2:3], rep("call-001", 2L))
  expect_identical(deputy_events$name[2:3], rep("search", 2L))
  expect_identical(
    deputy_events$value[[2L]],
    list(query = "scans", api_key = "<redacted>")
  )
  expect_identical(deputy_events$value[[3L]], list(matches = 0L))
  expect_identical(deputy_events$duration[[3L]], 0.5)
  expect_identical(deputy_events$status[[3L]], "failed")
  expect_identical(deputy_events$error[[3L]], "Search failed")
  expect_true(any(losses$field == "events[[2]]$value$api_key"))
})

test_that("Deputy lifecycle event kinds retain their source order", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  events <- list(
    deputy::AgentEvent("start", task = "Research"),
    deputy::AgentEvent("text", text = "Working", is_complete = FALSE),
    deputy::AgentEvent("text_complete", text = "Done"),
    deputy::AgentEvent(
      "turn",
      turn = ellmer::AssistantTurn(list(ellmer::ContentText("Done"))),
      turn_number = 1L
    ),
    deputy::AgentEvent("warning", message = "Weak evidence"),
    deputy::AgentEvent(
      "content",
      content = list(citation = "source-001"),
      content_type = "citation"
    ),
    deputy::AgentEvent(
      "file_checkpoint",
      checkpoint_id = "checkpoint-001",
      name = "before-tool"
    ),
    deputy::AgentEvent(
      "usage",
      usage = deputy::AgentUsage(requests = 1L),
      limits = list(max_requests = 10L)
    ),
    deputy::AgentEvent("stop", reason = "complete")
  )

  bundle <- as_trajectory_deputy(deputy_result_fixture(events))
  event_types <- trajectory_events(bundle)$event_type
  event_types <- event_types[grepl("^deputy:", event_types)]

  expect_identical(
    event_types,
    paste0(
      "deputy:",
      c(
        "start",
        "text",
        "text_complete",
        "turn",
        "warning",
        "content",
        "file_checkpoint",
        "usage",
        "stop"
      )
    )
  )
})

test_that("unknown Deputy events retain safe properties and explicit losses", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  event <- deputy::AgentEvent(
    "provider_ping",
    provider = "fixture",
    live = new.env(parent = emptyenv())
  )
  bundle <- as_trajectory_deputy(deputy_result_fixture(list(event)))
  deputy_event <- trajectory_events(bundle)
  deputy_event <- deputy_event[
    deputy_event$event_type == "deputy:provider_ping",
    ,
    drop = FALSE
  ]
  losses <- trajectory_losses(bundle)

  expect_identical(deputy_event$metadata[[1L]]$provider, "fixture")
  expect_identical(deputy_event$metadata[[1L]]$live, "<unsupported>")
  expect_true(any(losses$field == "events[[1]]$metadata$live"))
})

test_that("unresolved Deputy parent runs remain explicit adapter losses", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  result <- deputy_result_fixture()
  result$parent_agent_id <- "agent-parent"
  result$parent_run_id <- "run-parent"
  result$delegation_id <- "delegation-001"

  bundle <- as_trajectory_deputy(result)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_true(is.na(info$parent_trajectory_id))
  expect_identical(info$metadata[[1L]]$parent_run_id, "run-parent")
  expect_identical(info$metadata[[1L]]$delegation_id, "delegation-001")
  expect_true(any(losses$field == "parent_run_id"))
  expect_true(any(grepl("parent trajectory", losses$detail, fixed = TRUE)))
})

test_that("Deputy result collections resolve included delegation parents", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  parent <- deputy_result_fixture()
  child <- deputy_result_fixture()
  child$run_id <- "run-child"
  child$agent_id <- "agent-child"
  child$agent_name <- "writer"
  child$parent_agent_id <- "agent-001"
  child$parent_run_id <- "run-001"
  child$delegation_id <- "delegation-001"

  bundle <- as_trajectory_deputy(list(parent, child))
  generic <- as_trajectory(list(parent, child))
  info <- trajectory_info(bundle)
  child_row <- match("run-child", info$run_id)

  expect_identical(S7::props(generic), S7::props(bundle))
  expect_identical(nrow(info), 2L)
  expect_identical(
    info$parent_trajectory_id[[child_row]],
    "deputy/run-001"
  )
  expect_false(any(trajectory_losses(bundle)$field == "parent_run_id"))
})

test_that("missing Deputy run identity is explicit and deterministic", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  result <- deputy_result_fixture()
  result$run_id <- NULL

  bundle <- as_trajectory_deputy(result)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(info$trajectory_id, "trajectory-000001")
  expect_true(is.na(info$run_id))
  expect_true(is.na(info$source_id))
  expect_true(any(losses$field == "run_id"))
})

test_that("multiple missing Deputy run IDs get distinct fallback identities", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  first <- deputy_result_fixture()
  first$run_id <- NULL
  second <- deputy_result_fixture()
  second$run_id <- NULL

  bundle <- as_trajectory_deputy(list(first, second))

  expect_identical(
    trajectory_info(bundle)$trajectory_id,
    c("trajectory-000001", "trajectory-000002")
  )
  expect_identical(
    sum(trajectory_losses(bundle)$field == "run_id"),
    2L
  )
})

test_that("Deputy source errors identify malformed public snapshots", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  invalid_turns <- deputy_result_fixture()
  invalid_turns$turns <- list("not a turn")
  condition <- rlang::catch_cnd(as_trajectory_deputy(invalid_turns))
  expect_s3_class(condition, "scans_error_deputy_source")
  expect_snapshot(error = TRUE, as_trajectory_deputy(invalid_turns))

  invalid_events <- deputy_result_fixture()
  invalid_events$events <- list(list(type = "start", timestamp = Sys.time()))
  expect_snapshot(error = TRUE, as_trajectory_deputy(invalid_events))
})

test_that("unsupported Deputy event text is represented as an adapter loss", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  event <- deputy::AgentEvent("text", text = list(value = "not scalar"))
  bundle <- as_trajectory_deputy(deputy_result_fixture(list(event)))
  deputy_event <- trajectory_events(bundle)
  deputy_event <- deputy_event[deputy_event$event_type == "deputy:text", ]
  losses <- trajectory_losses(bundle)

  expect_identical(deputy_event$text, "<unsupported>")
  expect_true(any(losses$field == "events[[1]]$text"))
  expect_identical(
    losses$event_id[losses$field == "events[[1]]$text"],
    deputy_event$event_id
  )
})

test_that("oversized Deputy tool errors are truncated safely", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  event <- deputy::AgentEvent(
    "tool_end",
    tool_call_id = "call-001",
    tool_name = "search",
    tool_error = simpleError(strrep("x", 70000L))
  )
  bundle <- as_trajectory_deputy(deputy_result_fixture(list(event)))
  deputy_event <- trajectory_events(bundle)
  deputy_event <- deputy_event[deputy_event$event_type == "deputy:tool_end", ]
  losses <- trajectory_losses(bundle)

  expect_lte(nchar(deputy_event$error, type = "bytes"), 65536L)
  expect_true(any(losses$field == "events[[1]]$error"))
  expect_true(any(losses$reason == "truncated"))
})

test_that("Deputy conversion is deterministic and contains no live source", {
  skip_if_not_installed("deputy", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  result <- deputy_result_fixture()
  first <- as_trajectory_deputy(result)
  second <- as_trajectory_deputy(result)
  restored <- unserialize(serialize(first, NULL))
  has_live_source <- function(value) {
    if (inherits(value, c("AgentResult", "AgentEvent", "AgentUsage"))) {
      return(TRUE)
    }
    if (is.list(value)) {
      return(any(vapply(value, has_live_source, logical(1))))
    }
    FALSE
  }

  expect_identical(S7::props(second), S7::props(first))
  expect_identical(S7::props(restored), S7::props(first))
  expect_false(has_live_source(S7::props(first)))
})
