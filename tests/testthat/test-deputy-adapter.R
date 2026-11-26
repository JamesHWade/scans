test_that("Deputy results pass the shared adapter contract", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  expect_adapter_conforms(
    source = deputy_result_fixture(),
    adapter = as_trajectory_deputy
  )
})

test_that("completed Deputy results become canonical trajectories", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  result <- deputy_result_fixture()
  bundle <- as_trajectory_deputy(result)
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
    deputy_event_timestamp(result$events[[1L]]$timestamp)
  )
  expect_identical(
    info$completed_at,
    deputy_event_timestamp(result$events[[2L]]$timestamp)
  )
  expect_identical(info$metadata[[1L]]$session_id, "session-001")
  expect_identical(info$metadata[[1L]]$usage$input_tokens, 8)
  expect_identical(info$metadata[[1L]]$usage$cost_usd, 0.001)
  expect_identical(
    info$metadata[[1L]]$run_context$research_run_id,
    "research-run-001"
  )
  expect_identical(trajectory_turns(bundle)$role, c("user", "assistant"))
  expect_identical(
    events$event_type,
    c("deputy:start", "content", "content", "deputy:stop")
  )
})

test_that("as_trajectory dispatches Deputy results", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  bundle <- as_trajectory(deputy_result_fixture())

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(trajectory_info(bundle)$source_type, "deputy")
})

test_that("Deputy AgentResult subclasses retain adapter dispatch", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  subclass <- S7::new_class("ScansAgentResult", parent = deputy::AgentResult)
  bundle <- as_trajectory(deputy_result_fixture(result_class = subclass))

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(trajectory_info(bundle)$source_type, "deputy")
})

test_that("objects that only look like AgentResults fail at the adapter boundary", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  fake_result <- new.env(parent = emptyenv())
  fake_result$n_turns <- length
  class(fake_result) <- c("AgentResult", "R6")

  condition <- rlang::catch_cnd(as_trajectory_deputy(fake_result))

  expect_s3_class(condition, "scans_error_deputy_source")
})

test_that("Deputy adapter arguments use source-specific conditions", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  condition <- rlang::catch_cnd(
    as_trajectory_deputy(deputy_result_fixture(), metadata = list("unnamed"))
  )

  expect_s3_class(condition, "scans_error_deputy_argument")
})

test_that("Deputy tool events preserve lifecycle correlation and failures", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

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
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

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
      limits = deputy::UsageLimits(max_requests = 10L)
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
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

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
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  result <- deputy_result_fixture(
    overrides = list(
      parent_agent_id = "agent-parent",
      parent_run_id = "run-parent",
      delegation_id = "delegation-001"
    )
  )

  bundle <- as_trajectory_deputy(result)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_true(is.na(info$parent_trajectory_id))
  expect_identical(info$metadata[[1L]]$parent_run_id, "run-parent")
  expect_identical(info$metadata[[1L]]$delegation_id, "delegation-001")
  expect_identical(
    losses$reason[losses$field == "parent_run_id"],
    "unsupported"
  )
})

test_that("Deputy result collections resolve included delegation parents", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  parent <- deputy_result_fixture()
  child <- deputy_result_fixture(
    overrides = list(
      run_id = "run-child",
      agent_id = "agent-child",
      agent_name = "writer",
      parent_agent_id = "agent-001",
      parent_run_id = "run-001",
      delegation_id = "delegation-001"
    )
  )

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
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  result <- deputy_result_fixture(overrides = list(run_id = NULL))

  bundle <- as_trajectory_deputy(result)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(info$trajectory_id, "trajectory-000001")
  expect_true(is.na(info$run_id))
  expect_true(is.na(info$source_id))
  expect_true(any(losses$field == "run_id"))
})

test_that("multiple missing Deputy run IDs get distinct fallback identities", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  first <- deputy_result_fixture(overrides = list(run_id = NULL))
  second <- deputy_result_fixture(overrides = list(run_id = NULL))

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
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  invalid_turns <- deputy_result_fixture(
    overrides = list(turns = list("not a turn"))
  )
  condition <- rlang::catch_cnd(as_trajectory_deputy(invalid_turns))
  expect_s3_class(condition, "scans_error_deputy_source")
  expect_snapshot(error = TRUE, as_trajectory_deputy(invalid_turns))
})

test_that("unsupported Deputy event text is represented as an adapter loss", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

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
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

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
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  result <- deputy_result_fixture()
  first <- as_trajectory_deputy(result)
  second <- as_trajectory_deputy(result)
  restored <- unserialize(serialize(first, NULL))
  has_live_source <- function(value) {
    if (
      inherits(value, c("AgentResult", "AgentEvent", "AgentUsage", "S7_object"))
    ) {
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

test_that("Deputy run endings set honest statuses and one stable ending shape", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  limit <- deputy::RunEndingLimit(
    "request_limit",
    field = "max_requests",
    actual = 3,
    limit = 3,
    wrapped_up = TRUE
  )
  failed <- deputy::RunEndingFailed(
    "setup_error",
    error = simpleError("Setup broke"),
    phase = "initialization"
  )
  cases <- list(
    answered = list(overrides = list(), status = "completed"),
    cut_off = list(
      overrides = list(finish_reason = "max_tokens"),
      status = "completed"
    ),
    stopped = list(
      overrides = list(stop_reason = "abandoned"),
      status = "interrupted"
    ),
    wrapped_up = list(
      overrides = list(
        stop_reason = "request_limit",
        ending = limit,
        finish_reason = "max_tokens"
      ),
      status = "interrupted"
    ),
    failed = list(
      overrides = list(stop_reason = "setup_error", ending = failed),
      status = "failed"
    )
  )
  endings <- lapply(cases, function(case) {
    result <- deputy_result_fixture(overrides = case$overrides)
    info <- trajectory_info(deputy_expect_clean(result))
    expect_identical(info$status, case$status)
    info$metadata[[1L]]$ending
  })

  expect_identical(
    vapply(endings, \(ending) ending$class, character(1)),
    c(
      answered = "RunEndingAnswered",
      cut_off = "RunEndingCutOff",
      stopped = "RunEndingStopped",
      wrapped_up = "RunEndingLimit",
      failed = "RunEndingFailed"
    )
  )
  for (ending in endings) {
    expect_named(
      ending,
      c(
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
    )
    expect_type(ending$actual, "double")
    expect_type(ending$limit, "double")
    expect_type(ending$wrapped_up, "logical")
    expect_type(ending$cut_off, "logical")
  }
  expect_false(endings$answered$cut_off)
  expect_identical(endings$cut_off$finish_reason, "max_tokens")
  expect_true(endings$cut_off$cut_off)
  expect_identical(endings$stopped$by, "abandoned")
  expect_identical(endings$stopped$reason, "abandoned")
  expect_identical(
    endings$wrapped_up[c("reason", "field", "actual", "limit", "wrapped_up")],
    list(
      reason = "request_limit",
      field = "max_requests",
      actual = 3,
      limit = 3,
      wrapped_up = TRUE
    )
  )
  expect_true(endings$wrapped_up$cut_off)
  expect_identical(endings$failed$reason, "setup_error")
  expect_identical(endings$failed$phase, "initialization")
  expect_true(is.na(endings$failed$by))
})

test_that("current Deputy lifecycle events project their payloads cleanly", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  limits <- deputy::UsageLimits(max_requests = 3L, on_exceed = "wrap_up")
  usage <- deputy::AgentUsage(
    requests = 3L,
    input_tokens = 40,
    output_tokens = 9
  )
  ending <- deputy::RunEndingLimit(
    "request_limit",
    field = "max_requests",
    actual = 3,
    limit = 3,
    wrapped_up = TRUE
  )
  events <- list(
    deputy::AgentEvent("start", task = "Review", usage_limits = limits),
    deputy::AgentEvent(
      "file_checkpoint",
      checkpoint_id = "checkpoint-001",
      name = "run run-001"
    ),
    deputy::AgentEvent(
      "request_start",
      request_number = 1L,
      fallback_index = 0L,
      provider = "fixture",
      model = "model-a"
    ),
    deputy::AgentEvent(
      "request_error",
      request_number = 1L,
      provider = "fixture",
      model = "model-a",
      condition = simpleError("HTTP 503")
    ),
    deputy::AgentEvent(
      "fallback",
      fallback_index = 1L,
      provider = "fixture",
      model = "model-b",
      condition = simpleError("HTTP 503"),
      usage = usage
    ),
    deputy::AgentEvent(
      "request_end",
      request_number = 2L,
      provider = "fixture",
      model = "model-b",
      outcome = "response"
    ),
    deputy::AgentEvent(
      "compaction_start",
      estimated_tokens = 900,
      estimate_source = "estimate",
      turns_compacted = 4L,
      turns_kept = 2L
    ),
    deputy::AgentEvent(
      "compaction_error",
      phase = "compaction",
      request_number = 3L,
      condition = simpleError("Summary failed"),
      usage = usage
    ),
    deputy::AgentEvent(
      "compaction",
      method = "summary",
      automatic = TRUE,
      turns_compacted = 4L,
      turns_kept = 2L,
      usage = usage,
      attempts = list()
    ),
    deputy::AgentEvent(
      "permission",
      tool_call_id = "call-001",
      decision = "deny"
    ),
    deputy::AgentEvent(
      "approval",
      approval_id = "approval-001",
      path = "approvals/approval-001.rds",
      status = "pending",
      tool_call_id = "call-002",
      tool_name = "write_file",
      tool_input = list(path = "report.md"),
      reason = "Writes need review"
    ),
    deputy::AgentEvent(
      "trusted_result",
      result_id = "result_001",
      result_type = "table",
      tool_name = "query",
      tool_fingerprint = "fingerprint-001",
      tool_call_id = "call-003",
      arguments = list(sql = "select 1"),
      value = list(rows = 1L)
    ),
    deputy::AgentEvent(
      "structured_attempt",
      attempt = 1L,
      valid = FALSE,
      value = list(score = "high"),
      turn = ellmer::AssistantTurn(list(ellmer::ContentText("{}"))),
      condition = NULL,
      feedback = "score must be a number"
    ),
    deputy::AgentEvent(
      "structured_attempt",
      attempt = 2L,
      valid = TRUE,
      value = list(score = 0.9),
      condition = NULL,
      feedback = NULL
    ),
    deputy::AgentEvent(
      "limit_wrap_up",
      reason = "request_limit",
      request_number = 3L
    ),
    deputy::AgentEvent(
      "run_error",
      phase = "stream",
      condition = simpleError("Stream closed")
    ),
    deputy::AgentEvent("usage", usage = usage, limits = limits),
    deputy::AgentEvent(
      "stop",
      reason = "request_limit",
      usage = usage,
      limit = list(
        reason = "request_limit",
        field = "max_requests",
        actual = 3,
        limit = 3
      ),
      ending = ending
    )
  )
  result <- deputy_result_fixture(
    events,
    overrides = list(stop_reason = "request_limit", ending = ending)
  )

  rows <- deputy_lifecycle_events(deputy_expect_clean(result))
  row <- \(type) rows[rows$event_type == paste0("deputy:", type), ]
  types <- vapply(events, \(event) event$type, character(1))

  expect_identical(rows$event_type, paste0("deputy:", types))
  expect_identical(
    rows$status[rows$status != "completed"],
    c("failed", "failed", "failed", "pending", "failed", "failed")
  )
  expect_identical(
    rows$event_type[rows$status == "failed"],
    paste0(
      "deputy:",
      c(
        "request_error",
        "compaction_error",
        "permission",
        "structured_attempt",
        "run_error"
      )
    )
  )
  expect_identical(row("request_error")$error, "HTTP 503")
  expect_identical(row("compaction_error")$error, "Summary failed")
  expect_identical(row("run_error")$error, "Stream closed")
  expect_identical(row("run_error")$metadata[[1L]]$phase, "stream")
  expect_true(is.na(row("permission")$error))
  expect_true(is.na(row("fallback")$error))
  expect_identical(
    row("fallback")$metadata[[1L]]$condition$message,
    "HTTP 503"
  )
  expect_identical(row("fallback")$metadata[[1L]]$usage$requests, 3L)

  expect_identical(
    row("start")$metadata[[1L]]$usage_limits$on_exceed,
    "wrap_up"
  )
  expect_identical(row("usage")$metadata[[1L]]$limits$max_requests, 3L)
  expect_identical(
    row("stop")$metadata[[1L]]$ending[c("class", "wrapped_up")],
    list(class = "RunEndingLimit", wrapped_up = TRUE)
  )
  expect_identical(row("limit_wrap_up")$metadata[[1L]]$request_number, 3L)
  expect_identical(row("compaction")$metadata[[1L]]$turns_compacted, 4L)
  expect_identical(row("compaction_start")$metadata[[1L]]$estimated_tokens, 900)
  expect_identical(row("request_end")$metadata[[1L]]$model, "model-b")
  expect_identical(
    row("file_checkpoint")$metadata[[1L]]$checkpoint_id,
    "checkpoint-001"
  )

  approval <- row("approval")
  expect_identical(approval$value[[1L]], list(path = "report.md"))
  expect_identical(approval$name, "write_file")
  expect_identical(approval$call_id, "call-002")
  expect_identical(approval$metadata[[1L]]$approval_id, "approval-001")

  trusted <- row("trusted_result")
  expect_identical(trusted$value[[1L]], list(rows = 1L))
  expect_identical(
    trusted$metadata[[1L]][c("result_id", "result_type")],
    list(result_id = "result_001", result_type = "table")
  )

  attempts <- row("structured_attempt")
  expect_identical(
    attempts$value,
    list(list(score = "high"), list(score = 0.9))
  )
  expect_identical(attempts$status, c("failed", "completed"))
  expect_identical(attempts$error, c("score must be a number", NA))
  expect_identical(
    lapply(attempts$metadata, \(metadata) metadata$valid),
    list(FALSE, TRUE)
  )
  expect_identical(
    attempts$metadata[[1L]]$feedback,
    "score must be a number"
  )
})

test_that("Deputy callback results become plain data", {
  skip_if_not_installed("deputy", "0.0.0.9007")

  expect_identical(
    deputy_plain_value(deputy::PermissionResultDeny("No writes")),
    list(
      class = "PermissionResultDeny",
      decision = "deny",
      reason = "No writes",
      interrupt = FALSE
    )
  )
  hook <- deputy_plain_value(deputy::HookResultPreToolUse(permission = "allow"))
  expect_identical(hook$class, "HookResultPreToolUse")
  expect_identical(hook$permission, "allow")
  expect_identical(
    deputy_plain_value(deputy::RunEndingFailed(error = simpleError("Boom"))),
    list(
      class = "RunEndingFailed",
      reason = "error",
      error = list(
        class = c("simpleError", "error", "condition"),
        message = "Boom"
      ),
      phase = NULL
    )
  )
})
