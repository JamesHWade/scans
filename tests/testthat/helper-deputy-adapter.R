deputy_result_fixture <- function(
  events = NULL,
  result_class = deputy::AgentResult,
  overrides = list()
) {
  run_context <- list(
    product = "tempest",
    research_run_id = "research-run-001"
  )

  if (is.null(events)) {
    events <- list(
      deputy::AgentEvent(
        "start",
        task = "Find the evidence",
        agent_id = "agent-001",
        agent_name = "researcher",
        session_id = "session-001",
        run_id = "run-001",
        run_context = run_context
      ),
      deputy::AgentEvent(
        "stop",
        reason = "complete",
        total_turns = 2L,
        cost = list(total = 0.001),
        usage = deputy::AgentUsage(
          requests = 1L,
          input_tokens = 8,
          output_tokens = 3,
          cost_usd = 0.001
        ),
        agent_id = "agent-001",
        agent_name = "researcher",
        session_id = "session-001",
        run_id = "run-001",
        run_context = run_context
      )
    )
  }

  args <- list(
    response = "Evidence found",
    turns = list(
      ellmer::UserTurn(list(ellmer::ContentText("Find the evidence"))),
      ellmer::AssistantTurn(
        list(ellmer::ContentText("Evidence found")),
        tokens = c(8, 3, 0),
        cost = 0.001,
        duration = 2,
        finish_reason = "success"
      )
    ),
    cost = list(input = 0, output = 0.001, cached = 0, total = 0.001),
    events = events,
    duration = 2,
    stop_reason = "complete",
    session_id = "session-001",
    run_id = "run-001",
    agent_id = "agent-001",
    agent_name = "researcher",
    run_context = run_context,
    usage = deputy::AgentUsage(
      requests = 1L,
      input_tokens = 8,
      output_tokens = 3,
      cost_usd = 0.001
    )
  )
  args[names(overrides)] <- overrides
  do.call(result_class, args)
}

# Converts one result and asserts the adapter recorded no losses beyond the
# ones named, so new Deputy values cannot silently degrade to placeholders.
deputy_expect_clean <- function(result, allowed = character()) {
  bundle <- as_trajectory_deputy(result)
  losses <- trajectory_losses(bundle)
  unexpected <- if (nrow(losses) == 0L) {
    character()
  } else {
    setdiff(losses$field, allowed)
  }
  testthat::expect_identical(unexpected, character())
  bundle
}

deputy_lifecycle_events <- function(bundle) {
  events <- trajectory_events(bundle)
  events[grepl("^deputy:", events$event_type), , drop = FALSE]
}
