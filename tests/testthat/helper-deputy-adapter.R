deputy_result_fixture <- function(
  events = NULL,
  result_class = deputy::AgentResult
) {
  started_at <- as.POSIXct("2026-08-22 17:00:00", tz = "UTC")
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
    events[[1L]]$timestamp <- started_at
    events[[2L]]$timestamp <- started_at + 2
  }

  result_class$new(
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
}
