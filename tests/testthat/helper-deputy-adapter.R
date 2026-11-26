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

deputy_offload_reference <- paste0(
  "deputy://tool-result/result_",
  strrep("a", 64L),
  "?text_sha256=",
  strrep("b", 64L)
)

deputy_offloaded_text <- function(reference = deputy_offload_reference) {
  paste(
    "[Tool result offloaded by Deputy]",
    paste0("reference: ", reference),
    "bytes: 90000",
    paste0("sha256: ", strrep("b", 64L)),
    "preview:\nyear,value\n2023,10",
    paste(
      "Call deputy_read_tool_result(reference, offset) for bounded chunks;",
      "the host can recover the R value with Agent$resolve_tool_result()."
    ),
    sep = "\n"
  )
}

deputy_manifest_fixture <- function(task, model = "gpt-test") {
  S7::props(deputy::DelegationManifest(
    input = S7::props(deputy::DelegationInput(task = task)),
    definition = list(
      name = "researcher",
      prompt = "You research.",
      id = "definition-001"
    ),
    sources = list(),
    scope = list(tenant = "acme"),
    system_prompt = "You research.",
    message = task,
    model = model,
    tools = "search",
    policies = list(
      permissions_id = "permissions-001",
      permission_mode = "default"
    ),
    size = list(text_bytes = 30L, estimated_tokens = 8L, manifest_bytes = 512L),
    provider = "OpenAI",
    params = list(temperature = 0)
  ))
}

# One settled delegation as `$export_subagents()` records it: a disclosed
# view built from Deputy's public value classes, with ellmer content records
# as its transcript.
deputy_export_child <- function(
  delegation_id,
  run_id,
  parent_run_id,
  parent_delegation_id = NULL,
  depth = 1L,
  task = "Find the 2023 figures",
  turns = NULL,
  references = list(),
  claims = list(missing_evidence = NULL, unresolved_work = NULL),
  status = "completed"
) {
  if (is.null(turns)) {
    turns <- list(
      ellmer::UserTurn(list(ellmer::ContentText(task))),
      ellmer::AssistantTurn(list(ellmer::ContentText("Figures found")))
    )
  }
  runtime <- list(
    delegation_id = delegation_id,
    agent_id = paste0("agent-", delegation_id),
    agent_name = "researcher",
    session_id = paste0("session-", delegation_id),
    run_id = run_id,
    parent_agent_id = "agent-001",
    parent_run_id = parent_run_id,
    parent_delegation_id = parent_delegation_id,
    depth = depth,
    root_agent_id = "agent-001",
    tool_call_id = paste0("call-", delegation_id),
    status = status,
    stop_reason = if (identical(status, "completed")) "complete" else "error",
    finish_reason = "stop",
    cut_off = FALSE,
    conversation_id = paste0("session-", delegation_id),
    task_success = "not_assessed",
    answer_truncated = FALSE,
    claim_omissions = list(),
    omitted_references = 0L
  )
  outcome <- deputy::DelegationOutcome(
    runtime = runtime,
    answer = "Figures found",
    references = references,
    claims = claims
  )
  usage <- deputy::AgentUsage(
    requests = 1L,
    input_tokens = 5,
    output_tokens = 2
  )
  list(
    task = task,
    outcome = S7::props(outcome),
    manifest = deputy_manifest_fixture(task),
    usage = S7::props(usage),
    cumulative_usage = S7::props(usage),
    errors = list(),
    transcript = lapply(turns, ellmer::contents_record),
    retention = list(
      transcript = "included",
      execution = "read_only",
      continuation = "unsupported"
    )
  )
}

# A lead result plus its disclosed delegation history, read back through
# deputy::delegation_history(). `delegation-001` ran under the lead and
# offloaded a large tool result; its nested `delegation-002` is disclosed
# without its transcript. No LeadAgent is driven: that needs a provider.
deputy_delegation_fixture <- function() {
  request <- ellmer::ContentToolRequest(
    id = "call-search-001",
    name = "search",
    arguments = list(query = "2023 figures")
  )
  turns <- list(
    ellmer::UserTurn(list(ellmer::ContentText("Find the 2023 figures"))),
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(
      ellmer::ContentToolResult(
        value = deputy_offloaded_text(),
        request = request
      )
    )),
    ellmer::AssistantTurn(list(ellmer::ContentText("Figures found")))
  )
  reference <- list(
    reference = deputy_offload_reference,
    source = "tool_result",
    bytes = 90000,
    sha256 = strrep("b", 64L),
    agent_id = "agent-delegation-001",
    run_id = "run-child-001",
    delegation_id = "delegation-001",
    tool_call_id = "call-search-001",
    tool_name = "search",
    verification = "not_assessed",
    approval = "not_granted",
    availability = "available"
  )
  export <- deputy_export_fixture(
    list(
      deputy_export_child(
        "delegation-001",
        "run-child-001",
        parent_run_id = "run-001",
        turns = turns,
        references = list(reference),
        claims = list(
          missing_evidence = "No 2024 figures were available",
          unresolved_work = NULL
        )
      ),
      deputy_export_child(
        "delegation-002",
        "run-child-002",
        parent_run_id = "run-child-001",
        parent_delegation_id = "delegation-001",
        depth = 2L,
        task = "Check the 2023 source",
        claims = list(missing_evidence = NULL, unresolved_work = "")
      )
    )
  )
  redact <- function(view, requester) {
    if (identical(view$outcome$runtime$delegation_id, "delegation-002")) {
      view$transcript <- NULL
    }
    view
  }
  list(
    lead = deputy_result_fixture(),
    export = export,
    history = deputy_read_history(export, redact)
  )
}

deputy_export_fixture <- function(children) {
  list(
    schema_version = 1L,
    settled = TRUE,
    scope = list(tenant = "acme"),
    children = children
  )
}

deputy_read_history <- function(
  export,
  redact = function(view, requester) view
) {
  disclosure <- deputy::DelegationDisclosure(
    authorize = \(requester, scope) identical(requester, "reviewer"),
    redact = redact
  )
  deputy::delegation_history(export, "reviewer", disclosure, export$scope)
}
