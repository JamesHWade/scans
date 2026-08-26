dsprrr_trace_fixture <- function() {
  completed_at <- as.POSIXct("2026-08-22 18:00:02", tz = "UTC")
  program_id <- paste0("sha256:", strrep("a", 64L))
  turns <- list(
    ellmer::UserTurn(list(ellmer::ContentText("Find the evidence"))),
    ellmer::AssistantTurn(
      list(ellmer::ContentText("Evidence found")),
      tokens = c(18, 6, 2),
      cost = 0.004,
      duration = 2,
      finish_reason = "success"
    )
  )

  tibble::tibble(
    timestamp = completed_at,
    latency_ms = 2000,
    input_tokens = 18L,
    cached_input_tokens = 2L,
    output_tokens = 6L,
    total_tokens = 24L,
    cost = 0.004,
    model = "mock-model",
    prompt_length = 17L,
    prompt = "Find the evidence",
    response = "Evidence found",
    program_artifact_id = program_id,
    trace_context = list(list(
      product = "tempest",
      research_run_id = "research-run-001",
      deputy_run_id = "deputy-run-001",
      agent_name = "research-module"
    )),
    output = list(list(answer = "Evidence found")),
    turns = list(turns),
    response_text = "Evidence found"
  )
}

dsprrr_module_fixture <- function() {
  program <- dsprrr::module_fn(
    "text -> answer",
    function(text) list(answer = paste("Echo", text))
  )
  dsprrr::run(program, text = "evidence", .progress = FALSE)
  program
}
