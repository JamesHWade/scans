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
  traces <- dsprrr_trace_fixture()
  program <- dsprrr::module(dsprrr::signature("text -> answer"))
  program$state$traces <- list(list(
    timestamp = traces$timestamp[[1L]],
    prompt = traces$prompt[[1L]],
    user_turn = traces$turns[[1L]][[1L]],
    assistant_turn = traces$turns[[1L]][[2L]],
    turns = traces$turns[[1L]],
    output = traces$output[[1L]],
    latency_ms = traces$latency_ms[[1L]],
    tokens = list(
      input_tokens = traces$input_tokens[[1L]],
      cached_input_tokens = traces$cached_input_tokens[[1L]],
      output_tokens = traces$output_tokens[[1L]],
      total_tokens = traces$total_tokens[[1L]]
    ),
    cost = traces$cost[[1L]],
    model = traces$model[[1L]],
    program_artifact_id = dsprrr::program_artifact_id(program),
    trace_context = traces$trace_context[[1L]]
  ))
  program
}
