test_that("application measures count trajectories and expose missing evidence", {
  data <- scans_app_data(performance_fixture())
  overview <- scans_app_performance_data(data, 1:3)

  expect_equal(overview$n, 3)
  expect_equal(overview$n_findings, 1)
  expect_equal(overview$n_evaluated, 1)
  expect_equal(overview$n_losses, 1)
  expect_equal(overview$unassigned_losses, 1)
  expect_equal(overview$n_timed, 2)
  expect_equal(overview$median_elapsed, 55)
  expect_equal(overview$p95_elapsed, 95.5)
  expect_equal(overview$n_tokens, 2)
  expect_equal(overview$median_tokens, 67.5)
  errors <- overview$patterns[overview$patterns$scan == "event_error", ]
  expect_equal(errors$trajectories, 1)
  expect_equal(errors$findings, 2)

  filtered <- scans_app_performance_data(data, 1L)
  expect_equal(filtered$n, 1)
  expect_equal(filtered$total, 3)
  expect_equal(filtered$median_elapsed, 10)
  expect_equal(filtered$n_findings, 0)
  expect_equal(filtered$n_losses, 0)
})

test_that("empty and unmeasured cohorts do not manufacture zero usage", {
  data <- scans_app_data(performance_fixture())
  for (indices in list(integer(), 3L)) {
    overview <- scans_app_performance_data(data, indices)
    expect_equal(overview$n_timed, 0)
    expect_equal(overview$n_tokens, 0)
    expect_identical(overview$median_elapsed, NA_real_)
    expect_identical(overview$p95_elapsed, NA_real_)
    expect_identical(overview$median_tokens, NA_real_)
  }
  empty <- scans_app_performance_data(
    scans_app_data(TrajectoryBundle(data.frame(), data.frame(), data.frame())),
    integer()
  )
  expect_equal(empty$n, 0)
})

test_that("recorded token measures retain one-sided usage and known zeros", {
  ids <- c("input", "output", "both", "zero", "unknown")
  bundle <- TrajectoryBundle(
    tibble::tibble(trajectory_id = ids, source_type = "manual"),
    tibble::tibble(
      trajectory_id = ids,
      turn_id = paste0(ids, "-turn"),
      turn_index = 1L,
      role = "assistant",
      input_tokens = c(10, NA, 5, 0, NA),
      output_tokens = c(NA, 20, 7, NA, NA)
    ),
    data.frame()
  )
  overview <- scans_app_performance_data(scans_app_data(bundle), seq_along(ids))

  expect_equal(overview$trajectories$tokens, c(10, 20, 12, 0, NA))
  expect_equal(overview$n_tokens, 4)
  expect_equal(overview$median_tokens, 11)
  expect_identical(
    scans_app_performance_order(overview$trajectories, "tokens")$trajectory_id,
    c("output", "both", "input", "zero", "unknown")
  )

  skip_if_not_installed("htmltools")
  html <- as.character(scans_app_performance_ui(
    overview,
    "Partial capture",
    "tokens",
    scan_registry()$scan
  ))
  expect_match(html, "Input or output recorded for 4 / 5", fixed = TRUE)
})

test_that("ranked trajectories keep identities and put unknown values last", {
  data <- scans_app_performance_data(scans_app_data(performance_fixture()), 1:3)
  expect_identical(
    scans_app_performance_order(data$trajectories, "elapsed")$trajectory_id,
    c("slow", "fast", "unknown")
  )
  expect_identical(
    scans_app_performance_order(data$trajectories, "tokens")$index,
    c(2L, 1L, 3L)
  )
})

test_that("application overview follows filters and opens exact evidence without loading again", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  calls <- 0L
  bundle <- performance_fixture()
  app <- scans_app(list(Example = function() {
    calls <<- calls + 1L
    bundle
  }))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    expect_equal(performance()$n, 3)
    session$setInputs(scans_app_query = "small")
    expect_equal(performance()$n, 1)
    expect_equal(performance()$median_elapsed, 10)
    session$setInputs(scans_app_query = "", scans_app_pattern = "event_error")
    expect_identical(visible(), 2L)
    expect_equal(performance()$n_findings, 1)
    session$setInputs(scans_app_clear_pattern = 1)
    expect_equal(performance()$n, 3)
    session$setInputs(scans_app_inspect = "slow")
    expect_identical(selected_trajectory_id(), "slow")
    expect_match(output$scans_app_evidence$html, "error-1", fixed = TRUE)
    expect_match(
      output$scans_app_transcript$html,
      "First failure",
      fixed = TRUE
    )
    session$setInputs(scans_app_scans = character())
    expect_equal(performance()$n_findings, 0)
    expect_match(output$scans_app_performance$html, "Scans off", fixed = TRUE)
    session$setInputs(scans_app_inspect = "invalid")
    expect_identical(selected_trajectory_id(), "slow")
    expect_identical(calls, 1L)
  })
  expect_identical(
    trajectory_info(bundle)$trajectory_id,
    c("fast", "slow", "unknown")
  )
})

test_that("application overview handles the public Tempest review fixture", {
  skip_if_not_installed("htmltools")
  bundle <- as_trajectory_tempest(readRDS(test_path(
    "fixtures",
    "tempest-review.rds"
  )))
  data <- scans_app_data(bundle)
  overview <- scans_app_performance_data(data, seq_len(nrow(data$info)))
  expect_equal(overview$n, 1)
  expect_equal(
    overview$trajectories$trajectory_id,
    trajectory_info(bundle)$trajectory_id
  )
  html <- as.character(scans_app_performance_ui(
    overview,
    "Tempest",
    "elapsed",
    scan_registry()$scan
  ))
  expect_match(html, "Evidence available", fixed = TRUE)
  expect_match(html, "adapter losses", fixed = TRUE)
  expect_no_match(html, "NaN|Inf")
})

test_that("overview escapes source labels and transcript titles", {
  skip_if_not_installed("htmltools")
  data <- scans_app_performance_data(scans_app_data(performance_fixture()), 1:3)
  data$trajectories$title[[1L]] <- "<script>alert(1)</script>"
  html <- as.character(scans_app_performance_ui(
    data,
    "<b>App</b>",
    "tokens",
    scan_registry()$scan
  ))
  expect_match(html, "&lt;b&gt;App&lt;/b&gt;", fixed = TRUE)
  expect_no_match(html, "<script>", fixed = TRUE)
})

test_that("the overview consumes public ellmer turns without inferring wall time", {
  skip_if_not_installed("ellmer", "0.4.2")
  bundle <- as_trajectory(ellmer_tool_turns_fixture())
  overview <- scans_app_performance_data(scans_app_data(bundle), 1L)

  expect_equal(overview$n, 1)
  expect_equal(overview$trajectories$n_tool_calls, 2)
  expect_equal(overview$median_tokens, 13)
  expect_identical(overview$median_elapsed, NA_real_)
  expect_equal(overview$n_timed, 0)
})

test_that("OTel overview and inspector use usage across all model calls", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("htmltools")
  span <- function(id, conversation, start, input_tokens, output_tokens) {
    list(
      trace_id = paste0("trace-", conversation),
      span_id = id,
      parent_span_id = "",
      start_time = as.character(start * 1e9),
      end_time = as.character((start + 1) * 1e9),
      attributes = list(
        "gen_ai.operation.name" = "chat",
        "gen_ai.conversation.id" = conversation,
        "gen_ai.request.model" = "test-model",
        "gen_ai.usage.input_tokens" = input_tokens,
        "gen_ai.usage.output_tokens" = output_tokens,
        "gen_ai.input.messages" = '[{"role":"user","parts":[{"type":"text","content":"Hello"}]}]',
        "gen_ai.output.messages" = '[{"role":"assistant","parts":[{"type":"text","content":"Hi"}]}]'
      )
    )
  }
  bundle <- as_trajectory_otel(list(
    span("early", "multi", 1, 500, 100),
    span("late", "multi", 5, 10, 2),
    span("single", "single", 3, 100, 20)
  ))
  data <- scans_app_data(bundle)
  multi <- match("otel/multi", data$info$trajectory_id)
  overview <- scans_app_performance_data(data, seq_len(nrow(data$info)))

  expect_equal(data$summaries$input_tokens[[multi]], 10)
  expect_equal(overview$trajectories$tokens[[multi]], 612)
  expect_equal(overview$median_tokens, 366)
  expect_equal(overview$n_tokens, 2)
  expect_identical(
    scans_app_performance_order(overview$trajectories, "tokens")$trajectory_id,
    c("otel/multi", "otel/single")
  )
  expect_match(
    as.character(scans_app_overview_ui(data, multi)),
    "510 in / 102 out",
    fixed = TRUE
  )
  expect_match(
    as.character(scans_app_performance_table(overview$trajectories, "tokens")),
    "<td>612</td>",
    fixed = TRUE
  )
})

test_that("pattern filters reset when changing application", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(
    First = performance_fixture(),
    Second = trajectory_fixture("simple_exchange")
  ))
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(scans_app_pattern = "event_error")
    expect_equal(performance()$n, 1)
    session$setInputs(scans_app_application = "Second")
    expect_null(pattern_filter())
    expect_equal(performance()$n, 1)
    expect_identical(
      performance()$trajectories$trajectory_id,
      "trajectory-simple"
    )
  })
})

test_that("disabling the active pattern's scan restores the cohort", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  calls <- 0L
  app <- scans_app(list(Example = function() {
    calls <<- calls + 1L
    performance_fixture()
  }))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(scans_app_pattern = "event_error")
    expect_equal(performance()$n, 1)
    session$setInputs(
      scans_app_scans = setdiff(scan_registry()$scan, "event_error")
    )
    expect_null(pattern_filter())
    expect_equal(performance()$n, 3)
    session$setInputs(scans_app_pattern = "event_error")
    expect_null(pattern_filter())
    expect_equal(performance()$n, 3)
    expect_identical(calls, 1L)
  })
})

test_that("patterns disappear when thresholds remove their findings", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Example = trajectory_fixture("repeated_tools")))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = "repeated_tool_call")
    session$setInputs(scans_app_pattern = "repeated_tool_call")
    expect_identical(pattern_filter(), "repeated_tool_call")
    session$setInputs(scans_app_query = "no matching trajectory")
    expect_identical(pattern_filter(), "repeated_tool_call")
    session$setInputs(scans_app_query = "", scans_app_repeat_threshold = 3L)
    expect_null(pattern_filter())
    expect_equal(performance()$n, 1)
    session$setInputs(scans_app_pattern = "repeated_tool_call")
    expect_null(pattern_filter())
    expect_equal(performance()$n, 1)
  })
})
