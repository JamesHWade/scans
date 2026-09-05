test_that("canonical measures preserve partial counts, zero and summary semantics", {
  bundle <- TrajectoryBundle(
    data.frame(trajectory_id = "a", source_type = "manual"),
    tibble::tibble(
      trajectory_id = "a",
      turn_id = letters[1:3],
      turn_index = 1:3,
      role = c("user", "assistant", "assistant"),
      input_tokens = c(NA, 10, NA),
      output_tokens = c(NA, 0, NA),
      duration = c(NA, 2, NA),
      cost = c(NA, 0.2, 0.3)
    ),
    data.frame()
  )
  rows <- resource_row(
    bundle,
    c("input_tokens", "output_tokens", "turn_duration")
  )
  expect_equal(rows$value, c(10, 0, 2))
  expect_equal(rows$n_recorded, rep(1L, 3))
  expect_equal(rows$n_total, rep(2L, 3))
  expect_equal(rows$coverage, rep("partial", 3))
  expect_equal(resource_row(bundle, "cost")$value, NA_real_)
  expect_equal(resource_row(bundle, "cost")$currency, NA_character_)
  expect_equal(resource_row(bundle, "cost")$price_basis, NA_character_)
  expect_equal(summarize_trajectories(bundle)$cost, 0.5)
  expect_equal(resource_row(bundle, "elapsed")$n_total, 1L)
  empty <- TrajectoryBundle(data.frame(), data.frame(), data.frame())
  expect_equal(nrow(measure_trajectories(empty)), 0L)
  expect_identical(
    names(measure_trajectories(empty)),
    names(measure_trajectories(bundle))
  )
  expect_error(
    measure_trajectories(list()),
    class = "scans_error_trajectory_type"
  )
})

test_that("captured calls count usage once despite repeated histories and duplicate spans", {
  skip_if_not_installed("jsonlite")
  message <- function(role, text) {
    list(role = role, parts = list(list(type = "text", content = text)))
  }
  early <- resource_span(
    "early",
    0,
    2,
    extra = list(
      "gen_ai.usage.input_tokens" = 100,
      "gen_ai.usage.output_tokens" = 20,
      "gen_ai.output.messages" = jsonlite::toJSON(
        list(message("assistant", "First answer")),
        auto_unbox = TRUE
      )
    )
  )
  late <- resource_span(
    "late",
    10,
    13,
    extra = list(
      "gen_ai.usage.input_tokens" = 200,
      "gen_ai.input.messages" = jsonlite::toJSON(
        list(message("assistant", "First answer")),
        auto_unbox = TRUE
      ),
      "gen_ai.output.messages" = jsonlite::toJSON(
        list(message("assistant", "Second answer")),
        auto_unbox = TRUE
      )
    )
  )
  for (source in list(
    list(early, late, early),
    list(conversation = list(early, late, early))
  )) {
    bundle <- as_trajectory_otel(source)
    rows <- resource_row(bundle, c("input_tokens", "output_tokens"))
    expect_equal(rows$value, c(300, 20))
    expect_equal(rows$n_recorded, c(2L, 1L))
    expect_equal(rows$n_total, c(2L, 2L))
    expect_equal(rows$coverage, c("complete", "partial"))
    expect_equal(rows$adapter, rep("as_trajectory_otel/1", 2))
    expect_equal(summarize_trajectories(bundle)$input_tokens, 200)
    expect_equal(nrow(trajectory_turns(bundle)), 2L)
    expect_equal(resource_row(bundle, "model_duration")$value, 5)
    expect_equal(resource_row(bundle, "unattributed_elapsed")$value, 8)
  }
})

test_that("nested and parallel calls are inclusive sums but unioned as recorded work", {
  skip_if_not_installed("jsonlite")
  bundle <- as_trajectory_otel(list(
    resource_span("model", 0, 10),
    resource_span("tool-a", 2, 7, "execute_tool", parent = "model"),
    resource_span("tool-b", 4, 9, "execute_tool", parent = "tool-a"),
    resource_span("next-model", 20, 22)
  ))
  rows <- resource_row(
    bundle,
    c(
      "elapsed",
      "model_duration",
      "tool_duration",
      "recorded_work",
      "unattributed_elapsed"
    )
  )
  expect_equal(rows$value, c(22, 12, 10, 12, 10))
  expect_equal(
    rows$aggregation,
    c("interval", "sum", "sum", "union", "difference")
  )
  expect_equal(rows$source_unit, rep("ns", 5))
  expect_equal(rows$coverage, rep("complete", 5))
})

test_that("missing and invalid intervals prevent invented idle time", {
  skip_if_not_installed("jsonlite")
  for (end in c(NA, -1, Inf)) {
    bundle <- as_trajectory_otel(list(
      resource_span("valid", 0, 2),
      resource_span("unknown", 3, end)
    ))
    work <- resource_row(bundle, "recorded_work")
    expect_equal(work$value, 2)
    expect_equal(work$coverage, "partial")
    expect_equal(work$n_recorded, 1L)
    expect_equal(work$n_total, 2L)
    expect_equal(resource_row(bundle, "unattributed_elapsed")$value, NA_real_)
    expect_equal(resource_row(bundle, "elapsed")$value, NA_real_)
  }
  bundle <- as_trajectory_otel(list(resource_span("unknown", NA, NA)))
  expect_equal(resource_row(bundle, "model_duration")$value, NA_real_)
  expect_equal(resource_row(bundle, "recorded_work")$coverage, "unavailable")
})

test_that("model-only capture supports measures without claiming a transcript or first token", {
  skip_if_not_installed("jsonlite")
  bundle <- as_trajectory_otel(list(
    resource_span(
      "one",
      0,
      2,
      extra = list(
        "gen_ai.response.time_to_first_chunk" = 0.3,
        "gen_ai.usage.input_tokens" = 0,
        "gen_ai.usage.cache_read.input_tokens" = 0
      )
    ),
    resource_span(
      "two",
      3,
      5,
      extra = list("gen_ai.response.time_to_first_chunk" = 0.7)
    ),
    resource_span("three", 6, 8)
  ))
  chunk <- resource_row(bundle, "first_chunk_latency")
  expect_equal(chunk$value, 0.5)
  expect_equal(chunk$n_recorded, 2L)
  expect_equal(chunk$n_total, 3L)
  expect_equal(chunk$source_unit, "s")
  expect_equal(resource_row(bundle, "first_token_latency")$value, NA_real_)
  expect_equal(resource_row(bundle, "cached_input_tokens")$value, 0)
  expect_equal(nrow(trajectory_turns(bundle)), 0L)
  expect_equal(nrow(scan_trajectories(bundle)), 0L)
})

test_that("old and malformed metadata retain safe fallbacks with unknown coverage", {
  for (metadata in list(
    list(otel = list(input_tokens = 50)),
    list(otel = list(input_tokens = 50, measures = list(version = 2L))),
    list(
      otel = list(
        input_tokens = 50,
        measures = list(
          version = 1L,
          adapter = "as_trajectory_otel",
          input_tokens = list(value = 80, n_recorded = 2, n_total = 1)
        )
      )
    )
  )) {
    bundle <- TrajectoryBundle(
      tibble::tibble(
        trajectory_id = "legacy",
        source_type = "otel",
        metadata = list(metadata)
      ),
      data.frame(),
      data.frame()
    )
    expect_equal(resource_row(bundle, "input_tokens")$value, 50)
    expect_equal(resource_row(bundle, "input_tokens")$coverage, "unknown")
    expect_equal(resource_row(bundle, "input_tokens")$n_recorded, NA_integer_)
  }
})

test_that("parent and child measurements retain attribution without pooling costs or durations", {
  start <- as.POSIXct("2026-09-01", tz = "UTC")
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("parent", "child"),
      source_type = "manual",
      parent_trajectory_id = c(NA, "parent"),
      started_at = start,
      completed_at = start + c(10, 8)
    ),
    data.frame(),
    data.frame()
  )
  rows <- resource_row(bundle, "elapsed")
  expect_equal(rows$value, c(10, 8))
  expect_equal(rows$parent_trajectory_id, c(NA, "parent"))
  expect_equal(
    resource_row(bundle, "recorded_work")$value,
    c(NA_real_, NA_real_)
  )
})

test_that("public Tempest review retains unsupported resource measures", {
  bundle <- as_trajectory_tempest(tempest_review_fixture())
  expect_equal(resource_row(bundle, "input_tokens")$coverage, "unavailable")
  expect_equal(resource_row(bundle, "model_duration")$value, NA_real_)
  expect_equal(resource_row(bundle, "first_token_latency")$value, NA_real_)
})

test_that("public ellmer turns expose partial usage without inventing call timing", {
  skip_if_not_installed("ellmer", "0.4.2")
  bundle <- as_trajectory_ellmer(ellmer_tool_turns_fixture())
  rows <- resource_row(
    bundle,
    c("input_tokens", "output_tokens", "cached_input_tokens", "turn_duration")
  )
  expect_equal(rows$value, c(12, 3, 2, 1.5))
  expect_equal(rows$n_recorded, rep(1L, 4))
  expect_equal(rows$n_total, rep(2L, 4))
  expect_equal(rows$coverage, rep("partial", 4))
  expect_equal(resource_row(bundle, "model_duration")$value, NA_real_)
  expect_equal(resource_row(bundle, "tool_duration")$n_total, 2L)
})

test_that("equivalent ellmer and OTel cache usage agrees without changing summaries", {
  skip_if_not_installed("ellmer", "0.4.2")
  skip_if_not_installed("jsonlite")
  ellmer <- as_trajectory_ellmer(list(ellmer::AssistantTurn(
    list(ellmer::ContentText("Hello")),
    tokens = c(10, 3, 2)
  )))
  otel <- as_trajectory_otel(list(resource_span(
    "cached",
    extra = list(
      "gen_ai.usage.input_tokens" = 12,
      "gen_ai.usage.output_tokens" = 3,
      "gen_ai.usage.cache_read.input_tokens" = 2
    )
  )))
  for (bundle in list(ellmer, otel)) {
    input <- resource_row(bundle, "input_tokens")
    expect_equal(input$value, 12)
    expect_all_true(input$includes_cached_input)
    expect_equal(input$coverage, "complete")
    expect_equal(
      scans_app_performance_data(scans_app_data(bundle), 1L)$median_tokens,
      15
    )
  }
  expect_equal(summarize_trajectories(ellmer)$input_tokens, 10)
  expect_equal(resource_row(ellmer, "cached_input_tokens")$value, 2)
})

test_that("missing ellmer cache components cannot become complete input totals", {
  skip_if_not_installed("ellmer", "0.4.2")
  components <- list(c(10, 3, 0), c(20, 3, NA), c(NA, 3, 5))
  bundles <- lapply(components, function(tokens) {
    as_trajectory_ellmer(ellmer::AssistantTurn(
      list(ellmer::ContentText("Hello")),
      tokens = tokens
    ))
  })
  expect_equal(resource_row(bundles[[1L]], "input_tokens")$value, 10)
  for (bundle in bundles[2:3]) {
    input <- resource_row(bundle, "input_tokens")
    expect_equal(input$value, NA_real_)
    expect_equal(input$n_recorded, 0L)
    expect_equal(input$n_total, 1L)
    expect_equal(input$coverage, "unavailable")
  }
  combined <- as_trajectory_ellmer(lapply(components, function(tokens) {
    ellmer::AssistantTurn(list(ellmer::ContentText("Hello")), tokens = tokens)
  }))
  input <- resource_row(combined, "input_tokens")
  expect_equal(input$value, 10)
  expect_equal(input$n_recorded, 1L)
  expect_equal(input$n_total, 3L)
  expect_equal(input$coverage, "partial")
})

test_that("manual input keeps unknown cache inclusion", {
  bundle <- TrajectoryBundle(
    data.frame(trajectory_id = "manual", source_type = "manual"),
    tibble::tibble(
      trajectory_id = "manual",
      turn_id = "turn",
      turn_index = 1L,
      role = "assistant",
      input_tokens = 10,
      cached_input_tokens = 2
    ),
    data.frame()
  )
  input <- resource_row(bundle, "input_tokens")
  expect_equal(input$value, 10)
  expect_identical(input$includes_cached_input, NA)
})

test_that("dsprrr ellmer turns retain their separate cache component semantics", {
  skip_if_not_installed("ellmer", "0.4.2")
  skip_if_not_installed("dsprrr", "0.0.0.9000")
  bundle <- as_trajectory_dsprrr(dsprrr_trace_fixture())
  input <- resource_row(bundle, "input_tokens")
  expect_equal(input$value, 20)
  expect_all_true(input$includes_cached_input)
  expect_equal(summarize_trajectories(bundle)$input_tokens, 18)
})
