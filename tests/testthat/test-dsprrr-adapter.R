test_that("exported dsprrr traces become canonical trajectories", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  bundle <- as_trajectory_dsprrr(traces)
  info <- trajectory_info(bundle)
  turns <- trajectory_turns(bundle)
  events <- trajectory_events(bundle)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(info$source_type, "dsprrr")
  expect_identical(info$source_id, traces$program_artifact_id)
  expect_identical(info$run_id, "deputy-run-001")
  expect_identical(info$agent, "research-module")
  expect_identical(info$model, "mock-model")
  expect_identical(
    info$started_at,
    as.POSIXct("2026-08-22 18:00:00", tz = "UTC")
  )
  expect_identical(info$completed_at, traces$timestamp)
  expect_identical(turns$role, c("user", "assistant"))
  expect_identical(
    events$event_type[events$event_type == "dsprrr:trace"],
    "dsprrr:trace"
  )
  expect_identical(
    events$value[[which(events$event_type == "dsprrr:trace")]],
    list(
      input_tokens = 18,
      cached_input_tokens = 2,
      output_tokens = 6,
      total_tokens = 24,
      cost = 0.004,
      latency_ms = 2000
    )
  )
  expect_identical(info$metadata[[1L]]$program_artifact_id, info$source_id)
  expect_identical(
    info$metadata[[1L]]$program_artifact_assurance,
    "integrity"
  )
  expect_identical(
    info$metadata[[1L]]$program_artifact_authenticity,
    "not_asserted"
  )
  expect_identical(info$metadata[[1L]]$prompt_length, 17)
  expect_identical(
    info$metadata[[1L]]$trace_context$research_run_id,
    "research-run-001"
  )
  expect_identical(
    info$metadata[[1L]]$content$output,
    list(answer = "Evidence found")
  )
})

test_that("dsprrr semantic tool events retain ellmer correlation", {
  skip_if_not_installed("ellmer", "0.4.2")

  request <- ellmer::ContentToolRequest(
    id = "call-001",
    name = "lookup",
    arguments = list(id = 42L)
  )
  result <- ellmer::ContentToolResult("Evidence", request = request)
  traces <- dsprrr_trace_fixture()
  traces$turns <- list(list(
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(result))
  ))

  events <- trajectory_events(as_trajectory_dsprrr(traces))
  call <- events[events$event_type == "tool_call", , drop = FALSE]
  result <- events[events$event_type == "tool_result", , drop = FALSE]

  expect_identical(call$call_id, "call-001")
  expect_identical(result$call_id, "call-001")
  expect_identical(result$parent_event_id, call$event_id)
})

test_that("partial dsprrr rows retain context and report unavailable semantics", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  traces$program_artifact_id <- NA_character_
  traces$trace_context <- list(list(
    product = "tempest",
    research_run_id = "research-run-001"
  ))
  traces$turns <- list(list("not an ellmer turn"))

  bundle <- as_trajectory_dsprrr(traces)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(info$source_id, NA_character_)
  expect_identical(info$run_id, NA_character_)
  expect_identical(
    info$metadata[[1L]]$trace_context$research_run_id,
    "research-run-001"
  )
  expect_identical(nrow(trajectory_turns(bundle)), 0L)
  expect_in("program_artifact_id", losses$field)
  expect_in("turns", losses$field)
  expect_setequal(unique(losses$reason), "unsupported")
})

test_that("as_trajectory dispatches authenticated dsprrr modules", {
  skip_if_not_installed("dsprrr", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  program <- dsprrr_module_fixture()
  explicit <- as_trajectory_dsprrr(program)
  generic <- as_trajectory(program)

  expect_identical(S7::props(generic), S7::props(explicit))
  expect_identical(trajectory_info(generic)$source_type, "dsprrr")
})

test_that("empty dsprrr modules convert quietly without provider calls", {
  skip_if_not_installed("dsprrr", "0.0.0.9000")

  program <- dsprrr::module(dsprrr::signature("text -> answer"))
  bundle <- expect_no_message(as_trajectory_dsprrr(program))

  expect_identical(nrow(trajectory_info(bundle)), 0L)
})

test_that("conflicting caller run identities are retained but not promoted", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  traces$trace_context <- list(list(
    run_id = "generic-run-001",
    deputy_run_id = "deputy-run-001",
    research_run_id = "research-run-001"
  ))

  bundle <- as_trajectory_dsprrr(traces)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(info$run_id, NA_character_)
  expect_identical(info$metadata[[1L]]$trace_context$run_id, "generic-run-001")
  expect_identical(
    info$metadata[[1L]]$trace_context$deputy_run_id,
    "deputy-run-001"
  )
  expect_in("trace_context$run_id", losses$field)
})

test_that("dsprrr source extensions are sanitized instead of silently dropped", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  traces$provider_note <- "cache hit"
  traces$live <- list(new.env(parent = emptyenv()))
  traces$trace_context <- list(list(
    deputy_run_id = "deputy-run-001",
    api_key = "secret"
  ))

  bundle <- as_trajectory_dsprrr(traces)
  source_fields <- trajectory_info(bundle)$metadata[[1L]]$source_fields
  losses <- trajectory_losses(bundle)

  expect_identical(source_fields$provider_note, "cache hit")
  expect_identical(source_fields$live, "<unsupported>")
  expect_identical(
    trajectory_info(bundle)$metadata[[1L]]$trace_context$api_key,
    "<redacted>"
  )
  expect_in("metadata$source_fields$live", losses$field)
  expect_in("metadata$trace_context$api_key", losses$field)
})

test_that("unavailable dsprrr metrics become missing values and explicit losses", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  traces$timestamp <- as.POSIXct(NA, tz = "UTC")
  traces$latency_ms <- -1
  traces$input_tokens <- NA_integer_
  traces$cached_input_tokens <- -2L
  traces$model <- NA_character_

  bundle <- as_trajectory_dsprrr(traces)
  info <- trajectory_info(bundle)
  event <- trajectory_events(bundle)
  event <- event[event$event_type == "dsprrr:trace", , drop = FALSE]
  losses <- trajectory_losses(bundle)

  expect_identical(info$started_at, as.POSIXct(NA, tz = "UTC"))
  expect_identical(info$completed_at, as.POSIXct(NA, tz = "UTC"))
  expect_identical(info$model, NA_character_)
  expect_identical(event$duration, NA_real_)
  expect_identical(event$value[[1L]]$input_tokens, NA_real_)
  expect_identical(event$value[[1L]]$cached_input_tokens, NA_real_)
  expect_setequal(
    intersect(
      losses$field,
      c(
        "timestamp",
        "latency_ms",
        "input_tokens",
        "cached_input_tokens",
        "model"
      )
    ),
    c(
      "timestamp",
      "latency_ms",
      "input_tokens",
      "cached_input_tokens",
      "model"
    )
  )
})

test_that("empty dsprrr snapshots return empty canonical bundles", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  empty <- as_trajectory_dsprrr(traces[0, ])

  expect_identical(nrow(trajectory_info(empty)), 0L)
  expect_identical(nrow(trajectory_turns(empty)), 0L)
  expect_identical(nrow(trajectory_events(empty)), 0L)
  expect_identical(nrow(trajectory_losses(empty)), 0L)
})

test_that("repeated dsprrr rows receive distinct local identities", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  repeated <- as_trajectory_dsprrr(traces[c(1L, 1L), ])

  expect_identical(
    basename(trajectory_info(repeated)$trajectory_id),
    c("trace-000001", "trace-000002")
  )
  expect_identical(nrow(trajectory_info(repeated)), 2L)
})

test_that("dsprrr output satisfies the ellmerverse correlation fixture", {
  skip_if_not_installed("ellmer", "0.4.2")

  expected <- trajectory_info(fixture_ellmerverse_correlation())
  expected <- expected[expected$source_type == "dsprrr", , drop = FALSE]
  traces <- dsprrr_trace_fixture()
  bundle <- as_trajectory_dsprrr(
    traces,
    trajectory_id = expected$trajectory_id
  )
  actual <- trajectory_info(bundle)
  trace_event <- trajectory_events(bundle)
  trace_event <- trace_event[
    trace_event$event_type == "dsprrr:trace",
    ,
    drop = FALSE
  ]

  expect_identical(actual$trajectory_id, expected$trajectory_id)
  expect_identical(actual$run_id, expected$run_id)
  expect_identical(actual$source_type, expected$source_type)
  expect_identical(actual$source_id, expected$source_id)
  expect_identical(actual$agent, expected$agent)
  expect_identical(
    actual$metadata[[1L]]$trace_context$research_run_id,
    expected$metadata[[1L]]$trace_context$research_run_id
  )
  expect_identical(
    actual$metadata[[1L]]$trace_context$deputy_run_id,
    expected$metadata[[1L]]$trace_context$deputy_run_id
  )
  expect_identical(
    trace_event$metadata[[1L]]$program_artifact_id,
    expected$metadata[[1L]]$program_artifact_id
  )
  expect_identical(
    trace_event$metadata[[1L]]$trace_context[
      names(expected$metadata[[1L]]$trace_context)
    ],
    expected$metadata[[1L]]$trace_context
  )
})

test_that("generic dispatch leaves exported dsprrr data frames explicit", {
  skip_if_not_installed("ellmer", "0.4.2")

  expect_s3_class(
    rlang::catch_cnd(as_trajectory(dsprrr_trace_fixture())),
    "scans_error_unsupported_source"
  )
})

test_that("generic dispatch does not claim fake dsprrr modules", {
  skip_if_not_installed("dsprrr", "0.0.0.9000")
  skip_if_not_installed("ellmer", "0.4.2")

  fake_module <- new.env(parent = emptyenv())
  fake_module$get_traces <- function() tibble::tibble()
  class(fake_module) <- c("PredictModule", "Module", "R6")
  expect_s3_class(
    rlang::catch_cnd(as_trajectory(fake_module)),
    "scans_error_unsupported_source"
  )
})

test_that("dsprrr source errors identify malformed exported snapshots", {
  skip_if_not_installed("ellmer", "0.4.2")

  expect_snapshot(error = TRUE, as_trajectory_dsprrr(list()))
  expect_snapshot(
    error = TRUE,
    as_trajectory_dsprrr(tibble::tibble(timestamp = Sys.time()))
  )

  traces <- dsprrr_trace_fixture()
  traces$trace_context <- "not a list column"
  expect_snapshot(error = TRUE, as_trajectory_dsprrr(traces))
  expect_snapshot(
    error = TRUE,
    as_trajectory_dsprrr(
      dsprrr_trace_fixture(),
      metadata = list("unnamed")
    )
  )
})

test_that("dsprrr conversion is deterministic and contains no live source", {
  skip_if_not_installed("ellmer", "0.4.2")

  traces <- dsprrr_trace_fixture()
  first <- as_trajectory_dsprrr(traces)
  second <- as_trajectory_dsprrr(traces)
  restored <- unserialize(serialize(first, NULL))
  has_live_source <- function(value) {
    if (is.environment(value) || inherits(value, c("Module", "Chat"))) {
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
