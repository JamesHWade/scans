test_that("fixture corpus covers the adapter contract scenarios", {
  fixtures <- trajectory_fixtures()

  expect_named(
    fixtures,
    c(
      "simple_exchange",
      "multiple_tools",
      "tool_error",
      "repeated_tools",
      "delegated_agent",
      "ellmerverse_correlation",
      "missing_data",
      "evaluated"
    )
  )
  expect_all_true(vapply(fixtures, is_trajectory_bundle, logical(1)))
  expect_setequal(
    unlist(lapply(fixtures, \(x) trajectory_losses(x)$reason)),
    c("unsupported", "redacted", "truncated", "externalized")
  )
})

test_that("ellmerverse fixture correlates sources without conflating them", {
  bundle <- trajectory_fixture("ellmerverse_correlation")
  info <- trajectory_info(bundle)
  metadata <- stats::setNames(info$metadata, info$source_type)

  expect_identical(
    info$source_type,
    c("tempest", "deputy", "dsprrr", "ellmer")
  )
  expect_identical(info$parent_trajectory_id, rep(NA_character_, 4L))

  deputy_run_id <- info$run_id[info$source_type == "deputy"]
  expect_identical(metadata$tempest$deputy_run_id, deputy_run_id)
  expect_identical(metadata$dsprrr$trace_context$deputy_run_id, deputy_run_id)
  expect_identical(metadata$ellmer$deputy_run_id, deputy_run_id)

  program_id <- info$source_id[info$source_type == "dsprrr"]
  expect_identical(metadata$tempest$program_artifact_id, program_id)
  expect_identical(metadata$deputy$run_context$program_artifact_id, program_id)

  events <- trajectory_events(bundle)
  tool_calls <- events$call_id[events$event_type == "tool_call"]
  expect_identical(tool_calls, rep("tool-call-001", 2L))
})

test_that("fixture corpus exercises every canonical column", {
  fixtures <- trajectory_fixtures()
  schemas <- trajectory_table_schemas()

  for (table in names(schemas)) {
    tables <- lapply(fixtures, \(x) S7::prop(x, table))
    for (column in names(schemas[[table]])) {
      values <- lapply(tables, \(x) x[[column]])
      expect_gt(
        sum(vapply(values, fixture_column_has_value, logical(1))),
        0L,
        label = paste("canonical column:", table, column)
      )
    }
  }
})

test_that("malformed fixtures fail with stable scans conditions", {
  fixtures <- malformed_trajectory_fixtures()

  for (name in names(fixtures)) {
    fixture <- fixtures[[name]]
    expect_adapter_rejects(
      fixture$source,
      fixture_adapter,
      fixture$condition
    )
  }
})

test_that("adapters share one trajectory status vocabulary", {
  expect_identical(trajectory_canonical_status("succeeded"), "completed")
  expect_identical(trajectory_canonical_status("provider_error"), "failed")
  expect_identical(trajectory_canonical_status("cancelled"), "cancelled")
  for (reason in c(
    "request_limit",
    "tool_call_limit",
    "cost_limit",
    "hook_requested_stop",
    "interrupted"
  )) {
    expect_identical(
      trajectory_canonical_status(reason),
      "interrupted",
      info = reason
    )
  }
  expect_identical(trajectory_canonical_status(NA_character_), "interrupted")
})
