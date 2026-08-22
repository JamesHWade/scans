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

test_that("adapter conformance compares deterministic canonical output", {
  fixtures <- trajectory_fixtures()

  for (name in names(fixtures)) {
    expected <- fixtures[[name]]
    source <- fixture_source(expected)
    actual <- expect_adapter_conforms(
      source,
      expected,
      adapter = fixture_adapter
    )

    expect_s7_class(actual, TrajectoryBundle)
  }
})

test_that("adapter conformance ignores physical row order", {
  expected <- trajectory_fixture("multiple_tools")
  source <- fixture_source(expected)
  source$turns <- source$turns[rev(seq_len(nrow(source$turns))), ]
  source$events <- source$events[rev(seq_len(nrow(source$events))), ]

  expect_no_error(
    expect_adapter_conforms(source, expected, adapter = fixture_adapter)
  )
})

test_that("malformed fixtures fail with stable scans conditions", {
  fixtures <- malformed_trajectory_fixtures()

  for (name in names(fixtures)) {
    fixture <- fixtures[[name]]
    condition <- expect_adapter_rejects(
      fixture$source,
      fixture_adapter,
      fixture$condition
    )

    expect_match(
      conditionMessage(condition),
      "TrajectoryBundle|trajectories|events",
      info = paste("fixture:", name)
    )
  }
})
