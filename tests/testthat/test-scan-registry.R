test_that("the registry advertises built-in names and severities", {
  registry <- scan_registry()
  expected <- c(
    ambiguous_tool_correlation = "warning",
    unresolved_tool_call = "warning",
    unmatched_tool_result = "warning",
    repeated_tool_call = "warning",
    suspicious_tool_loop = "warning",
    event_error = "error",
    error_chain = "error",
    turn_error = "error",
    trajectory_error = "error"
  )

  expect_setequal(registry$scan, names(expected))
  expect_length(registry$scan, length(expected))
  expect_identical(
    stats::setNames(registry$severity, registry$scan)[names(expected)],
    expected
  )
  expect_all_true(nzchar(registry$description))
})

test_that("selecting event errors retains both exact evidence records", {
  bundle <- trajectory_fixture("tool_error")
  findings <- scan_trajectories(bundle, scans = "event_error")

  expect_identical(findings$scan, rep("event_error", 2L))
  expect_identical(findings$trajectory_id, rep("trajectory-error", 2L))
  expect_identical(findings$event_id, c("error-event-3", "error-event-4"))
  expect_identical(findings$event_ids, list("error-event-3", "error-event-4"))
  expect_identical(findings$turn_id, rep("error-turn-3", 2L))
})

test_that("finding identifiers do not shift with the selection", {
  bundle <- trajectory_fixture("tool_error")
  all_findings <- scan_trajectories(bundle)
  narrowed <- scan_trajectories(bundle, scans = "event_error")

  expect_identical(
    all_findings$scan,
    c("trajectory_error", "turn_error", "event_error", "event_error")
  )
  expect_identical(
    narrowed$finding_id,
    c("scan-000001/finding-000003", "scan-000001/finding-000004")
  )
  expect_identical(narrowed, all_findings[3:4, ])
})

test_that("selecting no scans returns no findings", {
  bundle <- trajectory_fixture("tool_error")
  expect_equal(nrow(scan_trajectories(bundle, scans = character())), 0L)
})

test_that("scan selection preserves positional threshold compatibility", {
  bundle <- trajectory_fixture("tool_error")

  positional <- scan_trajectories(bundle, "positional", 4L, 5L)
  named <- scan_trajectories(
    bundle,
    scan_id = "positional",
    repeat_threshold = 4L,
    loop_threshold = 5L
  )

  expect_identical(positional, named)
})

test_that("an unknown scan name is refused", {
  bundle <- trajectory_fixture("tool_error")
  expect_error(
    scan_trajectories(bundle, scans = "not_a_scan"),
    class = "scans_error_scan_selection"
  )
  expect_error(
    scan_trajectories(bundle, scans = 1L),
    class = "scans_error_scan_selection"
  )
})
