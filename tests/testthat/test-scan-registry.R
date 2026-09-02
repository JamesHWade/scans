test_that("the registry lists every scan the detectors emit", {
  registry <- scan_registry()
  expect_setequal(
    registry$scan,
    c(
      "ambiguous_tool_correlation",
      "unresolved_tool_call",
      "unmatched_tool_result",
      "repeated_tool_call",
      "suspicious_tool_loop",
      "event_error",
      "error_chain",
      "turn_error",
      "trajectory_error"
    )
  )
  expect_true(all(registry$severity %in% c("warning", "error")))
  expect_true(all(nzchar(registry$description)))
})

test_that("selecting scans narrows the findings", {
  bundle <- do.call(
    TrajectoryBundle,
    fixture_source(trajectory_fixture("tool_error"))
  )
  all_findings <- scan_trajectories(bundle)
  skip_if(nrow(all_findings) == 0L, "fixture produces no findings")

  chosen <- unique(all_findings$scan)[[1L]]
  narrowed <- scan_trajectories(bundle, scans = chosen)

  expect_true(all(narrowed$scan == chosen))
  expect_equal(nrow(narrowed), sum(all_findings$scan == chosen))
  expect_identical(names(narrowed), names(all_findings))
})

test_that("finding identifiers do not shift with the selection", {
  bundle <- do.call(
    TrajectoryBundle,
    fixture_source(trajectory_fixture("tool_error"))
  )
  all_findings <- scan_trajectories(bundle)
  skip_if(nrow(all_findings) == 0L, "fixture produces no findings")

  chosen <- unique(all_findings$scan)[[1L]]
  narrowed <- scan_trajectories(bundle, scans = chosen)
  expected <- all_findings[all_findings$scan == chosen, , drop = FALSE]

  expect_equal(narrowed$finding_id, expected$finding_id)
})

test_that("selecting no scans returns no findings", {
  bundle <- do.call(
    TrajectoryBundle,
    fixture_source(trajectory_fixture("tool_error"))
  )
  expect_equal(nrow(scan_trajectories(bundle, scans = character())), 0L)
})

test_that("scan selection preserves positional threshold compatibility", {
  bundle <- do.call(
    TrajectoryBundle,
    fixture_source(trajectory_fixture("tool_error"))
  )

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
  bundle <- do.call(
    TrajectoryBundle,
    fixture_source(trajectory_fixture("tool_error"))
  )
  expect_error(
    scan_trajectories(bundle, scans = "not_a_scan"),
    class = "scans_error_scan_selection"
  )
  expect_error(
    scan_trajectories(bundle, scans = 1L),
    class = "scans_error_scan_selection"
  )
})
