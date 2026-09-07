test_that("reviews bind to selected evidence and retain exact origin", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  finding <- saved$analysis$findings$finding_id[[1L]]
  case <- review_case(saved, "otel/retry", finding)
  expect_identical(case$origin$revision_id, saved$manifest$revision_id)
  expect_identical(
    review_case(saved, "otel/retry", finding)$case_id,
    case$case_id
  )
  expect_identical(
    trajectory_info(review_evidence(case$evidence)$bundle)$trajectory_id,
    "otel/retry"
  )
  expect_identical(review_status(case)$status, "unreviewed")
  expect_error(
    review_case(saved, "otel/parallel", finding),
    class = "scans_error_review"
  )
  case$trajectory_id <- "otel/parallel"
  expect_error(review_status(case), class = "scans_error_review")
})

test_that("independent decisions require explicit adjudication", {
  case <- review_case(
    investigation_snapshot(investigation_bundle_fixture()),
    "otel/retry"
  )
  first <- review_decision(
    case,
    "confirmed",
    "A",
    "Repeated calls",
    "Bound retries"
  )
  second <- review_decision(first, "uncertain", "B", "Capture is incomplete")
  expect_identical(second$decisions[[1L]], first$decisions[[1L]])
  expect_identical(review_status(second)$status, "disputed")
  final <- review_decision(
    second,
    "confirmed",
    "C",
    "Checked evidence",
    "Bound retries",
    supersedes = review_status(second)$decision_ids
  )
  expect_identical(review_status(final)$status, "confirmed")
  expect_length(final$decisions, 3L)
  expect_error(
    review_decision(
      final,
      "rejected",
      "D",
      "No",
      supersedes = first$decisions[[1L]]$decision_id
    ),
    class = "scans_error_review"
  )
  final$decisions[[1L]]$rationale <- "edited"
  expect_error(review_status(final), class = "scans_error_review")
})

test_that("missed findings and uncertain capture remain explicit", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  case <- review_case(saved, "otel/parallel", scan = "suspicious_tool_loop")
  case <- review_decision(
    case,
    "missed",
    "A",
    "A different repeated pattern",
    "No repeated calls"
  )
  expect_identical(review_status(case)$status, "missed")
  case <- review_decision(
    case,
    "insufficient_capture",
    "B",
    "Missing input",
    supersedes = review_status(case)$decision_ids
  )
  expect_identical(review_status(case)$status, "insufficient_capture")
  expect_error(
    review_case(
      saved,
      "otel/parallel",
      event_ids = trajectory_events(saved$bundle)$event_id[[1L]]
    ),
    class = "scans_error_review"
  )
})
