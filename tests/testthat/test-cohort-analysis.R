test_that("tool calls pair results by call id and keep unclaimed failures", {
  data <- cohort_fixture_data()
  calls <- cohort_tool_calls(data, data$info$trajectory_id)

  expect_identical(calls$event_id, c("a-1", "a-3", "c-1", "c-2", "d-1", "d-3"))
  expect_identical(
    calls$tool,
    c("lookup", "lookup", "refund", "refund")[c(
      1,
      2,
      3,
      4,
      1,
      1
    )]
  )
  expect_identical(calls$failed, c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(calls$error, c(NA, "timeout", NA, "late", NA, NA))
  expect_identical(calls$duration, c(1, 4, NA, NA, 2, 3))
  expect_identical(cohort_tool_calls(data, "d")$call_id, c("d-1", "d-2"))
  expect_identical(nrow(cohort_tool_calls(data, c("b", "e"))), 0L)
})

test_that("the browser's tool error count is the count of failed calls", {
  data <- cohort_fixture_data()
  calls <- cohort_tool_calls(data, data$info$trajectory_id)
  expect_identical(data$records$n_tool_errors, c(1L, 0L, 2L, 0L, 0L))
  expect_identical(sum(data$records$n_tool_errors), sum(calls$failed))
})

test_that("a reused call id pairs each result with one call", {
  reused <- function(types, errors) {
    n <- length(types)
    bundle <- TrajectoryBundle(
      tibble::tibble(trajectory_id = "r", source_type = "manual"),
      tibble::tibble(
        trajectory_id = "r",
        turn_id = "r-turn",
        turn_index = 1L,
        role = "assistant"
      ),
      tibble::tibble(
        trajectory_id = "r",
        event_id = paste0("r-", seq_len(n)),
        event_index = seq_len(n),
        turn_id = "r-turn",
        event_type = types,
        name = "lookup",
        call_id = "again",
        error = errors
      )
    )
    scans_app_data(bundle)
  }

  one_result <- reused(
    c("tool_call", "tool_call", "tool_result"),
    c(NA, NA, "timeout")
  )
  calls <- cohort_tool_calls(one_result, "r")
  expect_identical(calls$event_id, c("r-1", "r-2"))
  expect_identical(calls$failed, c(TRUE, FALSE))
  expect_identical(one_result$records$n_tool_errors, 1L)
  expect_identical(cohort_tools(one_result, "r")$failed, 1L)

  two_results <- reused(
    c("tool_call", "tool_result", "tool_call", "tool_result"),
    c(NA, NA, NA, "timeout")
  )
  calls <- cohort_tool_calls(two_results, "r")
  expect_identical(calls$event_id, c("r-1", "r-3"))
  expect_identical(calls$failed, c(FALSE, TRUE))
  expect_identical(calls$error, c(NA, "timeout"))

  early_result <- reused(
    c("tool_result", "tool_call", "tool_call", "tool_result"),
    c("late", NA, NA, NA)
  )
  calls <- cohort_tool_calls(early_result, "r")
  expect_identical(calls$event_id, paste0("r-", 1:3))
  expect_identical(calls$failed, c(TRUE, FALSE, FALSE))

  before_its_call <- reused(c("tool_result", "tool_call"), c("late", NA))
  calls <- cohort_tool_calls(before_its_call, "r")
  expect_identical(calls$event_id, c("r-1", "r-2"))
  expect_identical(calls$failed, c(TRUE, FALSE))
})

test_that("tools rank by failures and keep untimed calls out of durations", {
  data <- cohort_fixture_data()
  tools <- cohort_tools(data, data$info$trajectory_id)

  expect_identical(tools$tool, c("refund", "lookup"))
  expect_identical(tools$calls, c(2L, 4L))
  expect_identical(tools$failed, c(2L, 1L))
  expect_identical(tools$failure_rate, c(1, 0.25))
  expect_identical(tools$trajectories, c(1L, 2L))
  expect_identical(tools$trajectories_failed, c(1L, 1L))
  expect_identical(tools$top_error, c("late", "timeout"))
  expect_identical(tools$top_error_n, c(1L, 1L))
  expect_identical(tools$n_timed, c(0L, 4L))
  expect_identical(tools$median_duration, c(NA, 2.5))
  expect_equal(tools$p95_duration, c(NA, 3.85))
  expect_identical(nrow(cohort_tools(data, "e")), 0L)
})

test_that("activity keeps empty buckets and partitions each bucket", {
  data <- cohort_fixture_data()
  ids <- data$info$trajectory_id
  activity <- cohort_activity(data, ids)

  expect_identical(attr(activity, "unit"), "day")
  expect_identical(attr(activity, "n_undated"), 1L)
  expect_identical(
    activity$bucket,
    as.POSIXct(c("2026-10-01", "2026-10-02", "2026-10-03"), tz = "UTC")
  )
  expect_identical(activity$n, c(2L, 0L, 2L))
  expect_identical(activity$n_error, c(1L, 0L, 1L))
  expect_identical(activity$n_warning, c(0L, 0L, 1L))
  expect_identical(activity$n_clean, c(1L, 0L, 0L))
  expect_identical(activity$n_unassessed, c(0L, 0L, 0L))
  expect_identical(
    activity$n,
    activity$n_error +
      activity$n_warning +
      activity$n_clean +
      activity$n_unassessed
  )

  hourly <- cohort_activity(data, c("a", "b"))
  expect_identical(attr(hourly, "unit"), "hour")
  expect_identical(hourly$n, c(1L, 0L, 0L, 0L, 0L, 1L))
  expect_identical(nrow(cohort_activity(data, ids, unit = "hour")), 50L)

  undated <- cohort_activity(data, "e")
  expect_identical(nrow(undated), 0L)
  expect_identical(attr(undated, "n_undated"), 1L)
})

test_that("a trajectory no scan could assess is not counted as clean", {
  data <- cohort_fixture_data()
  expect_identical(
    cohort_trajectories(data, data$info$trajectory_id)$class,
    c("error", "clean", "error", "warning", "unassessed")
  )
  off <- scans_app_data(
    cohort_fixture(),
    scans_app_scan_config(scans = character())
  )
  activity <- cohort_activity(off, off$info$trajectory_id)
  expect_identical(activity$n_unassessed, activity$n)
  expect_identical(cohort_headline(off, off$info$trajectory_id)$n_assessed, 0L)
})

test_that("patterns count affected trajectories, users, and a bucket series", {
  data <- cohort_fixture_data()
  ids <- data$info$trajectory_id
  patterns <- cohort_patterns(data, ids)

  expect_identical(
    patterns$scan,
    c(
      "event_error",
      "repeated_tool_call",
      "unmatched_tool_result",
      "unresolved_tool_call"
    )
  )
  expect_identical(patterns$severity, c("error", rep("warning", 3L)))
  expect_identical(patterns$trajectories, c(2L, 2L, 1L, 1L))
  expect_identical(patterns$share, c(0.4, 0.4, 0.2, 0.2))
  expect_identical(patterns$findings, c(3L, 2L, 1L, 1L))
  expect_identical(patterns$users, c(2L, 1L, 1L, 1L))
  expect_identical(
    patterns$last_seen[1:2],
    as.POSIXct(c("2026-10-03 10:00:00", "2026-10-03 11:00:00"), tz = "UTC")
  )
  expect_identical(patterns$series[[1L]], c(1L, 0L, 1L))
  expect_identical(patterns$series[[3L]], c(0L, 0L, 1L))
  expect_identical(patterns$evidence[[1L]], "3 events report errors")

  anonymous <- cohort_patterns(data, "d")
  expect_identical(anonymous$users, NA_integer_)
  expect_identical(nrow(cohort_patterns(data, "e")), 0L)
})

test_that("breakdowns label unknown groups and report measure coverage", {
  data <- cohort_fixture_data()
  ids <- data$info$trajectory_id
  models <- cohort_breakdown(data, ids, "model")

  expect_identical(models$value, c("large", "small", "Unknown"))
  expect_identical(models$n, c(2L, 2L, 1L))
  expect_identical(models$n_findings, c(2L, 1L, 0L))
  expect_identical(models$n_assessed, c(2L, 2L, 0L))
  expect_identical(models$n_error, c(1L, 1L, 0L))
  expect_identical(models$tool_calls, c(4L, 2L, 0L))
  expect_identical(models$tool_failed, c(2L, 1L, 0L))
  expect_identical(models$median_elapsed, c(40, 15, NA))
  expect_identical(models$n_timed, c(1L, 2L, 0L))
  expect_identical(models$median_tokens, c(100, 22.5, NA))
  expect_identical(models$n_tokens, c(1L, 2L, 0L))

  users <- cohort_breakdown(data, ids, "user")
  expect_identical(users$value, c("ada", "bo", "Unknown"))
  expect_identical(users$n, c(2L, 1L, 2L))
  expect_identical(users$n_assessed, c(2L, 1L, 1L))
  expect_identical(cohort_breakdown(data, ids, "source_type")$n, 5L)
  expect_error(cohort_breakdown(data, ids, "agent"))
})

test_that("headline facts carry their denominators", {
  data <- cohort_fixture_data()
  headline <- cohort_headline(data, data$info$trajectory_id)

  expect_identical(headline$n, 5L)
  expect_identical(headline$total, 5L)
  expect_identical(headline$n_findings, 3L)
  expect_identical(headline$n_assessed, 4L)
  expect_identical(headline$tool_calls, 6L)
  expect_identical(headline$tool_failed, 3L)
  expect_identical(headline$tool_trajectories_failed, 2L)
  expect_identical(headline$median_elapsed, 20)
  expect_identical(headline$n_timed, 3L)
  expect_identical(headline$median_tokens, 30)
  expect_identical(headline$n_tokens, 3L)
  expect_identical(headline$n_undated, 1L)
  expect_identical(
    headline$last_started,
    as.POSIXct("2026-10-03 11:00:00", tz = "UTC")
  )

  empty <- cohort_headline(data, character())
  expect_identical(empty$n, 0L)
  expect_identical(empty$total, 5L)
  expect_identical(empty$median_elapsed, NA_real_)
  expect_true(is.na(empty$first_started))
})

test_that("the review queue follows its documented ordering rule", {
  data <- cohort_fixture_data()
  ids <- data$info$trajectory_id
  queue <- cohort_review_queue(data, ids)

  # No store: class, then error findings (c has 2, a has 1), then position.
  expect_identical(queue$trajectory_id, c("c", "a", "d", "e", "b"))
  expect_identical(queue$rank, 1:5)
  expect_identical(queue$index, c(3L, 1L, 4L, 5L, 2L))
  expect_identical(
    queue$class,
    c("error", "error", "warning", "unassessed", "clean")
  )
  expect_true(all(is.na(queue$reviewed)))
  expect_true(all(is.na(queue$n_annotations)))

  reviewed <- cohort_review_queue(data, ids, cohort_fixture_annotations())
  expect_identical(reviewed$trajectory_id, c("a", "d", "e", "c", "b"))
  expect_identical(reviewed$reviewed, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(reviewed$label, c(NA, NA, NA, "tool failure", NA))
  expect_identical(reviewed$n_annotations, c(0L, 0L, 0L, 2L, 1L))
  expect_identical(reviewed$reviewed_by, c(NA, NA, NA, "bo", "ada"))

  # A subset keeps the same relative order, and ties break newest first.
  expect_identical(
    cohort_review_queue(data, c("b", "d", "c"))$trajectory_id,
    c("c", "d", "b")
  )
  tied <- data
  tied$records$n_errors[tied$records$trajectory_id == "c"] <- 1L
  tied$records$n_findings[tied$records$trajectory_id %in% c("a", "c")] <- 2L
  expect_identical(
    cohort_review_queue(tied, ids)$trajectory_id[1:2],
    c("c", "a")
  )
})

test_that("queue reasons state recorded facts, most important first", {
  data <- cohort_fixture_data()
  queue <- cohort_review_queue(data, data$info$trajectory_id)
  reasons <- stats::setNames(queue$reasons, queue$trajectory_id)

  expect_identical(
    reasons$c,
    c("2 tool errors", "Call without result", "Result without call")
  )
  expect_identical(reasons$a, c("1 tool error", "Repeated tool call"))
  expect_identical(reasons$d, "Repeated tool call")
  expect_identical(reasons$e, "Not assessed")
  expect_identical(reasons$b, character())

  failed <- data
  failed$info$status[[2L]] <- "failed"
  failed$turns$finish_reason[failed$turns$trajectory_id == "b"] <- "length"
  expect_identical(
    cohort_review_reasons(failed, "b", "clean")[[1L]],
    c("Run failed", "Answer cut off")
  )
  expect_identical(cohort_scan_word("custom_scan", "reason"), "Custom scan")
})

test_that("review progress counts flagged and reviewed without inventing zeros", {
  data <- cohort_fixture_data()
  ids <- data$info$trajectory_id
  progress <- cohort_review_progress(
    cohort_review_queue(data, ids, cohort_fixture_annotations())
  )
  expect_identical(progress$flagged, 3L)
  expect_identical(progress$flagged_reviewed, 1L)
  expect_identical(progress$total, 5L)
  expect_identical(progress$reviewed, 2L)
  expect_identical(
    progress$labels[[1L]],
    c("(no label)" = 1L, "tool failure" = 1L)
  )

  unknown <- cohort_review_progress(cohort_review_queue(data, ids))
  expect_identical(unknown$flagged, 3L)
  expect_identical(unknown$flagged_reviewed, NA_integer_)
  expect_identical(unknown$reviewed, NA_integer_)
})
