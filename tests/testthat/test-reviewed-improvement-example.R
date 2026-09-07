test_that("reviewed cases run through bounded dsprrr optimization and a held-out vitals Task", {
  skip_if_not_installed("dsprrr")
  skip_if_not_installed("vitals")
  env <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "reviewed-improvement.R", package = "scans"),
    env
  )
  result <- env$run_reviewed_improvement()
  expect_length(result$examples$cases, 2L)
  expect_identical(
    result$development$question,
    "Track order A after the service times out"
  )
  expect_identical(
    result$held_out$input[[1L]]$question,
    "Track order B after the service times out"
  )
  expect_identical(
    result$metric(
      "Try again",
      list(answer = "Service unavailable", expectation = "State unavailability")
    ),
    list(score = 0, feedback = "State unavailability")
  )
  expect_equal(result$task$get_samples()$score, 1)
  expect_equal(nrow(trajectory_info(result$traces)), 1L)
  expect_equal(nrow(trajectory_evaluations(result$traces)), 1L)
  expect_gt(result$mocked_requests, 0L)
  expect_lte(result$mocked_requests, 30L)
  reopened <- read_investigation(file.path(
    result$output_dir,
    "held-out-investigation.json"
  ))
  expect_identical(S7::props(reopened$bundle), S7::props(result$traces))
})
