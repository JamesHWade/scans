test_that("package metadata describes trajectory analysis", {
  description <- utils::packageDescription("scans")

  expect_identical(description$Package, "scans")
  expect_match(description$Description, "agent", ignore.case = TRUE)
  expect_match(description$Description, "trajectories", ignore.case = TRUE)
})
