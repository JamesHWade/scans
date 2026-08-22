test_that("scans_abort() creates a stable package error", {
  condition <- rlang::catch_cnd(
    scans_abort("Can't scan this trajectory.")
  )

  expect_s3_class(condition, "scans_error")
  expect_match(conditionMessage(condition), "Can't scan this trajectory")
})

test_that("scans_abort() formats cli bullets", {
  expect_snapshot(
    error = TRUE,
    scans_abort(c(
      "Can't scan this trajectory.",
      "x" = "The trajectory has no events.",
      "i" = "Supply at least one message or tool event."
    ))
  )
})
