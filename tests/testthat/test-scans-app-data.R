test_that("trajectory titles end at the first question mark", {
  bundle <- trajectory_fixture("simple_exchange")
  prompts <- c(
    "Where is my order? Please check the tracking page.",
    "Where is my order? Can it arrive tomorrow?",
    "  Where is **my order**?\nPlease check. ",
    "Please check my order."
  )
  expected <- c(rep("Where is my order?", 3L), "Please check my order.")

  for (i in seq_along(prompts)) {
    bundle@events$text[[1L]] <- prompts[[i]]
    expect_identical(scans_app_data(bundle)$records$title, expected[[i]])
  }
})
