# Run with Tempest installed and its matching source checkout as the argument.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
library(tempest)
library(testthat)
helpers <- new.env(parent = asNamespace("tempest"))
invisible(testthat::source_test_helpers(
  file.path(args[[1L]], "tests", "testthat"),
  env = helpers
))
fixture <- helpers$test_promotion_bundle()
store <- graft::graft_open(tempest_graft_schema(), ":memory:", okf = "disabled")
plan <- tempest_graft_plan(store, fixture$bundle)
commit <- graft::graft_commit(store, plan)
receipt <- tempest_promotion_receipt(store, fixture$bundle, plan, commit)
review <- tempest_trajectory_review(
  fixture$research,
  promotion_bundle = fixture$bundle,
  promotion_receipt = receipt
)
invisible(tempest_trajectory_review_data(review))
saveRDS(review, "tests/testthat/fixtures/tempest-review.rds", version = 3L)
graft::graft_close(store)
