tempest_review_fixture <- function() {
  testthat::skip_if_not_installed("tempest")
  testthat::skip_if(
    !tempest_review_accessor_available(),
    "Tempest does not export tempest_trajectory_review_data()."
  )
  readRDS(testthat::test_path("fixtures", "tempest-review.rds"))
}

tempest_incomplete_collection <- function(collection, omitted = 1L) {
  collection$total <- as.integer(collection$retained + omitted)
  collection$omitted <- as.integer(omitted)
  collection
}
