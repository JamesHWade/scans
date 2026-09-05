tempest_review_fixture <- local({
  review <- NULL

  function() {
    testthat::skip_if_not_installed("tempest")
    if (is.null(review)) {
      review <<- readRDS(testthat::test_path(
        "fixtures",
        "tempest-review.rds"
      ))
    }
    unserialize(serialize(review, NULL, version = 3L))
  }
})

tempest_incomplete_collection <- function(collection, omitted = 1L) {
  collection$total <- as.integer(collection$retained + omitted)
  collection$omitted <- as.integer(omitted)
  collection
}
