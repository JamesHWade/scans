commons_trajectory_fixture <- function() {
  testthat::skip_if_not_installed("commons", "0.0.0.9002")
  commons::trajectory_read(testthat::test_path("fixtures", "commons"))
}
