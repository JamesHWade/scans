commons_trajectory_fixture <- function() {
  testthat::skip_if_not_installed("commons", "0.0.0.9002")
  commons::trajectory_read(testthat::test_path("fixtures", "commons"))
}

commons_legacy_trajectory_fixture <- function() {
  source <- commons_trajectory_fixture()
  for (index in seq_along(source)) {
    conversation <- source[[index]]
    if ("turns" %in% names(conversation)) {
      turns <- conversation[["turns"]]
      attr(turns, "last_active") <- conversation[["last_active"]]
      source[[index]] <- turns
    }
  }
  attr(source, "source") <- list(kind = "local", path = "fixtures/commons")
  source
}
