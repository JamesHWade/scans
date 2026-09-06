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

# Keep mutation fixtures on the current record shape when testing against
# an older Commons installation. Conformance tests use the raw reader above.
commons_record_trajectory_fixture <- function() {
  source <- commons_trajectory_fixture()
  for (i in seq_along(source)) {
    conversation <- source[[i]]
    if (!"turns" %in% names(conversation)) {
      active <- attr(conversation, "last_active", exact = TRUE)
      attr(conversation, "last_active") <- NULL
      source[[i]] <- list(turns = conversation, last_active = active)
    }
  }
  source
}
