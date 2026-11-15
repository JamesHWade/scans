rebuild_tempest_fixture <- function() {
  # Run with the Tempest and Graft versions from Remotes installed and the
  # matching Tempest source checkout as the argument.
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) {
    cli::cli_abort(
      "Usage: Rscript tools/rebuild-tempest-fixture.R <tempest-checkout>"
    )
  }
  if (
    !file.exists("DESCRIPTION") ||
      !identical(read.dcf("DESCRIPTION", fields = "Package")[[1L]], "scans")
  ) {
    cli::cli_abort("Run this script from the scans repository root.")
  }
  library(tempest)
  library(testthat)
  helpers <- new.env(parent = asNamespace("tempest"))
  invisible(testthat::source_test_helpers(
    file.path(args[[1L]], "tests", "testthat"),
    env = helpers
  ))
  fixture <- helpers$test_promotion_bundle()
  store_path <- tempfile()
  store <- graft::graft_store(store_path, create = TRUE)
  on.exit(unlink(store_path, recursive = TRUE), add = TRUE)
  selection <- tempest_publish_artifact_research(fixture$research, store)
  exact_selection <- graft::graft_read_selection(store, selection)
  accepted <- graft::graft_accept(
    store = store,
    x = exact_selection,
    stream = "research",
    expected = NULL,
    key = "review-1",
    actor = "host",
    reason = "Reviewed",
    purpose = "briefing"
  )
  if (!identical(accepted@selection, selection)) {
    cli::cli_abort("The accepted decision does not retain the exact selection.")
  }
  review <- tempest_trajectory_review(
    fixture$research,
    store = store,
    selection = selection,
    stream = "research",
    decision = accepted@id
  )
  projection <- tempest_trajectory_review_data(review)
  if (
    !identical(projection$knowledge$proposal$selection_id, selection) ||
      !identical(projection$knowledge$acceptance$selection_id, selection) ||
      !identical(projection$knowledge$acceptance$decision_id, accepted@id)
  ) {
    cli::cli_abort(
      "The Tempest review does not retain the exact publication and acceptance."
    )
  }
  saveRDS(review, "tests/testthat/fixtures/tempest-review.rds", version = 3L)
}

rebuild_tempest_fixture()
