test_that("a selected public capture retains its analysis and evidence references", {
  bundle <- investigation_bundle_fixture()
  result <- assess_trajectory_scans(bundle)
  ids <- c("otel/retry", "otel/limited-capture")
  saved <- investigation_snapshot(
    bundle,
    ids,
    application = "Support assistant",
    source = list(read_info = list(incomplete = TRUE, truncated = TRUE)),
    view = list(selected_trajectory_id = "otel/retry", tab = "trajectory")
  )
  expect_identical(trajectory_info(saved$bundle)$trajectory_id, ids)
  expect_identical(
    saved$analysis$findings,
    result$findings[result$findings$trajectory_id %in% ids, ]
  )
  expect_identical(
    saved$analysis$measures,
    measure_trajectories(bundle)[
      measure_trajectories(bundle)$trajectory_id %in% ids,
    ]
  )
  expect_in("assessed_with_findings", saved$analysis$assessments$status)
  expect_in("insufficient_evidence", saved$analysis$assessments$status)
  expect_identical(saved$manifest$content_policy$omitted_trajectories, 2L)
  expect_identical(
    saved$manifest$source$read_info,
    list(incomplete = TRUE, truncated = TRUE)
  )
  expect_identical(
    saved$analysis$assessments$loss_rows,
    rep(list(integer()), nrow(saved$analysis$assessments))
  )
  path <- tempfile(fileext = ".json")
  write_investigation(saved, path)
  expect_identical(read_investigation(path), saved)
})

test_that("snapshot and revision identifiers distinguish evidence from view and settings", {
  bundle <- investigation_bundle_fixture()
  ids <- c("otel/retry", "otel/limited-capture")
  first <- investigation_snapshot(bundle, ids)
  same <- investigation_snapshot(bundle, rev(ids))
  expect_identical(same$manifest$snapshot_id, first$manifest$snapshot_id)
  expect_identical(same$manifest$revision_id, first$manifest$revision_id)
  view <- investigation_snapshot(bundle, ids, view = list(sort = "findings"))
  settings <- investigation_snapshot(bundle, ids, repeat_threshold = 4L)
  expect_identical(view$manifest$snapshot_id, first$manifest$snapshot_id)
  expect_identical(settings$manifest$snapshot_id, first$manifest$snapshot_id)
  expect_length(
    unique(c(
      first$manifest$revision_id,
      view$manifest$revision_id,
      settings$manifest$revision_id
    )),
    3L
  )
  events <- trajectory_events(bundle)
  events$text[[1L]] <- "Changed evidence"
  bundle@events <- events
  changed <- investigation_snapshot(bundle, ids, previous = first)
  expect_identical(
    changed$manifest$parent_revision_id,
    first$manifest$revision_id
  )
  expect_length(
    unique(c(changed$manifest$snapshot_id, first$manifest$snapshot_id)),
    2L
  )
})

test_that("selection excludes other records and preserves adapter redactions", {
  bundle <- investigation_bundle_fixture()
  events <- trajectory_events(bundle)
  events$text[
    events$trajectory_id == "otel/parallel"
  ] <- "UNSELECTED_PRIVATE_CONTENT"
  bundle@events <- events
  saved <- investigation_snapshot(bundle, "otel/retry")
  path <- tempfile()
  write_investigation(saved, path)
  expect_no_match(
    paste(readLines(path), collapse = " "),
    "UNSELECTED_PRIVATE_CONTENT"
  )
  expect_setequal(
    trajectory_events(read_investigation(path)$bundle)$trajectory_id,
    "otel/retry"
  )

  skip_if_not_installed("ellmer")
  request <- ellmer::ContentToolRequest(
    "call-1",
    "lookup",
    arguments = list(api_key = "PRIVATE_CREDENTIAL", query = "orders")
  )
  bundle <- as_trajectory_ellmer(list(ellmer::AssistantTurn(list(request))))
  saved <- investigation_snapshot(bundle)
  path <- tempfile()
  write_investigation(saved, path)
  expect_no_match(paste(readLines(path), collapse = " "), "PRIVATE_CREDENTIAL")
  expect_identical(
    trajectory_events(read_investigation(path)$bundle)$value[[1]]$api_key,
    "<redacted>"
  )
  expect_in("redacted", trajectory_losses(saved$bundle)$reason)
})

test_that("omitted parents remain explicit without importing their records", {
  bundle <- TrajectoryBundle(
    data.frame(
      trajectory_id = c("parent", "child"),
      parent_trajectory_id = c(NA, "parent"),
      source_type = "manual",
      status = "completed"
    ),
    NULL,
    NULL
  )
  saved <- investigation_snapshot(bundle, "child")
  expect_identical(trajectory_info(saved$bundle)$trajectory_id, "child")
  expect_identical(
    trajectory_info(saved$bundle)$parent_trajectory_id,
    NA_character_
  )
  loss <- trajectory_losses(saved$bundle)
  expect_identical(loss$reason, "scans:selection_omission")
  expect_identical(loss$metadata[[1]]$parent_trajectory_id, "parent")
  expect_identical(saved$analysis$assessments$loss_rows, rep(list(1L), 9))
  path <- tempfile()
  write_investigation(saved, path)
  expect_identical(read_investigation(path), saved)
})

test_that("empty selections and no selected scanners round trip", {
  bundle <- investigation_bundle_fixture()
  for (saved in list(
    investigation_snapshot(bundle, character()),
    investigation_snapshot(bundle, scans = character())
  )) {
    path <- tempfile()
    write_investigation(saved, path)
    expect_identical(read_investigation(path), saved)
    expect_identical(nrow(saved$analysis$assessments), 0L)
  }
})

test_that("invalid selection and edited analysis cannot be written", {
  bundle <- investigation_bundle_fixture()
  expect_snapshot(error = TRUE, investigation_snapshot(bundle, "missing"))
  expect_snapshot(
    error = TRUE,
    investigation_snapshot(bundle, c("otel/retry", "otel/retry"))
  )
  saved <- investigation_snapshot(bundle)
  saved$analysis$findings$event_ids[[1L]] <- "missing-event"
  expect_snapshot(error = TRUE, write_investigation(saved, tempfile()))
})

test_that("selection remaps scoped losses while retaining capture-wide limits", {
  bundle <- investigation_bundle_fixture()
  bundle@losses <- tibble::tibble(
    trajectory_id = c("otel/parallel", "otel/retry", NA_character_),
    turn_id = NA_character_,
    event_id = NA_character_,
    field = "metadata",
    reason = "unsupported",
    detail = c("Excluded loss", "Retained loss", "Capture-wide limit"),
    metadata = rep(list(list()), 3)
  )
  saved <- investigation_snapshot(bundle, "otel/retry")
  expect_identical(
    trajectory_losses(saved$bundle)$detail,
    c("Retained loss", "Capture-wide limit")
  )
  expect_identical(saved$analysis$assessments$loss_rows, rep(list(1:2), 9))
  path <- tempfile()
  write_investigation(saved, path)
  expect_identical(read_investigation(path), saved)
})

test_that("orphan findings fail before hashing", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  i <- which(lengths(saved$analysis$assessments$finding_ids) > 1L)[1L]
  saved$analysis$assessments$finding_ids[[i]] <-
    saved$analysis$assessments$finding_ids[[i]][1L]
  expect_snapshot(error = TRUE, write_investigation(saved, tempfile()))
})


test_that("resaving a captured subset preserves prior omission counts", {
  bundle <- investigation_bundle_fixture()
  first <- investigation_snapshot(
    bundle,
    c("otel/retry", "otel/limited-capture")
  )
  path <- tempfile()
  write_investigation(first, path)
  opened <- read_investigation(path)
  same <- investigation_snapshot(opened$bundle, previous = opened)
  expect_identical(same$manifest$content_policy$omitted_trajectories, 2L)
  smaller <- investigation_snapshot(
    opened$bundle,
    "otel/retry",
    previous = opened
  )
  expect_identical(smaller$manifest$content_policy$omitted_trajectories, 3L)
  restored <- investigation_snapshot(bundle, previous = opened)
  expect_identical(restored$manifest$content_policy$omitted_trajectories, 0L)
  for (invalid in list(NULL, NA_integer_, Inf, -1L, 0.5, "2", c(1L, 2L))) {
    malformed <- opened
    malformed$manifest$content_policy$omitted_trajectories <- invalid
    expect_error(
      investigation_validate(malformed),
      class = "scans_error_investigation"
    )
  }
})
