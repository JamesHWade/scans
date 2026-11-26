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
  expect_identical(saved$analysis$summaries$parent_trajectory_id, NA_character_)
  expect_identical(saved$analysis$summaries$trajectory_depth, 0L)
  expect_identical(saved$analysis$summaries$n_losses, 1L)
  expect_identical(
    unique(saved$analysis$measures$parent_trajectory_id),
    NA_character_
  )
  loss <- trajectory_losses(saved$bundle)
  expect_identical(loss$reason, "scans:selection_omission")
  expect_identical(loss$metadata[[1]]$parent_trajectory_id, "parent")
  expect_identical(
    saved$analysis$assessments$loss_rows,
    rep(list(1L), nrow(scan_registry()))
  )
  path <- tempfile()
  write_investigation(saved, path)
  expect_identical(read_investigation(path), saved)
})

test_that("an omitted parent qualifies only its own child's assessments", {
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("lead", "child", "sibling", "other"),
      parent_trajectory_id = c(NA, "lead", "lead", NA),
      source_type = "manual",
      status = "completed"
    ),
    tibble::tibble(
      trajectory_id = c("child", "other"),
      turn_id = c("child-1", "other-1"),
      turn_index = 1L,
      role = "assistant"
    ),
    NULL,
    losses = tibble::tibble(
      turn_id = c("other-1", "child-1", NA),
      field = "usage",
      reason = "unsupported",
      detail = "Usage unavailable"
    )
  )
  saved <- investigation_snapshot(
    bundle,
    c("child", "other"),
    scans = c("trajectory_error", "turn_error")
  )
  losses <- trajectory_losses(saved$bundle)
  owner <- scan_loss_trajectory_ids(
    losses,
    trajectory_turns(saved$bundle),
    trajectory_events(saved$bundle)
  )
  omission <- which(losses$reason == "scans:selection_omission")
  expect_identical(owner[omission], "child")
  assessments <- saved$analysis$assessments
  note <- "The parent trajectory was excluded from this saved selection."
  for (row in seq_len(nrow(assessments))) {
    id <- assessments$trajectory_id[[row]]
    # Its own losses, the capture-wide loss, and for the child the omission.
    expect_setequal(
      assessments$loss_rows[[row]],
      which(is.na(owner) | owner == id)
    )
    expect_identical(
      note %in% assessments$limitations[[row]],
      id == "child"
    )
    expect_identical(anyDuplicated(assessments$limitations[[row]]), 0L)
  }
  expect_identical(
    assessments$loss_rows[assessments$trajectory_id == "child"],
    rep(list(c(2L, 3L, omission)), 2L)
  )
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
  expect_identical(
    saved$analysis$assessments$loss_rows,
    rep(list(1:2), nrow(scan_registry()))
  )
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


test_that("selection updates descendant depths without rerunning saved diagnostics", {
  bundle <- TrajectoryBundle(
    data.frame(
      trajectory_id = c("root", "child", "grandchild"),
      parent_trajectory_id = c(NA, "root", "child"),
      source_type = "manual"
    ),
    NULL,
    NULL
  )
  saved <- investigation_snapshot(bundle)
  local_mocked_bindings(
    summarize_trajectories = function(...) stop("Do not rerun saved summaries"),
    assess_trajectory_scans = function(...) stop("Do not rerun saved scanners")
  )
  revised <- investigation_build(
    saved$bundle,
    c("child", "grandchild"),
    saved$manifest$application,
    saved$manifest$source,
    saved$settings,
    list(),
    saved$analysis,
    previous = saved
  )
  expect_identical(
    revised$analysis$summaries$parent_trajectory_id,
    c(NA, "child")
  )
  expect_identical(revised$analysis$summaries$trajectory_depth, c(0L, 1L))
  expect_identical(revised$analysis$summaries$n_losses, c(1L, 0L))
  attribution <- unique(revised$analysis$measures[c(
    "trajectory_id",
    "parent_trajectory_id"
  )])
  expect_identical(attribution$trajectory_id, c("child", "grandchild"))
  expect_identical(attribution$parent_trajectory_id, c(NA, "child"))
  path <- tempfile()
  write_investigation(revised, path)
  expect_identical(read_investigation(path), revised)
})


test_that("selection maps structural summaries by trajectory identity", {
  bundle <- TrajectoryBundle(
    data.frame(
      trajectory_id = c("root", "child", "grandchild"),
      parent_trajectory_id = c(NA, "root", "child"),
      source_type = "manual"
    ),
    NULL,
    NULL
  )
  saved <- investigation_snapshot(bundle)
  analysis <- saved$analysis
  analysis$summaries <- analysis$summaries[3:1, ]
  revised <- investigation_build(
    bundle,
    c("child", "grandchild"),
    "Trajectories",
    list(),
    saved$settings,
    list(),
    analysis
  )
  expect_identical(
    revised$analysis$summaries$trajectory_id,
    c("grandchild", "child")
  )
  expect_identical(
    revised$analysis$summaries$parent_trajectory_id,
    c("child", NA)
  )
  expect_identical(revised$analysis$summaries$trajectory_depth, c(1L, 0L))
  expect_identical(revised$analysis$summaries$n_losses, c(0L, 1L))
})

test_that("print methods dispatch for callers outside the package", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    application = "Support assistant"
  )
  caller <- list2env(list(x = saved), parent = globalenv())
  printed <- testthat::capture_messages(evalq(print(x), caller))
  expect_match(printed[[1L]], "scans_investigation", fixed = TRUE)
})

test_that("a snapshot fingerprints its content once and still rejects edits", {
  hashed <- new.env(parent = emptyenv())
  hashed$n <- 0L
  hash <- investigation_hash
  local_mocked_bindings(
    investigation_hash = function(x) {
      hashed$n <- hashed$n + 1L
      hash(x)
    }
  )
  saved <- investigation_snapshot(investigation_bundle_fixture())
  # One snapshot identifier and one revision identifier.
  expect_identical(hashed$n, 2L)

  expect_no_error(investigation_validate(saved))
  expect_identical(hashed$n, 4L)
  edited <- saved
  edited$analysis$findings$explanation[[1L]] <- "Edited"
  expect_error(
    investigation_validate(edited),
    class = "scans_error_investigation"
  )
  expect_error(
    write_investigation(edited, tempfile()),
    class = "scans_error_investigation"
  )
  missing <- saved
  missing$manifest$revision_id <- NULL
  expect_error(
    investigation_validate(missing, identifiers = FALSE),
    class = "scans_error_investigation"
  )
})

test_that("the bundled example keeps its snapshot and revision identifiers", {
  skip_if_not_installed("jsonlite")
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    application = "Support assistant example"
  )
  expect_identical(
    saved$manifest$snapshot_id,
    "sha256:a85034486ae5615b770d381220f685816f63135488233f2eb8e8482ea774b015"
  )
  # The revision covers the package version, so pin the one these
  # identifiers were recorded with.
  saved$manifest$package_version <- "0.0.0.9000"
  expect_identical(
    investigation_revision_id(saved),
    "sha256:fee200ae5473d643c5ede96a6528465fdacf6b32ab3e2ac888f6893706b76415"
  )
})

test_that("a build rejects an analysis that does not fit its evidence", {
  skip_if_not_installed("jsonlite")
  bundle <- investigation_bundle_fixture()
  saved <- investigation_snapshot(bundle, application = "Support assistant")
  analysis <- saved$analysis
  analysis$findings$event_id[[1L]] <- "no-such-event"
  expect_error(
    investigation_build(
      bundle,
      NULL,
      "Support assistant",
      list(),
      saved$settings,
      list(),
      analysis
    ),
    class = "scans_error_investigation"
  )
})
