test_that("TrajectoryBundle() constructs canonical tibble properties", {
  bundle <- minimal_trajectory_bundle()

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(S7::prop(bundle, "schema_version"), 1L)
  expect_s3_class(trajectory_info(bundle), "tbl_df")
  expect_s3_class(trajectory_turns(bundle), "tbl_df")
  expect_s3_class(trajectory_events(bundle), "tbl_df")
  expect_s3_class(trajectory_evaluations(bundle), "tbl_df")
  expect_s3_class(trajectory_losses(bundle), "tbl_df")
  expect_identical(nrow(trajectory_evaluations(bundle)), 0L)
  expect_identical(nrow(trajectory_losses(bundle)), 0L)
})

test_that("TrajectoryBundle() fills optional columns and preserves extras", {
  tables <- minimal_trajectory_tables()
  tables$trajectories$adapter_note <- "kept"

  bundle <- TrajectoryBundle(
    tables$trajectories,
    tables$turns,
    tables$events
  )

  info <- trajectory_info(bundle)
  expect_named(
    info,
    c(
      "trajectory_id",
      "run_id",
      "parent_trajectory_id",
      "source_type",
      "source_id",
      "source_uri",
      "task_id",
      "sample_id",
      "epoch",
      "agent",
      "model",
      "started_at",
      "completed_at",
      "status",
      "error",
      "metadata",
      "adapter_note"
    )
  )
  expect_identical(info$adapter_note, "kept")
  expect_identical(info$metadata, list(list()))
})

test_that("TrajectoryBundle() accepts empty semantic tables", {
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "trajectory-1",
      source_type = "manual"
    ),
    tibble::tibble(),
    tibble::tibble()
  )

  expect_identical(nrow(trajectory_turns(bundle)), 0L)
  expect_identical(nrow(trajectory_events(bundle)), 0L)
})

test_that("TrajectoryBundle() retains evaluations and audit losses", {
  tables <- minimal_trajectory_tables()
  evaluations <- tibble::tibble(
    trajectory_id = "trajectory-1",
    evaluation_id = "evaluation-1",
    scorer = "accuracy",
    value = list(0.5),
    target = list(1)
  )
  losses <- tibble::tibble(
    trajectory_id = "trajectory-1",
    turn_id = "turn-1",
    event_id = "event-1",
    field = "provider_json",
    reason = "redacted",
    detail = "Credentials removed"
  )

  bundle <- TrajectoryBundle(
    tables$trajectories,
    tables$turns,
    tables$events,
    evaluations,
    losses
  )

  expect_identical(trajectory_evaluations(bundle)$value, list(0.5))
  expect_identical(trajectory_evaluations(bundle)$target, list(1))
  expect_identical(trajectory_losses(bundle)$reason, "redacted")
})

test_that("TrajectoryBundle() retains namespaced source vocabularies", {
  tables <- minimal_trajectory_tables()
  tables$turns$role <- "tempest:researcher"
  tables$events$event_type <- "tempest:delegation"
  tables$events$content_type <- "ellmer:content_foo"

  bundle <- TrajectoryBundle(
    tables$trajectories,
    tables$turns,
    tables$events
  )

  expect_identical(trajectory_turns(bundle)$role, "tempest:researcher")
  expect_identical(
    trajectory_events(bundle)$event_type,
    "tempest:delegation"
  )
  expect_identical(
    trajectory_events(bundle)$content_type,
    "ellmer:content_foo"
  )
})

test_that("TrajectoryBundle() reports malformed source tables", {
  condition <- rlang::catch_cnd(
    TrajectoryBundle(
      tibble::tibble(source_type = "manual"),
      tibble::tibble(),
      tibble::tibble()
    )
  )
  expect_s3_class(condition, "scans_error_trajectory_schema")

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tibble::tibble(source_type = "manual"),
      tibble::tibble(),
      tibble::tibble()
    )
  )

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tibble::tibble(
        trajectory_id = "trajectory-1",
        source_type = "manual"
      ),
      tibble::tibble(
        trajectory_id = "trajectory-1",
        turn_id = "turn-1",
        turn_index = 1.5,
        role = "user"
      ),
      tibble::tibble()
    )
  )
})

test_that("TrajectoryBundle() validates identities and references", {
  tables <- minimal_trajectory_tables()
  tables$events$trajectory_id <- "missing"

  condition <- rlang::catch_cnd(
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )
  expect_s3_class(condition, "scans_error_trajectory_validation")

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )

  tables <- minimal_trajectory_tables()
  tables$events <- tables$events[c(1L, 1L), ]

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )
})

test_that("TrajectoryBundle() validates parent cycles", {
  tables <- minimal_trajectory_tables()
  tables$trajectories <- tibble::tibble(
    trajectory_id = c("trajectory-1", "trajectory-2"),
    parent_trajectory_id = c("trajectory-2", "trajectory-1"),
    source_type = "manual"
  )

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tibble::tibble(),
      tibble::tibble()
    )
  )
})

test_that("TrajectoryBundle() validates order and temporal bounds", {
  tables <- minimal_trajectory_tables()
  tables$turns$turn_index <- 0L
  tables$turns$duration <- Inf
  tables$events$turn_id <- NA_character_
  tables$trajectories$started_at <- as.POSIXct(
    "2026-08-22 12:00:00",
    tz = "UTC"
  )
  tables$trajectories$completed_at <- as.POSIXct(
    "2026-08-22 11:00:00",
    tz = "UTC"
  )

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )
})

test_that("TrajectoryBundle() validates loss references", {
  tables <- minimal_trajectory_tables()
  losses <- tibble::tibble(
    trajectory_id = "trajectory-1",
    turn_id = "missing",
    field = "provider_json",
    reason = "redacted",
    detail = "Credentials removed"
  )

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events,
      losses = losses
    )
  )
})

test_that("TrajectoryBundle() validates vocabularies and metadata", {
  tables <- minimal_trajectory_tables()
  tables$events$event_type <- "unknown"

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )

  tables <- minimal_trajectory_tables()
  tables$events$metadata <- list(list("unnamed"))

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )

  tables <- minimal_trajectory_tables()
  tables$events$metadata <- list(stats::setNames(
    list("first", "second"),
    c("kind", "kind")
  ))

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )
})

test_that("S7 property replacement preserves relational validity", {
  bundle <- minimal_trajectory_bundle()
  events <- trajectory_events(bundle)
  events$trajectory_id <- "missing"

  expect_snapshot(
    error = TRUE,
    S7::prop(bundle, "events") <- events
  )
})

test_that("S7 property replacement preserves schema version", {
  bundle <- minimal_trajectory_bundle()
  expect_snapshot(
    error = TRUE,
    S7::prop(bundle, "schema_version") <- 2L
  )
})

test_that("as_trajectory() has identity and unsupported-source methods", {
  bundle <- minimal_trajectory_bundle()

  expect_identical(as_trajectory(bundle), bundle)
  condition <- rlang::catch_cnd(as_trajectory(1))
  expect_s3_class(condition, "scans_error_unsupported_source")
  expect_snapshot(error = TRUE, as_trajectory(1))
})

test_that("trajectory accessors reject other objects", {
  condition <- rlang::catch_cnd(trajectory_events(list()))
  expect_s3_class(condition, "scans_error_trajectory_type")
  expect_snapshot(error = TRUE, trajectory_events(list()))
})

test_that("TrajectoryBundle survives R serialization", {
  bundle <- minimal_trajectory_bundle()
  restored <- unserialize(serialize(bundle, NULL))

  expect_s7_class(restored, TrajectoryBundle)
  expect_identical(S7::props(restored), S7::props(bundle))
})

test_that("TrajectoryBundle rejects live and unbounded payloads", {
  tables <- minimal_trajectory_tables()
  tables$events$value <- list(new.env(parent = emptyenv()))

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )

  tables <- minimal_trajectory_tables()
  tables$events$text <- strrep("x", 65537L)

  expect_snapshot(
    error = TRUE,
    TrajectoryBundle(
      tables$trajectories,
      tables$turns,
      tables$events
    )
  )
})

test_that("TrajectoryBundle print is compact", {
  expect_snapshot(minimal_trajectory_bundle())
})
