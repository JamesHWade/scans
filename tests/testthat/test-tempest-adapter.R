test_that("Tempest reviews become complete canonical product trajectories", {
  review <- tempest_review_fixture()
  bundle <- as_trajectory_tempest(review)
  info <- trajectory_info(bundle)
  events <- trajectory_events(bundle)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(info$trajectory_id, "tempest/research-run-001")
  expect_identical(info$run_id, "research-run-001")
  expect_identical(info$source_type, "tempest")
  expect_identical(info$source_id, review@review_id)
  expect_identical(info$agent, "storm")
  expect_identical(info$status, "completed")
  expect_identical(
    info$started_at,
    as.POSIXct("2026-08-22 16:00:00", tz = "UTC")
  )
  expect_identical(
    info$completed_at,
    as.POSIXct("2026-08-22 16:00:10", tz = "UTC")
  )
  expect_identical(nrow(trajectory_turns(bundle)), 0L)
  expect_setequal(
    unique(events$event_type),
    c(
      "tempest:stage_succeeded",
      "tempest:deputy_run",
      "tempest:deputy_delegation",
      "tempest:program",
      "tempest:knowledge",
      "tempest:accepted_revision",
      "tempest:evidence",
      "tempest:join",
      "tempest:finding"
    )
  )
})

test_that("Tempest source order and canonical records remain deterministic", {
  review <- tempest_review_fixture()
  first <- as_trajectory_tempest(review)
  second <- as_trajectory_tempest(review)
  events <- trajectory_events(first)
  stages <- events[grepl("^tempest:stage_", events$event_type), ]
  agents <- events[
    events$event_type %in%
      c("tempest:deputy_run", "tempest:deputy_delegation"),
  ]
  source_agents <- tempest::tempest_trajectory_review_data(
    review
  )$agent_runs$items

  expect_identical(stages$name, c("section_writing", "perspectives"))
  expect_identical(
    vapply(agents$metadata, `[[`, character(1), "deputy_run_id"),
    vapply(source_agents, `[[`, character(1), "deputy_run_id")
  )
  expect_identical(S7::props(second), S7::props(first))
  expect_identical(
    S7::props(unserialize(serialize(first, NULL))),
    S7::props(first)
  )
})

test_that("Tempest authority, identities, and findings remain distinct", {
  events <- trajectory_events(as_trajectory_tempest(tempest_review_fixture()))
  joins <- events[events$event_type == "tempest:join", ]
  programs <- events[events$event_type == "tempest:program", ]
  knowledge <- events[events$event_type == "tempest:knowledge", ]
  revisions <- events[events$event_type == "tempest:accepted_revision", ]
  evidence <- events[events$event_type == "tempest:evidence", ]
  finding <- events[events$event_type == "tempest:finding", ]
  stage <- events[events$name == "perspectives", ][1L, ]

  proof_kinds <- vapply(
    joins$value,
    function(join) join$proof$kind,
    character(1)
  )
  expect_setequal(
    proof_kinds,
    c("authority_validated", "exact_identity", "correlation_only")
  )
  expect_setequal(
    joins$name,
    c(
      "contains",
      "executed_as",
      "parent_of",
      "correlated_with",
      "read_from",
      "proposed_as",
      "accepted_as"
    )
  )
  correlation_join <- which(proof_kinds == "correlation_only")[[1L]]
  expect_identical(
    joins$value[[correlation_join]]$proof$matched_fields,
    list("correlation_id")
  )
  expect_identical(nrow(programs), 10L)
  expect_identical(programs$name[[1L]], "perspectives")
  expect_identical(programs$value[[1L]]$stage, "perspectives")
  expect_identical(knowledge$name, "accepted")
  expect_named(
    knowledge$value[[1L]]$acceptance$record_revisions,
    c("total", "retained", "omitted", "digest")
  )
  expect_identical(revisions$value[[1L]]$revision_id, "graft:REVISION001")
  expect_identical(revisions$name, "Claim")
  expect_identical(
    vapply(evidence$value, `[[`, character(1), "record_id"),
    c("claim-001", "span-001")
  )
  expect_setequal(
    finding$name,
    c(
      "exploratory_execution",
      "support_unverified",
      "publication_blocked"
    )
  )
  expect_identical(
    finding$status[[match("support_unverified", finding$name)]],
    "warning"
  )
  expect_true(all(finding$parent_event_id == stage$event_id))
})

test_that("Tempest deliberate omissions and bounded lanes become losses", {
  evidence <- lapply(seq_len(250L), function(index) {
    list(
      record_type = "claim",
      record_id = sprintf("claim-%03d", index)
    )
  })
  review <- tempest_review_fixture(function(payload) {
    payload$evidence <- tempest_fixture_collection(
      evidence,
      "z",
      omitted = 1L
    )
    join <- getFromNamespace("tempest_trajectory_join", "tempest")
    extra_joins <- lapply(2:236, function(index) {
      join(
        "product",
        "research-run-001",
        "contains",
        "claim",
        sprintf("claim-%03d", index),
        "authority_validated",
        c("research_run_id", "record_id")
      )
    })
    payload$joins <- tempest_fixture_collection(
      c(payload$joins$items, extra_joins),
      "x",
      omitted = 1L
    )
    payload
  })
  losses <- trajectory_losses(as_trajectory_tempest(review))
  bounded <- losses[losses$field == "evidence$items", ]

  expect_setequal(
    losses$field[losses$reason == "unsupported"],
    c("turns", "source_content", "runtime")
  )
  expect_identical(bounded$reason, "truncated")
  expect_identical(
    bounded$metadata[[1L]]$complete_digest,
    review@evidence$digest
  )
})

test_that("Tempest stage omissions leave product execution bounds unknown", {
  review <- tempest_review_fixture(function(payload) {
    stages <- lapply(seq_len(250L), function(index) {
      stage <- payload$stages$items[[(index - 1L) %% 2L + 1L]]
      stage$attempt_id <- sprintf("attempt-omitted-%03d", index)
      stage$trace_id <- stage$attempt_id
      stage$started_at <- "2026-08-22T16:00:00.000Z"
      stage$completed_at <- "2026-08-22T16:00:10.000Z"
      stage
    })
    payload$stages <- tempest_fixture_collection(
      stages,
      "z",
      omitted = 1L,
      preserve_order = TRUE
    )
    join <- getFromNamespace("tempest_trajectory_join", "tempest")
    extra_joins <- lapply(seq_len(235L), function(index) {
      join(
        "stage_attempt",
        sprintf("attempt-truncated-%03d", index),
        "contains",
        "output_digest",
        sprintf("output-truncated-%03d", index),
        "exact_identity",
        c("output_reference.kind", "output_reference.ids")
      )
    })
    payload$joins <- tempest_fixture_collection(
      c(payload$joins$items, extra_joins),
      "x",
      omitted = 1L
    )
    payload
  })
  bundle <- as_trajectory_tempest(review)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_true(is.na(info$started_at))
  expect_true(is.na(info$completed_at))
  expect_identical(
    losses$reason[losses$field == "stages$items"],
    "truncated"
  )
})

test_that("Tempest execution bounds require every retained stage time", {
  review <- tempest_review_fixture()
  projection <- tempest::tempest_trajectory_review_data(review)
  projection$stages$items[[2L]]$completed_at <- NULL
  testthat::local_mocked_bindings(
    tempest_review_snapshot = function(x, call) projection
  )

  incomplete <- trajectory_info(as_trajectory_tempest(review))
  expect_false(is.na(incomplete$started_at))
  expect_true(is.na(incomplete$completed_at))

  projection$stages$items[[1L]]$started_at <- "<redacted>"
  redacted <- trajectory_info(as_trajectory_tempest(review))
  expect_true(is.na(redacted$started_at))
  expect_true(is.na(redacted$completed_at))
})

test_that("Tempest caller metadata and source locators are sanitized", {
  bundle <- as_trajectory_tempest(
    tempest_review_fixture(),
    trajectory_id = "tempest-custom",
    source_uri = "https://user:secret@example.com/review?token=secret",
    metadata = list(api_key = "secret", batch = "batch-001")
  )
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(info$trajectory_id, "tempest-custom")
  expect_identical(info$source_uri, "https://example.com/review")
  expect_identical(info$metadata[[1L]]$application$api_key, "<redacted>")
  expect_identical(info$metadata[[1L]]$application$batch, "batch-001")
  expect_in("source_uri", losses$field)
  expect_in("metadata$application$api_key", losses$field)
  expect_setequal(
    unique(losses$reason[
      losses$field %in%
        c("source_uri", "metadata$application$api_key")
    ]),
    "redacted"
  )
})

test_that("Tempest adapter accepts only authoritative Tempest reviews", {
  review <- tempest_review_fixture()
  lookalike <- S7::props(review)

  condition <- rlang::catch_cnd(as_trajectory_tempest(lookalike))
  expect_s3_class(condition, "scans_error_tempest_source")
  expect_snapshot(error = TRUE, as_trajectory_tempest(lookalike))
})

test_that("Tempest adapter requires the authoritative accessor", {
  review <- tempest_review_fixture()
  testthat::local_mocked_bindings(
    tempest_review_accessor_available = function() FALSE
  )

  condition <- rlang::catch_cnd(as_trajectory_tempest(review))
  expect_s3_class(condition, "scans_error_missing_dependency")
  expect_snapshot(error = TRUE, as_trajectory_tempest(review))
})

test_that("Tempest projection enforces scans-owned payload limits", {
  oversized <- tempest_review_fixture(function(payload) {
    payload$programs$perspectives$evaluator_id <- strrep("e", 40000L)
    payload$programs$perspectives$evaluator_version <- strrep("v", 40000L)
    payload
  })

  condition <- rlang::catch_cnd(as_trajectory_tempest(oversized))
  expect_s3_class(condition, "scans_error_tempest_source")
  expect_snapshot(error = TRUE, as_trajectory_tempest(oversized))

  explicit_id <- strrep(
    "x",
    trajectory_payload_max_bytes -
      nchar("/tempest-event-000001", type = "bytes") +
      1L
  )
  condition <- rlang::catch_cnd(
    as_trajectory_tempest(tempest_review_fixture(), trajectory_id = explicit_id)
  )
  expect_s3_class(condition, "scans_error_tempest_argument")
  expect_snapshot(
    error = TRUE,
    as_trajectory_tempest(
      tempest_review_fixture(),
      trajectory_id = explicit_id
    )
  )

  condition <- rlang::catch_cnd(
    as_trajectory_tempest(tempest_review_fixture(), trajectory_id = "   ")
  )
  expect_s3_class(condition, "scans_error_tempest_argument")
  expect_snapshot(
    error = TRUE,
    as_trajectory_tempest(tempest_review_fixture(), trajectory_id = "   ")
  )

  oversized_run <- tempest_review_fixture(function(payload) {
    research_run_id <- strrep(
      "r",
      trajectory_payload_max_bytes + 1L
    )
    payload$product$research_run_id <- research_run_id
    payload$knowledge$proposal$research_run_id <- research_run_id
    for (index in seq_along(payload$joins$items)) {
      if (identical(payload$joins$items[[index]]$from_type, "product")) {
        payload$joins$items[[index]]$from_id <- research_run_id
      }
    }

    agent_id <- function(expert_id) {
      compute <- getFromNamespace("tempest_deputy_adapter_agent_id", "tempest")
      compute(list(
        product = "tempest",
        research_run_id = research_run_id,
        mode = "storm",
        stage = "research",
        role = "expert",
        knowledge_snapshot_id = "snapshot-001",
        expert_id = expert_id
      ))
    }
    first_agent_id <- agent_id("expert-001")
    session_id <- getFromNamespace(
      "tempest_storm_deputy_session_id",
      "tempest"
    )
    for (index in seq_along(payload$agent_runs$items)) {
      agent <- payload$agent_runs$items[[index]]
      payload$agent_runs$items[[index]]$agent_id <- agent_id(agent$expert_id)
      payload$agent_runs$items[[index]]$deputy_session_id <- session_id(
        research_run_id,
        agent$expert_id
      )
      if (!is.null(agent$parent_agent_id)) {
        payload$agent_runs$items[[index]]$parent_agent_id <- first_agent_id
      }
    }
    payload$agent_runs <- tempest_fixture_collection(payload$agent_runs$items)
    payload$joins <- tempest_fixture_collection(payload$joins$items)
    payload
  })
  condition <- rlang::catch_cnd(
    as_trajectory_tempest(oversized_run, trajectory_id = "tempest-short")
  )
  expect_s3_class(condition, "scans_error_tempest_source")
  expect_snapshot(
    error = TRUE,
    as_trajectory_tempest(oversized_run, trajectory_id = "tempest-short")
  )
})
