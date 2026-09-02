test_that("Tempest public reviews pass the shared adapter contract", {
  expect_adapter_conforms(
    source = tempest_review_fixture(),
    adapter = as_trajectory_tempest
  )
})

test_that("Tempest reviews become complete canonical product trajectories", {
  review <- tempest_review_fixture()
  bundle <- as_trajectory_tempest(review)
  info <- trajectory_info(bundle)
  events <- trajectory_events(bundle)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(info$trajectory_id, "tempest/research-promotion-1")
  expect_identical(info$run_id, "research-promotion-1")
  expect_identical(info$source_type, "tempest")
  expect_identical(info$source_id, review@review_id)
  expect_identical(info$agent, "storm")
  expect_identical(info$status, "completed")
  expect_identical(
    info$started_at,
    as.POSIXct("2026-08-16 00:01:00", tz = "UTC")
  )
  expect_identical(
    info$completed_at,
    as.POSIXct("2026-08-16 00:08:30", tz = "UTC")
  )
  expect_identical(nrow(trajectory_turns(bundle)), 0L)
  expect_setequal(
    unique(events$event_type),
    c(
      "tempest:stage_succeeded",
      "tempest:deputy_run",
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

  expect_identical(
    stages$name,
    c(
      "perspectives",
      "query_decomposition",
      "extract_claims",
      "verify_claim_support",
      "draft_outline",
      "refined_outline",
      "lead_section",
      "section_writing"
    )
  )
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
  stages <- events[grepl("^tempest:stage_", events$event_type), ]

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
      "correlated_with",
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
  expect_true(all(grepl(
    "^graft:",
    vapply(
      revisions$value,
      `[[`,
      character(1),
      "revision_id"
    )
  )))
  expect_in("Claim", revisions$name)
  expect_true(all(nzchar(vapply(
    evidence$value,
    `[[`,
    character(1),
    "record_id"
  ))))
  expect_setequal(
    finding$name,
    c(
      "exploratory_execution",
      "support_unverified",
      "publication_blocked"
    )
  )
  unverified <- match("support_unverified", finding$name)
  expect_identical(finding$status[[unverified]], "completed")
  expect_identical(finding$value[[unverified]]$severity, "warning")
  expect_true(all(finding$parent_event_id %in% stages$event_id))
})

test_that("Tempest deliberate omissions and bounded lanes become losses", {
  review <- tempest_review_fixture()
  projection <- tempest::tempest_trajectory_review_data(review)
  projection$evidence <- tempest_incomplete_collection(projection$evidence)
  projection$joins <- tempest_incomplete_collection(projection$joins)
  testthat::local_mocked_bindings(
    tempest_review_snapshot = function(x, call) projection
  )
  losses <- trajectory_losses(as_trajectory_tempest(review))
  bounded <- losses[losses$field == "evidence$items", ]

  expect_setequal(
    losses$field[losses$reason == "unsupported"],
    c("turns", "source_content", "runtime")
  )
  expect_identical(bounded$reason, "truncated")
  expect_identical(
    bounded$metadata[[1L]]$complete_digest,
    projection$evidence$digest
  )
})

test_that("Tempest stage omissions leave product execution bounds unknown", {
  review <- tempest_review_fixture()
  projection <- tempest::tempest_trajectory_review_data(review)
  projection$stages <- tempest_incomplete_collection(projection$stages)
  projection$joins <- tempest_incomplete_collection(projection$joins)
  testthat::local_mocked_bindings(
    tempest_review_snapshot = function(x, call) projection
  )
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

test_that("Tempest caller metadata paths are bounded before sanitizing", {
  oversized <- stats::setNames(
    list(list(api_key = "secret")),
    strrep("x", 65530L)
  )

  condition <- rlang::catch_cnd(
    as_trajectory_tempest(tempest_review_fixture(), metadata = oversized)
  )

  expect_s3_class(condition, "scans_error_tempest_argument")
  expect_match(conditionMessage(condition), "paths must not exceed")
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

test_that("Tempest adapter enforces scans-owned trajectory ID bounds", {
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
})
