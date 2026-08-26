tempest_fixture_digest <- function(letter) {
  index <- (match(letter, letters) - 1L) %% 6L + 1L
  letter <- letters[[index]]
  paste0("sha256:", strrep(letter, 64L))
}

tempest_fixture_snapshot <- function(
  store_id,
  schema_build_digest,
  commit_order,
  batch_id,
  committed_at
) {
  snapshot <- list(
    schema_version = 1L,
    snapshot_id = "",
    store_id = store_id,
    store_format_version = "3.0.0",
    schema_build_digest = schema_build_digest,
    commit_order = as.double(commit_order),
    batch_id = batch_id,
    committed_at = committed_at,
    history_complete = TRUE
  )
  digest <- getFromNamespace(
    "tempest_trajectory_graft_snapshot_digest",
    "tempest"
  )
  snapshot$snapshot_id <- digest(snapshot)
  snapshot
}

tempest_fixture_collection <- function(
  items,
  letter = "a",
  omitted = 0L,
  preserve_order = FALSE
) {
  items <- unname(items)
  if (!preserve_order && length(items) > 1L) {
    canonical_json <- getFromNamespace(
      "tempest_product_canonical_json",
      "tempest"
    )
    keys <- vapply(items, canonical_json, character(1))
    items <- items[order(keys, method = "radix")]
  }
  digest <- if (omitted == 0L) {
    digest <- getFromNamespace("tempest_trajectory_digest", "tempest")
    digest(items)
  } else {
    tempest_fixture_digest(letter)
  }
  list(
    total = as.integer(length(items) + omitted),
    retained = as.integer(length(items)),
    omitted = as.integer(omitted),
    digest = digest,
    items = items
  )
}

tempest_fixture_programs <- function() {
  stages <- c(
    "perspectives",
    "personas",
    "query_decomposition",
    "extract_claims",
    "verify_claim_support",
    "next_question",
    "draft_outline",
    "refined_outline",
    "section_writing",
    "lead_section"
  )
  stats::setNames(
    lapply(seq_along(stages), function(index) {
      list(
        stage = stages[[index]],
        contract_version = 1L,
        program_artifact_id = tempest_fixture_digest(letters[[index]]),
        evaluator_id = paste0("tempest::evaluator/", stages[[index]]),
        evaluator_version = "1",
        governed_procedure_ref = NULL
      )
    }),
    stages
  )
}

tempest_review_fixture <- function(transform = identity) {
  testthat::skip_if_not_installed("tempest", minimum_version = "0.3.0.9000")

  program_id <- tempest_fixture_digest("i")
  schema_build_digest <- getFromNamespace(
    "tempest_promotion_schema_build_digest",
    "tempest"
  )
  store_id <- paste0(
    "graft-store-20260822T160000.000000-",
    "abcdefghijklmnopqrst"
  )
  input_snapshot <- tempest_fixture_snapshot(
    store_id,
    schema_build_digest,
    1,
    "graft:INPUT001",
    "2026-08-22T15:59:00.000000Z"
  )
  agent_id <- function(expert_id) {
    compute <- getFromNamespace("tempest_deputy_adapter_agent_id", "tempest")
    compute(list(
      product = "tempest",
      research_run_id = "research-run-001",
      mode = "storm",
      stage = "research",
      role = "expert",
      knowledge_snapshot_id = input_snapshot$snapshot_id,
      expert_id = expert_id
    ))
  }
  first_agent_id <- agent_id("expert-001")
  second_agent_id <- agent_id("expert-002")
  session_id <- getFromNamespace("tempest_storm_deputy_session_id", "tempest")
  stages <- list(
    list(
      stage = "section_writing",
      attempt_id = "attempt-section-001",
      trace_id = "attempt-section-001",
      deputy_binding = NULL,
      status = "succeeded",
      started_at = "2026-08-22T16:00:00.000Z",
      completed_at = "2026-08-22T16:00:09.000Z",
      output = list(
        kind = "content_digest",
        count = 1L,
        digest = tempest_fixture_digest("k")
      ),
      program_artifact_id = program_id,
      governed_procedure_revision_id = NULL,
      failure_class = NULL,
      fallback_policy = "grounded_only",
      fallback_implementation = NULL,
      fallback_taken = FALSE,
      execution_path = "grounded",
      support_status = "verified",
      publication_allowed = TRUE
    ),
    list(
      stage = "perspectives",
      attempt_id = "attempt-perspectives-001",
      trace_id = "attempt-perspectives-001",
      deputy_binding = NULL,
      status = "succeeded",
      started_at = "2026-08-22T16:00:01.000Z",
      completed_at = "2026-08-22T16:00:10.000Z",
      output = list(
        kind = "state_field",
        count = 2L,
        digest = tempest_fixture_digest("l")
      ),
      program_artifact_id = tempest_fixture_digest("a"),
      governed_procedure_revision_id = NULL,
      failure_class = NULL,
      fallback_policy = "exploratory_allowed",
      fallback_implementation = NULL,
      fallback_taken = FALSE,
      execution_path = "exploratory",
      support_status = "unknown",
      publication_allowed = FALSE
    ),
    list(
      stage = "extract_claims",
      attempt_id = "attempt-extract-001",
      trace_id = "attempt-extract-001",
      deputy_binding = NULL,
      status = "succeeded",
      started_at = "2026-08-22T16:00:02.000Z",
      completed_at = "2026-08-22T16:00:08.000Z",
      output = list(
        kind = "workspace_claims",
        count = 1L,
        digest = tempest_fixture_digest("n")
      ),
      program_artifact_id = tempest_fixture_digest("d"),
      governed_procedure_revision_id = NULL,
      failure_class = NULL,
      fallback_policy = "fail_closed",
      fallback_implementation = NULL,
      fallback_taken = FALSE,
      execution_path = "grounded",
      support_status = "unknown",
      publication_allowed = FALSE
    ),
    list(
      stage = "verify_claim_support",
      attempt_id = "attempt-verify-001",
      trace_id = "attempt-verify-001",
      deputy_binding = NULL,
      status = "succeeded",
      started_at = "2026-08-22T16:00:03.000Z",
      completed_at = "2026-08-22T16:00:09.000Z",
      output = list(
        kind = "claim_supports",
        count = 1L,
        digest = tempest_fixture_digest("o")
      ),
      program_artifact_id = tempest_fixture_digest("e"),
      governed_procedure_revision_id = NULL,
      failure_class = NULL,
      fallback_policy = "fail_closed",
      fallback_implementation = NULL,
      fallback_taken = FALSE,
      execution_path = "grounded",
      support_status = "verified",
      publication_allowed = TRUE
    )
  )
  agents <- list(
    list(
      trace_id = "deputy-run-001",
      trace_type = "deputy_run",
      stage = "research",
      role = "expert",
      status = "complete",
      completion_disposition = "issued",
      agent_id = first_agent_id,
      expert_id = "expert-001",
      deputy_run_id = "deputy-run-001",
      deputy_session_id = session_id("research-run-001", "expert-001"),
      parent_agent_id = NULL,
      parent_run_id = NULL,
      delegation_id = NULL,
      tool_call_id = NULL,
      program_artifact_id = NULL,
      correlation_id = "correlation-001"
    ),
    list(
      trace_id = "deputy-run-002",
      trace_type = "deputy_delegation",
      stage = "research",
      role = "expert",
      status = "complete",
      completion_disposition = "issued",
      agent_id = second_agent_id,
      expert_id = "expert-002",
      deputy_run_id = "deputy-run-002",
      deputy_session_id = session_id("research-run-001", "expert-002"),
      parent_agent_id = first_agent_id,
      parent_run_id = "deputy-run-001",
      delegation_id = "delegation-001",
      tool_call_id = "tool-call-001",
      program_artifact_id = NULL,
      correlation_id = "correlation-001"
    )
  )
  evidence <- list(
    list(record_type = "claim", record_id = "claim-001"),
    list(record_type = "claim_support", record_id = "support-001"),
    list(record_type = "evidence_span", record_id = "span-001")
  )
  findings <- list(
    list(
      code = "exploratory_execution",
      severity = "info",
      ref_type = "stage_attempt",
      ref_id = "attempt-perspectives-001"
    ),
    list(
      code = "support_unverified",
      severity = "warning",
      ref_type = "stage_attempt",
      ref_id = "attempt-perspectives-001"
    ),
    list(
      code = "publication_blocked",
      severity = "warning",
      ref_type = "stage_attempt",
      ref_id = "attempt-perspectives-001"
    ),
    list(
      code = "support_unverified",
      severity = "warning",
      ref_type = "stage_attempt",
      ref_id = "attempt-extract-001"
    ),
    list(
      code = "publication_blocked",
      severity = "warning",
      ref_type = "stage_attempt",
      ref_id = "attempt-extract-001"
    )
  )
  revision <- list(
    class = "Claim",
    record_id = "claim-001",
    revision_id = "graft:REVISION001",
    revision_number = 1L,
    action = "insert",
    batch_id = "graft:PLAN001",
    content_digest = tempest_fixture_digest("m"),
    schema_build_digest = schema_build_digest
  )
  receipt_classes <- getFromNamespace(
    "tempest_promotion_receipt_classes",
    "tempest"
  )()
  zero_counts <- stats::setNames(
    as.list(rep(0L, length(receipt_classes))),
    receipt_classes
  )
  inserted_counts <- zero_counts
  observed_counts <- zero_counts
  inserted_counts$Claim <- 1L
  observed_counts$Claim <- 1L
  knowledge <- list(
    input_snapshot = input_snapshot,
    promotion_state = "accepted",
    proposal = list(
      bundle_id = tempest_fixture_digest("b"),
      research_run_id = "research-run-001",
      schema_build_digest = schema_build_digest,
      claim_selection = list(
        kind = "claim_ids",
        count = 1L,
        digest = getFromNamespace(
          "tempest_trajectory_claim_selection_digest",
          "tempest"
        )("claim-001")
      )
    ),
    acceptance = list(
      receipt_id = tempest_fixture_digest("c"),
      bundle_id = tempest_fixture_digest("b"),
      plan_id = "graft:PLAN001",
      plan_digest = tempest_fixture_digest("p"),
      batch_id = "graft:PLAN001",
      store_id = store_id,
      schema_build_digest = schema_build_digest,
      snapshot = tempest_fixture_snapshot(
        store_id,
        schema_build_digest,
        2,
        "graft:PLAN001",
        "2026-08-22T16:01:00.000000Z"
      ),
      counts = list(
        inserted = inserted_counts,
        updated = zero_counts,
        matched = zero_counts,
        observed = observed_counts
      ),
      record_revisions = tempest_fixture_collection(
        list(revision),
        "q",
        preserve_order = TRUE
      )
    )
  )
  acceptance <- knowledge$acceptance
  receipt_payload <- getFromNamespace(
    "tempest_promotion_receipt_payload",
    "tempest"
  )(
    acceptance$bundle_id,
    acceptance$plan_id,
    acceptance$plan_digest,
    acceptance$batch_id,
    acceptance$store_id,
    acceptance$schema_build_digest,
    acceptance$snapshot,
    acceptance$counts,
    acceptance$record_revisions$items
  )
  knowledge$acceptance$receipt_id <- getFromNamespace(
    "tempest_promotion_digest",
    "tempest"
  )(receipt_payload)
  join <- getFromNamespace("tempest_trajectory_join", "tempest")
  joins <- list(
    join(
      "product",
      "research-run-001",
      "contains",
      "stage_attempt",
      "attempt-section-001",
      "authority_validated",
      c("research_run_id", "attempt_id")
    ),
    join(
      "product",
      "research-run-001",
      "contains",
      "stage_attempt",
      "attempt-perspectives-001",
      "authority_validated",
      c("research_run_id", "attempt_id")
    ),
    join(
      "product",
      "research-run-001",
      "contains",
      "stage_attempt",
      "attempt-extract-001",
      "authority_validated",
      c("research_run_id", "attempt_id")
    ),
    join(
      "product",
      "research-run-001",
      "contains",
      "stage_attempt",
      "attempt-verify-001",
      "authority_validated",
      c("research_run_id", "attempt_id")
    ),
    join(
      "stage_attempt",
      "attempt-section-001",
      "executed_as",
      "program_artifact",
      program_id,
      "authority_validated",
      c(
        "stage",
        "program_artifact_id",
        "contract_version",
        "evaluator_id",
        "evaluator_version"
      )
    ),
    join(
      "stage_attempt",
      "attempt-perspectives-001",
      "executed_as",
      "program_artifact",
      tempest_fixture_digest("a"),
      "authority_validated",
      c(
        "stage",
        "program_artifact_id",
        "contract_version",
        "evaluator_id",
        "evaluator_version"
      )
    ),
    join(
      "stage_attempt",
      "attempt-extract-001",
      "executed_as",
      "program_artifact",
      tempest_fixture_digest("d"),
      "authority_validated",
      c(
        "stage",
        "program_artifact_id",
        "contract_version",
        "evaluator_id",
        "evaluator_version"
      )
    ),
    join(
      "stage_attempt",
      "attempt-verify-001",
      "executed_as",
      "program_artifact",
      tempest_fixture_digest("e"),
      "authority_validated",
      c(
        "stage",
        "program_artifact_id",
        "contract_version",
        "evaluator_id",
        "evaluator_version"
      )
    ),
    join(
      "stage_attempt",
      "attempt-section-001",
      "contains",
      "output_digest",
      tempest_fixture_digest("k"),
      "exact_identity",
      c("output_reference.kind", "output_reference.ids")
    ),
    join(
      "stage_attempt",
      "attempt-perspectives-001",
      "contains",
      "product_field",
      "title",
      "exact_identity",
      c("output_reference.kind", "output_reference.ids")
    ),
    join(
      "stage_attempt",
      "attempt-extract-001",
      "contains",
      "claim",
      "claim-001",
      "exact_identity",
      c("output_reference.kind", "output_reference.ids")
    ),
    join(
      "stage_attempt",
      "attempt-verify-001",
      "contains",
      "claim_support",
      "support-001",
      "exact_identity",
      c("output_reference.kind", "output_reference.ids")
    ),
    join(
      "stage_attempt",
      "attempt-perspectives-001",
      "contains",
      "product_field",
      "perspectives",
      "exact_identity",
      c("output_reference.kind", "output_reference.ids")
    ),
    join(
      "product",
      "research-run-001",
      "contains",
      "claim",
      "claim-001",
      "authority_validated",
      c("research_run_id", "record_id")
    ),
    join(
      "product",
      "research-run-001",
      "contains",
      "evidence_span",
      "span-001",
      "authority_validated",
      c("research_run_id", "record_id")
    ),
    join(
      "product",
      "research-run-001",
      "contains",
      "claim_support",
      "support-001",
      "authority_validated",
      c("research_run_id", "record_id")
    ),
    join(
      "product",
      "research-run-001",
      "read_from",
      "graft_snapshot",
      input_snapshot$snapshot_id,
      "authority_validated",
      c("snapshot_id", "store_id", "schema_build_digest", "commit_order")
    ),
    join(
      "deputy_run",
      "deputy-run-001",
      "parent_of",
      "deputy_run",
      "deputy-run-002",
      "exact_identity",
      c(
        "parent_agent_id",
        "parent_run_id",
        "delegation_id",
        "tool_call_id"
      )
    ),
    join(
      "deputy_run",
      "deputy-run-001",
      "correlated_with",
      "deputy_run",
      "deputy-run-002",
      "correlation_only",
      "correlation_id"
    ),
    join(
      "product",
      "research-run-001",
      "proposed_as",
      "promotion_bundle",
      knowledge$proposal$bundle_id,
      "authority_validated",
      c("research_run_id", "bundle_id", "claim_ids")
    ),
    join(
      "promotion_bundle",
      knowledge$proposal$bundle_id,
      "accepted_as",
      "promotion_receipt",
      knowledge$acceptance$receipt_id,
      "exact_identity",
      "bundle_id"
    ),
    join(
      "promotion_receipt",
      knowledge$acceptance$receipt_id,
      "accepted_as",
      "graft_revision",
      revision$revision_id,
      "exact_identity",
      c(
        "record_id",
        "revision_id",
        "batch_id",
        "content_digest",
        "schema_build_digest"
      )
    )
  )

  payload <- list(
    schema_version = 1L,
    product = list(
      research_run_id = "research-run-001",
      mode = "storm",
      status = "succeeded",
      config_digest = tempest_fixture_digest("s"),
      report_reference = list(
        report_id = "report_md",
        sha256 = tempest_fixture_digest("t")
      )
    ),
    stages = tempest_fixture_collection(stages, "u", preserve_order = TRUE),
    agent_runs = tempest_fixture_collection(agents, "v"),
    programs = tempest_fixture_programs(),
    knowledge = knowledge,
    evidence = tempest_fixture_collection(evidence, "w"),
    joins = tempest_fixture_collection(joins, "x"),
    findings = tempest_fixture_collection(findings, "y")
  )
  payload <- transform(payload)
  constructor <- getFromNamespace("TempestTrajectoryReview", "tempest")
  digest <- getFromNamespace("tempest_trajectory_digest", "tempest")
  do.call(
    constructor,
    c(
      list(
        schema_version = payload$schema_version,
        review_id = digest(payload)
      ),
      payload[setdiff(names(payload), "schema_version")]
    )
  )
}
