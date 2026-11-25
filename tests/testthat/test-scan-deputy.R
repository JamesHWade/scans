deputy_scan_status <- function(bundle, trajectory_id = NULL) {
  assessments <- assess_trajectory_scans(
    bundle,
    scans = c("cut_off_answer", "run_intervention")
  )$assessments
  if (!is.null(trajectory_id)) {
    assessments <- assessments[assessments$trajectory_id == trajectory_id, ]
  }
  stats::setNames(assessments$status, assessments$scan)
}

deputy_lifecycle_fixture <- function() {
  ending <- deputy::RunEndingLimit(
    "request_limit",
    field = "max_requests",
    actual = 3,
    limit = 3,
    wrapped_up = TRUE
  )
  events <- list(
    deputy::AgentEvent("start", task = "Review"),
    deputy::AgentEvent(
      "fallback",
      fallback_index = 1L,
      model = "model-b",
      condition = simpleError("HTTP 503")
    ),
    deputy::AgentEvent("compaction", method = "summary", turns_compacted = 4L),
    deputy::AgentEvent(
      "compaction_error",
      phase = "compaction",
      condition = simpleError("Summary failed")
    ),
    deputy::AgentEvent("compaction", method = "summary", turns_compacted = 2L),
    deputy::AgentEvent(
      "permission",
      tool_call_id = "call-001",
      decision = "deny"
    ),
    deputy::AgentEvent(
      "approval",
      approval_id = "approval-001",
      status = "pending",
      tool_call_id = "call-002",
      tool_name = "write_file",
      tool_input = list(path = "report.md")
    ),
    deputy::AgentEvent(
      "approval",
      approval_id = "approval-002",
      status = "pending",
      tool_call_id = "call-003",
      tool_name = "write_file",
      tool_input = list(path = "notes.md")
    ),
    deputy::AgentEvent(
      "permission",
      tool_call_id = "call-003",
      decision = "allow"
    ),
    deputy::AgentEvent(
      "limit_wrap_up",
      reason = "request_limit",
      request_number = 3L
    ),
    deputy::AgentEvent("stop", reason = "request_limit", ending = ending)
  )
  as_trajectory_deputy(deputy_result_fixture(
    events,
    overrides = list(stop_reason = "request_limit", ending = ending)
  ))
}

# A bundle from another adapter that projects the same Deputy run evidence.
deputy_projected_bundle <- function(ending = NULL, events = NULL) {
  metadata <- if (is.null(ending)) list() else list(ending = ending)
  if (is.null(events)) {
    events <- tibble::tibble(
      trajectory_id = character(),
      event_id = character(),
      event_index = integer(),
      event_type = character()
    )
  }
  TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "otel/run-001",
      source_type = "otel",
      status = "interrupted",
      metadata = list(metadata)
    ),
    data.frame(),
    events
  )
}

deputy_limit_ending <- function(cut_off = FALSE, finish_reason = "stop") {
  list(
    class = "RunEndingLimit",
    reason = "request_limit",
    by = NA_character_,
    field = "max_requests",
    actual = 3,
    limit = 3,
    wrapped_up = TRUE,
    phase = NA_character_,
    finish_reason = finish_reason,
    cut_off = cut_off
  )
}

test_that("a cut-off answer is a trajectory finding although the run completed", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  for (reason in c("max_tokens", "context_window", "content_filter")) {
    bundle <- as_trajectory_deputy(deputy_result_fixture(
      overrides = list(finish_reason = reason)
    ))
    findings <- scan_trajectories(bundle, scans = "cut_off_answer")

    expect_identical(trajectory_info(bundle)$status, "completed")
    expect_identical(findings$trajectory_id, "deputy/run-001")
    expect_identical(findings$severity, "error")
    expect_true(is.na(findings$event_id))
    expect_true(is.na(findings$turn_id))
    expect_identical(findings$event_ids, list(character()))
    expect_identical(
      findings$value[[1L]],
      list(
        status = "completed",
        ending_class = "RunEndingCutOff",
        finish_reason = reason,
        cut_off = TRUE
      )
    )
  }

  answered <- as_trajectory_deputy(deputy_result_fixture())
  expect_identical(
    nrow(scan_trajectories(answered, scans = "cut_off_answer")),
    0L
  )
  expect_identical(
    deputy_scan_status(answered),
    c(
      cut_off_answer = "assessed_no_findings",
      run_intervention = "assessed_no_findings"
    )
  )
})

test_that("a truncated final turn is reported at the turn and at the answer", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  result <- deputy_result_fixture(
    overrides = list(
      finish_reason = "max_tokens",
      turns = list(
        ellmer::UserTurn(list(ellmer::ContentText("Find the evidence"))),
        ellmer::AssistantTurn(
          list(ellmer::ContentText("Evidence fou")),
          finish_reason = "max_tokens"
        )
      )
    )
  )
  findings <- scan_trajectories(as_trajectory_deputy(result))

  expect_identical(findings$scan, c("turn_error", "cut_off_answer"))
  expect_identical(findings$turn_id[[1L]], "deputy/run-001/turn-000002")
  expect_true(is.na(findings$turn_id[[2L]]))
})

test_that("Deputy interventions produce findings with exact event evidence", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  bundle <- deputy_lifecycle_fixture()
  events <- trajectory_events(bundle)
  id <- \(type) events$event_id[events$event_type == paste0("deputy:", type)]
  findings <- scan_trajectories(bundle, scans = "run_intervention")
  kinds <- vapply(findings$value, \(value) value$kind, character(1))

  expect_identical(
    kinds,
    c(
      "provider_fallback",
      "repeated_compaction",
      "compaction_failure",
      "permission_denied",
      "approval_pending",
      "limit_wrap_up"
    )
  )
  expect_identical(unique(findings$severity), "warning")
  expect_identical(unique(findings$trajectory_id), "deputy/run-001")
  by_kind <- \(kind) findings[kinds == kind, ]

  expect_identical(by_kind("provider_fallback")$event_ids, list(id("fallback")))
  expect_identical(
    by_kind("provider_fallback")$value[[1L]][c("fallback_index", "error")],
    list(fallback_index = 1L, error = "HTTP 503")
  )
  expect_identical(
    by_kind("repeated_compaction")$event_ids,
    list(id("compaction"))
  )
  expect_identical(by_kind("repeated_compaction")$value[[1L]]$count, 2L)
  expect_identical(
    by_kind("compaction_failure")$event_ids,
    list(id("compaction_error"))
  )
  expect_identical(
    by_kind("compaction_failure")$value[[1L]]$error,
    "Summary failed"
  )
  expect_identical(
    by_kind("permission_denied")$event_id,
    id("permission")[[1L]]
  )
  expect_identical(by_kind("permission_denied")$value[[1L]]$call_id, "call-001")

  pending <- by_kind("approval_pending")
  expect_identical(pending$event_id, id("approval")[[1L]])
  expect_identical(
    pending$value[[1L]],
    list(
      kind = "approval_pending",
      tool = "write_file",
      call_id = "call-002",
      approval_id = "approval-001"
    )
  )

  wrap_up <- by_kind("limit_wrap_up")
  expect_identical(wrap_up$event_ids, list(id("limit_wrap_up")))
  expect_identical(
    wrap_up$value[[1L]],
    list(
      kind = "limit_wrap_up",
      reason = "request_limit",
      field = "max_requests",
      actual = 3,
      limit = 3,
      wrapped_up = TRUE
    )
  )
  expect_identical(
    deputy_scan_status(bundle),
    c(
      cut_off_answer = "assessed_no_findings",
      run_intervention = "assessed_with_findings"
    )
  )
})

test_that("event_error and run_intervention report the same failed event deliberately", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  bundle <- deputy_lifecycle_fixture()
  findings <- scan_trajectories(bundle)
  failed <- findings$event_id[findings$scan == "event_error"]
  failures <- findings[
    findings$scan == "run_intervention" &
      vapply(
        findings$value,
        \(value) {
          any(value$kind %in% c("compaction_failure", "permission_denied"))
        },
        logical(1)
      ),
  ]

  expect_length(failed, 2L)
  expect_setequal(failures$event_id, failed)
  expect_identical(
    findings$severity[findings$event_id %in% failed],
    rep(c("error", "warning"), 2L)
  )
})

test_that("one compaction is not repeated and a resolved approval is not pending", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  events <- list(
    deputy::AgentEvent("start", task = "Review"),
    deputy::AgentEvent("compaction", method = "summary", turns_compacted = 4L),
    deputy::AgentEvent(
      "approval",
      approval_id = "approval-001",
      status = "pending",
      tool_call_id = "call-001",
      tool_name = "write_file"
    ),
    deputy::AgentEvent(
      "tool_start",
      tool_call_id = "call-001",
      tool_name = "write_file"
    ),
    deputy::AgentEvent("stop", reason = "complete")
  )
  bundle <- as_trajectory_deputy(deputy_result_fixture(events))

  expect_identical(
    nrow(scan_trajectories(bundle, scans = "run_intervention")),
    0L
  )
  expect_identical(
    deputy_scan_status(bundle)[["run_intervention"]],
    "assessed_no_findings"
  )
})

test_that("Deputy scans key on projected run evidence, not the source type", {
  wrapped <- deputy_projected_bundle(deputy_limit_ending(
    cut_off = TRUE,
    finish_reason = "max_tokens"
  ))
  findings <- scan_trajectories(wrapped)

  expect_identical(findings$scan, c("cut_off_answer", "run_intervention"))
  expect_identical(findings$trajectory_id, rep("otel/run-001", 2L))
  expect_identical(findings$value[[2L]]$kind, "limit_wrap_up")
  expect_true(is.na(findings$event_id[[2L]]))
  result <- assess_trajectory_scans(
    wrapped,
    scans = c("cut_off_answer", "run_intervention")
  )
  expect_identical(
    result$assessments$status,
    c("assessed_with_findings", "assessed_with_findings")
  )
  expect_match(
    result$assessments$limitations[[2L]],
    "missing or lost run evidence",
    all = FALSE
  )

  fallback <- deputy_projected_bundle(
    events = tibble::tibble(
      trajectory_id = "otel/run-001",
      event_id = "otel/run-001/event-000001",
      event_index = 1L,
      event_type = "deputy:fallback",
      status = "completed"
    )
  )
  findings <- scan_trajectories(fallback, scans = "run_intervention")
  expect_identical(findings$event_ids, list("otel/run-001/event-000001"))
  expect_identical(
    deputy_scan_status(fallback),
    c(
      cut_off_answer = "insufficient_evidence",
      run_intervention = "assessed_with_findings"
    )
  )
})

test_that("Deputy scan assessments separate inapplicable, missing, and clean evidence", {
  manual <- TrajectoryBundle(
    data.frame(trajectory_id = "chat-1", source_type = "ellmer"),
    data.frame(),
    data.frame()
  )
  expect_identical(
    deputy_scan_status(manual),
    c(cut_off_answer = "not_applicable", run_intervention = "not_applicable")
  )

  bare_deputy <- TrajectoryBundle(
    data.frame(trajectory_id = "deputy/run-001", source_type = "deputy"),
    data.frame(),
    data.frame()
  )
  expect_identical(
    deputy_scan_status(bare_deputy),
    c(
      cut_off_answer = "insufficient_evidence",
      run_intervention = "insufficient_evidence"
    )
  )

  unknown_ending <- deputy_limit_ending()
  unknown_ending[c("cut_off", "finish_reason", "wrapped_up")] <- list(
    NA,
    NA_character_,
    NA
  )
  stopped <- deputy_projected_bundle(
    unknown_ending,
    tibble::tibble(
      trajectory_id = "otel/run-001",
      event_id = "otel/run-001/event-000001",
      event_index = 1L,
      event_type = "deputy:start",
      status = "completed"
    )
  )
  expect_identical(
    deputy_scan_status(stopped),
    c(
      cut_off_answer = "insufficient_evidence",
      run_intervention = "assessed_no_findings"
    )
  )

  no_events <- deputy_projected_bundle(deputy_limit_ending())
  expect_identical(
    assess_trajectory_scans(
      no_events,
      scans = "cut_off_answer"
    )$assessments$status,
    "assessed_no_findings"
  )
  answered <- deputy_limit_ending()
  answered[c("class", "wrapped_up")] <- list("RunEndingAnswered", NA)
  expect_identical(
    deputy_scan_status(deputy_projected_bundle(answered))[[
      "run_intervention"
    ]],
    "insufficient_evidence"
  )
  expect_identical(
    assess_trajectory_scans(
      deputy_projected_bundle(answered),
      scans = "run_intervention"
    )$assessments$settings,
    list(list(compaction_threshold = 2L))
  )
})

test_that("a limit that stopped the run without an answer is a limit stop", {
  stopped <- deputy_limit_ending()
  stopped$wrapped_up <- FALSE
  findings <- scan_trajectories(
    deputy_projected_bundle(stopped),
    scans = "run_intervention"
  )

  expect_identical(findings$trajectory_id, "otel/run-001")
  expect_true(is.na(findings$event_id))
  expect_identical(
    findings$value[[1L]][c("kind", "field", "wrapped_up")],
    list(kind = "limit_stop", field = "max_requests", wrapped_up = FALSE)
  )
})

test_that("reported delegation claims are findings on the child trajectory", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  fixture <- deputy_delegation_fixture()
  bundle <- as_trajectory_deputy(fixture$lead, delegations = fixture$history)
  findings <- scan_trajectories(bundle, scans = "run_intervention")

  expect_identical(findings$trajectory_id, "deputy/run-child-001")
  expect_true(is.na(findings$event_id))
  expect_identical(findings$event_ids, list(character()))
  expect_identical(
    findings$value[[1L]],
    list(
      kind = "delegation_missing_evidence",
      parent_trajectory_id = "deputy/run-001",
      delegation_id = "delegation-001",
      claim = "No 2024 figures were available"
    )
  )

  expect_identical(
    assess_trajectory_scans(
      bundle,
      scans = "cut_off_answer"
    )$assessments$status,
    rep("assessed_no_findings", 3L)
  )
  assessments <- assess_trajectory_scans(
    bundle,
    scans = "run_intervention"
  )$assessments
  expect_identical(
    stats::setNames(assessments$status, assessments$trajectory_id),
    c(
      "deputy/run-001" = "assessed_no_findings",
      "deputy/run-child-001" = "assessed_with_findings",
      "deputy/run-child-002" = "assessed_no_findings"
    )
  )
  expect_match(
    assessments$limitations[[2L]],
    "did not report unresolved work",
    all = FALSE
  )
  expect_match(
    assessments$limitations[[3L]],
    "did not report missing evidence",
    all = FALSE
  )
})

test_that("unreported and truncated delegation claims stay qualified", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  child <- function(claims) {
    deputy_export_child(
      "delegation-001",
      "run-child-001",
      NULL,
      claims = claims
    )
  }
  silent <- as_trajectory_deputy(deputy_read_history(deputy_export_fixture(
    list(child(list(missing_evidence = NULL, unresolved_work = NULL)))
  )))
  expect_identical(
    nrow(scan_trajectories(silent, scans = "run_intervention")),
    0L
  )
  expect_identical(
    deputy_scan_status(silent)[["run_intervention"]],
    "insufficient_evidence"
  )

  view <- deputy_read_history(deputy_export_fixture(list(child(list(
    missing_evidence = NULL,
    unresolved_work = "Verify the totals"
  )))))[[1L]]
  view$outcome$runtime$claim_omissions <- list(
    unresolved_work = list(omitted_entries = 2L, text_truncated = FALSE)
  )
  bounded <- as_trajectory_deputy(list(view))
  expect_identical(
    trajectory_losses(bounded)$field,
    "metadata$delegation$claims$unresolved_work"
  )
  findings <- scan_trajectories(bounded, scans = "run_intervention")
  expect_identical(findings$value[[1L]]$kind, "delegation_unresolved_work")
  expect_identical(findings$value[[1L]]$claim, "Verify the totals")
  result <- assess_trajectory_scans(bounded, scans = "run_intervention")
  expect_identical(result$assessments$status, "assessed_with_findings")
  expect_match(
    result$assessments$limitations[[1L]],
    "Deputy bounded the subagent's claims",
    all = FALSE
  )
})

test_that("the overview names each intervention kind", {
  findings <- tibble::tibble(
    scan = "run_intervention",
    value = list(
      list(kind = "provider_fallback"),
      list(kind = "permission_denied"),
      list(kind = "provider_fallback"),
      list(kind = "repeated_compaction")
    )
  )
  expect_identical(
    scans_app_pattern_evidence(findings),
    "2 provider fallbacks, 1 run with repeated compaction, 1 denied permission"
  )
  delegations <- tibble::tibble(
    scan = "run_intervention",
    value = list(
      list(kind = "delegation_unresolved_work"),
      list(kind = "delegation_missing_evidence"),
      list(kind = "delegation_missing_evidence"),
      list(kind = "limit_stop")
    )
  )
  expect_identical(
    scans_app_pattern_evidence(delegations),
    "1 limit stop, 2 delegations missing evidence, 1 delegation with unresolved work"
  )
  expect_identical(
    scans_app_pattern_evidence(tibble::tibble(
      scan = rep("cut_off_answer", 2L)
    )),
    "2 final answers cut off"
  )
})

test_that("Ask tools show run endings and can narrow findings to a scan", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  bundle <- as_trajectory_deputy(list(
    deputy_result_fixture(),
    deputy_result_fixture(
      overrides = list(run_id = "run-002", finish_reason = "max_tokens")
    )
  ))
  tools <- scans_tools(bundle)
  runs <- scans_tool_result(tools$scans_list_trajectories, sort = "oldest")
  rows <- runs$data[order(vapply(runs$data, \(row) row$trajectory_id, ""))]

  expect_identical(
    lapply(rows, \(row) {
      row[c("trajectory_id", "run_ending", "finish_reason", "cut_off")]
    }),
    list(
      list(
        trajectory_id = "deputy/run-001",
        run_ending = "RunEndingAnswered",
        finish_reason = NULL,
        cut_off = FALSE
      ),
      list(
        trajectory_id = "deputy/run-002",
        run_ending = "RunEndingCutOff",
        finish_reason = "max_tokens",
        cut_off = TRUE
      )
    )
  )
  cut_off <- scans_tool_result(
    tools$scans_get_findings,
    scans = list("cut_off_answer")
  )
  expect_identical(
    vapply(cut_off$data, \(row) row$trajectory_id, ""),
    "deputy/run-002"
  )
  expect_identical(cut_off$evidence[[1L]]$trajectory_id, "deputy/run-002")

  other <- scans_tools(TrajectoryBundle(
    data.frame(trajectory_id = "run-1", source_type = "manual"),
    data.frame(),
    data.frame()
  ))
  listed <- scans_tool_result(other$scans_list_trajectories)
  expect_false("run_ending" %in% names(listed$data[[1L]]))
})

test_that("Deputy scans assess runs read from OpenTelemetry spans", {
  skip_if_not_installed("jsonlite")
  bundle <- as_trajectory_otel(read_otel_traces(
    test_path("fixtures", "deputy", "otel")
  ))
  assessed <- assess_trajectory_scans(
    bundle,
    scans = c("cut_off_answer", "run_intervention")
  )
  findings <- assessed$findings
  kinds <- vapply(
    findings$value[findings$scan == "run_intervention"],
    \(value) value$kind,
    character(1)
  )

  expect_equal(
    findings$trajectory_id[findings$scan == "cut_off_answer"],
    "otel/c_cutoff"
  )
  expect_setequal(kinds, c("provider_fallback", "permission_denied"))
  expect_true(all(
    findings$event_ids[findings$scan == "run_intervention"] |>
      unlist() %in%
      trajectory_events(bundle)$event_id
  ))

  interventions <- assessed$assessments[
    assessed$assessments$scan == "run_intervention" &
      assessed$assessments$status != "not_applicable",
  ]
  expect_true(all(vapply(
    interventions$limitations,
    \(x) any(grepl("Deputy run spans do not export", x, fixed = TRUE)),
    logical(1)
  )))
})
