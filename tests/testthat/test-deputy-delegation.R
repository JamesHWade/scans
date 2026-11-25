test_that("Deputy delegation history passes the shared adapter contract", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  fixture <- deputy_delegation_fixture()
  expect_adapter_conforms(
    source = fixture$lead,
    adapter = as_trajectory_deputy,
    delegations = fixture$history
  )
  expect_adapter_conforms(
    source = fixture$history,
    adapter = as_trajectory_deputy
  )
})

test_that("a lead result and its delegations link through Deputy lineage", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  fixture <- deputy_delegation_fixture()
  bundle <- as_trajectory_deputy(fixture$lead, delegations = fixture$history)
  info <- trajectory_info(bundle)

  expect_identical(
    info$trajectory_id,
    c("deputy/run-001", "deputy/run-child-001", "deputy/run-child-002")
  )
  expect_identical(
    info$parent_trajectory_id,
    c(NA, "deputy/run-001", "deputy/run-child-001")
  )
  expect_identical(info$source_type, rep("deputy", 3L))
  expect_identical(info$status, rep("completed", 3L))
  expect_identical(info$model[-1L], c("gpt-test", "gpt-test"))

  child <- info$metadata[[2L]]
  expect_identical(child$source_class, "delegation_view")
  expect_identical(child$delegation_id, "delegation-001")
  expect_identical(child$usage$input_tokens, 5)
  expect_identical(
    child$delegation$manifest[c("schema_version", "provider", "tools")],
    list(schema_version = 2L, provider = "OpenAI", tools = "search")
  )
  expect_identical(child$delegation$manifest$params, list(temperature = 0))
  expect_null(child$delegation$manifest$system_prompt)
  expect_identical(
    child$delegation$claims,
    list(
      missing_evidence = "No 2024 figures were available",
      unresolved_work = NA_character_
    )
  )
  reference <- child$delegation$references[[1L]]
  expect_identical(reference$reference, deputy_offload_reference)
  expect_identical(reference$availability, "unresolved")

  nested <- info$metadata[[3L]]
  expect_identical(nested$parent_delegation_id, "delegation-001")
  expect_identical(nested$delegation$runtime$depth, 2L)
  expect_identical(
    nested$delegation$claims,
    list(missing_evidence = NA_character_, unresolved_work = "")
  )

  losses <- trajectory_losses(bundle)
  expect_identical(
    losses[c("trajectory_id", "field", "reason")],
    tibble::tibble(
      trajectory_id = c("deputy/run-child-001", "deputy/run-child-002"),
      field = c("contents$value", "capture$transcript"),
      reason = c("externalized", "redacted")
    )
  )
  events <- trajectory_events(bundle)
  offloaded <- events[events$event_id == losses$event_id[[1L]], ]
  expect_identical(offloaded$event_type, "tool_result")
  expect_identical(offloaded$call_id, "call-search-001")
  expect_identical(losses$metadata[[1L]]$reference, deputy_offload_reference)
  expect_false(any(events$trajectory_id == "deputy/run-child-002"))
})

test_that("standalone delegation history keeps unresolved parents explicit", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  bundle <- as_trajectory_deputy(deputy_delegation_fixture()$history)
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(info$parent_trajectory_id, c(NA, "deputy/run-child-001"))
  expect_identical(info$metadata[[1L]]$parent_run_id, "run-001")
  expect_identical(
    losses$trajectory_id[
      losses$field %in% c("parent_run_id", "parent_delegation_id")
    ],
    "deputy/run-child-001"
  )
})

test_that("Deputy delegation views must come through Deputy's disclosure", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  fixture <- deputy_delegation_fixture()

  expect_adapter_rejects(
    fixture$export,
    as_trajectory_deputy,
    "scans_error_deputy_source"
  )
  expect_adapter_rejects(
    fixture$export$children,
    as_trajectory_deputy,
    "scans_error_deputy_source"
  )
  expect_adapter_rejects(
    fixture$lead,
    \(x) as_trajectory_deputy(x, delegations = fixture$export),
    "scans_error_deputy_source"
  )
  expect_adapter_rejects(
    fixture$history,
    \(x) as_trajectory_deputy(x, delegations = fixture$history),
    "scans_error_deputy_argument"
  )
  expect_adapter_rejects(
    fixture$lead,
    \(x) as_trajectory_deputy(x, "lead", delegations = fixture$history),
    "scans_error_deputy_argument"
  )
  expect_adapter_rejects(
    fixture$history,
    as_trajectory,
    "scans_error_unsupported_source"
  )
})

test_that("Deputy delegations repeated in one call are rejected", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  history <- deputy_delegation_fixture()$history
  expect_adapter_rejects(
    c(history, history[1L]),
    as_trajectory_deputy,
    "scans_error_deputy_identity"
  )
})

test_that("Deputy delegation outcomes set honest statuses and losses", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  view <- function(delegation_id, run_id, status) {
    deputy_export_child(delegation_id, run_id, NULL, status = status)
  }
  views <- deputy_read_history(deputy_export_fixture(list(
    view("delegation-failed", "run-failed", "failed"),
    view("delegation-stopped", "run-stopped", "stopped"),
    view("delegation-suspended", "run-suspended", "suspended"),
    view("delegation-unstarted", NULL, "not_started")
  )))
  bundle <- as_trajectory_deputy(views)
  info <- trajectory_info(bundle)

  expect_identical(
    info$status,
    c("failed", "interrupted", "interrupted", "cancelled")
  )
  expect_identical(
    info$trajectory_id[[4L]],
    "deputy/delegation/delegation-unstarted"
  )
  expect_identical(
    trajectory_losses(bundle)$field,
    "run_id"
  )

  bounded <- views[[1L]]
  bounded$outcome$runtime$answer_truncated <- TRUE
  bounded$outcome$runtime$claim_omissions <- list(
    missing_evidence = list(omitted_entries = 3L, text_truncated = FALSE),
    unresolved_work = list(omitted_entries = 0L, text_truncated = FALSE)
  )
  bounded$transcript <- NULL
  bounded$turns <- NULL
  bounded$retention$transcript <- "not_requested"
  losses <- trajectory_losses(as_trajectory_deputy(list(bounded)))

  expect_identical(
    losses[c("field", "reason")],
    tibble::tibble(
      field = c(
        "capture$transcript",
        "metadata$delegation$answer",
        "metadata$delegation$claims$missing_evidence"
      ),
      reason = c("unsupported", "truncated", "truncated")
    )
  )
})

test_that("offloaded tool results in a Deputy result are externalized", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  request <- ellmer::ContentToolRequest(
    id = "call-001",
    name = "search",
    arguments = list(query = "figures")
  )
  result <- deputy_result_fixture(
    overrides = list(
      turns = list(
        ellmer::UserTurn(list(ellmer::ContentText("Find the figures"))),
        ellmer::AssistantTurn(list(request)),
        ellmer::UserTurn(list(
          ellmer::ContentToolResult(
            value = deputy_offloaded_text(),
            request = request
          )
        ))
      )
    )
  )
  losses <- trajectory_losses(as_trajectory_deputy(result))

  expect_identical(losses$field, "contents$value")
  expect_identical(losses$reason, "externalized")
  expect_identical(losses$event_id, "deputy/run-001/event-000003")
})
