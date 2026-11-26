domain_scanner_recipe <- function() {
  env <- new.env(parent = globalenv())
  for (file in c("domain-scanners.R", "domain-scanner-cases.R")) {
    sys.source(system.file("examples", file, package = "scans"), env)
  }
  env
}

domain_outcomes <- function(validation) {
  stats::setNames(
    validation$cases$outcome,
    sub("^[a-z]+/", "", validation$cases$trajectory_id)
  )
}

test_that("the unsupported action claim scanner matches its reviewed cases", {
  recipe <- domain_scanner_recipe()
  claims <- recipe$unsupported_action_claim_scanner()
  result <- assess_trajectory_scans(
    recipe$support_bundle(),
    scans = "unsupported_action_claim",
    scanners = claims
  )
  expect_identical(
    result$findings$event_id,
    paste0(
      "support/",
      c(
        "refund-failed/event-04",
        "refund-quoted",
        "cancel-none",
        "refund-policy"
      ),
      c("", rep("/event-02", 3L))
    )
  )
  expect_identical(
    result$findings$event_ids[[1L]],
    paste0("support/refund-failed/event-0", c(4, 2, 3))
  )
  expect_identical(unique(result$findings$severity), "error")

  validation <- validate_scanner(claims, recipe$support_reviewed_cases(claims))
  expect_identical(
    domain_outcomes(validation),
    c(
      "refund-done" = "true_negative",
      "refund-failed" = "true_positive",
      "refund-quoted" = "false_positive",
      "ticket-paraphrase" = "false_negative",
      "cancel-pending" = "reviewer_uncertain",
      "cancel-none" = "true_positive",
      "ticket-created" = "true_negative",
      "refund-untraced" = "scanner_unassessed",
      "refund-promised" = "false_negative",
      "refund-policy" = "false_positive"
    )
  )
  summary <- validation$summary
  expect_identical(summary$scored, c(4L, 4L))
  expect_identical(summary$precision, c(0.5, 0.5))
  expect_identical(summary$recall, c(0.5, 0.5))
  expect_identical(summary$insufficient_evidence, c(0L, 1L))
  expect_identical(summary$reviewer_uncertain, c(1L, 0L))
})

test_that("an action claim is judged only on evidence that can support it", {
  recipe <- domain_scanner_recipe()
  claims <- recipe$unsupported_action_claim_scanner(
    list(export = list(tools = "export_data", claim = "export is ready"))
  )
  status <- function(...) {
    bundle <- recipe$domain_bundle(list("chat" = list(...)))
    assess_trajectory_scans(bundle, scans = claims$name, scanners = claims)
  }
  call <- recipe$domain_tool_call("export_data", "call-1")
  done <- recipe$domain_tool_result(
    "export_data",
    "call-1",
    list(file = "a.csv")
  )
  claim <- recipe$domain_assistant("Your export is ready.")

  expect_identical(
    status(call, done, claim)$assessments$status,
    "assessed_no_findings"
  )
  late <- status(claim, call, done)
  expect_identical(late$assessments$status, "assessed_with_findings")
  expect_identical(late$findings$value[[1L]]$attempted_calls, 0L)
  expect_identical(
    status(
      recipe$domain_tool_call("export_data", NA_character_),
      claim
    )$assessments$status,
    "insufficient_evidence"
  )
  expect_identical(
    status(recipe$domain_user("Export my data."))$assessments$status,
    "insufficient_evidence"
  )
  expect_identical(
    status(recipe$domain_assistant(
      "Your refund has been issued."
    ))$assessments$status,
    "assessed_no_findings"
  )
})

test_that("the answer without support scanner matches its reviewed cases", {
  recipe <- domain_scanner_recipe()
  unsupported <- recipe$answer_without_support_scanner()
  result <- assess_trajectory_scans(
    recipe$retrieval_bundle(),
    scans = "answer_without_support",
    scanners = unsupported
  )
  by_provenance <- result$findings[
    result$findings$trajectory_id == "docs/provenance-untrusted",
  ]
  expect_identical(by_provenance$value[[1L]]$basis, "provenance")
  expect_identical(
    by_provenance$event_ids[[1L]],
    paste0("docs/provenance-untrusted/event-0", 2:3)
  )

  validation <- validate_scanner(
    unsupported,
    recipe$retrieval_reviewed_cases(unsupported)
  )
  expect_identical(
    domain_outcomes(validation),
    c(
      "found" = "true_negative",
      "empty-answered" = "true_positive",
      "empty-declined" = "true_negative",
      "failed-short" = "false_negative",
      "empty-clarify" = "false_positive",
      "empty-hedged" = "reviewer_uncertain",
      "provenance-untrusted" = "true_positive",
      "provenance-trusted" = "true_negative",
      "untraced" = "scanner_unassessed",
      "failed-answered" = "true_positive",
      "greeting" = "scanner_unassessed",
      "retry-recovered" = "true_negative",
      "no-hits-message" = "false_negative"
    )
  )
  summary <- validation$summary
  expect_identical(summary$n_cases, c(6L, 7L))
  expect_identical(summary$precision_denominator, c(2L, 2L))
  expect_identical(summary$recall_denominator, c(2L, 3L))
  expect_identical(summary$insufficient_evidence, c(0L, 1L))
  expect_identical(summary$not_applicable, c(0L, 1L))
})

test_that("provenance without a recognised tag cannot support a clean result", {
  recipe <- domain_scanner_recipe()
  unsupported <- recipe$answer_without_support_scanner()
  bundle <- recipe$domain_bundle(list(
    "chat" = list(
      recipe$domain_user("How much leave can I carry over?"),
      recipe$domain_assistant(strrep("Five days may be carried over. ", 4L)),
      recipe$domain_provenance(NA_character_)
    )
  ))
  result <- assess_trajectory_scans(
    bundle,
    scans = unsupported$name,
    scanners = unsupported
  )
  expect_identical(result$assessments$status, "insufficient_evidence")
})

test_that("both recipe scanners run beside the built-in scans and save", {
  recipe <- domain_scanner_recipe()
  scanners <- list(
    recipe$unsupported_action_claim_scanner(),
    recipe$answer_without_support_scanner()
  )
  bundle <- recipe$support_bundle()
  saved <- investigation_snapshot(bundle, scanners = scanners)
  path <- tempfile(fileext = ".json")
  write_investigation(saved, path)

  expect_identical(read_investigation(path), saved)
  expect_identical(
    saved$settings$scans,
    c(
      scan_registry()$scan,
      "unsupported_action_claim",
      "answer_without_support"
    )
  )
  builtin <- assess_trajectory_scans(bundle)
  expect_identical(
    saved$analysis$findings[seq_len(nrow(builtin$findings)), ],
    builtin$findings
  )
})
