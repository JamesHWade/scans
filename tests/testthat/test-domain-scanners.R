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
      "refund-policy" = "false_positive",
      "refund-not-yet" = "true_negative",
      "cancel-question" = "true_negative"
    )
  )
  summary <- validation$summary
  expect_identical(summary$scored, c(6L, 4L))
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

test_that("an action result after the claim does not support it", {
  recipe <- domain_scanner_recipe()
  claims <- recipe$unsupported_action_claim_scanner(
    list(export = list(tools = "export_data", claim = "export is ready"))
  )
  bundle <- recipe$domain_bundle(list(
    "chat" = list(
      recipe$domain_tool_call("export_data", "call-1"),
      recipe$domain_assistant("Your export is ready."),
      recipe$domain_tool_result("export_data", "call-1", list(file = "a.csv"))
    )
  ))
  result <- assess_trajectory_scans(
    bundle,
    scans = claims$name,
    scanners = claims
  )
  expect_identical(result$assessments$status, "assessed_with_findings")
})

test_that("a result is linked to its call only unambiguously", {
  recipe <- domain_scanner_recipe()
  claims <- recipe$unsupported_action_claim_scanner(
    list(export = list(tools = "export_data", claim = "export is ready"))
  )
  status <- function(...) {
    bundle <- recipe$domain_bundle(list("chat" = list(...)))
    assess_trajectory_scans(
      bundle,
      scans = claims$name,
      scanners = claims
    )$assessments$status
  }
  claim <- recipe$domain_assistant("Your export is ready.")
  # Another tool's call and result reuse the identity.
  expect_identical(
    status(
      recipe$domain_tool_call("export_data", "call-1"),
      recipe$domain_tool_call("send_mail", "call-1"),
      recipe$domain_tool_result("send_mail", "call-1", list(sent = TRUE)),
      claim
    ),
    "insufficient_evidence"
  )
  # A result that names another tool does not belong to the call.
  expect_identical(
    status(
      recipe$domain_tool_call("export_data", "call-1"),
      recipe$domain_tool_result("send_mail", "call-1", list(sent = TRUE)),
      claim
    ),
    "insufficient_evidence"
  )
  # An explicit parent link resolves a reused identity.
  linked <- recipe$domain_tool_result("export_data", "call-1", list(a = 1))
  linked$parent_event_id <- "chat/event-01"
  expect_identical(
    status(
      recipe$domain_tool_call("export_data", "call-1"),
      recipe$domain_tool_call("send_mail", "call-1"),
      linked,
      claim
    ),
    "assessed_no_findings"
  )
})

test_that("an action result without recorded completion is unknown", {
  recipe <- domain_scanner_recipe()
  claims <- recipe$unsupported_action_claim_scanner(
    list(export = list(tools = "export_data", claim = "export is ready"))
  )
  for (state in c("interrupted", "running", "pending", NA_character_)) {
    result <- recipe$domain_tool_result("export_data", "call-1", list(a = 1))
    result["status"] <- list(state)
    bundle <- recipe$domain_bundle(list(
      "chat" = list(
        recipe$domain_tool_call("export_data", "call-1"),
        result,
        recipe$domain_assistant("Your export is ready.")
      )
    ))
    expect_identical(
      assess_trajectory_scans(
        bundle,
        scans = claims$name,
        scanners = claims
      )$assessments$status,
      "insufficient_evidence"
    )
  }
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

test_that("the Connect app forwards custom scanners to the app", {
  expect_error(
    scans_app_connect(
      c(App = "11111111-1111-4111-8111-111111111111"),
      scanners = "claims"
    ),
    class = "scans_error_scanner"
  )
})

domain_retrieval_status <- function(recipe, ..., losses = NULL) {
  unsupported <- recipe$answer_without_support_scanner()
  bundle <- recipe$domain_bundle(list("chat" = list(...)))
  if (!is.null(losses)) {
    data <- S7::props(bundle)
    data$schema_version <- NULL
    data$losses <- losses
    bundle <- do.call(TrajectoryBundle, data)
  }
  result <- assess_trajectory_scans(
    bundle,
    scans = unsupported$name,
    scanners = unsupported
  )
  result$assessments$status
}

domain_long_answer <- paste(
  "Employees may carry over up to five days of unused annual leave into",
  "the next calendar year, with approval."
)

test_that("only a retrieval that succeeds before the answer supports it", {
  recipe <- domain_scanner_recipe()
  search <- function(id) recipe$domain_tool_call("search_docs", id)
  found <- list(documents = list(list(title = "Leave policy")))
  expect_identical(
    domain_retrieval_status(
      recipe,
      recipe$domain_user("How much leave can I carry over?"),
      search("call-1"),
      recipe$domain_tool_result("search_docs", "call-1", list()),
      recipe$domain_assistant(domain_long_answer),
      search("call-2"),
      recipe$domain_tool_result("search_docs", "call-2", found)
    ),
    "assessed_with_findings"
  )
})

test_that("a retrieval without recorded completion leaves the answer unsettled", {
  recipe <- domain_scanner_recipe()
  question <- recipe$domain_user("How much leave can I carry over?")
  search <- function(id) recipe$domain_tool_call("search_docs", id)
  for (state in c("interrupted", "running", "pending", NA_character_)) {
    retry <- recipe$domain_tool_result(
      "search_docs",
      "call-2",
      list(documents = list(list(title = "Leave policy")))
    )
    retry["status"] <- list(state)
    expect_identical(
      domain_retrieval_status(
        recipe,
        question,
        search("call-1"),
        recipe$domain_tool_result("search_docs", "call-1", list()),
        search("call-2"),
        retry,
        recipe$domain_assistant(domain_long_answer)
      ),
      "insufficient_evidence"
    )
  }
})

test_that("an answer after a retrieval with an unknown outcome is unsettled", {
  recipe <- domain_scanner_recipe()
  question <- recipe$domain_user("How much leave can I carry over?")
  for (state in c("interrupted", "running", "pending", NA_character_)) {
    only <- recipe$domain_tool_result(
      "search_docs",
      "call-1",
      list(documents = list(list(title = "Leave policy")))
    )
    only["status"] <- list(state)
    expect_identical(
      domain_retrieval_status(
        recipe,
        question,
        recipe$domain_tool_call("search_docs", "call-1"),
        only,
        recipe$domain_assistant(domain_long_answer)
      ),
      "insufficient_evidence"
    )
  }
})

test_that("an answer after an empty retrieval needs a capture that kept retries", {
  recipe <- domain_scanner_recipe()
  question <- recipe$domain_user("How much leave can I carry over?")
  steps <- list(
    question,
    recipe$domain_tool_call("search_docs", "call-1"),
    recipe$domain_tool_result("search_docs", "call-1", list()),
    recipe$domain_assistant(domain_long_answer)
  )
  expect_identical(
    do.call(domain_retrieval_status, c(list(recipe), steps)),
    "assessed_with_findings"
  )
  expect_identical(
    do.call(
      domain_retrieval_status,
      c(
        list(recipe),
        steps,
        list(
          losses = tibble::tibble(
            trajectory_id = "chat",
            field = "events",
            reason = "unsupported",
            detail = "Some events were not exported"
          )
        )
      )
    ),
    "insufficient_evidence"
  )
})

test_that("retrieval calls are found by name and results by call identity", {
  recipe <- domain_scanner_recipe()
  question <- recipe$domain_user("How much leave can I carry over?")
  answer <- recipe$domain_assistant(domain_long_answer)
  search <- recipe$domain_tool_call("search_docs", "call-1")
  unnamed <- list(
    role = "user",
    event_type = "tool_result",
    call_id = "call-1",
    value = list(),
    status = "completed",
    error = NA
  )
  expect_identical(
    domain_retrieval_status(recipe, question, search, unnamed, answer),
    "assessed_with_findings"
  )
  expect_identical(
    domain_retrieval_status(recipe, question, search, answer),
    "insufficient_evidence"
  )
  expect_identical(
    domain_retrieval_status(
      recipe,
      question,
      recipe$domain_tool_call("search_docs", NA_character_),
      answer
    ),
    "insufficient_evidence"
  )
})

test_that("a provenance loss means missing provenance is not evidence", {
  recipe <- domain_scanner_recipe()
  expect_identical(
    domain_retrieval_status(
      recipe,
      recipe$domain_user("How much leave can I carry over?"),
      recipe$domain_assistant(domain_long_answer),
      losses = tibble::tibble(
        trajectory_id = "chat",
        field = "provenance",
        reason = "unsupported",
        detail = "The commons conversation has no provenance records"
      )
    ),
    "insufficient_evidence"
  )
})

test_that("a blank retrieval result is not usable content", {
  recipe <- domain_scanner_recipe()
  for (blank in list("", "   ", "[]", list(documents = list()))) {
    expect_identical(
      domain_retrieval_status(
        recipe,
        recipe$domain_user("How much leave can I carry over?"),
        recipe$domain_tool_call("search_docs", "call-1"),
        recipe$domain_tool_result("search_docs", "call-1", blank),
        recipe$domain_assistant(domain_long_answer)
      ),
      "assessed_with_findings"
    )
  }
})

test_that("provenance is read against its exchange, not the nth user turn", {
  recipe <- domain_scanner_recipe()
  unsupported <- recipe$answer_without_support_scanner()
  bundle <- recipe$domain_bundle(list(
    "chat" = list(
      recipe$domain_user("How much leave can I carry over?"),
      recipe$domain_assistant(domain_long_answer),
      recipe$domain_user("[internal reminder: cite your sources]"),
      recipe$domain_assistant("Noted."),
      recipe$domain_user("And how much can I buy?"),
      recipe$domain_assistant(domain_long_answer),
      recipe$domain_provenance("A", 1L),
      recipe$domain_provenance("C", 2L)
    )
  ))
  data <- S7::props(bundle)
  data$schema_version <- NULL
  data$turns$metadata[[3L]] <- list(commons_internal = TRUE)
  derived <- do.call(TrajectoryBundle, data)
  data$turns$metadata[[3L]] <- list()
  data$turns$round_index <- c(1L, 1L, 1L, 1L, 2L, 2L)
  rounds <- do.call(TrajectoryBundle, data)

  for (x in list(derived, rounds)) {
    result <- assess_trajectory_scans(
      x,
      scans = unsupported$name,
      scanners = unsupported
    )
    expect_identical(result$findings$event_id, "chat/event-06")
    expect_identical(
      result$findings$event_ids[[1L]],
      c("chat/event-06", "chat/event-08")
    )
  }
})

test_that("a malformed provenance exchange is unclassified, not a crash", {
  recipe <- domain_scanner_recipe()
  for (exchange in list(0L, -1L, 1.5, "1", NA_integer_, c(1L, 2L))) {
    record <- recipe$domain_provenance("C")
    record$metadata$exchange_index <- exchange
    expect_identical(
      domain_retrieval_status(
        recipe,
        recipe$domain_user("How much leave can I carry over?"),
        recipe$domain_assistant(domain_long_answer),
        record
      ),
      "insufficient_evidence"
    )
  }
})

test_that("negated and interrogative action phrases are not claims", {
  recipe <- domain_scanner_recipe()
  claims <- recipe$unsupported_action_claim_scanner()
  findings <- function(text) {
    bundle <- recipe$domain_bundle(list(
      "chat" = list(
        recipe$domain_user("Please refund order A-1."),
        recipe$domain_assistant(text)
      )
    ))
    nrow(
      assess_trajectory_scans(
        bundle,
        scans = claims$name,
        scanners = claims
      )$findings
    )
  }
  expect_identical(
    findings("I have not issued a refund yet; billing must approve it first."),
    0L
  )
  expect_identical(findings("I haven't processed the refund yet."), 0L)
  expect_identical(
    findings("Have you already cancelled the order yourself?"),
    0L
  )
  expect_identical(
    findings("Your order has been cancelled. Is there anything else?"),
    1L
  )
  # Phrase matching stays approximate: a conditional still reads as a claim.
  expect_identical(
    findings("Once your refund is processed you will get an email."),
    1L
  )
})

test_that("another tool's result does not count as retrieval", {
  recipe <- domain_scanner_recipe()
  question <- recipe$domain_user("How much leave can I carry over?")
  search <- recipe$domain_tool_call("search_docs", "call-1")
  mail <- recipe$domain_tool_call("send_mail", "call-1")
  mailed <- recipe$domain_tool_result("send_mail", "call-1", list(sent = TRUE))
  answer <- recipe$domain_assistant(domain_long_answer)
  empty <- recipe$domain_tool_result("search_docs", "call-1", list())
  expect_identical(
    domain_retrieval_status(
      recipe,
      question,
      search,
      empty,
      mail,
      mailed,
      answer
    ),
    "insufficient_evidence"
  )
  empty$parent_event_id <- "chat/event-02"
  expect_identical(
    domain_retrieval_status(
      recipe,
      question,
      search,
      empty,
      mail,
      mailed,
      answer
    ),
    "assessed_with_findings"
  )
})
