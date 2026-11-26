test_that("a custom scanner distinguishes clean, positive, and unassessed trajectories", {
  result <- assess_trajectory_scans(
    scanner_bundle_fixture(),
    scans = "toy_keyword",
    scanners = scanner_keyword_fixture()
  )
  assessments <- result$assessments

  expect_identical(
    stats::setNames(assessments$status, assessments$trajectory_id),
    c(
      "chat-hit" = "assessed_with_findings",
      "chat-clean" = "assessed_no_findings",
      "chat-empty" = "insufficient_evidence",
      "batch-job" = "not_applicable"
    )
  )
  expect_identical(
    assessments$reason[3:4],
    c(
      "No conversation events are recorded.",
      "Batch jobs have no customer conversation."
    )
  )
  expect_identical(unique(assessments$scan_version), "1")
  expect_identical(
    unique(assessments$required_evidence),
    "Recorded assistant text."
  )
  expect_identical(assessments$settings[[1L]], list(keyword = "guaranteed"))

  findings <- result$findings
  expect_identical(findings$trajectory_id, c("chat-hit", "chat-hit"))
  expect_identical(findings$event_id, c("hit-e2", "hit-e3"))
  expect_identical(findings$turn_id, c("hit-2", "hit-2"))
  expect_identical(findings$event_ids, list("hit-e2", "hit-e3"))
  expect_identical(findings$severity, c("warning", "warning"))
  expect_identical(assessments$finding_ids[[1L]], findings$finding_id)
  expect_identical(
    names(findings),
    names(scan_trajectories(scanner_bundle_fixture()))
  )
})

test_that("built-in results are identical with and without custom scanners", {
  bundle <- trajectory_fixture("tool_error")
  thrower <- scanner_toy_fixture("toy_thrower", function(trajectory, settings) {
    stop("boom")
  })
  scanners <- list(scanner_keyword_fixture(), thrower)
  plain <- assess_trajectory_scans(bundle)
  mixed <- assess_trajectory_scans(bundle, scanners = scanners)

  expect_gt(nrow(plain$findings), 0L)
  expect_identical(
    mixed$findings[seq_len(nrow(plain$findings)), ],
    plain$findings
  )
  expect_identical(
    mixed$assessments[seq_len(nrow(plain$assessments)), ],
    plain$assessments
  )
  expect_identical(
    mixed$assessments$scan[-seq_len(nrow(plain$assessments))],
    c("toy_keyword", "toy_thrower")
  )
  expect_identical(
    scan_trajectories(bundle, scanners = scanner_keyword_fixture()),
    scan_trajectories(bundle)
  )
  expect_identical(
    assess_trajectory_scans(bundle, scans = "event_error", scanners = scanners),
    assess_trajectory_scans(bundle, scans = "event_error")
  )
})

test_that("a scanner that throws fails alone and leaks no condition text", {
  thrower <- scanner_toy_fixture("toy_thrower", function(trajectory, settings) {
    if (trajectory$trajectory_id == "chat-clean") {
      stop("database password=hunter2")
    }
    scanner_findings()
  })
  result <- assess_trajectory_scans(
    scanner_bundle_fixture(),
    scans = c("toy_thrower", "toy_keyword"),
    scanners = list(thrower, scanner_keyword_fixture())
  )
  thrown <- result$assessments[result$assessments$scan == "toy_thrower", ]

  expect_identical(
    thrown$status,
    c(
      "assessed_no_findings",
      "execution_failure",
      "assessed_no_findings",
      "assessed_no_findings"
    )
  )
  expect_identical(
    thrown$reason[[2L]],
    "The custom scanner function failed. Other scanners were retained; fix the scanner and rerun."
  )
  expect_false(any(grepl("hunter2", unlist(result), fixed = TRUE)))
  expect_identical(
    result$assessments$status[result$assessments$scan == "toy_keyword"][[2L]],
    "assessed_no_findings"
  )

  error <- expect_error(
    scan_trajectories(scanner_bundle_fixture(), scanners = thrower),
    class = "scans_error_scanner_failure"
  )
  expect_match(conditionMessage(error), "toy_thrower.*chat-clean")
  expect_match(conditionMessage(error$parent), "hunter2")
})

test_that("a finding that cites evidence outside the trajectory is an execution failure", {
  citing <- function(...) {
    scanner_toy_fixture("toy_citing", function(trajectory, settings) {
      if (trajectory$trajectory_id != "chat-hit") {
        return(scanner_findings())
      }
      scanner_findings(
        event_id = "hit-e2",
        label = c("Real", "Stray"),
        explanation = "Cites evidence.",
        ...
      )
    })
  }
  bundle <- scanner_bundle_fixture()
  for (bad in list(
    citing(event_ids = list("hit-e2", c("hit-e2", "clean-e1"))),
    citing(turn_id = c("hit-2", "clean-2")),
    citing(event_ids = list(character(), "missing-event"))
  )) {
    result <- assess_trajectory_scans(
      bundle,
      scans = "toy_citing",
      scanners = bad
    )
    expect_identical(
      result$assessments$status,
      c(
        "execution_failure",
        "assessed_no_findings",
        "assessed_no_findings",
        "assessed_no_findings"
      )
    )
    expect_match(result$assessments$reason[[1L]], "not in this trajectory")
    expect_identical(nrow(result$findings), 0L)
    expect_identical(result$assessments$finding_ids[[1L]], character())
    expect_error(
      scan_trajectories(bundle, scanners = bad),
      class = "scans_error_scanner_failure"
    )
  }
  elsewhere <- scanner_toy_fixture(
    "toy_elsewhere",
    function(trajectory, settings) {
      data.frame(
        trajectory_id = "chat-clean",
        label = "Other",
        explanation = "Belongs to another trajectory."
      )
    }
  )
  result <- assess_trajectory_scans(
    bundle,
    scans = "toy_elsewhere",
    scanners = elsewhere
  )
  expect_identical(
    result$assessments$status == "execution_failure",
    c(TRUE, FALSE, TRUE, TRUE)
  )
})

test_that("a scanner that returns nothing usable is never a clean negative", {
  bundle <- scanner_bundle_fixture()
  for (value in list(
    NULL,
    TRUE,
    "none",
    list(),
    data.frame(label = "No explanation")
  )) {
    silent <- scanner_toy_fixture("toy_silent", function(trajectory, settings) {
      value
    })
    result <- assess_trajectory_scans(
      bundle,
      scans = "toy_silent",
      scanners = silent
    )
    expect_identical(unique(result$assessments$status), "execution_failure")
    expect_match(result$assessments$reason[[1L]], "not a findings table")
  }
})

test_that("custom names cannot collide with built-ins or each other", {
  expect_error(
    scanner("event_error", function(trajectory, settings) NULL, "d", "e"),
    class = "scans_error_scanner"
  )
  expect_error(
    scanner("Bad Name", function(trajectory, settings) NULL, "d", "e"),
    class = "scans_error_scanner"
  )
  expect_error(
    assess_trajectory_scans(
      scanner_bundle_fixture(),
      scanners = list(
        scanner_keyword_fixture(),
        scanner_keyword_fixture("late")
      )
    ),
    class = "scans_error_scanner"
  )
  # A built-in added after the scanner was created is caught at the call.
  later <- scanner_keyword_fixture()
  later$name <- "turn_error"
  expect_error(
    scan_trajectories(scanner_bundle_fixture(), scanners = later),
    class = "scans_error_scanner"
  )
  expect_error(
    assess_trajectory_scans(scanner_bundle_fixture(), scanners = list("toy")),
    class = "scans_error_scanner"
  )
})

test_that("a custom scanner runs only when it is supplied to the call", {
  bundle <- scanner_bundle_fixture()
  expect_error(
    assess_trajectory_scans(bundle, scans = "toy_keyword"),
    class = "scans_error_scan_selection"
  )
  expect_false(
    "toy_keyword" %in% assess_trajectory_scans(bundle)$assessments$scan
  )
  expect_false("toy_keyword" %in% scan_registry()$scan)

  listed <- scan_registry(scanner_keyword_fixture())
  expect_identical(listed[seq_len(nrow(scan_registry())), ], scan_registry())
  expect_identical(
    as.list(listed[nrow(listed), ]),
    list(
      scan = "toy_keyword",
      severity = "warning",
      description = "The assistant used a configured keyword."
    )
  )
})

test_that("version and settings are recorded and identities stay stable", {
  bundle <- scanner_bundle_fixture()
  first <- assess_trajectory_scans(bundle, scanners = scanner_keyword_fixture())
  second <- assess_trajectory_scans(
    bundle,
    scans = "toy_keyword",
    scanners = list(
      scanner_toy_fixture("toy_other", function(trajectory, settings) {
        scanner_findings(label = "Record", explanation = "No event.")
      }),
      scanner_keyword_fixture(version = "2")
    )
  )
  custom <- first$findings[first$findings$scan == "toy_keyword", ]

  expect_identical(
    custom$finding_id,
    c(
      "scan-000001/toy_keyword/finding-000001",
      "scan-000001/toy_keyword/finding-000002"
    )
  )
  expect_identical(second$findings$finding_id, custom$finding_id)
  expect_identical(unique(custom$scan_version), "1")
  expect_identical(unique(second$findings$scan_version), "2")
  expect_identical(unique(second$assessments$scan_version), "2")

  narrowed <- assess_trajectory_scans(
    bundle,
    scans = "toy_keyword",
    scanners = scanner_keyword_fixture(keyword = "on its way")
  )
  expect_identical(narrowed$findings$event_id, "clean-e2")
  expect_identical(
    narrowed$assessments$settings[[1L]],
    list(keyword = "on its way")
  )
})

test_that("a record-level finding has no event and still belongs to the trajectory", {
  record <- scanner_toy_fixture("toy_record", function(trajectory, settings) {
    scanner_findings(label = "Record", explanation = "About the whole run.")
  })
  findings <- scan_trajectories(
    scanner_bundle_fixture(),
    scans = "toy_record",
    scanners = record
  )
  expect_identical(
    findings$trajectory_id,
    trajectory_info(scanner_bundle_fixture())$trajectory_id
  )
  expect_true(all(is.na(findings$event_id)))
  expect_identical(findings$event_ids, rep(list(character()), 4L))
})

test_that("a scanner sees one trajectory's ordered rows and its losses", {
  seen <- new.env(parent = emptyenv())
  spy <- scanner_toy_fixture("toy_spy", function(trajectory, settings) {
    assign(trajectory$trajectory_id, trajectory, envir = seen)
    scanner_findings()
  })
  bundle <- trajectory_fixture("tool_error")
  scan_trajectories(bundle, scanners = spy)
  id <- trajectory_info(bundle)$trajectory_id[[1L]]
  got <- get(id, envir = seen)

  expect_named(
    got,
    c("trajectory_id", "info", "turns", "events", "evaluations", "losses")
  )
  expect_identical(got$info, trajectory_info(bundle)[1L, ])
  expect_identical(unique(got$events$trajectory_id), id)
  expect_false(is.unsorted(got$events$event_index))
  expect_identical(got$losses, trajectory_losses(bundle))
})

test_that("saved investigations keep custom results without the scanner function", {
  bundle <- scanner_bundle_fixture()
  saved <- investigation_snapshot(bundle, scanners = scanner_keyword_fixture())
  path <- tempfile(fileext = ".json")
  write_investigation(saved, path)
  reopened <- read_investigation(path)

  expect_identical(reopened, saved)
  custom <- reopened$analysis$assessments[
    reopened$analysis$assessments$scan == "toy_keyword",
  ]
  expect_identical(
    custom$status,
    c(
      "assessed_with_findings",
      "assessed_no_findings",
      "insufficient_evidence",
      "not_applicable"
    )
  )
  expect_identical(
    reopened$manifest$scanner_versions$scan_version[
      reopened$manifest$scanner_versions$scan == "toy_keyword"
    ],
    "1"
  )
  expect_snapshot(
    investigation_snapshot(reopened$bundle, scans = reopened$settings$scans),
    error = TRUE
  )
  rerun <- investigation_snapshot(
    reopened$bundle,
    scans = reopened$settings$scans,
    scanners = scanner_keyword_fixture(version = "2"),
    previous = reopened
  )
  expect_identical(rerun$manifest$snapshot_id, saved$manifest$snapshot_id)
  expect_false(identical(
    rerun$manifest$revision_id,
    saved$manifest$revision_id
  ))
  expect_identical(
    rerun$manifest$parent_revision_id,
    saved$manifest$revision_id
  )

  case <- review_case(
    reopened,
    "chat-hit",
    finding_id = "scan-000001/toy_keyword/finding-000001"
  )
  expect_identical(case$scan, "toy_keyword")
  expect_identical(case$event_ids, "hit-e2")
})

test_that("scans_tools lists custom findings and assessments", {
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- scanner_bundle_fixture()
  tools <- scans_tools(bundle, scanners = scanner_keyword_fixture())
  findings <- scans_tool_result(tools$scans_get_findings, scans = "toy_keyword")
  expect_identical(
    vapply(findings$data, `[[`, "", "event_id"),
    c("hit-e2", "hit-e3")
  )
  assessments <- scans_tool_result(
    tools$scans_get_findings,
    table = "assessments",
    scans = "toy_keyword"
  )
  expect_identical(
    vapply(assessments$data, `[[`, "", "status"),
    c(
      "assessed_with_findings",
      "assessed_no_findings",
      "insufficient_evidence",
      "not_applicable"
    )
  )

  saved <- investigation_snapshot(bundle, scanners = scanner_keyword_fixture())
  from_saved <- scans_tool_result(
    scans_tools(saved)$scans_get_findings,
    scans = "toy_keyword"
  )
  expect_identical(from_saved$data, findings$data)
  expect_error(
    scans_tools(saved, scanners = scanner_keyword_fixture()),
    class = "scans_error_tools_input"
  )
  expect_error(
    scans_tools(bundle)$scans_get_findings(scans = "toy_keyword"),
    class = "scans_error_scan_selection"
  )
})

test_that("the app runs custom scanners beside built-ins and shows their status", {
  bundle <- scanner_bundle_fixture()
  custom <- scanner_keyword_fixture()
  app <- scans_app(bundle, scanners = custom)
  expect_match(
    as.character(scans_app_ui(
      scans_app_sources(bundle),
      scanners = list(custom)
    )),
    'value="toy_keyword"',
    fixed = TRUE
  )
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    session$setInputs(
      scans_app_scans = scan_registry(custom)$scan,
      scans_app_repeat_threshold = 2L,
      scans_app_loop_threshold = 3L
    )
    expected <- assess_trajectory_scans(bundle, scanners = custom)
    expect_identical(data()$findings, expected$findings)
    expect_identical(data()$assessments, expected$assessments)
    expect_identical(
      data()$records$n_findings,
      c(2L, 0L, 0L, 0L)
    )
    expect_match(output$scans_app_scan_summary, "12 of 12 scans")
    html <- as.character(scans_app_assessments_ui(data(), "chat-empty"))
    expect_match(html, "toy keyword")
    expect_match(html, "No conversation events are recorded.")

    session$setInputs(scans_app_scans = "event_error")
    expect_identical(unique(data()$assessments$scan), "event_error")
  })
})

test_that("a reopened investigation names the custom scanners it cannot rerun", {
  saved <- investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = scanner_keyword_fixture()
  )
  app <- scans_app(saved)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    expect_identical(data()$findings, saved$analysis$findings)
    expect_match(
      output$scans_app_investigation_status$html,
      "or session: toy_keyword\\..*cannot rerun them"
    )
    session$setInputs(scans_app_rescan_investigation = 1L)
    expect_false("toy_keyword" %in% data()$assessments$scan)
    expect_match(
      output$scans_app_investigation_status$html,
      "toy_keyword\\. They were not rerun"
    )
  })

  with_scanner <- scans_app(
    saved,
    scanners = scanner_keyword_fixture(version = "2")
  )
  shiny::testServer(with_scanner$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    expect_no_match(
      output$scans_app_investigation_status$html,
      "unavailable in this installation"
    )
    session$setInputs(
      scans_app_scans = "toy_keyword",
      scans_app_rescan_investigation = 1L
    )
    expect_identical(unique(data()$findings$scan_version), "2")
  })
})

test_that("the copied R snippet says custom scanners must be supplied", {
  code <- export_r_code(
    NULL,
    scans_app_scan_config(c("event_error", "toy_keyword")),
    "chat-hit"
  )
  expect_match(
    paste(code, collapse = "\n"),
    "scanners = list(...)",
    fixed = TRUE
  )
  expect_no_match(
    paste(
      export_r_code(NULL, scans_app_scan_config("event_error"), "x"),
      collapse = "\n"
    ),
    "scanners"
  )
})
