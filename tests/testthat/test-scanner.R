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
  expect_no_match(conditionMessage(error), "retained")
  expect_match(conditionMessage(error), "assess_trajectory_scans")
  expect_match(conditionMessage(error$parent), "hunter2")
})

test_that("a condition raised with stop() fails alone; warnings and messages pass", {
  signal <- structure(
    class = c("toy_condition", "condition"),
    list(message = "token=secret", call = NULL)
  )
  stopper <- scanner_toy_fixture("toy_stopper", function(trajectory, settings) {
    signalCondition(signal)
    if (trajectory$trajectory_id == "chat-clean") {
      stop(signal)
    }
    scanner_findings()
  })
  result <- assess_trajectory_scans(
    scanner_bundle_fixture(),
    scans = "toy_stopper",
    scanners = stopper
  )
  expect_identical(
    result$assessments$status,
    c(
      "assessed_no_findings",
      "execution_failure",
      "assessed_no_findings",
      "assessed_no_findings"
    )
  )
  expect_false(any(grepl("secret", unlist(result), fixed = TRUE)))
  error <- expect_error(
    scan_trajectories(scanner_bundle_fixture(), scanners = stopper),
    class = "scans_error_scanner_failure"
  )
  expect_s3_class(error$parent$parent, "toy_condition")

  noisy <- scanner_toy_fixture("toy_noisy", function(trajectory, settings) {
    if (trajectory$trajectory_id == "chat-hit") {
      warning("toy warning")
      message("toy message")
    }
    scanner_findings()
  })
  expect_warning(
    expect_message(
      result <- assess_trajectory_scans(
        scanner_bundle_fixture(),
        scans = "toy_noisy",
        scanners = noisy
      ),
      "toy message"
    ),
    "toy warning"
  )
  expect_identical(unique(result$assessments$status), "assessed_no_findings")
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
    citing(event_ids = list(character(), "missing-event")),
    citing(turn_id = "hit-1")
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
      found <- scanner_findings(
        label = "Other",
        explanation = "Belongs to another trajectory."
      )
      found$trajectory_id <- "chat-clean"
      found
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
  forged <- function(...) {
    structure(list(...), class = "scans_scanner_unassessed")
  }
  for (value in list(
    NULL,
    TRUE,
    "none",
    list(),
    data.frame(label = "No explanation"),
    data.frame(),
    tibble::tibble(event_id = character(), label = character()),
    tibble::tibble(event_id = "hit-e2", label = "Bare", explanation = "Bare."),
    forged(status = "assessed_no_findings", reason = "Looks clean."),
    forged(status = "execution_failure", reason = "password=hunter2"),
    forged(status = "whatever", reason = "Unknown status."),
    forged(status = "not_applicable", reason = c("One.", "Two.", "Three.")),
    forged(status = "not_applicable"),
    forged(status = "not_applicable", reason = NA_character_),
    forged(status = "not_applicable", reason = strrep("x", 2000L))
  )) {
    silent <- scanner_toy_fixture("toy_silent", function(trajectory, settings) {
      value
    })
    result <- assess_trajectory_scans(
      bundle,
      scans = "toy_silent",
      scanners = silent
    )
    expect_identical(nrow(result$assessments), 4L)
    expect_identical(unique(result$assessments$status), "execution_failure")
    expect_identical(
      unique(result$assessments$reason),
      "The custom scanner returned a value that is not a findings table or an unassessed result."
    )
  }
  expect_error(
    scanner_unassessed("not_applicable", strrep("x", 501L)),
    class = "scans_error_scanner"
  )
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
  # Rows are stored out of order and interleaved across trajectories, with a
  # loss for each trajectory, one owned through a turn, and one unassigned.
  bundle <- TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = c("run-a", "run-b"),
      source_type = "manual"
    ),
    turns = tibble::tibble(
      trajectory_id = c("run-b", "run-a", "run-b", "run-a"),
      turn_id = c("b-2", "a-1", "b-1", "a-2"),
      turn_index = c(2L, 1L, 1L, 2L),
      role = c("assistant", "user", "user", "assistant")
    ),
    events = tibble::tibble(
      trajectory_id = c("run-b", "run-a", "run-b", "run-a", "run-b"),
      event_id = c("b-e3", "a-e2", "b-e1", "a-e1", "b-e2"),
      event_index = c(3L, 2L, 1L, 1L, 2L),
      turn_id = c("b-2", "a-2", "b-1", "a-1", "b-2"),
      event_type = "content",
      text = "x"
    ),
    losses = tibble::tibble(
      trajectory_id = c("run-a", NA, "run-b", NA),
      turn_id = c(NA, "b-2", NA, NA),
      field = "usage",
      reason = "unsupported",
      detail = c("a", "b turn", "b", "unassigned")
    )
  )
  scan_trajectories(bundle, scanners = spy)
  got <- get("run-b", envir = seen)

  expect_named(
    got,
    c("trajectory_id", "info", "turns", "events", "evaluations", "losses")
  )
  expect_identical(got$info, trajectory_info(bundle)[2L, ])
  expect_identical(got$turns$turn_id, c("b-1", "b-2"))
  expect_identical(got$events$event_id, c("b-e1", "b-e2", "b-e3"))
  expect_identical(got$losses$detail, c("b turn", "b", "unassigned"))
  expect_identical(
    get("run-a", envir = seen)$losses$detail,
    c("a", "unassigned")
  )
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

test_that("the app's report on saved custom results is the script's report", {
  saved <- investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = scanner_keyword_fixture()
  )
  untimed <- function(text) sub("Generated [0-9: -]+ UTC", "", text)
  script <- untimed(format(investigation_report(
    saved,
    app_url = "http://mocksession:1234/mockpath"
  )))
  for (scanners in list(NULL, scanner_keyword_fixture())) {
    app <- scans_app(saved, scanners = scanners)
    shiny::testServer(app$serverFuncSource(), {
      session$flushReact()
      investigation_ack_inputs(session)
      session$setInputs(scans_app_export_report = 1L)
      expect_identical(untimed(export_text()), script)
    })
  }

  # Applying current scanners without the custom one leaves it out of the
  # analysis, and the report then describes built-in scans only.
  app <- scans_app(saved)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_rescan_investigation = 1L)
    session$setInputs(scans_app_export_report = 1L)
    text <- export_text()
    expect_false(any(grepl("toy", text, ignore.case = TRUE)))
    expect_false(any(grepl("custom scanner", text, fixed = TRUE)))
  })
})

test_that("a live app's report counts failed scans as the overview does", {
  boom <- scanner_toy_fixture("toy_boom", function(trajectory, settings) {
    if (trajectory$trajectory_id %in% c("chat-hit", "chat-clean")) {
      stop("boom")
    }
    scanner_findings()
  })
  app <- scans_app(scanner_bundle_fixture(), scanners = boom)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    session$setInputs(
      scans_app_scans = scan_registry(boom)$scan,
      scans_app_repeat_threshold = 2L,
      scans_app_loop_threshold = 3L
    )
    expect_match(
      output$scans_app_performance$html,
      "2 with an execution failure",
      fixed = TRUE
    )
    session$setInputs(scans_app_export_report = 1L)
    expect_identical(report()$scope$custom_scans, "toy_boom")
    text <- export_text()
    expect_true(
      "- 2 trajectories had a scan that failed to run; that scan's result for them is unknown." %in%
        text
    )
    expect_true("| toy boom | 2 | 0 | 0 | 0 | 2 |" %in% text)
  })
})

test_that("scanners the app could never run are refused", {
  saved <- investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = scanner_keyword_fixture()
  )
  expect_error(
    scans_app(
      saved,
      investigations = FALSE,
      scanners = scanner_keyword_fixture()
    ),
    class = "scans_error_app_scanners"
  )
  expect_no_error(scans_app(saved, investigations = FALSE))
  expect_no_error(scans_app(
    list(Saved = saved, Live = scanner_bundle_fixture()),
    scanners = scanner_keyword_fixture()
  ))
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

test_that("custom scan names are labelled in every export and the review queue", {
  other <- scanner_toy_fixture("toy_other", function(trajectory, settings) {
    switch(
      trajectory$trajectory_id,
      "chat-hit" = scanner_findings(
        event_id = "hit-e3",
        label = "Other",
        explanation = "Second scanner."
      ),
      "chat-clean" = stop("boom"),
      "chat-empty" = scanner_unassessed("insufficient_evidence", "None."),
      scanner_findings()
    )
  })
  data <- scans_app_data(
    scanner_bundle_fixture(),
    scanners = list(scanner_keyword_fixture(), other)
  )
  ids <- data$info$trajectory_id

  expect_identical(
    cohort_scan_word(c("toy_keyword", "toy_other", "event_error"), "label"),
    c("Toy keyword", "Toy other", "Event errors")
  )
  # The report labels its pattern, evidence, and coverage rows for both
  # custom scans, in Markdown and in HTML.
  report <- investigation_report(investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = list(scanner_keyword_fixture(), other)
  ))
  expect_identical(report$patterns$label, c("Toy keyword", "Toy other"))
  md <- format(report)
  expect_true(all(
    c(
      "| [Toy keyword](<#pattern-1>) | warning | 1 of 2 assessed | 2 of 4 | 2 | unknown | unknown | unknown | 2 findings |",
      "| [Toy other](<#pattern-2>) | warning | 1 of 2 assessed | 2 of 4 | 1 | unknown | unknown | unknown | 1 finding |",
      "### Toy keyword",
      "### Toy other",
      "`toy_other`: 1 of 2 assessed trajectories, 1 finding.",
      "| toy keyword | 1 | 1 | 1 | 1 | 0 |",
      "| toy other | 1 | 1 | 1 | 0 | 1 |"
    ) %in%
      md
  ))
  if (rlang::is_installed("htmltools")) {
    html <- format(report, "html")
    for (text in c(
      "<a href=\"#pattern-2\">Toy other</a></td>",
      "<h3 id=\"pattern-1\">Toy keyword</h3>",
      "<h3 id=\"pattern-2\">Toy other</h3>",
      "<tr><td>toy other</td><td>1</td><td>1</td><td>1</td><td>0</td><td>1</td></tr>"
    )) {
      expect_match(html, text, fixed = TRUE)
    }
  }
  markdown <- export_trajectory_markdown(data, 1L)
  expect_identical(
    grep("^- \\*\\*", markdown, value = TRUE),
    c(
      "- **Toy keyword** (warning): The assistant used the keyword. Evidence: content, event 2.",
      "- **Toy keyword** (warning): The assistant used the keyword. Evidence: content, event 3.",
      "- **Toy other** (warning): Second scanner. Evidence: content, event 3."
    )
  )
  queue <- cohort_review_queue(data, ids)
  expect_identical(
    queue$reasons[[match("chat-hit", queue$trajectory_id)]],
    c("Toy keyword", "Toy other")
  )

  findings <- export_findings(data, ids)
  expect_identical(
    findings$scan,
    c("toy_keyword", "toy_keyword", "toy_other")
  )
  conversations <- export_conversations(data, ids)
  expect_identical(conversations$n_findings, c(3L, 0L, 0L, 0L))
  code <- export_r_code(
    NULL,
    scans_app_scan_config(unique(data$assessments$scan)),
    ids
  )
  expect_true(any(grepl('"toy_keyword", "toy_other"', code, fixed = TRUE)))
})

test_that("data a saved investigation cannot hold is refused at the scanner boundary", {
  for (settings in list(
    list(f = function(x) x),
    list(e = globalenv()),
    list(b = scanner_bundle_fixture())
  )) {
    expect_error(
      scanner(
        "toy_settings",
        function(trajectory, settings) scanner_findings(),
        "d",
        "e",
        settings = settings
      ),
      class = "scans_error_scanner"
    )
  }
  dated <- scanner(
    "toy_dated",
    function(trajectory, settings) scanner_findings(),
    "d",
    "e",
    settings = list(since = as.Date("2026-01-01"))
  )
  expect_identical(dated$settings$since, as.Date("2026-01-01"))

  valued <- scanner_toy_fixture("toy_valued", function(trajectory, settings) {
    if (trajectory$trajectory_id != "chat-hit") {
      return(scanner_findings())
    }
    scanner_findings(
      event_id = "hit-e2",
      label = "Valued",
      explanation = "Holds a function.",
      value = list(list(f = function() 1))
    )
  })
  unlisted <- scanner_toy_fixture(
    "toy_unlisted",
    function(trajectory, settings) {
      scanner_findings(
        event_ids = list(NULL),
        label = "Record",
        explanation = "No supporting events."
      )
    }
  )
  bundle <- scanner_bundle_fixture()
  saved <- investigation_snapshot(bundle, scanners = list(valued, unlisted))
  assessments <- saved$analysis$assessments
  expect_identical(
    assessments$status[assessments$scan == "toy_valued"],
    c(
      "execution_failure",
      "assessed_no_findings",
      "assessed_no_findings",
      "assessed_no_findings"
    )
  )
  findings <- saved$analysis$findings
  expect_identical(
    findings$event_ids[findings$scan == "toy_unlisted"],
    rep(list(character()), 4L)
  )
  path <- tempfile(fileext = ".json")
  expect_no_error(write_investigation(saved, path))
})

test_that("a scanner modified after construction is checked again at the call", {
  edits <- list(
    list(name = "Toy/Key.*"),
    list(name = "Toy_Keyword"),
    list(version = 2),
    list(severity = "info"),
    list(required_evidence = NULL),
    list(description = NA_character_),
    list(fn = "not a function"),
    list(settings = list(f = function(x) x))
  )
  for (edit in edits) {
    edited <- scanner_keyword_fixture()
    edited[names(edit)] <- edit
    expect_error(
      assess_trajectory_scans(scanner_bundle_fixture(), scanners = edited),
      class = "scans_error_scanner"
    )
  }
})

test_that("scanner_findings() recycles only length-one arguments", {
  expect_identical(
    nrow(scanner_findings(
      event_id = c("a", "b"),
      label = "Same",
      explanation = "Same."
    )),
    2L
  )
  expect_identical(nrow(scanner_findings()), 0L)
  expect_error(
    scanner_findings(
      event_id = c("a", "b"),
      label = c("x", "y", "z"),
      explanation = "Mismatched."
    )
  )
  expect_error(
    scanner_findings(
      event_id = c("a", "b"),
      label = character(),
      explanation = character()
    )
  )
  expect_error(
    scanner_findings(
      label = c("x", "y"),
      explanation = "z",
      event_ids = list("a", "b", "c")
    )
  )
})

test_that("a custom explanation cannot add markdown structure to a trajectory export", {
  hostile <- scanner_toy_fixture("toy_hostile", function(trajectory, settings) {
    if (trajectory$trajectory_id != "chat-hit") {
      return(scanner_findings())
    }
    scanner_findings(
      event_id = "hit-e1",
      label = "Hostile",
      explanation = "line1\n\n## Injected heading\n| a | b |\n*bold* [x](y) <b>"
    )
  })
  data <- scans_app_data(scanner_bundle_fixture(), scanners = hostile)
  markdown <- strsplit(
    paste(export_trajectory_markdown(data, 1L), collapse = "\n"),
    "\n"
  )[[1L]]

  expect_false("## Injected heading" %in% markdown)
  expect_false(any(grepl("^\\|", markdown)))
  expect_true(
    paste0(
      "- **Toy hostile** (warning): line1 ## Injected heading \\| a \\| b \\| ",
      "\\*bold\\* \\[x\\](y) \\<b\\> Evidence: content, event 1."
    ) %in%
      markdown
  )
})

test_that("a custom explanation cannot add structure to the report", {
  hostile <- scanner_toy_fixture("toy_hostile", function(trajectory, settings) {
    if (trajectory$trajectory_id != "chat-hit") {
      return(scanner_findings())
    }
    scanner_findings(
      event_id = "hit-e1",
      label = "Hostile",
      explanation = paste(
        "line1\n\n## Injected heading\n| a | b |\n*bold* [x](y)",
        "<b>x</b> <script>alert(1)</script> &amp; `code` https://example.com"
      )
    )
  })
  report <- investigation_report(investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = hostile
  ))
  md <- format(report)
  expect_false(any(grepl("^#+ Injected", md)))
  expect_false(any(grepl("<script|<b>", md)))
  row <- grep("finding-1", md, value = TRUE)
  expect_length(row, 1L)
  expect_identical(
    lengths(regmatches(row, gregexpr("(?<!\\\\)\\|", row, perl = TRUE))),
    6L
  )
  expect_match(
    row,
    "| line1 \\#\\# Injected heading \\| a \\| b \\| \\*bold\\* \\[x\\](y) &lt;b&gt;x&lt;/b&gt; &lt;script&gt;alert(1)&lt;/script&gt; &amp;amp; \\`code\\` https:\\//example.com |",
    fixed = TRUE
  )

  skip_if_not_installed("htmltools")
  html <- format(report, "html")
  expect_no_match(html, "<script", fixed = TRUE)
  expect_no_match(html, "<b>", fixed = TRUE)
  expect_match(
    html,
    "&lt;script&gt;alert(1)&lt;/script&gt; &amp;amp; `code`",
    fixed = TRUE
  )
  expect_identical(
    lengths(regmatches(html, gregexpr("<h[1-6]", html))),
    lengths(regmatches(md, gregexpr("^#+ ", md))) |> sum()
  )
})

test_that("the report says custom explanations can quote the transcript", {
  saved <- investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = scanner_keyword_fixture()
  )
  md <- format(investigation_report(saved))
  expect_true(
    "- User identifiers, transcript text, tool arguments, and tool results are omitted from this report, except what a custom scanner's explanation quotes." %in%
      md
  )
  expect_true(
    "- Explanations from custom scanners are printed as their scanner wrote them and can quote transcript text: `toy_keyword`." %in%
      md
  )

  # Without a custom scan selected, the omission is unqualified.
  builtin <- format(investigation_report(investigation_snapshot(
    scanner_bundle_fixture(),
    scans = "event_error",
    scanners = scanner_keyword_fixture()
  )))
  expect_true(
    "- User identifiers, transcript text, tool arguments, and tool results are omitted from this report." %in%
      builtin
  )
  expect_false(any(grepl("custom scanner", builtin, fixed = TRUE)))
})

test_that("a custom scan that assessed nothing is reported as not assessed", {
  unsure <- scanner_toy_fixture("toy_unsure", function(trajectory, settings) {
    scanner_unassessed("insufficient_evidence", "Nothing to read.")
  })
  report <- investigation_report(investigation_snapshot(
    scanner_bundle_fixture(),
    scans = "toy_unsure",
    scanners = unsure
  ))
  expect_identical(report$headline$n_assessed, 0L)
  expect_identical(report$headline$n_not_assessed, 4L)
  expect_identical(unique(report$trajectories$class), "unassessed")
  md <- format(report)
  expect_true(all(
    c(
      "- None of the 4 trajectories could be assessed by a selected scan, so findings are unknown, not absent.",
      "No findings in 0 assessed trajectories. 4 trajectories could not be assessed.",
      "### Not assessed",
      "| toy unsure | 0 | 0 | 4 | 0 | 0 |"
    ) %in%
      md
  ))

  # Beside a built-in that assessed every trajectory, the custom scan's
  # trajectories count as assessed, and its own row says it assessed none.
  both <- investigation_report(investigation_snapshot(
    scanner_bundle_fixture(),
    scans = c("trajectory_error", "toy_unsure"),
    scanners = unsure
  ))
  expect_identical(both$headline$n_assessed, 4L)
  expect_true("| toy unsure | 0 | 0 | 4 | 0 | 0 |" %in% format(both))
})

test_that("a saved custom scan is reported without its scanner", {
  saved <- investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = scanner_keyword_fixture()
  )
  path <- tempfile(fileext = ".json")
  write_investigation(saved, path)
  # A new session, without the scanner: the report reads the saved results
  # and verifies their identifiers.
  reread <- read_investigation(path)
  report <- investigation_report(reread, verify = TRUE)
  expect_identical(
    report$evidence$finding_id[report$evidence$scan == "toy_keyword"],
    saved$analysis$findings$finding_id[
      saved$analysis$findings$scan == "toy_keyword"
    ]
  )
  expect_identical(
    report$evidence$event_id[report$evidence$scan == "toy_keyword"],
    c("hit-e2", "hit-e3")
  )
  expect_identical(report$scope$custom_scans, "toy_keyword")
  expect_identical(
    investigation_report(reread, include_errors = TRUE)$evidence,
    report$evidence
  )
  # The saved origin is covered by the revision identifier.
  edited <- reread
  edited$manifest$scanner_versions$origin[
    edited$manifest$scanner_versions$scan == "toy_keyword"
  ] <- "builtin"
  expect_error(
    investigation_report(edited),
    class = "scans_error_investigation"
  )
  # The report never reruns a scanner, so it does not say one is unavailable;
  # the app says so where it would rerun them.
  md <- paste(format(report), collapse = "\n")
  expect_no_match(md, "unavailable|cannot be rerun|rerun")
  expect_identical(
    sub("Generated [0-9: -]+ UTC", "", format(report)),
    sub("Generated [0-9: -]+ UTC", "", format(investigation_report(saved)))
  )
})

test_that("a trajectory with a failed scan says so in the queue and the headline", {
  boom <- scanner_toy_fixture("toy_boom", function(trajectory, settings) {
    if (trajectory$trajectory_id %in% c("chat-hit", "chat-clean")) {
      stop("boom")
    }
    scanner_findings()
  })
  data <- scans_app_data(scanner_bundle_fixture(), scanners = boom)
  ids <- data$info$trajectory_id
  trajectories <- cohort_trajectories(data, ids)

  expect_identical(trajectories$class, rep("clean", 4L))
  expect_identical(trajectories$execution_failure, c(TRUE, TRUE, FALSE, FALSE))
  queue <- cohort_review_queue(data, ids)
  expect_identical(
    vapply(queue$reasons, function(x) "Execution failure" %in% x, logical(1)),
    queue$trajectory_id %in% c("chat-hit", "chat-clean")
  )
  headline <- cohort_headline(data, ids)
  expect_identical(headline$n_execution_failure, 2L)
  expect_match(
    as.character(scans_app_tiles_ui(headline)),
    "2 with an execution failure",
    fixed = TRUE
  )
  report <- investigation_report(investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = boom
  ))
  expect_identical(report$headline$n_execution_failure, 2L)
  failed <- "2 trajectories had a scan that failed to run; that scan's result for them is unknown."
  expect_true(paste("-", failed) %in% format(report))
  if (rlang::is_installed("htmltools")) {
    expect_match(
      format(report, "html"),
      paste0("<li>", failed, "</li>"),
      fixed = TRUE
    )
  }

  plain <- scans_app_data(scanner_bundle_fixture())
  expect_identical(cohort_headline(plain, ids)$n_execution_failure, 0L)
  expect_no_match(
    paste(
      format(investigation_report(scanner_bundle_fixture())),
      collapse = "\n"
    ),
    "failed to run",
    fixed = TRUE
  )
  expect_no_match(
    as.character(scans_app_tiles_ui(cohort_headline(plain, ids))),
    "execution failure"
  )
})

test_that("a saved investigation records whether each scan is built-in or custom", {
  saved <- investigation_snapshot(
    scanner_bundle_fixture(),
    scanners = scanner_keyword_fixture()
  )
  versions <- saved$manifest$scanner_versions
  expect_named(versions, c("scan", "scan_version", "origin"))
  expect_identical(
    versions$origin[versions$scan %in% c("event_error", "toy_keyword")],
    c("builtin", "custom")
  )
  path <- tempfile(fileext = ".json")
  write_investigation(saved, path)
  expect_identical(read_investigation(path), saved)

  # A file written before origins were saved still reads; its origins are
  # inferred from the built-in names of that release.
  legacy <- saved
  legacy$manifest$scanner_versions$origin <- NULL
  legacy$manifest$revision_id <- investigation_revision_id(legacy)
  legacy_path <- tempfile(fileext = ".json")
  write_investigation(legacy, legacy_path)
  legacy <- read_investigation(legacy_path)
  expect_identical(
    investigation_scan_origins(legacy),
    investigation_scan_origins(saved)
  )

  # A later release adds a built-in with the saved custom name.
  registry <- scan_registry
  local_mocked_bindings(scan_registry = function(scanners = NULL) {
    out <- registry(scanners)
    tibble::add_row(
      out,
      scan = "toy_keyword",
      severity = "error",
      description = "A later built-in."
    )
  })
  for (opened in list(read_investigation(path), legacy)) {
    expect_identical(
      investigation_scan_origins(opened)[["toy_keyword"]],
      "custom"
    )
    expect_false("toy_keyword" %in% investigation_rerunnable_scans(opened))
    expect_true("event_error" %in% investigation_rerunnable_scans(opened))
    expect_match(
      as.character(scans_app_investigation_status_ui(opened, FALSE, FALSE)),
      "or session: toy_keyword.",
      fixed = TRUE
    )
  }
  reviewed <- review_case(
    read_investigation(path),
    "chat-hit",
    finding_id = "scan-000001/toy_keyword/finding-000001"
  )
  expect_identical(
    investigation_scan_origins(reviewed$evidence)[["toy_keyword"]],
    "custom"
  )
})

test_that("a custom scan's pattern changes are classified like a built-in's", {
  comparison <- scanner_comparison_fixture(
    scanner_applicable_fixture(),
    scanner_applicable_fixture()
  )
  rows <- comparison$patterns[comparison$patterns$scan == "toy_keyword", ]
  expect_identical(
    rows$change[match(
      c("appear", "resolve", "persist", "stop", "start"),
      rows$sample_id
    )],
    c(
      "appeared",
      "resolved",
      "persisted",
      "no_longer_applicable",
      "newly_applicable"
    )
  )
  hit <- rows[rows$sample_id == "appear", ]
  expect_identical(hit$candidate_event_ids[[1L]], "appear-e1")
  printed <- paste(
    cli::ansi_strip(capture.output(print(comparison), type = "message")),
    collapse = " "
  )
  expect_match(
    gsub("\\s+", " ", printed),
    "Pattern toy_keyword: appeared in 1, resolved in 1, persisted in 1, no longer applicable in 1, newly applicable in 1",
    fixed = TRUE
  )
  html <- as.character(scans_app_comparison_cases_ui(
    comparison,
    scans_app_comparison_sources(comparison)$labels,
    "patterns"
  ))
  for (text in c(
    "toy_keyword: appeared",
    "toy_keyword: no longer applicable",
    "toy_keyword: newly applicable"
  )) {
    expect_match(html, text, fixed = TRUE)
  }

  # A changed version or setting, or a scan run on one side only, is not
  # comparable for every case, with the reason.
  changed <- list(
    list(
      scanner_applicable_fixture(version = "2"),
      "Scanner version or settings differ."
    ),
    list(
      scanner_applicable_fixture(keyword = "fine"),
      "Scanner version or settings differ."
    ),
    list(NULL, "The scan was not selected for the candidate.")
  )
  for (case in changed) {
    other <- scanner_comparison_fixture(
      scanner_applicable_fixture(),
      case[[1L]]
    )
    rows <- other$patterns[other$patterns$scan == "toy_keyword", ]
    expect_identical(unique(rows$change), "not_comparable")
    expect_identical(unique(rows$reason), case[[2L]])
  }
})

test_that("a comparison opened with custom scanners is refused", {
  comparison <- scanner_comparison_fixture(
    scanner_applicable_fixture(),
    scanner_applicable_fixture()
  )
  expect_error(
    scans_app(comparison, scanners = scanner_applicable_fixture()),
    class = "scans_error_app_scanners"
  )
  expect_no_error(scans_app(comparison))
})

test_that("the bounded browser list keeps custom and failure reasons", {
  boom <- scanner_toy_fixture("toy_boom", function(trajectory, settings) {
    if (trajectory$trajectory_id == "chat-hit") {
      stop("boom")
    }
    scanner_findings()
  })
  data <- scans_app_data(
    scanner_bundle_fixture(),
    scanners = list(scanner_keyword_fixture(), boom)
  )
  queue <- cohort_review_queue(data, data$info$trajectory_id)
  hit <- queue$index[queue$trajectory_id == "chat-hit"]
  expect_identical(
    queue$reasons[[match(hit, queue$index)]],
    c("Toy keyword", "Execution failure")
  )
  others <- setdiff(queue$index, hit)
  reasons <- function(html) {
    regmatches(html, gregexpr("Toy keyword|Execution failure", html))[[1L]]
  }

  # The open entry past the head is shown after the note with its reasons,
  # and an entry in the head shows them in place.
  past <- as.character(scans_app_entries_ui(
    data,
    c(others, hit),
    queue,
    chosen = hit,
    limit = 1L
  ))
  expect_match(past, "Listing the first 1 of 4", fixed = TRUE)
  expect_match(past, "The open trajectory is number 4 of 4", fixed = TRUE)
  expect_identical(reasons(past), c("Toy keyword", "Execution failure"))
  head <- as.character(scans_app_entries_ui(
    data,
    c(hit, others),
    queue,
    chosen = hit,
    limit = 1L
  ))
  expect_no_match(head, "The open trajectory is number", fixed = TRUE)
  expect_identical(reasons(head), c("Toy keyword", "Execution failure"))
})

test_that("a link filtering by a custom pattern needs that scanner", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  bundle <- scanner_bundle_fixture()
  hash <- scans_app_scope_hash(
    "Trajectories",
    scope = list(pattern = "toy_keyword")
  )
  sent <- new.env(parent = emptyenv())
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      sent$notice <- c(sent$notice, ui)
    },
    .package = "shiny"
  )
  open <- function(scanners) {
    sent$notice <- NULL
    shiny::testServer(
      scans_app(bundle, scanners = scanners)$serverFuncSource(),
      {
        session$setInputs(scans_app_scans = scan_registry(scanners)$scan)
        session$setInputs(scans_app_hash = hash)
        sent$pattern <- pattern_filter()
        sent$visible <- data()$info$trajectory_id[visible()]
      }
    )
  }
  open(scanner_keyword_fixture())
  expect_identical(sent$pattern, "toy_keyword")
  expect_identical(sent$visible, "chat-hit")
  expect_null(sent$notice)

  open(NULL)
  expect_null(sent$pattern)
  expect_length(sent$visible, 4L)
  expect_match(
    sent$notice,
    "This link filters by Finding: toy keyword, which this app cannot apply",
    fixed = TRUE
  )
})

test_that("tools report a custom scan that assessed nothing as unknown", {
  skip_if_not_installed("ellmer", "0.5.0")
  unsure <- scanner_toy_fixture("toy_unsure", function(trajectory, settings) {
    scanner_unassessed("insufficient_evidence", "Nothing to read.")
  })
  saved <- investigation_snapshot(
    scanner_bundle_fixture(),
    scans = "toy_unsure",
    scanners = unsure
  )
  tools <- scans_tools(saved)
  summary <- scans_tool_result(tools$scans_summarize)
  expect_equal(summary$data[[1L]]$n_assessed, 0L)
  expect_equal(summary$data[[1L]]$n_findings, 0L)
  counts <- function(summary, scan) {
    rows <- Filter(function(row) row$scan == scan, summary$assessment_coverage)
    stats::setNames(
      vapply(rows, function(row) as.integer(row$trajectories), integer(1)),
      vapply(rows, `[[`, "", "status")
    )
  }
  coverage <- counts(summary, "toy_unsure")
  expect_identical(coverage[["insufficient_evidence"]], 4L)
  expect_identical(sum(coverage), 4L)
  runs <- scans_tool_result(tools$scans_list_trajectories)$data
  expect_false(any(vapply(runs, `[[`, logical(1), "assessed")))

  # Beside the built-ins the trajectories they assessed count, and the
  # custom scan's own coverage still says it assessed none.
  live <- scans_tools(scanner_bundle_fixture(), scanners = unsure)
  summary <- scans_tool_result(live$scans_summarize)
  expect_equal(summary$data[[1L]]$n_assessed, 4L)
  coverage <- counts(summary, "toy_unsure")
  expect_identical(coverage[["insufficient_evidence"]], 4L)
  expect_identical(sum(coverage), 4L)
})
