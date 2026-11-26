comparison_app_fixture <- function(...) {
  compare_trajectories(comparison_pairing(...))
}

test_that("a comparison opens as two applications with the comparison first", {
  skip_if_not_installed("vitals")
  comparison <- comparison_app_fixture()
  sources <- scans_app_sources(comparison)

  expect_identical(
    sources$labels,
    c(
      "Baseline: Support assistant baseline",
      "Candidate: Support assistant candidate"
    )
  )
  expect_identical(
    sources$sources[[2L]]$investigation,
    comparison$pairing$candidate
  )
  expect_s3_class(scans_app(comparison), "shiny.appobj")

  html <- as.character(scans_app_ui(sources))
  position <- function(text) regexpr(text, html, fixed = TRUE)[[1L]]
  expect_gt(position("Not compared"), 0L)
  expect_lt(position("Not compared"), position("Paired changes in 7 matched"))
  expect_gt(position("2 outcomes missing on a side"), 0L)
  expect_lt(
    position("2 outcomes missing on a side"),
    position("improved")
  )
  expect_lt(position("Paired changes"), position("scans_app_comparison_cases"))
  expect_lt(
    position("scans_app_comparison_cases"),
    position("scans_app_performance")
  )
  expect_match(html, "vitals%2Fretired%2Fepoch-000001", fixed = TRUE)

  expect_no_match(
    as.character(scans_app_ui(scans_app_sources(comparison$pairing$baseline))),
    "scans-app-comparison",
    fixed = TRUE
  )
  expect_error(
    scans_app(structure(list(), class = "scans_comparison")),
    class = "scans_error_app_source"
  )
})

test_that("case filters select by the counts the comparison recorded", {
  skip_if_not_installed("vitals")
  comparison <- comparison_app_fixture()
  samples <- function(filter) {
    ids <- scans_app_comparison_pairs(comparison, filter)
    comparison$cases$sample_id[match(ids, comparison$cases$pair_id)]
  }

  expect_identical(samples("all"), comparison$cases$sample_id)
  expect_identical(samples("regressed"), "regress")
  expect_identical(samples("improved"), c("improve", "repeated"))
  expect_identical(samples("patterns"), "regress")
  expect_identical(samples("resources"), c("improve", "regress"))
  expect_identical(
    samples("not_comparable"),
    c("unassessed", "untracked", "rescored")
  )
})

test_that("a case shows the comparison's values and links both trajectories", {
  skip_if_not_installed("vitals")
  comparison <- comparison_app_fixture()
  labels <- scans_app_sources(comparison)$labels
  html <- as.character(scans_app_comparison_cases_ui(
    comparison,
    labels,
    "regressed"
  ))
  resource <- comparison$resources[
    comparison$resources$sample_id == "regress" &
      comparison$resources$measure == "input_tokens",
  ]
  pattern <- comparison$patterns[
    comparison$patterns$sample_id == "regress" &
      comparison$patterns$scan == "event_error",
  ]

  expect_match(html, "exact_scorer: C → I, regressed", fixed = TRUE)
  expect_match(
    html,
    sprintf(
      "input_tokens: %s → %s tokens; change %s",
      resource$baseline_value,
      resource$candidate_value,
      resource$delta
    ),
    fixed = TRUE
  )
  expect_match(html, "event_error: appeared", fixed = TRUE)
  for (side in 1:2) {
    expect_match(
      html,
      paste0(
        "#",
        scans_app_hash(labels[[side]], "vitals/regress/epoch-000001")
      ),
      fixed = TRUE
    )
  }
  expect_match(
    html,
    paste0(
      "?event=",
      utils::URLencode(pattern$candidate_event_ids[[1L]], reserved = TRUE)
    ),
    fixed = TRUE
  )

  gaps <- as.character(scans_app_comparison_cases_ui(
    comparison,
    labels,
    "not_comparable"
  ))
  expect_match(
    gaps,
    "not comparable (missing). No evaluation by this scorer on the candidate.",
    fixed = TRUE
  )
  expect_match(
    gaps,
    "The candidate was not assessed (insufficient_evidence).",
    fixed = TRUE
  )
  expect_match(
    gaps,
    "input_tokens: 10 → unknown tokens; no delta.",
    fixed = TRUE
  )
  expect_match(gaps, "(coverage: complete, unavailable)", fixed = TRUE)
})

test_that("the server filters cases and a case link opens the other side", {
  skip_if_not_installed("vitals")
  comparison <- comparison_app_fixture()
  app <- scans_app(comparison)
  candidate <- "Candidate: Support assistant candidate"

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    all_cases <- output$scans_app_comparison_cases$html
    expect_match(all_cases, "sample same", fixed = TRUE)
    session$setInputs(scans_app_comparison_filter = "regressed")
    regressions <- output$scans_app_comparison_cases$html
    expect_match(regressions, "sample regress", fixed = TRUE)
    expect_no_match(regressions, "sample same", fixed = TRUE)

    expect_identical(application(), "Baseline: Support assistant baseline")
    session$setInputs(
      scans_app_hash = scans_app_hash(candidate, "vitals/regress/epoch-000001")
    )
    # testServer has no browser to apply the application switch.
    session$setInputs(scans_app_application = candidate)
    session$flushReact()
    expect_identical(application(), candidate)
    expect_identical(
      data()$info$trajectory_id[[selected()]],
      "vitals/regress/epoch-000001"
    )
    expect_identical(
      data()$findings,
      comparison$pairing$candidate$analysis$findings
    )
  })
})

test_that("an unpaired comparison is labelled and offers no case filter", {
  skip_if_not_installed("vitals")
  comparison <- comparison_app_fixture(dataset_version = NULL)
  html <- as.character(scans_app_comparison_ui(scans_app_sources(comparison)))

  expect_match(html, "17 trajectories missing a key value", fixed = TRUE)
  expect_match(html, "Unpaired description", fixed = TRUE)
  expect_match(html, "no paired estimate is produced", fixed = TRUE)
  expect_no_match(html, "scans_app_comparison_filter", fixed = TRUE)
  expect_no_match(html, "Paired changes", fixed = TRUE)
})

test_that("a comparison side keeps its investigation's application identity", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("jsonlite")
  comparison <- comparison_app_fixture()
  baseline <- comparison$pairing$baseline$manifest$application
  store <- scans_annotations(path = withr::local_tempfile(fileext = ".jsonl"))
  store$append(baseline, "vitals/same/epoch-000001", "follow up", "Earlier")
  app <- scans_app(comparison, annotations = store, investigations = TRUE)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    expect_identical(application(), paste("Baseline:", baseline))
    expect_identical(annotation_records()$note, "Earlier")

    selected_id <- data()$info$trajectory_id[[selected()]]
    session$setInputs(
      scans_app_annotation_label = "looks right",
      scans_app_annotation_note = "From the comparison",
      scans_app_annotation_save = 1L
    )
    session$flushReact()
    expect_identical(
      store$read(baseline, selected_id)$note,
      "From the comparison"
    )
    expect_identical(nrow(store$read(application())), 0L)

    session$setInputs(scans_app_save_investigation = 1L)
    expect_identical(pending_investigation()$manifest$application, baseline)
  })
})

test_that("the view shows a pattern that stopped applying and both units", {
  comparison <- compare_trajectories(pair_trajectories(
    comparison_changed_investigation("baseline"),
    comparison_changed_investigation("candidate"),
    by = "sample_id",
    dataset_version = "v1"
  ))
  # Stand-ins for rows a different capture or measurement contract gives;
  # the view reads them as recorded.
  pattern <- comparison$patterns$sample_id == "case-1" &
    comparison$patterns$scan == "event_error"
  comparison$patterns$change[pattern] <- "no_longer_applicable"
  comparison$patterns$reason[pattern] <-
    "The scan does not apply to the candidate capture."
  comparison$cases$patterns_appeared[[1L]] <- 0L
  comparison$cases$patterns_no_longer_applicable[[1L]] <- 1L
  resource <- comparison$resources$sample_id == "case-1" &
    comparison$resources$measure == "output_tokens"
  comparison$resources$unit[resource] <- NA
  comparison$resources$candidate_unit[resource] <- "k tokens"

  expect_true(
    comparison$cases$pair_id[[1L]] %in%
      scans_app_comparison_pairs(comparison, "patterns")
  )
  html <- as.character(scans_app_comparison_case(
    comparison$cases$pair_id[[1L]],
    comparison,
    scans_app_sources(comparison)$labels
  ))
  expect_match(html, "event_error: no longer applicable", fixed = TRUE)
  expect_match(
    html,
    ". The scan does not apply to the candidate capture.",
    fixed = TRUE
  )
  expect_no_match(html, "event_error: resolved", fixed = TRUE)
  expect_match(html, "output_tokens: 4 tokens → 9 k tokens", fixed = TRUE)
})

test_that("a comparison keeps each side on its saved analysis", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("jsonlite")
  comparison <- comparison_app_fixture()
  candidate <- "Candidate: Support assistant candidate"
  path <- withr::local_tempfile(fileext = ".json")
  write_investigation(comparison$pairing$baseline, path)
  saved_investigation <- read_investigation(path)
  app <- scans_app(comparison, investigations = TRUE)

  html <- as.character(
    app$httpHandler(list(
      PATH_INFO = "/",
      REQUEST_METHOD = "GET",
      QUERY_STRING = ""
    ))$content
  )
  expect_match(html, "scans_app_save_investigation", fixed = TRUE)
  expect_no_match(html, "scans_app_open_investigation", fixed = TRUE)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    saved <- comparison$pairing$baseline$analysis$findings
    expect_no_match(
      output$scans_app_investigation_status$html,
      "Apply current scanners",
      fixed = TRUE
    )
    session$setInputs(
      scans_app_scans = character(),
      scans_app_rescan_investigation = 1L
    )
    expect_false(rescan_investigation())
    expect_identical(data()$findings, saved)

    session$setInputs(
      scans_app_open_investigation = data.frame(datapath = path)
    )
    expect_null(opened_investigation())

    # Any other path that opens an investigation, such as a review import,
    # is left by a comparison link.
    opened_investigation(saved_investigation)
    session$flushReact()
    session$setInputs(
      scans_app_hash = scans_app_hash(candidate, "vitals/regress/epoch-000001")
    )
    session$setInputs(scans_app_application = candidate)
    session$flushReact()
    expect_null(opened_investigation())
    expect_identical(application(), candidate)
    expect_identical(
      data()$info$trajectory_id[[selected()]],
      "vitals/regress/epoch-000001"
    )
  })
})

test_that("Ask evidence from the other side switches sides in a comparison", {
  skip_if_not_installed("vitals")
  skip_if_not_installed("shinychat", "0.5.0")
  skip_if_not_installed("ellmer", "0.5.0")
  comparison <- comparison_app_fixture()
  baseline <- "Baseline: Support assistant baseline"
  candidate <- "Candidate: Support assistant candidate"
  app <- scans_app(comparison, chat_factory = scans_test_chat)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_application = candidate)
    session$flushReact()
    session$setInputs(
      scans_app_view = "trajectory",
      scans_app_ask_scope = "trajectory",
      scans_app_inspect = "vitals/regress/epoch-000001"
    )
    session$setInputs(scans_app_ask_user_input = list("What failed?"))
    scans_test_drain(session, function() {
      !is.null(chat_state$module$last_turn())
    })
    key <- ls(chat_state$references)[[1L]]
    session$setInputs(scans_app_application = baseline)
    session$flushReact()
    session$setInputs(scans_app_chat_evidence = key)
    # testServer has no browser to apply the application switch.
    session$setInputs(scans_app_application = candidate)
    session$flushReact()
    expect_null(opened_investigation())
    expect_identical(application(), candidate)
    expect_identical(selected_trajectory_id(), "vitals/regress/epoch-000001")

    session$setInputs(
      scans_app_hash = scans_app_hash(baseline, "vitals/same/epoch-000001")
    )
    session$setInputs(scans_app_application = baseline)
    session$flushReact()
    expect_identical(application(), baseline)
    expect_identical(selected_trajectory_id(), "vitals/same/epoch-000001")
  })
})
