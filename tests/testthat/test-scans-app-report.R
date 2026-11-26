# The minute a report was generated is the only thing that may differ.
report_without_time <- function(text) {
  sub("Generated [0-9: -]+ UTC", "Generated <time>", text)
}

test_that("the app's report for an opened investigation is the script's report", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  skip_if_not_installed("htmltools")
  saved <- report_fixture(list(
    query = "small",
    findings_only = TRUE,
    pattern = "event_error"
  ))
  script <- investigation_report(
    saved,
    app_url = "http://mocksession:1234/mockpath"
  )
  expect_gt(nrow(script$evidence), 0L)
  expect_lt(script$headline$n, script$headline$total)
  named <- investigation_report(
    saved,
    app_url = "http://mocksession:1234/mockpath",
    include_users = TRUE
  )

  app <- scans_app(saved)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(
      scans_app_query = saved$view$query,
      scans_app_findings_only = saved$view$findings_only
    )
    expect_identical(
      data()$info$trajectory_id[sort(visible())],
      script$trajectories$trajectory_id
    )
    session$setInputs(scans_app_export_report = 1L)
    expect_identical(
      report_without_time(export_text()),
      report_without_time(format(script))
    )
    expect_identical(
      report_without_time(readLines(
        output$scans_app_export_download,
        encoding = "UTF-8"
      )),
      report_without_time(format(script))
    )
    expect_identical(
      report_without_time(paste(
        readLines(output$scans_app_export_report_html, encoding = "UTF-8"),
        collapse = "\n"
      )),
      report_without_time(format(script, "html"))
    )
    # The browser's order does not reach the report.
    session$setInputs(scans_app_sort = "oldest")
    expect_identical(
      report_without_time(export_text()),
      report_without_time(format(script))
    )
    session$setInputs(scans_app_report_users = TRUE)
    expect_identical(
      report_without_time(export_text()),
      report_without_time(format(named))
    )
    session$setInputs(scans_app_report_users = FALSE)

    # A new analysis of the saved evidence is no longer that saved revision.
    session$setInputs(scans_app_rescan_investigation = 1L)
    rescanned <- export_text()
    expect_match(
      rescanned,
      "this analysis has not been saved as a revision",
      fixed = TRUE,
      all = FALSE
    )
    expect_no_match(rescanned, saved$manifest$revision_id, fixed = TRUE)
  })
})

test_that("a live view's report names its filters and its links reopen them", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  updates <- local_input_updates()
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    input_updates_ack(session, updates)
    current <- data()
    choices <- scans_app_filter_choices(current)
    session$setInputs(
      scans_app_source = "otel",
      scans_app_status = unname(choices$status[["Unknown"]]),
      scans_app_query = "SMALL",
      scans_app_findings_only = TRUE,
      scans_app_facet = list(facet = "model", value = "small")
    )
    ids <- current$info$trajectory_id[visible()]
    expect_identical(ids, "a")
    session$setInputs(scans_app_export_report = 1L)
    text <- export_text()
    expect_true("- Scope: 1 of 5 conversations." %in% text)
    expect_true(
      "- Filters: Source: otel; Status: unknown; Text contains \"SMALL\"; With findings; Model: small." %in%
        text
    )
    expect_no_match(text, "saved investigation", fixed = TRUE)
    expect_match(
      text,
      "Links set the filters named above and clear every other filter. They apply them to what the app holds when they are opened.",
      fixed = TRUE,
      all = FALSE
    )
    expect_identical(link()(), report()$scope$link)

    # The CSV exports state the same scope.
    conversations <- utils::read.csv(output$scans_app_export_conversations)
    expect_match(
      conversations$scope[[1L]],
      "Source: otel · Status: unknown · Text contains \"SMALL\"",
      fixed = TRUE
    )

    # The report's link, followed after the filters change, selects the
    # same rows again.
    link <- report()$scope$link
    session$setInputs(
      scans_app_source = choices$source_all,
      scans_app_query = "",
      scans_app_facet = list(facet = "model", value = "large")
    )
    expect_identical(report_follow_link(session, updates, link), "a")
  })
})

test_that("the report dialog's user option governs a user filter", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(
      scans_app_hash = scans_app_scope_hash(
        "Cohort",
        scope = list(user = "ada")
      )
    )
    expect_identical(facets(), list(user = "ada"))
    session$setInputs(scans_app_export_report = 1L)
    text <- paste(export_text(), collapse = "\n")
    expect_no_match(text, "ada", fixed = TRUE)
    expect_match(text, "One user (identifier omitted)", fixed = TRUE)
    expect_match(text, "Links leave out the user filter", fixed = TRUE)
    expect_no_match(text, "?scope=user", fixed = TRUE)

    session$setInputs(scans_app_report_users = TRUE)
    text <- paste(export_text(), collapse = "\n")
    expect_match(text, "User: ada", fixed = TRUE)
    expect_match(text, "?scope=user=ada", fixed = TRUE)
    expect_no_match(text, "Links leave out the user filter", fixed = TRUE)

    session$setInputs(scans_app_export_trajectory = 1L)
    expect_no_match(paste(export_text(), collapse = "\n"), "ada", fixed = TRUE)
  })
})

test_that("the app's report prints a revision only for the view it saved", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  saved <- report_fixture(list(query = "small"))
  saved_revision <- saved$manifest$revision_id
  app <- scans_app(saved)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_query = "small")
    session$setInputs(scans_app_export_report = 1L)
    expect_match(export_text(), saved_revision, fixed = TRUE, all = FALSE)

    changed <- list(
      list(scans_app_query = "large"),
      list(scans_app_facet = list(facet = "model", value = "small")),
      list(scans_app_findings_only = TRUE)
    )
    for (inputs in changed) {
      do.call(session$setInputs, inputs)
      text <- export_text()
      expect_no_match(text, saved_revision, fixed = TRUE)
      expect_match(text, saved$manifest$snapshot_id, fixed = TRUE, all = FALSE)
      expect_match(
        text,
        "the view was changed in the app since this revision was saved, so no saved revision describes it.",
        fixed = TRUE,
        all = FALSE
      )
      expect_null(report()$scope$snapshot$revision_id)
      # Back to the saved view.
      session$setInputs(
        scans_app_query = "small",
        scans_app_findings_only = FALSE
      )
      if (length(facets())) {
        session$setInputs(
          scans_app_facet = list(facet = "model", value = "small")
        )
      }
      expect_match(export_text(), saved_revision, fixed = TRUE, all = FALSE)
    }
  })
})

test_that("an annotation filter names the membership the report used", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  saved <- report_fixture(list(
    annotated_only = TRUE,
    annotation_ids = c("a", "c")
  ))
  script <- investigation_report(
    saved,
    app_url = "http://mocksession:1234/mockpath"
  )
  expect_identical(script$trajectories$trajectory_id, c("a", "c"))
  expect_identical(script$scope$filters[["annotated"]], "Annotated when saved")
  expect_identical(
    script$scope$snapshot$revision_id,
    saved$manifest$revision_id
  )

  # Without a store the app uses the saved membership: the script's report.
  shiny::testServer(scans_app(saved)$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_annotated_only = TRUE)
    session$setInputs(scans_app_export_report = 1L)
    expect_identical(
      report_without_time(export_text()),
      report_without_time(format(script))
    )
  })

  # With a store the app filters on the store's annotations as they are now.
  store <- scans_annotations(
    withr::local_tempfile(fileext = ".jsonl"),
    labels = "tool failure"
  )
  store$append("Cohort", "b", "tool failure", NULL, "reviewer")
  app <- scans_app(saved, annotations = store)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_annotated_only = TRUE)
    session$setInputs(scans_app_export_report = 1L)
    report <- report()
    expect_identical(report$trajectories$trajectory_id, "b")
    expect_identical(
      report$scope$filters[["annotated"]],
      "Annotated in this app's annotation store at export"
    )
    expect_null(report$scope$snapshot$revision_id)
    text <- export_text()
    expect_no_match(text, saved$manifest$revision_id, fixed = TRUE)
    expect_match(
      text,
      "the annotation filter uses this app's annotation store, not the membership saved with this revision, so no saved revision describes it.",
      fixed = TRUE,
      all = FALSE
    )
    expect_no_match(text, "reopen exactly this cohort", fixed = TRUE)
  })
})

test_that("the report's review progress is the overview's", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  store <- scans_annotations(
    withr::local_tempfile(fileext = ".jsonl"),
    labels = "tool failure"
  )
  app <- scans_app(list(Cohort = cohort_fixture()), annotations = store)
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(
      scans_app_disposition = list(
        label = "tool failure",
        trajectory_id = "c",
        nonce = 1
      )
    )
    session$setInputs(scans_app_export_report = 1L)
    expect_true(
      "- Review: 1 of 3 flagged conversations reviewed." %in% export_text()
    )
    session$setInputs(scans_app_review_state = "needs")
    expect_identical(report()$review_progress, review_progress())
    expect_true(
      "- Review: 1 of 3 flagged conversations reviewed, counted without the review filter." %in%
        export_text()
    )
  })
})
