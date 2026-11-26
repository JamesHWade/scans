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
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
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
      "Links carry the filters named above and apply them to what the app holds when they are opened.",
      fixed = TRUE,
      all = FALSE
    )

    # The link in the report selects the same rows when its scope is applied.
    target <- scans_app_parse_hash(
      sub("^[^#]*#", "", report()$scope$link),
      "Cohort"
    )
    restored <- scans_app_scope_restore(target$scope, current)
    expect_null(restored$notice)
    expect_identical(
      scans_app_facet_filter(
        current,
        scans_app_view_indices(current, restored),
        target$scope[intersect(names(target$scope), scans_app_facet_names)]
      ),
      sort(visible())
    )
    expect_identical(link()(), report()$scope$link)

    # The CSV exports state the same scope.
    conversations <- utils::read.csv(output$scans_app_export_conversations)
    expect_match(
      conversations$scope[[1L]],
      "Source: otel · Status: unknown · Text contains \"SMALL\"",
      fixed = TRUE
    )
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
