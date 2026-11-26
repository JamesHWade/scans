test_that("the report's numbers and evidence are the app's for the same view", {
  views <- list(
    list(),
    list(findings_only = TRUE),
    list(pattern = "event_error", query = "Small"),
    list(status = NA_character_, source_type = "otel"),
    list(annotated_only = TRUE, annotation_ids = c("a", "e"))
  )
  for (view in views) {
    saved <- report_fixture(view)
    app <- report_app_view(saved)
    report <- investigation_report(saved)
    shown <- app$performance

    expect_gt(length(app$ids), 0L)
    expect_identical(report$trajectories$trajectory_id, sort(app$ids))
    expect_identical(
      report$headline[names(shown$cohort$headline)],
      shown$cohort$headline
    )
    expect_identical(
      report$headline$n_not_assessed,
      shown$n - shown$cohort$headline$n_assessed
    )
    expect_identical(report$headline$n_losses, shown$n_losses)
    expect_identical(report$patterns$scan, shown$patterns$scan)
    expect_identical(report$patterns$trajectories, shown$patterns$trajectories)
    expect_identical(report$patterns$findings, shown$patterns$findings)
    expect_identical(report$tools, shown$cohort$tools)
    expect_identical(report$models, shown$cohort$models)
    expect_identical(report$measures, shown$cohort$measures)
    expect_identical(report$coverage, shown$assessment_coverage)
    expect_identical(report$losses, shown$cohort$losses)

    findings <- app$data$findings[
      app$data$findings$trajectory_id %in% app$ids,
    ]
    expect_setequal(report$evidence$finding_id, findings$finding_id)
    rows <- match(report$evidence$finding_id, findings$finding_id)
    expect_identical(
      report$evidence$trajectory_id,
      findings$trajectory_id[rows]
    )
    expect_identical(report$evidence$turn_id, findings$turn_id[rows])
    expect_identical(report$evidence$event_id, findings$event_id[rows])
    expect_identical(
      report$evidence$event_ids,
      lapply(findings$event_ids[rows], as.character)
    )
    # A pattern's reach is the trajectories its evidence rows name.
    expect_identical(
      vapply(
        report$patterns$scan,
        function(scan) {
          length(unique(
            report$evidence$trajectory_id[report$evidence$scan == scan]
          ))
        },
        integer(1),
        USE.NAMES = FALSE
      ),
      report$patterns$trajectories
    )
  }
})

test_that("the rendered report prints those numbers with their denominators", {
  saved <- report_fixture()
  report <- investigation_report(saved)
  headline <- report$headline
  expect_identical(c(headline$n, headline$n_assessed), c(5L, 4L))
  text <- format(report)

  expect_true("- 5 of 5 conversations are in scope." %in% text)
  expect_true(
    sprintf(
      "- Findings in %d of 4 assessed conversations (%d with error-severity findings).",
      headline$n_findings,
      headline$n_error
    ) %in%
      text
  )
  expect_true(
    "- 1 of 5 could not be assessed by any selected scan. Not assessed is not a clean result." %in%
      text
  )
  expect_true(
    sprintf(
      "- %d of %d tool calls failed, in %d conversations.",
      headline$tool_failed,
      headline$tool_calls,
      headline$tool_trajectories_failed
    ) %in%
      text
  )
  for (i in seq_len(nrow(report$patterns))) {
    row <- report$patterns[i, ]
    expect_true(any(
      startsWith(text, sprintf("| [%s](<#%s>) |", row$label, row$anchor)) &
        grepl(
          sprintf(
            "| %d of 5 | %d of 5 | %d |",
            row$trajectories,
            row$assessed,
            row$findings
          ),
          text,
          fixed = TRUE
        )
    ))
  }

  # Every finding's identities are in the evidence section, and the trajectory
  # no scan could assess is listed as such rather than left out as clean.
  for (i in seq_len(nrow(report$evidence))) {
    row <- report$evidence[i, ]
    line <- text[grepl(sprintf("<a id=\"%s\"></a>", row$anchor), text)]
    expect_length(line, 1L)
    expect_match(line, sprintf("`%s`", row$trajectory_id), fixed = TRUE)
    for (event in row$event_ids[[1L]]) {
      expect_match(line, sprintf("`%s`", event), fixed = TRUE)
    }
  }
  expect_identical(
    report$trajectories$trajectory_id[
      report$trajectories$class == "unassessed"
    ],
    "e"
  )
  expect_identical(text[match("### Not assessed", text) + 4L], "- `e`")

  html <- report_html_doc(report)
  expect_identical(
    xml2::xml_text(xml2::xml_find_all(html, "//h2")),
    sub("^## ", "", text[startsWith(text, "## ")])
  )
  expect_setequal(
    xml2::xml_attr(xml2::xml_find_all(html, "//td/a[@id]"), "id"),
    report$evidence$anchor
  )
  expect_match(xml2::xml_text(html), "Findings in", fixed = TRUE)
})

test_that("the scope names every active filter and the capped source read", {
  saved <- report_fixture(list(
    source_type = "otel",
    status = NA_character_,
    query = "Small",
    findings_only = TRUE,
    annotated_only = TRUE,
    annotation_ids = c("a", "d"),
    pattern = "event_error"
  ))
  report <- investigation_report(saved)
  expect_identical(
    report$scope$filters,
    c(
      source = "Source: otel",
      status = "Status: unknown",
      query = "Text contains \"Small\"",
      annotated = "Annotated when saved",
      pattern = "Finding: event error",
      findings = "With findings"
    )
  )
  text <- format(report)
  expect_true(
    "- Filters: Source: otel; Status: unknown; Text contains \"Small\"; Annotated when saved; Finding: event error; With findings." %in%
      text
  )
  expect_true(
    "- Source read: from 2026-10-01 00:00 UTC to 2026-10-04 00:00 UTC. Capped: the span ceiling of 1,000 was reached, so older traces were not read; only the 5 most recent of 9 conversations in the window were kept." %in%
      text
  )
  expect_identical(sum(startsWith(text, "- Source read:")), 2L)

  # A read that recorded no cap says so once; an unrecorded read is unknown.
  uncapped <- investigation_snapshot(
    cohort_fixture(),
    source = list(read_info = list(truncated = FALSE, incomplete = FALSE))
  )
  expect_identical(
    grep(
      "^- Source read",
      format(investigation_report(uncapped)),
      value = TRUE
    ),
    "- Source read: no time window recorded. The read recorded no cap."
  )
  # A reader's read details passed as the source itself are read the same;
  # other source fields are printed as recorded, never read as a cap.
  direct <- investigation_snapshot(
    cohort_fixture(),
    source = list(truncated = FALSE, incomplete = FALSE, kind = "nightly")
  )
  direct <- format(investigation_report(direct))
  expect_true(
    "- Source read: no time window recorded. The read recorded no cap." %in%
      direct
  )
  expect_true("- Source details as recorded: `kind` nightly." %in% direct)
  described <- investigation_snapshot(
    cohort_fixture(),
    source = list(kind = "bundled example", incomplete = TRUE)
  )
  described <- format(investigation_report(described))
  expect_true(
    "- Source details as recorded: `kind` bundled example; `incomplete` TRUE." %in%
      described
  )
  expect_no_match(described, "could not be read", fixed = TRUE)
  expect_true(
    "- Source read: not recorded, so whether it was capped is unknown." %in%
      described
  )
  unknown <- format(investigation_report(cohort_fixture()))
  expect_identical(
    grep("^- Source read", unknown, value = TRUE),
    rep("- Source read: not recorded, so whether it was capped is unknown.", 2L)
  )
})

test_that("a printed link reopens exactly the cohort the report describes", {
  views <- list(
    list(),
    list(findings_only = TRUE, pattern = "event_error"),
    list(source_type = "otel", status = NA_character_),
    list(query = "large"),
    list(query = "small", pattern = "repeated_tool_call")
  )
  for (view in views) {
    saved <- report_fixture(view)
    report <- investigation_report(saved, app_url = "https://scans.test/app/")
    app <- report_app_view(saved)
    for (link in c(report$scope$link, report$trajectories$link)) {
      expect_true(startsWith(link, "https://scans.test/app/#Cohort/"))
      target <- scans_app_parse_hash(
        sub("^[^#]*#", "", link),
        "Cohort"
      )
      restored <- scans_app_scope_restore(target$scope %||% list(), app$data)
      expect_null(restored$notice)
      expect_gt(nrow(report$trajectories), 0L)
      expect_identical(
        app$data$info$trajectory_id[scans_app_view_indices(app$data, restored)],
        report$trajectories$trajectory_id
      )
    }
  }

  # A filter term survives the link's encoding whatever it contains.
  odd <- investigation_report(
    report_fixture(list(query = "a&b=c ?scope=x#y/50%")),
    app_url = "https://scans.test/app/"
  )
  expect_identical(
    scans_app_parse_hash(sub("^[^#]*#", "", odd$scope$link), "Cohort")$scope,
    list(query = "a&b=c ?scope=x#y/50%")
  )

  report <- investigation_report(
    report_fixture(list(pattern = "event_error")),
    app_url = "https://scans.test/app/"
  )
  row <- report$evidence[!is.na(report$evidence$event_id), ][1L, ]
  target <- scans_app_parse_hash(sub("^[^#]*#", "", row$link), "Cohort")
  expect_identical(target$trajectory_id, row$trajectory_id)
  expect_identical(target$event_id, row$event_id)
  expect_identical(target$scope, list(pattern = "event_error"))
  expect_match(
    format(report),
    "They reopen exactly this cohort when the app at that address serves this saved investigation",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("the report says what a link cannot restore", {
  saved <- report_fixture(list(
    annotated_only = TRUE,
    annotation_ids = c("a", "c")
  ))
  report <- investigation_report(saved, app_url = "https://scans.test/app/")
  expect_identical(report$trajectories$trajectory_id, c("a", "c"))
  expect_match(
    report$scope$link_limits,
    "The annotation filter depends on annotations where the link is opened",
    fixed = TRUE,
    all = FALSE
  )
  # An app with no annotation membership drops the filter and says so.
  data <- report_app_view(saved)$data
  target <- scans_app_parse_hash(
    sub("^[^#]*#", "", report$scope$link),
    "Cohort"
  )
  expect_true(target$scope$annotated)
  restored <- scans_app_scope_restore(target$scope, data, annotations = FALSE)
  expect_false(restored$annotated_only)
  expect_match(restored$notice, "Annotated only", fixed = TRUE)
  expect_match(restored$notice, "wider cohort", fixed = TRUE)

  elsewhere <- scans_app_scope_restore(
    list(source = "vitals", status = "failed"),
    data
  )
  expect_null(elsewhere$source_type)
  expect_null(elsewhere$status)
  expect_match(elsewhere$notice, "Source: vitals, Status: failed", fixed = TRUE)

  # Without an address there are no links to qualify.
  plain <- investigation_report(saved)
  expect_null(plain$scope$link_limits)
  expect_true(all(is.na(plain$evidence$link)))
  expect_no_match(format(plain), "scans.test", fixed = TRUE)
  expect_snapshot(
    investigation_report(saved, app_url = "javascript:alert(1)"),
    error = TRUE
  )
})

test_that("user identifiers appear only on request", {
  skip_if_not_installed("htmltools")
  saved <- report_fixture(list(query = "ADA"))
  report <- investigation_report(saved, app_url = "https://scans.test/app/")
  text <- format(report)
  html <- format(report, "html")
  expect_null(report$users)
  expect_false("user" %in% names(report$trajectories))
  expect_no_match(text, "ada", ignore.case = TRUE)
  expect_no_match(html, "ada|\\bbo\\b", ignore.case = TRUE)
  expect_identical(
    report$scope$filters[["query"]],
    "Text filter (term omitted: it matches a user identifier)"
  )
  expect_match(
    report$scope$link_limits,
    "Links leave out the text filter",
    fixed = TRUE,
    all = FALSE
  )
  # The filter still narrowed the cohort.
  expect_identical(report$trajectories$trajectory_id, c("a", "b"))

  named <- investigation_report(saved, include_users = TRUE)
  expect_identical(named$users$value, "ada")
  expect_identical(named$trajectories$user, c("ada", "ada"))
  expect_identical(named$scope$filters[["query"]], "Text contains \"ADA\"")
  expect_true("## Users" %in% format(named))
})

test_that("hostile names, labels, and explanations stay text in both formats", {
  attack <- "<script>alert(1)</script><img src=x onerror=alert(2)>"
  markdown <- "](javascript:alert(3)) [x](http://evil.test) `|` **b**"
  bundle <- cohort_fixture()
  events <- trajectory_events(bundle)
  events$name[events$event_type == "tool_call"] <- paste(attack, markdown)
  events$error[!is.na(events$error)] <- paste("`", attack, markdown)
  info <- trajectory_info(bundle)
  info$model <- paste(attack, markdown)
  info$metadata <- lapply(info$metadata, function(metadata) {
    if (length(metadata)) list(otel = list(user = attack)) else metadata
  })
  bundle <- TrajectoryBundle(info, trajectory_turns(bundle), events)
  assessment <- assess_trajectory_scans(bundle)
  assessment$findings$explanation <- paste(attack, markdown)
  saved <- investigation_build(
    bundle,
    NULL,
    paste("App", attack, markdown),
    list(),
    list(
      scans = scan_registry()$scan,
      repeat_threshold = 2L,
      loop_threshold = 3L
    ),
    list(query = attack),
    c(
      list(summaries = summarize_trajectories(bundle)),
      assessment,
      list(measures = measure_trajectories(bundle))
    )
  )
  hidden <- investigation_report(saved, app_url = "https://scans.test/app/")
  shown <- investigation_report(
    saved,
    app_url = "https://scans.test/app/",
    include_users = TRUE
  )
  expect_gt(nrow(shown$evidence), 0L)
  expect_match(shown$scope$filters[["query"]], attack, fixed = TRUE)
  expect_no_match(hidden$scope$filters[["query"]], "script", fixed = TRUE)

  for (report in list(hidden, shown)) {
    for (doc in list(report_html_doc(report), report_md_doc(report))) {
      expect_length(xml2::xml_find_all(doc, "//script|//img|//b|//strong"), 0L)
      hrefs <- xml2::xml_attr(xml2::xml_find_all(doc, "//a[@href]"), "href")
      expect_true(all(grepl("^(#|https://scans\\.test/app/#)", hrefs)))
      expect_match(
        xml2::xml_text(doc),
        "<script>alert(1)</script>",
        fixed = TRUE
      )
      expect_match(xml2::xml_text(doc), "[x](http://evil.test)", fixed = TRUE)
    }
  }
  # A table row stays one row: the pipe in a name does not open a column.
  doc <- report_md_doc(shown)
  cells <- vapply(
    xml2::xml_find_all(doc, "//tr"),
    function(row) length(xml2::xml_find_all(row, "./td|./th")),
    integer(1)
  )
  tables <- xml2::xml_find_all(doc, "//table")
  expect_identical(
    sum(cells),
    sum(vapply(
      tables,
      function(table) {
        length(xml2::xml_find_all(table, ".//th")) *
          length(xml2::xml_find_all(table, ".//tr"))
      },
      integer(1)
    ))
  )
})

test_that("review decisions use the review case's own adjudication", {
  skip_if_not_installed("htmltools")
  saved <- report_fixture()
  finding <- saved$analysis$findings[1L, ]
  confirmed <- review_case(saved, finding$trajectory_id, finding$finding_id) |>
    review_decision("confirmed", "Reviewer One", "Private rationale text")
  disputed <- review_case(saved, "b", scan = "event_error") |>
    review_decision("missed", "Reviewer One", "Private rationale text") |>
    review_decision("rejected", "Reviewer Two", "Private rationale text")
  open <- review_case(saved, "e")

  report <- investigation_report(
    saved,
    reviews = list(confirmed, disputed, open)
  )
  expect_identical(
    report$reviews$status,
    c(
      review_status(confirmed)$status,
      review_status(disputed)$status,
      review_status(open)$status
    )
  )
  expect_identical(
    report$reviews$status,
    c("confirmed", "disputed", "unreviewed")
  )
  expect_identical(report$reviews$n_active, c(1L, 2L, 0L))
  expect_identical(
    report$reviews$anchor[[1L]],
    report$evidence$anchor[report$evidence$finding_id == finding$finding_id]
  )
  expect_false("reviewers" %in% names(report$reviews))
  text <- format(report)
  expect_true("## Review decisions" %in% text)
  expect_no_match(text, "Reviewer One|Private rationale")
  expect_no_match(format(report, "html"), "Reviewer One|Private rationale")

  named <- investigation_report(
    saved,
    reviews = list(disputed),
    include_users = TRUE
  )
  expect_identical(
    named$reviews$reviewers,
    list(c("Reviewer One", "Reviewer Two"))
  )
  expect_no_match(format(named), "Private rationale")

  other <- review_case(investigation_snapshot(performance_fixture()), "slow")
  expect_snapshot(
    investigation_report(saved, reviews = list(other)),
    error = TRUE
  )
  expect_null(investigation_report(saved)$reviews)
  expect_false("## Review decisions" %in% format(investigation_report(saved)))
})

test_that("a long pattern lists a bounded sample and says so", {
  report <- investigation_report(report_fixture())
  pattern <- report$patterns[which.max(report$patterns$trajectories), ]
  expect_gt(pattern$trajectories, 1L)
  text <- format(report, max_evidence = 1L)
  expect_match(
    text,
    sprintf(
      "Listing the first 1 of %d conversations; the report's evidence table holds the rest.",
      pattern$trajectories
    ),
    fixed = TRUE,
    all = FALSE
  )
  listed <- report$evidence$anchor[
    report$evidence$scan == pattern$scan &
      report$evidence$trajectory_id ==
        report$evidence$trajectory_id[report$evidence$scan == pattern$scan][[
          1L
        ]]
  ]
  anchors <- regmatches(text, regexpr("finding-[0-9]+", text))
  expect_true(all(listed %in% anchors))
  expect_lt(length(anchors), nrow(report$evidence))
})

test_that("reports are written as standalone files chosen by extension", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("xml2")
  report <- investigation_report(report_fixture())
  directory <- withr::local_tempdir()
  html <- file.path(directory, "report.html")
  markdown <- file.path(directory, "report.md")
  expect_identical(write_investigation_report(report, html), report)
  write_investigation_report(report, markdown)
  expect_identical(readLines(markdown, encoding = "UTF-8"), format(report))
  expect_identical(
    paste(readLines(html, encoding = "UTF-8"), collapse = "\n"),
    format(report, "html")
  )
  doc <- xml2::read_html(html)
  expect_length(
    xml2::xml_find_all(
      doc,
      "//script|//link|//img|//iframe|//object|//*[@src]|//*[@style]"
    ),
    0L
  )
  expect_no_match(format(report, "html"), "url\\(|@import|https?://")
  expect_snapshot(error = TRUE, {
    write_investigation_report(report, html)
    write_investigation_report(report, file.path(directory, "report.pdf"))
    write_investigation_report(report_fixture(), markdown)
  })
})

test_that("a bundle is reported through a snapshot with default settings", {
  bundle <- performance_fixture()
  direct <- investigation_report(bundle)
  saved <- investigation_report(investigation_snapshot(bundle))
  direct$generated <- saved$generated
  expect_identical(direct, saved)
  expect_identical(direct$scope$noun$many, "trajectories")
  expect_identical(direct$headline$unassigned_losses, 1L)
  expect_identical(sum(direct$losses$unassigned), 1L)
  expect_match(
    format(direct),
    "1 further loss cannot be assigned to one trajectory",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("an empty scan selection reports unknown, not clean", {
  saved <- investigation_snapshot(cohort_fixture(), scans = character())
  report <- investigation_report(saved)
  expect_identical(report$headline$n_assessed, 0L)
  expect_identical(report$headline$n_not_assessed, 5L)
  text <- format(report)
  expect_true(
    "- None of the 5 conversations could be assessed by a selected scan, so findings are unknown, not absent." %in%
      text
  )
  expect_true(
    "No scans were selected, so no patterns were looked for." %in% text
  )
  expect_no_match(text, "No findings in", fixed = TRUE)
})
