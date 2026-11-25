governance_deputy_bundle <- function() {
  as_trajectory_otel(read_otel_traces(test_path("fixtures", "deputy", "otel")))
}

governance_deputy_data <- function() {
  scans_app_data(governance_deputy_bundle())
}

governance_commons_bundle <- function() {
  as_trajectory_commons(route_commons_cohort())
}

test_that("wording covers both Deputy scans and every intervention kind", {
  kinds <- c(
    "limit_wrap_up",
    "limit_stop",
    "provider_fallback",
    "compaction_failure",
    "repeated_compaction",
    "permission_denied",
    "approval_pending",
    "delegation_missing_evidence",
    "delegation_unresolved_work"
  )
  expect_identical(
    cohort_scan_word(rep("run_intervention", 9L), "reason", kinds),
    c(
      "Wrapped up at limit",
      "Stopped at limit",
      "Provider fallback",
      "Compaction failed",
      "Repeated compaction",
      "Permission denied",
      "Approval pending",
      "Delegation: missing evidence",
      "Delegation: unresolved work"
    )
  )
  expect_identical(
    cohort_scan_word("cut_off_answer", "reason"),
    "Answer cut off"
  )
  expect_identical(
    cohort_scan_word("cut_off_answer", "label"),
    "Cut-off answers"
  )
  expect_identical(
    scans_app_pattern_label("run_intervention"),
    "Run interventions"
  )
  expect_identical(cohort_scan_word(character(), "reason"), character())
})

test_that("Deputy runs are queued with their recorded interventions", {
  data <- governance_deputy_data()
  ids <- data$info$trajectory_id
  queue <- cohort_review_queue(data, ids)
  reasons <- stats::setNames(queue$reasons, queue$trajectory_id)

  expect_identical(queue$class[queue$trajectory_id == "otel/c_cutoff"], "error")
  expect_identical(reasons[["otel/c_cutoff"]], "Answer cut off")
  expect_identical(
    reasons[["otel/c_denied"]],
    c("Event errors", "Permission denied")
  )
  expect_identical(reasons[["otel/c_review"]], "Provider fallback")

  interventions <- cohort_interventions(data, ids)
  expect_identical(
    interventions$kind,
    c("provider_fallback", "permission_denied")
  )
  expect_identical(
    interventions$label,
    c("Provider fallback", "Permission denied")
  )
  expect_identical(interventions$trajectories, c(1L, 1L))
  expect_identical(interventions$findings, c(2L, 1L))
  expect_identical(attr(interventions, "n_deputy"), 5L)
  expect_match(
    attr(interventions, "limitations"),
    "trace cannot rule those out"
  )
  expect_identical(nrow(cohort_interventions(data, "otel/c_team")), 0L)
  expect_identical(
    data$deputy,
    c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("a token total counts a delegated run once, through its lead", {
  data <- governance_deputy_data()
  ids <- data$info$trajectory_id
  team <- ids[startsWith(ids, "otel/c_team")]
  tokens <- cohort_trajectories(data, team)$tokens

  expect_identical(tokens, c(30, 15))
  together <- cohort_headline(data, team)
  expect_identical(together$total_tokens, 30)
  expect_identical(together$n_tokens_counted, 1L)
  expect_identical(together$n_delegated, 1L)
  expect_identical(together$median_tokens, 22.5)

  # A child whose lead is outside the cohort is counted on its own.
  alone <- cohort_headline(data, team[[2L]])
  expect_identical(alone$total_tokens, 15)
  expect_identical(alone$n_delegated, 0L)
  expect_identical(
    cohort_headline(data, character())$total_tokens,
    NA_real_
  )
})

test_that("a token total leaves out a run any included ancestor accounts for", {
  skip_if_not_installed("deputy", "0.0.0.9007")
  skip_if_not_installed("ellmer", "0.5.0")

  fixture <- deputy_delegation_fixture()
  data <- scans_app_data(
    as_trajectory_deputy(fixture$lead, delegations = fixture$history)
  )
  ids <- data$info$trajectory_id
  expect_identical(cohort_trajectories(data, ids)$tokens, c(11, 7, 7))
  total <- function(data, ids) {
    headline <- cohort_headline(data, ids)
    c(headline$total_tokens, headline$n_tokens_counted, headline$n_delegated)
  }

  expect_identical(total(data, ids), c(11, 1, 2))
  # The lead and its grandchild, without the run between them.
  expect_identical(total(data, ids[c(1L, 3L)]), c(11, 1, 1))
  expect_identical(total(data, ids[2:3]), c(7, 1, 1))
  expect_identical(total(data, ids[[3L]]), c(7, 1, 0))

  # A parent the snapshot does not hold ends the walk, and so does a cycle.
  absent <- data
  absent$records$parent_trajectory_id[[2L]] <- "deputy/absent"
  expect_identical(total(absent, ids[c(1L, 3L)]), c(18, 2, 0))
  cycle <- data
  cycle$records$parent_trajectory_id[[1L]] <- ids[[3L]]
  expect_identical(total(cycle, ids[[3L]]), c(7, 1, 0))
})

test_that("provenance counts keep unclassified apart from untrusted", {
  skip_if_not_installed("ellmer", "0.5.0")
  data <- scans_app_data(governance_commons_bundle())
  ids <- data$info$trajectory_id
  provenance <- cohort_provenance(data, ids)

  expect_identical(
    provenance$class,
    c("trusted", "cited", "untrusted", "unclassified")
  )
  expect_identical(provenance$exchanges, c(1L, 3L, 3L, 2L))
  expect_identical(
    sum(provenance$exchanges),
    attr(provenance, "total_exchanges")
  )
  expect_identical(attr(provenance, "total_trajectories"), 4L)
  expect_equal(sum(provenance$share), 1)
  expect_identical(
    vapply(provenance$series, sum, integer(1)),
    provenance$exchanges
  )
  expect_identical(attr(cohort_activity(data, ids), "basis"), "activity")
  expect_identical(cohort_headline(data, ids)$time_basis, "activity")

  one <- cohort_provenance(data, ids[[1L]])
  expect_identical(one$exchanges, c(1L, 0L, 1L, 0L))
  plain <- cohort_fixture_data()
  expect_null(plain$exchanges)
  expect_identical(
    attr(cohort_provenance(plain, plain$info$trajectory_id), "total_exchanges"),
    0L
  )
})

test_that("review next offers one trajectory per prevalent pattern", {
  data <- cohort_fixture_data()
  ids <- data$info$trajectory_id
  patterns <- cohort_patterns(data, ids)
  queue <- cohort_review_queue(data, ids)
  picks <- cohort_review_next(queue, data$findings, patterns)

  expect_identical(picks$trajectory_id, c("c", "a", "d", "e", "b"))
  expect_identical(
    picks$pattern,
    c("event_error", "repeated_tool_call", NA, NA, NA)
  )
  reviewed <- cohort_review_queue(data, ids, cohort_fixture_annotations())
  picks <- cohort_review_next(reviewed, data$findings, patterns)
  expect_identical(picks$trajectory_id, c("a", "d", "e"))
  expect_identical(picks$pattern, c("event_error", "repeated_tool_call", NA))
})

test_that("kind, provenance, and route facets narrow both panes", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  skip_if_not_installed("ellmer", "0.5.0")
  app <- scans_app(list(
    Deputy = governance_deputy_bundle(),
    Commons = governance_commons_bundle()
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    pick <- function(facet, value) {
      session$setInputs(scans_app_facet = list(facet = facet, value = value))
    }
    ids <- data()$info$trajectory_id
    pick("kind", "provider_fallback")
    expect_identical(ids[visible()], "otel/c_review")
    expect_identical(performance()$n, 1L)
    expect_match(output$scans_app_pattern_filter$html, "Provider fallback")
    pick("kind", "limit_stop")
    expect_identical(facets(), list())
    expect_null(candidates())
    expect_match(output$scans_app_performance$html, "Run interventions")
    expect_no_match(output$scans_app_performance$html, "Answer provenance")

    session$setInputs(scans_app_application = "Commons")
    ids <- data()$info$trajectory_id
    expect_identical(facets(), list())
    html <- output$scans_app_performance$html
    expect_match(html, "Answer provenance", fixed = TRUE)
    expect_match(html, "Promotion candidates", fixed = TRUE)
    expect_no_match(html, "Run interventions", fixed = TRUE)
    top <- candidates()$route_signature[[1L]]
    expect_identical(top, "describe_table > run_sql > run_r")

    pick("provenance", "trusted")
    trusted <- unique(data()$exchanges$trajectory_id[
      data()$exchanges$provenance_class == "trusted"
    ])
    expect_setequal(ids[visible()], trusted)
    n_trusted <- length(trusted)
    expect_identical(performance()$n, n_trusted)
    pick("provenance", "trusted")
    pick("route", top)
    routed <- unique(data()$exchanges$trajectory_id[
      data()$exchanges$route_signature %in% top
    ])
    expect_setequal(ids[visible()], routed)
    expect_match(output$scans_app_pattern_filter$html, "Route:")
    expect_true(all(
      unlist(candidates()$trajectory_ids) %in% ids[visible()]
    ))
    pick("route", "not a route")
    expect_identical(facets(), list())
  })
})

test_that("the header states the run ending and recorded lineage", {
  skip_if_not_installed("htmltools")
  data <- governance_deputy_data()
  ids <- data$info$trajectory_id
  header <- function(id) {
    gsub("\\s+", " ", as.character(scans_app_header_ui(data, match(id, ids))))
  }

  cut_off <- header("otel/c_cutoff")
  expect_match(cut_off, "Run completed", fixed = TRUE)
  expect_match(cut_off, "scans-app-run-ending-cut-off", fixed = TRUE)
  expect_match(cut_off, "Answer cut off \u00b7 max_tokens", fixed = TRUE)
  expect_match(header("otel/c_review"), ">\\s*Answered\\s*<")
  expect_no_match(header(ids[[1L]]), "scans-app-run-ending", fixed = TRUE)

  child <- ids[startsWith(ids, "otel/c_team/")]
  expect_match(
    header("otel/c_team"),
    sprintf('Subagent run: <button[^>]*data-scans-trajectory="%s"', child)
  )
  expect_match(
    header(child),
    'Delegated by <button[^>]*data-scans-trajectory="otel/c_team"'
  )
  expect_no_match(header("otel/c_cutoff"), "scans-app-lineage", fixed = TRUE)
  expect_match(
    as.character(scans_app_entry_ui(
      data$records[match(child, ids), , drop = FALSE],
      FALSE
    )),
    "scans-app-subagent",
    fixed = TRUE
  )

  expect_identical(
    scans_app_run_ending(list(
      ending = list(class = "RunEndingStopped", by = "caller")
    ))$text,
    "Stopped by caller"
  )
  expect_identical(
    scans_app_run_ending(list(
      ending = list(
        class = "RunEndingLimit",
        field = "turns",
        wrapped_up = TRUE
      )
    ))$text,
    "Limit reached \u00b7 turns \u00b7 wrapped up"
  )
  expect_identical(
    scans_app_run_ending(list(
      ending = list(class = "RunEndingFailed", phase = "tool")
    ))$text,
    "Failed in tool"
  )
  expect_null(scans_app_run_ending(list()))
})

test_that("the transcript marks exchange provenance and internal turns", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("ellmer", "0.5.0")
  data <- scans_app_data(governance_commons_bundle())
  html <- as.character(scans_app_transcript_ui(data, 1L))

  expect_identical(lengths(gregexpr("scans-app-provenance-chip", html)), 2L)
  expect_match(html, "Exchange 1 \u00b7 Untrusted", fixed = TRUE)
  expect_match(html, "Exchange 2 \u00b7 Trusted", fixed = TRUE)
  expect_match(html, 'title="Recorded by the application"', fixed = TRUE)
  expect_match(html, "scans-app-msg-internal", fixed = TRUE)
  expect_match(html, "Internal reminder", fixed = TRUE)

  conflicted <- as.character(scans_app_transcript_ui(data, 3L))
  expect_match(conflicted, "Unclassified", fixed = TRUE)
  expect_match(conflicted, "Conflicting provenance records", fixed = TRUE)
  expect_no_match(
    as.character(scans_app_transcript_ui(cohort_fixture_data(), 1L)),
    "scans-app-provenance-chip",
    fixed = TRUE
  )
})

test_that("provenance and candidates render with denominators and inert snippets", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- governance_commons_bundle()
  data <- scans_app_data(bundle)
  ids <- data$info$trajectory_id
  candidates <- route_candidates(
    bundle,
    trajectory_ids = ids,
    exchanges = data$exchanges,
    measures = NULL
  )
  performance <- scans_app_performance_data(
    data,
    seq_along(ids),
    candidates = candidates
  )
  html <- as.character(scans_app_performance_ui(
    performance,
    "Commons",
    "elapsed",
    scan_registry()$scan
  ))
  text <- gsub("\\s+", " ", gsub("<[^>]+>", " ", html))

  expect_match(
    text,
    "9 exchanges in 4 conversations that record provenance",
    fixed = TRUE
  )
  expect_match(
    html,
    "Untrusted: 3 of 9 exchanges, in 3 of 4 conversations",
    fixed = TRUE
  )
  expect_match(html, "Unclassified: 2 of 9 exchanges", fixed = TRUE)
  expect_match(html, 'data-scans-facet="provenance"', fixed = TRUE)
  expect_match(html, 'data-scans-facet="route"', fixed = TRUE)
  expect_match(text, "Recorded tool input and output", fixed = TRUE)
  expect_match(text, "never executed", fixed = TRUE)
  expect_no_match(html, "<script", fixed = TRUE)

  report <- paste(export_report(performance, "Commons"), collapse = "\n")
  expect_match(report, "## Answer provenance", fixed = TRUE)
  expect_match(report, "| Untrusted | 3 of 9 | 3 |", fixed = TRUE)
  expect_match(report, "## Promotion candidates", fixed = TRUE)
  expect_match(report, "| `describe_table > run_sql > run_r` |", fixed = TRUE)
  expect_no_match(report, "SELECT", fixed = TRUE)

  deputy <- governance_deputy_data()
  deputy_report <- paste(
    export_report(scans_app_performance_data(deputy, 1:6), "Deputy"),
    collapse = "\n"
  )
  expect_match(
    deputy_report,
    "| Provider fallback | 1 of 5 | 2 |",
    fixed = TRUE
  )
  expect_match(deputy_report, "trace cannot rule those out", fixed = TRUE)
  expect_match(
    deputy_report,
    "Tokens: median 15 (recorded for 6 of 6)",
    fixed = TRUE
  )
})

test_that("the timeline has one linked mark per timed event and shortens idle gaps", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  expect_identical(nrow(cohort_timeline(data, "a")), 0L)
  expect_null(scans_app_timeline_ui(cohort_timeline(data, "a")))

  timed <- data$events$trajectory_id == "a"
  data$events$timestamp[timed] <- as.POSIXct(
    "2026-10-01 10:00:00",
    tz = "UTC"
  ) +
    c(0, 1, 200, 204)
  marks <- cohort_timeline(data, "a")

  expect_identical(marks$event_id, c("a-1", "a-2", "a-3", "a-4"))
  expect_identical(marks$lane, rep("tool", 4L))
  expect_identical(marks$duration, c(1, NA, 4, NA))
  expect_identical(marks$failed, c(FALSE, FALSE, TRUE, TRUE))
  expect_identical(is.na(marks$width), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(range(marks$x), c(0, 1))
  breaks <- attr(marks, "breaks")
  expect_identical(nrow(breaks), 1L)
  expect_identical(breaks$seconds, 199)
  expect_lt(marks$x[[3L]] - marks$x[[2L]], 0.1)
  expect_identical(
    nrow(attr(cohort_timeline(data, "a", max_gap = 300), "breaks")),
    0L
  )

  ui <- htmltools::tagQuery(scans_app_timeline_ui(marks))
  links <- ui$find("a.scans-app-timeline-mark")$selectedTags()
  expect_length(links, 4L)
  expect_identical(
    unname(vapply(links, htmltools::tagGetAttribute, "", "data-scans-event")),
    marks$event_id
  )
  expect_identical(
    htmltools::tagGetAttribute(links[[3L]], "aria-label"),
    "Tools: lookup, failed at 10:03:20, 4.0 s"
  )
  expect_match(
    htmltools::tagGetAttribute(links[[2L]], "class"),
    "scans-app-timeline-tick",
    fixed = TRUE
  )
  expect_equal(ui$find(".scans-app-timeline-list li")$length(), 4L)
  expect_match(
    as.character(scans_app_timeline_ui(marks)),
    "1 idle gap shortened",
    fixed = TRUE
  )
  expect_null(scans_app_timeline_ui(marks[1L, ]))

  deputy <- governance_deputy_data()
  lifecycle <- cohort_timeline(deputy, "otel/c_review")
  expect_identical(unique(lifecycle$lane), "lifecycle")
  expect_true(all(is.na(lifecycle$width)))
  expect_match(
    as.character(scans_app_transcript_ui(deputy, 2L)),
    "scans-app-timeline",
    fixed = TRUE
  )
})
