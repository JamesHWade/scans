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
