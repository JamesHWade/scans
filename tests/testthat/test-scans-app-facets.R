facet_ids <- function(data, indices) {
  sort(data$info$trajectory_id[indices])
}

test_that("facet toggles set, replace, and clear values", {
  facets <- scans_app_facet_toggle(list(), "tool", "lookup")
  expect_identical(facets, list(tool = "lookup"))
  facets <- scans_app_facet_toggle(facets, "tool_failed", "lookup")
  expect_identical(facets, list(tool = "lookup", tool_failed = TRUE))
  expect_identical(
    scans_app_facet_toggle(facets, "tool", "lookup"),
    list(tool = "lookup")
  )
  expect_identical(
    scans_app_facet_toggle(facets, "tool_failed", "lookup"),
    list()
  )
  expect_identical(
    scans_app_facet_toggle(list(), "tool_failed", NULL),
    list(tool_failed = TRUE)
  )
  expect_identical(
    scans_app_facet_toggle(list(user = "ada"), "user", "ada"),
    list()
  )
  expect_identical(
    scans_app_facet_toggle(list(user = "ada"), "user", "bo"),
    list(user = "bo")
  )
  expect_identical(scans_app_facet_toggle(list(), "user", NA), list())
})

test_that("scope labels name every active filter once", {
  expect_identical(
    scans_app_scope_labels(
      list(tool = "lookup", tool_failed = TRUE, day = "2026-10-03"),
      pattern = "event_error",
      findings_only = TRUE
    ),
    c(
      pattern = "Finding: event error",
      findings = "With findings",
      tool = "lookup failed",
      day = "Oct 03"
    )
  )
  expect_identical(
    scans_app_scope_labels(list(day = "2026-10-03T10", tool_failed = TRUE)),
    c(tool = "Any tool call failed", day = "Oct 03 10:00")
  )
  expect_null(scans_app_scope_labels(list()))
})

test_that("each facet narrows the browser and the overview together", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    pick <- function(facet, value = NULL) {
      session$setInputs(scans_app_facet = list(facet = facet, value = value))
    }
    expect_shown <- function(ids) {
      expect_identical(facet_ids(data(), visible()), ids)
      n <- length(ids)
      expect_identical(performance()$n, n)
      expect_identical(sort(performance()$trajectories$trajectory_id), ids)
      expect_match(
        output$scans_app_visible_count,
        sprintf("^%d of 5", n)
      )
    }

    pick("day", "2026-10-03")
    expect_shown(c("c", "d"))
    expect_identical(performance()$cohort$activity$n, c(2L, 0L, 2L))
    expect_identical(performance()$cohort$headline$tool_calls, 4L)
    pick("day", "2026-10-03")
    expect_shown(c("a", "b", "c", "d", "e"))

    pick("tool", "lookup")
    expect_shown(c("a", "d"))
    pick("tool_failed", "lookup")
    expect_shown("a")
    expect_match(
      output$scans_app_pattern_filter$html,
      "lookup failed",
      fixed = TRUE
    )
    pick("tool_failed", "lookup")
    expect_identical(facets(), list())

    pick("tool_failed")
    expect_shown(c("a", "c"))
    pick("tool_failed")

    pick("user", "ada")
    expect_shown(c("a", "b"))
    session$setInputs(scans_app_query = "unopened")
    expect_shown("b")
    session$setInputs(scans_app_query = "")
    pick("user", "Unknown")
    expect_shown(c("d", "e"))
    pick("user", "Unknown")

    pick("model", "large")
    expect_shown(c("c", "d"))
    pick("day", "2026-10-01")
    expect_shown(character())
    pick("day", "2026-10-03")

    session$setInputs(scans_app_facet_clear = list(facet = "model"))
    expect_identical(facets(), list(day = "2026-10-03"))
    session$setInputs(scans_app_pattern = "event_error")
    expect_shown("c")
    expect_match(output$scans_app_pattern_filter$html, "Clear all")
    session$setInputs(scans_app_facet_clear = list(facet = "all"))
    expect_identical(facets(), list())
    expect_null(pattern_filter())
    expect_shown(c("a", "b", "c", "d", "e"))
  })
})

test_that("facets ignore values the snapshot does not contain", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    for (request in list(
      list(facet = "tool", value = "missing"),
      list(facet = "day", value = "2026-10-02"),
      list(facet = "user", value = list("ada", "bo")),
      list(facet = "agent", value = "ada"),
      list(value = "ada")
    )) {
      session$setInputs(scans_app_facet = request)
      expect_identical(facets(), list())
    }
    expect_length(visible(), 5L)
  })
})

test_that("facets reset when the application changes or reloads without them", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  loads <- new.env(parent = emptyenv())
  loads$bundle <- cohort_fixture()
  app <- scans_app(list(
    First = function() loads$bundle,
    Second = performance_fixture()
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(scans_app_facet = list(facet = "tool", value = "lookup"))
    expect_length(visible(), 2L)
    session$setInputs(scans_app_application = "Second")
    expect_identical(facets(), list())
    expect_length(visible(), 3L)

    session$setInputs(scans_app_application = "First")
    session$setInputs(scans_app_facet = list(facet = "user", value = "bo"))
    expect_identical(facets(), list(user = "bo"))
    loads$bundle <- performance_fixture()
    session$setInputs(scans_app_reload = 1L)
    expect_identical(facets(), list())
    expect_length(visible(), 3L)
  })
})

test_that("opening an investigation clears session facets", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  saved <- investigation_snapshot(investigation_bundle_fixture())
  app <- scans_app(list(Cohort = cohort_fixture()), investigations = TRUE)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(scans_app_facet = list(facet = "user", value = "ada"))
    expect_length(visible(), 2L)
    opened_investigation(saved)
    session$flushReact()
    investigation_ack_inputs(session)
    expect_identical(facets(), list())
    expect_length(visible(), nrow(data()$info))
  })
})

test_that("Ask scopes its tools to the faceted trajectories", {
  skip_if_not_installed("shinychat", "0.5.0")
  skip_if_not_installed("ellmer", "0.5.0")
  control <- new.env(parent = emptyenv())
  control$hold <- TRUE
  app <- scans_app(cohort_fixture(), chat_factory = function() {
    scans_test_chat(control)
  })

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_scans = scan_registry()$scan,
      scans_app_view = "application",
      scans_app_ask_scope = "filtered"
    )
    session$setInputs(scans_app_facet = list(facet = "model", value = "large"))
    session$setInputs(scans_app_ask_user_input = list("What failed?"))
    scans_test_drain(session, function() !is.null(control$resume))
    expect_setequal(chat_state$pinned()$context$ids, c("c", "d"))
    control$hold <- FALSE
    control$resume(NULL)
    scans_test_drain(session, function() {
      identical(chat_state$module$status(), "idle")
    })
  })
})
