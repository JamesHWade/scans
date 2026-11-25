review_queue_server <- function(store, ...) {
  scans_app_server(
    scans_app_sources(list(Cohort = cohort_fixture())),
    annotations = store,
    ...
  )
}

test_that("a disposition appends one record and opens the next unreviewed", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  skip_if_not_installed("jsonlite")
  store <- scans_annotations(
    withr::local_tempfile(fileext = ".jsonl"),
    labels = c("tool failure", "looks right")
  )

  shiny::testServer(review_queue_server(store), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    ids <- data()$info$trajectory_id
    mark <- function(label, id) {
      session$setInputs(
        scans_app_disposition = list(
          label = label,
          trajectory_id = id,
          nonce = stats::runif(1)
        )
      )
    }
    expect_identical(ids[visible()], c("c", "a", "d", "e", "b"))
    expect_identical(ids[[selected()]], "c")

    mark("tool failure", "c")
    expect_identical(nrow(store$read()), 1L)
    expect_identical(ids[[selected()]], "a")
    expect_identical(ids[visible()], c("a", "d", "e", "b", "c"))
    expect_match(output$scans_app_review_bar$html, "Not reviewed yet")
    expect_match(
      output$scans_app_review_bar$html,
      "Marked tool failure. Opened the next unreviewed.",
      fixed = TRUE
    )
    expect_match(
      output$scans_app_review_progress$html,
      "1 of 3 flagged conversations reviewed",
      fixed = TRUE
    )

    # A repeat aimed at the trajectory already left behind is dropped.
    mark("tool failure", "c")
    mark("not a label", "a")
    expect_identical(nrow(store$read()), 1L)
    expect_identical(ids[[selected()]], "a")

    session$setInputs(scans_app_advance = FALSE)
    mark("", "a")
    mark("", "a")
    records <- store$read()
    expect_identical(nrow(records), 2L)
    expect_identical(records$note[[1L]], "Reviewed")
    expect_true(is.na(records$label[[1L]]))
    expect_identical(ids[[selected()]], "a")
    expect_match(output$scans_app_review_bar$html, "Already marked reviewed.")
    expect_match(output$scans_app_entries$html, ">Reviewed<", fixed = TRUE)

    session$setInputs(scans_app_next_unreviewed = 1L)
    expect_identical(ids[[selected()]], "d")
    session$setInputs(scans_app_advance = TRUE)
    mark("looks right", "d")
    mark("looks right", "e")
    mark("looks right", "b")
    expect_identical(nrow(store$read()), 5L)
    expect_identical(ids[[selected()]], "b")
    expect_match(output$scans_app_review_bar$html, "Queue done")
    expect_identical(performance()$cohort$progress$flagged_reviewed, 3L)
    expect_identical(performance()$patterns$reviewed, c(2L, 2L, 1L, 1L))
  })
})

test_that("review-state filters narrow the browser and keep progress honest", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  skip_if_not_installed("jsonlite")
  store <- scans_annotations(
    withr::local_tempfile(fileext = ".jsonl"),
    labels = c("tool failure", "looks right")
  )
  store$append("Cohort", "c", label = "tool failure", note = NULL)
  store$append("Cohort", "b", label = NA_character_, note = "Fine")
  store$append("Other application", "a", label = "looks right", note = NULL)

  shiny::testServer(review_queue_server(store), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    ids <- data()$info$trajectory_id
    session$setInputs(scans_app_review_state = "reviewed")
    expect_setequal(ids[visible()], c("b", "c"))
    expect_identical(review_progress()$flagged_reviewed, 1L)
    expect_identical(review_progress()$flagged, 3L)
    expect_match(output$scans_app_pattern_filter$html, "Reviewed")

    session$setInputs(scans_app_review_label = "tool failure")
    expect_identical(ids[visible()], "c")
    session$setInputs(scans_app_review_label = "(no label)")
    expect_identical(ids[visible()], "b")
    session$setInputs(
      scans_app_review_label = "",
      scans_app_review_state = "needs"
    )
    expect_identical(ids[visible()], c("a", "d", "e"))
    session$setInputs(scans_app_sort = "newest")
    expect_identical(ids[visible()], c("d", "a", "e"))
  })
})

test_that("without a store the queue is ranked and nothing claims a state", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    ids <- data()$info$trajectory_id
    expect_true(all(is.na(queue()$reviewed)))
    session$setInputs(
      scans_app_review_state = "reviewed",
      scans_app_disposition = list(label = "", trajectory_id = "a")
    )
    expect_length(visible(), 5L)
    session$setInputs(scans_app_sort = "review")
    expect_identical(ids[visible()], c("c", "a", "d", "e", "b"))
    html <- output$scans_app_performance$html
    expect_match(html, "No annotation store is configured", fixed = TRUE)
    expect_no_match(html, "scans-app-pattern-reviewed", fixed = TRUE)
    expect_no_match(html, ">Reviewed<", fixed = TRUE)
  })
  ui <- as.character(scans_app_ui(scans_app_sources(cohort_fixture())))
  expect_no_match(ui, "scans_app_review_bar|scans_app_review_state")
})
