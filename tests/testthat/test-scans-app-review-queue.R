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

test_that("a review-filtered report gives the overview's review progress", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  skip_if_not_installed("jsonlite")
  store <- scans_annotations(
    withr::local_tempfile(fileext = ".jsonl"),
    labels = c("tool failure", "looks right")
  )
  store$append("Cohort", "c", label = "tool failure", note = NULL)

  shiny::testServer(review_queue_server(store), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    ids <- data()$info$trajectory_id
    session$setInputs(scans_app_review_state = "needs")
    expect_identical(ids[visible()], c("a", "d", "e", "b"))
    # The on-screen queue still counts the cohort before the review filter.
    expect_identical(review_progress()$flagged_reviewed, 1L)
    expect_identical(review_progress()$flagged, 3L)

    session$setInputs(scans_app_export_report = 1L)
    text <- paste(export_text(), collapse = "\n")
    expect_match(
      text,
      "- Filters: Needs review.",
      fixed = TRUE
    )
    expect_match(
      text,
      "- Review: 1 of 3 flagged conversations reviewed, counted without the review filter.",
      fixed = TRUE
    )
    expect_match(text, "#Cohort/a?scope=review=needs>)", fixed = TRUE)
    expect_match(
      text,
      "The review filter depends on the annotation store of the app the link is opened in.",
      fixed = TRUE
    )

    session$setInputs(
      scans_app_review_state = "reviewed",
      scans_app_review_label = "tool failure"
    )
    session$setInputs(
      scans_app_hash = "Cohort/?scope=review=reviewed&review_label=tool%20failure&user=bo"
    )
    expect_identical(ids[visible()], "c")
    text <- paste(export_text(), collapse = "\n")
    expect_match(
      text,
      "- Filters: Reviewed; Label: tool failure; One user (identifier omitted).",
      fixed = TRUE
    )
    expect_match(
      text,
      "- Review: 1 of 1 flagged conversations reviewed, counted without the review filter.",
      fixed = TRUE
    )
    expect_no_match(text, "\\bbo\\b")
    expect_match(
      text,
      "#Cohort/c?scope=review=reviewed&review_label=tool%20failure>)",
      fixed = TRUE
    )
  })
})

test_that("an exported link carries the review filter and reopens it", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  skip_if_not_installed("jsonlite")
  store <- scans_annotations(
    withr::local_tempfile(fileext = ".jsonl"),
    labels = c("tool failure", "looks right")
  )
  store$append("Cohort", "c", label = "tool failure", note = NULL)
  sent <- new.env(parent = emptyenv())
  local_mocked_bindings(
    updateSelectInput = function(session, inputId, ..., selected = NULL) {
      sent[[inputId]] <- selected
    },
    showNotification = function(ui, ...) {
      sent$notice <- c(sent$notice, ui)
    },
    .package = "shiny"
  )

  shiny::testServer(review_queue_server(store), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(
      scans_app_review_state = "reviewed",
      scans_app_review_label = "tool failure"
    )
    hash <- sub("^[^#]*#", "", link()())
    expect_identical(
      hash,
      "Cohort/?scope=review=reviewed&review_label=tool%20failure"
    )
    session$setInputs(
      scans_app_review_state = "all",
      scans_app_review_label = ""
    )

    session$setInputs(scans_app_hash = hash)
    expect_identical(sent$scans_app_review_state, "reviewed")
    expect_identical(sent$scans_app_review_label, "tool failure")
    expect_null(sent$notice)

    # A scope without a review filter clears the one in force.
    session$setInputs(scans_app_hash = "Cohort/?scope=findings=TRUE")
    expect_identical(sent$scans_app_review_state, "all")
    expect_identical(sent$scans_app_review_label, "")

    # A label this store does not offer cannot be selected, and says so.
    session$setInputs(
      scans_app_hash = "Cohort/?scope=review=needs&review_label=retired"
    )
    expect_identical(sent$scans_app_review_state, "needs")
    expect_identical(sent$scans_app_review_label, "")
    expect_length(sent$notice, 1L)
    expect_match(sent$notice, "Label: retired", fixed = TRUE)
    expect_match(sent$notice, "wider cohort", fixed = TRUE)
  })
})

test_that("a review-filtered link opened without a store says what it dropped", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  sent <- new.env(parent = emptyenv())
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      sent$notice <- c(sent$notice, ui)
    },
    .package = "shiny"
  )
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(
      scans_app_hash = "Cohort/?scope=review=reviewed&model=large"
    )
    expect_identical(facets(), list(model = "large"))
    expect_length(sent$notice, 1L)
    expect_match(sent$notice, "Reviewed", fixed = TRUE)
    expect_match(sent$notice, "no annotation store", fixed = TRUE)

    session$setInputs(scans_app_hash = "Cohort/?scope=model=small")
    expect_length(sent$notice, 1L)
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

test_that("a long browser list shows a bounded head and says what it left out", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")
  data <- cohort_fixture_data()
  queue <- cohort_review_queue(data, data$info$trajectory_id)
  entries <- function(html) {
    lengths(regmatches(html, gregexpr("scans-app-queue-entry", html)))
  }

  whole <- as.character(scans_app_entries_ui(data, queue$index, queue))
  expect_identical(entries(whole), 5L)
  expect_no_match(whole, "scans-app-entries-more", fixed = TRUE)

  head <- as.character(scans_app_entries_ui(
    data,
    queue$index,
    queue,
    chosen = queue$index[[4L]],
    limit = 3L
  ))
  expect_identical(entries(head), 3L)
  expect_match(
    head,
    "Listing the first 3 of 5 conversations in this order.",
    fixed = TRUE
  )
  # The listed entries are the first three in the order given.
  present <- vapply(
    queue$index,
    function(index) {
      grepl(sprintf("id=\"%s\"", scans_app_entry_id(index)), head, fixed = TRUE)
    },
    logical(1)
  )
  expect_identical(present, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(
    as.character(scans_app_entries_ui(data, queue$index, queue, limit = 5L)),
    whole
  )
})

test_that("an entry opens its trajectory once it is listed", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  local_mocked_bindings(scans_app_entry_limit = 2L)
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_scans = scan_registry()$scan,
      scans_app_sort = "oldest"
    )
    ids <- data()$info$trajectory_id
    expect_identical(ids[visible()], c("a", "b", "c", "d", "e"))
    expect_match(
      output$scans_app_entries$html,
      "Listing the first 2 of 5 conversations",
      fixed = TRUE
    )
    click <- function(index) {
      do.call(
        session$setInputs,
        stats::setNames(list(index + 100L), scans_app_entry_id(index))
      )
    }
    click(2L)
    expect_identical(selected_trajectory_id(), "b")
    # "d" is past the listed head, so nothing on the page can click it yet.
    session$setInputs(scans_app_query = "large")
    expect_identical(ids[visible()], c("c", "d"))
    click(4L)
    expect_identical(selected_trajectory_id(), "d")
    # Every trajectory stays reachable by stepping, listed or not.
    session$setInputs(scans_app_query = "")
    session$setInputs(scans_app_nav = list(direction = "next", nonce = 1))
    expect_identical(selected_trajectory_id(), "e")
  })
})
