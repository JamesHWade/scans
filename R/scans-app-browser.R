scans_app_outside_filters <- "Outside the current filters"

# Rendering an entry costs about a millisecond and a kilobyte of HTML, so a
# list of every trajectory in a large cohort takes tens of seconds to redraw
# on each filter change. docs/design/scale-profile.md records the measurement
# behind this number.
scans_app_entry_limit <- 500L

# The browser list: one entry per visible trajectory, in the order given, up
# to `limit`. Past that the list says how many it left out; they stay in the
# cohort, the overview, and the next and previous controls. `chosen` is the
# row to mark as open, if any.
scans_app_entries_ui <- function(
  data,
  indices,
  queue,
  chosen = NULL,
  limit = scans_app_entry_limit
) {
  listed <- utils::head(indices, limit)
  rows <- match(listed, queue$index)
  htmltools::tagList(
    lapply(seq_along(listed), function(i) {
      index <- listed[[i]]
      scans_app_entry_ui(
        data$records[index, , drop = FALSE],
        selected = identical(chosen, index),
        queue = queue[rows[[i]], , drop = FALSE]
      )
    }),
    if (length(listed) < length(indices)) {
      htmltools::tags$p(
        class = "scans-app-entries-more",
        role = "status",
        sprintf(
          "Listing the first %s of %s %s in this order. Narrow the filters or change the order to list others; all %s stay in the overview and in next and previous.",
          scans_app_count(length(listed)),
          scans_app_count(length(indices)),
          data$noun$many,
          scans_app_count(length(indices))
        )
      )
    }
  )
}

scans_app_browser_server <- function(
  input,
  output,
  session,
  state,
  chat_factory
) {
  # Stepping through the visible list, from the header buttons or the
  # keyboard. The list order is the sort order, so "next" means the next
  # entry the reviewer can see.
  move_selection <- function(step) {
    indices <- state$visible()
    if (length(indices) == 0L) {
      return()
    }
    position <- match(state$selected(), indices)
    if (is.na(position)) {
      state$selected(indices[[1L]])
      return()
    }
    target <- position + step
    if (target >= 1L && target <= length(indices)) {
      state$selected(indices[[target]])
    }
  }
  shiny::observeEvent(input$scans_app_next, ignoreInit = TRUE, {
    move_selection(1L)
  })
  shiny::observeEvent(input$scans_app_prev, ignoreInit = TRUE, {
    move_selection(-1L)
  })
  shiny::observeEvent(input$scans_app_nav, ignoreInit = TRUE, {
    direction <- input$scans_app_nav$direction
    if (identical(direction, "next")) {
      move_selection(1L)
    } else if (identical(direction, "prev")) {
      move_selection(-1L)
    }
  })

  # An entry needs its click observer only once it is listed. Creating one
  # per trajectory up front took most of a session's start on a large cohort.
  entry_observers <- new.env(parent = emptyenv())
  shiny::observe({
    current <- state$data()
    if (is.null(current) || nrow(current$records) == 0L) {
      return()
    }
    for (index in utils::head(state$visible(), scans_app_entry_limit)) {
      id <- scans_app_entry_id(index)
      if (exists(id, envir = entry_observers, inherits = FALSE)) {
        next
      }
      local({
        entry <- index
        observer <- shiny::observeEvent(input[[scans_app_entry_id(entry)]], {
          state$selected(entry)
          bslib::nav_select("scans_app_view", "trajectory", session = session)
        })
        assign(id, observer, envir = entry_observers)
      })
    }
  })

  # The search box names what it searches once the snapshot says what that is.
  shiny::observe({
    current <- state$data()
    shiny::req(current)
    label <- paste("Search", current$noun$many)
    shiny::updateTextInput(
      session,
      "scans_app_query",
      label = label,
      placeholder = label
    )
  })
  output$scans_app_browser_title <- shiny::renderText({
    current <- state$data()
    scans_app_title_case(
      if (is.null(current)) "trajectories" else current$noun$many
    )
  })
  output$scans_app_visible_count <- shiny::renderText({
    current <- state$data()
    if (is.null(current)) {
      return("Traces unavailable")
    }
    count <- length(state$visible())
    total <- nrow(current$records)
    sprintf(
      "%d of %d %s",
      count,
      total,
      if (total == 1L) current$noun$one else current$noun$many
    )
  })

  output$scans_app_position <- shiny::renderText({
    indices <- state$visible()
    position <- match(state$selected(), indices)
    if (is.null(state$selected())) {
      return("")
    }
    if (is.na(position)) {
      return(scans_app_outside_filters)
    }
    sprintf("%d of %d", position, length(indices))
  })

  output$scans_app_entries <- shiny::renderUI({
    current <- state$data()
    if (is.null(current)) {
      return(scans_app_source_error_ui(state$application()))
    }
    indices <- state$visible()
    if (length(indices) == 0L) {
      return(scans_app_empty_ui(
        if (nrow(current$records) == 0L) {
          paste0("This source has no ", current$noun$many, ".")
        } else {
          paste0("No ", current$noun$many, " match these filters.")
        }
      ))
    }
    scans_app_entries_ui(
      current,
      indices,
      state$queue(),
      if (!identical(shiny::isolate(input$scans_app_view), "application")) {
        shiny::isolate(state$selected())
      }
    )
  })

  # Progress ignores the review-state filters: "needs review" would
  # otherwise always report nothing reviewed.
  state$review_progress <- shiny::reactive({
    current <- state$data()
    queue <- state$queue()
    if (is.null(current) || is.null(queue)) {
      return(NULL)
    }
    scope <- scans_app_facet_filter(
      current,
      state$unreviewed_scope(),
      state$facets()
    )
    cohort_review_progress(queue[queue$index %in% scope, , drop = FALSE])
  })
  output$scans_app_review_progress <- shiny::renderUI({
    progress <- state$review_progress()
    if (is.null(progress)) {
      return(NULL)
    }
    scans_app_review_progress_ui(progress, state$data()$noun)
  })

  shiny::observeEvent(input$scans_app_inspect, {
    current <- state$data()
    if (is.null(current)) {
      return()
    }
    id <- input$scans_app_inspect
    if (!is.character(id) || length(id) != 1L || is.na(id)) {
      return()
    }
    # Like a deep link, this opens a run the current filters leave out; the
    # position and the review bar then say it is outside them.
    index <- match(id, current$info$trajectory_id)
    if (is.na(index)) {
      shiny::showNotification(
        sprintf(
          "%s is not in the loaded snapshot of this application, so it cannot be opened here.",
          id
        ),
        type = "warning",
        session = session
      )
      return()
    }
    state$selected(index)
    bslib::nav_select("scans_app_view", "trajectory", session = session)
  })

  # Promotion candidates need the bundle, so they are computed once per
  # snapshot and visible cohort, and only for sources with provenance or
  # tool routes worth ranking.
  state$candidates <- shiny::reactive({
    current <- state$data()
    scans_app_candidates(
      current,
      state$active()$bundle,
      current$info$trajectory_id[state$visible()]
    )
  })
  state$performance <- shiny::reactive({
    current <- state$data()
    if (is.null(current)) {
      return(NULL)
    }
    queue <- state$queue()
    scans_app_performance_data(
      current,
      state$visible(),
      state$facet_scopes(),
      queue = queue,
      progress = state$review_progress(),
      candidates = state$candidates()
    )
  })
  output$scans_app_performance <- shiny::renderUI({
    current <- state$performance()
    if (is.null(current)) {
      return(scans_app_source_error_ui(state$application()))
    }
    scans_app_performance_ui(
      current,
      state$application(),
      input$scans_app_priority %||% "elapsed",
      state$scan_config()$scans,
      chat = !is.null(chat_factory),
      scope = list(
        facets = state$facets(),
        pattern = state$pattern_filter(),
        findings_only = isTRUE(input$scans_app_findings_only),
        review = state$review_filter(),
        export_users = isTRUE(shiny::isolate(input$scans_app_export_users))
      )
    )
  })

  output$scans_app_overview <- shiny::renderUI({
    current <- state$data()
    if (is.null(current)) {
      return(NULL)
    }
    scans_app_overview_ui(current, state$selected())
  })
  output$scans_app_header <- shiny::renderUI({
    current <- state$data()
    if (is.null(current)) {
      return(scans_app_source_error_heading(state$application()))
    }
    scans_app_header_ui(current, state$selected())
  })
  output$scans_app_resources <- shiny::renderUI({
    current <- state$data()
    if (is.null(current) || is.null(state$selected())) {
      return(NULL)
    }
    scans_app_resources_ui(current, state$selected())
  })
  output$scans_app_transcript <- shiny::renderUI({
    current <- state$data()
    if (is.null(current)) {
      return(scans_app_source_error_ui(state$application()))
    }
    scans_app_transcript_ui(current, state$selected())
  })
  output$scans_app_evidence <- shiny::renderUI({
    current <- state$data()
    if (is.null(current)) {
      return(scans_app_source_error_ui(state$application()))
    }
    scans_app_evidence_ui(current, state$selected())
  })
}
