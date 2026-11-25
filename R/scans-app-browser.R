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

  entry_observers <- new.env(parent = emptyenv())
  shiny::observe({
    current <- state$data()
    if (is.null(current) || nrow(current$records) == 0L) {
      return()
    }
    for (index in seq_len(nrow(current$records))) {
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
    if (length(indices) == 0L || is.na(position)) {
      return("")
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
    chosen <- shiny::isolate(state$selected())
    queue <- state$queue()
    rows <- match(indices, queue$index)
    htmltools::tagList(lapply(seq_along(indices), function(i) {
      index <- indices[[i]]
      scans_app_entry_ui(
        current$records[index, , drop = FALSE],
        selected = identical(chosen, index) &&
          !identical(shiny::isolate(input$scans_app_view), "application"),
        queue = queue[rows[[i]], , drop = FALSE]
      )
    }))
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
    index <- match(id, current$info$trajectory_id)
    if (!is.na(index) && index %in% state$visible()) {
      state$selected(index)
      bslib::nav_select("scans_app_view", "trajectory", session = session)
    }
  })

  # Promotion candidates need the bundle, so they are computed once per
  # snapshot and visible cohort, and only for sources with provenance or
  # tool routes worth ranking.
  state$candidates <- shiny::reactive({
    current <- state$data()
    bundle <- state$active()$bundle
    if (is.null(current) || is.null(bundle) || is.null(current$exchanges)) {
      return(NULL)
    }
    route_candidates(
      bundle,
      trajectory_ids = current$info$trajectory_id[state$visible()],
      exchanges = current$exchanges,
      measures = NULL
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
        review = state$review_filter()
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
