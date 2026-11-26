scans_app_filter_server <- function(
  input,
  output,
  session,
  state
) {
  shiny::observeEvent(state$application(), {
    if (is.null(state$active_source()$investigation)) state$pattern_filter(NULL)
  })
  shiny::observeEvent(input$scans_app_pattern, {
    pattern <- input$scans_app_pattern
    if (
      is.character(pattern) &&
        length(pattern) == 1L &&
        pattern %in% available_patterns()
    ) {
      state$pattern_filter(pattern)
    }
  })
  shiny::observeEvent(input$scans_app_clear_pattern, {
    state$pattern_filter(NULL)
  })
  output$scans_app_pattern_filter <- shiny::renderUI({
    pattern <- state$pattern_filter()
    if (is.null(pattern)) {
      return(NULL)
    }
    htmltools::div(
      class = "scans-app-active-pattern",
      htmltools::tags$span(paste("Finding:", gsub("_", " ", pattern))),
      shiny::actionLink("scans_app_clear_pattern", "Clear")
    )
  })

  available_patterns <- shiny::reactive({
    current <- state$data()
    intersect(current$findings$scan, state$scan_config()$scans)
  })
  shiny::observe({
    pattern <- state$pattern_filter()
    if (!is.null(pattern) && !pattern %in% available_patterns()) {
      state$pattern_filter(NULL)
    }
  })

  # The row index is only a view into the current snapshot. Keep its stable
  # identity separately so an automatic refresh can rematch a trajectory
  # when capped or newly sorted data shifts its row.
  shiny::observeEvent(
    state$selected(),
    ignoreNULL = FALSE,
    priority = 20L,
    {
      current <- shiny::isolate(state$data())
      index <- state$selected()
      trajectory_id <- if (
        !is.null(current) &&
          length(index) == 1L &&
          !is.na(index) &&
          index >= 1L &&
          index <= nrow(current$info)
      ) {
        current$info$trajectory_id[[index]]
      }
      state$selected_trajectory_id(trajectory_id)
    }
  )

  output$scans_app_scan_summary <- shiny::renderText({
    selected <- length(state$scan_config()$scans)
    total <- nrow(scan_registry())
    current <- state$data()
    if (selected == 0L) {
      return("No scans selected \u00b7 findings are hidden")
    }
    findings <- if (is.null(current)) 0L else nrow(current$findings)
    if (
      !is.null(state$active()$investigation) && !state$rescan_investigation()
    ) {
      return(sprintf(
        "%d saved scan%s \u00b7 %d finding%s",
        selected,
        if (selected == 1L) "" else "s",
        findings,
        if (findings == 1L) "" else "s"
      ))
    }
    sprintf(
      "%d of %d scans \u00b7 %d finding%s",
      selected,
      total,
      findings,
      if (findings == 1L) "" else "s"
    )
  })

  # Only a newly selected or reloaded application resets the browser. Scan
  # settings merely derive new findings from the same cached bundle, and the
  # cache timer re-evaluates `state$active()` without changing the snapshot; neither
  # may move the reviewer away from the trajectory under inspection.
  reset_key <- shiny::reactiveVal(NULL)
  shiny::observeEvent(
    state$active(),
    ignoreNULL = TRUE,
    priority = 10L,
    {
      saved <- state$active()$investigation
      key <- list(
        state$application(),
        state$reload_revision(),
        saved$manifest$revision_id
      )
      if (identical(key, reset_key())) {
        current <- state$data()
        trajectory_id <- state$selected_trajectory_id()
        if (!is.null(current) && !is.null(trajectory_id)) {
          index <- match(trajectory_id, current$info$trajectory_id)
          state$selected(if (is.na(index)) NULL else index)
        }
        return()
      }
      reset_key(key)
      state$rescan_investigation(FALSE)
      current <- state$data()
      if (is.null(current)) {
        state$selected(NULL)
        return()
      }
      if (!is.null(saved)) {
        scans_app_restore_investigation(saved, current, session)
        state$pattern_filter(saved$view$pattern)
        focus <- saved$view$selected_trajectory_id
        state$selected(
          if (is.null(focus)) {
            NULL
          } else {
            match(focus, current$info$trajectory_id)
          }
        )
      } else {
        choices <- scans_app_filter_choices(current)
        bslib::update_toolbar_input_select(
          "scans_app_source",
          choices = choices$source,
          selected = choices$source_all,
          session = session
        )
        bslib::update_toolbar_input_select(
          "scans_app_status",
          choices = choices$status,
          selected = choices$status_all,
          session = session
        )
        state$selected(NULL)
      }
    }
  )

  state$visible <- shiny::reactive({
    current <- state$data()
    if (is.null(current)) {
      return(integer())
    }
    choices <- scans_app_filter_choices(current)
    source <- scans_app_input_or(
      input$scans_app_source,
      choices$source_all
    )
    if (!source %in% unname(choices$source)) {
      source <- choices$source_all
    }
    status <- scans_app_input_or(
      input$scans_app_status,
      choices$status_all
    )
    if (!status %in% unname(choices$status)) {
      status <- choices$status_all
    }
    indices <- scans_app_filter_records(
      current$records,
      source = source,
      status = status,
      query = scans_app_input_or(input$scans_app_query, ""),
      findings_only = isTRUE(input$scans_app_findings_only),
      annotated = state$annotated(),
      annotated_only = state$annotated_only()
    )
    pattern <- state$pattern_filter()
    if (!is.null(pattern)) {
      matching <- current$findings$trajectory_id[
        current$findings$scan %in% pattern
      ]
      indices <- indices[current$records$trajectory_id[indices] %in% matching]
    }
    scans_app_order_records(
      current$records,
      indices,
      scans_app_input_or(input$scans_app_sort, "findings")
    )
  })

  shiny::observeEvent(
    state$visible(),
    ignoreNULL = FALSE,
    {
      indices <- state$visible()
      if (length(indices) == 0L) {
        state$selected(NULL)
      } else if (is.null(state$selected()) || !state$selected() %in% indices) {
        state$selected(indices[[1L]])
      }
    }
  )
}
