scans_app_filter_server <- function(
  input,
  output,
  session,
  state
) {
  shiny::observeEvent(state$application(), {
    if (is.null(state$active_source()$investigation)) {
      state$pattern_filter(NULL)
    }
    state$facets(list())
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

  shiny::observeEvent(input$scans_app_facet, {
    request <- input$scans_app_facet
    current <- state$data()
    facet <- request$facet
    if (is.null(current) || !scans_app_has_string(facet)) {
      return()
    }
    if (identical(facet, "findings")) {
      bslib::update_switch(
        "scans_app_findings_only",
        value = !isTRUE(input$scans_app_findings_only),
        session = session
      )
      return()
    }
    if (facet %in% scans_app_facet_names) {
      state$facets(scans_app_facet_validate(
        current,
        scans_app_facet_toggle(state$facets(), facet, request$value)
      ))
    }
  })
  shiny::observeEvent(input$scans_app_facet_clear, {
    facet <- input$scans_app_facet_clear$facet
    all <- identical(facet, "all")
    facets <- state$facets()
    if (all) {
      facets <- list()
    } else if (identical(facet, "tool")) {
      facets[c("tool", "tool_failed")] <- NULL
    } else if (scans_app_has_string(facet)) {
      facets[[facet]] <- NULL
    }
    state$facets(scans_app_facet_list(facets))
    if (all || identical(facet, "pattern")) {
      state$pattern_filter(NULL)
    }
    if (all || identical(facet, "review")) {
      shiny::updateSelectInput(
        session,
        "scans_app_review_state",
        selected = "all"
      )
    }
    if (all || identical(facet, "review_label")) {
      shiny::updateSelectInput(session, "scans_app_review_label", selected = "")
    }
    if (
      isTRUE(input$scans_app_findings_only) &&
        (all || identical(facet, "findings"))
    ) {
      bslib::update_switch(
        "scans_app_findings_only",
        value = FALSE,
        session = session
      )
    }
  })
  shiny::observe({
    current <- state$data()
    facets <- state$facets()
    if (is.null(current) || !length(facets)) {
      return()
    }
    valid <- scans_app_facet_validate(current, facets)
    if (!identical(valid, facets)) {
      state$facets(valid)
    }
  })
  # Review-state filters exist only with an annotation store.
  state$review_filter <- shiny::reactive({
    if (!state$has_store) {
      return(list())
    }
    review <- list()
    if (isTRUE(input$scans_app_review_state %in% c("needs", "reviewed"))) {
      review$state <- input$scans_app_review_state
    }
    if (scans_app_has_string(input$scans_app_review_label)) {
      review$label <- input$scans_app_review_label
    }
    review
  })
  state$scope_labels <- shiny::reactive({
    scans_app_scope_labels(
      state$facets(),
      state$pattern_filter(),
      input$scans_app_findings_only,
      state$review_filter()
    )
  })
  output$scans_app_pattern_filter <- shiny::renderUI({
    scans_app_scope_chips_ui(state$scope_labels())
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
        state$facets(list())
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

  # The toolbar filters and pattern in the form a saved view records them.
  # The browser list, exports, and links all read this one value.
  state$view <- shiny::reactive({
    current <- state$data()
    shiny::req(current)
    scans_app_input_view(
      current,
      source = input$scans_app_source,
      status = input$scans_app_status,
      query = scans_app_input_or(input$scans_app_query, ""),
      findings_only = input$scans_app_findings_only,
      annotated_only = state$annotated_only(),
      pattern = state$pattern_filter()
    )
  })

  # Everything except the facets. The overview also shows the cohort one
  # facet wider than the browser list, and later filters build on this.
  state$unreviewed_scope <- shiny::reactive({
    current <- state$data()
    if (is.null(current)) {
      return(integer())
    }
    scans_app_view_indices(current, state$view(), state$annotated())
  })
  state$filtered <- shiny::reactive({
    scans_app_review_indices(
      state$unreviewed_scope(),
      state$review_filter(),
      state$queue()
    )
  })
  state$visible <- shiny::reactive({
    current <- state$data()
    if (is.null(current)) {
      return(integer())
    }
    indices <- scans_app_facet_filter(
      current,
      state$filtered(),
      state$facets()
    )
    sort <- scans_app_input_or(input$scans_app_sort, state$default_sort)
    if (identical(sort, "review")) {
      queue <- state$queue()
      return(queue$index[queue$index %in% indices])
    }
    scans_app_order_records(current$records, indices, sort)
  })
  # For each active facet, the cohort without it.
  state$facet_scopes <- shiny::reactive({
    current <- state$data()
    facets <- state$facets()
    if (is.null(current) || !length(facets)) {
      return(list())
    }
    scopes <- list()
    for (facet in setdiff(names(facets), "tool_failed")) {
      wider <- facets
      wider[[facet]] <- NULL
      if (facet == "tool") {
        wider$tool_failed <- NULL
      }
      scopes[[facet]] <- scans_app_facet_filter(
        current,
        state$filtered(),
        wider
      )
    }
    scopes
  })

  # A run the filters drop is replaced by the first one they keep. A run
  # opened from outside them, by a link, was never in the list and stays open
  # when the list is recomputed, as it is after every saved judgement.
  listed <- new.env(parent = emptyenv())
  shiny::observeEvent(
    state$visible(),
    ignoreNULL = FALSE,
    {
      indices <- state$visible()
      ids <- state$data()$info$trajectory_id
      chosen <- state$selected()
      outside <- !is.null(chosen) &&
        !chosen %in% indices &&
        !isTRUE(ids[chosen] %in% listed$ids)
      listed$ids <- ids[indices]
      if (is.null(chosen) || (!chosen %in% indices && !outside)) {
        state$selected(if (length(indices)) indices[[1L]])
      }
    }
  )
}
