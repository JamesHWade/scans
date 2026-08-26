scans_app_server <- function(sources) {
  sources <- scans_app_runtime_sources(sources)

  function(input, output, session) {
    cache <- new.env(parent = emptyenv())
    revision <- shiny::reactiveVal(0L)
    selected <- shiny::reactiveVal(NULL)

    application <- shiny::reactive({
      label <- scans_app_input_or(
        input$scans_app_application,
        sources$labels[[1L]]
      )
      if (!label %in% sources$labels) {
        return(sources$labels[[1L]])
      }
      label
    })

    active_source <- shiny::reactive({
      sources$sources[[match(application(), sources$labels)]]
    })

    output$scans_app_reload_control <- shiny::renderUI({
      if (!is.function(active_source()$load)) {
        return(NULL)
      }
      scans_app_reload_button()
    })

    active <- shiny::reactive({
      revision()
      label <- application()
      if (exists(label, envir = cache, inherits = FALSE)) {
        return(get(label, envir = cache, inherits = FALSE))
      }
      source <- active_source()
      result <- list(
        data = tryCatch(
          scans_app_load_source(source),
          error = \(...) NULL
        )
      )
      assign(label, result, envir = cache)
      result
    })

    data <- shiny::reactive(active()$data)
    shiny::observeEvent(
      input$scans_app_reload,
      ignoreInit = TRUE,
      {
        if (!is.function(active_source()$load)) {
          return()
        }
        label <- application()
        if (exists(label, envir = cache, inherits = FALSE)) {
          rm(list = label, envir = cache)
        }
        revision(revision() + 1L)
      }
    )

    shiny::observeEvent(
      data(),
      ignoreNULL = TRUE,
      {
        current <- data()
        choices <- scans_app_filter_choices(current)
        shiny::updateSelectInput(
          session,
          "scans_app_source",
          choices = choices$source,
          selected = choices$source_all
        )
        shiny::updateSelectInput(
          session,
          "scans_app_status",
          choices = choices$status,
          selected = choices$status_all
        )
        selected(if (nrow(current$records) == 0L) NULL else 1L)
      }
    )

    visible <- shiny::reactive({
      current <- data()
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
      scans_app_filter_records(
        current$records,
        source = source,
        status = status,
        query = scans_app_input_or(input$scans_app_query, ""),
        findings_only = isTRUE(input$scans_app_findings_only)
      )
    })

    shiny::observeEvent(
      visible(),
      ignoreNULL = FALSE,
      {
        indices <- visible()
        if (length(indices) == 0L) {
          selected(NULL)
        } else if (is.null(selected()) || !selected() %in% indices) {
          selected(indices[[1L]])
        }
      }
    )

    entry_observers <- new.env(parent = emptyenv())
    shiny::observe({
      current <- data()
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
            selected(entry)
          })
          assign(id, observer, envir = entry_observers)
        })
      }
    })

    output$scans_app_visible_count <- shiny::renderText({
      current <- data()
      if (is.null(current)) {
        return("Traces unavailable")
      }
      count <- length(visible())
      total <- nrow(current$records)
      sprintf(
        "%d of %d %s",
        count,
        total,
        if (total == 1L) "trajectory" else "trajectories"
      )
    })

    output$scans_app_entries <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_ui(application()))
      }
      indices <- visible()
      if (length(indices) == 0L) {
        return(scans_app_empty_ui(
          if (nrow(current$records) == 0L) {
            "This bundle has no trajectories."
          } else {
            "No trajectories match these filters."
          }
        ))
      }
      htmltools::tagList(lapply(indices, function(index) {
        scans_app_entry_ui(
          current$records[index, , drop = FALSE],
          selected = identical(selected(), index)
        )
      }))
    })

    output$scans_app_overview <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(NULL)
      }
      scans_app_overview_ui(current, selected())
    })
    output$scans_app_header <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_heading(application()))
      }
      scans_app_header_ui(current, selected())
    })
    output$scans_app_transcript <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_ui(application()))
      }
      scans_app_transcript_ui(current, selected())
    })
    output$scans_app_evidence <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_ui(application()))
      }
      scans_app_evidence_ui(current, selected())
    })
  }
}
