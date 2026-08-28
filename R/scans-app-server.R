scans_app_server <- function(sources, annotations = NULL) {
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

    # A failed load is kept, not discarded. Swallowing it renders an empty app
    # with nothing in the log, which reads as "this application has no
    # conversations" when the truth is that reading them failed.
    active <- shiny::reactive({
      revision()
      label <- application()
      if (exists(label, envir = cache, inherits = FALSE)) {
        return(get(label, envir = cache, inherits = FALSE))
      }
      source <- active_source()
      result <- if (!is.null(source$data)) {
        # Already-derived tables, as supplied internally; there is no bundle
        # to re-scan, so the scan selection does not apply to this source.
        list(bundle = NULL, data = source$data, error = NULL)
      } else {
        tryCatch(
          list(
            bundle = scans_app_load_source(source),
            data = NULL,
            error = NULL
          ),
          error = function(cnd) {
            list(bundle = NULL, data = NULL, error = conditionMessage(cnd))
          }
        )
      }
      assign(label, result, envir = cache)
      result
    })

    scan_config <- shiny::reactive({
      scans_app_scan_config(
        scans = input$scans_app_scans %||% character(),
        repeat_threshold = scans_app_threshold(
          input$scans_app_repeat_threshold,
          2L
        ),
        loop_threshold = scans_app_threshold(
          input$scans_app_loop_threshold,
          3L
        )
      )
    })

    # Findings are derived from the cached bundle, so changing the scan
    # selection re-scans in memory and never refetches traces.
    data <- shiny::reactive({
      current <- active()
      if (!is.null(current$data)) {
        return(current$data)
      }
      if (is.null(current$bundle)) {
        return(NULL)
      }
      scans_app_data(current$bundle, scan_config())
    })

    output$scans_app_load_error <- shiny::renderUI({
      message <- active()$error
      if (is.null(message)) {
        return(NULL)
      }
      scans_app_source_error_ui(application(), message)
    })

    annotation_revision <- shiny::reactiveVal(0L)
    annotation_status <- shiny::reactiveVal("")

    selected_annotation_target <- shiny::reactive({
      current <- data()
      index <- selected()
      if (is.null(current) || is.null(index)) {
        return(NULL)
      }
      list(
        application = application(),
        trajectory_id = current$info$trajectory_id[[index]]
      )
    })

    # A draft judgement belongs to the application and trajectory it was typed
    # for. Clear the form only when that pair changes; an in-memory re-scan
    # invalidates `data()` but leaves the target unchanged.
    annotation_target <- shiny::reactiveVal(NULL)
    shiny::observe({
      target <- selected_annotation_target()
      previous <- shiny::isolate(annotation_target())
      if (identical(target, previous)) {
        return()
      }
      annotation_target(target)
      annotation_status("")
      shiny::updateSelectInput(
        session,
        "scans_app_annotation_label",
        selected = ""
      )
      shiny::updateTextAreaInput(
        session,
        "scans_app_annotation_note",
        value = ""
      )
    })

    shiny::observeEvent(
      input$scans_app_annotation_save,
      ignoreInit = TRUE,
      {
        if (is.null(annotations)) {
          return()
        }
        target <- selected_annotation_target()
        if (is.null(target)) {
          annotation_status("Select a trajectory first.")
          return()
        }
        written <- tryCatch(
          {
            annotations$append(
              application = target$application,
              trajectory_id = target$trajectory_id,
              label = input$scans_app_annotation_label,
              note = input$scans_app_annotation_note,
              author = annotations_default_author(session)
            )
            TRUE
          },
          error = function(cnd) {
            annotation_status(conditionMessage(cnd))
            FALSE
          }
        )
        if (!isTRUE(written)) {
          return()
        }
        shiny::updateTextAreaInput(
          session,
          "scans_app_annotation_note",
          value = ""
        )
        annotation_status("Saved.")
        annotation_revision(annotation_revision() + 1L)
      }
    )

    output$scans_app_annotation_status <- shiny::renderText(annotation_status())

    output$scans_app_annotation_log <- shiny::renderUI({
      if (is.null(annotations)) {
        return(NULL)
      }
      annotation_revision()
      target <- selected_annotation_target()
      if (is.null(target)) {
        return(scans_app_empty_ui("Select a trajectory to annotate it."))
      }
      records <- tryCatch(
        annotations$read(
          application = target$application,
          trajectory_id = target$trajectory_id
        ),
        error = function(cnd) NULL
      )
      if (is.null(records)) {
        return(scans_app_empty_ui("Could not read the annotation store."))
      }
      scans_app_annotation_log_ui(records)
    })

    output$scans_app_scan_summary <- shiny::renderText({
      selected <- length(scan_config()$scans)
      total <- nrow(scan_registry())
      current <- data()
      if (selected == 0L) {
        return("No scans selected \u00b7 findings are hidden")
      }
      findings <- if (is.null(current)) 0L else nrow(current$findings)
      sprintf(
        "%d of %d scans \u00b7 %d finding%s",
        selected,
        total,
        findings,
        if (findings == 1L) "" else "s"
      )
    })
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

    # Only a newly selected or reloaded application resets the browser. Scan
    # settings merely derive new findings from the same cached bundle and must
    # not move the reviewer away from the trajectory under inspection.
    shiny::observeEvent(
      active(),
      ignoreNULL = TRUE,
      {
        current <- data()
        if (is.null(current)) {
          selected(NULL)
          return()
        }
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


# A threshold the user has cleared or typed nonsense into falls back to the
# default rather than erroring: the panel should not be able to break the
# view it controls.
scans_app_threshold <- function(value, default) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= 2 &&
    value <= .Machine$integer.max
  if (!valid) {
    return(default)
  }
  as.integer(value)
}
