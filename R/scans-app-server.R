# Loaded snapshots are cached per app object, not per session: the cache is
# created once when the server function is built, so a second reviewer -- or
# the same reviewer after a browser refresh -- sees the snapshot instantly
# rather than paying for another Connect read. A snapshot older than
# `cache_max_age` is refreshed the next time a session that has not yet
# displayed it asks for it; a session keeps what it is looking at until it
# reloads. A failed load is cached for the session that saw it fail, so the
# failure is shown instead of an empty app, but is retried by any other.
scans_app_server <- function(
  sources,
  annotations = NULL,
  cache_max_age = 30 * 60
) {
  sources <- scans_app_runtime_sources(sources)
  cache <- new.env(parent = emptyenv())

  function(input, output, session) {
    seen <- new.env(parent = emptyenv())
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
      entry <- scans_app_cache_get(cache, label)
      if (!is.null(entry)) {
        displayed <- exists(label, envir = seen, inherits = FALSE)
        fresh <- is.null(entry$error) &&
          scans_app_cache_age(entry) < cache_max_age
        if (displayed || fresh) {
          assign(label, TRUE, envir = seen)
          return(entry)
        }
      }
      entry <- scans_app_load_entry(active_source(), label, session)
      assign(label, entry, envir = cache)
      assign(label, TRUE, envir = seen)
      entry
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

    output$scans_app_load_info <- shiny::renderUI({
      shiny::invalidateLater(60 * 1000)
      current <- active()
      if (!is.null(current$error)) {
        return(NULL)
      }
      scans_app_load_info_ui(current, is.function(active_source()$load))
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
        for (store in list(cache, seen)) {
          if (exists(label, envir = store, inherits = FALSE)) {
            rm(list = label, envir = store)
          }
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
      priority = 10L,
      {
        current <- data()
        if (is.null(current)) {
          selected(NULL)
          return()
        }
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
        selected(NULL)
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
      indices <- scans_app_filter_records(
        current$records,
        source = source,
        status = status,
        query = scans_app_input_or(input$scans_app_query, ""),
        findings_only = isTRUE(input$scans_app_findings_only)
      )
      scans_app_order_records(
        current$records,
        indices,
        scans_app_input_or(input$scans_app_sort, "newest")
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

    # Stepping through the visible list, from the header buttons or the
    # keyboard. The list order is the sort order, so "next" means the next
    # entry the reviewer can see.
    move_selection <- function(step) {
      indices <- visible()
      if (length(indices) == 0L) {
        return()
      }
      position <- match(selected(), indices)
      if (is.na(position)) {
        selected(indices[[1L]])
        return()
      }
      target <- position + step
      if (target >= 1L && target <= length(indices)) {
        selected(indices[[target]])
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
    shiny::observeEvent(input$scans_app_select, ignoreInit = TRUE, {
      index <- suppressWarnings(as.integer(input$scans_app_select$index))
      current <- data()
      if (
        !is.null(current) &&
          length(index) == 1L &&
          !is.na(index) &&
          index >= 1L &&
          index <= nrow(current$records)
      ) {
        selected(index)
      }
    })

    # The selected entry is highlighted in the browser rather than by
    # re-rendering the whole list: with a hundred entries the re-render was
    # the slowest part of clicking.
    shiny::observe({
      index <- selected()
      session$sendCustomMessage(
        "scans-app-select",
        list(id = if (is.null(index)) NULL else scans_app_entry_id(index))
      )
    })

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

    output$scans_app_position <- shiny::renderText({
      indices <- visible()
      position <- match(selected(), indices)
      if (length(indices) == 0L || is.na(position)) {
        return("")
      }
      sprintf("%d of %d", position, length(indices))
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
      chosen <- shiny::isolate(selected())
      htmltools::tagList(lapply(indices, function(index) {
        scans_app_entry_ui(
          current$records[index, , drop = FALSE],
          selected = identical(chosen, index)
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

scans_app_cache_get <- function(cache, label) {
  if (exists(label, envir = cache, inherits = FALSE)) {
    get(label, envir = cache, inherits = FALSE)
  } else {
    NULL
  }
}

scans_app_cache_age <- function(entry) {
  if (is.null(entry$loaded_at)) {
    return(Inf)
  }
  as.numeric(Sys.time() - entry$loaded_at, units = "secs")
}

# One cache entry: the bundle (or already-derived tables), when it was
# loaded, and what the reader said about the read. The load runs under a
# progress notification so a Connect read that takes a while is visibly
# running rather than apparently hung; the reader reports pages through the
# `scans.progress` option.
scans_app_load_entry <- function(source, label, session = NULL) {
  loaded_at <- Sys.time()
  if (!is.null(source$data)) {
    return(list(
      bundle = NULL,
      data = source$data,
      error = NULL,
      loaded_at = loaded_at,
      read_info = NULL
    ))
  }
  if (!is.null(source$bundle)) {
    return(list(
      bundle = source$bundle,
      data = NULL,
      error = NULL,
      loaded_at = loaded_at,
      read_info = attr(source$bundle, "scans_read_info", exact = TRUE)
    ))
  }
  load <- function() {
    tryCatch(
      {
        bundle <- scans_app_load_source(source)
        list(
          bundle = bundle,
          data = NULL,
          error = NULL,
          loaded_at = Sys.time(),
          read_info = attr(bundle, "scans_read_info", exact = TRUE)
        )
      },
      error = function(cnd) {
        scans_app_log_source_error(label, cnd)
        list(
          bundle = NULL,
          data = NULL,
          error = scans_app_safe_source_error(),
          loaded_at = Sys.time(),
          read_info = NULL
        )
      }
    )
  }
  if (is.null(session)) {
    return(load())
  }
  shiny::withProgress(
    session = session,
    message = paste("Reading traces for", label),
    detail = "Connecting",
    value = NULL,
    {
      old <- options(
        scans.progress = function(detail) {
          shiny::setProgress(detail = detail, session = session)
        }
      )
      on.exit(options(old), add = TRUE)
      load()
    }
  )
}

scans_app_safe_source_error <- function() {
  "The trace source could not be read. Check the server logs for details."
}

scans_app_log_source_error <- function(label, cnd) {
  details <- conditionMessage(cnd)
  cli::cli_inform(c(
    "!" = "Failed to load traces for {.val {label}}.",
    "i" = "{details}"
  ))
  invisible(NULL)
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
