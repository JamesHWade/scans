# Loaded snapshots are cached per app object, not per session: the cache is
# created once when the server function is built, so a second reviewer -- or
# the same reviewer after a browser refresh -- sees the snapshot instantly
# rather than paying for another Connect read. A snapshot older than
# `cache_max_age` is refreshed for active sessions as well as new ones. A
# failed load is cached for the session that saw it fail, so the failure is
# shown instead of an empty app, but is retried by any other.
scans_app_server <- function(
  sources,
  annotations = NULL,
  cache_max_age = 30 * 60,
  clock = Sys.time,
  schedule = shiny::invalidateLater,
  annotation_poll_interval = 2000
) {
  sources <- scans_app_runtime_sources(sources)
  cache <- new.env(parent = emptyenv())

  function(input, output, session) {
    seen <- new.env(parent = emptyenv())
    revision <- shiny::reactiveVal(0L)
    reload_revision <- shiny::reactiveVal(0L)
    selected <- shiny::reactiveVal(NULL)
    selected_trajectory_id <- shiny::reactiveVal(NULL)
    pattern_filter <- shiny::reactiveVal(NULL)

    shiny::observeEvent(application(), {
      pattern_filter(NULL)
    })
    shiny::observeEvent(input$scans_app_pattern, {
      pattern <- input$scans_app_pattern
      if (
        is.character(pattern) &&
          length(pattern) == 1L &&
          pattern %in% available_patterns()
      ) {
        pattern_filter(pattern)
      }
    })
    shiny::observeEvent(input$scans_app_clear_pattern, {
      pattern_filter(NULL)
    })
    output$scans_app_pattern_filter <- shiny::renderUI({
      pattern <- pattern_filter()
      if (is.null(pattern)) {
        return(NULL)
      }
      htmltools::div(
        class = "scans-app-active-pattern",
        htmltools::tags$span(paste("Finding:", gsub("_", " ", pattern))),
        shiny::actionLink("scans_app_clear_pattern", "Clear")
      )
    })

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
        # `seen` records which entry this session displayed, so a failure
        # that replaced the shared entry after this session last looked is
        # retried here rather than shown as this session's own failure.
        displayed <- identical(
          scans_app_cache_get(seen, label),
          entry$loaded_at
        )
        age <- scans_app_cache_age(entry, now = clock())
        fresh <- is.null(entry$error) && age < cache_max_age
        if ((displayed && !is.null(entry$error)) || fresh) {
          if (
            fresh &&
              cache_max_age > 0 &&
              is.function(active_source()$load)
          ) {
            schedule(max(1, cache_max_age - age) * 1000, session)
          }
          assign(label, entry$loaded_at, envir = seen)
          return(entry)
        }
      }
      entry <- scans_app_load_entry(
        active_source(),
        label,
        session,
        clock = clock
      )
      assign(label, entry, envir = cache)
      assign(label, entry$loaded_at, envir = seen)
      if (
        is.null(entry$error) &&
          cache_max_age > 0 &&
          is.function(active_source()$load)
      ) {
        schedule(cache_max_age * 1000, session)
      }
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

    available_patterns <- shiny::reactive({
      current <- data()
      intersect(current$findings$scan, scan_config()$scans)
    })
    shiny::observe({
      pattern <- pattern_filter()
      if (!is.null(pattern) && !pattern %in% available_patterns()) {
        pattern_filter(NULL)
      }
    })

    # The row index is only a view into the current snapshot. Keep its stable
    # identity separately so an automatic refresh can rematch a trajectory
    # when capped or newly sorted data shifts its row.
    shiny::observeEvent(
      selected(),
      ignoreNULL = FALSE,
      priority = 20L,
      {
        current <- shiny::isolate(data())
        index <- selected()
        trajectory_id <- if (
          !is.null(current) &&
            length(index) == 1L &&
            !is.na(index) &&
            index >= 1L &&
            index <= nrow(current$info)
        ) {
          current$info$trajectory_id[[index]]
        }
        selected_trajectory_id(trajectory_id)
      }
    )

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

    annotation_records <- if (is.null(annotations)) {
      function() NULL
    } else {
      shiny::reactivePoll(
        intervalMillis = annotation_poll_interval,
        session = session,
        checkFunc = function() {
          list(
            application = application(),
            local_revision = annotation_revision(),
            file_revision = annotations_file_revision(annotations$path)
          )
        },
        valueFunc = function() {
          tryCatch(
            annotations$read(application = application()),
            error = function(cnd) NULL
          )
        }
      )
    }

    # The latest label (or "Note") per annotated trajectory of the current
    # application, so the browser can mark and filter reviewed entries.
    annotation_labels <- shiny::reactive({
      if (is.null(annotations)) {
        return(character())
      }
      scans_app_annotation_labels(annotation_records())
    })

    annotated <- shiny::reactive({
      current <- data()
      if (is.null(current)) {
        return(logical())
      }
      current$info$trajectory_id %in% names(annotation_labels())
    })

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
            annotation_status(scans_app_annotation_error(cnd))
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
      records <- annotation_records()
      if (is.null(records)) {
        return(scans_app_empty_ui("Could not read the annotation store."))
      }
      records <- records[
        records$trajectory_id %in% target$trajectory_id,
        ,
        drop = FALSE
      ]
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
        reload_revision(reload_revision() + 1L)
        revision(revision() + 1L)
      }
    )

    # Only a newly selected or reloaded application resets the browser. Scan
    # settings merely derive new findings from the same cached bundle, and the
    # cache timer re-evaluates `active()` without changing the snapshot; neither
    # may move the reviewer away from the trajectory under inspection.
    reset_key <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      active(),
      ignoreNULL = TRUE,
      priority = 10L,
      {
        key <- list(application(), reload_revision())
        if (identical(key, reset_key())) {
          current <- data()
          trajectory_id <- selected_trajectory_id()
          if (!is.null(current) && !is.null(trajectory_id)) {
            index <- match(trajectory_id, current$info$trajectory_id)
            selected(if (is.na(index)) NULL else index)
          }
          return()
        }
        reset_key(key)
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
        findings_only = isTRUE(input$scans_app_findings_only),
        annotated = annotated(),
        annotated_only = isTRUE(input$scans_app_annotated_only)
      )
      pattern <- pattern_filter()
      if (!is.null(pattern)) {
        matching <- current$findings$trajectory_id[
          current$findings$scan %in% pattern
        ]
        indices <- indices[current$records$trajectory_id[indices] %in% matching]
      }
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

    # The selected entry is highlighted in the browser rather than by
    # re-rendering the whole list: with a hundred entries the re-render was
    # the slowest part of clicking.
    shiny::observe({
      index <- selected()
      showing_trajectory <- !identical(input$scans_app_view, "application")
      current <- shiny::isolate(data())
      trajectory_id <- if (!is.null(index) && !is.null(current)) {
        current$info$trajectory_id[[index]]
      }
      session$sendCustomMessage(
        "scans-app-select",
        list(
          id = if (is.null(index)) NULL else scans_app_entry_id(index),
          hash = scans_app_hash(
            if (is.null(index) || !showing_trajectory) {
              NULL
            } else {
              shiny::isolate(application())
            },
            if (showing_trajectory) trajectory_id else NULL
          )
        )
      )
    })

    # A URL hash of "<application>/<trajectory_id>" deep-links to one
    # trajectory. The application is switched first; the selection is applied
    # once that application's data is available.
    pending_hash <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$scans_app_hash, {
      target <- scans_app_parse_hash(input$scans_app_hash, sources$labels)
      if (is.null(target)) {
        return()
      }
      pending_hash(target)
      if (!identical(target$application, application())) {
        shiny::updateSelectInput(
          session,
          "scans_app_application",
          selected = target$application
        )
      }
    })
    shiny::observe({
      target <- pending_hash()
      current <- data()
      if (is.null(target) || is.null(current)) {
        return()
      }
      if (!identical(target$application, application())) {
        return()
      }
      index <- match(target$trajectory_id, current$info$trajectory_id)
      if (!is.na(index)) {
        selected(index)
        bslib::nav_select("scans_app_view", "trajectory", session = session)
      }
      pending_hash(NULL)
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
            bslib::nav_select("scans_app_view", "trajectory", session = session)
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
      labels <- annotation_labels()
      htmltools::tagList(lapply(indices, function(index) {
        scans_app_entry_ui(
          current$records[index, , drop = FALSE],
          selected = identical(chosen, index),
          annotation = labels[current$info$trajectory_id[[index]]]
        )
      }))
    })

    shiny::observeEvent(input$scans_app_inspect, {
      current <- data()
      if (is.null(current)) {
        return()
      }
      id <- input$scans_app_inspect
      if (!is.character(id) || length(id) != 1L || is.na(id)) {
        return()
      }
      index <- match(id, current$info$trajectory_id)
      if (!is.na(index) && index %in% visible()) {
        selected(index)
        bslib::nav_select("scans_app_view", "trajectory", session = session)
      }
    })

    performance <- shiny::reactive({
      current <- data()
      if (is.null(current)) {
        return(NULL)
      }
      scans_app_performance_data(current, visible())
    })
    output$scans_app_performance <- shiny::renderUI({
      current <- performance()
      if (is.null(current)) {
        return(scans_app_source_error_ui(application()))
      }
      scans_app_performance_ui(
        current,
        application(),
        input$scans_app_priority %||% "elapsed",
        scan_config()$scans
      )
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

scans_app_cache_age <- function(entry, now = Sys.time()) {
  if (is.null(entry$loaded_at)) {
    return(Inf)
  }
  as.numeric(now - entry$loaded_at, units = "secs")
}

# One cache entry: the bundle (or already-derived tables), when it was
# loaded, and what the reader said about the read. The load runs under a
# progress notification so a Connect read that takes a while is visibly
# running rather than apparently hung; the reader reports pages through the
# `scans.progress` option.
scans_app_load_entry <- function(
  source,
  label,
  session = NULL,
  clock = Sys.time
) {
  loaded_at <- clock()
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
      read_info = NULL
    ))
  }
  load <- function() {
    tryCatch(
      {
        loaded <- scans_app_load_source(source)
        list(
          bundle = loaded$bundle,
          data = NULL,
          error = NULL,
          loaded_at = clock(),
          read_info = loaded$read_info
        )
      },
      error = function(cnd) {
        scans_app_log_source_error(label, cnd)
        list(
          bundle = NULL,
          data = NULL,
          error = scans_app_safe_source_error(),
          loaded_at = clock(),
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

# Validation failures from the store name what the reviewer must change and
# are shown as written. Anything else (an unwritable path, a full disk) is
# logged on the server and replaced, so the browser never learns the store's
# location.
scans_app_annotation_error <- function(cnd) {
  if (inherits(cnd, "scans_error_annotation_record")) {
    return(conditionMessage(cnd))
  }
  cli::cli_inform(c(
    "!" = "Failed to save an annotation.",
    "i" = "{conditionMessage(cnd)}"
  ))
  "The annotation could not be saved. Check the server logs for details."
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

# Latest annotation label per trajectory; a note-only record shows as "Note".
scans_app_annotation_labels <- function(records) {
  if (is.null(records) || nrow(records) == 0L) {
    return(character())
  }
  records <- records[
    order(records$created_at, decreasing = TRUE),
    ,
    drop = FALSE
  ]
  latest <- records[!duplicated(records$trajectory_id), , drop = FALSE]
  label <- latest$label
  label[is.na(label) | !nzchar(label)] <- "Note"
  stats::setNames(label, latest$trajectory_id)
}

scans_app_hash <- function(application, trajectory_id) {
  if (is.null(application) || is.null(trajectory_id)) {
    return("")
  }
  paste(
    utils::URLencode(application, reserved = TRUE),
    utils::URLencode(trajectory_id, reserved = TRUE),
    sep = "/"
  )
}

scans_app_parse_hash <- function(hash, labels) {
  if (!scans_app_has_string(hash)) {
    return(NULL)
  }
  hash <- sub("^#", "", hash)
  parts <- strsplit(hash, "/", fixed = TRUE)[[1L]]
  if (length(parts) < 2L) {
    return(NULL)
  }
  application <- utils::URLdecode(parts[[1L]])
  trajectory_id <- utils::URLdecode(paste(parts[-1L], collapse = "/"))
  if (!application %in% labels || !nzchar(trajectory_id)) {
    return(NULL)
  }
  list(application = application, trajectory_id = trajectory_id)
}
