scans_app_loading_server <- function(
  input,
  output,
  session,
  state,
  sources,
  cache,
  cache_max_age,
  clock,
  schedule
) {
  seen <- new.env(parent = emptyenv())

  state$application <- shiny::reactive({
    opened <- state$opened_investigation()
    if (!is.null(opened)) {
      return(opened$manifest$application)
    }
    label <- scans_app_input_or(
      input$scans_app_application,
      sources$labels[[1L]]
    )
    if (!label %in% sources$labels) {
      return(sources$labels[[1L]])
    }
    label
  })

  # A comparison is a fixed pair of saved analyses: no side is replaced or
  # reanalysed, and a snapshot of a side is shown by switching to that side.
  state$fixed_sides <- !is.null(sources$comparison)
  state$comparison_side <- function(snapshot) {
    if (!state$fixed_sides) {
      return(NULL)
    }
    revisions <- vapply(
      sources$sources,
      \(source) source$investigation$manifest$revision_id,
      ""
    )
    side <- match(snapshot$manifest$revision_id, revisions)
    if (is.na(side)) NULL else sources$labels[[side]]
  }

  # The application identity annotations and saved snapshots key on. It is
  # the label unless a source records its own, as a comparison side does.
  state$application_id <- shiny::reactive({
    state$active_source()$application %||% state$application()
  })

  state$active_source <- shiny::reactive({
    opened <- state$opened_investigation()
    if (!is.null(opened)) {
      return(scans_app_source(opened$manifest$application, opened))
    }
    sources$sources[[match(state$application(), sources$labels)]]
  })

  output$scans_app_reload_control <- shiny::renderUI({
    if (!is.function(state$active_source()$load)) {
      return(NULL)
    }
    scans_app_reload_button()
  })

  state$active <- shiny::reactive({
    state$revision()
    opened <- state$opened_investigation()
    if (!is.null(opened)) {
      return(scans_app_load_entry(
        state$active_source(),
        state$application(),
        clock = clock
      ))
    }
    label <- state$application()
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
            is.function(state$active_source()$load)
        ) {
          schedule(max(1, cache_max_age - age) * 1000, session)
        }
        assign(label, entry$loaded_at, envir = seen)
        return(entry)
      }
    }
    entry <- scans_app_load_entry(
      state$active_source(),
      label,
      session,
      clock = clock
    )
    assign(label, entry, envir = cache)
    assign(label, entry$loaded_at, envir = seen)
    if (
      is.null(entry$error) &&
        cache_max_age > 0 &&
        is.function(state$active_source()$load)
    ) {
      schedule(cache_max_age * 1000, session)
    }
    entry
  })

  state$scan_config <- shiny::reactive({
    saved <- state$active()$investigation
    if (!is.null(saved) && !state$rescan_investigation()) {
      return(saved$settings)
    }
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
  state$data <- shiny::reactive({
    current <- state$active()
    if (!is.null(current$data)) {
      return(current$data)
    }
    if (is.null(current$bundle)) {
      return(NULL)
    }
    saved <- current$investigation
    analysis <- if (!is.null(saved) && !state$rescan_investigation()) {
      saved$analysis
    }
    scans_app_data(
      current$bundle,
      state$scan_config(),
      analysis = analysis,
      scanners = state$custom_scanners
    )
  })

  output$scans_app_load_error <- shiny::renderUI({
    message <- state$active()$error
    if (is.null(message)) {
      return(NULL)
    }
    scans_app_source_error_ui(state$application(), message)
  })

  output$scans_app_load_info <- shiny::renderUI({
    shiny::invalidateLater(60 * 1000)
    current <- state$active()
    if (!is.null(current$error)) {
      return(NULL)
    }
    scans_app_load_info_ui(current, is.function(state$active_source()$load))
  })

  shiny::observeEvent(
    input$scans_app_reload,
    ignoreInit = TRUE,
    {
      if (!is.function(state$active_source()$load)) {
        return()
      }
      label <- state$application()
      for (store in list(cache, seen)) {
        if (exists(label, envir = store, inherits = FALSE)) {
          rm(list = label, envir = store)
        }
      }
      state$reload_revision(state$reload_revision() + 1L)
      state$revision(state$revision() + 1L)
    }
  )
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

# Cache the bundle, load time, and read details together. Forward reader
# progress through the scans.progress option to the Shiny notification.
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
      read_info = source$investigation$manifest$source$read_info,
      investigation = source$investigation
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
          read_info = loaded$read_info,
          recipe = loaded$recipe,
          investigation = loaded$investigation
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

scans_app_log_source_error <- function(label, cnd) {
  details <- conditionMessage(cnd)
  cli::cli_inform(c(
    "!" = "Failed to load traces for {.val {label}}.",
    "i" = "{details}"
  ))
  invisible(NULL)
}

# Use the default when the input is empty or invalid.
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
