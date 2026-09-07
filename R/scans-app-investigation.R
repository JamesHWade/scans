scans_app_investigation_max_bytes <- function() {
  getOption("shiny.maxRequestSize", 50 * 1024^2)
}

scans_app_investigation_start <- function() {
  if (!is.null(getOption("shiny.maxRequestSize"))) {
    return(invisible(NULL))
  }
  old <- options(shiny.maxRequestSize = scans_app_investigation_max_bytes())
  shiny::onStop(function() options(old), session = NULL)
}

scans_app_investigation_ui <- function() {
  htmltools::div(
    class = "scans-app-investigation",
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Save or open",
        shiny::actionButton(
          "scans_app_save_investigation",
          "Save visible trajectories",
          width = "100%"
        ),
        shiny::fileInput(
          "scans_app_open_investigation",
          "Open an investigation",
          accept = ".json",
          width = "100%"
        ),
        htmltools::tags$small(
          paste0(
            "Files include selected retained content. Opening stays in this session. Upload limit: ",
            format(scans_app_investigation_max_bytes() / 1024^2, trim = TRUE),
            " MiB."
          )
        )
      )
    ),
    shiny::uiOutput("scans_app_investigation_status")
  )
}

scans_app_investigation_status_ui <- function(saved, rescanned, uploaded) {
  if (is.null(saved)) {
    return(NULL)
  }
  htmltools::div(
    class = "scans-app-investigation-status",
    htmltools::tags$strong(paste("Opened:", saved$manifest$application)),
    htmltools::tags$p(
      if (rescanned) {
        "Showing a new analysis with the installed scanners. Save to create a revision."
      } else {
        paste0(
          "Showing saved analysis from scans ",
          saved$manifest$package_version,
          ". Change scan settings below, then apply current scanners to reanalyse."
        )
      }
    ),
    if (!rescanned && !all(saved$settings$scans %in% scan_registry()$scan)) {
      htmltools::tags$p(
        "Some saved scanners are unavailable in this installation. Saved results remain visible; choose current scanners below to run a new analysis."
      )
    },
    htmltools::tags$small(paste(
      "Snapshot",
      substr(saved$manifest$snapshot_id, 8, 19)
    )),
    if (!rescanned) {
      shiny::actionButton(
        "scans_app_rescan_investigation",
        "Apply current scanners",
        width = "100%"
      )
    },
    if (uploaded) {
      shiny::actionButton(
        "scans_app_close_investigation",
        "Return to application",
        width = "100%"
      )
    }
  )
}

scans_app_investigation_save_ui <- function(saved) {
  count <- nrow(trajectory_info(saved$bundle))
  shiny::modalDialog(
    title = "Save investigation",
    htmltools::tags$p(paste0(
      count,
      " selected ",
      if (count == 1L) "trajectory" else "trajectories",
      " with saved findings, assessments, measurements, filters, and scanner settings."
    )),
    htmltools::tags$p(
      "Includes retained text, tool arguments and results, evaluations, metadata, and capture limits. Existing redactions remain; no additional anonymization is performed."
    ),
    htmltools::tags$p(
      "Unselected records and annotation history are excluded. Review the selected evidence before sharing this file."
    ),
    shiny::downloadButton("scans_app_download_investigation", "Download JSON"),
    footer = shiny::modalButton("Close"),
    easyClose = TRUE
  )
}

scans_app_investigation_view <- function(
  input,
  data,
  ids,
  selected,
  pattern,
  annotated,
  annotated_only
) {
  choices <- scans_app_filter_choices(data)
  source <- input$scans_app_source
  if (is.null(source) || identical(source, choices$source_all)) {
    source <- NULL
  }
  status <- input$scans_app_status
  if (is.null(status) || identical(status, choices$status_all)) {
    status <- NULL
  }
  if (
    identical(
      status,
      scans_app_filter_sentinel(data$records$status, "status-unknown")
    )
  ) {
    status <- NA_character_
  }
  list(
    query = input$scans_app_query %||% "",
    source_type = source,
    status = status,
    findings_only = isTRUE(input$scans_app_findings_only),
    annotated_only = annotated_only,
    annotation_ids = intersect(annotated, ids),
    pattern = pattern,
    sort = input$scans_app_sort %||% "newest",
    selected_trajectory_id = if (length(selected) == 1L && selected %in% ids) {
      selected
    } else {
      NULL
    },
    tab = input$scans_app_view %||% "application",
    priority = input$scans_app_priority %||% "elapsed"
  )
}

scans_app_restore_investigation <- function(saved, data, session) {
  view <- saved$view
  choices <- scans_app_filter_choices(data)
  for (name in c(
    "source",
    "status",
    "query",
    "sort",
    "findings_only",
    "annotated_only",
    "scans",
    "repeat_threshold",
    "loop_threshold",
    "priority",
    "view"
  )) {
    shiny::freezeReactiveValue(session$input, paste0("scans_app_", name))
  }
  status <- view$status %||% choices$status_all
  if (is.na(status)) {
    status <- scans_app_filter_sentinel(data$records$status, "status-unknown")
  }
  bslib::update_toolbar_input_select(
    "scans_app_source",
    choices = choices$source,
    selected = view$source_type %||% choices$source_all,
    session = session
  )
  bslib::update_toolbar_input_select(
    "scans_app_status",
    choices = choices$status,
    selected = status,
    session = session
  )
  bslib::update_toolbar_input_select(
    "scans_app_sort",
    choices = scans_app_sort_choices,
    selected = view$sort,
    session = session
  )
  shiny::updateTextInput(session, "scans_app_query", value = view$query)
  bslib::update_switch(
    "scans_app_findings_only",
    value = view$findings_only,
    session = session
  )
  bslib::update_switch(
    "scans_app_annotated_only",
    value = view$annotated_only,
    session = session
  )
  shiny::updateCheckboxGroupInput(
    session,
    "scans_app_scans",
    selected = saved$settings$scans
  )
  shiny::updateNumericInput(
    session,
    "scans_app_repeat_threshold",
    value = saved$settings$repeat_threshold
  )
  shiny::updateNumericInput(
    session,
    "scans_app_loop_threshold",
    value = saved$settings$loop_threshold
  )
  shiny::updateSelectInput(
    session,
    "scans_app_priority",
    selected = view$priority
  )
  bslib::nav_select("scans_app_view", view$tab, session = session)
}
