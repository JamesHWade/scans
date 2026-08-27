scans_app_application_ui <- function(sources) {
  multiple <- length(sources$labels) > 1L
  if (!multiple && !sources$reloadable) {
    return(NULL)
  }
  selector <- if (multiple) {
    shiny::selectInput(
      "scans_app_application",
      "Application",
      choices = stats::setNames(sources$labels, sources$labels),
      width = "100%"
    )
  } else {
    htmltools::div(
      class = "scans-app-application-label",
      htmltools::tags$span("Application"),
      htmltools::tags$strong(sources$labels[[1L]])
    )
  }

  htmltools::div(
    class = "scans-app-application-control",
    selector,
    if (sources$reloadable) {
      shiny::uiOutput("scans_app_reload_control")
    }
  )
}

scans_app_reload_button <- function() {
  shiny::actionButton(
    "scans_app_reload",
    "Reload traces",
    class = "btn-sm btn-outline-secondary"
  )
}

# Filter choices need a snapshot at UI time. An eagerly supplied bundle can
# provide one; a lazy source cannot, and its choices are filled in by the
# server once the first load completes.
scans_app_initial_data <- function(source) {
  if (!is.null(source$data)) {
    return(source$data)
  }
  if (!is.null(source$bundle)) {
    return(scans_app_data(source$bundle))
  }
  NULL
}

scans_app_ui <- function(sources, annotations = NULL) {
  initial_data <- scans_app_initial_data(sources$sources[[1L]])
  choices <- scans_app_filter_choices(initial_data)

  page <- bslib::page_sidebar(
    title = htmltools::div(
      class = "scans-app-brand",
      htmltools::tags$span("scans"),
      htmltools::tags$small("Trajectory diagnostics")
    ),
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f6f7fb",
      fg = "#1e2430",
      primary = "#5356c9"
    ),
    fillable = TRUE,
    class = "bslib-page-dashboard scans-app",
    sidebar = bslib::sidebar(
      title = "Trajectories",
      width = 370,
      class = "scans-app-browser",
      scans_app_application_ui(sources),
      shiny::textInput(
        "scans_app_query",
        "Search",
        placeholder = "ID, source, agent, or transcript",
        width = "100%"
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::selectInput(
          "scans_app_source",
          "Source",
          choices = choices$source,
          width = "100%"
        ),
        shiny::selectInput(
          "scans_app_status",
          "Status",
          choices = choices$status,
          width = "100%"
        )
      ),
      bslib::input_switch(
        "scans_app_findings_only",
        "Only trajectories with findings"
      ),
      scans_app_scanner_ui(),
      htmltools::div(
        class = "scans-app-browser-count",
        shiny::textOutput("scans_app_visible_count", inline = TRUE)
      ),
      htmltools::div(
        class = "scans-app-browser-entries",
        shiny::uiOutput("scans_app_entries")
      )
    ),
    shiny::uiOutput("scans_app_load_error"),
    scans_app_annotation_ui(annotations),
    shiny::uiOutput("scans_app_overview"),
    bslib::card(
      fill = TRUE,
      full_screen = TRUE,
      class = "scans-app-workspace",
      bslib::card_header(
        class = "scans-app-workspace-header",
        shiny::uiOutput("scans_app_header")
      ),
      bslib::layout_sidebar(
        fillable = TRUE,
        border = FALSE,
        border_radius = FALSE,
        padding = 0,
        gap = 0,
        sidebar = bslib::sidebar(
          title = "Evidence",
          position = "right",
          width = 350,
          class = "scans-app-evidence",
          shiny::uiOutput("scans_app_evidence")
        ),
        htmltools::tags$main(
          class = "scans-app-transcript",
          shiny::uiOutput("scans_app_transcript")
        )
      )
    )
  )
  scans_app_attach_dependency(page)
}

scans_app_attach_dependency <- function(page) {
  htmltools::attachDependencies(
    page,
    scans_app_dependency(),
    append = TRUE
  )
}


# The scanner panel. Scans are cheap and run over the bundle already in
# memory, so they re-run as the selection changes rather than behind a button:
# a "Run" control would imply a cost that isn't there, and would let the
# findings on screen disagree with the boxes that produced them.
scans_app_scanner_ui <- function(registry = scan_registry()) {
  choices <- stats::setNames(registry$scan, scans_app_scan_label(registry))
  bslib::accordion(
    open = FALSE,
    class = "scans-app-scanners",
    bslib::accordion_panel(
      "Scans",
      icon = NULL,
      shiny::checkboxGroupInput(
        "scans_app_scans",
        label = NULL,
        choices = choices,
        selected = registry$scan,
        width = "100%"
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::numericInput(
          "scans_app_repeat_threshold",
          "Repeat at",
          value = 2L,
          min = 2L,
          step = 1L,
          width = "100%"
        ),
        shiny::numericInput(
          "scans_app_loop_threshold",
          "Loop at",
          value = 3L,
          min = 2L,
          step = 1L,
          width = "100%"
        )
      ),
      htmltools::div(
        class = "scans-app-scanner-note",
        shiny::textOutput("scans_app_scan_summary", inline = TRUE)
      )
    )
  )
}

# Severity rides along in the label so the panel says what a scan reports
# without a second column of chrome.
scans_app_scan_label <- function(registry) {
  paste0(
    gsub("_", " ", registry$scan),
    " \u00b7 ",
    registry$severity
  )
}


# The annotation panel appears only when the app was given a store. An app
# with nowhere to write should not offer a control that silently discards
# what someone typed.
scans_app_annotation_ui <- function(annotations) {
  if (is.null(annotations)) {
    return(NULL)
  }
  bslib::accordion(
    open = FALSE,
    class = "scans-app-annotations",
    bslib::accordion_panel(
      "Annotations",
      htmltools::div(
        class = "scans-app-annotation-form",
        shiny::selectInput(
          "scans_app_annotation_label",
          "Label",
          choices = c("(none)" = "", annotations$labels),
          width = "100%"
        ),
        shiny::textAreaInput(
          "scans_app_annotation_note",
          "Note",
          placeholder = "What did you notice about this trajectory?",
          width = "100%",
          rows = 3
        ),
        shiny::actionButton(
          "scans_app_annotation_save",
          "Save annotation",
          class = "btn-sm btn-primary"
        ),
        htmltools::div(
          class = "scans-app-annotation-status",
          shiny::textOutput("scans_app_annotation_status", inline = TRUE)
        )
      ),
      shiny::uiOutput("scans_app_annotation_log")
    )
  )
}
