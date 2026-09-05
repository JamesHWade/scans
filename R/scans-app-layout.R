scans_app_application_ui <- function(sources) {
  multiple <- length(sources$labels) > 1L
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
    class = "scans-app-application",
    htmltools::div(
      class = "scans-app-application-control",
      selector,
      if (sources$reloadable) {
        shiny::uiOutput("scans_app_reload_control")
      }
    ),
    shiny::uiOutput("scans_app_load_info", class = "scans-app-load-info")
  )
}

scans_app_reload_button <- function() {
  bslib::toolbar(
    align = "right",
    class = "scans-app-reload-toolbar",
    bslib::toolbar_input_button(
      "scans_app_reload",
      "Reload traces",
      icon = shiny::icon("rotate-right"),
      border = TRUE
    )
  )
}

# Filter choices need source and status metadata at UI time. Reading only the
# trajectory table avoids scanning, summarizing, and indexing an eager bundle
# before the server derives that data for the session. A lazy source has no
# metadata yet, so the server fills its choices after the first load.
scans_app_initial_choices <- function(source) {
  if (!is.null(source$data)) {
    return(scans_app_filter_choices(source$data))
  }
  if (!is.null(source$bundle)) {
    info <- trajectory_info(source$bundle)
    return(scans_app_filter_choices_from_values(
      info$source_type,
      info$status
    ))
  }
  scans_app_filter_choices()
}

scans_app_ui <- function(sources, annotations = NULL) {
  choices <- scans_app_initial_choices(sources$sources[[1L]])

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
      title = NULL,
      width = 380,
      class = "scans-app-browser",
      scans_app_application_ui(sources),
      shiny::textInput(
        "scans_app_query",
        label = NULL,
        placeholder = "Search ID, user, model, or transcript",
        width = "100%"
      ),
      scans_app_filter_toolbar(choices),
      shiny::uiOutput("scans_app_pattern_filter"),
      scans_app_scanner_ui(),
      htmltools::div(
        class = "scans-app-browser-count",
        shiny::textOutput("scans_app_visible_count", inline = TRUE),
        htmltools::div(
          class = "scans-app-browser-toggles",
          bslib::input_switch(
            "scans_app_findings_only",
            "With findings"
          ),
          if (!is.null(annotations)) {
            bslib::input_switch(
              "scans_app_annotated_only",
              "Annotated"
            )
          }
        )
      ),
      htmltools::div(
        class = "scans-app-browser-entries",
        shiny::uiOutput("scans_app_entries")
      )
    ),
    shiny::uiOutput("scans_app_load_error"),
    bslib::navset_card_underline(
      id = "scans_app_view",
      selected = "application",
      full_screen = TRUE,
      wrapper = function(...) bslib::card_body(..., padding = 0),
      bslib::nav_panel(
        "Application overview",
        value = "application",
        bslib::card_body(
          fill = FALSE,
          class = "scans-app-performance-controls",
          shiny::selectInput(
            "scans_app_priority",
            "Order trajectories by",
            choices = c(
              "Elapsed time" = "elapsed",
              "Recorded tokens" = "tokens",
              "Findings" = "findings"
            ),
            width = "220px"
          )
        ),
        shiny::uiOutput("scans_app_performance")
      ),
      bslib::nav_panel(
        "Trajectory",
        value = "trajectory",
        bslib::card_body(
          fill = FALSE,
          class = "scans-app-workspace-header",
          htmltools::div(
            class = "scans-app-workspace-bar",
            shiny::uiOutput("scans_app_header"),
            scans_app_workspace_toolbar()
          ),
          shiny::uiOutput("scans_app_overview")
        ),
        bslib::layout_sidebar(
          fillable = TRUE,
          border = FALSE,
          border_radius = FALSE,
          padding = 0,
          gap = 0,
          sidebar = bslib::sidebar(
            title = NULL,
            position = "right",
            width = 340,
            class = "scans-app-evidence",
            scans_app_annotation_ui(annotations),
            shiny::uiOutput("scans_app_evidence")
          ),
          htmltools::tags$main(
            class = "scans-app-transcript",
            shiny::uiOutput("scans_app_resources"),
            shiny::uiOutput("scans_app_transcript")
          )
        )
      )
    )
  )
  scans_app_attach_dependency(page)
}

# Filters live in one compact toolbar row rather than a stack of labelled
# selects: the sidebar's vertical space belongs to the trajectory list.
scans_app_filter_toolbar <- function(choices) {
  bslib::toolbar(
    align = "left",
    width = "100%",
    class = "scans-app-filters",
    bslib::toolbar_input_select(
      "scans_app_source",
      "Source",
      choices = choices$source,
      icon = shiny::icon("database")
    ),
    bslib::toolbar_input_select(
      "scans_app_status",
      "Status",
      choices = choices$status,
      icon = shiny::icon("circle-check")
    ),
    bslib::toolbar_input_select(
      "scans_app_sort",
      "Order",
      choices = scans_app_sort_choices,
      icon = shiny::icon("arrow-down-wide-short")
    )
  )
}

# Stepping and tool-disclosure controls sit in the card header. Tool
# traffic renders collapsed; a reviewer chasing a loop wants it all open at
# once, and a reviewer reading the conversation wants it all shut. Those two
# buttons are handled in the browser alone.
scans_app_workspace_toolbar <- function() {
  bslib::toolbar(
    align = "right",
    class = "scans-app-workspace-toolbar",
    htmltools::tags$span(
      class = "scans-app-heading-position",
      shiny::textOutput("scans_app_position", inline = TRUE)
    ),
    bslib::toolbar_input_button(
      "scans_app_prev",
      "Previous trajectory (K or \u2191)",
      icon = shiny::icon("chevron-up")
    ),
    bslib::toolbar_input_button(
      "scans_app_next",
      "Next trajectory (J or \u2193)",
      icon = shiny::icon("chevron-down")
    ),
    bslib::toolbar_divider(),
    bslib::toolbar_input_button(
      "scans_app_tools_open",
      "Expand all tool calls",
      icon = shiny::icon("angles-down")
    ),
    bslib::toolbar_input_button(
      "scans_app_tools_close",
      "Collapse all tool calls",
      icon = shiny::icon("angles-up")
    )
  )
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
# what someone typed. It sits with the evidence, beside the findings the
# judgement is about, rather than above the transcript.
scans_app_annotation_ui <- function(annotations) {
  if (is.null(annotations)) {
    return(NULL)
  }
  bslib::accordion(
    open = TRUE,
    class = "scans-app-annotations",
    bslib::accordion_panel(
      "Annotate",
      value = "annotate",
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
        htmltools::div(
          class = "scans-app-annotation-actions",
          shiny::actionButton(
            "scans_app_annotation_save",
            "Save annotation",
            class = "btn-sm btn-primary"
          ),
          htmltools::div(
            class = "scans-app-annotation-status",
            shiny::textOutput("scans_app_annotation_status", inline = TRUE)
          )
        )
      ),
      shiny::uiOutput("scans_app_annotation_log")
    )
  )
}
