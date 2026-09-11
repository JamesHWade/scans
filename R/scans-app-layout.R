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

scans_app_ui <- function(
  sources,
  annotations = NULL,
  investigations = FALSE,
  reviews = FALSE,
  chat = FALSE
) {
  choices <- scans_app_initial_choices(sources$sources[[1L]])
  page <- bslib::page_sidebar(
    window_title = "scans",
    title = htmltools::div(
      class = "scans-app-topbar",
      htmltools::div(class = "scans-app-brand", htmltools::tags$span("scans")),
      scans_app_application_ui(sources),
      shiny::actionButton(
        "scans_app_overview_action",
        "Overview",
        class = "btn-outline-secondary"
      ),
      bslib::popover(
        shiny::actionButton(
          "scans_app_options",
          "Options",
          icon = shiny::icon("ellipsis")
        ),
        title = "Snapshot options",
        if (investigations) scans_app_investigation_ui(),
        scans_app_scanner_ui()
      )
    ),
    theme = shinychat::page_chat_theme(
      primary = "#0d6efd",
      fg = "#171c32",
      bg = "#ffffff",
      `primary-bg-subtle` = "#edf5ff",
      `body-tertiary-bg` = "#f4f6f9",
      `body-secondary-color` = "#667085",
      `border-color` = "#dfe4ed",
      `border-radius` = "0.4rem"
    ),
    fillable = TRUE,
    fillable_mobile = TRUE,
    padding = 0,
    gap = 0,
    class = "scans-app-workspace-main",
    sidebar = bslib::sidebar(
      id = "scans_app_browser_pane",
      title = "Trajectories",
      width = 350,
      open = list(desktop = "open", mobile = "closed"),
      class = "scans-app-browser",
      padding = c("0.5rem", "1.2rem", "1.2rem"),
      gap = "1rem",
      htmltools::div(
        class = "scans-app-search",
        shiny::textInput(
          "scans_app_query",
          label = "Search trajectories",
          placeholder = "Search trajectories",
          width = "100%"
        )
      ),
      scans_app_filter_toolbar(choices, annotations),
      shiny::uiOutput("scans_app_pattern_filter"),
      htmltools::div(
        class = "scans-app-browser-count",
        shiny::textOutput("scans_app_visible_count", inline = TRUE)
      ),
      shiny::uiOutput("scans_app_snapshot_annotation_filter"),
      htmltools::div(
        class = "scans-app-browser-entries",
        shiny::uiOutput("scans_app_entries")
      )
    ),
    shiny::uiOutput("scans_app_load_error"),
    shiny::uiOutput("scans_app_retained_notice"),
    bslib::layout_sidebar(
      fillable = TRUE,
      border = FALSE,
      border_radius = FALSE,
      padding = 0,
      gap = 0,
      sidebar = bslib::sidebar(
        id = "scans_app_investigation_pane",
        title = NULL,
        position = "right",
        width = 460,
        open = "closed",
        fillable = TRUE,
        class = "scans-app-evidence",
        padding = c("0.4rem", "1.2rem", "1rem"),
        gap = "0.7rem",
        bslib::navset_underline(
          id = "scans_app_investigation_tab",
          selected = "findings",
          bslib::nav_panel(
            "Findings",
            value = "findings",
            shiny::uiOutput("scans_app_evidence"),
            scans_app_annotation_ui(annotations),
            if (reviews) scans_app_review_ui()
          ),
          if (chat) bslib::nav_panel("Ask", value = "ask", scans_app_ask_ui())
        )
      ),
      bslib::navset_hidden(
        id = "scans_app_view",
        selected = "application",
        bslib::nav_panel_hidden(
          "application",
          shiny::uiOutput("scans_app_performance")
        ),
        bslib::nav_panel_hidden(
          "trajectory",
          htmltools::div(
            class = "scans-app-workspace-header",
            shiny::uiOutput("scans_app_header"),
            htmltools::div(
              class = "scans-app-workspace-bar",
              htmltools::tags$strong("Transcript"),
              scans_app_workspace_toolbar()
            )
          ),
          htmltools::tags$main(
            class = "scans-app-transcript",
            shiny::uiOutput("scans_app_transcript"),
            shiny::uiOutput("scans_app_resources")
          )
        )
      )
    )
  )
  page <- htmltools::tagQuery(page)$filter("body")$addClass(
    "scans-app"
  )$allTags()
  scans_app_attach_dependency(page)
}

scans_app_filter_toolbar <- function(choices, annotations = NULL) {
  bslib::toolbar(
    align = "left",
    width = "100%",
    class = "scans-app-filters",
    bslib::popover(
      shiny::actionButton(
        "scans_app_filters",
        "Filters",
        icon = shiny::icon("filter")
      ),
      title = "Filter trajectories",
      scans_app_toolbar_select(
        "scans_app_source",
        "Source",
        choices = choices$source
      ),
      scans_app_toolbar_select(
        "scans_app_status",
        "Status",
        choices = choices$status
      ),
      bslib::input_switch("scans_app_findings_only", "With findings"),
      if (!is.null(annotations)) {
        bslib::input_switch("scans_app_annotated_only", "Annotated")
      }
    ),
    scans_app_toolbar_select(
      "scans_app_sort",
      "Order",
      choices = scans_app_sort_choices,
      selected = "findings"
    )
  )
}

scans_app_toolbar_select <- function(id, label, choices, selected = NULL) {
  control <- bslib::toolbar_input_select(
    id,
    label,
    choices,
    selected = selected
  )
  htmltools::tagQuery(control)$find("select")$addAttrs(
    `aria-label` = label
  )$allTags()
}

# Tool disclosure controls are handled in the browser.
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


# Changing scan settings recomputes findings from the loaded bundle.
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

scans_app_scan_label <- function(registry) {
  paste0(
    gsub("_", " ", registry$scan),
    " \u00b7 ",
    registry$severity
  )
}


# Offer annotation controls only when an annotation store was configured.
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
