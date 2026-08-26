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

scans_app_ui <- function(sources) {
  initial_data <- sources$sources[[1L]]$data
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
      htmltools::div(
        class = "scans-app-browser-count",
        shiny::textOutput("scans_app_visible_count", inline = TRUE)
      ),
      htmltools::div(
        class = "scans-app-browser-entries",
        shiny::uiOutput("scans_app_entries")
      )
    ),
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
