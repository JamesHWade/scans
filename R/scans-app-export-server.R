scans_app_export_menu_ui <- function(noun = scans_app_noun(), users = FALSE) {
  htmltools::div(
    class = "dropdown scans-app-export",
    htmltools::tags$button(
      type = "button",
      class = "btn btn-outline-secondary dropdown-toggle",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      "Export"
    ),
    htmltools::tags$ul(
      class = "dropdown-menu dropdown-menu-end",
      htmltools::tags$li(shiny::downloadLink(
        "scans_app_export_conversations",
        paste(scans_app_title_case(noun$many), "in view (CSV)"),
        class = "dropdown-item"
      )),
      htmltools::tags$li(shiny::downloadLink(
        "scans_app_export_findings",
        "Findings in view (CSV)",
        class = "dropdown-item"
      )),
      htmltools::tags$li(shiny::actionLink(
        "scans_app_export_report",
        "Report\u2026",
        class = "dropdown-item"
      )),
      htmltools::tags$li(shiny::actionLink(
        "scans_app_export_code",
        "R code\u2026",
        class = "dropdown-item"
      )),
      htmltools::tags$li(htmltools::tags$hr(class = "dropdown-divider")),
      htmltools::tags$li(
        class = "dropdown-item-text scans-app-export-option",
        # Ticking the option should not close the menu it sits in.
        onclick = "event.stopPropagation()",
        shiny::checkboxInput(
          "scans_app_export_users",
          "Include user identifiers in CSVs",
          value = users
        )
      )
    )
  )
}

scans_app_trajectory_export_ui <- function(link) {
  htmltools::div(
    class = "scans-app-trajectory-export",
    htmltools::tags$button(
      type = "button",
      class = "btn btn-sm btn-link",
      `data-scans-copy` = link,
      "Copy link"
    ),
    shiny::actionLink(
      "scans_app_export_trajectory",
      "Copy as Markdown\u2026",
      class = "btn btn-sm btn-link"
    )
  )
}

# The text sits in a read-only box the reader can select by hand if the
# browser refuses clipboard access.
scans_app_export_text_ui <- function(text, label) {
  htmltools::tagList(
    htmltools::tags$textarea(
      id = "scans_app_export_text",
      class = "form-control scans-app-export-text",
      readonly = NA,
      rows = 18,
      spellcheck = "false",
      `aria-label` = label,
      paste(text, collapse = "\n")
    )
  )
}

scans_app_export_modal <- function(
  title,
  note,
  options = NULL,
  download = NULL
) {
  shiny::modalDialog(
    title = title,
    size = "l",
    easyClose = TRUE,
    htmltools::tags$p(class = "scans-app-export-note", note),
    options,
    shiny::uiOutput("scans_app_export_body"),
    footer = htmltools::tagList(
      htmltools::tags$span(
        class = "scans-app-copy-status",
        role = "status",
        `aria-live` = "polite"
      ),
      htmltools::tags$button(
        type = "button",
        class = "btn btn-primary",
        `data-scans-copy-from` = "scans_app_export_text",
        "Copy"
      ),
      download,
      shiny::modalButton("Close")
    )
  )
}

scans_app_export_filename <- function(application, kind, extension) {
  paste0(
    "scans-",
    gsub("[^a-z0-9]+", "-", tolower(application)),
    "-",
    kind,
    "-",
    format(Sys.time(), "%Y%m%d", tz = "UTC"),
    ".",
    extension
  )
}

scans_app_export_server <- function(input, output, session, state) {
  # The app's own address, so exported links open this deployment.
  link_base <- shiny::reactive({
    client <- session$clientData
    host <- client$url_hostname
    if (!scans_app_has_string(host)) {
      return(NULL)
    }
    port <- client$url_port
    paste0(
      client$url_protocol %||% "http:",
      "//",
      host,
      if (scans_app_has_string(as.character(port))) paste0(":", port),
      client$url_pathname %||% "/"
    )
  })
  # A text filter can name a user, as the search covers user identifiers.
  # Exports without user identifiers then leave its term out, as the report
  # does.
  query_names_user <- shiny::reactive({
    report_query_names_user(state$view()$query, state$data()$records$user)
  })
  state$link <- shiny::reactive({
    scans_app_link(
      link_base(),
      state$application(),
      c(
        scans_app_view_scope(state$view()),
        list(
          review = state$review_filter()$state,
          review_label = state$review_filter()$label
        ),
        state$facets()
      ),
      private = c("user", if (query_names_user()) "query")
    )
  })
  # The report for the trajectories in view, built by the function a script
  # calls on a saved investigation. For an opened investigation the two are
  # the same report.
  state$report <- shiny::reactive({
    current <- state$data()
    shiny::req(current)
    entry <- state$active()
    saved <- entry$investigation
    context <- if (is.null(saved)) {
      list(
        read_info = entry$read_info,
        loaded_at = entry$loaded_at,
        analysis_version = as.character(utils::packageVersion("scans"))
      )
    } else {
      report_saved_context(saved)
    }
    if (!is.null(saved) && state$rescan_investigation()) {
      context$snapshot["revision_id"] <- list(NULL)
      context$analysis_version <- as.character(utils::packageVersion("scans"))
    }
    report_build(
      current,
      state$visible(),
      c(
        context,
        list(
          application = state$application(),
          view = state$view(),
          facets = state$facets(),
          review = state$review_filter(),
          annotated_label = scans_app_annotated_label(saved, state$has_store),
          app_url = link_base(),
          settings = state$scan_config(),
          bundle = entry$bundle,
          queue = state$queue(),
          include_users = isTRUE(input$scans_app_report_users)
        )
      )
    )
  })
  context <- shiny::reactive({
    current <- state$data()
    shiny::req(current)
    # Exports describe exactly the trajectories in view, so their tables are
    # not widened the way the on-screen breakdowns are, and review progress
    # is counted over those trajectories rather than the on-screen queue's.
    performance <- scans_app_performance_data(
      current,
      state$visible(),
      queue = state$queue(),
      candidates = state$candidates()
    )
    labels <- state$filter_labels()
    list(
      data = current,
      ids = current$info$trajectory_id[state$visible()],
      performance = performance,
      scope = function(include_users) {
        export_scope(
          performance$cohort$headline,
          labels,
          current$noun,
          include_users
        )
      }
    )
  })
  csv_users <- function() isTRUE(input$scans_app_export_users)

  output$scans_app_export_conversations <- shiny::downloadHandler(
    filename = function() {
      scans_app_export_filename(state$application(), "conversations", "csv")
    },
    content = function(file) {
      export <- context()
      export_csv(
        export_conversations(
          export$data,
          export$ids,
          state$queue(),
          state$link(),
          state$application(),
          export$scope(csv_users()),
          include_users = csv_users()
        ),
        file
      )
    },
    contentType = "text/csv"
  )
  output$scans_app_export_findings <- shiny::downloadHandler(
    filename = function() {
      scans_app_export_filename(state$application(), "findings", "csv")
    },
    content = function(file) {
      export <- context()
      export_csv(
        export_findings(
          export$data,
          export$ids,
          state$link(),
          state$application(),
          export$scope(csv_users()),
          include_users = csv_users()
        ),
        file
      )
    },
    contentType = "text/csv"
  )

  # The links sit in a closed menu; without this Shiny would not give a
  # hidden link its download address.
  for (id in c("scans_app_export_conversations", "scans_app_export_findings")) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }

  kind <- shiny::reactiveVal(NULL)
  state$export_text <- shiny::reactive({
    export <- context()
    switch(
      kind() %||% "report",
      report = format(state$report()),
      code = export_r_code(
        state$active()$recipe,
        state$scan_config(),
        export$ids,
        state$application()
      ),
      trajectory = {
        index <- state$selected()
        shiny::req(index)
        export_trajectory_markdown(
          export$data,
          index,
          state$link()(export$data$info$trajectory_id[[index]], users = FALSE)
        )
      }
    )
  })
  output$scans_app_export_body <- shiny::renderUI({
    scans_app_export_text_ui(state$export_text(), "Export text")
  })
  output$scans_app_export_download <- shiny::downloadHandler(
    filename = function() {
      scans_app_export_filename(state$application(), "report", "md")
    },
    content = function(file) {
      write_investigation_report(
        state$report(),
        file,
        "markdown",
        overwrite = TRUE
      )
    },
    contentType = "text/markdown"
  )
  output$scans_app_export_report_html <- shiny::downloadHandler(
    filename = function() {
      scans_app_export_filename(state$application(), "report", "html")
    },
    content = function(file) {
      write_investigation_report(state$report(), file, "html", overwrite = TRUE)
    },
    contentType = "text/html"
  )
  shiny::observeEvent(input$scans_app_export_report, {
    kind("report")
    shiny::showModal(
      scans_app_export_modal(
        "Report",
        "Counts, capture limits, and the evidence identities behind each pattern for the trajectories in view. It contains no transcript text. The HTML file and this Markdown hold the same report.",
        options = shiny::checkboxInput(
          "scans_app_report_users",
          "Include user identifiers",
          value = FALSE
        ),
        download = htmltools::tagList(
          shiny::downloadButton(
            "scans_app_export_report_html",
            "Download HTML",
            class = "btn-default"
          ),
          shiny::downloadButton(
            "scans_app_export_download",
            "Download Markdown",
            class = "btn-default"
          )
        )
      ),
      session = session
    )
  })
  shiny::observeEvent(input$scans_app_export_code, {
    kind("code")
    shiny::showModal(
      scans_app_export_modal(
        "R code",
        "Rebuilds this snapshot, reruns the active scans, and selects the trajectories in view. It contains no credentials."
      ),
      session = session
    )
  })
  shiny::observeEvent(input$scans_app_export_trajectory, {
    kind("trajectory")
    shiny::showModal(
      scans_app_export_modal(
        "Copy as Markdown",
        "Facts, findings, and the start of the transcript. This contains conversation content as recorded."
      ),
      session = session
    )
  })
  output$scans_app_trajectory_export <- shiny::renderUI({
    current <- state$data()
    index <- state$selected()
    if (is.null(current) || is.null(index)) {
      return(NULL)
    }
    scans_app_trajectory_export_ui(
      state$link()(current$info$trajectory_id[[index]])
    )
  })
}
