scans_app_navigation_server <- function(
  input,
  output,
  session,
  state,
  sources,
  chat_factory
) {
  # The selected entry is highlighted in the browser rather than by
  # re-rendering the whole list: with a hundred entries the re-render was
  # the slowest part of clicking.
  shiny::observe({
    index <- state$selected()
    showing_trajectory <- !identical(input$scans_app_view, "application")
    current <- shiny::isolate(state$data())
    trajectory_id <- if (!is.null(index) && !is.null(current)) {
      current$info$trajectory_id[[index]]
    }
    session$sendCustomMessage(
      "scans-app-select",
      list(
        id = if (is.null(index) || !showing_trajectory) {
          NULL
        } else {
          scans_app_entry_id(index)
        },
        hash = scans_app_hash(
          if (is.null(index) || !showing_trajectory) {
            NULL
          } else {
            shiny::isolate(state$application())
          },
          if (showing_trajectory) trajectory_id else NULL
        )
      )
    )
  })

  # A URL hash of "<application>/<trajectory_id>" deep-links to one
  # trajectory. The application is switched first; the selection is applied
  # once that application's data is available.
  state$pending_hash <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$scans_app_hash, {
    target <- scans_app_parse_hash(input$scans_app_hash, sources$labels)
    if (is.null(target)) {
      return()
    }
    state$pending_hash(target)
    if (!identical(target$application, state$application())) {
      shiny::updateSelectInput(
        session,
        "scans_app_application",
        selected = target$application
      )
    }
  })
  shiny::observe({
    target <- state$pending_hash()
    current <- state$data()
    if (is.null(target) || is.null(current)) {
      return()
    }
    if (!identical(target$application, state$application())) {
      return()
    }
    index <- match(target$trajectory_id, current$info$trajectory_id)
    if (!is.na(index)) {
      state$selected(index)
      bslib::nav_select("scans_app_view", "trajectory", session = session)
    }
    if (
      !is.na(index) &&
        scans_app_has_string(target$event_id) &&
        target$event_id %in%
          current$events$event_id[
            current$events$trajectory_id == target$trajectory_id
          ]
    ) {
      session$sendCustomMessage(
        "scans-app-reveal",
        list(
          id = scans_app_event_dom_id(target$event_id),
          event_id = target$event_id
        )
      )
    }
    state$pending_hash(NULL)
  })

  shiny::observeEvent(input$scans_app_overview_action, {
    bslib::nav_select("scans_app_view", "application", session = session)
    bslib::toggle_sidebar(
      "scans_app_investigation_pane",
      open = FALSE,
      session = session
    )
  })
  shiny::observeEvent(input$scans_app_view, {
    if (
      identical(input$scans_app_view, "trajectory") &&
        !isTRUE(input$scans_app_small_screen)
    ) {
      bslib::toggle_sidebar(
        "scans_app_investigation_pane",
        open = TRUE,
        session = session
      )
    }
  })
  shiny::observeEvent(input$scans_app_ask_overview, {
    shiny::req(!is.null(chat_factory))
    shiny::updateSelectInput(
      session,
      "scans_app_ask_scope",
      selected = "filtered"
    )
    bslib::toggle_sidebar(
      "scans_app_investigation_pane",
      open = TRUE,
      session = session
    )
    bslib::nav_select("scans_app_investigation_tab", "ask", session = session)
  })
}

scans_app_hash <- function(application, trajectory_id) {
  if (is.null(application) || is.null(trajectory_id)) {
    return("")
  }
  paste(
    utils::URLencode(application, reserved = TRUE, repeated = TRUE),
    utils::URLencode(trajectory_id, reserved = TRUE, repeated = TRUE),
    sep = "/"
  )
}

scans_app_parse_hash <- function(hash, labels) {
  if (!scans_app_has_string(hash)) {
    return(NULL)
  }
  hash <- sub("^#", "", hash)
  event_parts <- strsplit(hash, "?event=", fixed = TRUE)[[1L]]
  hash <- event_parts[[1L]]
  parts <- strsplit(hash, "/", fixed = TRUE)[[1L]]
  if (length(parts) < 2L) {
    return(NULL)
  }
  application <- utils::URLdecode(parts[[1L]])
  trajectory_id <- utils::URLdecode(paste(parts[-1L], collapse = "/"))
  if (!application %in% labels || !nzchar(trajectory_id)) {
    return(NULL)
  }
  target <- list(application = application, trajectory_id = trajectory_id)
  if (length(event_parts) == 2L) {
    target$event_id <- utils::URLdecode(event_parts[[2L]])
  }
  target
}
