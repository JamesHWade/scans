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
  # Runs after the resets an application change triggers, so a scope carried
  # by the link is not cleared again.
  shiny::observe(priority = -1L, {
    target <- state$pending_hash()
    current <- state$data()
    if (is.null(target) || is.null(current)) {
      return()
    }
    if (!identical(target$application, state$application())) {
      return()
    }
    # A link that names a scope replaces the pattern, facets, findings
    # switch, and review filter; one without leaves the current filters alone.
    scope <- target$scope
    if (length(scope)) {
      state$facets(scans_app_facet_validate(
        current,
        scope[intersect(names(scope), scans_app_facet_names)]
      ))
      known <- scans_app_has_string(scope$pattern) &&
        scope$pattern %in%
          intersect(current$findings$scan, state$scan_config()$scans)
      state$pattern_filter(if (known) scope$pattern)
      bslib::update_switch(
        "scans_app_findings_only",
        value = isTRUE(scope$findings),
        session = session
      )
      review <- scans_app_scope_review(
        scope,
        state$has_store,
        state$review_labels
      )
      if (state$has_store) {
        shiny::updateSelectInput(
          session,
          "scans_app_review_state",
          selected = review$state %||% "all"
        )
        shiny::updateSelectInput(
          session,
          "scans_app_review_label",
          selected = review$label %||% ""
        )
      }
      if (!is.null(review$notice)) {
        shiny::showNotification(
          review$notice,
          duration = NULL,
          type = "warning",
          session = session
        )
      }
    }
    index <- match(target$trajectory_id %||% NA, current$info$trajectory_id)
    if (!is.na(index)) {
      state$selected(index)
      bslib::nav_select("scans_app_view", "trajectory", session = session)
    } else if (is.null(target$trajectory_id)) {
      # A link to an application alone opens its overview.
      bslib::nav_select("scans_app_view", "application", session = session)
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

# The review filter a link's scope asks for, as far as this app can apply
# it. Review state lives in the annotation store of whoever opens the link,
# so a part that cannot be applied is named in `notice` rather than dropped
# quietly: the cohort on screen is then wider than the one the link describes.
scans_app_scope_review <- function(scope, has_store, labels = NULL) {
  asked <- c(
    if (!is.null(scope$review)) {
      c(
        state = switch(
          scope$review,
          needs = "Needs review",
          reviewed = "Reviewed",
          paste("Review state:", scope$review)
        )
      )
    },
    if (!is.null(scope$review_label)) {
      c(label = paste("Label:", scope$review_label))
    }
  )
  review <- list()
  if (has_store && isTRUE(scope$review %in% c("needs", "reviewed"))) {
    review$state <- scope$review
  }
  if (
    has_store && isTRUE(scope$review_label %in% c(labels, scans_app_no_label))
  ) {
    review$label <- scope$review_label
  }
  dropped <- asked[setdiff(names(asked), names(review))]
  if (length(dropped)) {
    review$notice <- sprintf(
      "This link filters by review (%s), which %s. That filter is not applied, so this is a wider cohort than the link describes.",
      paste(dropped, collapse = ", "),
      if (has_store) {
        "this app's annotation store does not offer"
      } else {
        "cannot be applied because this app has no annotation store"
      }
    )
  }
  review
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
  scope_parts <- strsplit(event_parts[[1L]], "?scope=", fixed = TRUE)[[1L]]
  scope <- if (length(scope_parts) == 2L) {
    scans_app_parse_scope(scope_parts[[2L]])
  }
  hash <- scope_parts[[1L]]
  parts <- strsplit(hash, "/", fixed = TRUE)[[1L]]
  # "<application>/" names an application's overview; the slash is required
  # so that an arbitrary fragment is never read as an application.
  if (length(parts) < 2L && !endsWith(hash, "/")) {
    return(NULL)
  }
  application <- utils::URLdecode(parts[[1L]])
  trajectory_id <- utils::URLdecode(paste(parts[-1L], collapse = "/"))
  if (!application %in% labels) {
    return(NULL)
  }
  target <- list(application = application)
  if (nzchar(trajectory_id)) {
    target$trajectory_id <- trajectory_id
  }
  if (length(scope)) {
    target$scope <- scope
  }
  if (length(event_parts) == 2L) {
    target$event_id <- utils::URLdecode(event_parts[[2L]])
  }
  target
}
