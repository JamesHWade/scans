#' Explore trajectory diagnostics in Scout
#'
#' `scout()` launches a read-only Shiny app for exploring a
#' [TrajectoryBundle]. Scout keeps the canonical bundle as its data boundary:
#' it does not call a model, run tools, modify the bundle, or infer missing
#' source facts.
#'
#' The app provides filters and a trajectory browser, a source-neutral
#' transcript and event stream, and the findings, evaluations, and adapter
#' losses associated with the selected trajectory. Built-in findings are
#' computed once with [scan_trajectories()] when the app is created.
#'
#' @param x A [TrajectoryBundle] containing one or more completed trajectory
#'   snapshots.
#'
#' @returns A [shiny::shinyApp()] object. Calling `scout()` at the console
#'   launches the app; the returned object can also be served from an `app.R`.
#'
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(
#'     trajectory_id = "trajectory-1",
#'     source_type = "manual"
#'   ),
#'   data.frame(),
#'   data.frame()
#' )
#'
#' if (interactive()) {
#'   scout(bundle)
#' }
#' @export
scout <- function(x) {
  check_trajectory_bundle(x)
  scout_check_packages()
  data <- scout_data(x)

  shiny::shinyApp(
    ui = scout_ui(data),
    server = scout_server(data)
  )
}

scout_check_packages <- function(call = rlang::caller_env()) {
  packages <- c("bslib", "htmltools", "shiny")
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) == 0L) {
    return(invisible(packages))
  }

  scans_abort(
    c(
      "{.fn scout} requires missing package{?s}: {.pkg {missing}}.",
      "i" = "Install {.pkg {missing}} to use the Scout app."
    ),
    class = "scans_error_scout_dependency",
    call = call,
    .envir = environment()
  )
}

scout_data <- function(x) {
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  evaluations <- trajectory_evaluations(x)
  losses <- trajectory_losses(x)
  findings <- scan_trajectories(x)
  summaries <- summarize_trajectories(x)

  list(
    info = info,
    turns = turns,
    events = events,
    evaluations = evaluations,
    losses = losses,
    findings = findings,
    summaries = summaries,
    records = scout_records(
      info,
      turns,
      events,
      findings,
      summaries
    )
  )
}

scout_records <- function(info, turns, events, findings, summaries) {
  if (nrow(info) == 0L) {
    return(tibble::tibble(
      index = integer(),
      trajectory_id = character(),
      source_type = character(),
      status = character(),
      title = character(),
      search = character(),
      n_turns = integer(),
      n_events = integer(),
      n_findings = integer(),
      n_errors = integer()
    ))
  }

  ids <- info$trajectory_id
  snippets <- scout_trajectory_snippets(ids, turns, events)
  titles <- vapply(
    seq_along(ids),
    function(index) {
      scout_first_string(
        snippets[[index]],
        info$agent[[index]],
        info$model[[index]],
        info$source_id[[index]],
        info$run_id[[index]],
        ids[[index]]
      )
    },
    character(1)
  )
  finding_groups <- match(findings$trajectory_id, ids)
  n_findings <- tabulate(finding_groups, nbins = length(ids))
  n_errors <- tabulate(
    finding_groups[findings$severity %in% "error"],
    nbins = length(ids)
  )
  search <- vapply(
    seq_along(ids),
    function(index) {
      paste(
        ids[[index]],
        info$run_id[[index]],
        info$source_type[[index]],
        info$source_id[[index]],
        info$agent[[index]],
        info$model[[index]],
        titles[[index]]
      )
    },
    character(1)
  )

  tibble::tibble(
    index = seq_along(ids),
    trajectory_id = ids,
    source_type = info$source_type,
    status = info$status,
    title = titles,
    search = tolower(search),
    n_turns = summaries$n_turns,
    n_events = summaries$n_events,
    n_findings = n_findings,
    n_errors = n_errors
  )
}

scout_trajectory_snippets <- function(ids, turns, events) {
  roles <- turns$role[match(events$turn_id, turns$turn_id)]
  vapply(
    ids,
    function(id) {
      rows <- which(
        events$trajectory_id == id &
          !is.na(events$text) &
          nzchar(trimws(events$text))
      )
      if (length(rows) == 0L) {
        return(NA_character_)
      }
      user_rows <- rows[roles[rows] %in% "user"]
      if (length(user_rows) > 0L) {
        rows <- user_rows
      }
      scout_truncate(gsub("\\s+", " ", trimws(events$text[[rows[[1L]]]])), 90L)
    },
    character(1)
  )
}

scout_first_string <- function(...) {
  values <- list(...)
  for (value in values) {
    if (
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    ) {
      return(value)
    }
  }
  "Untitled trajectory"
}

scout_filter_records <- function(
  records,
  source = "all",
  status = "all",
  query = "",
  findings_only = FALSE
) {
  keep <- rep(TRUE, nrow(records))
  if (!identical(source, "all")) {
    keep <- keep & records$source_type %in% source
  }
  if (identical(status, "missing")) {
    keep <- keep & is.na(records$status)
  } else if (!identical(status, "all")) {
    keep <- keep & records$status %in% status
  }
  query <- trimws(tolower(query))
  if (nzchar(query)) {
    keep <- keep & grepl(query, records$search, fixed = TRUE)
  }
  if (isTRUE(findings_only)) {
    keep <- keep & records$n_findings > 0L
  }
  records$index[keep]
}

scout_ui <- function(data) {
  sources <- sort(unique(data$records$source_type))
  sources <- sources[!is.na(sources) & nzchar(sources)]
  statuses <- sort(unique(data$records$status))
  statuses <- statuses[!is.na(statuses) & nzchar(statuses)]
  status_choices <- c("All statuses" = "all")
  if (anyNA(data$records$status)) {
    status_choices <- c(status_choices, "Unknown" = "missing")
  }
  status_choices <- c(status_choices, stats::setNames(statuses, statuses))

  page <- bslib::page_sidebar(
    title = htmltools::div(
      class = "scans-scout-brand",
      htmltools::tags$span("Scout"),
      htmltools::tags$small("Trajectory diagnostics")
    ),
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f6f7fb",
      fg = "#1e2430",
      primary = "#5356c9"
    ),
    fillable = TRUE,
    class = "bslib-page-dashboard scans-scout",
    sidebar = bslib::sidebar(
      title = "Trajectories",
      width = 370,
      class = "scans-scout-browser",
      shiny::textInput(
        "scout_query",
        "Search",
        placeholder = "ID, source, agent, or transcript",
        width = "100%"
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::selectInput(
          "scout_source",
          "Source",
          choices = c("All sources" = "all", sources),
          width = "100%"
        ),
        shiny::selectInput(
          "scout_status",
          "Status",
          choices = status_choices,
          width = "100%"
        )
      ),
      bslib::input_switch(
        "scout_findings_only",
        "Only trajectories with findings"
      ),
      htmltools::div(
        class = "scans-scout-browser-count",
        shiny::textOutput("scout_visible_count", inline = TRUE)
      ),
      htmltools::div(
        class = "scans-scout-browser-entries",
        shiny::uiOutput("scout_entries")
      )
    ),
    shiny::uiOutput("scout_overview"),
    bslib::card(
      fill = TRUE,
      full_screen = TRUE,
      class = "scans-scout-workspace",
      bslib::card_header(
        class = "scans-scout-workspace-header",
        shiny::uiOutput("scout_header")
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
          class = "scans-scout-evidence",
          shiny::uiOutput("scout_evidence")
        ),
        htmltools::tags$main(
          class = "scans-scout-transcript",
          shiny::uiOutput("scout_transcript")
        )
      )
    )
  )
  htmltools::attachDependencies(page, scout_dependency())
}

scout_server <- function(data) {
  function(input, output, session) {
    initial <- if (nrow(data$records) == 0L) NULL else 1L
    selected <- shiny::reactiveVal(initial)
    visible <- shiny::reactive({
      scout_filter_records(
        data$records,
        source = scout_input_or(input$scout_source, "all"),
        status = scout_input_or(input$scout_status, "all"),
        query = scout_input_or(input$scout_query, ""),
        findings_only = isTRUE(input$scout_findings_only)
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

    if (nrow(data$records) > 0L) {
      for (index in seq_len(nrow(data$records))) {
        local({
          entry <- index
          shiny::observeEvent(input[[scout_entry_id(entry)]], {
            selected(entry)
          })
        })
      }
    }

    output$scout_visible_count <- shiny::renderText({
      count <- length(visible())
      total <- nrow(data$records)
      sprintf(
        "%d of %d %s",
        count,
        total,
        if (total == 1L) "trajectory" else "trajectories"
      )
    })

    output$scout_entries <- shiny::renderUI({
      indices <- visible()
      if (length(indices) == 0L) {
        return(scout_empty_ui(
          if (nrow(data$records) == 0L) {
            "This bundle has no trajectories."
          } else {
            "No trajectories match these filters."
          }
        ))
      }
      htmltools::tagList(lapply(indices, function(index) {
        scout_entry_ui(
          data$records[index, , drop = FALSE],
          selected = identical(selected(), index)
        )
      }))
    })

    output$scout_overview <- shiny::renderUI({
      scout_overview_ui(data, selected())
    })
    output$scout_header <- shiny::renderUI({
      scout_header_ui(data, selected())
    })
    output$scout_transcript <- shiny::renderUI({
      scout_transcript_ui(data, selected())
    })
    output$scout_evidence <- shiny::renderUI({
      scout_evidence_ui(data, selected())
    })
  }
}

scout_input_or <- function(value, default) {
  if (is.null(value) || length(value) == 0L) default else value
}

scout_entry_id <- function(index) {
  paste0("scout_entry_", index)
}

scout_entry_ui <- function(record, selected) {
  findings <- record$n_findings[[1L]]
  errors <- record$n_errors[[1L]]
  tone <- if (errors > 0L) {
    "danger"
  } else if (findings > 0L) {
    "warning"
  } else {
    "quiet"
  }

  shiny::actionLink(
    scout_entry_id(record$index[[1L]]),
    label = htmltools::tagList(
      htmltools::div(
        class = "scans-scout-entry-heading",
        htmltools::tags$span(
          class = "scans-scout-entry-title",
          record$title[[1L]]
        ),
        scout_badge(as.character(findings), tone)
      ),
      htmltools::div(
        class = "scans-scout-entry-id",
        record$trajectory_id[[1L]]
      ),
      htmltools::div(
        class = "scans-scout-entry-meta",
        scout_badge(record$source_type[[1L]], "source"),
        scout_status_badge(record$status[[1L]]),
        htmltools::tags$span(
          sprintf(
            "%d turns \u00b7 %d events",
            record$n_turns[[1L]],
            record$n_events[[1L]]
          )
        )
      )
    ),
    class = paste(
      "scans-scout-entry",
      if (selected) "scans-scout-entry-selected" else ""
    ),
    `aria-current` = if (selected) "true" else NULL
  )
}

scout_overview_ui <- function(data, index) {
  if (is.null(index)) {
    return(NULL)
  }
  id <- data$info$trajectory_id[[index]]
  summary <- data$summaries[index, , drop = FALSE]
  counts <- c(
    Turns = summary$n_turns[[1L]],
    Events = summary$n_events[[1L]],
    Findings = sum(data$findings$trajectory_id == id),
    Evaluations = sum(data$evaluations$trajectory_id == id),
    Losses = sum(data$losses$trajectory_id == id)
  )
  themes <- c("primary", "primary", "warning", "success", "secondary")

  bslib::layout_column_wrap(
    width = "145px",
    fill = FALSE,
    class = "scans-scout-overview",
    !!!Map(
      function(title, value, theme) {
        bslib::value_box(
          title = title,
          value = format(value, big.mark = ",", scientific = FALSE),
          theme = theme,
          class = "scans-scout-value"
        )
      },
      names(counts),
      counts,
      themes
    )
  )
}

scout_header_ui <- function(data, index) {
  if (is.null(index)) {
    return(htmltools::div(
      class = "scans-scout-heading",
      htmltools::tags$strong("No trajectory selected")
    ))
  }
  info <- data$info[index, , drop = FALSE]
  record <- data$records[index, , drop = FALSE]
  htmltools::div(
    class = "scans-scout-heading",
    htmltools::div(
      htmltools::tags$strong(record$title[[1L]]),
      htmltools::tags$span(
        class = "scans-scout-heading-id",
        info$trajectory_id[[1L]]
      )
    ),
    htmltools::div(
      class = "scans-scout-heading-badges",
      scout_badge(info$source_type[[1L]], "source"),
      scout_status_badge(info$status[[1L]])
    )
  )
}

scout_transcript_ui <- function(data, index) {
  if (is.null(index)) {
    return(scout_empty_ui("Select a trajectory to inspect its path."))
  }
  id <- data$info$trajectory_id[[index]]
  turn_rows <- which(data$turns$trajectory_id == id)
  turn_rows <- turn_rows[
    order(
      data$turns$turn_index[turn_rows],
      data$turns$turn_id[turn_rows],
      method = "radix"
    )
  ]
  event_rows <- which(data$events$trajectory_id == id)
  event_rows <- event_rows[
    order(
      data$events$event_index[event_rows],
      data$events$event_id[event_rows],
      method = "radix"
    )
  ]

  if (length(turn_rows) == 0L && length(event_rows) == 0L) {
    return(scout_empty_ui("This trajectory has no turns or events."))
  }

  blocks <- list()
  known_turn_ids <- data$turns$turn_id[turn_rows]
  run_events <- event_rows[
    is.na(data$events$turn_id[event_rows]) |
      !data$events$turn_id[event_rows] %in% known_turn_ids
  ]
  if (length(run_events) > 0L) {
    blocks <- append(
      blocks,
      list(scout_event_group_ui(
        if (length(turn_rows) == 0L) "Event stream" else "Run events",
        data$events,
        run_events
      ))
    )
  }
  for (turn_row in turn_rows) {
    events <- event_rows[
      data$events$turn_id[event_rows] == data$turns$turn_id[[turn_row]] &
        !is.na(data$events$turn_id[event_rows])
    ]
    blocks <- append(
      blocks,
      list(scout_turn_ui(
        data$turns[turn_row, , drop = FALSE],
        data$events,
        events
      ))
    )
  }

  htmltools::div(class = "scans-scout-path", htmltools::tagList(blocks))
}

scout_event_group_ui <- function(title, events, rows) {
  htmltools::tags$section(
    class = "scans-scout-event-group",
    htmltools::tags$h2(title),
    htmltools::tagList(lapply(rows, function(row) {
      scout_event_ui(events[row, , drop = FALSE], row)
    }))
  )
}

scout_turn_ui <- function(turn, events, rows) {
  role <- scout_first_string(turn$role[[1L]], "unknown")
  title <- paste0(
    scout_title_case(role),
    " \u00b7 Turn ",
    turn$turn_index[[1L]]
  )
  if (!is.na(turn$round_index[[1L]])) {
    title <- paste0(title, " \u00b7 Round ", turn$round_index[[1L]])
  }
  body <- if (length(rows) == 0L) {
    scout_empty_ui("No events were recorded for this turn.", compact = TRUE)
  } else {
    htmltools::tagList(lapply(rows, function(row) {
      scout_event_ui(events[row, , drop = FALSE], row)
    }))
  }

  htmltools::tags$section(
    class = paste0("scans-scout-turn scans-scout-turn-", scout_css_token(role)),
    htmltools::div(
      class = "scans-scout-turn-header",
      htmltools::tags$h2(title),
      scout_status_badge(turn$status[[1L]])
    ),
    if (scout_has_string(turn$error[[1L]])) {
      htmltools::div(class = "scans-scout-turn-error", turn$error[[1L]])
    },
    body
  )
}

scout_event_ui <- function(event, row) {
  type <- scout_first_string(event$event_type[[1L]], "event")
  name <- event$name[[1L]]
  heading <- switch(
    type,
    content = scout_title_case(
      scout_first_string(event$content_type[[1L]], "content")
    ),
    tool_call = "Tool call",
    tool_result = "Tool result",
    error = "Error",
    scout_title_case(gsub("[:_]", " ", type))
  )
  if (scout_has_string(name)) {
    heading <- paste(heading, name, sep = " \u00b7 ")
  }
  meta <- scout_event_meta(event)
  value <- scout_value_text(event$value[[1L]])

  htmltools::tags$article(
    id = scout_event_dom_id(row),
    tabindex = "-1",
    class = paste(
      "scans-scout-event",
      paste0("scans-scout-event-", scout_css_token(type)),
      if (scout_has_string(event$error[[1L]])) {
        "scans-scout-event-failed"
      } else {
        ""
      }
    ),
    htmltools::div(
      class = "scans-scout-event-header",
      htmltools::tags$strong(heading),
      scout_status_badge(event$status[[1L]])
    ),
    if (length(meta) > 0L) {
      htmltools::div(class = "scans-scout-event-meta", meta)
    },
    if (scout_has_string(event$text[[1L]])) {
      htmltools::div(class = "scans-scout-event-text", event$text[[1L]])
    },
    if (!is.null(value)) {
      htmltools::tags$pre(
        class = "scans-scout-event-value",
        htmltools::tags$code(value)
      )
    },
    if (scout_has_string(event$error[[1L]])) {
      htmltools::div(
        class = "scans-scout-event-error",
        event$error[[1L]]
      )
    }
  )
}

scout_event_meta <- function(event) {
  values <- character()
  if (scout_has_string(event$call_id[[1L]])) {
    values <- c(values, paste("Call", event$call_id[[1L]]))
  }
  if (!is.na(event$timestamp[[1L]])) {
    values <- c(
      values,
      format(event$timestamp[[1L]], "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
    )
  }
  if (!is.na(event$duration[[1L]])) {
    values <- c(values, sprintf("%.3f s", event$duration[[1L]]))
  }
  if (length(values) == 0L) {
    return(NULL)
  }
  htmltools::tagList(Map(
    function(value, index) {
      htmltools::tagList(
        if (index > 1L) htmltools::tags$span("\u00b7"),
        htmltools::tags$span(value)
      )
    },
    values,
    seq_along(values)
  ))
}

scout_evidence_ui <- function(data, index) {
  if (is.null(index)) {
    return(scout_empty_ui("Select a trajectory to inspect its evidence."))
  }
  id <- data$info$trajectory_id[[index]]
  finding_rows <- which(data$findings$trajectory_id == id)
  evaluation_rows <- which(data$evaluations$trajectory_id == id)
  loss_rows <- which(data$losses$trajectory_id == id)
  open <- c(
    if (length(finding_rows) > 0L) "findings",
    if (length(loss_rows) > 0L) "losses"
  )
  if (length(open) == 0L) {
    open <- "context"
  }

  bslib::accordion(
    open = open,
    multiple = TRUE,
    class = "scans-scout-evidence-accordion",
    bslib::accordion_panel(
      paste0("Findings (", length(finding_rows), ")"),
      value = "findings",
      scout_findings_ui(data, finding_rows)
    ),
    bslib::accordion_panel(
      paste0("Evaluations (", length(evaluation_rows), ")"),
      value = "evaluations",
      scout_evaluations_ui(data$evaluations, evaluation_rows)
    ),
    bslib::accordion_panel(
      paste0("Losses (", length(loss_rows), ")"),
      value = "losses",
      scout_losses_ui(data, loss_rows)
    ),
    bslib::accordion_panel(
      "Context",
      value = "context",
      scout_context_ui(data$info[index, , drop = FALSE])
    )
  )
}

scout_findings_ui <- function(data, rows) {
  if (length(rows) == 0L) {
    return(scout_empty_ui(
      "Built-in scans found no diagnostic issues.",
      compact = TRUE
    ))
  }
  htmltools::tagList(lapply(rows, function(row) {
    finding <- data$findings[row, , drop = FALSE]
    evidence <- finding$event_ids[[1L]]
    if (length(evidence) == 0L && scout_has_string(finding$event_id[[1L]])) {
      evidence <- finding$event_id[[1L]]
    }
    links <- scout_event_links(evidence, data$events)
    htmltools::tags$article(
      class = paste0(
        "scans-scout-finding scans-scout-finding-",
        scout_css_token(finding$severity[[1L]])
      ),
      htmltools::div(
        class = "scans-scout-finding-header",
        scout_badge(
          scout_title_case(finding$severity[[1L]]),
          finding$severity[[1L]]
        ),
        htmltools::tags$strong(
          scout_first_string(finding$label[[1L]], finding$scan[[1L]])
        )
      ),
      if (scout_has_string(finding$explanation[[1L]])) {
        htmltools::tags$p(finding$explanation[[1L]])
      },
      if (length(links) > 0L) {
        htmltools::div(
          class = "scans-scout-evidence-links",
          htmltools::tags$span("Evidence"),
          links
        )
      }
    )
  }))
}

scout_event_links <- function(ids, events) {
  ids <- ids[!is.na(ids) & nzchar(ids)]
  rows <- match(ids, events$event_id)
  keep <- !is.na(rows)
  if (!any(keep)) {
    return(NULL)
  }
  htmltools::tagList(Map(
    function(id, row) {
      htmltools::tags$a(
        href = paste0("#", scout_event_dom_id(row)),
        scout_truncate(id, 28L)
      )
    },
    ids[keep],
    rows[keep]
  ))
}

scout_evaluations_ui <- function(evaluations, rows) {
  if (length(rows) == 0L) {
    return(scout_empty_ui("No evaluations are joined.", compact = TRUE))
  }
  htmltools::tagList(lapply(rows, function(row) {
    evaluation <- evaluations[row, , drop = FALSE]
    htmltools::tags$article(
      class = "scans-scout-evaluation",
      htmltools::div(
        class = "scans-scout-evidence-item-header",
        htmltools::tags$strong(scout_first_string(
          evaluation$scorer[[1L]],
          evaluation$evaluation_id[[1L]]
        ))
      ),
      scout_labeled_value("Value", evaluation$value[[1L]]),
      scout_labeled_value("Target", evaluation$target[[1L]]),
      if (scout_has_string(evaluation$explanation[[1L]])) {
        htmltools::tags$p(evaluation$explanation[[1L]])
      }
    )
  }))
}

scout_losses_ui <- function(data, rows) {
  if (length(rows) == 0L) {
    return(scout_empty_ui("No adapter losses are recorded.", compact = TRUE))
  }
  htmltools::tagList(lapply(rows, function(row) {
    loss <- data$losses[row, , drop = FALSE]
    links <- scout_event_links(loss$event_id[[1L]], data$events)
    htmltools::tags$article(
      class = "scans-scout-loss",
      htmltools::div(
        class = "scans-scout-evidence-item-header",
        scout_badge(scout_title_case(loss$reason[[1L]]), "quiet"),
        htmltools::tags$strong(loss$field[[1L]])
      ),
      htmltools::tags$p(loss$detail[[1L]]),
      if (length(links) > 0L) {
        htmltools::div(class = "scans-scout-evidence-links", links)
      }
    )
  }))
}

scout_context_ui <- function(info) {
  fields <- c(
    "Trajectory" = info$trajectory_id[[1L]],
    "Run" = info$run_id[[1L]],
    "Parent" = info$parent_trajectory_id[[1L]],
    "Source" = info$source_type[[1L]],
    "Source ID" = info$source_id[[1L]],
    "Agent" = info$agent[[1L]],
    "Model" = info$model[[1L]],
    "Started" = scout_time_string(info$started_at[[1L]]),
    "Completed" = scout_time_string(info$completed_at[[1L]])
  )
  fields <- fields[!is.na(fields) & nzchar(fields)]
  if (length(fields) == 0L) {
    return(scout_empty_ui("No additional context is recorded.", compact = TRUE))
  }
  htmltools::tags$dl(
    class = "scans-scout-context",
    htmltools::tagList(Map(
      function(label, value) {
        htmltools::tagList(
          htmltools::tags$dt(label),
          htmltools::tags$dd(value)
        )
      },
      names(fields),
      fields
    ))
  )
}

scout_labeled_value <- function(label, value) {
  text <- scout_value_text(value)
  if (is.null(text)) {
    return(NULL)
  }
  htmltools::div(
    class = "scans-scout-labeled-value",
    htmltools::tags$strong(label),
    htmltools::tags$pre(htmltools::tags$code(text))
  )
}

scout_badge <- function(text, tone = "quiet") {
  if (!scout_has_string(text)) {
    return(NULL)
  }
  htmltools::tags$span(
    class = paste0(
      "scans-scout-badge scans-scout-badge-",
      scout_css_token(tone)
    ),
    text
  )
}

scout_status_badge <- function(status) {
  if (!scout_has_string(status)) {
    return(NULL)
  }
  tone <- if (status %in% c("completed", "succeeded", "success", "passed")) {
    "success"
  } else if (status %in% c("failed", "error")) {
    "danger"
  } else {
    "quiet"
  }
  scout_badge(scout_title_case(status), tone)
}

scout_empty_ui <- function(text, compact = FALSE) {
  htmltools::div(
    class = paste(
      "scans-scout-empty",
      if (compact) "scans-scout-empty-compact" else ""
    ),
    text
  )
}

scout_value_text <- function(value, max_chars = 4000L) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }
  text <- paste(
    utils::capture.output(
      utils::str(
        value,
        max.level = 4L,
        give.attr = FALSE,
        strict.width = "cut",
        width = 80L
      )
    ),
    collapse = "\n"
  )
  scout_truncate(text, max_chars)
}

scout_truncate <- function(text, max_chars) {
  if (is.na(text) || nchar(text) <= max_chars) {
    return(text)
  }
  paste0(substr(text, 1L, max_chars - 1L), "\u2026")
}

scout_has_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

scout_title_case <- function(x) {
  if (!scout_has_string(x)) {
    return("")
  }
  x <- gsub("_", " ", x)
  paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

scout_css_token <- function(x) {
  token <- tolower(scout_first_string(x, "quiet"))
  gsub("[^a-z0-9-]+", "-", token)
}

scout_event_dom_id <- function(row) {
  paste0("scout-event-", row)
}

scout_time_string <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

scout_dependency <- function() {
  htmltools::htmlDependency(
    name = "scans-scout",
    version = "0.0.0.9000",
    src = c(
      file = system.file("www", "scout", package = "scans")
    ),
    stylesheet = "scout.css"
  )
}
