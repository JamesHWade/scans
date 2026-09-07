scans_app_review_ui <- function() {
  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      "Review and create example",
      htmltools::tags$p(
        "Reviews stay in this session. Download an example to keep its evidence and decision history."
      ),
      shiny::fileInput(
        "scans_app_open_examples",
        "Open reviewed examples",
        accept = ".json"
      ),
      shiny::uiOutput("scans_app_imported_cases"),
      shiny::uiOutput("scans_app_review_finding"),
      shiny::textInput("scans_app_review_author", "Reviewer"),
      shiny::selectInput(
        "scans_app_review_judgment",
        "Judgment",
        choices = c(
          "Confirmed" = "confirmed",
          "Rejected" = "rejected",
          "Uncertain" = "uncertain",
          "Missed finding" = "missed",
          "Insufficient capture" = "insufficient_capture"
        )
      ),
      shiny::textAreaInput(
        "scans_app_review_rationale",
        "Reason for this judgment"
      ),
      shiny::textAreaInput(
        "scans_app_review_expectation",
        "Expected future behavior"
      ),
      shiny::checkboxInput(
        "scans_app_review_adjudicate",
        "Adjudicate all active decisions",
        FALSE
      ),
      shiny::actionButton("scans_app_record_review", "Record decision"),
      shiny::uiOutput("scans_app_review_history"),
      shiny::selectInput(
        "scans_app_example_purpose",
        "Use example for",
        c(
          "Application improvement" = "application",
          "Scanner validation" = "scanner"
        )
      ),
      shiny::textInput("scans_app_example_group", "Task group"),
      shiny::selectInput(
        "scans_app_example_split",
        "Use in",
        c("Development" = "development", "Held-out test" = "test")
      ),
      shiny::conditionalPanel(
        "input.scans_app_example_purpose === 'application'",
        shiny::textInput(
          "scans_app_example_input_name",
          "Input field",
          "question"
        ),
        shiny::textAreaInput("scans_app_example_input", "Task input to run"),
        shiny::textInput(
          "scans_app_example_target_name",
          "Target field",
          "answer"
        ),
        shiny::textAreaInput("scans_app_example_target", "Expected output")
      ),
      shiny::conditionalPanel(
        "input.scans_app_example_purpose === 'scanner'",
        shiny::selectInput(
          "scans_app_example_operator",
          "Expected finding count",
          c("Equals" = "eq", "At least" = "gte", "At most" = "lte")
        ),
        shiny::numericInput(
          "scans_app_example_count",
          "Count",
          1L,
          min = 0,
          step = 1
        )
      ),
      shiny::actionButton(
        "scans_app_preview_example",
        "Select and preview example"
      ),
      shiny::textOutput("scans_app_review_message")
    )
  )
}

scans_app_review_server <- function(
  input,
  output,
  session,
  data,
  active,
  application,
  scan_config,
  selected_id,
  rescanned,
  opened
) {
  cases <- shiny::reactiveVal(list())
  imported <- shiny::reactiveVal(NULL)
  current <- shiny::reactiveVal(NULL)
  pending <- shiny::reactiveVal(NULL)
  message <- shiny::reactiveVal("")
  pending_open <- shiny::reactiveVal(NULL)
  last_context <- NULL
  attempt <- function(code) {
    tryCatch(code, error = function(cnd) {
      message(conditionMessage(cnd))
      NULL
    })
  }
  output$scans_app_review_message <- shiny::renderText(message())
  output$scans_app_review_finding <- shiny::renderUI({
    d <- data()
    shiny::req(d, selected_id())
    findings <- d$findings[
      d$findings$trajectory_id == selected_id(),
      ,
      drop = FALSE
    ]
    choices <- c("Whole trajectory / missed finding" = "")
    if (nrow(findings)) {
      choices <- c(
        choices,
        stats::setNames(
          findings$finding_id,
          paste(findings$scan, findings$finding_id)
        )
      )
    }
    htmltools::tagList(
      shiny::selectInput(
        "scans_app_review_finding_id",
        "Review evidence",
        choices
      ),
      shiny::selectInput(
        "scans_app_review_scan",
        "Scanner for whole-trajectory review",
        c(
          "Application only" = "",
          stats::setNames(scan_config()$scans, scan_config()$scans)
        )
      )
    )
  })
  shiny::observeEvent(
    list(selected_id(), application(), scan_config(), active(), pending_open()),
    {
      context <- list(
        id = selected_id(),
        application = application(),
        settings = scan_config(),
        revision = active()$investigation$manifest$revision_id,
        loaded_at = active()$loaded_at
      )
      if (!identical(context, last_context)) {
        current(NULL)
        pending(NULL)
        message("")
        shiny::updateTextAreaInput(
          session,
          "scans_app_review_rationale",
          value = ""
        )
        shiny::updateTextAreaInput(
          session,
          "scans_app_review_expectation",
          value = ""
        )
        last_context <<- context
      }
      restore <- pending_open()
      if (
        !is.null(restore) &&
          identical(context$revision, restore$evidence$manifest$revision_id) &&
          identical(context$id, restore$trajectory_id)
      ) {
        current(restore)
        all <- cases()
        all[[restore$case_id]] <- restore
        cases(all)
        shiny::updateSelectInput(
          session,
          "scans_app_review_finding_id",
          selected = restore$finding_id %||% ""
        )
        shiny::updateSelectInput(
          session,
          "scans_app_review_scan",
          selected = restore$scan %||% ""
        )
        pending_open(NULL)
      }
    },
    priority = -10
  )
  shiny::observeEvent(input$scans_app_record_review, {
    result <- attempt({
      shiny::req(selected_id(), data())
      entry <- active()
      saved <- entry$investigation
      if (is.null(saved) || rescanned()) {
        saved <- investigation_build(
          entry$bundle,
          NULL,
          application(),
          list(read_info = entry$read_info, loaded_at = entry$loaded_at),
          scan_config(),
          list(),
          data()[c("summaries", "findings", "assessments", "measures")],
          previous = saved
        )
      }
      finding <- input$scans_app_review_finding_id %||% ""
      scan <- input$scans_app_review_scan %||% ""
      case <- current()
      if (
        !is.null(case) &&
          (!identical(case$trajectory_id, selected_id()) ||
            !saved$manifest$revision_id %in%
              c(case$origin$revision_id, case$evidence$manifest$revision_id))
      ) {
        case <- NULL
      }
      if (
        !is.null(case) &&
          (!identical(case$finding_id %||% "", finding) ||
            (is.null(case$finding_id) && !identical(case$scan %||% "", scan)))
      ) {
        case <- NULL
      }
      if (is.null(case)) {
        case <- review_case(
          saved,
          selected_id(),
          finding_id = if (nzchar(finding)) finding,
          scan = if (!nzchar(finding) && nzchar(scan)) scan
        )
        previous <- cases()[[case$case_id]]
        if (!is.null(previous)) case <- previous
      }
      expectation <- input$scans_app_review_expectation %||% ""
      review_decision(
        case,
        input$scans_app_review_judgment,
        input$scans_app_review_author,
        input$scans_app_review_rationale,
        expected_behavior = if (nzchar(trimws(expectation))) expectation,
        supersedes = if (isTRUE(input$scans_app_review_adjudicate)) {
          review_status(case)$decision_ids
        } else {
          character()
        }
      )
    })
    if (!is.null(result)) {
      all <- cases()
      all[[result$case_id]] <- result
      cases(all)
      current(result)
      pending(NULL)
      message(
        "Decision recorded in this session. Select and download an example to keep it."
      )
    }
  })
  output$scans_app_review_history <- shiny::renderUI({
    case <- current()
    if (is.null(case)) {
      return(NULL)
    }
    htmltools::tagList(
      htmltools::tags$strong(paste("Review:", review_status(case)$status)),
      lapply(case$decisions, function(d) {
        htmltools::tags$p(
          paste(d$reviewer, d$judgment, d$rationale, sep = " \u00b7 ")
        )
      })
    )
  })
  shiny::observeEvent(input$scans_app_preview_example, {
    result <- attempt({
      case <- current()
      if (is.null(case)) {
        review_abort("Record or open a review before selecting an example.")
      }
      purpose <- input$scans_app_example_purpose
      inputs <- targets <- predicates <- NULL
      if (identical(purpose, "application")) {
        review_string(input$scans_app_example_input_name, "Input field")
        review_string(input$scans_app_example_target_name, "Target field")
        review_string(input$scans_app_example_input, "Task input")
        review_string(input$scans_app_example_target, "Expected output")
        inputs <- list(stats::setNames(
          list(input$scans_app_example_input),
          input$scans_app_example_input_name
        ))
        targets <- list(stats::setNames(
          list(input$scans_app_example_target),
          input$scans_app_example_target_name
        ))
      } else {
        predicates <- list(list(
          operator = input$scans_app_example_operator,
          value = input$scans_app_example_count
        ))
      }
      prior <- imported()
      if (!is.null(prior) && prior$purpose != purpose) {
        review_abort(
          "Keep the imported set's purpose when creating a revision."
        )
      }
      review_examples(
        list(case),
        purpose,
        input$scans_app_example_group,
        input$scans_app_example_split,
        inputs,
        targets,
        predicates,
        previous = prior
      )
    })
    if (!is.null(result)) {
      pending(result)
      message("")
      shiny::showModal(scans_app_review_preview_ui(result), session = session)
    }
  })
  output$scans_app_download_examples <- shiny::downloadHandler(
    filename = function() {
      paste0("scans-example-", substr(pending()$version_id, 8, 19), ".json")
    },
    content = function(file) {
      shiny::req(pending())
      write_review_examples(pending(), file, overwrite = TRUE)
    }
  )
  shiny::observeEvent(input$scans_app_open_examples, {
    result <- attempt(read_review_examples(
      input$scans_app_open_examples$datapath,
      max_bytes = scans_app_investigation_max_bytes()
    ))
    if (!is.null(result)) {
      imported(result)
      pending(NULL)
      message("Choose an imported case to inspect its evidence and history.")
    }
  })
  output$scans_app_imported_cases <- shiny::renderUI({
    x <- imported()
    if (is.null(x)) {
      return(NULL)
    }
    htmltools::tagList(
      shiny::selectInput(
        "scans_app_imported_case",
        "Imported case",
        stats::setNames(
          seq_along(x$cases),
          vapply(x$cases, `[[`, "", "trajectory_id")
        )
      ),
      shiny::actionButton(
        "scans_app_inspect_imported_case",
        "Inspect imported case"
      ),
      shiny::actionButton("scans_app_close_examples", "Return to application")
    )
  })
  shiny::observeEvent(input$scans_app_close_examples, {
    pending_open(NULL)
    imported(NULL)
    current(NULL)
    pending(NULL)
    opened(NULL)
  })
  shiny::observeEvent(input$scans_app_inspect_imported_case, {
    x <- imported()
    i <- suppressWarnings(as.integer(input$scans_app_imported_case))
    shiny::req(x, length(i) == 1L, !is.na(i), i >= 1L, i <= length(x$cases))
    case <- structure(x$cases[[i]], class = "scans_review_case")
    opened(review_evidence(case$evidence))
    for (kind in c("input", "target")) {
      fields <- x[[paste0(kind, "s")]][[i]]
      editable <- length(fields) == 1L &&
        rlang::is_string(fields[[1L]]) &&
        !is.na(fields[[1L]])
      shiny::updateTextInput(
        session,
        paste0("scans_app_example_", kind, "_name"),
        value = if (editable) names(fields)[[1L]] else ""
      )
      shiny::updateTextAreaInput(
        session,
        paste0("scans_app_example_", kind),
        value = if (editable) fields[[1L]] else ""
      )
    }
    if (x$purpose == "scanner") {
      shiny::updateSelectInput(
        session,
        "scans_app_example_operator",
        selected = x$predicates[[i]]$operator
      )
      shiny::updateNumericInput(
        session,
        "scans_app_example_count",
        value = x$predicates[[i]]$value
      )
    }
    pending_open(case)
    shiny::updateSelectInput(
      session,
      "scans_app_example_purpose",
      selected = x$purpose
    )
    shiny::updateTextInput(
      session,
      "scans_app_example_group",
      value = x$groups[[i]]
    )
    shiny::updateSelectInput(
      session,
      "scans_app_example_split",
      selected = x$splits[[i]]
    )
  })
  list(cases = cases, current = current, pending = pending, imported = imported)
}

scans_app_review_preview_ui <- function(x) {
  case <- x$cases[[1L]]
  evidence <- review_evidence(case$evidence)
  fields_ui <- function(title, fields) {
    if (is.null(fields)) {
      return(NULL)
    }
    htmltools::tagList(
      htmltools::tags$h4(title),
      lapply(names(fields), function(name) {
        htmltools::tagList(
          htmltools::tags$strong(name),
          htmltools::tags$pre(
            style = "white-space: pre-wrap; overflow-wrap: anywhere;",
            if (is.character(fields[[name]])) {
              paste(fields[[name]], collapse = "\n")
            } else {
              as.character(jsonlite::toJSON(
                fields[[name]],
                auto_unbox = TRUE,
                pretty = TRUE
              ))
            }
          )
        )
      })
    )
  }
  shiny::modalDialog(
    title = "Selected example preview",
    htmltools::tags$p(paste("Trajectory:", case$trajectory_id)),
    htmltools::tags$p(paste(
      "Purpose:",
      x$purpose,
      "\u00b7 Group:",
      x$groups,
      "\u00b7 Split:",
      x$splits
    )),
    htmltools::tags$p(paste(
      nrow(trajectory_turns(evidence$bundle)),
      "turns,",
      nrow(trajectory_events(evidence$bundle)),
      "events, and",
      nrow(trajectory_losses(evidence$bundle)),
      "loss records will be included."
    )),
    htmltools::tags$p(
      "Includes retained text, tool arguments and results, capture metadata, and complete review history. Existing redactions remain; no additional anonymization is performed."
    ),
    fields_ui("Task input", x$inputs[[1L]]),
    fields_ui("Expected output", x$targets[[1L]]),
    if (!is.null(x$predicates)) {
      htmltools::tags$p(paste(
        "Finding count:",
        x$predicates[[1L]]$operator,
        x$predicates[[1L]]$value
      ))
    },
    htmltools::tags$h4("Decision history"),
    lapply(case$decisions, function(d) {
      htmltools::tags$div(
        htmltools::tags$strong(paste(d$reviewer, d$judgment, sep = " \u00b7 ")),
        htmltools::tags$p(d$rationale),
        if (!is.null(d$expected_behavior)) {
          htmltools::tags$p(paste("Expected behavior:", d$expected_behavior))
        }
      )
    }),
    shiny::downloadButton(
      "scans_app_download_examples",
      "Download example JSON"
    ),
    footer = shiny::modalButton("Close"),
    size = "l"
  )
}
