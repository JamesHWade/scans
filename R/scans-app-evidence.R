scans_app_evidence_ui <- function(data, index) {
  if (is.null(index)) {
    return(scans_app_empty_ui("Select a trajectory to inspect its evidence."))
  }
  id <- data$info$trajectory_id[[index]]
  finding_rows <- which(data$findings$trajectory_id == id)
  evaluation_rows <- which(data$evaluations$trajectory_id == id)
  loss_rows <- which(data$loss_trajectory_ids %in% id)
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
    class = "scans-app-evidence-accordion",
    bslib::accordion_panel(
      paste0("Findings (", length(finding_rows), ")"),
      value = "findings",
      scans_app_findings_ui(data, finding_rows)
    ),
    bslib::accordion_panel(
      paste0("Evaluations (", length(evaluation_rows), ")"),
      value = "evaluations",
      scans_app_evaluations_ui(data$evaluations, evaluation_rows)
    ),
    bslib::accordion_panel(
      paste0("Losses (", length(loss_rows), ")"),
      value = "losses",
      scans_app_losses_ui(data, loss_rows)
    ),
    bslib::accordion_panel(
      "Context",
      value = "context",
      scans_app_context_ui(data$info[index, , drop = FALSE])
    )
  )
}

scans_app_findings_ui <- function(data, rows) {
  if (length(rows) == 0L) {
    return(scans_app_empty_ui(
      "Built-in scans found no diagnostic issues.",
      compact = TRUE
    ))
  }
  htmltools::tagList(lapply(rows, function(row) {
    finding <- data$findings[row, , drop = FALSE]
    evidence <- finding$event_ids[[1L]]
    if (
      length(evidence) == 0L && scans_app_has_string(finding$event_id[[1L]])
    ) {
      evidence <- finding$event_id[[1L]]
    }
    links <- scans_app_event_links(evidence, data$events)
    htmltools::tags$article(
      class = paste0(
        "scans-app-finding scans-app-finding-",
        scans_app_css_token(finding$severity[[1L]])
      ),
      htmltools::div(
        class = "scans-app-finding-header",
        scans_app_badge(
          scans_app_title_case(finding$severity[[1L]]),
          finding$severity[[1L]]
        ),
        htmltools::tags$strong(
          scans_app_first_string(finding$label[[1L]], finding$scan[[1L]])
        )
      ),
      if (scans_app_has_string(finding$explanation[[1L]])) {
        htmltools::tags$p(finding$explanation[[1L]])
      },
      if (length(links) > 0L) {
        htmltools::div(
          class = "scans-app-evidence-links",
          htmltools::tags$span("Evidence"),
          links
        )
      }
    )
  }))
}

scans_app_event_links <- function(ids, events) {
  ids <- ids[!is.na(ids) & nzchar(ids)]
  rows <- match(ids, events$event_id)
  keep <- !is.na(rows)
  if (!any(keep)) {
    return(NULL)
  }
  htmltools::tagList(Map(
    function(id, row) {
      htmltools::tags$a(
        href = paste0("#", scans_app_event_dom_id(row)),
        scans_app_truncate(id, 28L)
      )
    },
    ids[keep],
    rows[keep]
  ))
}

scans_app_evaluations_ui <- function(evaluations, rows) {
  if (length(rows) == 0L) {
    return(scans_app_empty_ui("No evaluations are joined.", compact = TRUE))
  }
  htmltools::tagList(lapply(rows, function(row) {
    evaluation <- evaluations[row, , drop = FALSE]
    title <- evaluation$evaluation_id[[1L]]
    if (scans_app_has_string(evaluation$scorer[[1L]])) {
      title <- paste0(evaluation$scorer[[1L]], " \u00b7 ", title)
    }
    htmltools::tags$article(
      class = "scans-app-evaluation",
      htmltools::div(
        class = "scans-app-evidence-item-header",
        htmltools::tags$strong(title)
      ),
      scans_app_labeled_value("Value", evaluation$value[[1L]]),
      scans_app_labeled_value("Target", evaluation$target[[1L]]),
      if (scans_app_has_string(evaluation$explanation[[1L]])) {
        htmltools::tags$p(evaluation$explanation[[1L]])
      }
    )
  }))
}

scans_app_losses_ui <- function(data, rows) {
  if (length(rows) == 0L) {
    return(scans_app_empty_ui(
      "No adapter losses are recorded.",
      compact = TRUE
    ))
  }
  htmltools::tagList(lapply(rows, function(row) {
    loss <- data$losses[row, , drop = FALSE]
    links <- scans_app_event_links(loss$event_id[[1L]], data$events)
    htmltools::tags$article(
      class = "scans-app-loss",
      htmltools::div(
        class = "scans-app-evidence-item-header",
        scans_app_badge(scans_app_title_case(loss$reason[[1L]]), "quiet"),
        htmltools::tags$strong(loss$field[[1L]])
      ),
      htmltools::tags$p(loss$detail[[1L]]),
      if (length(links) > 0L) {
        htmltools::div(class = "scans-app-evidence-links", links)
      }
    )
  }))
}
