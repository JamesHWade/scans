scans_app_source_error_heading <- function(label) {
  htmltools::div(
    class = "scans-app-heading",
    htmltools::tags$strong(paste("Traces unavailable for", label))
  )
}

# The reason is shown, not just the failure. Without it the app looks like an
# application with no conversations, and the actual cause -- an expired key, a
# server error on the trace endpoint -- is invisible to whoever is looking.
scans_app_source_error_ui <- function(label, message = NULL) {
  htmltools::div(
    class = "scans-app-load-error",
    htmltools::tags$strong(paste0("Could not load traces for ", label, ".")),
    if (scans_app_has_string(message)) {
      htmltools::div(class = "scans-app-load-error-detail", message)
    },
    htmltools::div(
      class = "scans-app-load-error-hint",
      "Reload traces to try again."
    )
  )
}

scans_app_input_or <- function(value, default) {
  if (is.null(value) || length(value) == 0L) default else value
}

scans_app_entry_id <- function(index) {
  paste0("scans_app_entry_", index)
}

scans_app_entry_ui <- function(record, selected) {
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
    scans_app_entry_id(record$index[[1L]]),
    label = htmltools::tagList(
      htmltools::div(
        class = "scans-app-entry-heading",
        htmltools::tags$span(
          class = "scans-app-entry-title",
          record$title[[1L]]
        ),
        scans_app_badge(as.character(findings), tone)
      ),
      htmltools::div(
        class = "scans-app-entry-id",
        record$trajectory_id[[1L]]
      ),
      htmltools::div(
        class = "scans-app-entry-meta",
        scans_app_badge(record$source_type[[1L]], "source"),
        scans_app_status_badge(record$status[[1L]]),
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
      "scans-app-entry",
      if (selected) "scans-app-entry-selected" else ""
    ),
    `aria-current` = if (selected) "true" else NULL
  )
}

scans_app_overview_ui <- function(data, index) {
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
    Losses = sum(data$loss_trajectory_ids %in% id)
  )
  themes <- c("primary", "primary", "warning", "success", "secondary")

  bslib::layout_column_wrap(
    width = "145px",
    fill = FALSE,
    class = "scans-app-overview",
    !!!Map(
      function(title, value, theme) {
        bslib::value_box(
          title = title,
          value = format(value, big.mark = ",", scientific = FALSE),
          theme = theme,
          class = "scans-app-value"
        )
      },
      names(counts),
      counts,
      themes
    )
  )
}

scans_app_header_ui <- function(data, index) {
  if (is.null(index)) {
    return(htmltools::div(
      class = "scans-app-heading",
      htmltools::tags$strong("No trajectory selected")
    ))
  }
  info <- data$info[index, , drop = FALSE]
  record <- data$records[index, , drop = FALSE]
  htmltools::div(
    class = "scans-app-heading",
    htmltools::div(
      htmltools::tags$strong(record$title[[1L]]),
      htmltools::tags$span(
        class = "scans-app-heading-id",
        info$trajectory_id[[1L]]
      )
    ),
    htmltools::div(
      class = "scans-app-heading-badges",
      scans_app_badge(info$source_type[[1L]], "source"),
      scans_app_status_badge(info$status[[1L]])
    )
  )
}


# Annotations render newest-first as a short log rather than a single current
# value: the store is append-only, and seeing that an earlier reviewer
# disagreed is the point.
scans_app_annotation_log_ui <- function(records) {
  if (nrow(records) == 0L) {
    return(scans_app_empty_ui(
      "No annotations yet for this trajectory.",
      compact = TRUE
    ))
  }
  htmltools::div(
    class = "scans-app-annotation-log",
    htmltools::tagList(lapply(seq_len(nrow(records)), function(row) {
      htmltools::div(
        class = "scans-app-annotation",
        htmltools::div(
          class = "scans-app-annotation-header",
          scans_app_badge(records$label[[row]], "source"),
          htmltools::tags$span(
            class = "scans-app-annotation-meta",
            paste(
              c(
                records$author[[row]],
                scans_app_time_string(records$created_at[[row]])
              ),
              collapse = " \u00b7 "
            )
          )
        ),
        if (scans_app_has_string(records$note[[row]])) {
          htmltools::div(
            class = "scans-app-annotation-note",
            records$note[[row]]
          )
        }
      )
    }))
  )
}
