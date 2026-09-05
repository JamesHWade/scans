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

scans_app_entry_ui <- function(record, selected, annotation = NULL) {
  findings <- record$n_findings[[1L]]
  errors <- record$n_errors[[1L]]
  tone <- if (errors > 0L) {
    "danger"
  } else if (findings > 0L) {
    "warning"
  } else {
    "quiet"
  }
  facts <- c(
    scans_app_short_time(record$started_at[[1L]]),
    if (scans_app_has_string(record$user[[1L]])) record$user[[1L]],
    if (scans_app_has_string(record$model[[1L]])) record$model[[1L]]
  )

  shiny::actionLink(
    scans_app_entry_id(record$index[[1L]]),
    label = htmltools::tagList(
      htmltools::div(
        class = "scans-app-entry-heading",
        htmltools::tags$span(
          class = "scans-app-entry-title",
          record$title[[1L]]
        ),
        if (findings > 0L) {
          scans_app_badge(
            sprintf(
              "%d finding%s",
              findings,
              if (findings == 1L) "" else "s"
            ),
            tone
          )
        },
        if (scans_app_has_string(annotation)) {
          scans_app_badge(annotation, "annotation")
        }
      ),
      if (length(facts) > 0L) {
        htmltools::div(
          class = "scans-app-entry-facts",
          htmltools::tagList(lapply(facts, htmltools::tags$span))
        )
      },
      htmltools::div(
        class = "scans-app-entry-meta",
        scans_app_badge(record$source_type[[1L]], "source"),
        scans_app_notable_status_badge(record$status[[1L]]),
        htmltools::tags$span(
          sprintf(
            "%d turns \u00b7 %d events",
            record$n_turns[[1L]],
            record$n_events[[1L]]
          )
        ),
        htmltools::tags$span(
          class = "scans-app-entry-id",
          record$trajectory_id[[1L]]
        )
      )
    ),
    class = paste(
      "scans-app-entry",
      paste0("scans-app-entry-", tone),
      if (selected) "scans-app-entry-selected" else ""
    ),
    `aria-current` = if (selected) "true" else NULL
  )
}

scans_app_short_time <- function(time) {
  if (length(time) == 0L || is.na(time)) {
    return(NULL)
  }
  format(time, "%b %d %H:%M", tz = "UTC")
}

# The header's stat strip: the counts the value boxes used to show, plus
# what a reviewer looks up first -- who, which model, when, how long, how
# many tokens -- in one line above the transcript.
scans_app_overview_ui <- function(data, index) {
  if (is.null(index)) {
    return(NULL)
  }
  id <- data$info$trajectory_id[[index]]
  info <- data$info[index, , drop = FALSE]
  summary <- data$summaries[index, , drop = FALSE]
  usage <- scans_app_token_usage(data, id)
  tokens_in <- usage$input_tokens[[1L]]
  tokens_out <- usage$output_tokens[[1L]]
  elapsed <- scans_app_measure_rows(data, id, "elapsed")$value[[1L]]
  duration <- if (is.na(elapsed)) NULL else scans_app_seconds(elapsed)
  n_findings <- sum(data$findings$trajectory_id == id)
  n_errors <- sum(
    data$findings$trajectory_id == id & data$findings$severity %in% "error"
  )

  stats <- list(
    list("Turns", scans_app_count(summary$n_turns[[1L]])),
    list("Events", scans_app_count(summary$n_events[[1L]])),
    list(
      "Findings",
      scans_app_count(n_findings),
      if (n_errors > 0L) {
        "danger"
      } else if (n_findings > 0L) {
        "warning"
      }
    ),
    list(
      "Evaluations",
      scans_app_count(sum(
        data$evaluations$trajectory_id == id
      ))
    ),
    list("Losses", scans_app_count(sum(data$loss_trajectory_ids %in% id))),
    list(
      "Tokens",
      if (is.na(tokens_in) && is.na(tokens_out)) {
        NULL
      } else {
        sprintf(
          "%s in / %s out",
          scans_app_count(tokens_in),
          scans_app_count(tokens_out)
        )
      }
    ),
    list("Duration", duration),
    list(
      "Started",
      if (!is.na(info$started_at[[1L]])) {
        scans_app_time_string(info$started_at[[1L]])
      }
    )
  )
  stats <- Filter(function(stat) !is.null(stat[[2L]]), stats)

  htmltools::div(
    class = "scans-app-overview",
    htmltools::tagList(lapply(stats, function(stat) {
      htmltools::div(
        class = paste(
          "scans-app-stat",
          if (length(stat) > 2L && !is.null(stat[[3L]])) {
            paste0("scans-app-stat-", stat[[3L]])
          }
        ),
        htmltools::tags$span(class = "scans-app-stat-label", stat[[1L]]),
        htmltools::tags$span(class = "scans-app-stat-value", stat[[2L]])
      )
    }))
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
    htmltools::tags$strong(record$title[[1L]]),
    htmltools::div(
      class = "scans-app-heading-badges",
      scans_app_badge(info$source_type[[1L]], "source"),
      scans_app_status_badge(info$status[[1L]]),
      if (scans_app_has_string(record$user[[1L]])) {
        scans_app_badge(
          paste0("\U0001F464 ", record$user[[1L]]),
          "user"
        )
      },
      if (scans_app_has_string(info$model[[1L]])) {
        scans_app_badge(info$model[[1L]], "quiet")
      },
      htmltools::tags$span(
        class = "scans-app-heading-id",
        info$trajectory_id[[1L]]
      )
    )
  )
}

# What the last load found. For a Connect source this says how far back it
# looked and whether the span ceiling was hit -- the difference between "a
# quiet week" and "traces are missing".
scans_app_load_info_ui <- function(entry, reloadable = FALSE) {
  info <- entry$read_info
  loaded_at <- entry$loaded_at
  parts <- list()
  if (!is.null(loaded_at)) {
    parts <- c(
      parts,
      list(htmltools::tags$span(
        title = scans_app_time_string(loaded_at),
        paste("Loaded", scans_app_age_string(loaded_at))
      ))
    )
  }
  if (is.list(info)) {
    found <- info$conversations_found %||% info$conversations
    kept <- info$conversations
    conversations <- if (!is.null(kept) && !is.null(found) && kept < found) {
      sprintf(
        "%s of %s conversations",
        format(kept, big.mark = ","),
        format(found, big.mark = ",")
      )
    } else if (!is.null(kept)) {
      sprintf(
        "%s conversation%s",
        format(kept, big.mark = ","),
        if (identical(as.integer(kept), 1L)) "" else "s"
      )
    }
    if (!is.null(conversations)) {
      parts <- c(parts, list(htmltools::tags$span(conversations)))
    }
    if (!is.null(info$spans)) {
      parts <- c(
        parts,
        list(htmltools::tags$span(
          sprintf("%s GenAI spans", format(info$spans, big.mark = ","))
        ))
      )
    }
    window <- scans_app_window_string(info$from, info$to)
    if (!is.null(window)) {
      parts <- c(parts, list(htmltools::tags$span(window)))
    }
  }
  if (length(parts) == 0L) {
    return(NULL)
  }
  htmltools::div(
    class = "scans-app-load-summary",
    htmltools::div(class = "scans-app-load-facts", htmltools::tagList(parts)),
    if (isTRUE(info$truncated)) {
      htmltools::div(
        class = "scans-app-load-warning",
        role = "status",
        sprintf(
          "GenAI span ceiling of %s reached \u2014 older traces were not read.",
          format(info$max_spans, big.mark = ",")
        )
      )
    },
    if (isTRUE(info$incomplete)) {
      htmltools::div(
        class = "scans-app-load-warning",
        role = "status",
        "A page of traces could not be read from Connect \u2014 this snapshot may be incomplete. Reload traces to try again."
      )
    },
    if (
      !is.null(info$n) &&
        !is.null(info$conversations_found) &&
        info$conversations_found > info$n
    ) {
      htmltools::div(
        class = "scans-app-load-warning",
        role = "status",
        sprintf(
          "Showing the %s most recent of %s conversations in the window.",
          format(info$n, big.mark = ","),
          format(info$conversations_found, big.mark = ",")
        )
      )
    }
  )
}

scans_app_age_string <- function(time, now = Sys.time()) {
  seconds <- as.numeric(now - time, units = "secs")
  if (!is.finite(seconds) || seconds < 45) {
    return("just now")
  }
  if (seconds < 3600) {
    return(sprintf("%d min ago", max(1L, round(seconds / 60))))
  }
  if (seconds < 86400) {
    return(sprintf("%d h ago", round(seconds / 3600)))
  }
  sprintf("%d d ago", round(seconds / 86400))
}

scans_app_window_string <- function(from, to) {
  if (is.null(from) && is.null(to)) {
    return(NULL)
  }
  if (!is.null(from) && !is.null(to)) {
    days <- as.numeric(to, units = "secs") - as.numeric(from, units = "secs")
    days <- days / 86400
    if (is.finite(days) && abs(days - round(days)) < 0.05 && days >= 1) {
      return(sprintf(
        "last %d day%s",
        round(days),
        if (round(days) == 1) "" else "s"
      ))
    }
    return(sprintf(
      "%s \u2013 %s",
      format(from, "%b %d", tz = "UTC"),
      format(to, "%b %d", tz = "UTC")
    ))
  }
  if (!is.null(from)) {
    return(paste("since", format(from, "%b %d", tz = "UTC")))
  }
  paste("until", format(to, "%b %d", tz = "UTC"))
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
