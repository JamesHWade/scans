# The browser list as a review queue, and the controls that record a
# disposition. A disposition is an annotation record in the configured store;
# it is unrelated to the session-local evidence reviews that build examples.

# Built on demand: htmltools is optional and must not load with the package.
scans_app_marker_shape <- function(class) {
  switch(
    class,
    error = htmltools::tags$path(d = "M6 0.5 11.5 6 6 11.5 0.5 6Z"),
    warning = htmltools::tags$path(d = "M6 1 11.5 11H0.5Z"),
    clean = htmltools::tags$circle(cx = 6, cy = 6, r = 4.25),
    unassessed = htmltools::tags$path(d = "M2 6h8")
  )
}

# Shape and colour both carry the class; the hidden text states it.
scans_app_marker <- function(class) {
  htmltools::tags$span(
    class = paste0("scans-app-marker scans-app-marker-", class),
    htmltools::tags$svg(
      viewBox = "0 0 12 12",
      `aria-hidden` = "true",
      focusable = "false",
      scans_app_marker_shape(class)
    ),
    htmltools::tags$span(
      class = "visually-hidden",
      scans_app_activity_classes[[class]]
    )
  )
}

scans_app_reason_chips <- function(reasons, limit = 2L) {
  shown <- utils::head(reasons, limit)
  more <- length(reasons) - length(shown)
  htmltools::tagList(
    lapply(shown, function(reason) {
      htmltools::tags$span(class = "scans-app-reason", reason)
    }),
    if (more > 0L) {
      htmltools::tags$span(
        class = "scans-app-reason scans-app-reason-more",
        title = paste(utils::tail(reasons, more), collapse = ", "),
        sprintf("+%d", more)
      )
    }
  )
}

scans_app_reviewed_mark <- function(label) {
  htmltools::tags$span(
    class = "scans-app-reviewed",
    htmltools::tags$span(`aria-hidden` = "true", "\u2713"),
    htmltools::tags$span(class = "visually-hidden", "Reviewed:"),
    scans_app_badge(
      if (is.na(label)) "Reviewed" else label,
      "annotation"
    )
  )
}

# Without a queue row (a caller that has only the record) the entry shows the
# class its own finding counts imply and no reasons or review state.
scans_app_entry_ui <- function(record, selected, queue = NULL) {
  if (is.null(queue)) {
    queue <- list(
      class = if (record$n_errors[[1L]] > 0L) {
        "error"
      } else if (record$n_findings[[1L]] > 0L) {
        "warning"
      } else {
        "clean"
      },
      reasons = list(character()),
      reviewed = NA,
      label = NA_character_
    )
  }
  empty <- record$n_events[[1L]] == 0L && record$n_turns[[1L]] == 0L
  reasons <- queue$reasons[[1L]]
  elapsed <- record$elapsed[[1L]] %||% NA_real_
  facts <- c(
    scans_app_short_time(record$started_at[[1L]]),
    if (!is.na(record$user[[1L]])) record$user[[1L]],
    if (!is.na(elapsed)) scans_app_seconds(elapsed)
  )
  shiny::actionLink(
    scans_app_entry_id(record$index[[1L]]),
    label = htmltools::tagList(
      scans_app_marker(queue$class[[1L]]),
      htmltools::div(
        class = "scans-app-entry-body",
        htmltools::div(class = "scans-app-entry-title", record$title[[1L]]),
        htmltools::div(
          class = "scans-app-entry-facts",
          if (!is.na(record$parent_trajectory_id[[1L]] %||% NA)) {
            htmltools::tags$span(
              class = "scans-app-reason scans-app-subagent",
              "subagent"
            )
          },
          if (length(reasons)) {
            scans_app_reason_chips(reasons)
          } else if (empty) {
            "No transcript recorded"
          } else {
            "No findings"
          }
        ),
        htmltools::div(
          class = "scans-app-entry-time",
          paste(facts, collapse = " \u00b7 ")
        ),
        if (isTRUE(queue$reviewed[[1L]])) {
          scans_app_reviewed_mark(queue$label[[1L]])
        }
      )
    ),
    title = record$trajectory_id[[1L]],
    class = paste(
      "scans-app-entry scans-app-queue-entry",
      if (selected) "scans-app-entry-selected" else ""
    ),
    `aria-current` = if (selected) "true" else NULL
  )
}

scans_app_review_progress_ui <- function(progress, noun = scans_app_noun()) {
  if (is.na(progress$flagged_reviewed)) {
    return(NULL)
  }
  flagged <- progress$flagged > 0L
  done <- if (flagged) progress$flagged_reviewed else progress$reviewed
  of <- if (flagged) progress$flagged else progress$total
  htmltools::div(
    class = "scans-app-review-progress",
    htmltools::tags$span(sprintf(
      "%s of %s %s%s reviewed",
      scans_app_count(done),
      scans_app_count(of),
      if (flagged) "flagged " else "",
      if (of == 1L) noun$one else noun$many
    )),
    scans_app_meter(if (of > 0L) done / of else 0, "primary")
  )
}

scans_app_review_state_text <- function(queue, now = Sys.time()) {
  if (!isTRUE(queue$reviewed[[1L]])) {
    return("Not reviewed yet")
  }
  paste(
    c(
      paste(
        "Reviewed by",
        if (is.na(queue$reviewed_by[[1L]])) {
          "unknown"
        } else {
          queue$reviewed_by[[1L]]
        }
      ),
      if (!is.na(queue$label[[1L]])) queue$label[[1L]],
      if (!is.na(queue$reviewed_at[[1L]])) {
        scans_app_age_string(queue$reviewed_at[[1L]], now)
      }
    ),
    collapse = " \u00b7 "
  )
}

# One button per store label plus a label-free "Reviewed". `data-scans-key`
# is the shortcut the page script clicks the button for.
scans_app_review_bar_ui <- function(
  queue,
  labels,
  status = "",
  outside = FALSE
) {
  id <- queue$trajectory_id[[1L]]
  state <- paste(
    c(
      if (outside) scans_app_outside_filters,
      scans_app_review_state_text(queue)
    ),
    collapse = " \u00b7 "
  )
  keys <- utils::head(seq_along(labels), 9L)
  # The note is a sibling of the buttons, not a child, so the layout can put
  # it beside them or on its own line.
  htmltools::tagList(
    scans_app_review_marks_ui(id, queue, labels, keys),
    htmltools::tags$span(
      class = "scans-app-review-note",
      htmltools::tags$span(
        class = "scans-app-review-state",
        title = state,
        state
      ),
      htmltools::tags$span(
        class = "scans-app-review-status",
        role = "status",
        `aria-live` = "polite",
        status
      )
    )
  )
}

scans_app_review_marks_ui <- function(id, queue, labels, keys) {
  htmltools::div(
    class = "scans-app-review-bar",
    role = "group",
    `aria-label` = "Mark this trajectory",
    `data-scans-target` = id,
    htmltools::tags$span(class = "scans-app-review-bar-label", "Mark as"),
    lapply(seq_along(labels), function(i) {
      key <- if (i %in% keys) as.character(i)
      htmltools::tags$button(
        type = "button",
        class = "btn btn-sm btn-default scans-app-disposition",
        `data-scans-disposition` = labels[[i]],
        `data-scans-key` = key,
        `aria-pressed` = if (identical(queue$label[[1L]], labels[[i]])) {
          "true"
        } else {
          "false"
        },
        title = paste0(
          "Mark as ",
          labels[[i]],
          if (!is.null(key)) sprintf(" (key %s)", key)
        ),
        if (!is.null(key)) htmltools::tags$kbd(key),
        labels[[i]]
      )
    }),
    htmltools::tags$button(
      type = "button",
      class = "btn btn-sm btn-default scans-app-disposition",
      `data-scans-disposition` = "",
      `data-scans-key` = "e",
      title = "Mark reviewed without a label (key E)",
      htmltools::tags$kbd("E"),
      "Reviewed"
    )
  )
}

scans_app_shortcuts <- function(labels) {
  rows <- c(
    "J or \u2193" = "Next in the list",
    "K or \u2191" = "Previous in the list",
    "U" = "Next unreviewed",
    stats::setNames(
      paste("Mark as", utils::head(labels, 9L)),
      utils::head(seq_along(labels), 9L)
    ),
    "E" = "Mark reviewed without a label",
    "?" = "Show these shortcuts"
  )
  htmltools::tags$dl(
    class = "scans-app-shortcuts",
    lapply(names(rows), function(key) {
      htmltools::tagList(
        htmltools::tags$dt(htmltools::tags$kbd(key)),
        htmltools::tags$dd(rows[[key]])
      )
    })
  )
}

# Static controls beside the review bar: they keep their state while the bar
# re-renders for each trajectory.
scans_app_review_controls_ui <- function(annotations) {
  if (is.null(annotations)) {
    return(NULL)
  }
  htmltools::div(
    class = "scans-app-review-host",
    htmltools::div(
      class = "scans-app-review-controls",
      scans_app_review_controls_inner(annotations)
    )
  )
}

scans_app_review_controls_inner <- function(annotations) {
  htmltools::tagList(
    shiny::uiOutput("scans_app_review_bar"),
    htmltools::div(
      class = "scans-app-review-options",
      htmltools::tagAppendAttributes(
        shiny::checkboxInput("scans_app_advance", "Advance", value = TRUE),
        title = "After marking, open the next unreviewed"
      ),
      htmltools::tagAppendAttributes(
        shiny::actionButton(
          "scans_app_next_unreviewed",
          "Next",
          class = "btn-sm btn-default",
          title = "Next unreviewed (key U)",
          `aria-label` = "Next unreviewed"
        ),
        `data-scans-key` = "u"
      ),
      bslib::popover(
        htmltools::tags$button(
          type = "button",
          id = "scans_app_shortcuts",
          class = "btn btn-sm btn-default",
          `data-scans-key` = "?",
          `aria-label` = "Keyboard shortcuts",
          title = "Keyboard shortcuts (key ?)",
          "?"
        ),
        title = "Keyboard shortcuts",
        scans_app_shortcuts(annotations$labels)
      )
    )
  )
}

# The top of the queue, each row opening its trajectory.
scans_app_review_next_ui <- function(
  queue,
  rows = utils::head(queue, 5L),
  noun = scans_app_noun()
) {
  store <- !anyNA(queue$reviewed)
  pending <- sum(!queue$reviewed %in% TRUE)
  htmltools::tagList(
    htmltools::tags$p(
      class = "scans-app-review-next-note",
      if (!store) {
        paste0(
          "No annotation store is configured, so review state is not recorded. ",
          "One per pattern, most prevalent first."
        )
      } else if (pending == 0L) {
        sprintf(
          "Every %s in view is reviewed.",
          noun$one
        )
      } else {
        sprintf(
          "%s in view still to review. One per pattern, most prevalent first.",
          scans_app_n(pending, noun)
        )
      }
    ),
    htmltools::div(
      class = "scans-app-review-next",
      lapply(seq_len(nrow(rows)), function(i) {
        facts <- c(
          scans_app_short_time(rows$started_at[[i]]),
          if (!is.na(rows$user[[i]])) rows$user[[i]]
        )
        htmltools::tags$button(
          type = "button",
          class = "scans-app-review-next-row",
          `data-scans-trajectory` = rows$trajectory_id[[i]],
          scans_app_marker(rows$class[[i]]),
          htmltools::tags$strong(rows$title[[i]]),
          htmltools::tags$span(
            class = "scans-app-entry-facts",
            if (length(rows$reasons[[i]])) {
              scans_app_reason_chips(rows$reasons[[i]], 3L)
            } else {
              "No findings"
            }
          ),
          htmltools::tags$span(
            class = "scans-app-review-next-facts",
            paste(facts, collapse = " \u00b7 ")
          ),
          htmltools::tags$span(
            class = "scans-app-review-next-pattern",
            if (!is.null(rows$pattern) && !is.na(rows$pattern[[i]])) {
              scans_app_pattern_label(rows$pattern[[i]])
            }
          ),
          shiny::icon("chevron-right")
        )
      })
    )
  )
}
