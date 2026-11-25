scans_app_annotation_server <- function(
  input,
  output,
  session,
  state,
  annotations,
  annotation_poll_interval
) {
  saved_annotation_filter <- shiny::reactive({
    saved <- state$active()$investigation
    !is.null(saved) &&
      (length(saved$view$annotation_ids) > 0L || saved$view$annotated_only)
  })
  state$annotated_only <- shiny::reactive({
    (!is.null(annotations) || saved_annotation_filter()) &&
      isTRUE(
        input$scans_app_annotated_only %||%
          state$active()$investigation$view$annotated_only
      )
  })
  output$scans_app_snapshot_annotation_filter <- shiny::renderUI({
    saved <- state$active()$investigation
    if (is.null(annotations) && saved_annotation_filter()) {
      bslib::input_switch(
        "scans_app_annotated_only",
        "Annotated at save",
        value = saved$view$annotated_only
      )
    }
  })

  state$annotation_revision <- shiny::reactiveVal(0L)
  annotation_status <- shiny::reactiveVal("")

  # The poll only watches for writes by other sessions. A write from this
  # session bumps `annotation_revision`, so it shows without waiting.
  annotation_file <- if (!is.null(annotations)) {
    shiny::reactivePoll(
      intervalMillis = annotation_poll_interval,
      session = session,
      checkFunc = function() annotations_file_revision(annotations$path),
      valueFunc = function() annotations_file_revision(annotations$path)
    )
  }
  annotation_records <- shiny::reactive({
    if (is.null(annotations)) {
      return(NULL)
    }
    state$annotation_revision()
    annotation_file()
    tryCatch(
      annotations$read(application = state$application()),
      error = function(cnd) NULL
    )
  })
  state$annotation_records <- annotation_records

  # The queue ranks every trajectory once per snapshot and annotation change;
  # filters only subset it, which keeps the relative order.
  state$queue <- shiny::reactive({
    current <- state$data()
    if (is.null(current)) {
      return(NULL)
    }
    cohort_review_queue(
      current,
      current$info$trajectory_id,
      annotation_records()
    )
  })

  # The latest label (or "Note") per annotated trajectory of the current
  # application, so the browser can mark and filter reviewed entries.
  state$annotation_labels <- shiny::reactive({
    if (is.null(annotations)) {
      return(character())
    }
    scans_app_annotation_labels(annotation_records())
  })

  state$annotated <- shiny::reactive({
    current <- state$data()
    if (is.null(current)) {
      return(logical())
    }
    saved <- state$active()$investigation
    if (!is.null(saved) && is.null(annotations)) {
      return(current$info$trajectory_id %in% saved$view$annotation_ids)
    }
    current$info$trajectory_id %in% names(state$annotation_labels())
  })

  selected_annotation_target <- shiny::reactive({
    current <- state$data()
    index <- state$selected()
    if (is.null(current) || is.null(index)) {
      return(NULL)
    }
    list(
      application = state$application(),
      trajectory_id = current$info$trajectory_id[[index]]
    )
  })

  # A draft judgement belongs to the application and trajectory it was typed
  # for. Clear the form only when that pair changes; an in-memory re-scan
  # invalidates `state$data()` but leaves the target unchanged.
  annotation_target <- shiny::reactiveVal(NULL)
  shiny::observe({
    target <- selected_annotation_target()
    previous <- shiny::isolate(annotation_target())
    if (identical(target, previous)) {
      return()
    }
    annotation_target(target)
    annotation_status("")
    shiny::updateSelectInput(
      session,
      "scans_app_annotation_label",
      selected = ""
    )
    shiny::updateTextAreaInput(
      session,
      "scans_app_annotation_note",
      value = ""
    )
  })

  shiny::observeEvent(
    input$scans_app_annotation_save,
    ignoreInit = TRUE,
    {
      if (is.null(annotations)) {
        return()
      }
      target <- selected_annotation_target()
      if (is.null(target)) {
        annotation_status("Select a trajectory first.")
        return()
      }
      written <- tryCatch(
        {
          annotations$append(
            application = target$application,
            trajectory_id = target$trajectory_id,
            label = input$scans_app_annotation_label,
            note = input$scans_app_annotation_note,
            author = annotations_default_author(session)
          )
          TRUE
        },
        error = function(cnd) {
          annotation_status(scans_app_annotation_error(cnd))
          FALSE
        }
      )
      if (!isTRUE(written)) {
        return()
      }
      shiny::updateTextAreaInput(
        session,
        "scans_app_annotation_note",
        value = ""
      )
      annotation_status("Saved.")
      state$annotation_revision(state$annotation_revision() + 1L)
    }
  )

  output$scans_app_annotation_status <- shiny::renderText(annotation_status())

  # Dispositions: one annotation record per click or key, on the selected
  # trajectory only. The request names the trajectory the reviewer was
  # looking at, so a key pressed while the next one is still loading is
  # dropped rather than applied to it.
  disposition_status <- shiny::reactiveVal("")
  next_unreviewed <- function(order, from = NULL) {
    queue <- state$queue()
    pending <- queue$index[!queue$reviewed %in% TRUE]
    position <- match(from, order)
    if (!is.null(from) && !is.na(position)) {
      order <- c(order[-seq_len(position)], order[seq_len(position - 1L)])
    }
    candidates <- order[order %in% pending]
    if (length(candidates)) candidates[[1L]] else NULL
  }
  shiny::observeEvent(input$scans_app_disposition, {
    request <- input$scans_app_disposition
    target <- selected_annotation_target()
    label <- request$label %||% ""
    if (
      is.null(annotations) ||
        is.null(target) ||
        !identical(request$trajectory_id, target$trajectory_id) ||
        !is.character(label) ||
        length(label) != 1L ||
        !label %in% c("", annotations$labels)
    ) {
      return()
    }
    author <- annotations_default_author(session)
    name <- if (nzchar(label)) label else "reviewed"
    records <- annotation_records()
    latest <- records[
      records$trajectory_id %in% target$trajectory_id,
      ,
      drop = FALSE
    ]
    if (
      !is.null(records) &&
        nrow(latest) > 0L &&
        identical(latest$author[[1L]], author) &&
        identical(
          latest$label[[1L]],
          if (nzchar(label)) label else NA_character_
        ) &&
        (nzchar(label) || identical(latest$note[[1L]], "Reviewed"))
    ) {
      disposition_status(sprintf("Already marked %s.", name))
      return()
    }
    order <- state$visible()
    index <- state$selected()
    written <- tryCatch(
      {
        annotations$append(
          application = target$application,
          trajectory_id = target$trajectory_id,
          label = label,
          note = if (nzchar(label)) NULL else "Reviewed",
          author = author
        )
        TRUE
      },
      error = function(cnd) {
        disposition_status(scans_app_annotation_error(cnd))
        FALSE
      }
    )
    if (!isTRUE(written)) {
      return()
    }
    state$annotation_revision(state$annotation_revision() + 1L)
    message <- sprintf("Marked %s.", name)
    if (!isFALSE(input$scans_app_advance)) {
      following <- next_unreviewed(order, index)
      if (is.null(following)) {
        message <- paste(message, "Queue done: nothing left to review here.")
      } else {
        state$selected(following)
        message <- paste(message, "Opened the next unreviewed.")
      }
    }
    disposition_status(message)
  })
  shiny::observeEvent(input$scans_app_next_unreviewed, ignoreInit = TRUE, {
    following <- next_unreviewed(state$visible(), state$selected())
    if (is.null(following)) {
      disposition_status("Queue done: nothing left to review here.")
    } else {
      disposition_status("")
      state$selected(following)
    }
  })
  output$scans_app_review_bar <- shiny::renderUI({
    target <- selected_annotation_target()
    queue <- state$queue()
    if (is.null(annotations) || is.null(target) || is.null(queue)) {
      return(NULL)
    }
    scans_app_review_bar_ui(
      queue[match(target$trajectory_id, queue$trajectory_id), ],
      annotations$labels,
      disposition_status()
    )
  })

  output$scans_app_annotation_log <- shiny::renderUI({
    if (is.null(annotations)) {
      return(NULL)
    }
    state$annotation_revision()
    target <- selected_annotation_target()
    if (is.null(target)) {
      return(scans_app_empty_ui("Select a trajectory to annotate it."))
    }
    records <- annotation_records()
    if (is.null(records)) {
      return(scans_app_empty_ui("Could not read the annotation store."))
    }
    records <- records[
      records$trajectory_id %in% target$trajectory_id,
      ,
      drop = FALSE
    ]
    scans_app_annotation_log_ui(records)
  })
}

# Latest annotation label per trajectory; a note-only record shows as "Note".
scans_app_annotation_labels <- function(records) {
  if (is.null(records) || nrow(records) == 0L) {
    return(character())
  }
  records <- records[
    order(records$created_at, decreasing = TRUE),
    ,
    drop = FALSE
  ]
  latest <- records[!duplicated(records$trajectory_id), , drop = FALSE]
  label <- latest$label
  label[is.na(label) | !nzchar(label)] <- "Note"
  stats::setNames(label, latest$trajectory_id)
}

# Validation failures from the store name what the reviewer must change and
# are shown as written. Anything else (an unwritable path, a full disk) is
# logged on the server and replaced, so the browser never learns the store's
# location.
scans_app_annotation_error <- function(cnd) {
  if (inherits(cnd, "scans_error_annotation_record")) {
    return(conditionMessage(cnd))
  }
  cli::cli_inform(c(
    "!" = "Failed to save an annotation.",
    "i" = "{conditionMessage(cnd)}"
  ))
  "The annotation could not be saved. Check the server logs for details."
}
