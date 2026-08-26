scans_app_transcript_ui <- function(data, index) {
  if (is.null(index)) {
    return(scans_app_empty_ui("Select a trajectory to inspect its path."))
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
    return(scans_app_empty_ui("This trajectory has no turns or events."))
  }

  blocks <- list()
  known_turn_ids <- data$turns$turn_id[turn_rows]
  event_turn_ids <- data$events$turn_id[event_rows]
  known_event_turns <- !is.na(event_turn_ids) &
    event_turn_ids %in% known_turn_ids
  if (length(event_rows) > 0L) {
    owners <- ifelse(
      known_event_turns,
      paste0("turn:", event_turn_ids),
      "run"
    )
    segments <- cumsum(c(TRUE, owners[-1L] != owners[-length(owners)]))
    event_groups <- split(event_rows, segments)
    blocks <- lapply(event_groups, function(rows) {
      turn_id <- data$events$turn_id[[rows[[1L]]]]
      if (!is.na(turn_id) && turn_id %in% known_turn_ids) {
        turn_row <- turn_rows[
          match(turn_id, data$turns$turn_id[turn_rows])
        ]
        return(list(
          event_index = data$events$event_index[[rows[[1L]]]],
          turn_index = data$turns$turn_index[[turn_row]],
          ui = scans_app_turn_ui(
            data$turns[turn_row, , drop = FALSE],
            data$events,
            rows
          )
        ))
      }
      list(
        event_index = data$events$event_index[[rows[[1L]]]],
        turn_index = NA_integer_,
        ui = scans_app_event_group_ui(
          if (length(turn_rows) == 0L) "Event stream" else "Run events",
          data$events,
          rows
        )
      )
    })
  }
  turns_with_events <- unique(event_turn_ids[known_event_turns])
  eventless_turn_rows <- turn_rows[
    !data$turns$turn_id[turn_rows] %in% turns_with_events
  ]
  for (turn_row in eventless_turn_rows) {
    blocks <- append(
      blocks,
      list(list(
        event_index = Inf,
        turn_index = data$turns$turn_index[[turn_row]],
        ui = scans_app_turn_ui(
          data$turns[turn_row, , drop = FALSE],
          data$events,
          integer()
        )
      ))
    )
  }

  event_indices <- vapply(blocks, `[[`, numeric(1), "event_index")
  event_blocks <- blocks[is.finite(event_indices)]
  block_order <- order(
    vapply(event_blocks, `[[`, numeric(1), "event_index"),
    method = "radix"
  )
  ordered_blocks <- event_blocks[block_order]
  eventless_blocks <- blocks[!is.finite(event_indices)]
  eventless_blocks <- eventless_blocks[
    order(
      vapply(eventless_blocks, `[[`, integer(1), "turn_index"),
      method = "radix"
    )
  ]
  for (block in eventless_blocks) {
    ordered_blocks <- scans_app_insert_eventless_turn(ordered_blocks, block)
  }
  blocks <- lapply(ordered_blocks, `[[`, "ui")

  htmltools::div(class = "scans-app-path", htmltools::tagList(blocks))
}

scans_app_insert_eventless_turn <- function(blocks, block) {
  if (length(blocks) == 0L) {
    return(list(block))
  }
  turn_indices <- vapply(blocks, `[[`, integer(1), "turn_index")
  later <- which(!is.na(turn_indices) & turn_indices > block$turn_index)
  if (length(later) > 0L) {
    return(append(blocks, list(block), after = min(later) - 1L))
  }
  earlier <- which(!is.na(turn_indices) & turn_indices < block$turn_index)
  if (length(earlier) > 0L) {
    return(append(blocks, list(block), after = max(earlier)))
  }
  append(blocks, list(block), after = 0L)
}

scans_app_event_group_ui <- function(title, events, rows) {
  htmltools::tags$section(
    class = "scans-app-event-group",
    htmltools::tags$h2(title),
    htmltools::tagList(lapply(rows, function(row) {
      scans_app_event_ui(events[row, , drop = FALSE], row)
    }))
  )
}

scans_app_turn_ui <- function(turn, events, rows) {
  role <- scans_app_first_string(turn$role[[1L]], "unknown")
  title <- paste0(
    scans_app_title_case(role),
    " \u00b7 Turn ",
    turn$turn_index[[1L]]
  )
  if (!is.na(turn$round_index[[1L]])) {
    title <- paste0(title, " \u00b7 Round ", turn$round_index[[1L]])
  }
  body <- if (length(rows) == 0L) {
    scans_app_empty_ui("No events were recorded for this turn.", compact = TRUE)
  } else {
    htmltools::tagList(lapply(rows, function(row) {
      scans_app_event_ui(events[row, , drop = FALSE], row)
    }))
  }

  htmltools::tags$section(
    class = paste0("scans-app-turn scans-app-turn-", scans_app_css_token(role)),
    htmltools::div(
      class = "scans-app-turn-header",
      htmltools::tags$h2(title),
      scans_app_status_badge(turn$status[[1L]])
    ),
    if (scans_app_has_string(turn$error[[1L]])) {
      htmltools::div(class = "scans-app-turn-error", turn$error[[1L]])
    },
    body
  )
}

scans_app_event_ui <- function(event, row) {
  type <- scans_app_first_string(event$event_type[[1L]], "event")
  name <- event$name[[1L]]
  heading <- switch(
    type,
    content = scans_app_title_case(
      scans_app_first_string(event$content_type[[1L]], "content")
    ),
    tool_call = "Tool call",
    tool_result = "Tool result",
    error = "Error",
    scans_app_title_case(gsub("[:_]", " ", type))
  )
  if (scans_app_has_string(name)) {
    heading <- paste(heading, name, sep = " \u00b7 ")
  }
  meta <- scans_app_event_meta(event)
  value <- scans_app_value_text(event$value[[1L]])

  htmltools::tags$article(
    id = scans_app_event_dom_id(row),
    tabindex = "-1",
    class = paste(
      "scans-app-event",
      paste0("scans-app-event-", scans_app_css_token(type)),
      if (scans_app_has_string(event$error[[1L]])) {
        "scans-app-event-failed"
      } else {
        ""
      }
    ),
    htmltools::div(
      class = "scans-app-event-header",
      htmltools::tags$strong(heading),
      scans_app_status_badge(event$status[[1L]])
    ),
    if (length(meta) > 0L) {
      htmltools::div(class = "scans-app-event-meta", meta)
    },
    if (scans_app_has_string(event$text[[1L]])) {
      htmltools::div(class = "scans-app-event-text", event$text[[1L]])
    },
    if (!is.null(value)) {
      htmltools::tags$pre(
        class = "scans-app-event-value",
        htmltools::tags$code(value)
      )
    },
    if (scans_app_has_string(event$error[[1L]])) {
      htmltools::div(
        class = "scans-app-event-error",
        event$error[[1L]]
      )
    }
  )
}

scans_app_event_meta <- function(event) {
  values <- character()
  if (scans_app_has_string(event$call_id[[1L]])) {
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
