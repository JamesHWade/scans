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

  htmltools::div(
    class = "scans-app-path scans-app-chat",
    htmltools::tagList(blocks)
  )
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

# A turn renders as a chat message rather than a titled card: user turns as a
# trailing bubble, assistant turns against an avatar, and the system prompt --
# which is long, identical across a conversation, and rarely what someone
# opened the transcript for -- collapsed behind a disclosure. The diagnostic
# facts a card used to spell out (turn index, tokens, duration, status) move
# to a quiet footer, and a status badge is shown only when the status is worth
# noticing; a green "Completed" on every turn is noise that hides the failures.
scans_app_turn_ui <- function(turn, events, rows) {
  role <- scans_app_first_string(turn$role[[1L]], "unknown")
  token <- scans_app_css_token(role)
  # A turn whose events are all tool traffic is machinery, not speech. commons
  # hands back tool results as user-role turns (ellmer carries a
  # ContentToolResult on a UserTurn), and rendering those as a person's
  # message puts "Returned add_citation" in a chat bubble on the user's side.
  activity <- length(rows) > 0L &&
    all(events$event_type[rows] %in% c("tool_call", "tool_result"))
  body <- htmltools::tagList(
    if (scans_app_has_string(turn$error[[1L]])) {
      htmltools::div(class = "scans-app-turn-error", turn$error[[1L]])
    },
    if (length(rows) == 0L) {
      scans_app_empty_ui(
        "No events were recorded for this turn.",
        compact = TRUE
      )
    } else {
      htmltools::tagList(lapply(rows, function(row) {
        scans_app_event_ui(events[row, , drop = FALSE], row)
      }))
    }
  )
  meta <- scans_app_turn_meta(turn)

  content <- if (activity) {
    htmltools::div(class = "scans-app-msg-body", body, meta)
  } else if (identical(role, "system")) {
    htmltools::tags$details(
      class = "scans-app-msg-disclosure",
      htmltools::tags$summary(
        htmltools::tags$span(class = "scans-app-msg-role", "System prompt"),
        scans_app_turn_position(turn)
      ),
      htmltools::div(class = "scans-app-msg-body", body, meta)
    )
  } else if (identical(role, "user")) {
    htmltools::div(class = "scans-app-msg-bubble", body, meta)
  } else {
    htmltools::tagList(
      htmltools::div(
        class = "scans-app-msg-avatar",
        `aria-hidden` = "true",
        substr(toupper(role), 1L, 1L)
      ),
      htmltools::div(class = "scans-app-msg-body", body, meta)
    )
  }

  htmltools::tags$section(
    class = paste(
      "scans-app-msg",
      paste0("scans-app-msg-", token),
      if (activity) "scans-app-msg-activity" else ""
    ),
    `data-role` = role,
    content
  )
}

# Turn index and round, as a quiet positional label.
scans_app_turn_position <- function(turn) {
  text <- paste0("Turn ", turn$turn_index[[1L]])
  if (!is.na(turn$round_index[[1L]])) {
    text <- paste0(text, " \u00b7 Round ", turn$round_index[[1L]])
  }
  htmltools::tags$span(class = "scans-app-msg-position", text)
}

scans_app_turn_meta <- function(turn) {
  parts <- list(scans_app_turn_position(turn))
  tokens <- c(turn$input_tokens[[1L]], turn$output_tokens[[1L]])
  if (!all(is.na(tokens))) {
    parts <- c(
      parts,
      list(htmltools::tags$span(sprintf(
        "%s in / %s out",
        scans_app_count(tokens[[1L]]),
        scans_app_count(tokens[[2L]])
      )))
    )
  }
  if (!is.na(turn$duration[[1L]])) {
    parts <- c(
      parts,
      list(htmltools::tags$span(sprintf("%.2f s", turn$duration[[1L]])))
    )
  }
  if (scans_app_has_string(turn$finish_reason[[1L]])) {
    parts <- c(parts, list(htmltools::tags$span(turn$finish_reason[[1L]])))
  }
  badge <- scans_app_notable_status_badge(turn$status[[1L]])
  htmltools::div(
    class = "scans-app-msg-meta",
    htmltools::tagList(parts),
    badge
  )
}

scans_app_count <- function(x) {
  if (is.na(x)) "\u2013" else format(x, big.mark = ",", trim = TRUE)
}

# Only statuses that deserve attention get a badge; a routine completion is
# the absence of one.
scans_app_notable_status_badge <- function(status) {
  if (!scans_app_has_string(status)) {
    return(NULL)
  }
  if (status %in% c("completed", "succeeded", "success", "passed")) {
    return(NULL)
  }
  scans_app_status_badge(status)
}

scans_app_event_ui <- function(event, row) {
  type <- scans_app_first_string(event$event_type[[1L]], "event")
  if (type %in% c("tool_call", "tool_result")) {
    return(scans_app_tool_event_ui(event, row, type))
  }
  if (identical(type, "content")) {
    return(scans_app_content_event_ui(event, row))
  }
  scans_app_aside_event_ui(event, row, type)
}

# Message text is the thing people came to read, so it renders as prose with
# no card around it.
scans_app_content_event_ui <- function(event, row) {
  content <- htmltools::tagList(
    scans_app_markdown(event$text[[1L]]),
    scans_app_event_value_ui(event),
    scans_app_event_error_ui(event)
  )
  content_type <- scans_app_first_string(event$content_type[[1L]], "text")
  if (identical(content_type, "thinking")) {
    return(htmltools::tags$details(
      id = scans_app_event_dom_id(row),
      tabindex = "-1",
      class = "scans-app-event scans-app-event-thinking",
      htmltools::tags$summary("Thinking"),
      htmltools::div(class = "scans-app-event-thinking-body", content)
    ))
  }
  htmltools::tags$article(
    id = scans_app_event_dom_id(row),
    tabindex = "-1",
    class = "scans-app-event scans-app-event-content",
    content
  )
}

# Tool activity is supporting detail: one compact, collapsed row per call,
# with the tool name in monospace and the payload inside.
scans_app_tool_event_ui <- function(event, row, type) {
  name <- scans_app_first_string(event$name[[1L]], "tool")
  label <- if (identical(type, "tool_call")) "Called" else "Returned"
  failed <- scans_app_has_string(event$error[[1L]])
  htmltools::tags$details(
    id = scans_app_event_dom_id(row),
    tabindex = "-1",
    class = paste(
      "scans-app-event scans-app-tool",
      paste0("scans-app-tool-", scans_app_css_token(type)),
      if (failed) "scans-app-tool-failed" else ""
    ),
    htmltools::tags$summary(
      htmltools::tags$span(class = "scans-app-tool-label", label),
      htmltools::tags$code(class = "scans-app-tool-name", name),
      scans_app_notable_status_badge(event$status[[1L]])
    ),
    htmltools::div(
      class = "scans-app-tool-body",
      scans_app_event_meta_ui(event),
      if (scans_app_has_string(event$text[[1L]])) {
        htmltools::tags$pre(
          class = "scans-app-event-value",
          htmltools::tags$code(event$text[[1L]])
        )
      },
      scans_app_event_value_ui(event),
      scans_app_event_error_ui(event)
    )
  )
}

# Anything else (an error, a source-specific event such as commons
# provenance) stays visible but quiet.
scans_app_aside_event_ui <- function(event, row, type) {
  heading <- scans_app_title_case(gsub("[:_]", " ", type))
  if (scans_app_has_string(event$name[[1L]])) {
    heading <- paste(heading, event$name[[1L]], sep = " \u00b7 ")
  }
  htmltools::tags$article(
    id = scans_app_event_dom_id(row),
    tabindex = "-1",
    class = paste(
      "scans-app-event scans-app-aside",
      paste0("scans-app-event-", scans_app_css_token(type)),
      if (scans_app_has_string(event$error[[1L]])) {
        "scans-app-event-failed"
      } else {
        ""
      }
    ),
    htmltools::div(
      class = "scans-app-aside-header",
      htmltools::tags$strong(heading),
      scans_app_notable_status_badge(event$status[[1L]])
    ),
    scans_app_event_meta_ui(event),
    if (scans_app_has_string(event$text[[1L]])) {
      htmltools::div(class = "scans-app-event-text", event$text[[1L]])
    },
    scans_app_event_value_ui(event),
    scans_app_event_error_ui(event)
  )
}

scans_app_event_value_ui <- function(event) {
  value <- scans_app_value_text(event$value[[1L]])
  if (is.null(value)) {
    return(NULL)
  }
  htmltools::tags$pre(
    class = "scans-app-event-value",
    htmltools::tags$code(value)
  )
}

scans_app_event_error_ui <- function(event) {
  if (!scans_app_has_string(event$error[[1L]])) {
    return(NULL)
  }
  htmltools::div(class = "scans-app-event-error", event$error[[1L]])
}

scans_app_event_meta_ui <- function(event) {
  meta <- scans_app_event_meta(event)
  if (length(meta) == 0L) {
    return(NULL)
  }
  htmltools::div(class = "scans-app-event-meta", meta)
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
