# The scanner settings the app runs with. Held as one value so the server
# can recompute findings from a cached bundle when the settings change,
# without refetching traces -- a Connect read is seconds of HTTP, and
# choosing a different scan should not pay for it.
scans_app_scan_config <- function(
  scans = NULL,
  repeat_threshold = 2L,
  loop_threshold = 3L
) {
  list(
    scans = scans,
    repeat_threshold = repeat_threshold,
    loop_threshold = loop_threshold
  )
}

scans_app_data <- function(x, scan_config = scans_app_scan_config()) {
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  evaluations <- trajectory_evaluations(x)
  losses <- trajectory_losses(x)
  findings <- scan_trajectories(
    x,
    scans = scan_config$scans,
    repeat_threshold = scan_config$repeat_threshold,
    loop_threshold = scan_config$loop_threshold
  )
  summaries <- summarize_trajectories(x)
  loss_trajectory_ids <- scans_app_loss_trajectory_ids(losses, turns, events)

  list(
    info = info,
    turns = turns,
    events = events,
    evaluations = evaluations,
    losses = losses,
    loss_trajectory_ids = loss_trajectory_ids,
    findings = findings,
    summaries = summaries,
    records = scans_app_records(
      info,
      turns,
      events,
      findings,
      summaries
    )
  )
}

# Conversation totals can include model calls omitted from reconstructed
# turns. Both app views use them when valid, with per-component fallbacks.
scans_app_token_usage <- function(info, summaries) {
  usage <- summaries[c("input_tokens", "output_tokens")]
  for (i in seq_len(nrow(info))) {
    metadata <- info$metadata[[i]]
    if (!is.list(metadata) || !is.list(metadata$otel)) {
      next
    }
    for (field in names(usage)) {
      usage[[field]][[i]] <- scans_app_token_count(
        metadata$otel[[field]],
        usage[[field]][[i]]
      )
    }
  }
  usage
}

scans_app_token_count <- function(value, fallback) {
  if (
    length(value) != 1L ||
      !is.numeric(value) ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0
  ) {
    return(fallback)
  }
  value
}

scans_app_loss_trajectory_ids <- function(losses, turns, events) {
  owners <- losses$trajectory_id
  event_owners <- events$trajectory_id[match(losses$event_id, events$event_id)]
  missing <- is.na(owners) & !is.na(event_owners)
  owners[missing] <- event_owners[missing]

  turn_owners <- turns$trajectory_id[match(losses$turn_id, turns$turn_id)]
  missing <- is.na(owners) & !is.na(turn_owners)
  owners[missing] <- turn_owners[missing]
  owners
}

scans_app_records <- function(info, turns, events, findings, summaries) {
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
      n_errors = integer(),
      started_at = as.POSIXct(character(), tz = "UTC"),
      user = character(),
      model = character()
    ))
  }

  ids <- info$trajectory_id
  event_text_groups <- scans_app_event_text_groups(ids, events)
  snippets <- scans_app_trajectory_snippets(event_text_groups, turns, events)
  transcripts <- scans_app_trajectory_text(event_text_groups, events)
  titles <- vapply(
    seq_along(ids),
    function(index) {
      scans_app_first_string(
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
  users <- vapply(info$metadata, scans_app_metadata_user, character(1))
  search <- vapply(
    seq_along(ids),
    function(index) {
      scans_app_search_string(
        ids[[index]],
        info$run_id[[index]],
        info$source_type[[index]],
        info$source_id[[index]],
        info$agent[[index]],
        info$model[[index]],
        users[[index]],
        titles[[index]],
        transcripts[[index]]
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
    n_errors = n_errors,
    started_at = info$started_at,
    user = users,
    model = info$model
  )
}

# The user a trajectory belongs to, when the OTel source recorded one. Source
# identifiers remain namespaced until adapters share an explicit contract.
scans_app_metadata_user <- function(metadata) {
  if (!is.list(metadata)) {
    return(NA_character_)
  }
  if (!is.list(metadata$otel)) {
    return(NA_character_)
  }
  value <- metadata$otel$user
  if (
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  ) {
    return(value)
  }
  NA_character_
}

scans_app_sort_choices <- c(
  "Newest first" = "newest",
  "Oldest first" = "oldest",
  "Most findings" = "findings",
  "Longest" = "longest"
)

# Ordering is applied to the visible indices, never to the records: the
# index is the trajectory's identity in the browser and in the selection.
scans_app_order_records <- function(records, indices, sort = "newest") {
  if (length(indices) < 2L) {
    return(indices)
  }
  subset <- records[indices, , drop = FALSE]
  started <- as.numeric(subset$started_at)
  ordering <- switch(
    sort %||% "newest",
    oldest = order(started, subset$index, na.last = TRUE, method = "radix"),
    findings = order(
      -subset$n_errors,
      -subset$n_findings,
      -ifelse(is.na(started), -Inf, started),
      subset$index,
      method = "radix"
    ),
    longest = order(
      -subset$n_events,
      -subset$n_turns,
      subset$index,
      method = "radix"
    ),
    order(
      -ifelse(is.na(started), -Inf, started),
      subset$index,
      method = "radix"
    )
  )
  indices[ordering]
}

scans_app_search_string <- function(...) {
  values <- vapply(
    list(...),
    function(value) {
      if (is.character(value) && length(value) == 1L && !is.na(value)) {
        value
      } else {
        ""
      }
    },
    character(1)
  )
  paste(values, collapse = " ")
}

scans_app_event_text_groups <- function(ids, events) {
  groups <- rep(list(integer()), length(ids))
  if (length(ids) == 0L || nrow(events) == 0L) {
    return(groups)
  }
  rows <- which(
    events$trajectory_id %in%
      ids &
      !is.na(events$text) &
      nzchar(trimws(events$text))
  )
  if (length(rows) == 0L) {
    return(groups)
  }
  group_index <- match(events$trajectory_id[rows], ids)
  order_index <- order(
    group_index,
    events$event_index[rows],
    events$event_id[rows],
    method = "radix"
  )
  rows <- rows[order_index]
  group_index <- group_index[order_index]
  grouped <- split(rows, group_index)
  groups[as.integer(names(grouped))] <- unname(grouped)
  groups
}

scans_app_trajectory_text <- function(event_text_groups, events) {
  vapply(
    event_text_groups,
    function(rows) {
      text <- trimws(events$text[rows])
      paste(gsub("\\s+", " ", text), collapse = " ")
    },
    character(1)
  )
}

scans_app_trajectory_snippets <- function(event_text_groups, turns, events) {
  roles <- turns$role[match(events$turn_id, turns$turn_id)]
  vapply(
    event_text_groups,
    function(rows) {
      if (length(rows) == 0L) {
        return(NA_character_)
      }
      user_rows <- rows[roles[rows] %in% "user"]
      if (length(user_rows) > 0L) {
        rows <- user_rows
      }
      text <- scans_app_strip_markdown(events$text[[rows[[1L]]]])
      scans_app_truncate(gsub("\\s+", " ", trimws(text)), 90L)
    },
    character(1)
  )
}

scans_app_first_string <- function(...) {
  values <- list(...)
  for (value in values) {
    if (
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    ) {
      return(value)
    }
  }
  "Untitled trajectory"
}

scans_app_filter_records <- function(
  records,
  source = NULL,
  status = NULL,
  query = "",
  findings_only = FALSE,
  annotated = NULL,
  annotated_only = FALSE
) {
  keep <- rep(TRUE, nrow(records))
  source_all <- scans_app_filter_sentinel(records$source_type, "source-all")
  if (!is.null(source) && !identical(source, source_all)) {
    keep <- keep & records$source_type %in% source
  }
  status_all <- scans_app_filter_sentinel(records$status, "status-all")
  missing_status <- scans_app_filter_sentinel(records$status, "status-unknown")
  if (identical(status, missing_status)) {
    keep <- keep & scans_app_unknown_status(records$status)
  } else if (!is.null(status) && !identical(status, status_all)) {
    keep <- keep & records$status %in% status
  }
  query <- trimws(tolower(query))
  if (nzchar(query)) {
    keep <- keep & grepl(query, records$search, fixed = TRUE)
  }
  if (isTRUE(findings_only)) {
    keep <- keep & records$n_findings > 0L
  }
  if (isTRUE(annotated_only)) {
    keep <- keep & (annotated %||% rep(FALSE, nrow(records)))
  }
  records$index[keep]
}

scans_app_filter_sentinel <- function(values, type) {
  value <- paste0(".scans-app-", type)
  while (value %in% values) {
    value <- paste0(value, "-")
  }
  value
}

scans_app_unknown_status <- function(status) {
  is.na(status) | (!is.na(status) & !nzchar(trimws(status)))
}

scans_app_filter_choices <- function(data = NULL) {
  if (is.null(data)) {
    source_values <- character()
    status_values <- character()
  } else {
    source_values <- data$records$source_type
    status_values <- data$records$status
  }
  scans_app_filter_choices_from_values(source_values, status_values)
}

scans_app_filter_choices_from_values <- function(source_values, status_values) {
  sources <- sort(unique(source_values))
  sources <- sources[!is.na(sources) & nzchar(sources)]
  statuses <- sort(unique(status_values))
  statuses <- statuses[!scans_app_unknown_status(statuses)]
  source_all <- scans_app_filter_sentinel(source_values, "source-all")
  status_all <- scans_app_filter_sentinel(status_values, "status-all")
  status_choices <- c("All statuses" = status_all)
  if (any(scans_app_unknown_status(status_values))) {
    status_choices <- c(
      status_choices,
      "Unknown" = scans_app_filter_sentinel(status_values, "status-unknown")
    )
  }
  status_choices <- c(status_choices, stats::setNames(statuses, statuses))

  list(
    source = c("All sources" = source_all, sources),
    status = status_choices,
    source_all = source_all,
    status_all = status_all
  )
}
