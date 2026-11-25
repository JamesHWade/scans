# Exports are built from the same visible ids and cohort tables the screen
# shows. Each builder is a plain function so its content can be checked
# without Shiny. None of them reads credentials: a link is the app's own URL
# and the R snippet names environment variables without their values.

# A shareable hash: "<application>/<trajectory_id>", optionally followed by
# "?scope=key=value&..." naming the pattern, facets, and findings switch.
# Older hashes have no scope and parse as before.
scans_app_scope_keys <- c(
  "pattern",
  "findings",
  "day",
  "tool",
  "tool_failed",
  "user",
  "model"
)

scans_app_scope_hash <- function(
  application,
  trajectory_id = NULL,
  scope = list()
) {
  encode <- function(x) utils::URLencode(x, reserved = TRUE, repeated = TRUE)
  scope <- scope[intersect(scans_app_scope_keys, names(scope))]
  scope <- Filter(function(value) !is.null(value) && !isFALSE(value), scope)
  paste0(
    encode(application),
    "/",
    if (!is.null(trajectory_id)) encode(trajectory_id),
    if (length(scope)) {
      paste0(
        "?scope=",
        paste0(
          names(scope),
          "=",
          vapply(scope, function(value) encode(as.character(value)), ""),
          collapse = "&"
        )
      )
    }
  )
}

scans_app_parse_scope <- function(text) {
  pairs <- strsplit(strsplit(text, "&", fixed = TRUE)[[1L]], "=", fixed = TRUE)
  pairs <- Filter(
    function(pair) length(pair) == 2L && pair[[1L]] %in% scans_app_scope_keys,
    pairs
  )
  scope <- lapply(pairs, function(pair) utils::URLdecode(pair[[2L]]))
  names(scope) <- vapply(pairs, function(pair) pair[[1L]], "")
  for (flag in intersect(c("findings", "tool_failed"), names(scope))) {
    scope[[flag]] <- if (identical(scope[[flag]], "TRUE")) TRUE
  }
  scope
}

# `base` is the app's own address; with none (a test session) links are
# relative fragments.
scans_app_link <- function(base, application, scope = list()) {
  force(base)
  force(application)
  force(scope)
  function(trajectory_id = NULL) {
    paste0(
      base %||% "",
      "#",
      scans_app_scope_hash(application, trajectory_id, scope)
    )
  }
}

export_time <- function(time) {
  ifelse(
    is.na(time),
    NA_character_,
    format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

# What the rows of an export cover, repeated on every row so a file read on
# its own still states its scope and capture window.
export_scope <- function(headline, labels = NULL, noun = scans_app_noun()) {
  window <- scans_app_day_range(headline$first_started, headline$last_started)
  paste(
    c(
      sprintf(
        "%s of %s %s",
        scans_app_count(headline$n),
        scans_app_count(headline$total),
        noun$many
      ),
      unname(labels),
      if (!is.null(window) && !"day" %in% names(labels)) {
        paste("started", window, "UTC")
      }
    ),
    collapse = " \u00b7 "
  )
}

export_conversations <- function(
  data,
  ids,
  queue = NULL,
  link = scans_app_link(NULL, ""),
  application = "",
  scope = ""
) {
  trajectories <- cohort_trajectories(data, ids)
  calls <- cohort_tool_calls(data, ids)
  queue <- queue %||%
    cohort_review_queue(data, ids, trajectories = trajectories)
  review <- queue[match(ids, queue$trajectory_id), , drop = FALSE]
  count <- function(rows) tabulate(match(rows, ids), nbins = length(ids))
  tibble::tibble(
    trajectory_id = ids,
    started_at = export_time(trajectories$started_at),
    user = trajectories$user,
    model = trajectories$model,
    status = data$info$status[match(ids, data$info$trajectory_id)],
    class = trajectories$class,
    n_findings = trajectories$n_findings,
    n_error_findings = trajectories$n_errors,
    tool_calls = count(calls$trajectory_id),
    tool_failed = count(calls$trajectory_id[calls$failed]),
    elapsed_s = round(trajectories$elapsed, 3),
    tokens = trajectories$tokens,
    review_state = ifelse(
      is.na(review$reviewed),
      NA_character_,
      ifelse(review$reviewed, "reviewed", "needs review")
    ),
    review_label = review$label,
    reviewed_by = review$reviewed_by,
    reviewed_at = export_time(review$reviewed_at),
    link = vapply(ids, link, "", USE.NAMES = FALSE),
    application = rep(application, length(ids)),
    scope = rep(scope, length(ids))
  )
}

export_findings <- function(
  data,
  ids,
  link = scans_app_link(NULL, ""),
  application = "",
  scope = ""
) {
  findings <- data$findings[
    data$findings$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  findings <- findings[
    order(match(findings$trajectory_id, ids), findings$finding_id),
    ,
    drop = FALSE
  ]
  tibble::tibble(
    scan = findings$scan,
    severity = findings$severity,
    trajectory_id = findings$trajectory_id,
    turn_id = findings$turn_id,
    event_id = findings$event_id,
    event_ids = vapply(
      findings$event_ids,
      function(ids) paste(ids, collapse = " "),
      ""
    ),
    explanation = findings$explanation,
    link = vapply(findings$trajectory_id, link, "", USE.NAMES = FALSE),
    application = rep(application, nrow(findings)),
    scope = rep(scope, nrow(findings))
  )
}

export_csv <- function(table, file) {
  utils::write.csv(table, file, row.names = FALSE, na = "")
}

export_md_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  gsub("\\s+", " ", gsub("|", "\\|", x, fixed = TRUE))
}

export_md_table <- function(table) {
  if (nrow(table) == 0L) {
    return(character())
  }
  cells <- lapply(table, export_md_cell)
  c(
    paste0("| ", paste(names(table), collapse = " | "), " |"),
    paste0("|", paste(rep(" --- ", length(table)), collapse = "|"), "|"),
    paste0("| ", do.call(paste, c(cells, sep = " | ")), " |")
  )
}

export_md_seconds <- function(x) {
  vapply(
    x,
    function(value) if (is.na(value)) "unknown" else scans_app_seconds(value),
    ""
  )
}

export_md_count <- function(x) {
  vapply(
    x,
    function(value) {
      if (is.na(value)) "unknown" else scans_app_count(round(value))
    },
    ""
  )
}

# The report a reviewer pastes into an issue for the application's
# developers. It carries counts and links, never transcript text, and names
# users only when asked.
export_report <- function(
  performance,
  application,
  labels = NULL,
  link = scans_app_link(NULL, application),
  read_info = NULL,
  include_users = FALSE,
  generated = Sys.time()
) {
  cohort <- performance$cohort
  headline <- cohort$headline
  noun <- cohort$noun
  patterns <- performance$patterns
  queue <- utils::head(cohort$queue, 5L)
  progress <- cohort$progress
  window <- scans_app_day_range(headline$first_started, headline$last_started)
  section <- function(title, lines) {
    if (length(lines)) c(paste("##", title), "", lines, "")
  }
  c(
    paste0("# ", application, ": review of recorded ", noun$many),
    "",
    paste0(
      "- Scope: ",
      export_scope(headline, labels, noun),
      " ([open in scans](",
      link(),
      "))"
    ),
    paste0(
      "- Capture window: ",
      if (is.null(window)) {
        "no start times recorded"
      } else {
        paste(window, "UTC, by start time")
      },
      if (!is.null(read_info$from) || !is.null(read_info$to)) {
        paste0(
          "; source read ",
          scans_app_window_string(read_info$from, read_info$to)
        )
      }
    ),
    paste0(
      "- Generated ",
      format(generated, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      " by scans ",
      utils::packageVersion("scans")
    ),
    "",
    "## Headline",
    "",
    sprintf(
      "- %s of %s captured %s are in scope.",
      scans_app_count(headline$n),
      scans_app_count(headline$total),
      noun$many
    ),
    sprintf(
      "- %s of %s assessed %s have findings (%s with error-severity findings).",
      scans_app_count(headline$n_findings),
      scans_app_count(headline$n_assessed),
      noun$many,
      scans_app_count(headline$n_error)
    ),
    if (headline$n > headline$n_assessed) {
      sprintf(
        "- %s could not be assessed by any selected scan.",
        scans_app_n(headline$n - headline$n_assessed, noun)
      )
    },
    if (headline$tool_calls > 0L) {
      sprintf(
        "- %s of %s failed, in %s.",
        scans_app_count(headline$tool_failed),
        scans_app_plural(headline$tool_calls, "tool call"),
        scans_app_n(headline$tool_trajectories_failed, noun)
      )
    } else {
      "- No tool calls were recorded."
    },
    sprintf(
      "- Elapsed: median %s, p95 %s (timed for %s of %s).",
      export_md_seconds(headline$median_elapsed),
      export_md_seconds(headline$p95_elapsed),
      scans_app_count(headline$n_timed),
      scans_app_count(headline$n)
    ),
    sprintf(
      "- Tokens: median %s (recorded for %s of %s).",
      export_md_count(headline$median_tokens),
      scans_app_count(headline$n_tokens),
      scans_app_count(headline$n)
    ),
    if (!is.na(progress$flagged_reviewed)) {
      sprintf(
        "- Review: %s of %s flagged %s reviewed.",
        scans_app_count(progress$flagged_reviewed),
        scans_app_count(progress$flagged),
        noun$many
      )
    },
    "",
    section(
      "Patterns",
      if (nrow(patterns) == 0L) {
        sprintf(
          "No findings in %s.",
          scans_app_n(
            headline$n_assessed,
            list(
              one = paste("assessed", noun$one),
              many = paste("assessed", noun$many)
            )
          )
        )
      } else {
        export_md_table(tibble::tibble(
          Pattern = scans_app_pattern_label(patterns$scan),
          Severity = patterns$severity,
          Affected = sprintf("%d of %d", patterns$trajectories, headline$n),
          `Users affected` = ifelse(
            is.na(patterns$users),
            "unknown",
            patterns$users
          ),
          `Last seen` = format(patterns$last_seen, "%b %d", tz = "UTC"),
          Evidence = patterns$evidence
        ))
      }
    ),
    section(
      "Tools",
      export_md_table(tibble::tibble(
        Tool = paste0("`", cohort$tools$tool, "`"),
        Calls = cohort$tools$calls,
        Failed = sprintf(
          "%d (%s)",
          cohort$tools$failed,
          vapply(cohort$tools$failure_rate, scans_app_percent, "")
        ),
        `Top error` = ifelse(
          is.na(cohort$tools$top_error),
          "",
          sprintf(
            "`%s` (%d)",
            cohort$tools$top_error,
            cohort$tools$top_error_n
          )
        ),
        `Median` = export_md_seconds(cohort$tools$median_duration),
        p95 = export_md_seconds(cohort$tools$p95_duration)
      ))
    ),
    section(
      "Models",
      export_md_table(tibble::tibble(
        Model = cohort$models$value,
        Recorded = cohort$models$n,
        `With findings` = cohort$models$n_findings,
        `Median tokens` = export_md_count(cohort$models$median_tokens),
        `Median elapsed` = export_md_seconds(cohort$models$median_elapsed)
      ))
    ),
    if (include_users) {
      section(
        "Users",
        export_md_table(tibble::tibble(
          User = cohort$users$value,
          Recorded = cohort$users$n,
          `With findings` = cohort$users$n_findings
        ))
      )
    },
    section(
      "Examples to open first",
      c(
        sprintf(
          "%d. [`%s`](%s): %s%s",
          seq_len(nrow(queue)),
          queue$trajectory_id,
          vapply(queue$trajectory_id, link, "", USE.NAMES = FALSE),
          vapply(
            queue$reasons,
            function(reasons) {
              if (length(reasons)) {
                paste(reasons, collapse = ", ")
              } else {
                "no findings"
              }
            },
            ""
          ),
          if (include_users) {
            ifelse(is.na(queue$user), "", paste0(" (", queue$user, ")"))
          } else {
            ""
          }
        )
      )
    ),
    "## Caveats",
    "",
    "- Findings describe recorded behavior, not an outcome score.",
    "- No findings does not establish success. Source limits and missing captures can affect coverage.",
    "- Elapsed time may include user pauses; token totals may be partial.",
    if (isTRUE(read_info$truncated) || isTRUE(read_info$incomplete)) {
      "- The source read was cut short, so older or failed pages are missing."
    },
    if (!include_users) {
      "- User identifiers and transcript text are omitted from this report."
    } else {
      "- Transcript text is omitted from this report."
    }
  )
}

export_r_time <- function(time) {
  if (is.null(time)) {
    return("NULL")
  }
  sprintf(
    'as.POSIXct("%s", tz = "UTC")',
    format(time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
}

export_r_strings <- function(x, indent = "  ") {
  quoted <- encodeString(x, quote = '"')
  lines <- strwrap(paste(quoted, collapse = ", "), width = 76L)
  paste0("c(\n", paste0(indent, lines, collapse = "\n"), "\n)")
}

# A snippet that rebuilds the snapshot, reruns the active scans, and selects
# the trajectories in view. `recipe` describes a Connect read; without one
# the reader supplies the bundle the app was started with.
export_r_code <- function(recipe, scan_config, ids, application = "") {
  source <- if (is.null(recipe)) {
    c(
      "# Supply the TrajectoryBundle this app was started with, for example",
      "# from as_trajectory_otel() or read_investigation(file)$bundle.",
      "bundle <- NULL"
    )
  } else {
    reader <- if (identical(recipe$reader, "commons")) {
      c(
        "bundle <- commons::trajectory_read(",
        sprintf('  "%s",', recipe$guid),
        sprintf("  n = %s,", if (is.null(recipe$n)) "NULL" else recipe$n),
        sprintf("  from = %s,", export_r_time(recipe$from)),
        sprintf("  to = %s", export_r_time(recipe$to)),
        ") |>",
        "  as_trajectory_commons()"
      )
    } else {
      c(
        "bundle <- read_connect_traces(",
        sprintf('  "%s",', recipe$guid),
        sprintf("  n = %s,", if (is.null(recipe$n)) "NULL" else recipe$n),
        sprintf("  from = %s,", export_r_time(recipe$from)),
        sprintf("  to = %s,", export_r_time(recipe$to)),
        sprintf("  jobs = %s", recipe$jobs),
        ") |>",
        "  as_trajectory_otel()"
      )
    }
    c(
      "# Reads Posit Connect with CONNECT_SERVER and CONNECT_API_KEY from the",
      "# environment. Conversations recorded since this snapshot can appear too.",
      reader
    )
  }
  c(
    "library(scans)",
    "",
    if (nzchar(application)) paste("# Application:", application),
    source,
    "",
    "assessment <- assess_trajectory_scans(",
    "  bundle,",
    sprintf("  repeat_threshold = %dL,", scan_config$repeat_threshold),
    sprintf("  loop_threshold = %dL,", scan_config$loop_threshold),
    paste0(
      "  scans = ",
      gsub("\n", "\n  ", export_r_strings(scan_config$scans %||% character())),
      ""
    ),
    ")",
    "",
    sprintf("# The %d trajectories in view when this was copied.", length(ids)),
    paste0("ids <- ", export_r_strings(ids)),
    "",
    "trajectories <- trajectory_info(bundle)",
    "trajectories[trajectories$trajectory_id %in% ids, ]",
    "findings <- assessment$findings",
    "findings[findings$trajectory_id %in% ids, ]"
  )
}

# One trajectory as Markdown: facts, findings with their evidence, and the
# transcript up to `max_chars`. Unlike the report this contains content.
export_trajectory_markdown <- function(
  data,
  index,
  link = "",
  max_chars = 6000L
) {
  id <- data$info$trajectory_id[[index]]
  record <- data$records[index, , drop = FALSE]
  findings <- data$findings[data$findings$trajectory_id == id, , drop = FALSE]
  events <- data$events[data$events$trajectory_id == id, , drop = FALSE]
  events <- events[order(events$event_index), , drop = FALSE]
  roles <- data$turns$role[match(events$turn_id, data$turns$turn_id)]
  label <- function(ids) {
    rows <- match(ids, events$event_id)
    rows <- rows[!is.na(rows)]
    paste(
      sprintf(
        "%s, event %d",
        ifelse(
          is.na(events$name[rows]),
          events$event_type[rows],
          events$name[rows]
        ),
        events$event_index[rows]
      ),
      collapse = "; "
    )
  }
  lines <- vapply(
    seq_len(nrow(events)),
    function(row) {
      event <- events[row, , drop = FALSE]
      failed <- cohort_event_failed(event)
      text <- switch(
        event$event_type,
        tool_call = sprintf(
          "Called `%s`%s",
          event$name,
          if (failed) " (failed)" else ""
        ),
        tool_result = paste0(
          "Result",
          if (failed) " (failed)" else "",
          if (!is.na(event$text)) paste0(": ", event$text)
        ),
        if (is.na(event$text)) "" else event$text
      )
      if (scans_app_has_string(event$error)) {
        text <- paste0(text, " [error: ", event$error, "]")
      }
      who <- if (is.na(roles[[row]])) event$event_type else roles[[row]]
      if (nzchar(text)) sprintf("**%s:** %s", who, text) else ""
    },
    ""
  )
  lines <- lines[nzchar(lines)]
  kept <- cumsum(nchar(lines) + 2L) <= max_chars
  elapsed <- record$elapsed[[1L]]
  c(
    paste("#", record$title[[1L]]),
    "",
    paste0("- Trajectory: `", id, "`"),
    if (nzchar(link)) paste0("- Link: ", link),
    if (!is.na(record$started_at[[1L]])) {
      paste("- Started:", scans_app_time_string(record$started_at[[1L]]))
    },
    if (!is.na(record$model[[1L]])) {
      paste0("- Model: `", record$model[[1L]], "`")
    },
    if (!is.na(elapsed)) paste("- Elapsed:", scans_app_seconds(elapsed)),
    sprintf(
      "- %s recorded, %s failed",
      scans_app_plural(sum(events$event_type == "tool_call"), "tool call"),
      scans_app_count(record$n_tool_errors[[1L]])
    ),
    "",
    "## Findings",
    "",
    if (nrow(findings) == 0L) {
      "No findings. This does not establish success."
    } else {
      sprintf(
        "- **%s** (%s): %s%s",
        scans_app_pattern_label(findings$scan),
        findings$severity,
        findings$explanation,
        vapply(
          findings$event_ids,
          function(ids) {
            evidence <- label(ids)
            if (nzchar(evidence)) paste0(" Evidence: ", evidence, ".") else ""
          },
          ""
        )
      )
    },
    "",
    "## Transcript",
    "",
    "This section contains conversation content as recorded.",
    "",
    if (length(lines) == 0L) "No transcript was recorded.",
    paste(lines[kept], collapse = "\n\n"),
    if (!all(kept)) {
      c(
        "",
        sprintf(
          "Transcript cut at %s characters; %d of %d entries shown.",
          scans_app_count(max_chars),
          sum(kept),
          length(lines)
        )
      )
    }
  )
}
