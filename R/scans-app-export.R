# Exports are built from the same visible ids and cohort tables the screen
# shows. Each builder is a plain function so its content can be checked
# without Shiny. None of them reads credentials: a link is the app's own URL
# and the R snippet names environment variables without their values.

# A shareable hash: "<application>/<trajectory_id>", optionally followed by
# "?scope=key=value&..." naming the pattern, facets, findings switch, and
# review filter, and the source, status, text, and annotation filters. A
# scope with no filters is written "?scope=none", so the link still says
# which filters it sets: none. A hash without "?scope=" (the address bar's
# own, and older links) sets no filters.
scans_app_scope_keys <- c(
  "source",
  "status",
  "status_unknown",
  "query",
  "annotated",
  "pattern",
  "findings",
  "review",
  "review_label",
  "day",
  "tool",
  "tool_failed",
  "user",
  "model",
  "kind",
  "provenance",
  "route"
)

scans_app_scope_hash <- function(
  application,
  trajectory_id = NULL,
  scope = NULL
) {
  encode <- function(x) utils::URLencode(x, reserved = TRUE, repeated = TRUE)
  explicit <- !is.null(scope)
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
    } else if (explicit) {
      "?scope=none"
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
  flags <- c("findings", "tool_failed", "status_unknown", "annotated")
  for (flag in intersect(flags, names(scope))) {
    scope[[flag]] <- if (identical(scope[[flag]], "TRUE")) TRUE
  }
  scope
}

# `base` is the app's own address; with none (a test session) links are
# relative fragments. `users = FALSE` leaves the `private` scope keys out of
# the link, for exports that must not name anyone: the user filter, and the
# text filter when its term names a user.
scans_app_link <- function(
  base,
  application,
  scope = list(),
  private = "user"
) {
  force(base)
  force(application)
  force(scope)
  force(private)
  function(trajectory_id = NULL, users = TRUE) {
    if (!users) {
      scope[private] <- NULL
    }
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
# its own still states its scope and when those rows started. A user filter
# still narrows the cohort, so without `include_users` the scope says there
# is one without saying who, and so does a text filter whose term names a
# user (`query_names_user`).
export_scope <- function(
  headline,
  labels = NULL,
  noun = scans_app_noun(),
  include_users = TRUE,
  query_names_user = FALSE
) {
  if (!include_users && "user" %in% names(labels)) {
    labels[["user"]] <- "One user (identifier omitted)"
  }
  if (!include_users && query_names_user && "query" %in% names(labels)) {
    labels[["query"]] <- report_query_hidden_label
  }
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
  scope = "",
  include_users = FALSE
) {
  trajectories <- cohort_trajectories(data, ids)
  calls <- cohort_tool_calls(data, ids)
  queue <- queue %||%
    cohort_review_queue(data, ids, trajectories = trajectories)
  review <- queue[match(ids, queue$trajectory_id), , drop = FALSE]
  count <- function(rows) tabulate(match(rows, ids), nbins = length(ids))
  # A trajectory no scan could assess has no finding count, not a count of 0.
  found <- function(n) {
    n <- as.integer(n)
    n[trajectories$class %in% "unassessed"] <- NA_integer_
    n
  }
  out <- tibble::tibble(
    trajectory_id = ids,
    started_at = export_time(trajectories$started_at),
    user = trajectories$user,
    model = trajectories$model,
    status = data$info$status[match(ids, data$info$trajectory_id)],
    class = trajectories$class,
    assessed = trajectories$assessed,
    n_findings = found(trajectories$n_findings),
    n_error_findings = found(trajectories$n_errors),
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
    link = vapply(ids, link, "", users = include_users, USE.NAMES = FALSE),
    application = rep(application, length(ids)),
    scope = rep(scope, length(ids))
  )
  # A blank column would read as no user recorded, so it is left out.
  if (!include_users) {
    out$user <- NULL
  }
  out
}

export_findings <- function(
  data,
  ids,
  link = scans_app_link(NULL, ""),
  application = "",
  scope = "",
  include_users = FALSE
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
    link = vapply(
      findings$trajectory_id,
      link,
      "",
      users = include_users,
      USE.NAMES = FALSE
    ),
    application = rep(application, nrow(findings)),
    scope = rep(scope, nrow(findings))
  )
}

export_csv <- function(table, file) {
  utils::write.csv(table, file, row.names = FALSE, na = "")
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

# `c()` is `NULL`, which a scans argument reads as every scan, so an empty
# vector and `NULL` are each written as themselves. Lines are filled item by
# item because wrapping the joined text would rewrite spaces inside a string.
export_r_strings <- function(x, indent = "  ") {
  if (is.null(x)) {
    return("NULL")
  }
  if (length(x) == 0L) {
    return("character()")
  }
  items <- paste0(
    encodeString(x, quote = '"'),
    rep(c(",", ""), c(length(x) - 1L, 1L))
  )
  line <- integer(length(items))
  used <- 0L
  for (i in seq_along(items)) {
    fits <- used > 0L && used + 1L + nchar(items[[i]]) <= 75L
    line[[i]] <- if (i == 1L) 1L else line[[i - 1L]] + !fits
    used <- if (fits) used + 1L + nchar(items[[i]]) else nchar(items[[i]])
  }
  lines <- vapply(split(items, line), paste, "", collapse = " ")
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
      gsub("\n", "\n  ", export_r_strings(scan_config$scans)),
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
    if (nrow(findings) == 0L && !cohort_assessed(data, id)) {
      "Not assessed: no selected scan could assess this trajectory."
    } else if (nrow(findings) == 0L) {
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
