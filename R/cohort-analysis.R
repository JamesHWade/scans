# Cohort calculations for the review app. Each takes the plain tables from
# `scans_app_data()` and the visible trajectory ids, and returns a tibble, so
# every number the overview shows can be reproduced without Shiny. They stay
# internal until a second consumer settles which need a public interface.

# One rule for a failed tool event, shared by the browser, the overview, and
# the tool facet.
cohort_event_failed <- function(events) {
  (!is.na(events$error) & nzchar(events$error)) |
    events$status %in% c("failed", "error")
}

# A result is paired with the first call in its trajectory that carries the
# same call id. A result no call claims keeps its own row, so a recorded
# failure is never dropped for want of a matching call.
cohort_tool_calls <- function(data, ids) {
  events <- data$events
  events <- events[
    events$event_type %in%
      c("tool_call", "tool_result") &
      events$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  events$failed <- cohort_event_failed(events)
  events$error[!is.na(events$error) & !nzchar(events$error)] <- NA_character_
  events$key <- ifelse(
    is.na(events$call_id),
    NA_character_,
    paste(events$trajectory_id, events$call_id, sep = "\r")
  )
  calls <- events[events$event_type == "tool_call", , drop = FALSE]
  results <- events[events$event_type == "tool_result", , drop = FALSE]
  paired <- match(calls$key, results$key, incomparables = NA_character_)
  orphans <- results[
    !seq_len(nrow(results)) %in% paired,
    ,
    drop = FALSE
  ]
  result_failed <- results$failed[paired]
  result_error <- results$error[paired]
  result_duration <- results$duration[paired]

  out <- tibble::tibble(
    trajectory_id = c(calls$trajectory_id, orphans$trajectory_id),
    tool = c(calls$name, orphans$name),
    call_id = c(calls$call_id, orphans$call_id),
    event_id = c(calls$event_id, orphans$event_id),
    failed = c(calls$failed | result_failed %in% TRUE, orphans$failed),
    error = c(
      ifelse(is.na(result_error), calls$error, result_error),
      orphans$error
    ),
    duration = c(
      ifelse(is.na(calls$duration), result_duration, calls$duration),
      orphans$duration
    ),
    timestamp = c(calls$timestamp, orphans$timestamp)
  )
  out$tool[!is.na(out$tool) & !nzchar(out$tool)] <- NA_character_
  out[
    order(
      match(out$trajectory_id, ids),
      match(out$event_id, events$event_id),
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
}

cohort_unknown <- "Unknown"

cohort_label <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- cohort_unknown
  x
}

cohort_tools <- function(data, ids, calls = cohort_tool_calls(data, ids)) {
  calls$tool <- cohort_label(calls$tool)
  tools <- unique(calls$tool)
  rows <- lapply(tools, function(tool) {
    rows <- calls[calls$tool == tool, , drop = FALSE]
    errors <- rows$error[rows$failed & !is.na(rows$error)]
    counts <- sort(table(errors), decreasing = TRUE)
    tibble::tibble(
      tool = tool,
      calls = nrow(rows),
      failed = sum(rows$failed),
      failure_rate = sum(rows$failed) / nrow(rows),
      trajectories = length(unique(rows$trajectory_id)),
      trajectories_failed = length(unique(rows$trajectory_id[rows$failed])),
      top_error = if (length(counts)) names(counts)[[1L]] else NA_character_,
      top_error_n = if (length(counts)) counts[[1L]] else 0L,
      median_duration = scans_app_median(rows$duration),
      p95_duration = scans_app_quantile(rows$duration, 0.95),
      n_timed = sum(!is.na(rows$duration))
    )
  })
  if (!length(rows)) {
    return(tibble::tibble(
      tool = character(),
      calls = integer(),
      failed = integer(),
      failure_rate = double(),
      trajectories = integer(),
      trajectories_failed = integer(),
      top_error = character(),
      top_error_n = integer(),
      median_duration = double(),
      p95_duration = double(),
      n_timed = integer()
    ))
  }
  out <- trajectory_bind_rows(rows)
  out[order(-out$failed, -out$calls, out$tool), , drop = FALSE]
}

# Per-trajectory facts every cohort table groups by. `class` partitions the
# cohort: a trajectory with no findings is "clean" only when a selected scan
# could assess it, because an unassessed trajectory is not a negative result.
cohort_trajectories <- function(data, ids) {
  rows <- match(ids, data$records$trajectory_id)
  records <- data$records[rows, , drop = FALSE]
  assessed <- ids %in%
    data$assessments$trajectory_id[
      data$assessments$status %in%
        c("assessed_no_findings", "assessed_with_findings")
    ]
  usage <- scans_app_token_usage(data, ids)
  tokens <- rowSums(usage, na.rm = TRUE)
  tokens[rowSums(!is.na(usage)) == 0L] <- NA_real_
  tibble::tibble(
    trajectory_id = ids,
    started_at = records$started_at,
    user = records$user,
    model = records$model,
    source_type = records$source_type,
    n_findings = records$n_findings,
    n_errors = records$n_errors,
    assessed = assessed,
    class = ifelse(
      records$n_errors > 0L,
      "error",
      ifelse(
        records$n_findings > 0L,
        "warning",
        ifelse(assessed, "clean", "unassessed")
      )
    ),
    elapsed = scans_app_measure_rows(data, ids, "elapsed")$value,
    tokens = tokens
  )
}

cohort_bucket_seconds <- c(hour = 3600, day = 86400)

cohort_bucket <- function(time, unit) {
  width <- cohort_bucket_seconds[[unit]]
  as.POSIXct(floor(as.numeric(time) / width) * width, tz = "UTC")
}

# The key a `day` facet carries. Its format states the unit, so a saved key
# still selects the same bucket after the chart changes resolution.
cohort_bucket_key <- function(time, unit) {
  format(time, if (unit == "hour") "%Y-%m-%dT%H" else "%Y-%m-%d", tz = "UTC")
}

cohort_bucket_key_unit <- function(key) {
  if (grepl("T", key, fixed = TRUE)) "hour" else "day"
}

cohort_activity <- function(
  data,
  ids,
  unit = NULL,
  trajectories = cohort_trajectories(data, ids)
) {
  started <- trajectories$started_at
  known <- !is.na(started)
  if (is.null(unit)) {
    span <- if (any(known)) diff(range(as.numeric(started[known]))) else 0
    unit <- if (span <= 48 * 3600) "hour" else "day"
  }
  unit <- rlang::arg_match0(unit, names(cohort_bucket_seconds))
  if (!any(known)) {
    buckets <- as.POSIXct(numeric(), tz = "UTC")
  } else {
    span <- range(cohort_bucket(started[known], unit))
    buckets <- seq(span[[1L]], span[[2L]], by = cohort_bucket_seconds[[unit]])
  }
  group <- match(
    as.numeric(cohort_bucket(started, unit)),
    as.numeric(buckets)
  )
  count <- function(class) {
    tabulate(group[trajectories$class == class], nbins = length(buckets))
  }
  out <- tibble::tibble(
    bucket = buckets,
    n = tabulate(group, nbins = length(buckets)),
    n_error = count("error"),
    n_warning = count("warning"),
    n_clean = count("clean"),
    n_unassessed = count("unassessed")
  )
  attr(out, "unit") <- unit
  attr(out, "n_undated") <- sum(!known)
  out
}

cohort_patterns <- function(
  data,
  ids,
  trajectories = cohort_trajectories(data, ids),
  activity = cohort_activity(data, ids, trajectories = trajectories)
) {
  findings <- data$findings[
    data$findings$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  scans <- sort(unique(findings$scan))
  unit <- attr(activity, "unit")
  users_known <- !all(is.na(trajectories$user))
  rows <- lapply(scans, function(scan) {
    rows <- findings[findings$scan == scan, , drop = FALSE]
    affected <- trajectories[
      trajectories$trajectory_id %in% rows$trajectory_id,
      ,
      drop = FALSE
    ]
    started <- affected$started_at[!is.na(affected$started_at)]
    tibble::tibble(
      scan = scan,
      severity = if (any(rows$severity %in% "error")) "error" else "warning",
      trajectories = nrow(affected),
      share = nrow(affected) / length(ids),
      findings = nrow(rows),
      users = if (users_known) {
        length(unique(affected$user[!is.na(affected$user)]))
      } else {
        NA_integer_
      },
      first_seen = if (length(started)) {
        min(started)
      } else {
        as.POSIXct(NA_real_, tz = "UTC")
      },
      last_seen = if (length(started)) {
        max(started)
      } else {
        as.POSIXct(NA_real_, tz = "UTC")
      },
      series = list(tabulate(
        match(
          as.numeric(cohort_bucket(started, unit)),
          as.numeric(activity$bucket)
        ),
        nbins = nrow(activity)
      )),
      evidence = scans_app_pattern_evidence(rows)
    )
  })
  if (!length(rows)) {
    return(tibble::tibble(
      scan = character(),
      severity = character(),
      trajectories = integer(),
      share = double(),
      findings = integer(),
      users = integer(),
      first_seen = as.POSIXct(character(), tz = "UTC"),
      last_seen = as.POSIXct(character(), tz = "UTC"),
      series = list(),
      evidence = character()
    ))
  }
  out <- trajectory_bind_rows(rows)
  out[order(-out$trajectories, out$scan), , drop = FALSE]
}

cohort_breakdown <- function(
  data,
  ids,
  by = c("model", "user", "source_type"),
  trajectories = cohort_trajectories(data, ids),
  calls = cohort_tool_calls(data, ids)
) {
  by <- rlang::arg_match(by)
  group <- cohort_label(trajectories[[by]])
  call_group <- group[match(calls$trajectory_id, trajectories$trajectory_id)]
  values <- unique(group)
  rows <- lapply(values, function(value) {
    rows <- trajectories[group == value, , drop = FALSE]
    tibble::tibble(
      value = value,
      n = nrow(rows),
      n_findings = sum(rows$n_findings > 0L),
      n_error = sum(rows$n_errors > 0L),
      tool_calls = sum(call_group == value),
      tool_failed = sum(calls$failed[call_group == value]),
      median_elapsed = scans_app_median(rows$elapsed),
      n_timed = sum(!is.na(rows$elapsed)),
      median_tokens = scans_app_median(rows$tokens),
      n_tokens = sum(!is.na(rows$tokens))
    )
  })
  if (!length(rows)) {
    return(tibble::tibble(
      value = character(),
      n = integer(),
      n_findings = integer(),
      n_error = integer(),
      tool_calls = integer(),
      tool_failed = integer(),
      median_elapsed = double(),
      n_timed = integer(),
      median_tokens = double(),
      n_tokens = integer()
    ))
  }
  out <- trajectory_bind_rows(rows)
  out[
    order(out$value == cohort_unknown, -out$n, out$value),
    ,
    drop = FALSE
  ]
}

cohort_headline <- function(
  data,
  ids,
  trajectories = cohort_trajectories(data, ids),
  calls = cohort_tool_calls(data, ids)
) {
  started <- trajectories$started_at[!is.na(trajectories$started_at)]
  tibble::tibble(
    n = length(ids),
    total = nrow(data$info),
    n_findings = sum(trajectories$n_findings > 0L),
    n_error = sum(trajectories$n_errors > 0L),
    n_assessed = sum(trajectories$assessed),
    tool_calls = nrow(calls),
    tool_failed = sum(calls$failed),
    tool_trajectories = length(unique(calls$trajectory_id)),
    tool_trajectories_failed = length(unique(calls$trajectory_id[
      calls$failed
    ])),
    median_elapsed = scans_app_median(trajectories$elapsed),
    p95_elapsed = scans_app_quantile(trajectories$elapsed, 0.95),
    n_timed = sum(!is.na(trajectories$elapsed)),
    median_tokens = scans_app_median(trajectories$tokens),
    n_tokens = sum(!is.na(trajectories$tokens)),
    first_started = if (length(started)) {
      min(started)
    } else {
      as.POSIXct(NA_real_, tz = "UTC")
    },
    last_started = if (length(started)) {
      max(started)
    } else {
      as.POSIXct(NA_real_, tz = "UTC")
    },
    n_undated = sum(is.na(trajectories$started_at))
  )
}
