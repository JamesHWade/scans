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

# A result resolves the earliest pending call before it in its trajectory that
# carries the same call id, the pairing `scan_tool_relation_indices()` gives
# the tool scans, so a reused id never hands one result to two calls. A result
# no call claims keeps its own row, so a recorded failure is never dropped for
# want of a matching call.
cohort_tool_calls <- function(data, ids) {
  events <- data$events
  events <- events[
    events$event_type %in%
      c("tool_call", "tool_result") &
      events$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  events <- events[order(events$event_index, method = "radix"), , drop = FALSE]
  events$failed <- cohort_event_failed(events)
  events$error[!is.na(events$error) & !nzchar(events$error)] <- NA_character_
  events$key <- ifelse(
    is.na(events$call_id),
    NA_character_,
    paste(events$trajectory_id, events$call_id, sep = "\r")
  )
  is_call <- events$event_type == "tool_call"
  calls <- events[is_call, , drop = FALSE]
  results <- events[!is_call, , drop = FALSE]
  paired <- cohort_result_pairs(events$key, is_call)
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

# For each call, the row of its result among the results, or `NA`. `key` and
# `is_call` describe tool events in order. A key used by one call and one
# result pairs them directly; only reused keys need the ordered pass.
cohort_result_pairs <- function(key, is_call) {
  call_at <- which(is_call)
  result_at <- which(!is_call)
  paired <- match(key[call_at], key[result_at], incomparables = NA_character_)
  paired[which(result_at[paired] < call_at)] <- NA_integer_

  reused <- !is.na(key) & key %in% key[duplicated(paste(is_call, key))]
  if (!any(reused)) {
    return(paired)
  }
  rows <- which(reused)
  relations <- scan_tool_relation_indices(
    ifelse(is_call[rows], "tool_call", "tool_result"),
    key[rows]
  )
  resolved <- setdiff(rows[is_call[rows]], rows[relations$unresolved_calls])
  matched <- setdiff(rows[!is_call[rows]], rows[relations$unmatched_results])
  # Results resolve calls first in, first out, so within one key the nth
  # resolved call holds the nth matched result.
  turn <- function(key) {
    paste(key, stats::ave(seq_along(key), key, FUN = seq_along), sep = "\r")
  }
  paired[reused[call_at]] <- NA_integer_
  paired[match(resolved, call_at)] <- match(matched, result_at)[match(
    turn(key[resolved]),
    turn(key[matched])
  )]
  paired
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
    at = records$active_at %||% records$started_at,
    parent_trajectory_id = records$parent_trajectory_id %||%
      rep(NA_character_, length(ids)),
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
  as.POSIXct(
    floor(as.numeric(time) / width) * width,
    origin = "1970-01-01",
    tz = "UTC"
  )
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
  started <- trajectories$at
  known <- !is.na(started)
  if (is.null(unit)) {
    span <- if (any(known)) diff(range(as.numeric(started[known]))) else 0
    unit <- if (span <= 48 * 3600) "hour" else "day"
  }
  unit <- rlang::arg_match0(unit, names(cohort_bucket_seconds))
  if (!any(known)) {
    buckets <- as.POSIXct(numeric(), origin = "1970-01-01", tz = "UTC")
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
  attr(out, "basis") <- if (all(!known | !is.na(trajectories$started_at))) {
    "start"
  } else {
    "activity"
  }
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
    started <- affected$at[!is.na(affected$at)]
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
        as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
      },
      last_seen = if (length(started)) {
        max(started)
      } else {
        as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
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
  started <- trajectories$at[!is.na(trajectories$at)]
  # A lead run's usage already includes its delegated runs, so a total counts
  # a child only when its parent is outside the cohort.
  counted <- !trajectories$parent_trajectory_id %in% ids &
    !is.na(trajectories$tokens)
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
      as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    },
    last_started = if (length(started)) {
      max(started)
    } else {
      as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    },
    n_undated = sum(is.na(trajectories$at)),
    time_basis = if (
      all(is.na(trajectories$at) | !is.na(trajectories$started_at))
    ) {
      "start"
    } else {
      "activity"
    },
    total_tokens = if (any(counted)) {
      sum(trajectories$tokens[counted])
    } else {
      NA_real_
    },
    n_tokens_counted = sum(counted),
    n_delegated = sum(trajectories$parent_trajectory_id %in% ids)
  )
}

# Review state comes only from annotation records: a trajectory is reviewed
# once it has any record, and its label is the latest non-empty one. Without
# records (`NULL`, no store configured) nothing claims a state.
cohort_review_state <- function(annotations, ids) {
  if (is.null(annotations)) {
    return(tibble::tibble(
      trajectory_id = ids,
      reviewed = NA,
      label = NA_character_,
      n_annotations = NA_integer_,
      reviewed_at = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
      reviewed_by = NA_character_
    ))
  }
  records <- annotations[
    order(annotations$created_at, decreasing = TRUE, na.last = TRUE),
    ,
    drop = FALSE
  ]
  latest <- match(ids, records$trajectory_id)
  labelled <- records[
    !is.na(records$label) & nzchar(records$label),
    ,
    drop = FALSE
  ]
  tibble::tibble(
    trajectory_id = ids,
    reviewed = !is.na(latest),
    label = labelled$label[match(ids, labelled$trajectory_id)],
    n_annotations = tabulate(
      match(records$trajectory_id, ids),
      nbins = length(ids)
    ),
    reviewed_at = records$created_at[latest],
    reviewed_by = records$author[latest]
  )
}

# Short recorded facts explaining why a trajectory is in the queue, most
# important first.
cohort_review_reasons <- function(data, ids, class) {
  rows <- match(ids, data$records$trajectory_id)
  findings <- data$findings[
    data$findings$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  scans <- split(findings$scan, findings$trajectory_id)
  status <- data$info$status[match(ids, data$info$trajectory_id)]
  cut_off <- ids %in%
    data$turns$trajectory_id[scan_turn_is_truncated(data$turns$finish_reason)]
  tool_errors <- data$records$n_tool_errors[rows]
  empty <- data$records$n_turns[rows] == 0L & data$records$n_events[rows] == 0L
  lost <- ids %in% data$loss_trajectory_ids
  kinds <- split(data$interventions$kind, data$interventions$trajectory_id)
  lapply(seq_along(ids), function(i) {
    found <- unique(scans[[ids[[i]]]] %||% character())
    kind <- unique(stats::na.omit(kinds[[ids[[i]]]] %||% character()))
    quiet <- c(
      "trajectory_error",
      "run_intervention",
      if (tool_errors[[i]] > 0L) "event_error",
      if (cut_off[[i]]) "turn_error",
      if ("suspicious_tool_loop" %in% found) "repeated_tool_call"
    )
    listed <- intersect(unique(cohort_scan_wording$scan), found)
    other <- setdiff(found, cohort_scan_wording$scan)
    unique(as.character(c(
      if (
        "trajectory_error" %in% found || status[[i]] %in% c("failed", "error")
      ) {
        "Run failed"
      },
      if (tool_errors[[i]] > 0L) {
        sprintf(
          "%d tool error%s",
          tool_errors[[i]],
          if (tool_errors[[i]] == 1L) "" else "s"
        )
      },
      if (cut_off[[i]]) "Answer cut off",
      cohort_scan_word(setdiff(c(listed, sort(other)), quiet), "reason"),
      cohort_scan_word(rep("run_intervention", length(kind)), "reason", kind),
      if (empty[[i]]) "No transcript",
      if (lost[[i]]) "Capture gap",
      if (class[[i]] == "unassessed") "Not assessed"
    )))
  })
}

cohort_class_order <- c("error", "warning", "unassessed", "clean")

# The review queue is ordered by a lexicographic rule, not a score:
# unreviewed before reviewed; then class error, warning, unassessed, clean;
# then more error-severity findings; then more findings; then newest first;
# then the trajectory's position in the snapshot.
cohort_review_queue <- function(
  data,
  ids,
  annotations = NULL,
  trajectories = cohort_trajectories(data, ids)
) {
  state <- cohort_review_state(annotations, ids)
  index <- match(ids, data$info$trajectory_id)
  started <- as.numeric(trajectories$at)
  ordering <- order(
    state$reviewed %in% TRUE,
    match(trajectories$class, cohort_class_order),
    -trajectories$n_errors,
    -trajectories$n_findings,
    -ifelse(is.na(started), -Inf, started),
    index,
    method = "radix"
  )
  out <- tibble::tibble(
    trajectory_id = ids,
    rank = order(ordering),
    index = index,
    class = trajectories$class,
    reasons = cohort_review_reasons(data, ids, trajectories$class),
    reviewed = state$reviewed,
    label = state$label,
    n_annotations = state$n_annotations,
    reviewed_at = state$reviewed_at,
    reviewed_by = state$reviewed_by
  )
  out[ordering, , drop = FALSE]
}

# "Flagged" is a recorded class (error or warning findings), not a judgment.
# Without a store the reviewed counts are unknown, not zero.
cohort_review_progress <- function(queue) {
  is_flagged <- queue$class %in% c("error", "warning")
  known <- !anyNA(queue$reviewed)
  is_reviewed <- queue$reviewed %in% TRUE
  counts <- table(
    ifelse(is.na(queue$label), "(no label)", queue$label)[is_reviewed]
  )
  tibble::tibble(
    flagged = sum(is_flagged),
    flagged_reviewed = if (known) {
      sum(is_flagged & is_reviewed)
    } else {
      NA_integer_
    },
    total = nrow(queue),
    reviewed = if (known) sum(is_reviewed) else NA_integer_,
    labels = list(stats::setNames(as.integer(counts), names(counts)))
  )
}

# The kind of each run intervention finding, one row per finding.
cohort_intervention_kinds <- function(findings) {
  rows <- findings[findings$scan %in% "run_intervention", , drop = FALSE]
  tibble::tibble(
    trajectory_id = rows$trajectory_id,
    finding_id = rows$finding_id,
    kind = vapply(
      rows$value,
      function(value) {
        kind <- if (is.list(value)) value$kind
        if (rlang::is_string(kind)) kind else NA_character_
      },
      character(1)
    )
  )
}

# Whether each trajectory carries Deputy run evidence, by the same rule the
# Deputy scans use to decide what they can assess.
cohort_has_deputy_evidence <- function(info, events) {
  groups <- split(seq_len(nrow(events)), events$trajectory_id)
  vapply(
    seq_len(nrow(info)),
    function(row) {
      scan_has_deputy_evidence(
        info[row, , drop = FALSE],
        events[
          groups[[info$trajectory_id[[row]]]] %||% integer(),
          ,
          drop = FALSE
        ]
      )
    },
    logical(1)
  )
}

# Run interventions by kind over the runs that carry Deputy evidence. The
# denominator and any stated capture limitation travel as attributes.
cohort_interventions <- function(
  data,
  ids,
  trajectories = cohort_trajectories(data, ids)
) {
  rows <- data$interventions[
    data$interventions$trajectory_id %in% ids & !is.na(data$interventions$kind),
    ,
    drop = FALSE
  ]
  kinds <- cohort_scan_wording$kind[
    cohort_scan_wording$scan == "run_intervention" &
      !is.na(cohort_scan_wording$kind)
  ]
  kinds <- c(intersect(kinds, rows$kind), sort(setdiff(rows$kind, kinds)))
  at <- trajectories$at[match(rows$trajectory_id, trajectories$trajectory_id)]
  out <- tibble::tibble(
    kind = kinds,
    label = cohort_scan_word("run_intervention", "label", kinds),
    trajectories = vapply(
      kinds,
      function(kind) length(unique(rows$trajectory_id[rows$kind == kind])),
      integer(1),
      USE.NAMES = FALSE
    ),
    findings = vapply(
      kinds,
      function(kind) sum(rows$kind == kind),
      integer(1),
      USE.NAMES = FALSE
    ),
    last_seen = do.call(
      c,
      c(
        list(as.POSIXct(character(), tz = "UTC")),
        lapply(kinds, function(kind) {
          seen <- at[rows$kind == kind & !is.na(at)]
          if (length(seen)) {
            max(seen)
          } else {
            as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
          }
        })
      )
    )
  )
  out <- out[order(-out$trajectories, -out$findings), , drop = FALSE]
  assessed <- data$assessments[
    data$assessments$scan %in%
      "run_intervention" &
      data$assessments$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  generic <- "Assessment covers retained records only; complete capture is not established."
  attr(out, "n_deputy") <- sum(
    data$deputy[match(ids, data$info$trajectory_id)]
  )
  attr(out, "limitations") <- setdiff(
    unique(unlist(assessed$limitations)),
    generic
  )
  out
}

cohort_provenance_classes <- c(
  trusted = "Trusted",
  cited = "Cited",
  untrusted = "Untrusted",
  unclassified = "Unclassified"
)

# Exchanges by recorded provenance class, over the conversations in scope
# that are provenance sources. Unclassified is its own class and is never
# folded into untrusted. `series` aligns to the activity buckets, placing an
# exchange at its conversation's time.
cohort_provenance <- function(
  data,
  ids,
  trajectories = cohort_trajectories(data, ids),
  activity = cohort_activity(data, ids, trajectories = trajectories)
) {
  rows <- data$exchanges
  if (is.null(rows)) {
    rows <- tibble::tibble(
      trajectory_id = character(),
      provenance_class = character()
    )
  }
  rows <- rows[
    rows$trajectory_id %in%
      ids &
      rows$provenance_class %in% names(cohort_provenance_classes),
    ,
    drop = FALSE
  ]
  unit <- attr(activity, "unit")
  at <- trajectories$at[match(
    rows$trajectory_id,
    trajectories$trajectory_id
  )]
  bucket <- match(
    as.numeric(cohort_bucket(at, unit)),
    as.numeric(activity$bucket)
  )
  classes <- names(cohort_provenance_classes)
  total <- nrow(rows)
  out <- tibble::tibble(
    class = classes,
    label = unname(cohort_provenance_classes),
    exchanges = vapply(
      classes,
      function(class) sum(rows$provenance_class == class),
      integer(1),
      USE.NAMES = FALSE
    ),
    trajectories = vapply(
      classes,
      function(class) {
        length(unique(
          rows$trajectory_id[rows$provenance_class == class]
        ))
      },
      integer(1),
      USE.NAMES = FALSE
    ),
    share = if (total > 0L) {
      vapply(
        classes,
        function(class) sum(rows$provenance_class == class) / total,
        double(1),
        USE.NAMES = FALSE
      )
    } else {
      rep(NA_real_, length(classes))
    },
    series = lapply(classes, function(class) {
      tabulate(
        bucket[rows$provenance_class == class],
        nbins = nrow(activity)
      )
    })
  )
  attr(out, "total_exchanges") <- total
  attr(out, "total_trajectories") <- length(unique(rows$trajectory_id))
  attr(out, "n_undated") <- sum(is.na(bucket))
  out
}

# Review next offers coverage rather than the head of the queue: the top
# unreviewed trajectory of each of the most prevalent patterns, one per
# pattern, then the best of the rest.
cohort_review_next <- function(queue, findings, patterns, n = 5L) {
  open <- queue[!queue$reviewed %in% TRUE, , drop = FALSE]
  chosen <- character()
  pattern <- character()
  for (scan in patterns$scan) {
    if (length(chosen) >= n) {
      break
    }
    affected <- findings$trajectory_id[findings$scan == scan]
    pick <- setdiff(
      open$trajectory_id[open$trajectory_id %in% affected],
      chosen
    )
    if (length(pick)) {
      chosen <- c(chosen, pick[[1L]])
      pattern <- c(pattern, scan)
    }
  }
  rest <- utils::head(setdiff(open$trajectory_id, chosen), n - length(chosen))
  out <- queue[match(c(chosen, rest), queue$trajectory_id), , drop = FALSE]
  out$pattern <- c(pattern, rep(NA_character_, length(rest)))
  out
}

cohort_timeline_lanes <- c(
  model = "Model",
  tool = "Tools",
  lifecycle = "Run events",
  provenance = "Provenance",
  other = "Other"
)

# One mark per timed event of a trajectory, placed on a compressed time axis.
# An idle gap longer than `max_gap` seconds (elapsed time includes user
# pauses) is shortened to a fixed break so the recorded work stays readable;
# `breaks` records where. A mark with no recorded duration is a tick, never a
# zero-length bar. Positions `x` and `width` are fractions of the axis.
cohort_timeline <- function(data, id, max_gap = 30) {
  events <- data$events[
    data$events$trajectory_id %in% id & !is.na(data$events$timestamp),
    ,
    drop = FALSE
  ]
  events <- events[
    order(events$timestamp, events$event_index, method = "radix"),
    ,
    drop = FALSE
  ]
  type <- events$event_type
  lane <- ifelse(
    type %in% c("tool_call", "tool_result"),
    "tool",
    ifelse(
      scan_is_deputy_event(type),
      "lifecycle",
      ifelse(
        type %in% "commons:provenance",
        "provenance",
        ifelse(type %in% "content", "model", "other")
      )
    )
  )
  turn <- match(events$turn_id, data$turns$turn_id)
  first_in_turn <- !duplicated(events$turn_id) & !is.na(events$turn_id)
  duration <- ifelse(
    lane == "model" & first_in_turn & is.na(events$duration),
    data$turns$duration[turn],
    events$duration
  )
  duration[!is.na(duration) & duration <= 0] <- NA_real_
  calls <- data$tool_calls
  start <- as.numeric(events$timestamp)
  end <- start + ifelse(is.na(duration), 0, duration)
  covered <- cummax(end)
  gap <- c(0, start[-1L] - covered[-length(covered)])
  gap[gap < 0] <- 0
  long <- gap > max_gap
  active <- if (length(start)) max(end) - min(start) - sum(gap[long]) else 0
  break_width <- if (any(long)) max(active, 1) * 0.06 else 0
  shift <- cumsum(ifelse(long, gap - break_width, 0))
  total <- max(active + sum(long) * break_width, .Machine$double.eps)
  position <- (start - min(start, Inf) - shift) / total
  out <- tibble::tibble(
    event_id = events$event_id,
    lane = lane,
    label = ifelse(
      is.na(events$name),
      gsub("^deputy:|^commons:", "", type),
      events$name
    ),
    timestamp = events$timestamp,
    duration = duration,
    failed = cohort_event_failed(events) |
      events$event_id %in% calls$event_id[calls$failed],
    x = position,
    width = ifelse(is.na(duration), NA_real_, duration / total)
  )
  attr(out, "breaks") <- tibble::tibble(
    x = (position[long] - break_width / total),
    width = rep(break_width / total, sum(long)),
    seconds = gap[long]
  )
  out
}
