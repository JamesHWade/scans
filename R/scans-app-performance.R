# Application measures use the same visible trajectory identities as the
# browser. Findings count affected trajectories, not repeated detector hits.
# `activity_indices` is the same cohort without the time facet, so the chart
# keeps its other buckets when one is selected.
scans_app_performance_data <- function(
  data,
  indices,
  activity_indices = indices
) {
  summaries <- data$summaries[indices, , drop = FALSE]
  ids <- summaries$trajectory_id
  trajectories <- cohort_trajectories(data, ids)
  calls <- cohort_tool_calls(data, ids)
  activity <- cohort_activity(
    data,
    data$info$trajectory_id[activity_indices]
  )
  patterns <- cohort_patterns(data, ids, trajectories, activity)
  headline <- cohort_headline(data, ids, trajectories, calls)
  summaries$index <- indices
  summaries$title <- data$records$title[indices]
  summaries$n_findings <- data$records$n_findings[indices]
  summaries$n_tool_errors <- data$records$n_tool_errors[indices]
  summaries$elapsed <- trajectories$elapsed
  summaries$tokens <- trajectories$tokens
  work <- scans_app_measure_rows(data, ids, "recorded_work")
  gap <- scans_app_measure_rows(data, ids, "unattributed_elapsed")

  list(
    total = headline$total,
    n = headline$n,
    n_findings = headline$n_findings,
    n_evaluated = sum(ids %in% data$evaluations$trajectory_id),
    n_losses = sum(ids %in% data$loss_trajectory_ids),
    unassigned_losses = sum(is.na(data$loss_trajectory_ids)),
    n_children = sum(!is.na(summaries$parent_trajectory_id)),
    n_timed = headline$n_timed,
    n_tokens = headline$n_tokens,
    median_elapsed = headline$median_elapsed,
    p95_elapsed = headline$p95_elapsed,
    median_tokens = headline$median_tokens,
    n_work = sum(work$coverage == "complete"),
    n_work_partial = sum(work$coverage == "partial"),
    median_work = scans_app_median(work$value[work$coverage == "complete"]),
    median_gap = scans_app_median(gap$value[gap$coverage == "complete"]),
    assessment_coverage = scans_app_assessment_coverage(data$assessments[
      data$assessments$trajectory_id %in% ids,
      ,
      drop = FALSE
    ]),
    patterns = patterns,
    cohort = list(
      headline = headline,
      activity = activity,
      tools = cohort_tools(data, ids, calls),
      models = cohort_breakdown(data, ids, "model", trajectories, calls),
      users = cohort_breakdown(data, ids, "user", trajectories, calls)
    ),
    trajectories = summaries
  )
}

scans_app_pattern_evidence <- function(findings) {
  scan <- findings$scan[[1L]]
  if (scan == "event_error") {
    return(sprintf("%d events report errors", nrow(findings)))
  }
  if (
    nrow(findings) == 1L &&
      scan %in% c("repeated_tool_call", "suspicious_tool_loop")
  ) {
    count <- findings$value[[1L]]$count
    if (is.numeric(count) && length(count) == 1L && !is.na(count)) {
      return(sprintf(
        if (scan == "repeated_tool_call") {
          "Same request made %d times"
        } else {
          "%d consecutive identical requests"
        },
        count
      ))
    }
  }
  if (scan == "cut_off_answer") {
    return(sprintf(
      "%d final answer%s cut off",
      nrow(findings),
      if (nrow(findings) == 1L) "" else "s"
    ))
  }
  if (scan == "run_intervention") {
    return(scans_app_intervention_evidence(findings))
  }
  sprintf("%d finding%s", nrow(findings), if (nrow(findings) == 1L) "" else "s")
}

# One count per intervention kind, in a fixed order, so the overview says
# what happened rather than only how often.
scans_app_intervention_evidence <- function(findings) {
  singular <- c(
    limit_wrap_up = "limit wrap-up",
    limit_stop = "limit stop",
    provider_fallback = "provider fallback",
    compaction_failure = "failed compaction",
    repeated_compaction = "run with repeated compaction",
    permission_denied = "denied permission",
    approval_pending = "pending approval",
    delegation_missing_evidence = "delegation missing evidence",
    delegation_unresolved_work = "delegation with unresolved work"
  )
  plural <- c(
    limit_wrap_up = "limit wrap-ups",
    limit_stop = "limit stops",
    provider_fallback = "provider fallbacks",
    compaction_failure = "failed compactions",
    repeated_compaction = "runs with repeated compaction",
    permission_denied = "denied permissions",
    approval_pending = "pending approvals",
    delegation_missing_evidence = "delegations missing evidence",
    delegation_unresolved_work = "delegations with unresolved work"
  )
  kinds <- vapply(
    findings$value,
    function(value) {
      kind <- value$kind
      if (rlang::is_string(kind) && kind %in% names(singular)) kind else NA
    },
    character(1)
  )
  counts <- table(factor(kinds, levels = names(singular)))
  counts <- counts[counts > 0L]
  if (!length(counts)) {
    return(sprintf(
      "%d finding%s",
      nrow(findings),
      if (nrow(findings) == 1L) "" else "s"
    ))
  }
  kinds <- names(counts)
  paste(
    sprintf(
      "%d %s",
      counts,
      ifelse(counts == 1L, singular[kinds], plural[kinds])
    ),
    collapse = ", "
  )
}

scans_app_median <- function(x) {
  if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
}

scans_app_quantile <- function(x, probability) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  unname(stats::quantile(x, probability, na.rm = TRUE))
}

scans_app_performance_order <- function(data, priority = "elapsed") {
  measure <- switch(
    priority,
    tokens = data$tokens,
    findings = data$n_findings,
    data$elapsed
  )
  data[order(-measure, data$trajectory_id, na.last = TRUE), , drop = FALSE]
}

scans_app_seconds <- function(x) {
  if (is.na(x)) {
    return("Not recorded")
  }
  if (x < 60) {
    return(sprintf("%.1f s", x))
  }
  if (x < 3600) {
    return(sprintf("%.1f min", x / 60))
  }
  sprintf("%.1f h", x / 3600)
}

# `scope` carries what narrowed the cohort (facets, pattern, findings switch)
# so the overview can name it and mark the matching controls as active.
scans_app_performance_ui <- function(
  data,
  application,
  priority,
  scans,
  chat = FALSE,
  scope = list()
) {
  n <- data$n
  cohort <- data$cohort
  facets <- scope$facets %||% list()
  next_run <- if (n > 0L) {
    scans_app_performance_order(data$trajectories, "findings")[1L, ]
  }
  htmltools::div(
    class = "scans-app-performance",
    `aria-label` = paste("Overview for", application),
    htmltools::div(
      class = "scans-app-overview-title",
      htmltools::tags$h1("Overview"),
      if (chat) {
        shiny::actionButton(
          "scans_app_ask_overview",
          "Ask about these trajectories",
          icon = shiny::icon("comment"),
          class = "btn-outline-secondary"
        )
      }
    ),
    scans_app_scope_sentence_ui(
      cohort$headline,
      scans_app_scope_labels(facets, scope$pattern, scope$findings_only)
    ),
    if (n == 0L) {
      scans_app_empty_ui(
        "No trajectories match these filters. Broaden the filters to continue."
      )
    } else {
      htmltools::tagList(
        scans_app_tiles_ui(
          cohort$headline,
          n_empty = sum(
            data$trajectories$n_turns == 0 & data$trajectories$n_events == 0
          ),
          scans_on = length(scans) > 0L,
          facets = facets,
          findings_only = isTRUE(scope$findings_only)
        ),
        htmltools::tags$section(
          class = "scans-app-performance-section",
          htmltools::tags$h2("Activity"),
          scans_app_activity_ui(cohort$activity, facets$day)
        ),
        htmltools::tags$section(
          class = "scans-app-performance-section",
          htmltools::tags$h2("Patterns to investigate"),
          scans_app_patterns_ui(data$patterns, n, scans, scope$pattern),
          htmltools::tags$p(
            class = "scans-app-pattern-overlap",
            if (nrow(data$patterns) > 1L && data$n_findings == 1L) {
              "These patterns all affect the same trajectory."
            } else {
              "A trajectory can appear in more than one pattern."
            }
          )
        ),
        if (nrow(cohort$tools) > 0L) {
          htmltools::tags$section(
            class = "scans-app-performance-section",
            htmltools::tags$h2("Tools"),
            scans_app_tools_ui(cohort$tools, facets)
          )
        },
        htmltools::div(
          class = "scans-app-breakdowns",
          scans_app_models_ui(cohort$models, n, facets),
          scans_app_users_ui(cohort$users, n, facets)
        ),
        htmltools::tags$section(
          class = "scans-app-performance-section",
          htmltools::tags$h2("Suggested next step"),
          htmltools::div(
            class = "scans-app-next-step",
            htmltools::div(
              htmltools::tags$strong(next_run$title[[1L]]),
              if (next_run$n_tool_errors[[1L]] > 0L) {
                htmltools::tags$p(sprintf(
                  "%d tool errors recorded in this trajectory.",
                  next_run$n_tool_errors[[1L]]
                ))
              },
              htmltools::tags$p(sprintf(
                "%d findings \u00b7 %s elapsed",
                next_run$n_findings[[1L]],
                scans_app_seconds(next_run$elapsed[[1L]])
              ))
            ),
            htmltools::tags$button(
              type = "button",
              class = "btn btn-primary",
              `data-scans-trajectory` = next_run$trajectory_id[[1L]],
              "Inspect trajectory ",
              shiny::icon("arrow-right")
            )
          )
        ),
        htmltools::tags$details(
          class = "scans-app-overview-disclosure",
          htmltools::tags$summary(
            "Timing and token usage",
            htmltools::tags$small(paste0(
              "Median elapsed ",
              scans_app_seconds(data$median_elapsed),
              " \u00b7 Median recorded tokens ",
              scans_app_count(data$median_tokens)
            ))
          ),
          htmltools::tags$p(sprintf(
            "Start to completion recorded for %d / %d. Input or output recorded for %d / %d.",
            data$n_timed,
            n,
            data$n_tokens,
            n
          )),
          htmltools::tags$p(paste0(
            "95th percentile elapsed: ",
            scans_app_seconds(data$p95_elapsed),
            "."
          )),
          htmltools::tags$p(paste0(
            "Median recorded work: ",
            scans_app_seconds(data$median_work),
            "; median time outside recorded work: ",
            scans_app_seconds(data$median_gap),
            "."
          )),
          htmltools::tags$p(
            "Elapsed time may include pauses. Recorded work counts overlapping spans once. Partial timing is excluded from work medians; tokens may be partial."
          ),
          htmltools::tags$p(sprintf(
            "All captured call intervals timed for %d / %d. Partial timing excluded from work medians: %d / %d. Complete timing of captured calls does not establish complete capture.",
            data$n_work,
            n,
            data$n_work_partial,
            n
          )),
          scans_app_performance_table(data$trajectories, priority)
        ),
        htmltools::tags$details(
          class = "scans-app-overview-disclosure",
          htmltools::tags$summary(
            "Evidence coverage",
            htmltools::tags$small(
              if (
                any(
                  data$assessment_coverage$status == "insufficient_evidence" &
                    data$assessment_coverage$trajectories > 0L
                )
              ) {
                "Some checks lack enough recorded evidence"
              } else {
                "Assessments cover the retained snapshot"
              }
            )
          ),
          scans_app_assessment_coverage_ui(data$assessment_coverage),
          htmltools::tags$p(sprintf(
            "%d / %d trajectories have linked evaluations. %d / %d have adapter losses. %d are delegated trajectories.",
            data$n_evaluated,
            n,
            data$n_losses,
            n,
            data$n_children
          )),
          if (data$unassigned_losses > 0L) {
            htmltools::tags$p(sprintf(
              "%d additional losses cannot be assigned to a trajectory.",
              data$unassigned_losses
            ))
          },
          htmltools::tags$p(
            "No findings does not establish success. Source limits and missing captures can affect coverage."
          )
        ),
        htmltools::tags$p(
          class = "scans-app-overview-footnote",
          "Findings describe recorded behavior, not an outcome score."
        )
      )
    }
  )
}

scans_app_pattern_label <- function(scan) {
  switch(
    scan,
    event_error = "Event errors",
    repeated_tool_call = "Repeated tool calls",
    suspicious_tool_loop = "Suspicious tool loop",
    scans_app_title_case(gsub("_", " ", scan))
  )
}

scans_app_patterns_ui <- function(patterns, n, scans, selected = NULL) {
  if (length(scans) == 0L) {
    return(scans_app_empty_ui(
      "No scans selected. Enable scans in Options to see diagnostic patterns.",
      compact = TRUE
    ))
  }
  if (nrow(patterns) == 0L) {
    return(scans_app_empty_ui(
      "No patterns to display. Check Scanner coverage for unassessed trajectories.",
      compact = TRUE
    ))
  }
  users_known <- !all(is.na(patterns$users))
  htmltools::div(
    class = paste(
      "scans-app-patterns",
      if (!users_known) "scans-app-patterns-anonymous"
    ),
    htmltools::div(
      class = "scans-app-pattern-head",
      `aria-hidden` = "true",
      htmltools::tags$span("Severity"),
      htmltools::tags$span("Pattern"),
      htmltools::tags$span("Affected"),
      if (users_known) htmltools::tags$span("Users"),
      htmltools::tags$span("Last seen"),
      htmltools::tags$span("Over time"),
      htmltools::tags$span()
    ),
    lapply(seq_len(nrow(patterns)), function(i) {
      row <- patterns[i, ]
      label <- scans_app_pattern_label(row$scan)
      htmltools::tags$button(
        type = "button",
        class = "scans-app-pattern",
        `data-scans-pattern` = row$scan,
        `aria-pressed` = if (identical(selected, row$scan)) "true" else "false",
        `aria-label` = sprintf(
          "%s, %s: %d of %d conversations. %s. Show these conversations.",
          label,
          row$severity,
          row$trajectories,
          n,
          row$evidence
        ),
        htmltools::tags$span(
          scans_app_badge(scans_app_title_case(row$severity), row$severity)
        ),
        htmltools::tags$span(
          class = "scans-app-pattern-name",
          htmltools::tags$strong(label),
          htmltools::tags$small(row$evidence)
        ),
        htmltools::tags$span(
          class = "scans-app-cell-meter",
          sprintf("%d of %d", row$trajectories, n),
          scans_app_meter(row$share, row$severity)
        ),
        if (users_known) {
          htmltools::tags$span(
            class = "scans-app-pattern-users",
            scans_app_count(row$users)
          )
        },
        htmltools::tags$span(
          class = "scans-app-pattern-seen",
          if (is.na(row$last_seen)) {
            htmltools::tags$span(class = "scans-app-unknown", "Unknown")
          } else {
            format(row$last_seen, "%b %d", tz = "UTC")
          }
        ),
        htmltools::tags$span(
          class = "scans-app-pattern-trend",
          scans_app_sparkline(row$series[[1L]], row$severity)
        ),
        shiny::icon("chevron-right")
      )
    })
  )
}

scans_app_performance_table <- function(data, priority) {
  rows <- utils::head(scans_app_performance_order(data, priority), 10L)
  htmltools::div(
    class = "scans-app-performance-table",
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$caption(sprintf(
        "Showing %d of %d matching trajectories. Use the browser for the full list.",
        nrow(rows),
        nrow(data)
      )),
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tagList(lapply(
          c("Trajectory", "Elapsed", "Recorded tokens", "Findings"),
          function(label) {
            htmltools::tags$th(scope = "col", label)
          }
        ))
      )),
      htmltools::tags$tbody(htmltools::tagList(lapply(
        seq_len(nrow(rows)),
        function(i) {
          htmltools::tags$tr(
            htmltools::tags$td(
              htmltools::tags$button(
                type = "button",
                class = "scans-app-inspect",
                `data-scans-trajectory` = rows$trajectory_id[[i]],
                rows$title[[i]]
              ),
              htmltools::tags$small(rows$trajectory_id[[i]])
            ),
            htmltools::tags$td(scans_app_seconds(rows$elapsed[[i]])),
            htmltools::tags$td(
              if (is.na(rows$tokens[[i]])) {
                "Not recorded"
              } else {
                scans_app_count(rows$tokens[[i]])
              }
            ),
            htmltools::tags$td(rows$n_findings[[i]])
          )
        }
      )))
    )
  )
}
