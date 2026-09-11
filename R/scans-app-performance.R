# Application measures use the same visible trajectory identities as the
# browser. Findings count affected trajectories, not repeated detector hits.
scans_app_performance_data <- function(data, indices) {
  summaries <- data$summaries[indices, , drop = FALSE]
  ids <- summaries$trajectory_id
  findings <- data$findings[
    data$findings$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  patterns <- sort(unique(findings$scan))
  evidence <- vapply(
    patterns,
    function(scan) {
      scans_app_pattern_evidence(findings[findings$scan == scan, ])
    },
    character(1),
    USE.NAMES = FALSE
  )
  patterns <- tibble::tibble(
    scan = patterns,
    trajectories = vapply(
      patterns,
      function(scan) {
        length(unique(findings$trajectory_id[findings$scan == scan]))
      },
      integer(1),
      USE.NAMES = FALSE
    ),
    findings = vapply(
      patterns,
      function(scan) {
        sum(findings$scan == scan)
      },
      integer(1),
      USE.NAMES = FALSE
    ),
    evidence = evidence
  )
  patterns <- patterns[order(-patterns$trajectories, patterns$scan), ]
  summaries$index <- indices
  summaries$title <- data$records$title[indices]
  summaries$n_findings <- data$records$n_findings[indices]
  summaries$n_tool_errors <- data$records$n_tool_errors[indices]
  summaries$elapsed <- scans_app_measure_rows(data, ids, "elapsed")$value
  usage <- scans_app_token_usage(data, ids)
  summaries$tokens <- rowSums(usage, na.rm = TRUE)
  summaries$tokens[rowSums(!is.na(usage)) == 0L] <- NA_real_
  work <- scans_app_measure_rows(data, ids, "recorded_work")
  gap <- scans_app_measure_rows(data, ids, "unattributed_elapsed")

  list(
    total = nrow(data$info),
    n = length(ids),
    n_findings = length(unique(findings$trajectory_id)),
    n_evaluated = sum(ids %in% data$evaluations$trajectory_id),
    n_losses = sum(ids %in% data$loss_trajectory_ids),
    unassigned_losses = sum(is.na(data$loss_trajectory_ids)),
    n_children = sum(!is.na(summaries$parent_trajectory_id)),
    n_timed = sum(!is.na(summaries$elapsed)),
    n_tokens = sum(!is.na(summaries$tokens)),
    median_elapsed = scans_app_median(summaries$elapsed),
    p95_elapsed = scans_app_quantile(summaries$elapsed, 0.95),
    median_tokens = scans_app_median(summaries$tokens),
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
  sprintf("%d finding%s", nrow(findings), if (nrow(findings) == 1L) "" else "s")
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

scans_app_performance_ui <- function(
  data,
  application,
  priority,
  scans,
  chat = FALSE
) {
  n <- data$n
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
    htmltools::tags$p(sprintf(
      "%d of %d captured trajectories match your filters.",
      n,
      data$total
    )),
    if (n == 0L) {
      scans_app_empty_ui(
        "No trajectories match these filters. Broaden the filters to continue."
      )
    } else {
      htmltools::tagList(
        htmltools::tags$p(
          class = "scans-app-scope-facts",
          if (!length(scans)) "Scans off \u00b7 ",
          sprintf(
            "%d with findings \u00b7 %d with recorded timing \u00b7 %d without a transcript",
            data$n_findings,
            data$n_timed,
            sum(
              data$trajectories$n_turns == 0 & data$trajectories$n_events == 0
            )
          )
        ),
        htmltools::tags$section(
          class = "scans-app-performance-section",
          htmltools::tags$h2("Patterns to investigate"),
          scans_app_patterns_ui(data$patterns, n, scans),
          htmltools::tags$p(
            class = "scans-app-pattern-overlap",
            if (nrow(data$patterns) > 1L && data$n_findings == 1L) {
              "These patterns all affect the same trajectory."
            } else {
              "A trajectory can appear in more than one pattern."
            }
          )
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

scans_app_patterns_ui <- function(patterns, n, scans) {
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
  htmltools::div(
    class = "scans-app-patterns",
    htmltools::div(
      class = "scans-app-pattern-head",
      htmltools::tags$span("Pattern"),
      htmltools::tags$span("Affected trajectories"),
      htmltools::tags$span("Recorded evidence")
    ),
    lapply(seq_len(nrow(patterns)), function(i) {
      label <- switch(
        patterns$scan[[i]],
        event_error = "Event errors",
        repeated_tool_call = "Repeated tool calls",
        suspicious_tool_loop = "Suspicious tool loop",
        scans_app_title_case(gsub("_", " ", patterns$scan[[i]]))
      )
      htmltools::tags$button(
        type = "button",
        class = "scans-app-pattern",
        `data-scans-pattern` = patterns$scan[[i]],
        htmltools::tags$strong(label),
        htmltools::tags$span(sprintf(
          "%d of %d",
          patterns$trajectories[[i]],
          n
        )),
        htmltools::tags$span(patterns$evidence[[i]]),
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
