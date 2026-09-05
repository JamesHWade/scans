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
    )
  )
  patterns <- patterns[order(-patterns$trajectories, patterns$scan), ]
  summaries$index <- indices
  summaries$title <- data$records$title[indices]
  summaries$n_findings <- data$records$n_findings[indices]
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
    patterns = patterns,
    trajectories = summaries
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

scans_app_performance_metric <- function(label, value, detail) {
  htmltools::div(
    class = "scans-app-performance-metric",
    htmltools::tags$span(class = "scans-app-stat-label", label),
    htmltools::tags$strong(value),
    htmltools::tags$small(detail)
  )
}

scans_app_performance_ui <- function(data, application, priority, scans) {
  n <- data$n
  htmltools::div(
    class = "scans-app-performance",
    htmltools::div(
      class = "scans-app-performance-intro",
      htmltools::tags$h2(application),
      htmltools::tags$p(
        sprintf(
          "%d of %d captured trajectories match the browser filters.",
          n,
          data$total
        ),
        " Select a pattern or trajectory to inspect its evidence."
      )
    ),
    if (n == 0L) {
      scans_app_empty_ui(
        "No trajectories match these filters. Broaden the browser filters to review this application."
      )
    } else {
      htmltools::tagList(
        htmltools::div(
          class = "scans-app-performance-metrics",
          scans_app_performance_metric(
            "Trajectories with findings",
            if (length(scans) == 0L) {
              "Scans off"
            } else {
              sprintf("%d / %d", data$n_findings, n)
            },
            "Flags from selected scans; not an outcome score"
          ),
          scans_app_performance_metric(
            "Median elapsed time",
            scans_app_seconds(data$median_elapsed),
            sprintf("Start to completion recorded for %d / %d", data$n_timed, n)
          ),
          scans_app_performance_metric(
            "95th percentile elapsed",
            scans_app_seconds(data$p95_elapsed),
            sprintf("Across %d timed trajectories", data$n_timed)
          ),
          scans_app_performance_metric(
            "Median recorded tokens",
            if (is.na(data$median_tokens)) {
              "Not recorded"
            } else {
              scans_app_count(data$median_tokens)
            },
            sprintf("Input or output recorded for %d / %d", data$n_tokens, n)
          )
        ),
        htmltools::div(
          class = "scans-app-performance-note",
          "Elapsed time may include pauses between user messages. Token counts use recorded conversation totals when available, otherwise known turn values, and may be partial. Each trajectory is counted separately, including delegated trajectories."
        ),
        htmltools::div(
          class = "scans-app-performance-section",
          htmltools::tags$h3("Where the recorded time went"),
          htmltools::div(
            class = "scans-app-performance-metrics",
            scans_app_performance_metric(
              "Median recorded work",
              scans_app_seconds(data$median_work),
              sprintf(
                "All captured call intervals timed for %d / %d",
                data$n_work,
                n
              )
            ),
            scans_app_performance_metric(
              "Median time outside recorded work",
              scans_app_seconds(data$median_gap),
              "May include pauses or work that was not captured"
            )
          ),
          htmltools::tags$p(sprintf(
            "Recorded work counts overlapping model and tool spans once. Partial timing excluded from these medians: %d / %d trajectories. Complete timing of captured calls does not establish complete capture. Open a trajectory's Resource measurements for values and denominators.",
            data$n_work_partial,
            n
          ))
        ),
        htmltools::div(
          class = "scans-app-performance-section",
          htmltools::tags$h3("Patterns to investigate"),
          htmltools::tags$p(
            "Affected trajectories can appear in more than one pattern."
          ),
          scans_app_patterns_ui(data$patterns, n, scans)
        ),
        htmltools::div(
          class = "scans-app-performance-section",
          htmltools::tags$h3("Trajectories to inspect"),
          scans_app_performance_table(data$trajectories, priority)
        ),
        htmltools::div(
          class = "scans-app-performance-coverage",
          htmltools::tags$h3("Evidence available"),
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
              "%d additional losses belong to the application snapshot and cannot be assigned to a trajectory.",
              data$unassigned_losses
            ))
          },
          htmltools::tags$p(
            "No findings does not establish success. Counts describe the loaded snapshot; source limits and missing captures can affect coverage."
          )
        )
      )
    }
  )
}

scans_app_patterns_ui <- function(patterns, n, scans) {
  if (length(scans) == 0L) {
    return(scans_app_empty_ui(
      "No scans selected. Enable scans in the browser to see diagnostic patterns.",
      compact = TRUE
    ))
  }
  if (nrow(patterns) == 0L) {
    return(scans_app_empty_ui(
      "The selected scans found no patterns in these captured trajectories.",
      compact = TRUE
    ))
  }
  htmltools::div(
    class = "scans-app-patterns",
    htmltools::tagList(lapply(seq_len(nrow(patterns)), function(i) {
      htmltools::tags$button(
        type = "button",
        class = "scans-app-pattern",
        `data-scans-pattern` = patterns$scan[[i]],
        htmltools::tags$span(gsub("_", " ", patterns$scan[[i]])),
        htmltools::tags$strong(sprintf(
          "%d / %d",
          patterns$trajectories[[i]],
          n
        )),
        htmltools::tags$span(
          class = "scans-app-pattern-track",
          `aria-hidden` = "true",
          htmltools::tags$span(
            style = sprintf(
              "width: %.2f%%",
              100 * patterns$trajectories[[i]] / n
            )
          )
        ),
        htmltools::tags$small(sprintf("%d findings", patterns$findings[[i]]))
      )
    }))
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
