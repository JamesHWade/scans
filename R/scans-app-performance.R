# Application measures use the same visible trajectory identities as the
# browser. Findings count affected trajectories, not repeated detector hits.
# `scopes` holds, per facet, the same cohort without that facet. The chart and
# each breakdown use theirs, so a selected bar, tool, user, or model stays
# next to the rows it can be compared with.
# `queue` is the review queue for the whole snapshot and `progress` the review
# progress of the cohort before review-state filters; both are optional.
# `candidates` are promotion candidates for the same ids, computed from the
# bundle by the caller because route analysis reads it.
scans_app_performance_data <- function(
  data,
  indices,
  scopes = list(),
  queue = NULL,
  progress = NULL,
  candidates = NULL
) {
  summaries <- data$summaries[indices, , drop = FALSE]
  ids <- summaries$trajectory_id
  trajectories <- cohort_trajectories(data, ids)
  calls <- cohort_tool_calls(data, ids)
  scope_ids <- function(facet) {
    scope <- scopes[[facet]]
    if (is.null(scope)) ids else data$info$trajectory_id[scope]
  }
  breakdown <- function(facet) {
    if (is.null(scopes[[facet]])) {
      return(cohort_breakdown(data, ids, facet, trajectories, calls))
    }
    cohort_breakdown(data, scope_ids(facet), facet)
  }
  activity <- cohort_activity(data, scope_ids("day"))
  patterns <- cohort_patterns(data, ids, trajectories, activity)
  headline <- cohort_headline(data, ids, trajectories, calls)
  queue <- if (is.null(queue)) {
    cohort_review_queue(data, ids, trajectories = trajectories)
  } else {
    queue[queue$trajectory_id %in% ids, , drop = FALSE]
  }
  queue$title <- data$records$title[queue$index]
  queue$started_at <- data$records$started_at[queue$index]
  queue$user <- data$records$user[queue$index]
  reviewed <- queue$trajectory_id[queue$reviewed %in% TRUE]
  patterns$reviewed <- if (anyNA(queue$reviewed)) {
    rep(NA_integer_, nrow(patterns))
  } else {
    vapply(
      patterns$scan,
      function(scan) {
        affected <- data$findings$trajectory_id[data$findings$scan == scan]
        length(intersect(unique(affected), reviewed))
      },
      integer(1),
      USE.NAMES = FALSE
    )
  }
  summaries$index <- indices
  summaries$title <- data$records$title[indices]
  summaries$n_findings <- data$records$n_findings[indices]
  summaries$assessed <- trajectories$assessed
  summaries$n_tool_errors <- data$records$n_tool_errors[indices]
  summaries$elapsed <- trajectories$elapsed
  summaries$tokens <- trajectories$tokens
  work <- scans_app_measure_rows(data, ids, "recorded_work")
  gap <- scans_app_measure_rows(data, ids, "unattributed_elapsed")

  list(
    total = headline$total,
    n = headline$n,
    n_findings = headline$n_findings,
    n_assessed = headline$n_assessed,
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
      tools = if (is.null(scopes[["tool"]])) {
        cohort_tools(data, ids, calls)
      } else {
        cohort_tools(data, scope_ids("tool"))
      },
      models = breakdown("model"),
      users = breakdown("user"),
      interventions = if (any(data$deputy[indices])) {
        cohort_interventions(data, ids, trajectories)
      },
      provenance = if (any(ids %in% data$exchanges$trajectory_id)) {
        provenance <- cohort_provenance(data, ids, trajectories, activity)
        if (attr(provenance, "total_exchanges") > 0L) provenance
      },
      candidates = candidates,
      review_next = cohort_review_next(queue, data$findings, patterns),
      queue = queue,
      progress = progress %||% cohort_review_progress(queue),
      noun = data$noun %||% scans_app_noun(data$info$source_type)
    ),
    trajectories = summaries
  )
}

# One wording per scan: `label` names the pattern in the overview, `reason`
# is the short fact shown on a queue entry. `run_intervention` findings carry
# a kind, and each kind has its own row. A scan not listed here falls back to
# its own name.
cohort_scan_wording <- tibble::tribble(
  ~scan                        , ~kind                         , ~label                         , ~reason                        ,
  "trajectory_error"           , NA                            , "Trajectory error"             , "Run failed"                   ,
  "cut_off_answer"             , NA                            , "Cut-off answers"              , "Answer cut off"               ,
  "turn_error"                 , NA                            , "Turn error"                   , "Turn failed"                  ,
  "error_chain"                , NA                            , "Error chain"                  , "Error chain"                  ,
  "event_error"                , NA                            , "Event errors"                 , "Event errors"                 ,
  "run_intervention"           , NA                            , "Run interventions"            , "Run intervention"             ,
  "run_intervention"           , "limit_stop"                  , "Stopped at limit"             , "Stopped at limit"             ,
  "run_intervention"           , "limit_wrap_up"               , "Wrapped up at limit"          , "Wrapped up at limit"          ,
  "run_intervention"           , "permission_denied"           , "Permission denied"            , "Permission denied"            ,
  "run_intervention"           , "approval_pending"            , "Approval pending"             , "Approval pending"             ,
  "run_intervention"           , "compaction_failure"          , "Compaction failed"            , "Compaction failed"            ,
  "run_intervention"           , "repeated_compaction"         , "Repeated compaction"          , "Repeated compaction"          ,
  "run_intervention"           , "provider_fallback"           , "Provider fallback"            , "Provider fallback"            ,
  "run_intervention"           , "delegation_missing_evidence" , "Delegation: missing evidence" , "Delegation: missing evidence" ,
  "run_intervention"           , "delegation_unresolved_work"  , "Delegation: unresolved work"  , "Delegation: unresolved work"  ,
  "suspicious_tool_loop"       , NA                            , "Suspicious tool loop"         , "Tool loop"                    ,
  "repeated_tool_call"         , NA                            , "Repeated tool calls"          , "Repeated tool call"           ,
  "unresolved_tool_call"       , NA                            , "Unresolved tool call"         , "Call without result"          ,
  "unmatched_tool_result"      , NA                            , "Unmatched tool result"        , "Result without call"          ,
  "ambiguous_tool_correlation" , NA                            , "Ambiguous tool correlation"   , "Ambiguous call ids"
)

cohort_scan_word <- function(scan, column, kind = NA_character_) {
  if (!length(scan)) {
    return(character())
  }
  row <- match(
    paste(scan, kind),
    paste(cohort_scan_wording$scan, cohort_scan_wording$kind)
  )
  word <- cohort_scan_wording[[column]][row]
  ifelse(is.na(word), scans_app_title_case(gsub("_", " ", scan)), word)
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
  noun <- cohort$noun
  facets <- scope$facets %||% list()
  htmltools::div(
    class = "scans-app-performance",
    `aria-label` = paste("Overview for", application),
    htmltools::div(
      class = "scans-app-overview-title",
      htmltools::tags$h1("Overview"),
      if (chat) {
        shiny::actionButton(
          "scans_app_ask_overview",
          paste("Ask about these", noun$many),
          icon = shiny::icon("comment"),
          class = "btn-outline-secondary"
        )
      },
      if (n > 0L) scans_app_export_menu_ui(noun, isTRUE(scope$export_users))
    ),
    scans_app_scope_sentence_ui(
      cohort$headline,
      scans_app_scope_labels(facets, scope$pattern, scope$findings_only),
      noun
    ),
    if (n == 0L) {
      scans_app_empty_ui(
        if (data$total == 0L) {
          sprintf("This source returned no %s for the window read.", noun$many)
        } else {
          sprintf(
            "No %s match these filters. Remove a filter to widen the cohort.",
            noun$many
          )
        }
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
          findings_only = isTRUE(scope$findings_only),
          noun = noun,
          progress = cohort$progress
        ),
        htmltools::tags$section(
          class = "scans-app-performance-section",
          htmltools::tags$h2("Activity"),
          scans_app_activity_ui(cohort$activity, facets$day, noun)
        ),
        htmltools::tags$section(
          class = "scans-app-performance-section",
          htmltools::tags$h2("Patterns to investigate"),
          scans_app_patterns_ui(
            data$patterns,
            n,
            scans,
            scope$pattern,
            noun,
            cohort$headline$n_assessed
          ),
          if (nrow(data$patterns) > 0L) {
            htmltools::tags$p(
              class = "scans-app-pattern-overlap",
              if (nrow(data$patterns) > 1L && data$n_findings == 1L) {
                paste0("These patterns all affect the same ", noun$one, ".")
              } else {
                paste("A", noun$one, "can appear in more than one pattern.")
              }
            )
          }
        ),
        if (!is.null(cohort$interventions)) {
          htmltools::tags$section(
            class = "scans-app-performance-section",
            htmltools::tags$h2("Run interventions"),
            scans_app_interventions_ui(cohort$interventions, facets, noun)
          )
        },
        if (!is.null(cohort$provenance)) {
          htmltools::tags$section(
            class = "scans-app-performance-section",
            htmltools::tags$h2("Answer provenance"),
            scans_app_provenance_ui(
              cohort$provenance,
              cohort$activity,
              facets,
              noun
            )
          )
        },
        if (!is.null(cohort$candidates) && nrow(cohort$candidates) > 0L) {
          htmltools::tags$section(
            class = "scans-app-performance-section",
            htmltools::tags$h2("Promotion candidates"),
            scans_app_candidates_ui(cohort$candidates, facets, noun)
          )
        },
        if (nrow(cohort$tools) > 0L) {
          htmltools::tags$section(
            class = "scans-app-performance-section",
            htmltools::tags$h2("Tools"),
            scans_app_tools_ui(cohort$tools, facets, noun)
          )
        },
        htmltools::div(
          class = "scans-app-breakdowns",
          scans_app_models_ui(cohort$models, n, facets, noun),
          scans_app_users_ui(cohort$users, n, facets, noun)
        ),
        htmltools::tags$section(
          class = "scans-app-performance-section",
          htmltools::tags$h2("Review next"),
          scans_app_review_next_ui(cohort$queue, cohort$review_next, noun)
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
  cohort_scan_word(scan, "label")
}

scans_app_patterns_ui <- function(
  patterns,
  n,
  scans,
  selected = NULL,
  noun = scans_app_noun(),
  assessed = n
) {
  if (length(scans) == 0L) {
    return(scans_app_empty_ui(
      "No scans selected. Enable scans in Options to see diagnostic patterns.",
      compact = TRUE
    ))
  }
  if (nrow(patterns) == 0L) {
    # No findings is good news about what was recorded; say so plainly and
    # name what the scans could not look at.
    return(htmltools::tags$p(
      class = "scans-app-no-findings",
      sprintf(
        "No findings in %s assessed %s.",
        scans_app_count(assessed),
        if (assessed == 1L) noun$one else noun$many
      ),
      if (n > assessed) {
        sprintf(
          " %s could not be assessed; see Evidence coverage.",
          scans_app_n(n - assessed, noun)
        )
      }
    ))
  }
  users_known <- !all(is.na(patterns$users))
  reviews_known <- !is.null(patterns$reviewed) && !anyNA(patterns$reviewed)
  htmltools::div(
    class = paste0(
      "scans-app-patterns scans-app-patterns-extra-",
      users_known + reviews_known
    ),
    htmltools::div(
      class = "scans-app-pattern-head",
      `aria-hidden` = "true",
      htmltools::tags$span("Severity"),
      htmltools::tags$span("Pattern"),
      htmltools::tags$span("Affected"),
      if (users_known) {
        htmltools::tags$span(class = "scans-app-pattern-users", "Users")
      },
      if (reviews_known) {
        htmltools::tags$span(class = "scans-app-pattern-reviewed", "Reviewed")
      },
      htmltools::tags$span(class = "scans-app-pattern-seen", "Last seen"),
      htmltools::tags$span(class = "scans-app-pattern-trend", "Over time"),
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
          "%s, %s: %d of %d assessed %s. %s. Show these %s.",
          label,
          row$severity,
          row$trajectories,
          row$assessed,
          noun$many,
          row$evidence,
          noun$many
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
          sprintf("%d of %d assessed", row$trajectories, row$assessed),
          scans_app_meter(row$share, row$severity)
        ),
        if (users_known) {
          htmltools::tags$span(
            class = "scans-app-pattern-users",
            scans_app_count(row$users)
          )
        },
        if (reviews_known) {
          htmltools::tags$span(
            class = "scans-app-pattern-reviewed",
            sprintf("%d of %d", row$reviewed, row$trajectories)
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
            htmltools::tags$td(
              if (rows$assessed[[i]] || rows$n_findings[[i]] > 0L) {
                rows$n_findings[[i]]
              } else {
                "Not assessed"
              }
            )
          )
        }
      )))
    )
  )
}
