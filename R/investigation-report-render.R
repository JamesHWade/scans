# A report is laid out once as blocks (headings, paragraphs, lists, tables)
# of inline spans, then written as Markdown or as HTML. Recorded names and
# explanations are plain text or code spans in that layout, and each writer
# escapes them for its own format, so a hostile tool name cannot become
# markup in either.

report_code <- function(text) {
  list(span = "code", text = as.character(text))
}

report_link <- function(text, href, code = FALSE) {
  list(span = "link", text = as.character(text), href = href, code = code)
}

report_anchor <- function(id) {
  list(span = "anchor", id = id)
}

# A code span that links when there is somewhere to link to.
report_identity <- function(text, href = NA_character_) {
  if (is.na(href)) report_code(text) else report_link(text, href, code = TRUE)
}

report_heading <- function(text, level = 2L, id = NULL) {
  list(block = "heading", text = text, level = level, id = id)
}

report_para <- function(...) {
  list(block = "para", inline = list(...))
}

# Each item is a string or a list of spans; `NULL` items are dropped.
report_items <- function(...) {
  items <- Filter(Negate(is.null), list(...))
  if (length(items)) list(block = "items", items = items)
}

report_table <- function(header, rows) {
  if (length(rows)) list(block = "table", header = header, rows = rows)
}

# An inline is a string, a span, or a list of either, nested freely.
report_spans <- function(inline) {
  if (is.null(inline)) {
    return(list())
  }
  if (!is.list(inline) || !is.null(inline$span)) {
    return(list(inline))
  }
  unlist(lapply(inline, report_spans), recursive = FALSE)
}

report_unknown <- "unknown"

report_count <- function(x) {
  if (is.null(x) || is.na(x)) report_unknown else scans_app_count(round(x))
}

# A count over the trajectories a scan could assess. With none assessed the
# count is unknown, so it is not printed as a share of anything.
report_assessed <- function(x, assessed) {
  if (assessed == 0L) {
    return("not assessed")
  }
  sprintf("%s of %s assessed", report_count(x), report_count(assessed))
}

report_seconds <- function(x) {
  if (is.na(x)) report_unknown else scans_app_seconds(x)
}

report_day <- function(x) {
  if (is.na(x)) report_unknown else format(x, "%Y-%m-%d", tz = "UTC")
}

report_minute <- function(x) {
  format(x, "%Y-%m-%d %H:%M UTC", tz = "UTC")
}

report_measure_value <- function(value, unit) {
  if (is.na(value)) {
    return(report_unknown)
  }
  if (unit %in% "s") scans_app_seconds(value) else report_count(value)
}

# What the source read covered. A cap is stated only when the read recorded
# one; a read that recorded nothing leaves it unknown. `capped` carries that
# state so the capture section can repeat a cap without repeating its absence.
report_source_items <- function(source, noun) {
  if (!source$recorded) {
    return(structure(
      list("Source read: not recorded, so whether it was capped is unknown."),
      capped = NA
    ))
  }
  window <- if (!is.null(source$from) || !is.null(source$to)) {
    paste(
      c(
        if (!is.null(source$from)) paste("from", report_minute(source$from)),
        if (!is.null(source$to)) paste("to", report_minute(source$to))
      ),
      collapse = " "
    )
  } else {
    "no time window recorded"
  }
  capped <- c(
    if (isTRUE(source$truncated)) {
      sprintf(
        "the span ceiling%s was reached, so older traces were not read",
        if (is.null(source$max_spans)) {
          ""
        } else {
          paste0(" of ", scans_app_count(source$max_spans))
        }
      )
    },
    if (isTRUE(source$incomplete)) {
      "a page of traces could not be read"
    },
    if (
      !is.null(source$limit) &&
        !is.null(source$found) &&
        source$found > source$limit
    ) {
      sprintf(
        "only the %s most recent of %s %s in the window were kept",
        scans_app_count(source$limit),
        scans_app_count(source$found),
        noun$many
      )
    }
  )
  known <- !is.null(source$truncated) || !is.null(source$incomplete)
  structure(
    list(paste0(
      "Source read: ",
      window,
      if (!is.null(source$read_at)) {
        paste0(", read ", report_minute(source$read_at))
      },
      ". ",
      if (length(capped)) {
        paste0("Capped: ", paste(capped, collapse = "; "), ".")
      } else if (known) {
        "The read recorded no cap."
      } else {
        "Whether the read was capped is unknown."
      }
    )),
    capped = if (length(capped)) {
      TRUE
    } else if (known) {
      FALSE
    } else {
      NA
    }
  )
}

report_blocks <- function(x, max_evidence = 100L) {
  scope <- x$scope
  noun <- scope$noun
  headline <- x$headline
  n <- headline$n
  assessed_noun <- list(
    one = paste("assessed", noun$one),
    many = paste("assessed", noun$many)
  )
  window <- scans_app_day_range(scope$window$first, scope$window$last)
  settings <- scope$settings

  scope_items <- c(
    list(
      sprintf(
        "Scope: %s of %s %s%s.",
        scans_app_count(n),
        scans_app_count(headline$total),
        noun$many,
        if (is.null(scope$snapshot)) "" else " in this saved investigation"
      ),
      if (length(scope$filters)) {
        paste0(
          "Filters: ",
          paste(unname(scope$filters), collapse = "; "),
          "."
        )
      } else {
        "Filters: none."
      },
      paste0(
        if (identical(scope$window$basis, "start")) {
          "Start times in scope: "
        } else {
          "Activity times in scope: "
        },
        if (is.null(window)) "none recorded" else paste(window, "UTC"),
        if (scope$window$n_undated > 0L) {
          sprintf(
            "; %s with no recorded time",
            scans_app_n(scope$window$n_undated, noun)
          )
        },
        "."
      )
    ),
    report_source_items(scope$source, noun),
    list(
      if (length(scope$source$details)) {
        c(
          list("Source details as recorded: "),
          lapply(seq_along(scope$source$details), function(i) {
            list(
              if (i > 1L) "; ",
              report_code(names(scope$source$details)[[i]]),
              paste0(" ", scope$source$details[[i]])
            )
          }),
          list(".")
        )
      },
      if (!is.null(scope$snapshot)) {
        list(
          "Snapshot ",
          report_code(scope$snapshot$snapshot_id),
          ", revision ",
          report_code(scope$snapshot$revision_id),
          "."
        )
      },
      if (!is.null(settings)) {
        list(
          sprintf(
            "Scans: %s selected, repeat threshold %s, loop threshold %s%s.",
            scans_app_count(length(settings$scans)),
            settings$repeat_threshold,
            settings$loop_threshold,
            if (is.null(scope$analysis_version)) {
              ""
            } else {
              paste0("; analysis by scans ", scope$analysis_version)
            }
          )
        )
      },
      if (!is.null(scope$link)) {
        list(report_link("Open this scope in scans", scope$link), ".")
      }
    ),
    as.list(scope$link_limits),
    list(sprintf(
      "Generated %s by scans %s.",
      report_minute(x$generated),
      x$package_version
    ))
  )

  headline_items <- list(
    sprintf(
      "%s of %s %s are in scope.",
      scans_app_count(n),
      scans_app_count(headline$total),
      noun$many
    ),
    if (headline$n_assessed == 0L && n > 0L) {
      sprintf(
        "None of the %s could be assessed by a selected scan, so findings are unknown, not absent.",
        scans_app_n(n, noun)
      )
    } else {
      sprintf(
        "Findings in %s of %s (%s with error-severity findings).",
        scans_app_count(headline$n_findings),
        scans_app_n(headline$n_assessed, assessed_noun),
        scans_app_count(headline$n_error)
      )
    },
    if (headline$n_not_assessed > 0L && headline$n_assessed > 0L) {
      sprintf(
        "%s of %s could not be assessed by any selected scan. Not assessed is not a clean result.",
        scans_app_count(headline$n_not_assessed),
        scans_app_count(n)
      )
    },
    if (headline$tool_calls > 0L) {
      sprintf(
        "%s of %s failed, in %s.",
        scans_app_count(headline$tool_failed),
        scans_app_plural(headline$tool_calls, "tool call"),
        scans_app_n(headline$tool_trajectories_failed, noun)
      )
    } else {
      "No tool calls were recorded."
    },
    if (!is.na(x$review_progress$flagged_reviewed)) {
      sprintf(
        "Review: %s of %s flagged %s reviewed.",
        scans_app_count(x$review_progress$flagged_reviewed),
        scans_app_count(x$review_progress$flagged),
        noun$many
      )
    }
  )

  c(
    list(
      report_heading(
        paste0(x$application, ": review of recorded ", noun$many),
        level = 1L
      ),
      do.call(report_items, scope_items),
      report_heading("Headline"),
      do.call(report_items, headline_items)
    ),
    report_pattern_blocks(x),
    report_cohort_blocks(x),
    report_resource_blocks(x),
    report_capture_blocks(x),
    report_evidence_blocks(x, max_evidence),
    report_review_blocks(x, report_listed_anchors(x, max_evidence)),
    list(
      report_heading("What this report can and cannot establish"),
      report_items(
        "Findings describe recorded behavior, not an outcome score.",
        "No findings does not establish success. Source limits and missing captures can affect coverage.",
        "Not assessed and unknown are not zero: a scan that could not read a record says nothing about it.",
        "Elapsed time may include user pauses; token totals may be partial.",
        if (x$include_users) {
          "Transcript text, tool arguments, and tool results are omitted from this report."
        } else {
          "User identifiers, transcript text, tool arguments, and tool results are omitted from this report."
        }
      )
    )
  )
}

report_pattern_blocks <- function(x) {
  patterns <- x$patterns
  headline <- x$headline
  noun <- x$scope$noun
  if (length(x$scope$settings$scans) == 0L && !is.null(x$scope$settings)) {
    return(list(
      report_heading("Patterns"),
      report_para("No scans were selected, so no patterns were looked for.")
    ))
  }
  if (nrow(patterns) == 0L) {
    return(list(
      report_heading("Patterns"),
      report_para(sprintf(
        "No findings in %s.%s",
        scans_app_n(
          headline$n_assessed,
          list(
            one = paste("assessed", noun$one),
            many = paste("assessed", noun$many)
          )
        ),
        if (headline$n_not_assessed > 0L) {
          sprintf(
            " %s could not be assessed.",
            scans_app_n(headline$n_not_assessed, noun)
          )
        } else {
          ""
        }
      ))
    ))
  }
  list(
    report_heading("Patterns"),
    report_para(
      "Affected counts ",
      noun$many,
      ", not repeated findings. A ",
      noun$one,
      " can appear in more than one pattern."
    ),
    report_table(
      c(
        "Pattern",
        "Severity",
        "Affected",
        "Assessed by this scan",
        "Findings",
        "Users affected",
        "First seen",
        "Last seen",
        "Summary"
      ),
      lapply(seq_len(nrow(patterns)), function(i) {
        row <- patterns[i, ]
        list(
          list(report_link(row$label, paste0("#", row$anchor))),
          row$severity,
          report_assessed(row$trajectories, row$assessed),
          sprintf("%s of %s", report_count(row$assessed), report_count(row$n)),
          report_count(row$findings),
          report_count(row$users),
          report_day(row$first_seen),
          report_day(row$last_seen),
          row$summary
        )
      })
    )
  )
}

report_cohort_blocks <- function(x) {
  noun <- x$scope$noun
  tools <- x$tools
  models <- x$models
  breakdown <- function(table, name) {
    report_table(
      c(name, "Recorded", "With findings", "Median tokens", "Median elapsed"),
      lapply(seq_len(nrow(table)), function(i) {
        row <- table[i, ]
        list(
          row$value,
          report_count(row$n),
          report_assessed(row$n_findings, row$n_assessed),
          report_count(row$median_tokens),
          report_seconds(row$median_elapsed)
        )
      })
    )
  }
  c(
    list(
      report_heading("Tool failures"),
      if (nrow(tools) == 0L) {
        report_para("No tool calls were recorded.")
      } else {
        report_table(
          c("Tool", "Calls", "Failed", "Top error", "Median", "p95", "Timed"),
          lapply(seq_len(nrow(tools)), function(i) {
            row <- tools[i, ]
            list(
              list(report_code(row$tool)),
              report_count(row$calls),
              sprintf(
                "%s (%s)",
                report_count(row$failed),
                scans_app_percent(row$failure_rate)
              ),
              if (is.na(row$top_error)) {
                ""
              } else {
                list(
                  report_code(row$top_error),
                  sprintf(" (%s)", report_count(row$top_error_n))
                )
              },
              report_seconds(row$median_duration),
              report_seconds(row$p95_duration),
              sprintf(
                "%s of %s",
                report_count(row$n_timed),
                report_count(row$calls)
              )
            )
          })
        )
      },
      report_heading("Models"),
      breakdown(models, "Model")
    ),
    if (!is.null(x$interventions) && nrow(x$interventions) > 0L) {
      list(
        report_heading("Run interventions"),
        report_table(
          c("Intervention", "Affected runs", "Recorded"),
          lapply(seq_len(nrow(x$interventions)), function(i) {
            row <- x$interventions[i, ]
            list(
              row$label,
              sprintf(
                "%s of %s",
                report_count(row$trajectories),
                report_count(attr(x$interventions, "n_deputy"))
              ),
              report_count(row$findings)
            )
          })
        ),
        do.call(report_items, as.list(attr(x$interventions, "limitations")))
      )
    },
    if (!is.null(x$provenance)) {
      list(
        report_heading("Answer provenance"),
        report_para(sprintf(
          "%s in %s that record provenance. Unclassified means no usable record, not untrusted.",
          scans_app_plural(attr(x$provenance, "total_exchanges"), "exchange"),
          scans_app_n(attr(x$provenance, "total_trajectories"), noun)
        )),
        report_table(
          c("Class", "Exchanges", scans_app_title_case(noun$many)),
          lapply(seq_len(nrow(x$provenance)), function(i) {
            row <- x$provenance[i, ]
            list(
              row$label,
              sprintf(
                "%s of %s",
                report_count(row$exchanges),
                report_count(attr(x$provenance, "total_exchanges"))
              ),
              report_count(row$trajectories)
            )
          })
        )
      )
    },
    if (!is.null(x$candidates) && nrow(x$candidates) > 0L) {
      list(
        report_heading("Promotion candidates"),
        report_para(
          "Recurring exploratory tool routes that could become governed measures."
        ),
        report_table(
          c(
            "Route",
            scans_app_title_case(noun$many),
            "Exchanges",
            "Users",
            "Basis"
          ),
          lapply(seq_len(nrow(x$candidates)), function(i) {
            row <- x$candidates[i, ]
            list(
              list(report_code(row$route_signature)),
              report_count(row$n_trajectories),
              report_count(row$n_exchanges),
              report_count(row$n_users),
              row$candidate_basis
            )
          })
        )
      )
    },
    if (!is.null(x$users)) {
      list(report_heading("Users"), breakdown(x$users, "User"))
    }
  )
}

report_resource_blocks <- function(x) {
  headline <- x$headline
  measures <- x$measures
  n <- headline$n
  list(
    report_heading("Resource measurements"),
    report_items(
      sprintf(
        "Elapsed: median %s, p95 %s (timed for %s of %s).",
        report_seconds(headline$median_elapsed),
        report_seconds(headline$p95_elapsed),
        scans_app_count(headline$n_timed),
        scans_app_count(n)
      ),
      sprintf(
        "Tokens: median %s (recorded for %s of %s).",
        report_count(headline$median_tokens),
        scans_app_count(headline$n_tokens),
        scans_app_count(n)
      ),
      sprintf(
        "Recorded work: median %s; time outside recorded work: median %s (all captured call intervals timed for %s of %s; partial timing excluded for %s).",
        report_seconds(headline$median_work),
        report_seconds(headline$median_gap),
        scans_app_count(headline$n_work),
        scans_app_count(n),
        scans_app_count(headline$n_work_partial)
      )
    ),
    report_para(sprintf(
      "Coverage counts %s by how completely each measure was recorded. A median covers complete records only. Coverage describes the loaded records and does not establish that the source captured every call.",
      x$scope$noun$many
    )),
    report_table(
      c(
        "Measure",
        "Complete",
        "Partial",
        "Unavailable",
        "Unknown",
        "Median where complete"
      ),
      lapply(seq_len(nrow(measures)), function(i) {
        row <- measures[i, ]
        of_n <- function(count) {
          sprintf("%s of %s", report_count(count), report_count(row$n))
        }
        list(
          gsub("_", " ", row$measure),
          of_n(row$complete),
          of_n(row$partial),
          of_n(row$unavailable),
          of_n(row$unknown),
          report_measure_value(row$median_complete, row$unit)
        )
      })
    )
  )
}

report_capture_blocks <- function(x) {
  headline <- x$headline
  noun <- x$scope$noun
  snapshot <- x$scope$snapshot
  losses <- x$losses
  coverage <- x$coverage
  scans <- unique(coverage$scan)
  source <- report_source_items(x$scope$source, noun)
  list(
    report_heading("Capture limits and losses"),
    do.call(
      report_items,
      c(
        list(
          if (!isFALSE(attr(source, "capped"))) source[[1L]],
          if (!is.null(snapshot)) {
            if (snapshot$omitted_trajectories > 0) {
              sprintf(
                "This saved selection left out %s of its source.",
                scans_app_plural(
                  snapshot$omitted_trajectories,
                  "trajectory",
                  "trajectories"
                )
              )
            } else {
              "This saved selection left out no trajectories of the bundle it was made from."
            }
          },
          sprintf(
            "%s of %s have recorded capture losses.%s",
            scans_app_count(headline$n_losses),
            scans_app_n(headline$n, noun),
            if (headline$unassigned_losses > 0L) {
              sprintf(
                " %s cannot be assigned to one %s and may affect any of them.",
                scans_app_plural(
                  headline$unassigned_losses,
                  "further loss",
                  "further losses"
                ),
                noun$one
              )
            } else {
              ""
            }
          ),
          sprintf(
            "%s of %s have linked evaluations.",
            scans_app_count(headline$n_evaluated),
            scans_app_count(headline$n)
          ),
          if (nrow(losses) == 0L) {
            "No capture losses are recorded. That does not establish complete capture."
          }
        )
      )
    ),
    report_table(
      c("Field", "Reason", "Losses", "Trajectories", "Not assigned"),
      lapply(seq_len(nrow(losses)), function(i) {
        row <- losses[i, ]
        list(
          list(report_code(row$field)),
          list(report_code(row$reason)),
          report_count(row$losses),
          report_count(row$trajectories),
          report_count(row$unassigned)
        )
      })
    ),
    if (length(scans)) {
      report_para(sprintf(
        "Each cell counts %s for one selected scanner. Only the two assessed columns are results; the others are not negative results.",
        noun$many
      ))
    },
    report_table(
      c("Scanner", unname(scans_app_assessment_labels)),
      lapply(scans, function(scan) {
        rows <- coverage[coverage$scan == scan, , drop = FALSE]
        c(
          list(gsub("_", " ", scan)),
          lapply(
            rows$trajectories[match(
              names(scans_app_assessment_labels),
              rows$status
            )],
            report_count
          )
        )
      })
    )
  )
}

# The anchors of the findings the rendered evidence lists: those of the
# first `max_evidence` trajectories of each pattern.
report_listed_anchors <- function(x, max_evidence) {
  evidence <- x$evidence
  unlist(lapply(unique(evidence$scan), function(scan) {
    rows <- evidence[evidence$scan == scan, , drop = FALSE]
    shown <- utils::head(unique(rows$trajectory_id), max_evidence)
    rows$anchor[rows$trajectory_id %in% shown]
  }))
}

# For each pattern, the trajectories behind its count and the exact turn and
# event identities of each finding. At most `max_evidence` trajectories are
# listed per pattern, and the heading line says when that is fewer than the
# pattern counted.
report_evidence_blocks <- function(x, max_evidence) {
  patterns <- x$patterns
  evidence <- x$evidence
  noun <- x$scope$noun
  trajectories <- x$trajectories
  listed <- report_listed_anchors(x, max_evidence)
  unassessed <- trajectories[trajectories$class == "unassessed", , drop = FALSE]
  blocks <- list(
    report_heading("Evidence"),
    report_para(
      "Identities are exact: each row names the trajectory, turn, and events a finding rests on.",
      if (is.null(x$scope$app_url)) {
        " No app address was supplied, so identities are not links."
      } else {
        " Identities link to the app."
      }
    )
  )
  for (i in seq_len(nrow(patterns))) {
    row <- patterns[i, ]
    rows <- evidence[evidence$scan == row$scan, , drop = FALSE]
    affected <- unique(rows$trajectory_id)
    rows <- rows[rows$anchor %in% listed, , drop = FALSE]
    shown <- unique(rows$trajectory_id)
    blocks <- c(
      blocks,
      list(
        report_heading(row$label, level = 3L, id = row$anchor),
        report_para(
          list(report_code(row$scan)),
          sprintf(
            ": %s, %s.%s",
            if (row$assessed == 0L) {
              sprintf("%s, not assessed", scans_app_n(row$trajectories, noun))
            } else {
              sprintf(
                "%s of %s",
                report_count(row$trajectories),
                scans_app_n(
                  row$assessed,
                  list(
                    one = paste("assessed", noun$one),
                    many = paste("assessed", noun$many)
                  )
                )
              )
            },
            scans_app_plural(row$findings, "finding"),
            if (length(shown) < length(affected)) {
              sprintf(
                " Listing the first %s of %s; the other %s %s counted above but not listed in this report.",
                report_count(length(shown)),
                scans_app_n(length(affected), noun),
                report_count(length(affected) - length(shown)),
                if (length(affected) - length(shown) == 1L) "is" else "are"
              )
            } else {
              ""
            }
          )
        ),
        report_table(
          c("Trajectory", "Severity", "Turn", "Events", "Explanation"),
          lapply(seq_len(nrow(rows)), function(j) {
            finding <- rows[j, ]
            events <- finding$event_ids[[1L]]
            links <- finding$event_links[[1L]]
            list(
              list(
                report_anchor(finding$anchor),
                report_identity(
                  finding$trajectory_id,
                  trajectories$link[[match(
                    finding$trajectory_id,
                    trajectories$trajectory_id
                  )]]
                )
              ),
              finding$severity,
              if (is.na(finding$turn_id)) {
                ""
              } else {
                list(report_code(finding$turn_id))
              },
              if (!length(events)) {
                "none recorded"
              } else {
                lapply(seq_along(events), function(k) {
                  list(
                    if (k > 1L) ", ",
                    report_identity(events[[k]], links[[k]])
                  )
                })
              },
              finding$explanation
            )
          })
        )
      )
    )
  }
  if (nrow(unassessed) > 0L) {
    shown <- utils::head(unassessed, max_evidence)
    blocks <- c(
      blocks,
      list(
        report_heading("Not assessed", level = 3L, id = "not-assessed"),
        report_para(sprintf(
          "%s no selected scan could assess. They are not counted as clean.%s",
          scans_app_n(nrow(unassessed), noun),
          if (nrow(shown) < nrow(unassessed)) {
            sprintf(
              " Listing the first %s; the rest are not listed in this report.",
              report_count(nrow(shown))
            )
          } else {
            ""
          }
        )),
        do.call(
          report_items,
          lapply(seq_len(nrow(shown)), function(j) {
            list(report_identity(shown$trajectory_id[[j]], shown$link[[j]]))
          })
        )
      )
    )
  }
  blocks
}

# `listed` are the evidence anchors the report renders; a case links only to
# one of those.
report_review_blocks <- function(x, listed) {
  reviews <- x$reviews
  if (is.null(reviews)) {
    return(NULL)
  }
  list(
    report_heading("Review decisions"),
    report_para(
      "Status is the adjudicated state of each supplied review case: one active decision gives its judgment, several active decisions are disputed, and none is unreviewed. Rationales are not printed."
    ),
    report_table(
      c(
        "Case",
        "Trajectory",
        "Scanner",
        "Finding",
        "Status",
        "Decisions",
        "In scope",
        if ("reviewers" %in% names(reviews)) "Reviewers"
      ),
      lapply(seq_len(nrow(reviews)), function(i) {
        row <- reviews[i, ]
        c(
          list(
            list(report_code(substr(row$case_id, 8L, 19L))),
            list(report_code(row$trajectory_id)),
            if (is.na(row$scan)) "" else list(report_code(row$scan)),
            if (is.na(row$finding_id)) {
              "none (unflagged)"
            } else if (row$anchor %in% listed) {
              list(report_link("evidence", paste0("#", row$anchor)))
            } else if (!is.na(row$anchor)) {
              "in scope, beyond the evidence listed"
            } else if (row$in_analysis) {
              "outside this report's filters"
            } else {
              "not in this analysis"
            },
            gsub("_", " ", row$status),
            sprintf(
              "%s (%s active)%s",
              report_count(row$n_decisions),
              report_count(row$n_active),
              if (row$same_revision) "" else "; made on another revision"
            ),
            if (row$in_scope) "yes" else "no"
          ),
          if ("reviewers" %in% names(reviews)) {
            list(paste(row$reviewers[[1L]], collapse = ", "))
          }
        )
      })
    )
  )
}

# Markdown ---------------------------------------------------------------

# Backslash-escape what Markdown or an embedded HTML parser would read as
# markup, and break the bare addresses GitHub would turn into links.
report_md_text <- function(text) {
  text <- gsub("[[:space:]]+", " ", as.character(text))
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("([\\\\`*_\\[\\]#~|@])", "\\\\\\1", text, perl = TRUE)
  text <- gsub("://", ":\\//", text, fixed = TRUE)
  gsub("www.", "www\\.", text, fixed = TRUE)
}

# A code span fenced by more backticks than its text contains. A table cell
# also needs its pipes escaped, which a table strips again inside code.
report_md_code <- function(text, table = FALSE) {
  text <- gsub("[[:space:]]+", " ", as.character(text))
  runs <- regmatches(text, gregexpr("`+", text))[[1L]]
  fence <- strrep("`", max(c(0L, nchar(runs))) + 1L)
  if (table) {
    text <- gsub("|", "\\|", text, fixed = TRUE)
  }
  pad <- if (grepl("^`|`$|^ .* $", text)) " " else ""
  paste0(fence, pad, text, pad, fence)
}

report_md_href <- function(href) {
  paste0("<", gsub("([<> ])", "", href), ">")
}

report_md_inline <- function(inline, table = FALSE) {
  paste(
    vapply(
      report_spans(inline),
      function(span) {
        if (!is.list(span)) {
          return(report_md_text(span))
        }
        switch(
          span$span,
          code = report_md_code(span$text, table),
          anchor = sprintf("<a id=\"%s\"></a>", span$id),
          link = sprintf(
            "[%s](%s)",
            if (span$code) {
              report_md_code(span$text, table)
            } else {
              report_md_text(span$text)
            },
            report_md_href(span$href)
          )
        )
      },
      ""
    ),
    collapse = ""
  )
}

report_markdown <- function(blocks) {
  lines <- lapply(Filter(Negate(is.null), blocks), function(block) {
    switch(
      block$block,
      heading = c(
        if (!is.null(block$id)) sprintf("<a id=\"%s\"></a>", block$id),
        if (!is.null(block$id)) "",
        paste(strrep("#", block$level), report_md_text(block$text)),
        ""
      ),
      para = c(report_md_inline(block$inline), ""),
      items = c(
        paste("-", vapply(block$items, report_md_inline, "")),
        ""
      ),
      table = c(
        paste0("| ", paste(block$header, collapse = " | "), " |"),
        paste0("|", strrep(" --- |", length(block$header))),
        vapply(
          block$rows,
          function(row) {
            paste0(
              "| ",
              paste(
                vapply(row, report_md_inline, "", table = TRUE),
                collapse = " | "
              ),
              " |"
            )
          },
          ""
        ),
        ""
      )
    )
  })
  unlist(lines, use.names = FALSE)
}

# HTML -------------------------------------------------------------------

report_html_inline <- function(inline) {
  escape <- htmltools::htmlEscape
  paste(
    vapply(
      report_spans(inline),
      function(span) {
        if (!is.list(span)) {
          return(escape(span))
        }
        switch(
          span$span,
          code = paste0("<code>", escape(span$text), "</code>"),
          anchor = sprintf("<a id=\"%s\"></a>", escape(span$id, TRUE)),
          link = sprintf(
            "<a href=\"%s\">%s</a>",
            escape(span$href, attribute = TRUE),
            if (span$code) {
              paste0("<code>", escape(span$text), "</code>")
            } else {
              escape(span$text)
            }
          )
        )
      },
      ""
    ),
    collapse = ""
  )
}

report_html_style <- "
:root { color-scheme: light dark; --fg: #1c2024; --muted: #5b6470; --line: #d5dae0; --bg: #ffffff; --soft: #f4f6f8; --link: #0b5cad; }
@media (prefers-color-scheme: dark) { :root { --fg: #e7eaee; --muted: #a2abb6; --line: #3a424c; --bg: #15181c; --soft: #1f242a; --link: #7db8f5; } }
body { margin: 0; background: var(--bg); color: var(--fg); font: 16px/1.5 system-ui, -apple-system, 'Segoe UI', sans-serif; }
main { max-width: 68rem; margin: 0 auto; padding: 1.5rem 1rem 4rem; }
h1 { font-size: 1.6rem; line-height: 1.25; }
h2 { font-size: 1.2rem; margin-top: 2.2rem; padding-top: 0.8rem; border-top: 1px solid var(--line); }
h3 { font-size: 1rem; margin-top: 1.6rem; }
a { color: var(--link); }
code { font: 0.85em ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; background: var(--soft); padding: 0.05em 0.3em; border-radius: 3px; overflow-wrap: anywhere; }
ul { padding-left: 1.2rem; }
li { margin: 0.25rem 0; }
.table { overflow-x: auto; margin: 0.8rem 0; }
table { border-collapse: collapse; font-size: 0.9rem; min-width: 100%; }
th, td { text-align: left; vertical-align: top; padding: 0.35rem 0.7rem 0.35rem 0; border-bottom: 1px solid var(--line); }
th { color: var(--muted); font-weight: 600; white-space: nowrap; }
"

report_html <- function(blocks, title) {
  escape <- htmltools::htmlEscape
  body <- lapply(Filter(Negate(is.null), blocks), function(block) {
    switch(
      block$block,
      heading = sprintf(
        "<h%d%s>%s</h%d>",
        block$level,
        if (is.null(block$id)) {
          ""
        } else {
          sprintf(" id=\"%s\"", escape(block$id, attribute = TRUE))
        },
        escape(block$text),
        block$level
      ),
      para = paste0("<p>", report_html_inline(block$inline), "</p>"),
      items = c(
        "<ul>",
        paste0("<li>", vapply(block$items, report_html_inline, ""), "</li>"),
        "</ul>"
      ),
      table = c(
        "<div class=\"table\"><table>",
        paste0(
          "<thead><tr>",
          paste0(
            "<th scope=\"col\">",
            escape(block$header),
            "</th>",
            collapse = ""
          ),
          "</tr></thead>"
        ),
        "<tbody>",
        vapply(
          block$rows,
          function(row) {
            paste0(
              "<tr>",
              paste0(
                "<td>",
                vapply(row, report_html_inline, ""),
                "</td>",
                collapse = ""
              ),
              "</tr>"
            )
          },
          ""
        ),
        "</tbody></table></div>"
      )
    )
  })
  paste(
    c(
      "<!DOCTYPE html>",
      "<html lang=\"en\">",
      "<head>",
      "<meta charset=\"utf-8\">",
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
      "<meta name=\"referrer\" content=\"no-referrer\">",
      sprintf("<title>%s: scans report</title>", escape(title)),
      paste0("<style>", report_html_style, "</style>"),
      "</head>",
      "<body>",
      "<main>",
      unlist(body, use.names = FALSE),
      "</main>",
      "</body>",
      "</html>"
    ),
    collapse = "\n"
  )
}
