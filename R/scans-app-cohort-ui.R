# Overview components for the cohort tables. Each visible count is a button
# carrying `data-scans-facet`, so an aggregate always leads to the
# trajectories behind it.

scans_app_plural <- function(n, singular, plural = paste0(singular, "s")) {
  paste(scans_app_count(n), if (n == 1) singular else plural)
}

# The noun for one trajectory in user-facing copy. Connect and OTel cohorts
# are conversations; other sources (evaluation samples, agent runs) are not.
scans_app_noun <- function(source_types = "otel") {
  types <- source_types[!is.na(source_types)]
  if (length(types) > 0L && all(types %in% c("otel", "commons"))) {
    list(one = "conversation", many = "conversations")
  } else {
    list(one = "trajectory", many = "trajectories")
  }
}

scans_app_n <- function(n, noun) {
  scans_app_plural(n, noun$one, noun$many)
}

scans_app_percent <- function(x) {
  if (is.na(x)) {
    return("\u2013")
  }
  if (x > 0 && x < 0.01) {
    return("<1%")
  }
  paste0(round(x * 100), "%")
}

scans_app_day_range <- function(first, last) {
  if (is.na(first)) {
    return(NULL)
  }
  first <- format(first, "%b %d", tz = "UTC")
  last <- format(last, "%b %d", tz = "UTC")
  if (first == last) first else paste(first, "\u2013", last)
}

# One sentence both panes agree on: the match count, the active filters, and
# the window the matching trajectories started in.
scans_app_scope_sentence_ui <- function(
  headline,
  labels = NULL,
  noun = scans_app_noun()
) {
  window <- scans_app_day_range(headline$first_started, headline$last_started)
  htmltools::tags$p(
    class = "scans-app-scope-sentence",
    htmltools::tags$strong(sprintf(
      "%s of %s",
      scans_app_count(headline$n),
      scans_app_count(headline$total)
    )),
    paste0(" ", if (headline$total == 1L) noun$one else noun$many),
    lapply(unname(labels), function(label) {
      htmltools::tagList(
        " \u00b7 ",
        htmltools::tags$span(class = "scans-app-scope-term", label)
      )
    }),
    if (!is.null(window) && !"day" %in% names(labels)) {
      paste0(
        if (identical(headline$time_basis, "activity")) {
          " \u00b7 active "
        } else {
          " \u00b7 started "
        },
        window,
        " UTC"
      )
    }
  )
}

scans_app_tile <- function(label, figure, note, facet = NULL, ...) {
  content <- htmltools::tagList(
    htmltools::tags$span(class = "scans-app-tile-label", label),
    htmltools::tags$span(class = "scans-app-tile-figure", figure),
    htmltools::tags$span(class = "scans-app-tile-note", note)
  )
  if (is.null(facet)) {
    return(htmltools::div(class = "scans-app-tile", content))
  }
  htmltools::tags$button(
    type = "button",
    class = "scans-app-tile scans-app-tile-action",
    `data-scans-facet` = facet,
    ...,
    content
  )
}

scans_app_tiles_ui <- function(
  headline,
  n_empty = 0L,
  scans_on = TRUE,
  facets = list(),
  findings_only = FALSE,
  noun = scans_app_noun(),
  progress = NULL
) {
  n <- headline$n
  unassessed <- n - headline$n_assessed
  failing <- isTRUE(facets$tool_failed) && is.null(facets[["tool"]])
  htmltools::div(
    class = "scans-app-tiles",
    scans_app_tile(
      scans_app_title_case(noun$many),
      scans_app_count(n),
      paste0(
        "of ",
        scans_app_count(headline$total),
        " captured",
        if (n_empty > 0L) {
          paste0(" \u00b7 ", scans_app_count(n_empty), " without a transcript")
        }
      )
    ),
    if (!scans_on) {
      scans_app_tile(
        "With findings",
        "\u2013",
        paste("Scans off \u00b7 no", noun$one, "was assessed")
      )
    } else {
      scans_app_tile(
        "With findings",
        scans_app_count(headline$n_findings),
        paste0(
          "of ",
          scans_app_count(headline$n_assessed),
          " assessed",
          if (unassessed > 0L) {
            paste0(" \u00b7 ", scans_app_count(unassessed), " not assessed")
          },
          if (isTRUE(headline$n_execution_failure > 0L)) {
            paste0(
              " \u00b7 ",
              scans_app_count(headline$n_execution_failure),
              " with an execution failure"
            )
          }
        ),
        facet = "findings",
        `aria-pressed` = if (findings_only) "true" else "false",
        `aria-label` = sprintf(
          "%d of %d assessed %s have findings. Show only %s with findings.",
          headline$n_findings,
          headline$n_assessed,
          noun$many,
          noun$many
        )
      )
    },
    if (headline$tool_calls == 0L) {
      scans_app_tile("Tool calls failed", "\u2013", "No tool calls recorded")
    } else {
      scans_app_tile(
        "Tool calls failed",
        scans_app_count(headline$tool_failed),
        paste0(
          "of ",
          scans_app_plural(headline$tool_calls, "tool call"),
          " \u00b7 in ",
          scans_app_n(headline$tool_trajectories_failed, noun)
        ),
        facet = "tool_failed",
        `aria-pressed` = if (failing) "true" else "false",
        `aria-label` = sprintf(
          "%d of %d tool calls failed, in %s. Show only %s with a failed tool call.",
          headline$tool_failed,
          headline$tool_calls,
          scans_app_n(headline$tool_trajectories_failed, noun),
          noun$many
        )
      )
    },
    if (!is.null(progress) && !is.na(progress$flagged_reviewed)) {
      flagged <- progress$flagged > 0L
      scans_app_tile(
        "Reviewed",
        scans_app_count(
          if (flagged) progress$flagged_reviewed else progress$reviewed
        ),
        if (flagged) {
          paste("of", scans_app_count(progress$flagged), "flagged")
        } else {
          sprintf(
            "of %s \u00b7 none flagged",
            scans_app_n(progress$total, noun)
          )
        }
      )
    },
    scans_app_tile(
      "Elapsed, median",
      if (headline$n_timed == 0L) {
        "Unknown"
      } else {
        scans_app_seconds(headline$median_elapsed)
      },
      paste0(
        if (headline$n_timed > 0L) {
          paste0("p95 ", scans_app_seconds(headline$p95_elapsed), " \u00b7 ")
        },
        sprintf(
          "timed for %s of %s",
          scans_app_count(headline$n_timed),
          scans_app_count(n)
        )
      )
    ),
    scans_app_tile(
      "Tokens, median",
      if (headline$n_tokens == 0L) {
        "Unknown"
      } else {
        scans_app_count(round(headline$median_tokens))
      },
      paste0(
        sprintf(
          "recorded for %s of %s",
          scans_app_count(headline$n_tokens),
          scans_app_count(n)
        ),
        if (!is.na(headline$total_tokens %||% NA)) {
          paste0(
            " \u00b7 total ",
            scans_app_count(headline$total_tokens),
            if (headline$n_delegated > 0L) " (delegated runs counted once)"
          )
        }
      )
    )
  )
}

scans_app_activity_classes <- c(
  error = "Error findings",
  warning = "Warnings only",
  clean = "No findings",
  unassessed = "Not assessed"
)

scans_app_bucket_title <- function(bucket, unit) {
  format(bucket, if (unit == "hour") "%b %d %H:00" else "%a %b %d", tz = "UTC")
}

# Dates under the first and last bar and at each week start; under hourly
# bars, at each midnight. A label next to an end label is dropped.
scans_app_activity_ticks <- function(buckets, unit) {
  n <- length(buckets)
  starts <- if (unit == "hour") {
    format(buckets, "%H", tz = "UTC") == "00"
  } else {
    format(buckets, "%u", tz = "UTC") == "1"
  }
  shown <- which(starts)
  shown <- shown[shown > 2L & shown < n - 1L]
  shown <- sort(unique(c(1L, shown, n)))
  labels <- rep(NA_character_, n)
  labels[shown] <- format(buckets[shown], "%b %d", tz = "UTC")
  if (unit == "hour") {
    ends <- intersect(c(1L, n), shown[!starts[shown]])
    labels[ends] <- format(buckets[ends], "%H:00", tz = "UTC")
  }
  labels
}

# A stacked count per time bucket, one focusable bar per bucket. `classes`
# names the stacked series (keys match `n_<key>` columns and the
# `scans-app-series-<key>` colours); `noun` names what is counted. The
# activity and provenance charts are both this component.
scans_app_activity_ui <- function(
  activity,
  selected = NULL,
  noun = scans_app_noun(),
  classes = scans_app_activity_classes,
  phrases = c(
    "with error findings",
    "with warnings only",
    "with no findings",
    "not assessed"
  ),
  about = "recorded findings"
) {
  unit <- attr(activity, "unit")
  undated <- attr(activity, "n_undated") %||% 0L
  # Buckets use the start time, or the last recorded activity for sources
  # that keep no start.
  started <- !identical(attr(activity, "basis"), "activity")
  timing <- if (started) {
    paste("start", unit)
  } else {
    paste(unit, "of last activity")
  }
  counts <- as.list(colSums(activity[paste0(
    "n_",
    names(classes)
  )]))
  names(counts) <- names(classes)
  breakdown <- function(row) {
    paste(
      sprintf(
        "%d %s",
        unlist(row[paste0("n_", names(classes))]),
        tolower(classes)
      ),
      collapse = ", "
    )
  }
  undated_note <- if (undated > 0L) {
    htmltools::tags$p(
      class = "scans-app-activity-note",
      sprintf(
        "%s no recorded %s and %s not charted.",
        if (undated == 1L) {
          paste("1", noun$one, "has")
        } else {
          paste(scans_app_count(undated), noun$many, "have")
        },
        if (started) "start time" else "time",
        if (undated == 1L) "is" else "are"
      )
    )
  }
  if (nrow(activity) == 0L) {
    return(htmltools::tagList(
      htmltools::tags$p(
        class = "scans-app-activity-note",
        "No times were recorded, so activity over time is unknown."
      )
    ))
  }
  if (nrow(activity) == 1L) {
    values <- unlist(activity[paste0("n_", names(classes))])
    return(htmltools::tagList(
      htmltools::tags$p(
        class = "scans-app-activity-note",
        sprintf(
          "%s %s %s %s UTC: %s.",
          if (activity$n == 1L) {
            paste("The only dated", noun$one)
          } else {
            sprintf("All %s dated %s", scans_app_count(activity$n), noun$many)
          },
          if (started) "started" else "last active",
          if (unit == "hour") "in the hour of" else "on",
          scans_app_bucket_title(activity$bucket, unit),
          paste(
            sprintf("%d %s", values[values > 0], phrases[values > 0]),
            collapse = ", "
          )
        )
      ),
      undated_note
    ))
  }

  ticks_y <- pretty(c(0, max(activity$n, 1L)), n = 3L)
  ticks_y <- ticks_y[ticks_y == round(ticks_y)]
  top <- max(ticks_y)
  tick_style <- sprintf("bottom: %.2f%%", ticks_y / top * 100)
  keys <- cohort_bucket_key(activity$bucket, unit)
  ticks <- scans_app_activity_ticks(activity$bucket, unit)
  n <- nrow(activity)
  bars <- lapply(seq_len(n), function(i) {
    row <- activity[i, ]
    title <- scans_app_bucket_title(row$bucket, unit)
    values <- unlist(row[paste0("n_", names(classes))])
    tops <- top - cumsum(values)
    pressed <- identical(selected, keys[[i]])
    htmltools::tags$button(
      type = "button",
      class = paste(
        "scans-app-bar",
        if (i <= n * 0.2) "scans-app-bar-start",
        if (i > n * 0.8) "scans-app-bar-end"
      ),
      `data-scans-facet` = "day",
      `data-scans-value` = keys[[i]],
      `aria-pressed` = if (pressed) "true" else "false",
      `aria-label` = sprintf(
        "%s: %s of %s. %s.",
        title,
        scans_app_n(row$n, noun),
        scans_app_count(sum(activity$n)),
        breakdown(row)
      ),
      disabled = if (row$n == 0L && !pressed) NA,
      htmltools::tags$svg(
        class = "scans-app-bar-stack",
        viewBox = sprintf("0 0 10 %s", top),
        preserveAspectRatio = "none",
        `aria-hidden` = "true",
        focusable = "false",
        lapply(which(values > 0), function(j) {
          htmltools::tags$rect(
            class = paste0(
              "scans-app-series-",
              names(classes)[[j]]
            ),
            x = 0,
            y = tops[[j]],
            width = 10,
            height = values[[j]]
          )
        })
      ),
      if (unit == "day") {
        htmltools::tags$span(
          class = "scans-app-bar-weekday",
          `aria-hidden` = "true",
          substr(format(row$bucket, "%a", tz = "UTC"), 1L, 1L)
        )
      },
      htmltools::tags$span(
        class = "scans-app-bar-date",
        `aria-hidden` = "true",
        if (is.na(ticks[[i]])) "\u00a0" else ticks[[i]]
      ),
      htmltools::tags$span(
        class = "scans-app-bar-tip",
        `aria-hidden` = "true",
        htmltools::tags$strong(title),
        htmltools::tags$span(scans_app_n(row$n, noun)),
        lapply(which(values > 0), function(j) {
          htmltools::tags$span(
            htmltools::tags$i(
              class = paste0(
                "scans-app-swatch scans-app-series-",
                names(classes)[[j]]
              )
            ),
            sprintf(
              "%d %s",
              values[[j]],
              tolower(classes[[j]])
            )
          )
        })
      )
    )
  })

  htmltools::div(
    class = paste(
      "scans-app-activity",
      if (!is.null(selected)) "scans-app-activity-selected"
    ),
    htmltools::tags$p(
      class = "scans-app-activity-caption",
      sprintf(
        "%s by %s (UTC). Select a bar to filter; arrow keys move between bars.",
        scans_app_title_case(noun$many),
        timing
      )
    ),
    htmltools::div(
      class = "scans-app-activity-plot",
      htmltools::div(
        class = "scans-app-activity-axis",
        `aria-hidden` = "true",
        lapply(seq_along(ticks_y), function(i) {
          htmltools::tags$span(
            style = tick_style[[i]],
            scans_app_count(ticks_y[[i]])
          )
        })
      ),
      htmltools::div(
        class = paste0("scans-app-activity-bars scans-app-activity-", unit),
        role = "group",
        `aria-label` = sprintf(
          "%s by %s, UTC. Select a %s to filter.",
          scans_app_title_case(noun$many),
          timing,
          unit
        ),
        htmltools::div(
          class = "scans-app-activity-grid",
          `aria-hidden` = "true",
          lapply(tick_style, function(style) {
            htmltools::tags$span(style = style)
          })
        ),
        bars
      )
    ),
    htmltools::tags$ul(
      class = "scans-app-legend",
      lapply(names(classes), function(class) {
        htmltools::tags$li(
          htmltools::tags$i(
            class = paste0("scans-app-swatch scans-app-series-", class)
          ),
          classes[[class]],
          htmltools::tags$span(scans_app_count(counts[[class]]))
        )
      })
    ),
    undated_note,
    htmltools::tags$details(
      class = "scans-app-chart-data",
      htmltools::tags$summary("Show data"),
      htmltools::div(
        class = "scans-app-performance-table",
        htmltools::tags$table(
          class = "table table-sm",
          htmltools::tags$caption(sprintf(
            "%s by %s (UTC) and %s.",
            scans_app_title_case(noun$many),
            timing,
            about
          )),
          htmltools::tags$thead(htmltools::tags$tr(lapply(
            c(
              if (unit == "hour") "Hour" else "Day",
              scans_app_title_case(noun$many),
              unname(classes)
            ),
            function(label) htmltools::tags$th(scope = "col", label)
          ))),
          htmltools::tags$tbody(lapply(seq_len(n), function(i) {
            htmltools::tags$tr(
              htmltools::tags$th(
                scope = "row",
                scans_app_bucket_title(activity$bucket[[i]], unit)
              ),
              lapply(
                unlist(activity[i, c("n", paste0("n_", names(classes)))]),
                htmltools::tags$td
              )
            )
          }))
        )
      )
    )
  )
}

# A thin track with the share filled in. The number beside it carries the
# value; the bar only makes rows comparable at a glance.
scans_app_meter <- function(share, tone = "neutral") {
  share <- if (is.na(share)) 0 else min(max(share, 0), 1)
  htmltools::tags$span(
    class = paste0("scans-app-meter scans-app-meter-", tone),
    `aria-hidden` = "true",
    htmltools::tags$span(style = sprintf("width: %.1f%%", share * 100))
  )
}

scans_app_sparkline <- function(series, severity = "warning") {
  if (length(series) < 2L) {
    return(NULL)
  }
  top <- max(series, 1L)
  htmltools::tags$svg(
    class = "scans-app-sparkline",
    viewBox = sprintf("0 0 %d %s", length(series) * 3L, top),
    preserveAspectRatio = "none",
    `aria-hidden` = "true",
    focusable = "false",
    lapply(which(series > 0), function(i) {
      htmltools::tags$rect(
        class = paste0("scans-app-series-", severity),
        x = (i - 1L) * 3L,
        y = top - series[[i]],
        width = 2,
        height = series[[i]]
      )
    })
  )
}

scans_app_duration_cell <- function(value) {
  if (is.na(value)) {
    htmltools::tags$span(class = "scans-app-unknown", "Unknown")
  } else {
    scans_app_seconds(value)
  }
}

scans_app_tools_ui <- function(
  tools,
  facets = list(),
  noun = scans_app_noun()
) {
  selected <- facets[["tool"]]
  failed_only <- isTRUE(facets$tool_failed)
  htmltools::div(
    class = "scans-app-performance-table scans-app-cohort-table",
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$thead(htmltools::tags$tr(lapply(
        c("Tool", "Calls", "Failed", "Top error", "Median", "p95"),
        function(label) {
          htmltools::tags$th(
            scope = "col",
            class = if (label == "Top error") "scans-app-cell-text",
            label
          )
        }
      ))),
      htmltools::tags$tbody(lapply(seq_len(nrow(tools)), function(i) {
        row <- tools[i, ]
        active <- identical(selected, row$tool)
        htmltools::tags$tr(
          class = if (active) "scans-app-row-selected",
          `data-scans-facet` = "tool",
          `data-scans-value` = row$tool,
          htmltools::tags$th(
            scope = "row",
            htmltools::tags$button(
              type = "button",
              class = "scans-app-facet-link scans-app-mono",
              `data-scans-facet` = "tool",
              `data-scans-value` = row$tool,
              `aria-pressed` = if (active && !failed_only) "true" else "false",
              `aria-label` = sprintf(
                "%s: %s in %s. Show these %s.",
                row$tool,
                scans_app_plural(row$calls, "call"),
                scans_app_n(row$trajectories, noun),
                noun$many
              ),
              row$tool
            )
          ),
          htmltools::tags$td(scans_app_count(row$calls)),
          htmltools::tags$td(
            class = "scans-app-cell-meter",
            if (row$failed == 0L) {
              htmltools::tags$span(class = "scans-app-unknown", "0")
            } else {
              htmltools::tags$button(
                type = "button",
                class = "scans-app-facet-link",
                `data-scans-facet` = "tool_failed",
                `data-scans-value` = row$tool,
                `aria-pressed` = if (active && failed_only) {
                  "true"
                } else {
                  "false"
                },
                `aria-label` = sprintf(
                  "%s: %d of %d calls failed, in %s. Show these %s.",
                  row$tool,
                  row$failed,
                  row$calls,
                  scans_app_n(row$trajectories_failed, noun),
                  noun$many
                ),
                htmltools::tags$strong(scans_app_count(row$failed)),
                paste0(" \u00b7 ", scans_app_percent(row$failure_rate))
              )
            },
            scans_app_meter(row$failure_rate, "danger")
          ),
          htmltools::tags$td(
            class = "scans-app-cell-text",
            if (is.na(row$top_error)) {
              if (row$failed > 0L) {
                htmltools::tags$span(
                  class = "scans-app-unknown",
                  "No error text"
                )
              }
            } else {
              htmltools::tagList(
                htmltools::tags$span(class = "scans-app-mono", row$top_error),
                htmltools::tags$small(sprintf(
                  "%s of %s",
                  scans_app_count(row$top_error_n),
                  scans_app_plural(row$failed, "failure")
                ))
              )
            }
          ),
          htmltools::tags$td(
            scans_app_duration_cell(row$median_duration),
            if (row$n_timed > 0L && row$n_timed < row$calls) {
              htmltools::tags$small(sprintf("%d timed", row$n_timed))
            }
          ),
          htmltools::tags$td(scans_app_duration_cell(row$p95_duration))
        )
      }))
    )
  )
}

# Rows of a model or user breakdown. `tokens` adds the usage columns that
# make a model change visible.
scans_app_breakdown_rows <- function(
  rows,
  facet,
  total,
  selected,
  tokens,
  top_n,
  noun,
  limit
) {
  top_tokens <- max(rows$median_tokens, 1, na.rm = TRUE)
  lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    active <- identical(selected, row$value)
    htmltools::tags$tr(
      class = paste(
        if (active) "scans-app-row-selected",
        if (i > limit && !active) "scans-app-row-extra"
      ),
      `data-scans-facet` = facet,
      `data-scans-value` = row$value,
      htmltools::tags$th(
        scope = "row",
        htmltools::tags$button(
          type = "button",
          class = paste(
            "scans-app-facet-link",
            if (facet == "model") "scans-app-mono"
          ),
          `data-scans-facet` = facet,
          `data-scans-value` = row$value,
          `aria-pressed` = if (active) "true" else "false",
          `aria-label` = sprintf(
            "%s: %d of %d %s. %s Show these %s.",
            row$value,
            row$n,
            total,
            noun$many,
            if (row$n_assessed == 0L) {
              sprintf("No %s was assessed.", noun$one)
            } else {
              sprintf(
                "%d of %d assessed %s have findings.",
                row$n_findings,
                row$n_assessed,
                noun$many
              )
            },
            noun$many
          ),
          row$value
        )
      ),
      htmltools::tags$td(
        class = "scans-app-cell-meter",
        scans_app_count(row$n),
        scans_app_meter(row$n / top_n)
      ),
      htmltools::tags$td(
        if (row$n_assessed == 0L) {
          htmltools::tags$span(class = "scans-app-unknown", "Not assessed")
        } else {
          htmltools::tagList(
            scans_app_count(row$n_findings),
            htmltools::tags$small(sprintf(
              "of %s assessed",
              scans_app_count(row$n_assessed)
            )),
            if (tokens && row$n_error > 0L) {
              htmltools::tags$small(sprintf(
                "%s with errors",
                scans_app_count(row$n_error)
              ))
            }
          )
        }
      ),
      if (tokens) {
        htmltools::tagList(
          htmltools::tags$td(
            class = "scans-app-cell-meter",
            if (row$n_tokens == 0L) {
              htmltools::tags$span(class = "scans-app-unknown", "Unknown")
            } else {
              htmltools::tagList(
                scans_app_count(round(row$median_tokens)),
                scans_app_meter(row$median_tokens / top_tokens, "primary"),
                if (row$n_tokens < row$n) {
                  htmltools::tags$small(sprintf(
                    "%d of %d recorded",
                    row$n_tokens,
                    row$n
                  ))
                }
              )
            }
          ),
          htmltools::tags$td(
            scans_app_duration_cell(row$median_elapsed),
            if (row$n_timed > 0L && row$n_timed < row$n) {
              htmltools::tags$small(sprintf(
                "%d of %d timed",
                row$n_timed,
                row$n
              ))
            }
          )
        )
      }
    )
  })
}

scans_app_breakdown_table <- function(
  rows,
  facet,
  label,
  total,
  selected,
  tokens = FALSE,
  noun = scans_app_noun(),
  limit = Inf
) {
  top_n <- max(rows$n, 1L)
  columns <- c(
    label,
    scans_app_title_case(noun$many),
    "With findings",
    if (tokens) c("Median tokens", "Median elapsed")
  )
  htmltools::div(
    class = "scans-app-performance-table scans-app-cohort-table",
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$thead(
        htmltools::tags$tr(lapply(
          columns,
          function(column) htmltools::tags$th(scope = "col", column)
        ))
      ),
      htmltools::tags$tbody(
        scans_app_breakdown_rows(
          rows,
          facet,
          total,
          selected,
          tokens,
          top_n,
          noun,
          limit
        )
      )
    )
  )
}

scans_app_models_ui <- function(
  models,
  total,
  facets = list(),
  noun = scans_app_noun()
) {
  known <- models[models$value != cohort_unknown, , drop = FALSE]
  if (nrow(known) == 0L) {
    return(NULL)
  }
  htmltools::tags$section(
    class = "scans-app-breakdown",
    htmltools::tags$h2("By model"),
    if (nrow(models) == 1L) {
      htmltools::tags$p(
        class = "scans-app-breakdown-fact",
        if (models$n == 1L) {
          paste("This", noun$one, "used ")
        } else {
          sprintf("All %s %s used ", scans_app_count(models$n), noun$many)
        },
        htmltools::tags$span(
          class = "scans-app-mono",
          .noWS = "after",
          models$value
        ),
        sprintf(
          ". Median tokens %s (recorded for %s of %s).",
          if (models$n_tokens == 0L) {
            "unknown"
          } else {
            scans_app_count(round(models$median_tokens))
          },
          scans_app_count(models$n_tokens),
          scans_app_count(models$n)
        )
      )
    } else {
      scans_app_breakdown_table(
        models,
        "model",
        "Model",
        total,
        facets$model,
        tokens = TRUE,
        noun = noun
      )
    }
  )
}

# Rows past `limit` stay in the same table, hidden until the reader asks, so
# their columns line up with the rows above.
scans_app_users_ui <- function(
  users,
  total,
  facets = list(),
  noun = scans_app_noun(),
  limit = 8L
) {
  if (sum(users$value != cohort_unknown) < 2L) {
    return(NULL)
  }
  htmltools::tags$section(
    class = "scans-app-breakdown",
    htmltools::tags$h2("Most active users"),
    scans_app_breakdown_table(
      users,
      "user",
      "User",
      total,
      facets$user,
      noun = noun,
      limit = limit
    ),
    if (nrow(users) > limit) {
      htmltools::tags$button(
        type = "button",
        class = "scans-app-show-rows",
        `data-scans-expand` = "rows",
        `aria-expanded` = "false",
        sprintf("Show all %s users", scans_app_count(nrow(users)))
      )
    }
  )
}
