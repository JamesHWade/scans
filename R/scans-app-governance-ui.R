# Deputy run evidence and commons provenance in the app. Every fact here is
# read from recorded metadata, events, or the route tables; nothing is
# inferred from correlation.

# The run ending as one short fact, or NULL when none is recorded. A cut-off
# answer is flagged because its status still reads "completed".
scans_app_run_ending <- function(metadata) {
  ending <- scan_run_ending(metadata)
  if (is.null(ending)) {
    return(NULL)
  }
  class <- scan_ending_string(ending, "class")
  detail <- function(name) {
    value <- scan_ending_string(ending, name)
    if (is.na(value)) NULL else value
  }
  cut_off <- scan_is_cut_off(ending)
  text <- if (cut_off) {
    paste(c("Answer cut off", detail("finish_reason")), collapse = " · ")
  } else if (class %in% "RunEndingAnswered") {
    "Answered"
  } else if (class %in% "RunEndingStopped") {
    paste(
      c("Stopped by", detail("by") %||% "an unrecorded party"),
      collapse = " "
    )
  } else if (class %in% "RunEndingLimit") {
    paste(
      c(
        "Limit reached",
        detail("field"),
        if (isTRUE(scan_ending_flag(ending, "wrapped_up"))) "wrapped up"
      ),
      collapse = " · "
    )
  } else if (class %in% "RunEndingFailed") {
    paste("Failed in", detail("phase") %||% "an unrecorded phase")
  } else if (!is.na(class)) {
    paste("Ending:", sub("^RunEnding", "", class))
  } else {
    return(NULL)
  }
  list(text = text, cut_off = cut_off)
}

scans_app_run_ending_ui <- function(metadata) {
  ending <- scans_app_run_ending(metadata)
  if (is.null(ending)) {
    return(NULL)
  }
  htmltools::tags$span(
    class = paste(
      "scans-app-run-ending",
      if (ending$cut_off) "scans-app-run-ending-cut-off"
    ),
    if (ending$cut_off) scans_app_marker("error"),
    ending$text
  )
}

# Lineage comes from `parent_trajectory_id` alone.
scans_app_lineage_ui <- function(data, index) {
  id <- data$info$trajectory_id[[index]]
  parent <- data$info$parent_trajectory_id[[index]]
  children <- which(data$info$parent_trajectory_id %in% id)
  link <- function(row) {
    htmltools::tags$button(
      type = "button",
      class = "scans-app-inspect",
      `data-scans-trajectory` = data$info$trajectory_id[[row]],
      data$records$title[[row]]
    )
  }
  if (is.na(parent) && !length(children)) {
    return(NULL)
  }
  parent_row <- match(parent, data$info$trajectory_id)
  htmltools::div(
    class = "scans-app-lineage",
    if (!is.na(parent)) {
      htmltools::tags$span(
        "Delegated by ",
        if (is.na(parent_row)) {
          htmltools::tags$code(title = "Not in this snapshot", parent)
        } else {
          link(parent_row)
        }
      )
    },
    if (length(children)) {
      htmltools::tags$span(
        sprintf(
          "%s: ",
          if (length(children) == 1L) "Subagent run" else "Subagent runs"
        ),
        lapply(children, link)
      )
    }
  )
}

scans_app_interventions_ui <- function(
  interventions,
  facets = list(),
  noun = scans_app_noun()
) {
  n_deputy <- attr(interventions, "n_deputy")
  limitations <- attr(interventions, "limitations")
  htmltools::tagList(
    htmltools::tags$p(
      class = "scans-app-section-note",
      sprintf(
        "Recorded on %s with Deputy run evidence.",
        scans_app_n(n_deputy, noun)
      ),
      if (length(limitations)) paste0(" ", paste(limitations, collapse = " "))
    ),
    if (nrow(interventions) == 0L) {
      htmltools::tags$p(
        class = "scans-app-no-findings",
        sprintf(
          "No run interventions recorded in %s.",
          scans_app_n(n_deputy, noun)
        )
      )
    } else {
      htmltools::div(
        class = "scans-app-performance-table scans-app-cohort-table",
        htmltools::tags$table(
          class = "table table-sm",
          htmltools::tags$thead(htmltools::tags$tr(lapply(
            c("Intervention", "Affected runs", "Recorded", "Last seen"),
            function(label) htmltools::tags$th(scope = "col", label)
          ))),
          htmltools::tags$tbody(lapply(
            seq_len(nrow(interventions)),
            function(i) {
              row <- interventions[i, ]
              active <- identical(facets$kind, row$kind)
              htmltools::tags$tr(
                class = if (active) "scans-app-row-selected",
                `data-scans-facet` = "kind",
                `data-scans-value` = row$kind,
                htmltools::tags$th(
                  scope = "row",
                  htmltools::tags$button(
                    type = "button",
                    class = "scans-app-facet-link",
                    `data-scans-facet` = "kind",
                    `data-scans-value` = row$kind,
                    `aria-pressed` = if (active) "true" else "false",
                    `aria-label` = sprintf(
                      "%s: %d of %d runs with Deputy evidence, recorded %d times. Show these %s.",
                      row$label,
                      row$trajectories,
                      n_deputy,
                      row$findings,
                      noun$many
                    ),
                    row$label
                  )
                ),
                htmltools::tags$td(
                  class = "scans-app-cell-meter",
                  sprintf("%d of %d", row$trajectories, n_deputy),
                  scans_app_meter(row$trajectories / n_deputy, "warning")
                ),
                htmltools::tags$td(scans_app_count(row$findings)),
                htmltools::tags$td(
                  if (is.na(row$last_seen)) {
                    htmltools::tags$span(class = "scans-app-unknown", "Unknown")
                  } else {
                    format(row$last_seen, "%b %d", tz = "UTC")
                  }
                )
              )
            }
          ))
        )
      )
    }
  )
}

scans_app_exchange_noun <- list(one = "exchange", many = "exchanges")

# Class counts as facet buttons, then the per-bucket chart of the same
# classes. Unclassified is always listed, apart from untrusted.
scans_app_provenance_ui <- function(
  provenance,
  activity,
  facets = list(),
  noun = scans_app_noun()
) {
  total <- attr(provenance, "total_exchanges")
  sources <- attr(provenance, "total_trajectories")
  chart <- activity["bucket"]
  for (i in seq_len(nrow(provenance))) {
    chart[[paste0("n_", provenance$class[[i]])]] <- provenance$series[[i]]
  }
  chart$n <- as.integer(rowSums(chart[-1L]))
  attr(chart, "unit") <- attr(activity, "unit")
  attr(chart, "basis") <- attr(activity, "basis")
  attr(chart, "n_undated") <- attr(provenance, "n_undated")
  htmltools::tagList(
    htmltools::tags$p(
      class = "scans-app-section-note",
      sprintf(
        "%s in %s that record provenance. Unclassified means no usable record, not untrusted.",
        scans_app_plural(total, "exchange"),
        scans_app_n(sources, noun)
      )
    ),
    htmltools::div(
      class = "scans-app-provenance-classes",
      lapply(seq_len(nrow(provenance)), function(i) {
        row <- provenance[i, ]
        htmltools::tags$button(
          type = "button",
          class = "scans-app-tile scans-app-tile-action scans-app-provenance-class",
          `data-scans-facet` = "provenance",
          `data-scans-value` = row$class,
          `aria-pressed` = if (identical(facets$provenance, row$class)) {
            "true"
          } else {
            "false"
          },
          `aria-label` = sprintf(
            "%s: %d of %d exchanges, in %d of %d %s. Show these %s.",
            row$label,
            row$exchanges,
            total,
            row$trajectories,
            sources,
            noun$many,
            noun$many
          ),
          htmltools::tags$span(
            class = "scans-app-tile-label",
            htmltools::tags$i(
              class = paste0("scans-app-swatch scans-app-series-", row$class)
            ),
            row$label
          ),
          htmltools::tags$span(
            class = "scans-app-tile-figure",
            scans_app_count(row$exchanges)
          ),
          htmltools::tags$span(
            class = "scans-app-tile-note",
            sprintf(
              "of %s · %s · in %s",
              scans_app_plural(total, "exchange"),
              scans_app_percent(row$share),
              scans_app_n(row$trajectories, noun)
            )
          )
        )
      })
    ),
    scans_app_activity_ui(
      chart,
      facets$day,
      scans_app_exchange_noun,
      classes = cohort_provenance_classes,
      phrases = tolower(cohort_provenance_classes),
      about = "recorded provenance class"
    )
  )
}

scans_app_route_ui <- function(signature) {
  steps <- strsplit(signature, " > ", fixed = TRUE)[[1L]]
  htmltools::tags$span(
    class = "scans-app-route",
    lapply(seq_along(steps), function(i) {
      htmltools::tagList(
        if (i > 1L) {
          htmltools::tags$span(
            class = "scans-app-route-step-sep",
            `aria-hidden` = "true",
            "→"
          )
        },
        htmltools::tags$span(class = "scans-app-mono", steps[[i]])
      )
    })
  )
}

# Recorded tool inputs and outputs, shown as text. Nothing here is run.
scans_app_route_snippets_ui <- function(snippets) {
  if (nrow(snippets) == 0L) {
    return(NULL)
  }
  htmltools::tags$details(
    class = "scans-app-route-snippets",
    htmltools::tags$summary(sprintf(
      "Recorded tool input and output (%s)",
      scans_app_plural(nrow(snippets), "call")
    )),
    htmltools::tags$p(
      "Text as recorded in the traces, cut to a fixed length. It is shown for reading and is never executed."
    ),
    lapply(seq_len(nrow(snippets)), function(i) {
      row <- snippets[i, ]
      cut <- function(shown, full) {
        if (!is.na(full) && !is.na(shown) && nchar(shown) < full) {
          sprintf(" (first %d of %d characters)", nchar(shown), full)
        }
      }
      htmltools::div(
        class = "scans-app-route-snippet",
        htmltools::tags$strong(class = "scans-app-mono", row$tool),
        htmltools::tags$small(row$trajectory_id),
        htmltools::tags$div(
          class = "scans-app-route-snippet-label",
          "Recorded input",
          cut(row$input, row$input_chars)
        ),
        htmltools::tags$pre(
          if (is.na(row$input)) "Not recorded" else row$input
        ),
        htmltools::tags$div(
          class = "scans-app-route-snippet-label",
          "Recorded output",
          cut(row$output, row$output_chars)
        ),
        htmltools::tags$pre(
          if (is.na(row$output)) "Not recorded" else row$output
        )
      )
    })
  )
}

scans_app_candidates_ui <- function(
  candidates,
  facets = list(),
  noun = scans_app_noun()
) {
  classes <- names(cohort_provenance_classes)
  htmltools::tagList(
    htmltools::tags$p(
      class = "scans-app-section-note",
      "A candidate is a recurring exploratory route: the same sequence of tools answered several exchanges recorded as untrusted or cited, or from a source without provenance. It could become a governed measure."
    ),
    htmltools::div(
      class = "scans-app-candidates",
      lapply(seq_len(nrow(candidates)), function(i) {
        row <- candidates[i, ]
        counts <- unlist(row[paste0("n_", classes)])
        active <- identical(facets$route, row$route_signature)
        htmltools::div(
          class = paste(
            "scans-app-candidate",
            if (active) "scans-app-candidate-selected"
          ),
          htmltools::tags$button(
            type = "button",
            class = "scans-app-candidate-row",
            `data-scans-facet` = "route",
            `data-scans-value` = row$route_signature,
            `aria-pressed` = if (active) "true" else "false",
            `aria-label` = sprintf(
              "Route %s: %d exchanges in %s. Show these %s.",
              row$route_signature,
              row$n_exchanges,
              scans_app_n(row$n_trajectories, noun),
              noun$many
            ),
            scans_app_route_ui(row$route_signature),
            htmltools::tags$span(scans_app_n(row$n_trajectories, noun)),
            htmltools::tags$span(scans_app_plural(row$n_exchanges, "exchange")),
            htmltools::tags$span(
              if (row$n_users == 0L && row$n_unknown_user_trajectories > 0L) {
                htmltools::tags$span(
                  class = "scans-app-unknown",
                  "Users unknown"
                )
              } else {
                scans_app_plural(row$n_users, "user")
              }
            ),
            htmltools::tags$span(
              class = "scans-app-stack",
              title = paste(
                sprintf(
                  "%d %s",
                  counts[counts > 0],
                  tolower(cohort_provenance_classes)[counts > 0]
                ),
                collapse = ", "
              ),
              lapply(which(counts > 0), function(j) {
                htmltools::tags$span(
                  class = paste0("scans-app-series-", classes[[j]]),
                  style = sprintf("flex-grow: %d", counts[[j]])
                )
              }),
              htmltools::tags$span(
                class = "visually-hidden",
                paste(
                  sprintf(
                    "%d %s",
                    counts[counts > 0],
                    tolower(cohort_provenance_classes)[counts > 0]
                  ),
                  collapse = ", "
                )
              )
            ),
            htmltools::tags$span(
              class = "scans-app-candidate-basis",
              if (row$candidate_basis == "provenance") {
                "Untrusted or cited by record"
              } else {
                "Source without provenance"
              }
            )
          ),
          scans_app_route_snippets_ui(row$snippets[[1L]])
        )
      })
    )
  )
}

# A chip for the first turn of each exchange of a provenance source, keyed
# by turn id.
scans_app_exchange_chips <- function(data, id) {
  if (is.null(data$exchanges)) {
    return(list())
  }
  rows <- data$exchanges[data$exchanges$trajectory_id %in% id, , drop = FALSE]
  rows <- rows[
    rows$provenance_class %in% names(cohort_provenance_classes),
    ,
    drop = FALSE
  ]
  chips <- list()
  reasons <- c(
    recorded = "Recorded by the application",
    missing_event = "No provenance record for this exchange",
    missing_tag = "The provenance record has no tag",
    conflicting_tags = "Conflicting provenance records",
    unrecognized_tag = "The recorded tag is not recognized"
  )
  for (i in seq_len(nrow(rows))) {
    first <- rows$turn_ids[[i]]
    if (!length(first)) {
      next
    }
    class <- rows$provenance_class[[i]]
    reason <- rows$provenance_reason[[i]]
    chips[[first[[1L]]]] <- htmltools::tags$span(
      class = paste0("scans-app-provenance-chip scans-app-provenance-", class),
      title = if (reason %in% names(reasons)) reasons[[reason]] else reason,
      htmltools::tags$i(
        class = paste0("scans-app-swatch scans-app-series-", class)
      ),
      sprintf(
        "Exchange %d · %s",
        rows$exchange_index[[i]],
        cohort_provenance_classes[[class]]
      )
    )
  }
  chips
}
