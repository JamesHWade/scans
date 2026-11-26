# The comparison view shows a `compare_trajectories()` result. Every number
# and reason on it is read from that value; nothing is computed here.

scans_app_comparison_sources <- function(x, call = rlang::caller_env()) {
  pairing <- x$pairing
  if (!inherits(pairing, "scans_pairing")) {
    scans_abort(
      "{.arg x} must be a comparison from {.fn compare_trajectories}.",
      class = "scans_error_app_source",
      call = call
    )
  }
  labels <- paste0(
    c("Baseline: ", "Candidate: "),
    pairing$snapshots$application
  )
  # The labels name the sides for display. Annotations and saved snapshots
  # keep the application each investigation records.
  catalog <- scans_app_source_catalog(Map(
    function(label, x) {
      source <- scans_app_source(label, x)
      source$application <- x$manifest$application
      source
    },
    labels,
    list(pairing$baseline, pairing$candidate),
    USE.NAMES = FALSE
  ))
  catalog$comparison <- x
  catalog
}

scans_app_comparison_filters <- c(
  "All matched cases" = "all",
  "Regressions" = "regressed",
  "Improvements" = "improved",
  "Pattern changes" = "patterns",
  "Resource changes" = "resources",
  "Not comparable" = "not_comparable"
)

# Selects pairs by the per-family counts `compare_trajectories()` recorded.
scans_app_comparison_pairs <- function(comparison, filter = "all") {
  cases <- comparison$cases
  keep <- switch(
    filter,
    regressed = cases$outcomes_regressed > 0L,
    improved = cases$outcomes_improved > 0L,
    patterns = cases$patterns_appeared +
      cases$patterns_resolved +
      cases$patterns_no_longer_applicable +
      cases$patterns_newly_applicable >
      0L,
    resources = cases$resources_increased + cases$resources_decreased > 0L,
    not_comparable = cases$outcomes_not_comparable +
      cases$patterns_not_comparable +
      cases$resources_unknown >
      0L,
    rep(TRUE, nrow(cases))
  )
  cases$pair_id[keep]
}

scans_app_comparison_max_cases <- 100L

scans_app_comparison_link <- function(
  label,
  trajectory_id,
  text,
  event_id = NULL
) {
  href <- paste0("#", scans_app_hash(label, trajectory_id))
  if (!is.null(event_id)) {
    href <- paste0(
      href,
      "?event=",
      utils::URLencode(event_id, reserved = TRUE, repeated = TRUE)
    )
  }
  htmltools::tags$a(href = href, text)
}

scans_app_comparison_ui <- function(sources) {
  comparison <- sources$comparison
  pairing <- comparison$pairing
  labels <- sources$labels
  filters <- scans_app_comparison_filters
  names(filters) <- sprintf(
    "%s (%d)",
    names(filters),
    vapply(
      filters,
      \(filter) length(scans_app_comparison_pairs(comparison, filter)),
      integer(1)
    )
  )
  paired <- comparison$design$type == "paired"
  htmltools::tags$details(
    class = "scans-app-comparison",
    open = NA,
    htmltools::tags$style(htmltools::HTML(scans_app_comparison_css)),
    htmltools::tags$summary(
      htmltools::tags$strong("Comparison"),
      sprintf(
        " %s \u2192 %s",
        pairing$snapshots$application[[1L]],
        pairing$snapshots$application[[2L]]
      )
    ),
    htmltools::tags$p(
      class = "scans-app-comparison-identity",
      sprintf(
        "Key: %s; %s. Baseline snapshot %s; candidate snapshot %s.",
        paste(pairing$key$by, collapse = ", "),
        pairing_version_text(pairing),
        scans_app_comparison_short_id(pairing$snapshots$snapshot_id[[1L]]),
        scans_app_comparison_short_id(pairing$snapshots$snapshot_id[[2L]])
      )
    ),
    htmltools::tags$section(
      class = "scans-app-comparison-gaps",
      htmltools::tags$h3("Not compared"),
      htmltools::tags$ul(lapply(
        unname(comparison_gap_bullets(comparison)),
        htmltools::tags$li
      )),
      scans_app_comparison_excluded(pairing, labels)
    ),
    htmltools::tags$section(
      htmltools::tags$h3(
        if (paired) {
          sprintf("Paired changes in %d matched cases", nrow(pairing$pairs))
        } else {
          "Unpaired description"
        }
      ),
      if (paired) {
        htmltools::tags$ul(lapply(
          comparison_summary_lines(comparison$summaries),
          htmltools::tags$li
        ))
      } else {
        htmltools::tagList(
          htmltools::tags$p(
            comparison$design$reason,
            " Each side is described separately; no paired estimate is produced."
          ),
          lapply(
            names(comparison$summaries),
            function(name) {
              scans_app_comparison_table(
                comparison$summaries[[name]],
                scans_app_title_case(name)
              )
            }
          )
        )
      }
    ),
    if (paired) {
      htmltools::tags$section(
        htmltools::div(
          class = "scans-app-comparison-filter",
          shiny::selectInput(
            "scans_app_comparison_filter",
            "Show cases",
            choices = filters,
            selected = "all"
          )
        ),
        shiny::uiOutput("scans_app_comparison_cases")
      )
    }
  )
}

scans_app_comparison_server <- function(input, output, session, sources) {
  comparison <- sources$comparison
  if (is.null(comparison)) {
    return(invisible(NULL))
  }
  output$scans_app_comparison_cases <- shiny::renderUI({
    filter <- input$scans_app_comparison_filter %||% "all"
    if (!filter %in% scans_app_comparison_filters) {
      filter <- "all"
    }
    scans_app_comparison_cases_ui(comparison, sources$labels, filter)
  })
}

scans_app_comparison_short_id <- function(id) {
  substr(sub("^sha256:", "", id), 1L, 12L)
}

# Unmatched, duplicated, and missing-key trajectories, each linked on its side.
scans_app_comparison_excluded <- function(pairing, labels) {
  side_label <- c(baseline = labels[[1L]], candidate = labels[[2L]])
  item <- function(side, trajectory_id, why) {
    htmltools::tags$li(
      scans_app_comparison_link(
        side_label[[side]],
        trajectory_id,
        trajectory_id
      ),
      sprintf(" (%s): %s", side, why)
    )
  }
  rows <- function(table, side, why) {
    lapply(seq_len(nrow(table)), function(i) {
      item(
        side %||% table$side[[i]],
        table$trajectory_id[[i]],
        if (is.function(why)) why(table[i, ]) else why
      )
    })
  }
  items <- c(
    rows(pairing$missing_key, NULL, function(row) {
      paste("missing", paste(row$missing[[1L]], collapse = ", "))
    }),
    rows(pairing$duplicates, NULL, function(row) {
      sprintf(
        "key occurs %d times in the baseline and %d in the candidate",
        row$n_baseline,
        row$n_candidate
      )
    }),
    rows(pairing$only_baseline, "baseline", "no candidate case with this key"),
    rows(pairing$only_candidate, "candidate", "no baseline case with this key")
  )
  if (!length(items)) {
    return(NULL)
  }
  shown <- utils::head(items, scans_app_comparison_max_cases)
  htmltools::tags$details(
    htmltools::tags$summary(sprintf(
      "%d trajectories outside every matched case",
      length(items)
    )),
    htmltools::tags$ul(shown),
    if (length(items) > length(shown)) {
      htmltools::tags$p(sprintf(
        "Showing the first %d of %d.",
        length(shown),
        length(items)
      ))
    }
  )
}

scans_app_comparison_cases_ui <- function(comparison, labels, filter = "all") {
  pair_ids <- scans_app_comparison_pairs(comparison, filter)
  if (!length(pair_ids)) {
    return(htmltools::tags$p(
      class = "scans-app-comparison-empty",
      "No matched case has this kind of change."
    ))
  }
  shown <- utils::head(pair_ids, scans_app_comparison_max_cases)
  htmltools::tagList(
    if (length(shown) < length(pair_ids)) {
      htmltools::tags$p(sprintf(
        "Showing the first %d of %d cases.",
        length(shown),
        length(pair_ids)
      ))
    },
    lapply(shown, scans_app_comparison_case, comparison, labels)
  )
}

scans_app_comparison_case <- function(pair_id, comparison, labels) {
  pair <- comparison$cases[comparison$cases$pair_id == pair_id, ]
  of_pair <- function(table) table[table$pair_id == pair_id, ]
  key <- c(
    task = pair$task_id,
    sample = pair$sample_id,
    epoch = pair$epoch
  )
  key <- key[!is.na(key)]
  link <- function(side, text, event_id = NULL) {
    scans_app_comparison_link(
      labels[[match(side, pairing_sides)]],
      pair[[paste0(side, "_trajectory_id")]],
      text,
      event_id
    )
  }
  event_links <- function(side, event_ids) {
    lapply(seq_along(event_ids), function(i) {
      htmltools::tagList(
        " ",
        link(side, sprintf("%s event %d", side, i), event_ids[[i]])
      )
    })
  }

  outcomes <- of_pair(comparison$outcomes)
  outcome_items <- lapply(seq_len(nrow(outcomes)), function(i) {
    row <- outcomes[i, ]
    htmltools::tags$li(
      class = paste0(
        "scans-app-comparison-",
        scans_app_comparison_or(row$change, "unknown")
      ),
      sprintf(
        "%s: %s \u2192 %s, %s",
        scans_app_comparison_or(row$scorer_id, "unrecorded scorer"),
        scans_app_comparison_or(row$baseline_value, "no value"),
        scans_app_comparison_or(row$candidate_value, "no value"),
        if (row$comparable) {
          row$change
        } else {
          paste0("not comparable (", row$compatibility, "). ", row$reason)
        }
      )
    )
  })

  patterns <- of_pair(comparison$patterns)
  patterns <- patterns[patterns$change != "absent", ]
  pattern_items <- lapply(seq_len(nrow(patterns)), function(i) {
    row <- patterns[i, ]
    htmltools::tags$li(
      class = paste0("scans-app-comparison-", row$change),
      sprintf("%s: %s", row$scan, gsub("_", " ", row$change)),
      if (!is.na(row$reason)) paste0(". ", row$reason),
      event_links("baseline", row$baseline_event_ids[[1L]]),
      event_links("candidate", row$candidate_event_ids[[1L]])
    )
  })

  resources <- of_pair(comparison$resources)
  resources <- resources[
    !is.na(resources$baseline_value) | !is.na(resources$candidate_value),
  ]
  resource_items <- lapply(seq_len(nrow(resources)), function(i) {
    row <- resources[i, ]
    value <- function(x) if (is.na(x)) "unknown" else format(x)
    # Two units name each value's own; one names both.
    units <- if (is.na(row$unit)) {
      c(
        scans_app_comparison_or(row$baseline_unit, ""),
        scans_app_comparison_or(row$candidate_unit, "")
      )
    } else {
      c("", row$unit)
    }
    htmltools::tags$li(
      sprintf(
        "%s: %s \u2192 %s; %s",
        row$measure,
        trimws(paste(value(row$baseline_value), units[[1L]])),
        trimws(paste(value(row$candidate_value), units[[2L]])),
        if (row$comparable) {
          paste("change", format(row$delta))
        } else {
          paste("no delta.", row$reason)
        }
      ),
      sprintf(
        " (coverage: %s, %s)",
        scans_app_comparison_or(row$baseline_coverage, "unmeasured"),
        scans_app_comparison_or(row$candidate_coverage, "unmeasured")
      )
    )
  })

  family <- function(title, items, empty) {
    htmltools::div(
      class = "scans-app-comparison-family",
      htmltools::tags$h5(title),
      if (length(items)) {
        htmltools::tags$ul(items)
      } else {
        htmltools::tags$p(empty)
      }
    )
  }
  htmltools::tags$article(
    class = "scans-app-comparison-case",
    htmltools::tags$h4(
      paste(names(key), key, collapse = " \u00b7 "),
      " ",
      link("baseline", "Open baseline"),
      " ",
      link("candidate", "Open candidate")
    ),
    family("Outcomes", outcome_items, "No evaluation on either side."),
    family(
      "Diagnostic patterns",
      pattern_items,
      "No finding on either side in any compared scan."
    ),
    family(
      "Resources",
      resource_items,
      "No resource value recorded on either side."
    )
  )
}

scans_app_comparison_or <- function(x, y) if (is.na(x)) y else x

scans_app_comparison_table <- function(table, title) {
  cell <- function(x) {
    if (is.list(x)) {
      x <- vapply(x, \(value) paste(value, collapse = ", "), "")
    }
    ifelse(is.na(x), "unknown", format(x))
  }
  htmltools::tags$details(
    htmltools::tags$summary(sprintf(
      "%s by side (%d rows)",
      title,
      nrow(table)
    )),
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$thead(htmltools::tags$tr(lapply(
        gsub("_", " ", names(table)),
        htmltools::tags$th
      ))),
      htmltools::tags$tbody(lapply(seq_len(nrow(table)), function(i) {
        htmltools::tags$tr(lapply(
          names(table),
          \(column) htmltools::tags$td(cell(table[[column]][[i]]))
        ))
      }))
    )
  )
}

scans_app_comparison_css <- "
.scans-app-comparison { padding: 1rem 1.2rem; border-bottom: 1px solid var(--bs-border-color); }
.scans-app-comparison > summary { font-size: 1.05rem; cursor: pointer; }
.scans-app-comparison h3 { font-size: 1rem; margin: 1rem 0 0.4rem; }
.scans-app-comparison h4 { font-size: 0.95rem; margin: 0 0 0.4rem; }
.scans-app-comparison h4 a { font-weight: 400; margin-left: 0.5rem; }
.scans-app-comparison h5 { font-size: 0.8rem; margin: 0.4rem 0 0.1rem; color: var(--bs-secondary-color); }
.scans-app-comparison ul { margin-bottom: 0.3rem; }
.scans-app-comparison-identity { color: var(--bs-secondary-color); margin: 0.3rem 0 0; }
.scans-app-comparison-gaps { border-left: 3px solid var(--bs-warning); padding-left: 0.8rem; }
.scans-app-comparison-case { border: 1px solid var(--bs-border-color); border-radius: 0.4rem; padding: 0.7rem 0.9rem; margin-bottom: 0.6rem; }
.scans-app-comparison-family p { margin: 0; color: var(--bs-secondary-color); }
"
