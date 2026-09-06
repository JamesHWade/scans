scans_app_assessment_labels <- c(
  assessed_no_findings = "Assessed: no findings",
  assessed_with_findings = "Assessed: findings",
  insufficient_evidence = "Insufficient evidence",
  not_applicable = "Not applicable",
  execution_failure = "Execution failure"
)

scans_app_assessments_ui <- function(data, id) {
  rows <- data$assessments[data$assessments$trajectory_id == id, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(scans_app_empty_ui("No scans selected.", compact = TRUE))
  }
  htmltools::tagList(
    htmltools::tags$p(
      "Assessments cover retained evidence. No findings does not establish a successful or fully captured run."
    ),
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      htmltools::tags$details(
        class = "scans-app-assessment",
        open = if (
          row$status %in% c("insufficient_evidence", "execution_failure")
        ) {
          "open"
        } else {
          NULL
        },
        htmltools::tags$summary(
          htmltools::tags$strong(gsub("_", " ", row$scan)),
          " \u2014 ",
          unname(scans_app_assessment_labels[row$status])
        ),
        htmltools::tags$p(row$reason),
        htmltools::tags$p("Required evidence: ", row$required_evidence),
        htmltools::tags$p("Scanner version: ", row$scan_version),
        if (length(row$settings[[1L]])) {
          scans_app_labeled_value("Settings", row$settings[[1L]])
        },
        if (length(row$finding_ids[[1L]])) {
          htmltools::div(
            class = "scans-app-evidence-links",
            htmltools::tags$span("Findings"),
            lapply(row$finding_ids[[1L]], function(id) {
              index <- match(id, data$findings$finding_id)
              htmltools::tags$a(href = paste0("#scans-app-finding-", index), id)
            })
          )
        },
        htmltools::tags$ul(lapply(row$limitations[[1L]], htmltools::tags$li))
      )
    })
  )
}

scans_app_assessment_coverage <- function(assessments) {
  scans <- unique(assessments$scan)
  counts <- lapply(scans, function(scan) {
    rows <- assessments[assessments$scan == scan, , drop = FALSE]
    tibble::tibble(
      scan = scan,
      status = names(scans_app_assessment_labels),
      trajectories = tabulate(
        match(rows$status, names(scans_app_assessment_labels)),
        nbins = length(scans_app_assessment_labels)
      )
    )
  })
  if (!length(counts)) {
    return(tibble::tibble(
      scan = character(),
      status = character(),
      trajectories = integer()
    ))
  }
  trajectory_bind_rows(counts)
}

scans_app_assessment_coverage_ui <- function(coverage) {
  if (nrow(coverage) == 0L) {
    return(scans_app_empty_ui("No scans selected.", compact = TRUE))
  }
  scans <- unique(coverage$scan)
  htmltools::div(
    class = "scans-app-performance-table scans-app-coverage-table",
    htmltools::tags$table(
      class = "table table-sm",
      htmltools::tags$caption(
        "Each cell counts trajectories for one selected scanner in the current filters. Unassessed trajectories are not negative results."
      ),
      htmltools::tags$thead(htmltools::tags$tr(lapply(
        c("Scanner", unname(scans_app_assessment_labels)),
        function(label) htmltools::tags$th(scope = "col", label)
      ))),
      htmltools::tags$tbody(lapply(scans, function(scan) {
        rows <- coverage[coverage$scan == scan, , drop = FALSE]
        htmltools::tags$tr(
          htmltools::tags$th(scope = "row", gsub("_", " ", scan)),
          lapply(rows$trajectories, htmltools::tags$td)
        )
      }))
    )
  )
}
