scans_app_resources_ui <- function(data, index) {
  rows <- data$measures[
    data$measures$trajectory_id == data$info$trajectory_id[[index]],
  ]
  labels <- c(
    "Conversation elapsed",
    "Input tokens",
    "Output tokens",
    "Cached input tokens",
    "Turn duration sum",
    "Model duration sum",
    "Tool duration sum",
    "Recorded work",
    "Time outside recorded work",
    "First-chunk latency (median)",
    "First-token latency",
    "Comparable cost"
  )
  htmltools::tags$details(
    class = "scans-app-resources",
    htmltools::tags$summary("Resource measurements"),
    htmltools::tags$p(
      "Coverage counts eligible captured records, not all work the application performed. Expand a measure for its source and interpretation."
    ),
    htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      value <- if (is.na(row$value)) {
        "Unavailable"
      } else if (identical(row$unit, "s")) {
        scans_app_seconds(row$value)
      } else {
        scans_app_count(row$value)
      }
      coverage <- if (is.na(row$n_total) || is.na(row$n_recorded)) {
        paste(row$coverage, "coverage")
      } else {
        sprintf(
          "%s; %d / %d records",
          row$coverage,
          row$n_recorded,
          row$n_total
        )
      }
      htmltools::tags$details(
        htmltools::tags$summary(
          htmltools::tags$span(labels[[i]]),
          htmltools::tags$strong(value),
          htmltools::tags$small(coverage)
        ),
        htmltools::tags$p(row$limitation),
        htmltools::tags$small(sprintf(
          "Source: %s. Source unit: %s. Aggregation: %s. Adapter: %s. Contract: %d.",
          row$source,
          if (is.na(row$source_unit)) "unknown" else row$source_unit,
          row$aggregation,
          row$adapter,
          row$contract_version
        ))
      )
    }))
  )
}
