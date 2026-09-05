scans_app_resources_ui <- function(data, index) {
  rows <- data$measures[
    data$measures$trajectory_id == data$info$trajectory_id[[index]],
  ]
  labels <- c(
    elapsed = "Conversation elapsed",
    input_tokens = "Input tokens",
    output_tokens = "Output tokens",
    cached_input_tokens = "Cached input tokens",
    turn_duration = "Turn duration sum",
    model_duration = "Model duration sum",
    tool_duration = "Tool duration sum",
    recorded_work = "Recorded work",
    unattributed_elapsed = "Time outside recorded work",
    first_chunk_latency = "First-chunk latency (median)",
    first_token_latency = "First-token latency",
    cost = "Comparable cost"
  )
  htmltools::tags$details(
    class = "scans-app-resources",
    htmltools::tags$summary("Resource measurements"),
    htmltools::tags$p(
      "Coverage counts eligible source records in the loaded snapshot: spans, turns, tool-call events, or a pair of trajectory bounds, depending on the measure. Expand a measure for its source and interpretation."
    ),
    htmltools::tagList(lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      label <- unname(labels[row$measure])
      if (is.na(label)) {
        label <- row$measure
      }
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
          htmltools::tags$span(label),
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
