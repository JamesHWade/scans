scans_app_context_ui <- function(info) {
  fields <- c(
    "Trajectory" = info$trajectory_id[[1L]],
    "Run" = info$run_id[[1L]],
    "Parent" = info$parent_trajectory_id[[1L]],
    "Source" = info$source_type[[1L]],
    "Source ID" = info$source_id[[1L]],
    "Source URI" = info$source_uri[[1L]],
    "Task" = info$task_id[[1L]],
    "Sample" = info$sample_id[[1L]],
    "Epoch" = info$epoch[[1L]],
    "Agent" = info$agent[[1L]],
    "Model" = info$model[[1L]],
    "Started" = scans_app_time_string(info$started_at[[1L]]),
    "Completed" = scans_app_time_string(info$completed_at[[1L]]),
    "Error" = info$error[[1L]]
  )
  fields <- fields[!is.na(fields) & nzchar(fields)]
  metadata <- info$metadata[[1L]]
  has_metadata <- is.list(metadata) && length(metadata) > 0L
  if (length(fields) == 0L && !has_metadata) {
    return(scans_app_empty_ui(
      "No additional context is recorded.",
      compact = TRUE
    ))
  }
  htmltools::tags$dl(
    class = "scans-app-context",
    htmltools::tagList(Map(
      function(label, value) {
        htmltools::tagList(
          htmltools::tags$dt(label),
          htmltools::tags$dd(value)
        )
      },
      names(fields),
      fields
    )),
    if (has_metadata) {
      htmltools::tagList(
        htmltools::tags$dt("Metadata"),
        htmltools::tags$dd(
          htmltools::tags$pre(
            htmltools::tags$code(scans_app_value_text(metadata))
          )
        )
      )
    }
  )
}

scans_app_labeled_value <- function(label, value) {
  text <- scans_app_value_text(value)
  if (is.null(text)) {
    return(NULL)
  }
  htmltools::div(
    class = "scans-app-labeled-value",
    htmltools::tags$strong(label),
    htmltools::tags$pre(htmltools::tags$code(text))
  )
}

scans_app_badge <- function(text, tone = "quiet") {
  if (!scans_app_has_string(text)) {
    return(NULL)
  }
  htmltools::tags$span(
    class = paste0(
      "scans-app-badge scans-app-badge-",
      scans_app_css_token(tone)
    ),
    text
  )
}

scans_app_status_badge <- function(status) {
  if (!scans_app_has_string(status)) {
    return(NULL)
  }
  tone <- if (status %in% c("completed", "succeeded", "success", "passed")) {
    "success"
  } else if (status %in% c("failed", "error")) {
    "danger"
  } else {
    "quiet"
  }
  scans_app_badge(scans_app_title_case(status), tone)
}

scans_app_empty_ui <- function(text, compact = FALSE) {
  htmltools::div(
    class = paste(
      "scans-app-empty",
      if (compact) "scans-app-empty-compact" else ""
    ),
    text
  )
}

scans_app_value_text <- function(value, max_chars = 4000L) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }
  text <- paste(
    utils::capture.output(
      utils::str(
        value,
        max.level = 4L,
        give.attr = FALSE,
        strict.width = "cut",
        width = 80L
      )
    ),
    collapse = "\n"
  )
  scans_app_truncate(text, max_chars)
}

scans_app_truncate <- function(text, max_chars) {
  if (is.na(text) || nchar(text) <= max_chars) {
    return(text)
  }
  paste0(substr(text, 1L, max_chars - 1L), "\u2026")
}

scans_app_has_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

scans_app_title_case <- function(x) {
  if (!scans_app_has_string(x)) {
    return("")
  }
  x <- gsub("_", " ", x)
  paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

scans_app_css_token <- function(x) {
  token <- tolower(scans_app_first_string(x, "quiet"))
  gsub("[^a-z0-9-]+", "-", token)
}

scans_app_event_dom_id <- function(row) {
  paste0("scans-app-event-", row)
}

scans_app_time_string <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

scans_app_dependency <- function(package_version = utils::packageVersion) {
  htmltools::htmlDependency(
    name = "scans-app",
    version = as.character(package_version("scans")),
    src = c(
      file = system.file("www", "scans-app", package = "scans")
    ),
    stylesheet = "scans-app.css"
  )
}
