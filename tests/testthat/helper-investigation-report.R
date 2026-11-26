# The cohort fixture saved with a capped source read. Trajectory "e" has no
# start time and nothing a scan could assess.
report_fixture <- function(view = list(), ...) {
  investigation_snapshot(
    cohort_fixture(),
    application = "Cohort",
    source = list(
      read_info = list(
        from = as.POSIXct("2026-10-01", tz = "UTC"),
        to = as.POSIXct("2026-10-04", tz = "UTC"),
        n = 5L,
        conversations_found = 9L,
        truncated = TRUE,
        max_spans = 1000L,
        incomplete = FALSE
      )
    ),
    view = view,
    ...
  )
}

# What the app shows for a saved investigation: the data and visible rows its
# server derives, and the overview's numbers for them.
report_app_view <- function(saved) {
  data <- scans_app_data(saved$bundle, saved$settings, saved$analysis)
  indices <- scans_app_view_indices(
    data,
    saved$view,
    data$info$trajectory_id %in% saved$view$annotation_ids
  )
  list(
    data = data,
    ids = data$info$trajectory_id[indices],
    performance = scans_app_performance_data(data, indices)
  )
}

report_html_doc <- function(report, ...) {
  testthat::skip_if_not_installed("htmltools")
  testthat::skip_if_not_installed("xml2")
  xml2::read_html(format(report, "html", ...))
}

report_md_doc <- function(report, ...) {
  testthat::skip_if_not_installed("commonmark")
  testthat::skip_if_not_installed("xml2")
  xml2::read_html(commonmark::markdown_html(
    paste(format(report, "markdown", ...), collapse = "\n"),
    extensions = TRUE
  ))
}
