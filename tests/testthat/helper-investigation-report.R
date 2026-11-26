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

# testServer has no browser to apply the input updates a server sends. This
# records them so a test can acknowledge them as a browser would, and so
# follow a link through the app's own navigation.
local_input_updates <- function(env = parent.frame()) {
  updates <- new.env(parent = emptyenv())
  record <- function(id, value) {
    if (!is.null(value)) assign(id, value, envir = updates)
  }
  testthat::local_mocked_bindings(
    update_toolbar_input_select = function(id, ..., selected = NULL) {
      record(id, selected)
    },
    update_switch = function(id, label = NULL, value = NULL, ...) {
      record(id, value)
    },
    .package = "bslib",
    .env = env
  )
  testthat::local_mocked_bindings(
    updateTextInput = function(session, inputId, ..., value = NULL) {
      record(inputId, value)
    },
    updateSelectInput = function(session, inputId, ..., selected = NULL) {
      record(inputId, selected)
    },
    .package = "shiny",
    .env = env
  )
  updates
}

input_updates_ack <- function(session, updates) {
  values <- mget(ls(updates), envir = updates)
  rm(list = names(values), envir = updates)
  if (length(values)) do.call(session$setInputs, values)
}

# Open a link's hash in a test session and acknowledge what navigation
# changes. Returns the trajectory ids then in view, in snapshot order.
report_follow_link <- function(session, updates, link) {
  session$setInputs(scans_app_hash = sub("^[^#]*#", "", link))
  input_updates_ack(session, updates)
  ids <- session$env$data()$info$trajectory_id
  ids[sort(session$env$visible())]
}
