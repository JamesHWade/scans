investigation_bundle_fixture <- function() {
  as_trajectory_otel(jsonlite::read_json(system.file(
    "extdata",
    "support-investigation.json",
    package = "scans"
  )))
}

# testServer has no browser to acknowledge updateInput messages.
investigation_ack_inputs <- function(session) {
  session$setInputs(
    scans_app_source = ".scans-app-source-all",
    scans_app_status = ".scans-app-status-all",
    scans_app_query = "",
    scans_app_sort = "newest",
    scans_app_findings_only = FALSE,
    scans_app_annotated_only = FALSE,
    scans_app_scans = scan_registry()$scan,
    scans_app_repeat_threshold = 2L,
    scans_app_loop_threshold = 3L,
    scans_app_priority = "elapsed",
    scans_app_view = "application"
  )
}
