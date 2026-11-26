# Sessions share loaded snapshots within one app object. Refresh entries
# older than cache_max_age. Keep failures visible in the session that saw
# them; other sessions may retry the source.
scans_app_server <- function(
  sources,
  annotations = NULL,
  cache_max_age = 30 * 60,
  clock = Sys.time,
  schedule = shiny::invalidateLater,
  annotation_poll_interval = 2000,
  investigations = FALSE,
  reviews = FALSE,
  chat_factory = NULL
) {
  sources <- scans_app_runtime_sources(sources)
  cache <- new.env(parent = emptyenv())

  function(input, output, session) {
    # The session's frame is its shared reactive state. Each helper below
    # registers its reactives here and reads the others' through `state`,
    # lazily, so helpers may refer to reactives a later helper defines.
    state <- environment()
    opened_investigation <- shiny::reactiveVal(NULL)
    pending_investigation <- shiny::reactiveVal(NULL)
    rescan_investigation <- shiny::reactiveVal(FALSE)
    revision <- shiny::reactiveVal(0L)
    reload_revision <- shiny::reactiveVal(0L)
    selected <- shiny::reactiveVal(NULL)
    selected_trajectory_id <- shiny::reactiveVal(NULL)
    pattern_filter <- shiny::reactiveVal(NULL)
    facets <- shiny::reactiveVal(list())

    scans_app_loading_server(
      input,
      output,
      session,
      state,
      sources,
      cache,
      cache_max_age,
      clock,
      schedule
    )
    review_state <- if (reviews) {
      scans_app_review_server(
        input,
        output,
        session,
        state$data,
        state$active,
        state$application,
        state$scan_config,
        selected_trajectory_id,
        rescan_investigation,
        opened_investigation
      )
    }
    scans_app_investigation_server(
      input,
      output,
      session,
      state,
      investigations
    )
    scans_app_annotation_server(
      input,
      output,
      session,
      state,
      annotations,
      annotation_poll_interval
    )
    scans_app_filter_server(input, output, session, state)
    scans_app_browser_server(input, output, session, state, chat_factory)
    scans_app_navigation_server(
      input,
      output,
      session,
      state,
      sources,
      chat_factory
    )
    scans_app_ask_server(input, output, session, state, chat_factory)
  }
}
