#' Review Posit Connect traces with the scans app
#'
#' `scans_app_connect()` creates a [scans_app()] whose application switcher
#' reads completed conversations from Posit Connect's content observability
#' store. Each deployed application is loaded lazily through
#' `commons::trajectory_read()` and converted with [as_trajectory_commons()].
#' Switching back to an application uses the session cache; **Reload traces**
#' fetches that application's current trace snapshot.
#'
#' Connect trace access requires `CONNECT_SERVER`, `CONNECT_API_KEY`, and
#' editor access to every selected content item. Content observability must be
#' enabled on the server and on each application. Because the review app reads
#' with its own Connect API key, deploy it only to viewers who may inspect all
#' configured applications and their potentially sensitive conversations.
#'
#' @param x A named character vector or list of Posit Connect content GUIDs.
#'   Names appear in the application switcher. Sources must belong to the
#'   `CONNECT_SERVER` that supplies the review app's API key.
#' @param n The maximum number of recent conversations to read per application.
#'   The default is 100. Use `NULL` to read all available conversations.
#' @param from,to Optional lower-inclusive and upper-exclusive activity bounds
#'   passed to `commons::trajectory_read()`. When both are omitted, each load
#'   reads the seven days ending at load time. Supply either bound as `NULL`
#'   explicitly to leave that side of the window open.
#'
#' @returns A [shiny::shinyApp()] object.
#'
#' @examples
#' if (interactive() && rlang::is_installed("commons")) {
#'   scans_app_connect(c(
#'     "Support assistant" = "11111111-1111-4111-8111-111111111111",
#'     "Research assistant" = "22222222-2222-4222-8222-222222222222"
#'   ))
#' }
#' @export
scans_app_connect <- function(
  x,
  n = 100L,
  from = NULL,
  to = NULL
) {
  default_from <- missing(from)
  default_to <- missing(to)
  rlang::check_number_whole(n, min = 1, allow_null = TRUE)
  scans_app_connect_check_package()
  scans_app(scans_app_connect_loaders(
    x,
    n,
    from,
    to,
    default_from,
    default_to
  ))
}

scans_app_connect_loaders <- function(
  x,
  n,
  from,
  to,
  default_from,
  default_to,
  call = rlang::caller_env()
) {
  if (is.character(x)) {
    x <- as.list(x)
  }
  if (!is.list(x) || length(x) == 0L) {
    scans_abort(
      "{.arg x} must be a named character vector or list of Posit Connect content references.",
      class = "scans_error_connect_source",
      call = call
    )
  }
  labels <- names(x)
  scans_app_check_labels(labels, "scans_error_connect_source", call)
  valid <- vapply(
    x,
    function(source) {
      is.character(source) &&
        length(source) == 1L &&
        !is.na(source) &&
        scans_app_connect_guid(source)
    },
    logical(1)
  )
  if (!all(valid)) {
    label <- labels[[which(!valid)[[1L]]]]
    scans_abort(
      "Application source {.val {label}} must be one Posit Connect content GUID.",
      class = "scans_error_connect_source",
      call = call,
      .envir = environment()
    )
  }

  Map(
    function(label, source) {
      force(label)
      force(source)
      function() {
        read_now <- Sys.time()
        read_from <- if (default_from) {
          read_now - 7 * 24 * 60 * 60
        } else {
          from
        }
        read_to <- if (default_to) read_now else to
        commons::trajectory_read(
          source,
          n = n,
          from = read_from,
          to = read_to
        ) |>
          as_trajectory_commons(metadata = list(application = label))
      }
    },
    labels,
    x
  ) |>
    stats::setNames(labels)
}

scans_app_connect_guid <- function(x) {
  grepl(
    "^[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}$",
    x
  )
}

scans_app_connect_check_package <- function(call = rlang::caller_env()) {
  minimum <- numeric_version("0.0.0.9002")
  available <- requireNamespace("commons", quietly = TRUE)
  if (available && utils::packageVersion("commons") >= minimum) {
    return(invisible(TRUE))
  }
  scans_abort(
    c(
      "{.fn scans_app_connect} requires {.pkg commons} >= {minimum}.",
      "i" = "Install it with {.code pak::pak(\"posit-dev/commons/pkg-r\")}."
    ),
    class = "scans_error_app_dependency",
    call = call
  )
}

#' Explore trajectory diagnostics with the scans app
#'
#' `scans_app()` launches a read-only Shiny app for exploring one or more
#' [TrajectoryBundle] snapshots. A named list creates an application switcher;
#' each entry can be a bundle or a zero-argument loader that returns one. Lazy
#' loaders make it practical to review snapshots from multiple deployed apps
#' without downloading every snapshot when the review app starts.
#'
#' The scans app keeps the canonical bundle as its data boundary. It does not
#' call a model, run tools, modify a bundle, or infer missing source facts.
#' Caller-supplied loaders are invoked only when their application is first
#' selected in a session or explicitly reloaded.
#'
#' The app provides filters and a trajectory browser, a source-neutral
#' transcript and event stream, and the findings, evaluations, and adapter
#' losses associated with the selected trajectory. Built-in findings are
#' computed with [scan_trajectories()] when each application snapshot is first
#' loaded.
#'
#' @section Posit Connect:
#' Use [scans_app_connect()] when Connect content observability is enabled. It
#' reads native OTLP traces for named deployed applications and supplies the
#' lazy loaders used by the application switcher. Custom loaders remain useful
#' for completed snapshots held outside Connect's trace store.
#'
#' @param x A [TrajectoryBundle], or a named list of application sources. Each
#'   source must be a `TrajectoryBundle` or a zero-argument function that
#'   returns one. Source names are shown in the application switcher.
#'
#' @returns A [shiny::shinyApp()] object. Calling `scans_app()` at the console
#'   launches the app; the returned object can also be served from an `app.R`.
#'
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(
#'     trajectory_id = "trajectory-1",
#'     source_type = "manual"
#'   ),
#'   data.frame(),
#'   data.frame()
#' )
#'
#' if (interactive()) {
#'   scans_app(bundle)
#'
#'   scans_app(list(
#'     "Support assistant" = bundle,
#'     "Research assistant" = function() readRDS("research-bundle.rds")
#'   ))
#' }
#' @export
scans_app <- function(x) {
  sources <- scans_app_sources(x)
  scans_app_check_packages()

  shiny::shinyApp(
    ui = scans_app_ui(sources),
    server = scans_app_server(sources)
  )
}

scans_app_sources <- function(x, call = rlang::caller_env()) {
  if (is_trajectory_bundle(x)) {
    return(scans_app_source_catalog(list(
      scans_app_source("Trajectories", x)
    )))
  }
  if (!is.list(x) || length(x) == 0L) {
    scans_abort(
      c(
        "{.arg x} must be a {.cls TrajectoryBundle} or a named list of application sources.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_app_source",
      call = call
    )
  }
  labels <- names(x)
  scans_app_check_labels(labels, "scans_error_app_source", call)

  sources <- Map(
    function(label, value) {
      if (!is_trajectory_bundle(value) && !is.function(value)) {
        scans_abort(
          c(
            "Application source {.val {label}} must be a {.cls TrajectoryBundle} or a function.",
            "x" = "It is {.obj_type_friendly {value}}."
          ),
          class = "scans_error_app_source",
          call = call,
          .envir = environment()
        )
      }
      scans_app_source(label, value)
    },
    labels,
    x
  )
  scans_app_source_catalog(unname(sources))
}

scans_app_check_labels <- function(labels, class, call) {
  if (
    is.null(labels) ||
      anyNA(labels) ||
      !all(nzchar(trimws(labels))) ||
      anyDuplicated(labels) > 0L
  ) {
    scans_abort(
      "{.arg x} must have unique, non-empty application names.",
      class = class,
      call = call
    )
  }
  invisible(labels)
}

scans_app_source <- function(label, value) {
  if (is_trajectory_bundle(value)) {
    return(list(
      label = label,
      data = scans_app_data(value),
      load = NULL
    ))
  }
  list(label = label, data = NULL, load = value)
}

scans_app_source_catalog <- function(sources) {
  list(
    sources = sources,
    labels = vapply(sources, `[[`, character(1), "label"),
    reloadable = any(vapply(
      sources,
      function(x) is.function(x$load),
      logical(1)
    ))
  )
}

scans_app_runtime_sources <- function(x) {
  if (is.list(x) && all(c("records", "info") %in% names(x))) {
    return(scans_app_source_catalog(list(list(
      label = "Trajectories",
      data = x,
      load = NULL
    ))))
  }
  x
}

scans_app_load_source <- function(source) {
  if (!is.null(source$data)) {
    return(source$data)
  }
  bundle <- source$load()
  if (!is_trajectory_bundle(bundle)) {
    label <- source$label
    scans_abort(
      "Application source {.val {label}} must return a {.cls TrajectoryBundle}.",
      class = "scans_error_app_source",
      call = NULL,
      .envir = environment()
    )
  }
  scans_app_data(bundle)
}

scans_app_check_packages <- function(
  call = rlang::caller_env(),
  namespace_available = requireNamespace,
  package_version = utils::packageVersion
) {
  packages <- c("bslib", "htmltools", "shiny")
  minimum <- c(bslib = "0.11.0", htmltools = NA, shiny = "1.11.1")
  available <- vapply(
    packages,
    namespace_available,
    logical(1),
    quietly = TRUE
  )
  outdated <- vapply(
    packages,
    function(package) {
      available[[package]] &&
        !is.na(minimum[[package]]) &&
        package_version(package) < numeric_version(minimum[[package]])
    },
    logical(1)
  )
  version_problems <- character()
  if (any(outdated)) {
    version_problems <- paste0(
      packages[outdated],
      " (>= ",
      minimum[outdated],
      ")"
    )
  }
  problems <- c(packages[!available], version_problems)
  if (length(problems) == 0L) {
    return(invisible(packages))
  }
  requirements <- paste(problems, collapse = ", ")

  scans_abort(
    c(
      "{.fn scans_app} requires unavailable package requirements: {requirements}.",
      "i" = "Install or update these packages to use the scans app."
    ),
    class = "scans_error_app_dependency",
    call = call,
    .envir = environment()
  )
}

scans_app_data <- function(x) {
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  evaluations <- trajectory_evaluations(x)
  losses <- trajectory_losses(x)
  findings <- scan_trajectories(x)
  summaries <- summarize_trajectories(x)
  loss_trajectory_ids <- scans_app_loss_trajectory_ids(losses, turns, events)

  list(
    info = info,
    turns = turns,
    events = events,
    evaluations = evaluations,
    losses = losses,
    loss_trajectory_ids = loss_trajectory_ids,
    findings = findings,
    summaries = summaries,
    records = scans_app_records(
      info,
      turns,
      events,
      findings,
      summaries
    )
  )
}

scans_app_loss_trajectory_ids <- function(losses, turns, events) {
  owners <- losses$trajectory_id
  event_owners <- events$trajectory_id[match(losses$event_id, events$event_id)]
  missing <- is.na(owners) & !is.na(event_owners)
  owners[missing] <- event_owners[missing]

  turn_owners <- turns$trajectory_id[match(losses$turn_id, turns$turn_id)]
  missing <- is.na(owners) & !is.na(turn_owners)
  owners[missing] <- turn_owners[missing]
  owners
}

scans_app_records <- function(info, turns, events, findings, summaries) {
  if (nrow(info) == 0L) {
    return(tibble::tibble(
      index = integer(),
      trajectory_id = character(),
      source_type = character(),
      status = character(),
      title = character(),
      search = character(),
      n_turns = integer(),
      n_events = integer(),
      n_findings = integer(),
      n_errors = integer()
    ))
  }

  ids <- info$trajectory_id
  event_text_groups <- scans_app_event_text_groups(ids, events)
  snippets <- scans_app_trajectory_snippets(event_text_groups, turns, events)
  transcripts <- scans_app_trajectory_text(event_text_groups, events)
  titles <- vapply(
    seq_along(ids),
    function(index) {
      scans_app_first_string(
        snippets[[index]],
        info$agent[[index]],
        info$model[[index]],
        info$source_id[[index]],
        info$run_id[[index]],
        ids[[index]]
      )
    },
    character(1)
  )
  finding_groups <- match(findings$trajectory_id, ids)
  n_findings <- tabulate(finding_groups, nbins = length(ids))
  n_errors <- tabulate(
    finding_groups[findings$severity %in% "error"],
    nbins = length(ids)
  )
  search <- vapply(
    seq_along(ids),
    function(index) {
      scans_app_search_string(
        ids[[index]],
        info$run_id[[index]],
        info$source_type[[index]],
        info$source_id[[index]],
        info$agent[[index]],
        info$model[[index]],
        titles[[index]],
        transcripts[[index]]
      )
    },
    character(1)
  )

  tibble::tibble(
    index = seq_along(ids),
    trajectory_id = ids,
    source_type = info$source_type,
    status = info$status,
    title = titles,
    search = tolower(search),
    n_turns = summaries$n_turns,
    n_events = summaries$n_events,
    n_findings = n_findings,
    n_errors = n_errors
  )
}

scans_app_search_string <- function(...) {
  values <- vapply(
    list(...),
    function(value) {
      if (is.character(value) && length(value) == 1L && !is.na(value)) {
        value
      } else {
        ""
      }
    },
    character(1)
  )
  paste(values, collapse = " ")
}

scans_app_event_text_groups <- function(ids, events) {
  groups <- rep(list(integer()), length(ids))
  if (length(ids) == 0L || nrow(events) == 0L) {
    return(groups)
  }
  rows <- which(
    events$trajectory_id %in%
      ids &
      !is.na(events$text) &
      nzchar(trimws(events$text))
  )
  if (length(rows) == 0L) {
    return(groups)
  }
  group_index <- match(events$trajectory_id[rows], ids)
  order_index <- order(
    group_index,
    events$event_index[rows],
    events$event_id[rows],
    method = "radix"
  )
  rows <- rows[order_index]
  group_index <- group_index[order_index]
  grouped <- split(rows, group_index)
  groups[as.integer(names(grouped))] <- unname(grouped)
  groups
}

scans_app_trajectory_text <- function(event_text_groups, events) {
  vapply(
    event_text_groups,
    function(rows) {
      text <- trimws(events$text[rows])
      paste(gsub("\\s+", " ", text), collapse = " ")
    },
    character(1)
  )
}

scans_app_trajectory_snippets <- function(event_text_groups, turns, events) {
  roles <- turns$role[match(events$turn_id, turns$turn_id)]
  vapply(
    event_text_groups,
    function(rows) {
      if (length(rows) == 0L) {
        return(NA_character_)
      }
      user_rows <- rows[roles[rows] %in% "user"]
      if (length(user_rows) > 0L) {
        rows <- user_rows
      }
      scans_app_truncate(
        gsub("\\s+", " ", trimws(events$text[[rows[[1L]]]])),
        90L
      )
    },
    character(1)
  )
}

scans_app_first_string <- function(...) {
  values <- list(...)
  for (value in values) {
    if (
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    ) {
      return(value)
    }
  }
  "Untitled trajectory"
}

scans_app_filter_records <- function(
  records,
  source = NULL,
  status = NULL,
  query = "",
  findings_only = FALSE
) {
  keep <- rep(TRUE, nrow(records))
  source_all <- scans_app_filter_sentinel(records$source_type, "source-all")
  if (!is.null(source) && !identical(source, source_all)) {
    keep <- keep & records$source_type %in% source
  }
  status_all <- scans_app_filter_sentinel(records$status, "status-all")
  missing_status <- scans_app_filter_sentinel(records$status, "status-unknown")
  if (identical(status, missing_status)) {
    keep <- keep & scans_app_unknown_status(records$status)
  } else if (!is.null(status) && !identical(status, status_all)) {
    keep <- keep & records$status %in% status
  }
  query <- trimws(tolower(query))
  if (nzchar(query)) {
    keep <- keep & grepl(query, records$search, fixed = TRUE)
  }
  if (isTRUE(findings_only)) {
    keep <- keep & records$n_findings > 0L
  }
  records$index[keep]
}

scans_app_filter_sentinel <- function(values, type) {
  value <- paste0(".scans-app-", type)
  while (value %in% values) {
    value <- paste0(value, "-")
  }
  value
}

scans_app_unknown_status <- function(status) {
  is.na(status) | (!is.na(status) & !nzchar(trimws(status)))
}

scans_app_filter_choices <- function(data = NULL) {
  if (is.null(data)) {
    source_values <- character()
    status_values <- character()
  } else {
    source_values <- data$records$source_type
    status_values <- data$records$status
  }
  sources <- sort(unique(source_values))
  sources <- sources[!is.na(sources) & nzchar(sources)]
  statuses <- sort(unique(status_values))
  statuses <- statuses[!scans_app_unknown_status(statuses)]
  source_all <- scans_app_filter_sentinel(source_values, "source-all")
  status_all <- scans_app_filter_sentinel(status_values, "status-all")
  status_choices <- c("All statuses" = status_all)
  if (any(scans_app_unknown_status(status_values))) {
    status_choices <- c(
      status_choices,
      "Unknown" = scans_app_filter_sentinel(status_values, "status-unknown")
    )
  }
  status_choices <- c(status_choices, stats::setNames(statuses, statuses))

  list(
    source = c("All sources" = source_all, sources),
    status = status_choices,
    source_all = source_all,
    status_all = status_all
  )
}

scans_app_application_ui <- function(sources) {
  multiple <- length(sources$labels) > 1L
  if (!multiple && !sources$reloadable) {
    return(NULL)
  }
  selector <- if (multiple) {
    shiny::selectInput(
      "scans_app_application",
      "Application",
      choices = stats::setNames(sources$labels, sources$labels),
      width = "100%"
    )
  } else {
    htmltools::div(
      class = "scans-app-application-label",
      htmltools::tags$span("Application"),
      htmltools::tags$strong(sources$labels[[1L]])
    )
  }

  htmltools::div(
    class = "scans-app-application-control",
    selector,
    if (sources$reloadable) {
      shiny::uiOutput("scans_app_reload_control")
    }
  )
}

scans_app_reload_button <- function() {
  shiny::actionButton(
    "scans_app_reload",
    "Reload traces",
    class = "btn-sm btn-outline-secondary"
  )
}

scans_app_ui <- function(sources) {
  initial_data <- sources$sources[[1L]]$data
  choices <- scans_app_filter_choices(initial_data)

  page <- bslib::page_sidebar(
    title = htmltools::div(
      class = "scans-app-brand",
      htmltools::tags$span("scans"),
      htmltools::tags$small("Trajectory diagnostics")
    ),
    theme = bslib::bs_theme(
      version = 5,
      bg = "#f6f7fb",
      fg = "#1e2430",
      primary = "#5356c9"
    ),
    fillable = TRUE,
    class = "bslib-page-dashboard scans-app",
    sidebar = bslib::sidebar(
      title = "Trajectories",
      width = 370,
      class = "scans-app-browser",
      scans_app_application_ui(sources),
      shiny::textInput(
        "scans_app_query",
        "Search",
        placeholder = "ID, source, agent, or transcript",
        width = "100%"
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::selectInput(
          "scans_app_source",
          "Source",
          choices = choices$source,
          width = "100%"
        ),
        shiny::selectInput(
          "scans_app_status",
          "Status",
          choices = choices$status,
          width = "100%"
        )
      ),
      bslib::input_switch(
        "scans_app_findings_only",
        "Only trajectories with findings"
      ),
      htmltools::div(
        class = "scans-app-browser-count",
        shiny::textOutput("scans_app_visible_count", inline = TRUE)
      ),
      htmltools::div(
        class = "scans-app-browser-entries",
        shiny::uiOutput("scans_app_entries")
      )
    ),
    shiny::uiOutput("scans_app_overview"),
    bslib::card(
      fill = TRUE,
      full_screen = TRUE,
      class = "scans-app-workspace",
      bslib::card_header(
        class = "scans-app-workspace-header",
        shiny::uiOutput("scans_app_header")
      ),
      bslib::layout_sidebar(
        fillable = TRUE,
        border = FALSE,
        border_radius = FALSE,
        padding = 0,
        gap = 0,
        sidebar = bslib::sidebar(
          title = "Evidence",
          position = "right",
          width = 350,
          class = "scans-app-evidence",
          shiny::uiOutput("scans_app_evidence")
        ),
        htmltools::tags$main(
          class = "scans-app-transcript",
          shiny::uiOutput("scans_app_transcript")
        )
      )
    )
  )
  scans_app_attach_dependency(page)
}

scans_app_attach_dependency <- function(page) {
  htmltools::attachDependencies(
    page,
    scans_app_dependency(),
    append = TRUE
  )
}

scans_app_server <- function(sources) {
  sources <- scans_app_runtime_sources(sources)

  function(input, output, session) {
    cache <- new.env(parent = emptyenv())
    revision <- shiny::reactiveVal(0L)
    selected <- shiny::reactiveVal(NULL)

    application <- shiny::reactive({
      label <- scans_app_input_or(
        input$scans_app_application,
        sources$labels[[1L]]
      )
      if (!label %in% sources$labels) {
        return(sources$labels[[1L]])
      }
      label
    })

    active_source <- shiny::reactive({
      sources$sources[[match(application(), sources$labels)]]
    })

    output$scans_app_reload_control <- shiny::renderUI({
      if (!is.function(active_source()$load)) {
        return(NULL)
      }
      scans_app_reload_button()
    })

    active <- shiny::reactive({
      revision()
      label <- application()
      if (exists(label, envir = cache, inherits = FALSE)) {
        return(get(label, envir = cache, inherits = FALSE))
      }
      source <- active_source()
      result <- list(
        data = tryCatch(
          scans_app_load_source(source),
          error = \(...) NULL
        )
      )
      assign(label, result, envir = cache)
      result
    })

    data <- shiny::reactive(active()$data)
    shiny::observeEvent(
      input$scans_app_reload,
      ignoreInit = TRUE,
      {
        if (!is.function(active_source()$load)) {
          return()
        }
        label <- application()
        if (exists(label, envir = cache, inherits = FALSE)) {
          rm(list = label, envir = cache)
        }
        revision(revision() + 1L)
      }
    )

    shiny::observeEvent(
      data(),
      ignoreNULL = TRUE,
      {
        current <- data()
        choices <- scans_app_filter_choices(current)
        shiny::updateSelectInput(
          session,
          "scans_app_source",
          choices = choices$source,
          selected = choices$source_all
        )
        shiny::updateSelectInput(
          session,
          "scans_app_status",
          choices = choices$status,
          selected = choices$status_all
        )
        selected(if (nrow(current$records) == 0L) NULL else 1L)
      }
    )

    visible <- shiny::reactive({
      current <- data()
      if (is.null(current)) {
        return(integer())
      }
      choices <- scans_app_filter_choices(current)
      source <- scans_app_input_or(
        input$scans_app_source,
        choices$source_all
      )
      if (!source %in% unname(choices$source)) {
        source <- choices$source_all
      }
      status <- scans_app_input_or(
        input$scans_app_status,
        choices$status_all
      )
      if (!status %in% unname(choices$status)) {
        status <- choices$status_all
      }
      scans_app_filter_records(
        current$records,
        source = source,
        status = status,
        query = scans_app_input_or(input$scans_app_query, ""),
        findings_only = isTRUE(input$scans_app_findings_only)
      )
    })

    shiny::observeEvent(
      visible(),
      ignoreNULL = FALSE,
      {
        indices <- visible()
        if (length(indices) == 0L) {
          selected(NULL)
        } else if (is.null(selected()) || !selected() %in% indices) {
          selected(indices[[1L]])
        }
      }
    )

    entry_observers <- new.env(parent = emptyenv())
    shiny::observe({
      current <- data()
      if (is.null(current) || nrow(current$records) == 0L) {
        return()
      }
      for (index in seq_len(nrow(current$records))) {
        id <- scans_app_entry_id(index)
        if (exists(id, envir = entry_observers, inherits = FALSE)) {
          next
        }
        local({
          entry <- index
          observer <- shiny::observeEvent(input[[scans_app_entry_id(entry)]], {
            selected(entry)
          })
          assign(id, observer, envir = entry_observers)
        })
      }
    })

    output$scans_app_visible_count <- shiny::renderText({
      current <- data()
      if (is.null(current)) {
        return("Traces unavailable")
      }
      count <- length(visible())
      total <- nrow(current$records)
      sprintf(
        "%d of %d %s",
        count,
        total,
        if (total == 1L) "trajectory" else "trajectories"
      )
    })

    output$scans_app_entries <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_ui(application()))
      }
      indices <- visible()
      if (length(indices) == 0L) {
        return(scans_app_empty_ui(
          if (nrow(current$records) == 0L) {
            "This bundle has no trajectories."
          } else {
            "No trajectories match these filters."
          }
        ))
      }
      htmltools::tagList(lapply(indices, function(index) {
        scans_app_entry_ui(
          current$records[index, , drop = FALSE],
          selected = identical(selected(), index)
        )
      }))
    })

    output$scans_app_overview <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(NULL)
      }
      scans_app_overview_ui(current, selected())
    })
    output$scans_app_header <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_heading(application()))
      }
      scans_app_header_ui(current, selected())
    })
    output$scans_app_transcript <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_ui(application()))
      }
      scans_app_transcript_ui(current, selected())
    })
    output$scans_app_evidence <- shiny::renderUI({
      current <- data()
      if (is.null(current)) {
        return(scans_app_source_error_ui(application()))
      }
      scans_app_evidence_ui(current, selected())
    })
  }
}

scans_app_source_error_heading <- function(label) {
  htmltools::div(
    class = "scans-app-heading",
    htmltools::tags$strong(paste("Traces unavailable for", label))
  )
}

scans_app_source_error_ui <- function(label) {
  scans_app_empty_ui(paste(
    "Could not load traces for",
    paste0(label, "."),
    "Reload traces to try again."
  ))
}

scans_app_input_or <- function(value, default) {
  if (is.null(value) || length(value) == 0L) default else value
}

scans_app_entry_id <- function(index) {
  paste0("scans_app_entry_", index)
}

scans_app_entry_ui <- function(record, selected) {
  findings <- record$n_findings[[1L]]
  errors <- record$n_errors[[1L]]
  tone <- if (errors > 0L) {
    "danger"
  } else if (findings > 0L) {
    "warning"
  } else {
    "quiet"
  }

  shiny::actionLink(
    scans_app_entry_id(record$index[[1L]]),
    label = htmltools::tagList(
      htmltools::div(
        class = "scans-app-entry-heading",
        htmltools::tags$span(
          class = "scans-app-entry-title",
          record$title[[1L]]
        ),
        scans_app_badge(as.character(findings), tone)
      ),
      htmltools::div(
        class = "scans-app-entry-id",
        record$trajectory_id[[1L]]
      ),
      htmltools::div(
        class = "scans-app-entry-meta",
        scans_app_badge(record$source_type[[1L]], "source"),
        scans_app_status_badge(record$status[[1L]]),
        htmltools::tags$span(
          sprintf(
            "%d turns \u00b7 %d events",
            record$n_turns[[1L]],
            record$n_events[[1L]]
          )
        )
      )
    ),
    class = paste(
      "scans-app-entry",
      if (selected) "scans-app-entry-selected" else ""
    ),
    `aria-current` = if (selected) "true" else NULL
  )
}

scans_app_overview_ui <- function(data, index) {
  if (is.null(index)) {
    return(NULL)
  }
  id <- data$info$trajectory_id[[index]]
  summary <- data$summaries[index, , drop = FALSE]
  counts <- c(
    Turns = summary$n_turns[[1L]],
    Events = summary$n_events[[1L]],
    Findings = sum(data$findings$trajectory_id == id),
    Evaluations = sum(data$evaluations$trajectory_id == id),
    Losses = sum(data$loss_trajectory_ids %in% id)
  )
  themes <- c("primary", "primary", "warning", "success", "secondary")

  bslib::layout_column_wrap(
    width = "145px",
    fill = FALSE,
    class = "scans-app-overview",
    !!!Map(
      function(title, value, theme) {
        bslib::value_box(
          title = title,
          value = format(value, big.mark = ",", scientific = FALSE),
          theme = theme,
          class = "scans-app-value"
        )
      },
      names(counts),
      counts,
      themes
    )
  )
}

scans_app_header_ui <- function(data, index) {
  if (is.null(index)) {
    return(htmltools::div(
      class = "scans-app-heading",
      htmltools::tags$strong("No trajectory selected")
    ))
  }
  info <- data$info[index, , drop = FALSE]
  record <- data$records[index, , drop = FALSE]
  htmltools::div(
    class = "scans-app-heading",
    htmltools::div(
      htmltools::tags$strong(record$title[[1L]]),
      htmltools::tags$span(
        class = "scans-app-heading-id",
        info$trajectory_id[[1L]]
      )
    ),
    htmltools::div(
      class = "scans-app-heading-badges",
      scans_app_badge(info$source_type[[1L]], "source"),
      scans_app_status_badge(info$status[[1L]])
    )
  )
}

scans_app_transcript_ui <- function(data, index) {
  if (is.null(index)) {
    return(scans_app_empty_ui("Select a trajectory to inspect its path."))
  }
  id <- data$info$trajectory_id[[index]]
  turn_rows <- which(data$turns$trajectory_id == id)
  turn_rows <- turn_rows[
    order(
      data$turns$turn_index[turn_rows],
      data$turns$turn_id[turn_rows],
      method = "radix"
    )
  ]
  event_rows <- which(data$events$trajectory_id == id)
  event_rows <- event_rows[
    order(
      data$events$event_index[event_rows],
      data$events$event_id[event_rows],
      method = "radix"
    )
  ]

  if (length(turn_rows) == 0L && length(event_rows) == 0L) {
    return(scans_app_empty_ui("This trajectory has no turns or events."))
  }

  blocks <- list()
  known_turn_ids <- data$turns$turn_id[turn_rows]
  event_turn_ids <- data$events$turn_id[event_rows]
  known_event_turns <- !is.na(event_turn_ids) &
    event_turn_ids %in% known_turn_ids
  if (length(event_rows) > 0L) {
    owners <- ifelse(
      known_event_turns,
      paste0("turn:", event_turn_ids),
      "run"
    )
    segments <- cumsum(c(TRUE, owners[-1L] != owners[-length(owners)]))
    event_groups <- split(event_rows, segments)
    blocks <- lapply(event_groups, function(rows) {
      turn_id <- data$events$turn_id[[rows[[1L]]]]
      if (!is.na(turn_id) && turn_id %in% known_turn_ids) {
        turn_row <- turn_rows[
          match(turn_id, data$turns$turn_id[turn_rows])
        ]
        return(list(
          event_index = data$events$event_index[[rows[[1L]]]],
          turn_index = data$turns$turn_index[[turn_row]],
          ui = scans_app_turn_ui(
            data$turns[turn_row, , drop = FALSE],
            data$events,
            rows
          )
        ))
      }
      list(
        event_index = data$events$event_index[[rows[[1L]]]],
        turn_index = NA_integer_,
        ui = scans_app_event_group_ui(
          if (length(turn_rows) == 0L) "Event stream" else "Run events",
          data$events,
          rows
        )
      )
    })
  }
  turns_with_events <- unique(event_turn_ids[known_event_turns])
  eventless_turn_rows <- turn_rows[
    !data$turns$turn_id[turn_rows] %in% turns_with_events
  ]
  for (turn_row in eventless_turn_rows) {
    blocks <- append(
      blocks,
      list(list(
        event_index = Inf,
        turn_index = data$turns$turn_index[[turn_row]],
        ui = scans_app_turn_ui(
          data$turns[turn_row, , drop = FALSE],
          data$events,
          integer()
        )
      ))
    )
  }

  event_indices <- vapply(blocks, `[[`, numeric(1), "event_index")
  event_blocks <- blocks[is.finite(event_indices)]
  block_order <- order(
    vapply(event_blocks, `[[`, numeric(1), "event_index"),
    method = "radix"
  )
  ordered_blocks <- event_blocks[block_order]
  eventless_blocks <- blocks[!is.finite(event_indices)]
  eventless_blocks <- eventless_blocks[
    order(
      vapply(eventless_blocks, `[[`, integer(1), "turn_index"),
      method = "radix"
    )
  ]
  for (block in eventless_blocks) {
    ordered_blocks <- scans_app_insert_eventless_turn(ordered_blocks, block)
  }
  blocks <- lapply(ordered_blocks, `[[`, "ui")

  htmltools::div(class = "scans-app-path", htmltools::tagList(blocks))
}

scans_app_insert_eventless_turn <- function(blocks, block) {
  if (length(blocks) == 0L) {
    return(list(block))
  }
  turn_indices <- vapply(blocks, `[[`, integer(1), "turn_index")
  later <- which(!is.na(turn_indices) & turn_indices > block$turn_index)
  if (length(later) > 0L) {
    return(append(blocks, list(block), after = min(later) - 1L))
  }
  earlier <- which(!is.na(turn_indices) & turn_indices < block$turn_index)
  if (length(earlier) > 0L) {
    return(append(blocks, list(block), after = max(earlier)))
  }
  append(blocks, list(block), after = 0L)
}

scans_app_event_group_ui <- function(title, events, rows) {
  htmltools::tags$section(
    class = "scans-app-event-group",
    htmltools::tags$h2(title),
    htmltools::tagList(lapply(rows, function(row) {
      scans_app_event_ui(events[row, , drop = FALSE], row)
    }))
  )
}

scans_app_turn_ui <- function(turn, events, rows) {
  role <- scans_app_first_string(turn$role[[1L]], "unknown")
  title <- paste0(
    scans_app_title_case(role),
    " \u00b7 Turn ",
    turn$turn_index[[1L]]
  )
  if (!is.na(turn$round_index[[1L]])) {
    title <- paste0(title, " \u00b7 Round ", turn$round_index[[1L]])
  }
  body <- if (length(rows) == 0L) {
    scans_app_empty_ui("No events were recorded for this turn.", compact = TRUE)
  } else {
    htmltools::tagList(lapply(rows, function(row) {
      scans_app_event_ui(events[row, , drop = FALSE], row)
    }))
  }

  htmltools::tags$section(
    class = paste0("scans-app-turn scans-app-turn-", scans_app_css_token(role)),
    htmltools::div(
      class = "scans-app-turn-header",
      htmltools::tags$h2(title),
      scans_app_status_badge(turn$status[[1L]])
    ),
    if (scans_app_has_string(turn$error[[1L]])) {
      htmltools::div(class = "scans-app-turn-error", turn$error[[1L]])
    },
    body
  )
}

scans_app_event_ui <- function(event, row) {
  type <- scans_app_first_string(event$event_type[[1L]], "event")
  name <- event$name[[1L]]
  heading <- switch(
    type,
    content = scans_app_title_case(
      scans_app_first_string(event$content_type[[1L]], "content")
    ),
    tool_call = "Tool call",
    tool_result = "Tool result",
    error = "Error",
    scans_app_title_case(gsub("[:_]", " ", type))
  )
  if (scans_app_has_string(name)) {
    heading <- paste(heading, name, sep = " \u00b7 ")
  }
  meta <- scans_app_event_meta(event)
  value <- scans_app_value_text(event$value[[1L]])

  htmltools::tags$article(
    id = scans_app_event_dom_id(row),
    tabindex = "-1",
    class = paste(
      "scans-app-event",
      paste0("scans-app-event-", scans_app_css_token(type)),
      if (scans_app_has_string(event$error[[1L]])) {
        "scans-app-event-failed"
      } else {
        ""
      }
    ),
    htmltools::div(
      class = "scans-app-event-header",
      htmltools::tags$strong(heading),
      scans_app_status_badge(event$status[[1L]])
    ),
    if (length(meta) > 0L) {
      htmltools::div(class = "scans-app-event-meta", meta)
    },
    if (scans_app_has_string(event$text[[1L]])) {
      htmltools::div(class = "scans-app-event-text", event$text[[1L]])
    },
    if (!is.null(value)) {
      htmltools::tags$pre(
        class = "scans-app-event-value",
        htmltools::tags$code(value)
      )
    },
    if (scans_app_has_string(event$error[[1L]])) {
      htmltools::div(
        class = "scans-app-event-error",
        event$error[[1L]]
      )
    }
  )
}

scans_app_event_meta <- function(event) {
  values <- character()
  if (scans_app_has_string(event$call_id[[1L]])) {
    values <- c(values, paste("Call", event$call_id[[1L]]))
  }
  if (!is.na(event$timestamp[[1L]])) {
    values <- c(
      values,
      format(event$timestamp[[1L]], "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
    )
  }
  if (!is.na(event$duration[[1L]])) {
    values <- c(values, sprintf("%.3f s", event$duration[[1L]]))
  }
  if (length(values) == 0L) {
    return(NULL)
  }
  htmltools::tagList(Map(
    function(value, index) {
      htmltools::tagList(
        if (index > 1L) htmltools::tags$span("\u00b7"),
        htmltools::tags$span(value)
      )
    },
    values,
    seq_along(values)
  ))
}

scans_app_evidence_ui <- function(data, index) {
  if (is.null(index)) {
    return(scans_app_empty_ui("Select a trajectory to inspect its evidence."))
  }
  id <- data$info$trajectory_id[[index]]
  finding_rows <- which(data$findings$trajectory_id == id)
  evaluation_rows <- which(data$evaluations$trajectory_id == id)
  loss_rows <- which(data$loss_trajectory_ids %in% id)
  open <- c(
    if (length(finding_rows) > 0L) "findings",
    if (length(loss_rows) > 0L) "losses"
  )
  if (length(open) == 0L) {
    open <- "context"
  }

  bslib::accordion(
    open = open,
    multiple = TRUE,
    class = "scans-app-evidence-accordion",
    bslib::accordion_panel(
      paste0("Findings (", length(finding_rows), ")"),
      value = "findings",
      scans_app_findings_ui(data, finding_rows)
    ),
    bslib::accordion_panel(
      paste0("Evaluations (", length(evaluation_rows), ")"),
      value = "evaluations",
      scans_app_evaluations_ui(data$evaluations, evaluation_rows)
    ),
    bslib::accordion_panel(
      paste0("Losses (", length(loss_rows), ")"),
      value = "losses",
      scans_app_losses_ui(data, loss_rows)
    ),
    bslib::accordion_panel(
      "Context",
      value = "context",
      scans_app_context_ui(data$info[index, , drop = FALSE])
    )
  )
}

scans_app_findings_ui <- function(data, rows) {
  if (length(rows) == 0L) {
    return(scans_app_empty_ui(
      "Built-in scans found no diagnostic issues.",
      compact = TRUE
    ))
  }
  htmltools::tagList(lapply(rows, function(row) {
    finding <- data$findings[row, , drop = FALSE]
    evidence <- finding$event_ids[[1L]]
    if (
      length(evidence) == 0L && scans_app_has_string(finding$event_id[[1L]])
    ) {
      evidence <- finding$event_id[[1L]]
    }
    links <- scans_app_event_links(evidence, data$events)
    htmltools::tags$article(
      class = paste0(
        "scans-app-finding scans-app-finding-",
        scans_app_css_token(finding$severity[[1L]])
      ),
      htmltools::div(
        class = "scans-app-finding-header",
        scans_app_badge(
          scans_app_title_case(finding$severity[[1L]]),
          finding$severity[[1L]]
        ),
        htmltools::tags$strong(
          scans_app_first_string(finding$label[[1L]], finding$scan[[1L]])
        )
      ),
      if (scans_app_has_string(finding$explanation[[1L]])) {
        htmltools::tags$p(finding$explanation[[1L]])
      },
      if (length(links) > 0L) {
        htmltools::div(
          class = "scans-app-evidence-links",
          htmltools::tags$span("Evidence"),
          links
        )
      }
    )
  }))
}

scans_app_event_links <- function(ids, events) {
  ids <- ids[!is.na(ids) & nzchar(ids)]
  rows <- match(ids, events$event_id)
  keep <- !is.na(rows)
  if (!any(keep)) {
    return(NULL)
  }
  htmltools::tagList(Map(
    function(id, row) {
      htmltools::tags$a(
        href = paste0("#", scans_app_event_dom_id(row)),
        scans_app_truncate(id, 28L)
      )
    },
    ids[keep],
    rows[keep]
  ))
}

scans_app_evaluations_ui <- function(evaluations, rows) {
  if (length(rows) == 0L) {
    return(scans_app_empty_ui("No evaluations are joined.", compact = TRUE))
  }
  htmltools::tagList(lapply(rows, function(row) {
    evaluation <- evaluations[row, , drop = FALSE]
    htmltools::tags$article(
      class = "scans-app-evaluation",
      htmltools::div(
        class = "scans-app-evidence-item-header",
        htmltools::tags$strong(scans_app_first_string(
          evaluation$scorer[[1L]],
          evaluation$evaluation_id[[1L]]
        ))
      ),
      scans_app_labeled_value("Value", evaluation$value[[1L]]),
      scans_app_labeled_value("Target", evaluation$target[[1L]]),
      if (scans_app_has_string(evaluation$explanation[[1L]])) {
        htmltools::tags$p(evaluation$explanation[[1L]])
      }
    )
  }))
}

scans_app_losses_ui <- function(data, rows) {
  if (length(rows) == 0L) {
    return(scans_app_empty_ui(
      "No adapter losses are recorded.",
      compact = TRUE
    ))
  }
  htmltools::tagList(lapply(rows, function(row) {
    loss <- data$losses[row, , drop = FALSE]
    links <- scans_app_event_links(loss$event_id[[1L]], data$events)
    htmltools::tags$article(
      class = "scans-app-loss",
      htmltools::div(
        class = "scans-app-evidence-item-header",
        scans_app_badge(scans_app_title_case(loss$reason[[1L]]), "quiet"),
        htmltools::tags$strong(loss$field[[1L]])
      ),
      htmltools::tags$p(loss$detail[[1L]]),
      if (length(links) > 0L) {
        htmltools::div(class = "scans-app-evidence-links", links)
      }
    )
  }))
}

scans_app_context_ui <- function(info) {
  fields <- c(
    "Trajectory" = info$trajectory_id[[1L]],
    "Run" = info$run_id[[1L]],
    "Parent" = info$parent_trajectory_id[[1L]],
    "Source" = info$source_type[[1L]],
    "Source ID" = info$source_id[[1L]],
    "Agent" = info$agent[[1L]],
    "Model" = info$model[[1L]],
    "Started" = scans_app_time_string(info$started_at[[1L]]),
    "Completed" = scans_app_time_string(info$completed_at[[1L]]),
    "Error" = info$error[[1L]]
  )
  fields <- fields[!is.na(fields) & nzchar(fields)]
  if (length(fields) == 0L) {
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
    ))
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
