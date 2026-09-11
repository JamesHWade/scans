#' Explore trajectory diagnostics with the scans app
#'
#' `scans_app()` launches a Shiny app for exploring one or more
#' [TrajectoryBundle] snapshots or saved [investigation_snapshot()] values.
#' A named list creates an application switcher; each entry can be a bundle,
#' saved investigation, or a zero-argument loader that returns either. Lazy
#' loaders make it practical to review snapshots from multiple deployed apps
#' without downloading every snapshot when the review app starts.
#'
#' The scans app keeps the canonical bundle as its data boundary. Ordinary
#' inspection does not call a model, replay tools, or modify a bundle.
#' Caller-supplied loaders are invoked only when their application is first
#' selected in a session or explicitly reloaded.
#'
#' The app opens with an application overview of the filtered trajectories:
#' recurring findings, elapsed-time quantiles, recorded token usage, and
#' evidence coverage. Selecting a pattern filters the browser; selecting a
#' trajectory opens its evidence. Elapsed time includes any pauses between
#' user messages and is not model response latency. Unknown measurements
#' remain unknown. Token counts use preserved conversation totals when
#' available, otherwise sum known turn values, and may be partial.
#'
#' The app also provides filters and a trajectory browser, a source-neutral
#' transcript and event stream, and the findings, evaluations, and adapter
#' losses associated with the selected trajectory. Built-in findings are
#' computed with [scan_trajectories()] when each application snapshot is first
#' loaded.
#'
#' @section Ask about the evidence:
#' Supply `chat_factory` to enable an optional Ask tab beside Findings. It must
#' return an ellmer chat configured with your chosen provider. Each session
#' clones the client, clears its prior turns and tools, and registers only
#' [scans_tools()]. Provider requests begin when the user submits a question.
#' The permitted trajectories and snapshot are fixed at submission; browsing
#' while a response streams cannot change them. Changing scope starts fresh
#' model context while keeping earlier answers and drafts visible. Links in
#' older answers open their retained snapshot, with a return-to-source action.
#' Chat history and evidence references last only for the current session.
#' The provider receives the submitted question and any retained evidence its
#' tools read. Existing redactions remain intact; no new anonymization occurs.
#' A recent shinychat with `page_chat_theme()` and
#' `chat_server()$set_client()` is required for this surface.
#'
#' @section Saved investigations:
#' Set `investigations = TRUE` to enable save/open controls in a regular app.
#' They are enabled by default when `x` is a single saved investigation.
#' Save visible trajectories downloads the selected retained evidence, analysis,
#' scanner settings, and browser state as JSON. The preview describes the
#' content policy before download. Open an investigation restores a file in the
#' current session. Saved diagnostics remain unchanged until the reviewer
#' chooses Apply current scanners; saving again records a parent revision.
#' Opening does not contact the original source. The default upload limit is
#' 50 MiB. An explicit `shiny.maxRequestSize` option sets both the app upload
#' ceiling and its investigation reader limit; `Inf` disables the size limit.
#' See [investigation_files()].
#'
#' @section Posit Connect:
#' Use [scans_app_connect()] when Connect content observability is enabled. It
#' reads native OTLP traces for named deployed applications and supplies the
#' lazy loaders used by the application switcher. Custom loaders remain useful
#' for completed snapshots held outside Connect's trace store.
#'
#' @param annotations Optional [scans_annotations()] store. When supplied, the
#'   app shows an annotation panel for the selected trajectory and appends what
#'   reviewers write to that store. The store also supplies current annotation
#'   filter membership when opening a saved investigation. Without a store,
#'   saved membership is used and the app writes only explicitly requested
#'   investigation downloads.
#' @param x A [TrajectoryBundle], a saved [investigation_snapshot()], or a named
#'   list of application sources. Each source can be either value or a
#'   zero-argument function that returns one. Source names are shown in the
#'   application switcher.
#'
#' @param reviews Whether to enable session-local evidence review and example
#'   export controls. Defaults to `FALSE`. Download selected examples to keep
#'   them after closing the session; no shared review store is written.
#' @param investigations Whether to enable investigation save, upload, and
#'   rescan controls. Defaults to `TRUE` for a single saved investigation and
#'   `FALSE` for other inputs. Set explicitly for named sources or lazy loaders.
#' @param chat_factory Optional zero-argument function returning an ellmer chat,
#'   for example `function() ellmer::chat_openai()`. Defaults to `NULL`, which
#'   omits Ask. Tools configured on this client are replaced with the bounded
#'   scans tools; its system prompt is retained with evidence instructions.
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
scans_app <- function(
  x,
  annotations = NULL,
  investigations = inherits(x, "scans_investigation"),
  reviews = FALSE,
  chat_factory = NULL
) {
  sources <- scans_app_sources(x)
  scans_app_check_packages()
  scans_app_check_annotations(annotations)
  rlang::check_bool(investigations)
  rlang::check_bool(reviews)
  scans_app_check_chat_factory(chat_factory)

  shiny::shinyApp(
    ui = scans_app_ui(
      sources,
      annotations,
      investigations,
      reviews,
      chat = !is.null(chat_factory)
    ),
    server = scans_app_server(
      sources,
      annotations,
      investigations = investigations,
      reviews = reviews,
      chat_factory = chat_factory
    ),
    onStart = if (investigations || reviews) scans_app_investigation_start
  )
}

scans_app_check_annotations <- function(
  annotations,
  call = rlang::caller_env()
) {
  if (is.null(annotations) || inherits(annotations, "scans_annotations")) {
    return(invisible(annotations))
  }
  scans_abort(
    c(
      "{.arg annotations} must be a {.cls scans_annotations} store.",
      i = "Create one with {.fn scans_annotations}."
    ),
    class = "scans_error_app_annotations",
    call = call
  )
}

scans_app_sources <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "scans_investigation")) {
    investigation_validate(x)
    return(scans_app_source_catalog(list(
      scans_app_source(x$manifest$application, x)
    )))
  }
  if (is_trajectory_bundle(x)) {
    return(scans_app_source_catalog(list(
      scans_app_source("Trajectories", x)
    )))
  }
  if (!is.list(x) || length(x) == 0L) {
    scans_abort(
      c(
        "{.arg x} must be a {.cls TrajectoryBundle}, {.cls scans_investigation}, or a named list of application sources.",
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
      if (
        !is_trajectory_bundle(value) &&
          !inherits(value, "scans_investigation") &&
          !is.function(value)
      ) {
        scans_abort(
          c(
            "Application source {.val {label}} must be a {.cls TrajectoryBundle}, {.cls scans_investigation}, or a function.",
            "x" = "It is {.obj_type_friendly {value}}."
          ),
          class = "scans_error_app_source",
          call = call,
          .envir = environment()
        )
      }
      if (is.function(value)) {
        scans_app_check_loader(value, label, call)
      }
      scans_app_source(label, value)
    },
    labels,
    x
  )
  scans_app_source_catalog(unname(sources))
}

scans_app_check_loader <- function(loader, label, call) {
  loader_formals <- formals(loader)
  if (is.primitive(loader)) {
    loader_signature <- args(loader)
    if (!is.null(loader_signature)) {
      loader_formals <- formals(loader_signature)
    }
  }
  if (is.primitive(loader) && is.null(loader_formals)) {
    scans_abort(
      c(
        "Application source {.val {label}} loader must be callable without arguments.",
        "x" = "Its argument list could not be inspected.",
        "i" = "Wrap it in an ordinary zero-argument function."
      ),
      class = "scans_error_app_source",
      call = call,
      .envir = environment()
    )
  }

  required <- names(loader_formals)[vapply(
    loader_formals,
    rlang::is_missing,
    logical(1)
  )]
  required <- setdiff(required, "...")
  if (length(required) == 0L) {
    return(invisible(NULL))
  }

  detail <- if (length(required) == 1L) {
    paste0("Required argument: ", required, ".")
  } else {
    paste0("Required arguments: ", paste(required, collapse = ", "), ".")
  }
  scans_abort(
    c(
      "Application source {.val {label}} loader must be callable without arguments.",
      "x" = detail
    ),
    class = "scans_error_app_source",
    call = call,
    .envir = environment()
  )
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
  if (inherits(value, "scans_investigation")) {
    investigation_validate(value)
    return(list(
      label = label,
      bundle = value$bundle,
      investigation = value,
      load = NULL
    ))
  }
  if (is_trajectory_bundle(value)) {
    return(list(label = label, bundle = value, load = NULL))
  }
  list(label = label, bundle = NULL, load = value)
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

# A loader can carry source read details beside the typed bundle without
# mutating the S7 object with an ad hoc attribute.
scans_app_loaded_source <- function(bundle, read_info = NULL) {
  structure(
    list(bundle = bundle, read_info = read_info),
    class = "scans_app_loaded_source"
  )
}

# Returns the typed bundle and its source read details: the server caches both
# per application and derives findings from the bundle, so re-running a scan
# costs no network.
scans_app_load_source <- function(source) {
  if (!is.null(source$bundle)) {
    return(list(bundle = source$bundle, read_info = NULL))
  }
  loaded <- source$load()
  if (inherits(loaded, "scans_investigation")) {
    investigation_validate(loaded)
    return(list(
      bundle = loaded$bundle,
      investigation = loaded,
      read_info = loaded$manifest$source$read_info
    ))
  }
  if (inherits(loaded, "scans_app_loaded_source")) {
    bundle <- loaded$bundle
    read_info <- loaded$read_info
  } else {
    bundle <- loaded
    read_info <- NULL
  }
  if (!is_trajectory_bundle(bundle)) {
    label <- source$label
    scans_abort(
      "Application source {.val {label}} must return a {.cls TrajectoryBundle}.",
      class = "scans_error_app_source",
      call = NULL,
      .envir = environment()
    )
  }
  list(bundle = bundle, read_info = read_info)
}

scans_app_check_packages <- function(
  call = rlang::caller_env(),
  namespace_available = requireNamespace,
  package_version = utils::packageVersion
) {
  packages <- c("bslib", "htmltools", "shiny", "shinychat")
  minimum <- c(
    bslib = "0.11.0",
    htmltools = NA,
    shiny = "1.11.1",
    shinychat = "0.4.0.9000"
  )
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
  if (
    available[["shinychat"]] &&
      !"page_chat_theme" %in% getNamespaceExports("shinychat")
  ) {
    problems <- c(problems, "shinychat (with page_chat_theme())")
  }
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
