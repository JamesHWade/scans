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
