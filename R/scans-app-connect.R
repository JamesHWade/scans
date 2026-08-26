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
