#' Store trajectory annotations
#'
#' @description
#' `scans_annotations()` creates the annotation store the app writes to when a
#' reviewer labels or comments on a trajectory. Pass it to [scans_app()] or
#' [scans_app_connect()] through their `annotations` argument; without one the
#' app stays strictly read-only and shows no annotation controls.
#'
#' Annotations are an append-only log: saving never rewrites or deletes an
#' earlier record, and an application's trajectory annotations are every record
#' written against that pair, newest first. The application namespace matters
#' because trajectory identifiers are unique only within one bundle. A log
#' rather than a mutable field means two reviewers working at once cannot
#' overwrite each other, and that a judgement made about a trajectory remains
#' visible after it is revised.
#'
#' @param path Path to the log file. Each line is one JSON record. Defaults to
#'   `annotations.jsonl` under `tools::R_user_dir("scans", "data")`. On Posit
#'   Connect, point this at a directory every viewer's process can write to;
#'   the per-user default is not shared.
#' @param labels The labels a reviewer may apply, as a character vector.
#'
#' @returns A `scans_annotations` object: a list of `read()`, `append()`, and
#'   `labels` for the app to use. `append()` requires both an application
#'   namespace and a trajectory identifier; `read()` can filter by either.
#'   Labels outside the store's configured vocabulary are rejected; a missing
#'   label remains valid for note-only annotations.
#'
#' @export
#'
#' @examples
#' store <- scans_annotations(path = tempfile(fileext = ".jsonl"))
#' store$append(
#'   "Support assistant",
#'   "trajectory-1",
#'   label = "follow up",
#'   note = "Tool loop here."
#' )
#' store$read("Support assistant", "trajectory-1")
scans_annotations <- function(
  path = NULL,
  labels = c("looks right", "wrong answer", "tool failure", "follow up")
) {
  call <- rlang::caller_env()
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    scans_abort(
      c(
        "Annotations require the {.pkg jsonlite} package.",
        i = "Install {.pkg jsonlite} to enable them."
      ),
      class = "scans_error_annotation_dependency",
      call = call
    )
  }
  if (!is.character(labels) || length(labels) == 0L || anyNA(labels)) {
    scans_abort(
      "{.arg labels} must be a non-empty character vector.",
      class = "scans_error_annotation_labels",
      call = call
    )
  }
  path <- path %||%
    file.path(tools::R_user_dir("scans", "data"), "annotations.jsonl")

  store <- list(
    path = path,
    labels = labels,
    read = function(application = NULL, trajectory_id = NULL) {
      annotations_read(path, application, trajectory_id)
    },
    append = function(application, trajectory_id, label, note, author = NULL) {
      annotations_append(
        path,
        application,
        trajectory_id,
        label,
        note,
        labels,
        author
      )
    }
  )
  structure(store, class = "scans_annotations")
}

#' @export
print.scans_annotations <- function(x, ...) {
  records <- x$read()
  cli::cli_text("{.cls scans_annotations} {.file {x$path}}")
  cli::cli_text(
    "{nrow(records)} annotation{?s} \u00b7 labels: {.val {x$labels}}"
  )
  invisible(x)
}

annotations_columns <- function() {
  list(
    application = character(),
    trajectory_id = character(),
    label = character(),
    note = character(),
    author = character(),
    created_at = as.POSIXct(character(), tz = "UTC")
  )
}

annotations_empty <- function() {
  tibble::as_tibble(annotations_columns())
}

# A record that cannot be parsed is skipped rather than failing the read: one
# malformed line, however it got there, must not hide every annotation.
annotations_read <- function(path, application = NULL, trajectory_id = NULL) {
  if (!file.exists(path)) {
    return(annotations_empty())
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0L) {
    return(annotations_empty())
  }
  records <- lapply(lines, function(line) {
    parsed <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.list(parsed) || !scans_app_has_string(parsed$trajectory_id)) {
      return(NULL)
    }
    tibble::tibble(
      application = if (scans_app_has_string(parsed$application)) {
        as.character(parsed$application)
      } else {
        NA_character_
      },
      trajectory_id = as.character(parsed$trajectory_id),
      label = as.character(parsed$label %||% NA_character_),
      note = as.character(parsed$note %||% NA_character_),
      author = as.character(parsed$author %||% NA_character_),
      created_at = annotations_parse_time(parsed$created_at)
    )
  })
  out <- trajectory_bind_rows(Filter(Negate(is.null), records))
  if (nrow(out) == 0L) {
    return(annotations_empty())
  }
  out <- out[order(out$created_at, decreasing = TRUE), , drop = FALSE]
  if (!is.null(application)) {
    out <- out[out$application %in% application, , drop = FALSE]
  }
  if (is.null(trajectory_id)) {
    return(out)
  }
  out[out$trajectory_id %in% trajectory_id, , drop = FALSE]
}

annotations_parse_time <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(as.character(x))) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  parsed <- as.POSIXct(
    as.character(x),
    tz = "UTC",
    tryFormats = c(
      "%Y-%m-%dT%H:%M:%OSZ",
      "%Y-%m-%dT%H:%M:%OS",
      "%Y-%m-%d %H:%M:%OS"
    )
  )
  parsed
}

# One line, appended. A whole-file rewrite would lose a concurrent reviewer's
# record; an append of a single short line does not, which is why notes are
# capped rather than allowed to grow unbounded.
annotations_append <- function(
  path,
  application,
  trajectory_id,
  label,
  note,
  labels,
  author = NULL,
  max_note = 4000L
) {
  call <- rlang::caller_env()
  if (!scans_app_has_string(application)) {
    scans_abort(
      "{.arg application} must be one non-empty string.",
      class = "scans_error_annotation_record",
      call = call
    )
  }
  if (!scans_app_has_string(trajectory_id)) {
    scans_abort(
      "{.arg trajectory_id} must be one non-empty string.",
      class = "scans_error_annotation_record",
      call = call
    )
  }
  label <- if (scans_app_has_string(label)) label else NA_character_
  if (!is.na(label) && !label %in% labels) {
    scans_abort(
      c(
        "Label {.val {label}} is not configured for this annotation store.",
        i = "Choose one of {.val {labels}} or leave the label blank."
      ),
      class = "scans_error_annotation_record",
      call = call
    )
  }
  note <- if (scans_app_has_string(note)) {
    substr(note, 1L, max_note)
  } else {
    NA_character_
  }
  if (is.na(label) && is.na(note)) {
    scans_abort(
      "An annotation needs a label, a note, or both.",
      class = "scans_error_annotation_record",
      call = call
    )
  }
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }
  record <- list(
    application = application,
    trajectory_id = trajectory_id,
    label = label,
    note = note,
    author = author %||% annotations_default_author(),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null")
  # Newline first would leave a leading blank line in a fresh file; the read
  # skips blank lines either way, but the file should be clean.
  connection <- file(path, open = "a")
  on.exit(close(connection), add = TRUE)
  writeLines(as.character(line), connection)
  invisible(record)
}

# Connect runs content as a known user; locally the OS user is the best
# available answer. Neither is authenticated by this package, so the author is
# a convenience for the team reading the log, not an identity claim.
annotations_default_author <- function(session = NULL) {
  user <- NULL
  if (!is.null(session)) {
    user <- session$user
  }
  if (is.null(user) || !nzchar(user)) {
    user <- Sys.getenv("CONNECT_USER", "")
  }
  if (!nzchar(user)) {
    user <- tryCatch(Sys.info()[["user"]], error = function(e) "")
  }
  if (is.null(user) || is.na(user) || !nzchar(user)) "unknown" else user
}

is_scans_annotations <- function(x) {
  inherits(x, "scans_annotations")
}
