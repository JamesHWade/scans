# scans review app for Posit Connect content observability traces.
#
# Serves the read-only scans explorer over the completed conversations of one
# or more deployed applications, read lazily from Connect's trace store with
# scans' native OpenTelemetry reader. Configure it entirely through Connect
# Vars:
#
#   CONNECT_SERVER          injected by Connect; configure only if disabled
#   CONNECT_API_KEY         injected ephemeral deployment-owner key; the owner
#                           must own or collaborate on every source
#   SCANS_CONNECT_SOURCES   "Label=<guid>;Other label=<guid>" -- one entry per
#                           application shown in the switcher
#   SCANS_CONNECT_N         optional; max recent conversations per app
#                           (default 100, "all" for no limit)
#   SCANS_ANNOTATIONS_DIR   optional; persistent directory for reviewer
#                           labels and notes (no controls when unset)

# These packages are Suggests of scans; attaching them here makes rsconnect /
# Posit Publisher bundle the app and native trace-reader dependencies.
library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(scans)

parse_sources <- function(spec) {
  entries <- trimws(strsplit(spec, ";", fixed = TRUE)[[1]])
  entries <- entries[nzchar(entries)]
  if (length(entries) == 0) {
    cli::cli_abort(
      "Set {.envvar SCANS_CONNECT_SOURCES} to {.code Label=<guid>;Label=<guid>}."
    )
  }
  parts <- regmatches(
    entries,
    regexec("^(.*?)\\s*=\\s*([0-9a-fA-F-]{36})$", entries)
  )
  bad <- lengths(parts) != 3L
  if (any(bad)) {
    cli::cli_abort(
      "Malformed {.envvar SCANS_CONNECT_SOURCES} entry: {.val {entries[bad][[1]]}}."
    )
  }
  stats::setNames(
    vapply(parts, `[[`, character(1), 3L),
    vapply(parts, `[[`, character(1), 2L)
  )
}

sources <- parse_sources(Sys.getenv("SCANS_CONNECT_SOURCES"))

n <- Sys.getenv("SCANS_CONNECT_N", "100")
n <- if (identical(tolower(n), "all")) NULL else as.integer(n)

annotations_dir <- Sys.getenv("SCANS_ANNOTATIONS_DIR", "")
annotations <- if (nzchar(annotations_dir)) {
  scans_annotations(annotations_dir)
} else {
  NULL
}

scans_app_connect(sources, n = n, annotations = annotations)
