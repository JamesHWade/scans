# scans review app for Posit Connect content observability traces.
#
# Serves the scans review app over the completed conversations of one or more
# deployed applications, read lazily from Connect's trace store. Configure it
# entirely through Connect Vars; config.R validates each one.
#
#   CONNECT_SERVER           injected by Connect; configure only if disabled
#   CONNECT_API_KEY          injected ephemeral deployment-owner key; the owner
#                            must own or collaborate on every source
#   SCANS_CONNECT_SOURCES    "Label=<guid>;Other label=<guid>" -- one entry per
#                            application shown in the switcher
#   SCANS_CONNECT_N          optional; max recent conversations per app
#                            (default 100, "all" for no limit)
#   SCANS_CONNECT_DAYS       optional; days of activity read per load
#                            (default 7)
#   SCANS_CONNECT_READER     optional; "otel" (default) or "commons"
#   SCANS_CONNECT_JOBS       optional; "true" (default) or "false" to skip the
#                            retained per-job trace stores
#   SCANS_ANNOTATIONS_DIR    optional; persistent directory for reviewer
#                            labels and notes (read-only app when unset)
#   SCANS_ANNOTATION_LABELS  optional; comma-separated labels for the store
#   SCANS_REVIEWS            optional; "true" for session-local evidence review
#   SCANS_INVESTIGATIONS     optional; "true" to save and reopen investigations

# These packages are Suggests of scans; attaching them here makes rsconnect /
# Posit Publisher bundle the app and native trace-reader dependencies.
library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(scans)

source("config.R", local = TRUE)
config <- scans_connect_config()

annotations <- if (!is.null(config$annotations_dir)) {
  path <- file.path(config$annotations_dir, "annotations.jsonl")
  if (is.null(config$annotation_labels)) {
    scans_annotations(path)
  } else {
    scans_annotations(path, labels = config$annotation_labels)
  }
}

scans_app_connect(
  config$sources,
  n = config$n,
  days = config$days,
  reader = config$reader,
  jobs = config$jobs,
  annotations = annotations,
  reviews = config$reviews,
  investigations = config$investigations
)
