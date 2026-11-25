# Configuration for the scans review app on Posit Connect, read from Connect
# Vars. Each reader validates one Var and fails with a message that names it,
# so a mistyped value stops the deployment instead of changing what reviewers
# see. app.R sources this file; the tests source it without starting the app.

config_abort <- function(var, problem) {
  # `var` and `problem` are substituted as data, so braces in a Var's value
  # are shown as written and never read as cli markup.
  cli::cli_abort("{var} {problem}", call = NULL)
}

config_get <- function(var, env = Sys.getenv()) {
  # `[[` on a missing name is NULL for a list but an error for the named
  # character vector Sys.getenv() returns.
  value <- if (var %in% names(env)) env[[var]]
  if (is.null(value) || is.na(value)) "" else trimws(value)
}

# "Label=<guid>;Other label=<guid>"
config_sources <- function(spec) {
  entries <- trimws(strsplit(spec, ";", fixed = TRUE)[[1]])
  entries <- entries[nzchar(entries)]
  if (length(entries) == 0) {
    config_abort(
      "SCANS_CONNECT_SOURCES",
      "must be set to Label=<guid>;Label=<guid>."
    )
  }
  parts <- regmatches(
    entries,
    regexec("^(.*?)\\s*=\\s*([0-9a-fA-F-]{36})$", entries)
  )
  bad <- lengths(parts) != 3L
  if (any(bad)) {
    config_abort(
      "SCANS_CONNECT_SOURCES",
      sprintf("has a malformed entry: \"%s\".", entries[bad][[1]])
    )
  }
  stats::setNames(
    vapply(parts, `[[`, character(1), 3L),
    vapply(parts, `[[`, character(1), 2L)
  )
}

config_whole <- function(value, var, default) {
  if (!nzchar(value)) {
    return(default)
  }
  if (!grepl("^[0-9]+$", value) || as.numeric(value) < 1) {
    config_abort(
      var,
      sprintf("must be a whole number of at least 1, not \"%s\".", value)
    )
  }
  as.integer(value)
}

# Maximum recent conversations per application; "all" removes the limit.
config_n <- function(value) {
  if (identical(tolower(value), "all")) {
    return(NULL)
  }
  config_whole(value, "SCANS_CONNECT_N", 100L)
}

config_flag <- function(value, var, default) {
  if (!nzchar(value)) {
    return(default)
  }
  if (!tolower(value) %in% c("true", "false")) {
    config_abort(var, sprintf("must be true or false, not \"%s\".", value))
  }
  identical(tolower(value), "true")
}

config_reader <- function(value) {
  if (!nzchar(value)) {
    return("otel")
  }
  if (!value %in% c("otel", "commons")) {
    config_abort(
      "SCANS_CONNECT_READER",
      sprintf("must be otel or commons, not \"%s\".", value)
    )
  }
  value
}

config_labels <- function(value) {
  if (!nzchar(value)) {
    return(NULL)
  }
  labels <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  if (!all(nzchar(labels)) || anyDuplicated(labels)) {
    config_abort(
      "SCANS_ANNOTATION_LABELS",
      "must be distinct, non-empty labels separated by commas."
    )
  }
  labels
}

# Every Var at once, so app.R states its whole configuration in one place.
scans_connect_config <- function(env = Sys.getenv()) {
  get <- function(var) config_get(var, env)
  labels <- config_labels(get("SCANS_ANNOTATION_LABELS"))
  directory <- get("SCANS_ANNOTATIONS_DIR")
  if (!is.null(labels) && !nzchar(directory)) {
    config_abort(
      "SCANS_ANNOTATION_LABELS",
      "needs SCANS_ANNOTATIONS_DIR; labels have no effect without a store."
    )
  }
  list(
    sources = config_sources(get("SCANS_CONNECT_SOURCES")),
    n = config_n(get("SCANS_CONNECT_N")),
    days = config_whole(get("SCANS_CONNECT_DAYS"), "SCANS_CONNECT_DAYS", 7L),
    reader = config_reader(get("SCANS_CONNECT_READER")),
    jobs = config_flag(get("SCANS_CONNECT_JOBS"), "SCANS_CONNECT_JOBS", TRUE),
    reviews = config_flag(get("SCANS_REVIEWS"), "SCANS_REVIEWS", FALSE),
    investigations = config_flag(
      get("SCANS_INVESTIGATIONS"),
      "SCANS_INVESTIGATIONS",
      FALSE
    ),
    annotations_dir = if (nzchar(directory)) directory,
    annotation_labels = labels
  )
}
