#' Read local OpenTelemetry trace files
#'
#' @description
#' `read_otel_traces()` reads GenAI spans from OTLP NDJSON files written by
#' otelsdk's file exporter and groups them into conversations, ready for
#' [as_trajectory_otel()]. This is where `commons::agent(log = TRUE)` and any
#' ellmer or deputy application traced with `OTEL_TRACES_EXPORTER=otlp/file`
#' write their spans when they are not running on Posit Connect.
#'
#' The exporter rotates through `trace-0.jsonl`, `trace-1.jsonl`, ... and keeps
#' `trace-latest.jsonl` pointing at the newest file as a symbolic link, hard
#' link, or copy. Each span is read once: files that resolve to the same path
#' or have identical contents are read once, and a span that still appears in
#' two files (a copy taken mid-write) is kept once.
#'
#' @param path Trace files to read: one or more OTLP NDJSON files, or a
#'   directory of them. A directory contributes its `trace.jsonl`,
#'   `trace-<n>.jsonl`, and `trace-latest.jsonl` files, plus files named from
#'   the `OTEL_EXPORTER_OTLP_TRACES_FILE` (or `OTEL_EXPORTER_OTLP_FILE`)
#'   pattern when one is set. `NULL` reads the directory of that pattern, and
#'   is an error when neither variable names a file.
#' @param n Maximum number of recent conversations to keep. `NULL` keeps all.
#' @param from,to Optional lower-inclusive and upper-exclusive bounds on the
#'   start time of a conversation's model calls, as for
#'   [read_connect_traces()]: a conversation is kept when one of its model
#'   calls started in the window, with all of its spans before `to`. Accepts
#'   POSIXct, Date, or ISO 8601 strings; a string without a zone is read as
#'   UTC. `NULL` leaves that side open. Unlike [read_connect_traces()], the
#'   default reads every file in full rather than the last seven days.
#' @param max_spans Ceiling on how many GenAI spans are kept, counting back
#'   from the most recent. Only spans carrying `gen_ai.*` attributes count
#'   towards it, as for [read_connect_traces()].
#'
#' @returns A named list of conversations, each a list of spans, oldest-first,
#'   in the same form as [read_connect_traces()]. The `"source_uri"` attribute
#'   is a `file://` URI when a single file or directory was read. The
#'   `"read_info"` attribute records the window, how many spans and
#'   conversations were found, whether the `max_spans` ceiling cut the read
#'   short (`truncated`), how many files were read (`files`), and how many
#'   lines could not be parsed (`lines_dropped`); `incomplete` is `TRUE` when
#'   any line was dropped.
#'
#' @seealso [as_trajectory_otel()] to convert the result, and
#'   [read_connect_traces()] for traces retained by Posit Connect.
#' @export
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' path <- system.file("extdata", "otel-traces", package = "scans")
#' conversations <- read_otel_traces(path)
#' attr(conversations, "read_info")$conversations
#'
#' bundle <- as_trajectory_otel(conversations)
#' trajectory_turns(bundle)[, c("trajectory_id", "role")]
read_otel_traces <- function(
  path = NULL,
  n = 100L,
  from = NULL,
  to = NULL,
  max_spans = 50000L
) {
  call <- rlang::caller_env()
  otel_check_jsonlite(call)
  from <- connect_check_bound(from, "from", call)
  to <- connect_check_bound(to, "to", call)
  rlang::check_number_whole(n, min = 1, allow_null = TRUE)
  rlang::check_number_whole(max_spans, min = 1)
  path <- path %||% otel_default_traces_dir(call)
  files <- otel_trace_files(path, call)

  read_at <- Sys.time()
  spans <- list()
  dropped <- 0L
  for (file in files) {
    parsed <- otel_parse_otlp_lines_counted(readLines(file, warn = FALSE))
    spans <- c(spans, parsed$spans)
    dropped <- dropped + parsed$dropped
  }
  if (length(spans) == 0L) {
    scans_abort(
      c(
        "No OpenTelemetry spans could be parsed from the trace files.",
        i = "Read {.file {files}}."
      ),
      class = "scans_error_otel_traces",
      call = call
    )
  }

  spans <- otel_unique_spans(spans)
  in_budget <- vapply(
    spans,
    function(span) {
      otel_is_genai_span(span) &&
        otel_span_in_window(span, from = NULL, to = to)
    },
    logical(1)
  )
  truncated <- sum(in_budget) > max_spans
  if (truncated) {
    cli::cli_warn(
      "Kept the most recent {max_spans} span{?s}; raise {.arg max_spans} to
       read further back."
    )
  }
  conversations <- otel_read_conversations(
    spans,
    from = from,
    to = to,
    n = n,
    max_spans = max_spans,
    read_at = read_at,
    truncated = truncated,
    incomplete = dropped > 0L,
    newest_first = TRUE
  )
  if (length(path) == 1L) {
    attr(conversations, "source_uri") <- paste0(
      "file://",
      normalizePath(path, winslash = "/")
    )
  }
  info <- attr(conversations, "read_info", exact = TRUE)
  info$files <- length(files)
  info$lines_dropped <- dropped
  attr(conversations, "read_info") <- info
  conversations
}

# otelsdk names its output file from OTEL_EXPORTER_OTLP_TRACES_FILE, then
# OTEL_EXPORTER_OTLP_FILE, defaulting to `trace-%N.jsonl` in the working
# directory. Only an explicit setting is trusted as the location: the default
# is relative to whichever process wrote the traces.
otel_traces_file_template <- function() {
  for (name in c("OTEL_EXPORTER_OTLP_TRACES_FILE", "OTEL_EXPORTER_OTLP_FILE")) {
    value <- Sys.getenv(name)
    if (nzchar(value)) {
      return(if (value %in% c("stdout", "stderr")) NULL else value)
    }
  }
  NULL
}

otel_default_traces_dir <- function(call = rlang::caller_env()) {
  template <- otel_traces_file_template()
  if (is.null(template)) {
    scans_abort(
      c(
        "{.arg path} must name trace files or a directory of them.",
        i = "Set {.envvar OTEL_EXPORTER_OTLP_TRACES_FILE} to read the
             exporter's directory by default."
      ),
      class = "scans_error_otel_path",
      call = call
    )
  }
  dirname(template)
}

# Rotated files, the exporter's default alias, and files named from a custom
# file pattern. A custom alias is not matched; it duplicates a rotated file.
otel_trace_file_pattern <- function(template = otel_traces_file_template()) {
  patterns <- "trace(-[0-9]+|-latest)?[.]jsonl"
  if (!is.null(template)) {
    regex <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", basename(template))
    regex <- gsub("%F", "[0-9]{4}-[0-9]{2}-[0-9]{2}", regex, fixed = TRUE)
    regex <- gsub("%T", "[0-9]{2}:[0-9]{2}:[0-9]{2}", regex, fixed = TRUE)
    regex <- gsub("%R", "[0-9]{2}:[0-9]{2}", regex, fixed = TRUE)
    regex <- gsub("%[YymjdwHIMSNn]", "[0-9]+", regex)
    patterns <- c(patterns, regex)
  }
  paste0("^(", paste(unique(patterns), collapse = "|"), ")$")
}

otel_trace_files <- function(path, call = rlang::caller_env()) {
  if (!is.character(path) || length(path) == 0L || anyNA(path)) {
    scans_abort(
      "{.arg path} must be a character vector of trace files or directories.",
      class = "scans_error_otel_path",
      call = call
    )
  }
  missing <- path[!file.exists(path)]
  if (length(missing) > 0L) {
    scans_abort(
      "Can't find {.path {missing}}.",
      class = "scans_error_otel_path",
      call = call
    )
  }
  pattern <- otel_trace_file_pattern()
  files <- character()
  for (entry in path) {
    if (dir.exists(entry)) {
      found <- list.files(entry, pattern = pattern, full.names = TRUE)
      if (length(found) == 0L) {
        scans_abort(
          "Can't find OTLP trace files ({.file trace-*.jsonl}) in
           {.path {entry}}.",
          class = "scans_error_otel_path",
          call = call
        )
      }
      files <- c(files, found)
    } else {
      files <- c(files, entry)
    }
  }
  otel_distinct_files(files)
}

# A symbolic link resolves to its target; a hard link or copy has the same
# contents. Only files of equal size are hashed.
otel_distinct_files <- function(files) {
  files <- unique(normalizePath(files, winslash = "/", mustWork = TRUE))
  sizes <- file.size(files)
  shared <- sizes %in% sizes[duplicated(sizes)]
  if (!any(shared)) {
    return(files)
  }
  digests <- rep(NA_character_, length(files))
  digests[shared] <- unname(tools::md5sum(files[shared]))
  files[!(shared & duplicated(digests))]
}
