#' Read OpenTelemetry traces from Posit Connect
#'
#' @description
#' `read_connect_traces()` fetches a deployed application's GenAI spans from
#' Posit Connect's content observability store and groups them into
#' conversations, ready for [as_trajectory_otel()].
#'
#' Reading traces requires `CONNECT_SERVER` and `CONNECT_API_KEY`, and the key's
#' user must own the content or be a collaborator on it: Connect gates the
#' traces endpoint at editor level.
#'
#' @section Trace endpoints:
#' Connect exposes traces both for a content item as a whole and per job. The
#' current content-wide store and retained per-job stores are merged and
#' de-duplicated because Connect did not migrate legacy traces into the current
#' store. Only jobs that started before the content-wide store's earliest span
#' (and that overlap the window) are read, since later jobs are already in it.
#' When the content-wide endpoint is missing or fails, the per-job store is
#' used alone.
#'
#' @param source A Posit Connect content GUID, a content URL
#'   (`.../content/<guid>/`), or a dashboard URL (`.../connect/#/apps/<guid>/`).
#' @param n Maximum number of recent conversations to keep. `NULL` keeps all.
#' @param from,to Optional lower-inclusive and upper-exclusive bounds on the
#'   start time of a conversation's model calls: a conversation is kept when
#'   one of its model calls started in the window, and its earlier spans are
#'   kept with it as far back as the read reached (an hour before `from`).
#'   Accepts POSIXct, Date, or ISO 8601 strings; a string without a zone is
#'   read as UTC. `NULL` leaves that side open. When both are omitted, the
#'   seven days ending now are read.
#' @param server,api_key Connect server URL and API key. Default to
#'   `CONNECT_SERVER` and `CONNECT_API_KEY`.
#' @param max_spans Ceiling on how many GenAI spans are read. Only spans
#'   carrying `gen_ai.*` attributes count towards it: Shiny applications emit
#'   thousands of reactive spans per session, and those are read past rather
#'   than allowed to exhaust the budget before the conversations are reached.
#'   Framework spans are kept only as long as grouping needs them to link a
#'   model call to its conversation, and are dropped afterwards.
#' @param jobs Whether to also read the retained per-job trace stores that
#'   predate the content-wide store (see *Trace endpoints*). The default,
#'   `TRUE`, is complete; `FALSE` skips those requests, which on a
#'   deployment with many past jobs is most of the read time, and is safe
#'   once the content-wide store is known to hold everything of interest.
#'
#' @returns A named list of conversations, each a list of spans, oldest-first.
#'   The names are conversation identifiers. The `"read_info"` attribute
#'   records how the read went: the window, how many spans and conversations
#'   were found, whether the `max_spans` ceiling cut the read short
#'   (`truncated`), and whether a page failed to load (`incomplete`).
#'
#' @section Progress:
#' Set `options(scans.progress = function(message) ...)` to be told about
#' each page and job as it is read; [scans_app_connect()] uses this to show
#' progress while a load runs.
#'
#' @seealso [as_trajectory_otel()] to convert the result, and
#'   [scans_app_connect()] to review it.
#' @export
read_connect_traces <- function(
  source,
  n = 100L,
  from = NULL,
  to = NULL,
  server = NULL,
  api_key = NULL,
  max_spans = 50000L,
  jobs = TRUE
) {
  call <- rlang::caller_env()
  rlang::check_bool(jobs)
  use_default_window <- missing(from) && missing(to)
  from <- connect_check_bound(from, "from", call)
  to <- connect_check_bound(to, "to", call)
  rlang::check_number_whole(n, min = 1, allow_null = TRUE)
  rlang::check_number_whole(max_spans, min = 1)
  guid <- connect_source_guid(source, call)
  client <- connect_client(server, api_key, call)

  if (use_default_window) {
    now <- Sys.time()
    from <- now - 7 * 24 * 60 * 60
    to <- now
  }

  read_at <- Sys.time()
  lines <- connect_trace_lines(
    client,
    guid,
    from,
    to,
    max_spans,
    call,
    jobs = jobs
  )
  spans <- attr(lines, "spans", exact = TRUE)
  if (is.null(spans)) {
    connect_progress("Parsing spans")
    spans <- otel_parse_otlp_lines(lines)
  }
  spans <- otel_limit_spans(spans, max_spans = max_spans, to = to)
  connect_progress("Grouping conversations")
  conversations <- otel_group_conversations_in_window(spans, from, to)
  found <- length(conversations)

  if (!is.null(n) && found > n) {
    conversations <- utils::tail(conversations, n)
  }
  attr(conversations, "source_uri") <- connect_content_url(client, guid)
  attr(conversations, "read_info") <- list(
    read_at = read_at,
    from = from,
    to = to,
    n = n,
    spans = sum(vapply(spans, otel_is_selected_genai_span, logical(1))),
    spans_total = length(spans),
    max_spans = max_spans,
    truncated = isTRUE(attr(lines, "truncated", exact = TRUE)),
    incomplete = isTRUE(attr(lines, "incomplete", exact = TRUE)),
    conversations_found = found,
    conversations = length(conversations)
  )
  conversations
}

# The span budget and upper window bound, applied to already-parsed spans.
otel_limit_spans <- function(spans, max_spans = Inf, to = NULL) {
  spans <- Filter(
    \(span) otel_span_in_window(span, from = NULL, to = to),
    spans
  )
  selected <- vapply(spans, otel_is_selected_genai_span, logical(1))
  genai <- 0L
  cutoff <- NULL
  for (index in seq_along(spans)) {
    if (selected[[index]]) {
      if (genai >= max_spans) {
        cutoff <- index
        break
      }
      genai <- genai + 1L
    }
  }
  if (is.null(cutoff)) {
    return(spans)
  }

  keep <- seq_along(spans) < cutoff
  # A later parent can supply the selected child's conversation id and wrapper
  # metadata. Retain that ancestry for grouping, but do not expose or count an
  # ancestor that fell beyond the GenAI ceiling.
  positions <- new.env(parent = emptyenv())
  for (index in seq_along(spans)) {
    span <- spans[[index]]
    assign(paste(span$trace_id, span$span_id), index, envir = positions)
  }
  for (index in which(keep & selected)) {
    current <- spans[[index]]
    for (step in seq_len(64L)) {
      parent <- current$parent_span_id
      if (is.null(parent) || is.na(parent) || !nzchar(parent)) {
        break
      }
      key <- paste(current$trace_id, parent)
      if (!exists(key, envir = positions, inherits = FALSE)) {
        break
      }
      parent_index <- get(key, envir = positions, inherits = FALSE)
      if (!keep[[parent_index]]) {
        keep[[parent_index]] <- TRUE
        attr(spans[[parent_index]], "otel_context_only") <- TRUE
      }
      current <- spans[[parent_index]]
    }
  }
  spans[keep]
}

connect_progress <- function(message) {
  hook <- getOption("scans.progress")
  if (is.function(hook)) {
    hook(message)
  }
  invisible(NULL)
}

connect_client <- function(server, api_key, call = rlang::caller_env()) {
  otel_check_installed(call)
  server <- server %||% Sys.getenv("CONNECT_SERVER", "")
  api_key <- api_key %||% Sys.getenv("CONNECT_API_KEY", "")
  if (!nzchar(server) || !nzchar(api_key)) {
    scans_abort(
      c(
        "Reading Posit Connect traces needs a server and an API key.",
        i = "Set {.envvar CONNECT_SERVER} and {.envvar CONNECT_API_KEY}."
      ),
      class = "scans_error_connect_credentials",
      call = call
    )
  }
  list(server = sub("/__api__/?$", "", server), api_key = api_key)
}

otel_check_installed <- function(call = rlang::caller_env()) {
  missing <- Filter(
    function(pkg) !requireNamespace(pkg, quietly = TRUE),
    c("httr2", "jsonlite")
  )
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }
  scans_abort(
    c(
      "Reading Posit Connect traces requires {.pkg {missing}}.",
      i = "Install {.pkg {missing}} to enable it."
    ),
    class = "scans_error_otel_dependency",
    call = call
  )
}

otel_jsonlite_available <- function() {
  requireNamespace("jsonlite", quietly = TRUE)
}

otel_check_jsonlite <- function(call = rlang::caller_env()) {
  if (otel_jsonlite_available()) {
    return(invisible(TRUE))
  }
  scans_abort(
    c(
      "Converting OpenTelemetry spans requires {.pkg jsonlite}.",
      i = "Install {.pkg jsonlite} to enable it."
    ),
    class = "scans_error_otel_dependency",
    call = call
  )
}

connect_content_url <- function(client, guid) {
  paste0(client$server, "/content/", guid, "/")
}

connect_request <- function(client, ...) {
  httr2::request(client$server) |>
    httr2::req_url_path_append("__api__", "v1", ...) |>
    httr2::req_headers(
      Authorization = paste("Key", client$api_key),
      .redact = "Authorization"
    ) |>
    httr2::req_user_agent("scans")
}

# Bounds are compared as seconds, so a Date (days) or a string must become a
# POSIXct first; silently mis-scaling a Date would shift the window by years.
connect_check_bound <- function(x, arg, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "POSIXct") && length(x) == 1L && !is.na(x)) {
    return(x)
  }
  if (inherits(x, "Date") && length(x) == 1L && !is.na(x)) {
    return(as.POSIXct(format(x), tz = "UTC"))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    parsed <- connect_parse_bound_string(x)
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  scans_abort(
    c(
      "{.arg {arg}} must be a single POSIXct, Date, or ISO 8601 string, or {.code NULL}.",
      "i" = "Times without a zone are read as UTC."
    ),
    class = "scans_error_connect_window",
    call = call
  )
}

# ISO 8601 with an optional time, fractional seconds, and a zone: "Z" or a
# numeric offset is honoured, and a string without a zone is read as UTC.
connect_parse_bound_string <- function(x) {
  x <- trimws(x)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    x <- paste0(x, "T00:00:00")
  }
  x <- sub("^(\\d{4}-\\d{2}-\\d{2}) ", "\\1T", x)
  x <- sub(
    "^(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2})(?![:\\d])",
    "\\1:00",
    x,
    perl = TRUE
  )
  x <- sub("Z$", "+0000", x)
  x <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x)
  if (!grepl("[+-]\\d{4}$", x)) {
    x <- paste0(x, "+0000")
  }
  parsed <- tryCatch(
    suppressWarnings(as.POSIXct(
      x,
      format = "%Y-%m-%dT%H:%M:%OS%z",
      tz = "UTC"
    )),
    error = function(cnd) NA
  )
  if (length(parsed) != 1L) NA else parsed
}

connect_source_guid <- function(source, call = rlang::caller_env()) {
  if (!is.character(source) || length(source) != 1L || is.na(source)) {
    scans_abort(
      "{.arg source} must be one Posit Connect content GUID or URL.",
      class = "scans_error_connect_source",
      call = call
    )
  }
  pattern <- "[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}"
  if (grepl(paste0("^", pattern, "$"), source)) {
    return(source)
  }
  match <- regmatches(source, regexpr(pattern, source))
  if (length(match) == 1L && nzchar(match)) {
    return(match)
  }
  scans_abort(
    "Can't find a Posit Connect content GUID in {.val {source}}.",
    class = "scans_error_connect_source",
    call = call
  )
}

# Connect ignores timestamps it cannot parse, including any without an
# explicit zone, so bounds are always formatted here. They are padded
# outward -- `from` by an hour to keep the earlier-starting parent spans that
# carry conversation ids, `to` by a second for sub-second truncation -- and
# the exact window is applied to the parsed spans afterwards.
connect_window_param <- function(time, pad) {
  if (is.null(time)) {
    return(NULL)
  }
  format(time + pad, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

connect_trace_request <- function(client, guid, from, to, limit, offset) {
  connect_request(client, "content", guid, "traces") |>
    httr2::req_url_query(
      limit = limit,
      offset = offset,
      from = connect_window_param(from, -3600),
      to = connect_window_param(to, 1)
    )
}

# Pages after the first are fetched several at a time. Transport pages stay
# large even when `max_spans` is small because framework-only lines do not
# consume that GenAI budget. Bounded waves limit speculative downloads once
# parsing reaches the ceiling.
connect_trace_lines <- function(
  client,
  guid,
  from,
  to,
  max_spans,
  call,
  page_size = 5000L,
  wave_size = 8L,
  jobs = TRUE
) {
  limit <- page_size
  response <- connect_perform(
    connect_trace_request(client, guid, from, to, limit, 0L),
    call
  )
  if (is.null(response)) {
    if (!jobs) {
      lines <- character()
      attr(lines, "spans") <- list()
      return(lines)
    }
    return(connect_job_trace_lines(
      client,
      guid,
      from,
      to,
      max_spans,
      call,
      page_size
    ))
  }
  page <- connect_trace_page(response)
  added <- connect_add_trace_lines(character(), page, to)
  lines <- added$lines
  spans <- added$spans
  span_count <- added$span_count
  total <- connect_total_count(response)
  offset <- length(page)
  page_number <- 1L
  connect_progress(sprintf(
    "Read page %d (%s GenAI spans)",
    page_number,
    format(span_count, big.mark = ",")
  ))

  while (
    span_count < max_spans &&
      length(page) > 0L &&
      (is.na(total) || offset < total)
  ) {
    offsets <- seq.int(offset, by = page_size, length.out = wave_size)
    if (!is.na(total)) {
      offsets <- offsets[offsets < total]
    }
    if (length(offsets) == 0L) {
      break
    }
    requests <- lapply(offsets, function(page_offset) {
      connect_trace_request(
        client,
        guid,
        from,
        to,
        if (is.na(total)) page_size else min(page_size, total - page_offset),
        page_offset
      )
    })
    responses <- connect_perform_batch(requests, call)
    if (any(vapply(responses, is.null, logical(1)))) {
      if (!jobs) {
        scans_abort(
          "Couldn't finish reading this content's traces from Posit Connect.",
          class = "scans_error_connect_traces",
          call = call
        )
      }
      # Pages already read are kept: the per-job stores hold nothing recorded
      # after Connect's content-wide store began, so dropping them would turn
      # one failed page into an empty, silently incomplete result.
      cli::cli_warn(c(
        "A page of this content's traces could not be read from Posit Connect.",
        "i" = "The result holds the pages read so far plus retained per-job traces and may be incomplete."
      ))
      # Distinct from `truncated`: nothing about the span budget was reached,
      # a page simply failed, and the app must say that rather than blame the
      # ceiling.
      attr(lines, "incomplete") <- TRUE
      attr(lines, "spans") <- spans
      return(connect_job_trace_lines(
        client,
        guid,
        from,
        to,
        max_spans,
        call,
        page_size,
        lines = lines
      ))
    }
    for (response in responses) {
      page <- connect_trace_page(response)
      added <- connect_add_trace_lines(lines, page, to)
      lines <- added$lines
      spans <- c(spans, added$spans)
      span_count <- span_count + added$span_count
      offset <- offset + length(page)
      page_number <- page_number + 1L
      connect_progress(sprintf(
        "Read page %d (%s GenAI spans)",
        page_number,
        format(span_count, big.mark = ",")
      ))
      if (length(page) == 0L) {
        break
      }
    }
    if (length(responses) == 0L) {
      break
    }
  }
  if (span_count >= max_spans) {
    if (span_count > max_spans || is.na(total) || offset < total) {
      cli::cli_warn(
        "Stopped after {max_spans} span{?s}; raise
         {.arg max_spans} to read further back."
      )
      attr(lines, "truncated") <- TRUE
    }
  }
  attr(lines, "spans") <- spans
  if (!jobs) {
    return(lines)
  }
  connect_job_trace_lines(
    client,
    guid,
    from,
    to,
    max_spans,
    call,
    page_size,
    lines = lines
  )
}

# A missing or unparseable X-Total-Count is NA, and callers then page until a
# short or empty page rather than trusting a count they do not have.
connect_total_count <- function(response) {
  header <- httr2::resp_header(response, "X-Total-Count") %||% NA_character_
  suppressWarnings(as.integer(header))
}

# A missing or failing aggregate endpoint falls through to the per-job one.
# Authentication and permission failures are reported rather than retried:
# every job request would fail the same way, and the cause is the key, not
# the endpoint.
connect_perform <- function(request, call) {
  tryCatch(
    httr2::req_perform(request),
    error = function(err) connect_response_result(err, call)
  )
}

# Apply the same narrow recovery policy to sequential and parallel responses.
# Only endpoint-missing and transient server errors fall back; request errors,
# rate limits, timeouts, and other unexpected failures retain their condition.
connect_response_result <- function(response, call) {
  if (inherits(response, c("httr2_http_401", "httr2_http_403"))) {
    connect_access_abort(response, call)
  }
  if (
    inherits(
      response,
      c("httr2_http_404", "httr2_http_500", "httr2_http_502", "httr2_http_503")
    )
  ) {
    return(NULL)
  }
  if (inherits(response, "error")) {
    stop(response)
  }
  response
}

# Several requests at once, with the same failure policy as connect_perform().
connect_perform_batch <- function(requests, call, max_active = 6L) {
  if (length(requests) == 0L) {
    return(list())
  }
  if (length(requests) == 1L) {
    return(list(connect_perform(requests[[1L]], call)))
  }
  responses <- httr2::req_perform_parallel(
    requests,
    on_error = "continue",
    progress = FALSE,
    max_active = max_active
  )
  lapply(responses, connect_response_result, call = call)
}

connect_access_abort <- function(err, call) {
  scans_abort(
    c(
      "Couldn't read this content's traces from Posit Connect.",
      i = "Reading traces requires editor access: the {.envvar CONNECT_API_KEY}
           user must own the content or be a collaborator on it."
    ),
    class = "scans_error_connect_access",
    call = call,
    parent = err
  )
}

connect_job_trace_lines <- function(
  client,
  guid,
  from,
  to,
  max_spans,
  call,
  page_size,
  lines = character(),
  wave_size = 8L
) {
  truncated <- isTRUE(attr(lines, "truncated", exact = TRUE))
  incomplete <- isTRUE(attr(lines, "incomplete", exact = TRUE))
  spans <- attr(lines, "spans", exact = TRUE)
  lines <- unique(lines)
  parsed <- connect_trace_lines_summary(lines, to, spans = spans)
  spans <- parsed$spans
  span_count <- parsed$span_count
  finish <- function() {
    attr(lines, "spans") <- spans
    if (truncated) {
      attr(lines, "truncated") <- TRUE
    }
    if (incomplete) {
      attr(lines, "incomplete") <- TRUE
    }
    lines
  }
  if (span_count >= max_spans && truncated) {
    return(finish())
  }
  jobs <- connect_perform(
    connect_request(client, "content", guid, "jobs"),
    call
  )
  if (is.null(jobs)) {
    scans_abort(
      "Couldn't read this content's traces from Posit Connect.",
      class = "scans_error_connect_traces",
      call = call
    )
  }
  jobs <- httr2::resp_body_json(jobs)
  keys <- connect_job_keys(jobs, from, to, before = parsed$earliest)
  if (length(keys) > 0L) {
    connect_progress(sprintf(
      "Reading %d retained job trace store%s",
      length(keys),
      if (length(keys) == 1L) "" else "s"
    ))
  }

  job_request <- function(key, limit, offset) {
    connect_request(client, "content", guid, "jobs", key, "traces") |>
      httr2::req_url_query(
        limit = limit,
        offset = offset,
        since = connect_window_param(from, -3600)
      )
  }
  add_page <- function(response, key, page_number) {
    page <- connect_trace_page(response)
    added <- connect_add_trace_lines(lines, page, to, drop_after = TRUE)
    lines <<- added$lines
    spans <<- c(spans, added$spans)
    span_count <<- span_count + added$span_count
    connect_progress(sprintf(
      "Read retained job %s page %d (%s GenAI spans)",
      key,
      page_number,
      format(span_count, big.mark = ",")
    ))
    list(
      n = length(page),
      total = connect_total_count(response)
    )
  }
  abort_failed_page <- function(key) {
    scans_abort(
      c(
        "Couldn't finish reading this content's traces from Posit Connect.",
        i = "A retained trace page for job {.val {key}} failed."
      ),
      class = "scans_error_connect_traces",
      call = call
    )
  }

  if (span_count >= max_spans) {
    truncated <- length(keys) > 0L || span_count > max_spans
    return(finish())
  }
  if (length(keys) == 0L) {
    return(finish())
  }

  # Queue only a bounded wave of retained stores. Once the budget is reached,
  # no later wave is downloaded merely to be discarded.
  for (wave_start in seq.int(1L, length(keys), by = wave_size)) {
    wave_indices <- seq.int(
      wave_start,
      min(length(keys), wave_start + wave_size - 1L)
    )
    request_limit <- page_size
    first_pages <- connect_perform_batch(
      lapply(
        keys[wave_indices],
        job_request,
        limit = request_limit,
        offset = 0L
      ),
      call,
      max_active = wave_size
    )
    for (wave_position in seq_along(wave_indices)) {
      index <- wave_indices[[wave_position]]
      response <- first_pages[[wave_position]]
      if (is.null(response)) {
        abort_failed_page(keys[[index]])
      }
      page_number <- 1L
      page <- add_page(response, keys[[index]], page_number)
      offset <- page$n
      page_has_more <- if (is.na(page$total)) {
        page$n >= request_limit
      } else {
        offset < page$total
      }
      while (span_count < max_spans && page$n > 0L && page_has_more) {
        request_limit <- page_size
        response <- connect_perform(
          job_request(keys[[index]], request_limit, offset),
          call
        )
        if (is.null(response)) {
          abort_failed_page(keys[[index]])
        }
        page_number <- page_number + 1L
        page <- add_page(response, keys[[index]], page_number)
        offset <- offset + page$n
        page_has_more <- if (is.na(page$total)) {
          page$n >= request_limit
        } else {
          offset < page$total
        }
      }
      if (span_count >= max_spans) {
        later_keys <- index < length(keys)
        truncated <- span_count > max_spans || page_has_more || later_keys
        return(finish())
      }
    }
  }
  finish()
}

# The GenAI span count and the earliest span start of a set of lines, from
# one parse.
connect_trace_lines_summary <- function(lines, to = NULL, spans = NULL) {
  if (is.null(spans)) {
    parsed <- otel_parse_otlp_lines_counted(lines)
    spans <- parsed$spans
  }
  earliest <- Inf
  count <- 0L
  for (span in spans) {
    start <- otel_nanos(span$start_time)
    if (is.finite(start)) {
      earliest <- min(earliest, start / 1e9)
    }
    if (
      otel_is_genai_span(span) &&
        otel_span_in_window(span, from = NULL, to = to)
    ) {
      count <- count + 1L
    }
  }
  list(
    spans = spans,
    span_count = count,
    earliest = if (is.finite(earliest)) earliest else NULL
  )
}

# Jobs are read newest-first. Two filters keep a long-lived deployment with
# hundreds of past jobs from costing a request each: a job that finished
# before the window opened cannot hold a span inside it, and a job that
# started after the content-wide store's earliest span is already covered
# by that store -- per-job traces are only ever *older* than it, because
# Connect did not migrate them when the content-wide store arrived. Jobs
# without a parseable time are kept.
connect_job_keys <- function(jobs, from, to = NULL, before = NULL) {
  keys <- vapply(jobs, function(job) job$key %||% "", character(1))
  starts <- vapply(jobs, function(job) job$start_time %||% "", character(1))
  ends <- vapply(jobs, function(job) job$end_time %||% "", character(1))
  started <- connect_parse_time(starts)
  keep <- nzchar(keys)
  if (!is.null(from)) {
    ended <- connect_parse_time(ends)
    keep <- keep & (is.na(ended) | ended >= as.numeric(from) - 3600)
  }
  if (!is.null(before)) {
    keep <- keep & (is.na(started) | started < before)
  }
  if (!is.null(to)) {
    keep <- keep & (is.na(started) | started < as.numeric(to))
  }
  keys <- keys[keep]
  keys[order(started[keep], decreasing = TRUE, na.last = TRUE)]
}

connect_parse_time <- function(x) {
  x <- sub("Z$", "+0000", x)
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  parsed <- suppressWarnings(as.POSIXct(
    x,
    format = "%Y-%m-%dT%H:%M:%OS%z",
    tz = "UTC"
  ))
  as.numeric(parsed)
}

# New lines are parsed here, once, and the spans travel with the lines so no
# later step has to parse them again.
connect_add_trace_lines <- function(
  lines,
  page,
  to = NULL,
  drop_after = FALSE
) {
  page <- unique(page)
  page <- page[!page %in% lines]
  envelopes <- otel_parse_envelopes_each(page)
  if (drop_after && !is.null(to)) {
    keep <- vapply(
      envelopes,
      function(envelope) {
        page_spans <- otel_envelope_spans(envelope)
        length(page_spans) == 0L ||
          any(vapply(
            page_spans,
            otel_span_in_window,
            logical(1),
            from = NULL,
            to = to
          ))
      },
      logical(1)
    )
    page <- page[keep]
    envelopes <- envelopes[keep]
  }
  spans <- unlist(lapply(envelopes, otel_envelope_spans), recursive = FALSE)
  span_count <- as.integer(sum(vapply(
    spans,
    function(span) {
      otel_is_genai_span(span) &&
        otel_span_in_window(span, from = NULL, to = to)
    },
    logical(1)
  )))
  list(
    lines = c(lines, page),
    spans = spans,
    span_count = span_count
  )
}

# The legacy endpoint accepts a lower `since` bound but no upper bound. Drop
# envelopes that contain only post-window spans before applying max_spans, or
# a busy recent job can consume the entire budget for a historical query.
# Keep malformed or undated envelopes here; the parser handles malformed
# input later, and an undated span cannot safely be classified as post-window.
connect_trace_lines_before <- function(lines, to) {
  if (is.null(to) || length(lines) == 0L) {
    return(lines)
  }
  keep <- vapply(
    otel_parse_envelopes_each(lines),
    function(envelope) {
      spans <- otel_envelope_spans(envelope)
      length(spans) == 0L ||
        any(vapply(
          spans,
          otel_span_in_window,
          logical(1),
          from = NULL,
          to = to
        ))
    },
    logical(1)
  )
  lines[keep]
}

# One envelope per line, NULL where a line is malformed (so positions line
# up with the input).
otel_parse_envelopes_each <- function(lines) {
  batch <- tryCatch(
    jsonlite::fromJSON(
      paste0("[", paste(lines, collapse = ","), "]"),
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  if (is.list(batch) && length(batch) == length(lines)) {
    return(batch)
  }
  lapply(lines, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })
}

otel_envelope_spans <- function(envelope) {
  spans <- list()
  for (resource in envelope$resourceSpans %||% list()) {
    for (scope in resource$scopeSpans %||% list()) {
      for (span in scope$spans %||% list()) {
        spans[[length(spans) + 1L]] <- otel_span(span, scope, resource)
      }
    }
  }
  spans
}

connect_trace_page <- function(response) {
  body <- tryCatch(
    httr2::resp_body_string(response),
    error = function(e) ""
  )
  page <- strsplit(body, "\n", fixed = TRUE)[[1L]]
  page[nzchar(page)]
}

# ---- OTLP parsing -----------------------------------------------------------

# Each line is one OTLP envelope: {"resourceSpans":[{"scopeSpans":[{"spans":
# [...]}]}]}. A malformed line is skipped rather than failing the read; one
# bad record should not cost the whole history.
otel_parse_otlp_lines <- function(lines, max_spans = Inf, to = NULL) {
  spans <- list()
  genai <- 0L
  for (envelope in otel_parse_envelopes(lines)) {
    for (resource in envelope$resourceSpans %||% list()) {
      for (scope in resource$scopeSpans %||% list()) {
        for (span in scope$spans %||% list()) {
          parsed_span <- otel_span(span, scope, resource)
          if (!otel_span_in_window(parsed_span, from = NULL, to = to)) {
            next
          }
          if (otel_is_genai_span(parsed_span)) {
            if (genai >= max_spans) {
              return(spans)
            }
            genai <- genai + 1L
          }
          spans[[length(spans) + 1L]] <- parsed_span
        }
      }
    }
  }
  spans
}

# Lines are parsed as one JSON array: a call per line spent most of a
# store's read time in jsonlite's setup. If any line is malformed the array
# fails to parse and the lines are parsed one by one, skipping the bad ones.
otel_parse_envelopes <- function(lines) {
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return(list())
  }
  batch <- tryCatch(
    jsonlite::fromJSON(
      paste0("[", paste(lines, collapse = ","), "]"),
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  if (is.list(batch) && length(batch) == length(lines)) {
    attr(batch, "dropped") <- 0L
    return(batch)
  }
  envelopes <- lapply(lines, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
  })
  kept <- envelopes[!vapply(envelopes, is.null, logical(1))]
  attr(kept, "dropped") <- length(lines) - length(kept)
  kept
}

# Spans from a set of lines, plus how many lines could not be parsed at all.
otel_parse_otlp_lines_counted <- function(lines, to = NULL) {
  envelopes <- otel_parse_envelopes(lines)
  spans <- list()
  for (envelope in envelopes) {
    for (resource in envelope$resourceSpans %||% list()) {
      for (scope in resource$scopeSpans %||% list()) {
        for (span in scope$spans %||% list()) {
          spans[[length(spans) + 1L]] <- otel_span(span, scope, resource)
        }
      }
    }
  }
  list(spans = spans, dropped = attr(envelopes, "dropped") %||% 0L)
}

otel_span <- function(span, scope = NULL, resource = NULL) {
  list(
    trace_id = span$traceId %||% "",
    span_id = span$spanId %||% "",
    parent_span_id = span$parentSpanId %||% "",
    name = span$name %||% "",
    scope = scope$scope$name %||% NA_character_,
    start_time = as.character(span$startTimeUnixNano %||% NA),
    end_time = as.character(span$endTimeUnixNano %||% NA),
    status = span$status %||% list(),
    attributes = otel_attributes(span$attributes),
    resource = otel_attributes(resource$resource$attributes)
  )
}

otel_attributes <- function(attributes) {
  values <- lapply(attributes %||% list(), function(attribute) {
    otel_attribute_value(attribute$value)
  })
  names(values) <- vapply(
    attributes %||% list(),
    function(attribute) attribute$key %||% "",
    character(1)
  )
  values[nzchar(names(values))]
}

otel_attribute_value <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }
  for (field in c("stringValue", "intValue", "doubleValue", "boolValue")) {
    if (!is.null(value[[field]])) {
      return(value[[field]])
    }
  }
  if (!is.null(value$arrayValue)) {
    return(paste(
      vapply(
        value$arrayValue$values %||% list(),
        function(x) as.character(otel_attribute_value(x)),
        character(1)
      ),
      collapse = ", "
    ))
  }
  NA_character_
}

# A span without a GenAI semantic-convention attribute is framework activity
# (a Shiny reactive, an HTTP handler) and does not consume the GenAI budget.
otel_is_genai_span <- function(span) {
  keys <- names(span$attributes)
  any(startsWith(keys, "gen_ai."))
}

otel_is_selected_genai_span <- function(span) {
  otel_is_genai_span(span) &&
    !isTRUE(attr(span, "otel_context_only", exact = TRUE))
}

otel_is_selected_chat_span <- function(span) {
  otel_is_chat_span(span) &&
    !isTRUE(attr(span, "otel_context_only", exact = TRUE))
}

otel_span_in_window <- function(span, from, to) {
  start <- otel_nanos(span$start_time)
  if (!is.finite(start)) {
    return(TRUE)
  }
  time <- start / 1e9
  (is.null(from) || time >= as.numeric(from)) &&
    (is.null(to) || time < as.numeric(to))
}

# Group with the padded ancestry still present, then apply the time window.
# The window selects conversations: one is kept when a model call started
# inside [from, to). Within a kept conversation every span before `to` stays,
# because the latest model call carries the whole history anyway and an
# earlier tool failure or token total is part of the conversation being
# reviewed. Spans at or after `to` are dropped so the read is "as of `to`".
# Only spans the read fetched can be kept: Connect is asked from an hour
# before `from`, so a conversation that began earlier still keeps its full
# transcript (the latest model call carries it) but not the timing or usage
# of calls before that hour. Widen `from` to recover them.
otel_group_conversations_in_window <- function(spans, from, to) {
  groups <- otel_group_conversations(spans)
  groups <- Filter(
    function(group) {
      any(vapply(
        group,
        function(span) {
          otel_is_chat_span(span) && otel_span_in_window(span, from, to)
        },
        logical(1)
      ))
    },
    groups
  )
  groups <- lapply(groups, function(group) {
    context <- attr(group, "otel_context", exact = TRUE)
    group <- Filter(
      function(span) otel_span_in_window(span, from = NULL, to = to),
      group
    )
    attr(group, "otel_context") <- context
    group
  })
  otel_order_conversations(groups)
}
