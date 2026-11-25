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
  connect_progress("Grouping conversations")
  conversations <- otel_read_conversations(
    spans,
    from = from,
    to = to,
    n = n,
    max_spans = max_spans,
    read_at = read_at,
    truncated = isTRUE(attr(lines, "truncated", exact = TRUE)),
    incomplete = isTRUE(attr(lines, "incomplete", exact = TRUE))
  )
  attr(conversations, "source_uri") <- paste0(
    client$server,
    "/content/",
    guid,
    "/"
  )
  conversations
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

connect_request <- function(client, ...) {
  httr2::request(client$server) |>
    httr2::req_url_path_append("__api__", "v1", ...) |>
    httr2::req_headers(
      Authorization = paste("Key", client$api_key),
      .redact = "Authorization"
    ) |>
    httr2::req_user_agent("scans")
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
  failure <- new.env(parent = emptyenv())
  failure$transient <- FALSE
  response <- withCallingHandlers(
    connect_perform(
      connect_trace_request(client, guid, from, to, limit, 0L),
      call
    ),
    scans_connect_transient_failure = function(cnd) {
      failure$transient <- TRUE
    }
  )
  if (is.null(response)) {
    if (!jobs) {
      if (failure$transient) {
        scans_abort(
          "Couldn't read this content's traces from Posit Connect.",
          class = "scans_error_connect_traces",
          call = call
        )
      }
      lines <- character()
      attr(lines, "spans") <- list()
      return(lines)
    }
    lines <- character()
    if (failure$transient) {
      cli::cli_warn(c(
        "This content's current traces could not be read from Posit Connect.",
        "i" = "The result holds retained per-job traces and may be incomplete."
      ))
      attr(lines, "incomplete") <- TRUE
    }
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
  page <- connect_trace_page(response)
  seen <- connect_seen_spans()
  added <- connect_add_trace_lines(character(), page, to, seen = seen)
  lines <- added$lines
  spans <- added$spans
  span_count <- added$span_count
  total <- connect_total_count(response)
  offset <- length(page)
  page_number <- 1L
  # Without a total, a page shorter than requested is the last one, and pages
  # are read one at a time so no wave is spent past the end.
  last_full <- length(page) >= page_size
  connect_progress(sprintf(
    "Read page %d (%s GenAI spans)",
    page_number,
    format(span_count, big.mark = ",")
  ))

  while (
    span_count < max_spans &&
      length(page) > 0L &&
      (if (is.na(total)) last_full else offset < total)
  ) {
    offsets <- seq.int(
      offset,
      by = page_size,
      length.out = if (is.na(total)) 1L else wave_size
    )
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
      added <- connect_add_trace_lines(lines, page, to, seen = seen)
      lines <- added$lines
      spans <- c(spans, added$spans)
      span_count <- span_count + added$span_count
      offset <- offset + length(page)
      last_full <- length(page) >= page_size
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
  if (inherits(response, "httr2_http_404")) {
    return(NULL)
  }
  if (
    inherits(response, c("httr2_http_500", "httr2_http_502", "httr2_http_503"))
  ) {
    signalCondition(structure(
      list(message = conditionMessage(response), call = NULL),
      class = c("scans_connect_transient_failure", "condition")
    ))
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

# New lines are parsed here, once, and the spans travel with the lines so no
# later step has to parse them again.
# `seen` is an environment of "trace span" keys already accumulated: a span
# that arrives again, in another store or on a shifted page, is not added and
# does not count towards the ceiling, so a duplicate cannot displace a real
# conversation at the budget.
connect_add_trace_lines <- function(
  lines,
  page,
  to = NULL,
  drop_after = FALSE,
  seen = NULL
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
  spans <- connect_new_spans(spans, seen)
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

connect_span_key <- function(span) {
  paste(span$trace_id, span$span_id)
}

connect_seen_spans <- function(spans = list()) {
  seen <- new.env(parent = emptyenv())
  for (span in spans) {
    assign(connect_span_key(span), TRUE, envir = seen)
  }
  seen
}

connect_new_spans <- function(spans, seen) {
  if (is.null(seen) || length(spans) == 0L) {
    return(spans)
  }
  keep <- logical(length(spans))
  for (index in seq_along(spans)) {
    key <- connect_span_key(spans[[index]])
    if (!exists(key, envir = seen, inherits = FALSE)) {
      assign(key, TRUE, envir = seen)
      keep[[index]] <- TRUE
    }
  }
  spans[keep]
}

connect_trace_page <- function(response) {
  body <- tryCatch(
    httr2::resp_body_string(response),
    error = function(e) ""
  )
  page <- strsplit(body, "\n", fixed = TRUE)[[1L]]
  page[nzchar(page)]
}
