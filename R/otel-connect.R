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
#' store. When the content-wide endpoint is missing or fails, the per-job store
#' is used alone.
#'
#' @param source A Posit Connect content GUID, a content URL
#'   (`.../content/<guid>/`), or a dashboard URL (`.../connect/#/apps/<guid>/`).
#' @param n Maximum number of recent conversations to keep. `NULL` keeps all.
#' @param from,to Optional lower-inclusive and upper-exclusive bounds on span
#'   start time. `NULL` leaves that side open. When both are omitted, the seven
#'   days ending now are read.
#' @param server,api_key Connect server URL and API key. Default to
#'   `CONNECT_SERVER` and `CONNECT_API_KEY`.
#' @param max_spans Safety ceiling on how many spans are fetched. Shiny
#'   applications emit thousands of reactive spans per session, so a busy
#'   application's trace store is mostly not GenAI activity.
#'
#' @returns A named list of conversations, each a list of spans, oldest-first.
#'   The names are conversation identifiers.
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
  max_spans = 50000L
) {
  call <- rlang::caller_env()
  use_default_window <- missing(from) && missing(to)
  rlang::check_number_whole(n, min = 1, allow_null = TRUE)
  rlang::check_number_whole(max_spans, min = 1)
  guid <- connect_source_guid(source, call)
  client <- connect_client(server, api_key, call)

  if (use_default_window) {
    now <- Sys.time()
    from <- now - 7 * 24 * 60 * 60
    to <- now
  }

  lines <- connect_trace_lines(client, guid, from, to, max_spans, n, call)
  spans <- otel_parse_otlp_lines(lines, max_spans = max_spans)
  conversations <- otel_group_conversations_in_window(spans, from, to)

  if (!is.null(n) && length(conversations) > n) {
    conversations <- utils::tail(conversations, n)
  }
  attr(conversations, "source_uri") <- connect_content_url(client, guid)
  conversations
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

connect_trace_lines <- function(
  client,
  guid,
  from,
  to,
  max_spans,
  n,
  call,
  page_size = 1000L
) {
  lines <- character()
  span_count <- 0L
  offset <- 0L
  repeat {
    if (span_count >= max_spans) {
      break
    }
    limit <- min(page_size, max_spans - span_count)
    response <- connect_perform(
      connect_request(client, "content", guid, "traces") |>
        httr2::req_url_query(
          limit = limit,
          offset = offset,
          from = connect_window_param(from, -3600),
          to = connect_window_param(to, 1)
        ),
      call
    )
    if (is.null(response)) {
      return(connect_job_trace_lines(
        client,
        guid,
        from,
        to,
        max_spans,
        n,
        call,
        page_size
      ))
    }
    page <- connect_trace_page(response)
    added <- connect_add_trace_lines(lines, page)
    lines <- added$lines
    span_count <- span_count + added$span_count
    total <- suppressWarnings(as.integer(
      httr2::resp_header(response, "X-Total-Count")
    ))
    offset <- offset + length(page)
    if (span_count >= max_spans) {
      if (span_count > max_spans || is.na(total) || offset < total) {
        cli::cli_warn(
          "Stopped after {max_spans} span{?s}; raise
           {.arg max_spans} to read further back."
        )
      }
      break
    }
    if (length(page) == 0L || is.na(total) || offset >= total) {
      break
    }
  }
  connect_job_trace_lines(
    client,
    guid,
    from,
    to,
    max_spans,
    n,
    call,
    page_size,
    lines = lines
  )
}

# A missing or failing aggregate endpoint falls through to the per-job one.
# Authentication and permission failures are reported rather than retried:
# every job request would fail the same way, and the cause is the key, not
# the endpoint.
connect_perform <- function(request, call) {
  tryCatch(
    httr2::req_perform(request),
    httr2_http_401 = function(err) connect_access_abort(err, call),
    httr2_http_403 = function(err) connect_access_abort(err, call),
    httr2_http_404 = function(err) NULL,
    httr2_http_500 = function(err) NULL,
    httr2_http_502 = function(err) NULL,
    httr2_http_503 = function(err) NULL
  )
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
  n,
  call,
  page_size,
  lines = character()
) {
  lines <- unique(lines)
  span_count <- connect_trace_span_count(lines)
  if (span_count >= max_spans) {
    return(lines)
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
  keys <- vapply(jobs, function(job) job$key %||% "", character(1))
  keys <- keys[nzchar(keys)]
  starts <- vapply(jobs, function(job) job$start_time %||% "", character(1))
  keys <- keys[order(starts, decreasing = TRUE)]

  if (!is.null(n) && connect_conversation_count(lines, from, to) >= n) {
    return(unique(lines))
  }
  for (key in keys) {
    offset <- 0L
    repeat {
      if (span_count >= max_spans) {
        break
      }
      limit <- min(page_size, max_spans - span_count)
      response <- connect_perform(
        connect_request(client, "content", guid, "jobs", key, "traces") |>
          httr2::req_url_query(
            limit = limit,
            offset = offset,
            since = connect_window_param(from, -3600)
          ),
        call
      )
      if (is.null(response)) {
        break
      }
      page <- connect_trace_page(response)
      eligible <- connect_trace_lines_before(page, to)
      added <- connect_add_trace_lines(lines, eligible)
      lines <- added$lines
      span_count <- span_count + added$span_count
      total <- suppressWarnings(as.integer(
        httr2::resp_header(response, "X-Total-Count")
      ))
      offset <- offset + length(page)
      if (length(page) == 0L || is.na(total) || offset >= total) {
        break
      }
    }
    if (span_count >= max_spans) {
      break
    }
    # Jobs are walked newest-first, so once enough distinct conversations have
    # been seen the older jobs cannot add any that would survive the `n` most
    # recent. Content with hundreds of jobs is otherwise minutes of requests
    # for pages that are discarded.
    if (!is.null(n) && connect_conversation_count(lines, from, to) >= n) {
      break
    }
  }
  lines
}

connect_add_trace_lines <- function(lines, page) {
  page <- unique(page)
  page <- page[!page %in% lines]
  list(
    lines = c(lines, page),
    span_count = connect_trace_span_count(page)
  )
}

connect_trace_span_count <- function(lines) {
  counts <- vapply(
    lines,
    function(line) max(1L, length(otel_parse_otlp_lines(line))),
    integer(1)
  )
  sum(counts)
}

# The legacy endpoint accepts a lower `since` bound but no upper bound. Drop
# envelopes that contain only post-window spans before applying max_spans, or
# a busy recent job can consume the entire budget for a historical query.
# Keep malformed or undated envelopes here; the parser handles malformed
# input later, and an undated span cannot safely be classified as post-window.
connect_trace_lines_before <- function(lines, to) {
  if (is.null(to)) {
    return(lines)
  }
  keep <- vapply(
    lines,
    function(line) {
      spans <- otel_parse_otlp_lines(line)
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

# The early stop groups properly rather than counting conversation ids
# directly: without walking ancestry, sibling spans of one conversation look
# like separate conversations, the count runs ahead of the truth and the read
# stops before `n` are actually available. Grouping re-parses what has been
# fetched so far, which is cheap next to another round of HTTP.
connect_conversation_count <- function(lines, from, to) {
  spans <- otel_parse_otlp_lines(lines)
  length(otel_group_conversations_in_window(spans, from, to))
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
otel_parse_otlp_lines <- function(lines, max_spans = Inf) {
  spans <- list()
  for (line in lines) {
    envelope <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(envelope)) {
      next
    }
    for (resource in envelope$resourceSpans %||% list()) {
      for (scope in resource$scopeSpans %||% list()) {
        for (span in scope$spans %||% list()) {
          if (length(spans) >= max_spans) {
            return(spans)
          }
          spans[[length(spans) + 1L]] <- otel_span(span, scope)
        }
      }
    }
  }
  spans
}

otel_span <- function(span, scope = NULL) {
  list(
    trace_id = span$traceId %||% "",
    span_id = span$spanId %||% "",
    parent_span_id = span$parentSpanId %||% "",
    name = span$name %||% "",
    scope = scope$scope$name %||% NA_character_,
    start_time = as.character(span$startTimeUnixNano %||% NA),
    end_time = as.character(span$endTimeUnixNano %||% NA),
    status = span$status %||% list(),
    attributes = otel_attributes(span$attributes)
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

otel_span_in_window <- function(span, from, to) {
  start <- otel_nanos(span$start_time)
  if (!is.finite(start)) {
    return(TRUE)
  }
  time <- start / 1e9
  (is.null(from) || time >= as.numeric(from)) &&
    (is.null(to) || time < as.numeric(to))
}

# Group with the padded ancestry still present, then apply the exact time
# window within each group. Filtering first loses an earlier parent that may
# carry the conversation id; keeping it afterwards would incorrectly expose a
# span outside the requested window.
otel_group_conversations_in_window <- function(spans, from, to) {
  groups <- otel_group_conversations(spans)
  groups <- lapply(groups, function(group) {
    Filter(function(span) otel_span_in_window(span, from, to), group)
  })
  groups <- Filter(
    function(group) any(vapply(group, otel_is_chat_span, logical(1))),
    groups
  )
  otel_order_conversations(groups)
}
