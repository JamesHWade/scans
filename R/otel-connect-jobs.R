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
  state <- new.env(parent = emptyenv())
  state$lines <- lines
  state$spans <- parsed$spans
  state$span_count <- parsed$span_count
  seen <- connect_seen_spans(state$spans)
  finish <- function() {
    lines <- state$lines
    attr(lines, "spans") <- state$spans
    if (truncated) {
      attr(lines, "truncated") <- TRUE
    }
    if (incomplete) {
      attr(lines, "incomplete") <- TRUE
    }
    lines
  }
  if (state$span_count >= max_spans && truncated) {
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
    added <- connect_add_trace_lines(
      state$lines,
      page,
      to,
      drop_after = TRUE,
      seen = seen
    )
    state$lines <- added$lines
    state$spans <- c(state$spans, added$spans)
    state$span_count <- state$span_count + added$span_count
    connect_progress(sprintf(
      "Read retained job %s page %d (%s GenAI spans)",
      key,
      page_number,
      format(state$span_count, big.mark = ",")
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

  if (state$span_count >= max_spans) {
    truncated <- length(keys) > 0L || state$span_count > max_spans
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
      while (state$span_count < max_spans && page$n > 0L && page_has_more) {
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
      if (state$span_count >= max_spans) {
        later_keys <- index < length(keys)
        truncated <- state$span_count > max_spans || page_has_more || later_keys
        return(finish())
      }
    }
  }
  finish()
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
