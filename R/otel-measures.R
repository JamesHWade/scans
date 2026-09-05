# Preserve call aggregates before reconstructing the latest message history.
otel_resource_measures <- function(spans, chat_spans) {
  tool_spans <- Filter(otel_is_tool_span, spans)
  work <- c(chat_spans, tool_spans)
  intervals <- otel_work_intervals(work)
  durations <- intervals$end - intervals$start
  union <- otel_interval_union(intervals$start, intervals$end)
  recorded <- sum(scans_measure_valid(durations))
  total <- length(work)
  times <- otel_span_times(spans)
  elapsed <- as.numeric(difftime(
    times$completed_at,
    times$started_at,
    units = "secs"
  ))
  bounds <- otel_work_intervals(spans)
  n_bounds <- sum(scans_measure_valid(bounds$end - bounds$start))
  if (n_bounds != length(spans)) {
    elapsed <- NA_real_
  }
  gap <- if (total > 0L && recorded == total && is.finite(elapsed)) {
    max(0, elapsed - union)
  } else {
    NA_real_
  }
  attribute_measure <- function(key, aggregation = "sum") {
    scans_measure_values(
      vapply(chat_spans, function(span) otel_numeric(span, key), numeric(1)),
      aggregation
    )
  }
  list(
    version = 1L,
    adapter = "as_trajectory_otel",
    elapsed = list(
      value = elapsed,
      n_recorded = n_bounds,
      n_total = length(spans)
    ),
    input_tokens = attribute_measure("gen_ai.usage.input_tokens"),
    output_tokens = attribute_measure("gen_ai.usage.output_tokens"),
    cached_input_tokens = attribute_measure(
      "gen_ai.usage.cache_read.input_tokens"
    ),
    model_duration = scans_measure_values(vapply(
      chat_spans,
      otel_span_duration,
      numeric(1)
    )),
    tool_duration = scans_measure_values(vapply(
      tool_spans,
      otel_span_duration,
      numeric(1)
    )),
    recorded_work = list(value = union, n_recorded = recorded, n_total = total),
    unattributed_elapsed = list(
      value = gap,
      n_recorded = recorded,
      n_total = total
    ),
    first_chunk_latency = attribute_measure(
      "gen_ai.response.time_to_first_chunk",
      "median"
    )
  )
}

otel_work_intervals <- function(spans) {
  start <- vapply(
    spans,
    function(span) otel_nanos(span$start_time) / 1e9,
    numeric(1)
  )
  end <- vapply(
    spans,
    function(span) otel_nanos(span$end_time) / 1e9,
    numeric(1)
  )
  invalid <- !is.finite(start) | !is.finite(end) | end < start
  start[invalid] <- NA_real_
  end[invalid] <- NA_real_
  list(start = start, end = end)
}

otel_interval_union <- function(start, end) {
  valid <- is.finite(start) & is.finite(end) & end >= start
  if (!any(valid)) {
    return(NA_real_)
  }
  order <- order(start[valid], end[valid])
  start <- start[valid][order]
  end <- end[valid][order]
  previous_end <- c(start[[1L]], utils::head(cummax(end), -1L))
  sum(pmax(0, end - pmax(start, previous_end)))
}
