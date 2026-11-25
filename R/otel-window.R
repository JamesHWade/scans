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

otel_span_in_window <- function(span, from, to) {
  start <- otel_nanos(span$start_time)
  if (!is.finite(start)) {
    return(TRUE)
  }
  time <- start / 1e9
  (is.null(from) || time >= as.numeric(from)) &&
    (is.null(to) || time < as.numeric(to))
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
