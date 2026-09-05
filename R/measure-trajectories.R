#' Measure recorded trajectory resources
#'
#' `measure_trajectories()` reports resource values with their recording
#' denominators and source semantics. It prefers captured OpenTelemetry calls
#' over reconstructed turns. [summarize_trajectories()] retains its original
#' turn-based sums, including raw source costs.
#'
#' @param x A [TrajectoryBundle].
#'
#' @returns A tibble with one row per trajectory and measure:
#' - `trajectory_id`, `parent_trajectory_id`: attribution; children are not
#'   pooled with parents or subtracted from inclusive parent measurements.
#' - `measure`, `value`, `unit`, `aggregation`: the quantity and its operation.
#'   Durations are seconds (`s`); usage is `tokens`.
#' - `n_recorded`, `n_total`: eligible records with a valid value and eligible
#'   records in the loaded snapshot, respectively. Unknown counts are `NA`.
#' - `coverage`: `complete`, `partial`, `unavailable`, or `unknown` (an older
#'   conversation total without a denominator). Complete recording does not
#'   establish complete capture of an application's work.
#' - `source`, `source_unit`, `adapter`, `contract_version`: interpretation
#'   provenance. Contract version 1 describes this measurement interface.
#' - `currency`, `price_basis`: missing until supported cost semantics exist.
#' - `limitation`: the boundary on interpreting the value.
#'
#' @section Measures:
#' `elapsed` is time between trajectory bounds. New OTel captures require
#' valid bounds for every captured span; incomplete bounds are unavailable.
#' `input_tokens`,
#' `output_tokens`, and `cached_input_tokens` sum known captured call values,
#' otherwise canonical assistant turns and other turns explicitly carrying
#' usage. Cached input is a component of input, not an additional token total.
#' `turn_duration` sums known canonical turn durations without asserting one
#' turn equals one model call.
#'
#' `model_duration` sums captured OTel chat durations. `tool_duration` sums
#' OTel execute-tool durations, otherwise canonical tool-call durations.
#' Both include nested activity. `recorded_work` unions captured chat and tool
#' intervals, counting overlap once. `unattributed_elapsed` is elapsed minus
#' this union only when all captured intervals are valid. It does not prove
#' idle time. Do not sum per-trajectory intervals to claim application wall time.
#'
#' `first_chunk_latency` is the median recorded OTel time to first chunk;
#' chunks may contain no token. `first_token_latency` and `cost` are unavailable
#' because supported sources do not establish these semantics. Raw cost sums
#' remain accessible through [summarize_trajectories()]. Missing values never
#' become zero; partial sums describe only recorded components.
#'
#' @export
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "example", source_type = "manual"),
#'   data.frame(), data.frame()
#' )
#' measure_trajectories(bundle)
measure_trajectories <- function(x) {
  check_trajectory_bundle(x)
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  turn_groups <- split(seq_len(nrow(turns)), turns$trajectory_id)
  event_groups <- split(seq_len(nrow(events)), events$trajectory_id)
  rows <- lapply(seq_len(nrow(info)), function(i) {
    id <- info$trajectory_id[[i]]
    scans_measure_trajectory(
      info[i, ],
      turns[turn_groups[[id]] %||% integer(), ],
      events[event_groups[[id]] %||% integer(), ]
    )
  })
  vctrs::vec_rbind(!!!rows, .ptype = scans_measure_template()[0, ])
}

scans_measure_template <- function() {
  names <- c(
    "elapsed",
    "input_tokens",
    "output_tokens",
    "cached_input_tokens",
    "turn_duration",
    "model_duration",
    "tool_duration",
    "recorded_work",
    "unattributed_elapsed",
    "first_chunk_latency",
    "first_token_latency",
    "cost"
  )
  tibble::tibble(
    trajectory_id = NA_character_,
    parent_trajectory_id = NA_character_,
    measure = names,
    value = NA_real_,
    unit = c("s", rep("tokens", 3), rep("s", 7), NA_character_),
    aggregation = c(
      "interval",
      rep("sum", 6),
      "union",
      "difference",
      "median",
      "unavailable",
      "unavailable"
    ),
    n_recorded = NA_integer_,
    n_total = NA_integer_,
    coverage = "unavailable",
    source = "unsupported",
    source_unit = NA_character_,
    adapter = "canonical",
    contract_version = 1L,
    currency = NA_character_,
    price_basis = NA_character_,
    limitation = c(
      "Time between recorded bounds; may include user pauses.",
      "Known input counts only; missing calls can understate usage.",
      "Known output counts only; missing calls can understate usage.",
      "Cached input is included in input tokens; do not add it again.",
      "Inclusive turn durations; a turn need not equal one model call.",
      "Inclusive chat span durations; overlaps count more than once.",
      "Inclusive tool durations; overlaps count more than once.",
      "Union of recorded chat/tool intervals; omitted work is unknown.",
      "Time outside recorded work; does not establish idleness.",
      "Median time to first chunk; a chunk may contain no token.",
      "No supported per-request first-token measurement.",
      "Raw costs lack currency, price basis, and child-inclusion semantics."
    )
  )
}

scans_measure_valid <- function(x) {
  is.finite(x) & x >= 0
}

scans_measure_values <- function(x, aggregation = "sum") {
  valid <- x[scans_measure_valid(x)]
  list(
    value = if (length(valid) == 0L) {
      NA_real_
    } else if (aggregation == "median") {
      stats::median(valid)
    } else {
      sum(valid)
    },
    n_recorded = length(valid),
    n_total = length(x)
  )
}

scans_measure_set <- function(rows, name, measurement, source, source_unit) {
  i <- match(name, rows$measure)
  value <- measurement$value
  recorded <- measurement$n_recorded
  total <- measurement$n_total
  rows$value[[i]] <- value
  rows$n_recorded[[i]] <- as.integer(recorded)
  rows$n_total[[i]] <- as.integer(total)
  rows$coverage[[i]] <- if (is.na(value)) {
    "unavailable"
  } else if (is.na(recorded) || is.na(total)) {
    "unknown"
  } else if (recorded < total) {
    "partial"
  } else {
    "complete"
  }
  rows$source[[i]] <- source
  rows$source_unit[[i]] <- source_unit
  rows
}

scans_measure_trajectory <- function(info, turns, events) {
  rows <- scans_measure_template()
  rows$trajectory_id <- info$trajectory_id
  rows$parent_trajectory_id <- info$parent_trajectory_id
  elapsed <- as.numeric(difftime(
    info$completed_at,
    info$started_at,
    units = "secs"
  ))
  rows <- scans_measure_set(
    rows,
    "elapsed",
    scans_measure_values(elapsed),
    "trajectory_bounds",
    "s"
  )
  for (name in c(
    "input_tokens",
    "output_tokens",
    "cached_input_tokens",
    "turn_duration"
  )) {
    values <- turns[[if (name == "turn_duration") "duration" else name]]
    eligible <- turns$role %in% "assistant" | !is.na(values)
    rows <- scans_measure_set(
      rows,
      name,
      scans_measure_values(values[eligible]),
      "canonical_turns",
      if (name == "turn_duration") "s" else "tokens"
    )
  }
  rows <- scans_measure_set(
    rows,
    "tool_duration",
    scans_measure_values(events$duration[events$event_type %in% "tool_call"]),
    "canonical_tool_calls",
    "s"
  )
  otel <- info$metadata[[1L]]$otel
  if (!is.list(otel)) {
    return(rows)
  }
  # Pre-contract bundles retain their conversation totals without inventing
  # how many captured calls contributed to each component.
  for (name in c("input_tokens", "output_tokens")) {
    value <- otel[[name]]
    if (
      is.numeric(value) && length(value) == 1L && scans_measure_valid(value)
    ) {
      rows <- scans_measure_set(
        rows,
        name,
        list(value = value, n_recorded = NA_integer_, n_total = NA_integer_),
        "otel_conversation_total",
        "tokens"
      )
      rows$adapter[rows$measure == name] <- "as_trajectory_otel/legacy"
    }
  }
  capture <- otel$measures
  if (
    !is.list(capture) ||
      !identical(capture$version, 1L) ||
      !identical(capture$adapter, "as_trajectory_otel")
  ) {
    return(rows)
  }
  sources <- c(
    elapsed = "otel_span_bounds",
    input_tokens = "otel_chat_spans",
    output_tokens = "otel_chat_spans",
    cached_input_tokens = "otel_chat_spans",
    model_duration = "otel_chat_spans",
    tool_duration = "otel_tool_spans",
    recorded_work = "otel_chat_tool_spans",
    unattributed_elapsed = "otel_chat_tool_spans",
    first_chunk_latency = "otel_chat_spans"
  )
  for (name in names(sources)) {
    measurement <- capture[[name]]
    if (!scans_measure_capture_valid(measurement)) {
      next
    }
    rows <- scans_measure_set(
      rows,
      name,
      measurement,
      sources[[name]],
      if (grepl("tokens$", name)) {
        "tokens"
      } else if (name == "first_chunk_latency") {
        "s"
      } else {
        "ns"
      }
    )
    rows$adapter[rows$measure == name] <- "as_trajectory_otel/1"
  }
  rows
}

scans_measure_capture_valid <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  values <- x[c("value", "n_recorded", "n_total")]
  if (
    length(values) != 3L ||
      !all(vapply(
        values,
        function(value) {
          is.numeric(value) && length(value) == 1L
        },
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  is.finite(x$n_recorded) &&
    is.finite(x$n_total) &&
    x$n_recorded >= 0 &&
    x$n_total >= x$n_recorded &&
    x$n_total <= .Machine$integer.max &&
    x$n_recorded == floor(x$n_recorded) &&
    x$n_total == floor(x$n_total) &&
    (is.na(x$value) || (scans_measure_valid(x$value) && x$n_recorded > 0))
}
