#' Convert completed dsprrr traces
#'
#' `as_trajectory_dsprrr()` snapshots one or more rows returned by
#' `dsprrr::export_traces()`, or a dsprrr module through that public exporter,
#' into a canonical [TrajectoryBundle]. Each trace row becomes one trajectory.
#' Model-visible turns are delegated to [as_trajectory_ellmer()] when present.
#'
#' The program artifact digest becomes the source identity and is retained as
#' an integrity assurance, not an authenticity or trust claim. A caller-supplied
#' `run_id` or `deputy_run_id` in `trace_context` may become the canonical
#' `run_id`; product identifiers such as `research_run_id` remain correlation
#' metadata. The adapter never manufactures Deputy or Tempest ownership.
#'
#' `as_trajectory()` dispatches to this adapter for authenticated dsprrr module
#' objects when dsprrr is installed. Exported trace data frames use this
#' explicit adapter because they have no discriminating class.
#'
#' @param x A dsprrr module, or a data frame returned by
#'   `dsprrr::export_traces()`.
#' @param trajectory_id A bundle-unique trajectory ID. When `NULL`, a stable ID
#'   is derived from the program artifact ID and trace order. It must be `NULL`
#'   when `x` contains multiple traces.
#' @param source_uri An optional source locator. Credentials, query parameters,
#'   and fragments are removed.
#' @param metadata A uniquely named list of safe application metadata, applied
#'   to every trace. Sensitive fields are redacted and recorded as losses.
#'
#' @returns A [TrajectoryBundle].
#' @export
#'
#' @examples
#' traces <- tibble::tibble(
#'   timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
#'   latency_ms = 10,
#'   input_tokens = 2L,
#'   cached_input_tokens = 0L,
#'   output_tokens = 1L,
#'   total_tokens = 3L,
#'   cost = 0,
#'   model = "fixture-model",
#'   prompt_length = 5L,
#'   prompt = "Hello",
#'   response = "Hi",
#'   program_artifact_id = paste0("sha256:", strrep("a", 64L)),
#'   trace_context = list(list(run_id = "run-001"))
#' )
#' as_trajectory_dsprrr(traces)
as_trajectory_dsprrr <- function(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
) {
  call <- rlang::caller_env()
  rlang::check_string(trajectory_id, allow_null = TRUE, call = call)
  rlang::check_string(source_uri, allow_null = TRUE, call = call)
  metadata <- dsprrr_check_metadata(metadata, call)

  source <- dsprrr_source_snapshot(x, call)
  traces <- source$traces
  if (nrow(traces) == 0L) {
    return(TrajectoryBundle(NULL, NULL, NULL))
  }
  if (nrow(traces) > 1L && !is.null(trajectory_id)) {
    scans_abort(
      "{.arg trajectory_id} must be {.code NULL} for multiple dsprrr traces.",
      class = "scans_error_dsprrr_argument",
      call = call
    )
  }

  bundles <- lapply(seq_len(nrow(traces)), function(index) {
    id <- if (nrow(traces) == 1L) trajectory_id else NULL
    dsprrr_trace_bundle(
      traces[index, , drop = FALSE],
      index,
      id,
      source_uri,
      metadata,
      source$source_class
    )
  })

  TrajectoryBundle(
    trajectory_bind_rows(lapply(bundles, trajectory_info)),
    trajectory_bind_rows(lapply(bundles, trajectory_turns)),
    trajectory_bind_rows(lapply(bundles, trajectory_events)),
    losses = trajectory_bind_rows(lapply(bundles, trajectory_losses))
  )
}

dsprrr_check_metadata <- function(x, call) {
  if (!trajectory_is_named_list(x)) {
    scans_abort(
      "{.arg metadata} must be a uniquely named list.",
      class = "scans_error_dsprrr_argument",
      call = call
    )
  }
  x
}

dsprrr_source_snapshot <- function(x, call) {
  if (dsprrr_is_module(x)) {
    traces <- tryCatch(
      suppressMessages(dsprrr::export_traces(
        x,
        include_prompts = TRUE,
        include_outputs = TRUE
      )),
      error = function(error) {
        scans_abort(
          c(
            "Can't export traces from the dsprrr module.",
            "x" = conditionMessage(error)
          ),
          class = "scans_error_dsprrr_source",
          call = call,
          parent = error
        )
      }
    )
    source_class <- class(x)[[1L]]
  } else {
    traces <- x
    source_class <- "exported_traces"
  }
  dsprrr_check_snapshot(traces, call)
  list(traces = tibble::as_tibble(traces), source_class = source_class)
}

dsprrr_check_snapshot <- function(x, call) {
  if (!is.data.frame(x)) {
    scans_abort(
      c(
        "{.arg x} must be a dsprrr module or exported trace data frame.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_dsprrr_source",
      call = call
    )
  }
  if (anyDuplicated(names(x))) {
    scans_abort(
      "The exported dsprrr trace must have unique column names.",
      class = "scans_error_dsprrr_source",
      call = call
    )
  }
  required <- c(
    "timestamp",
    "latency_ms",
    "input_tokens",
    "cached_input_tokens",
    "output_tokens",
    "total_tokens",
    "cost",
    "model",
    "prompt_length",
    "prompt",
    "response",
    "program_artifact_id",
    "trace_context"
  )
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    scans_abort(
      c(
        "The exported dsprrr trace is missing required columns.",
        "x" = "Missing: {.field {missing}}."
      ),
      class = "scans_error_dsprrr_source",
      call = call
    )
  }
  if (!is.list(x$trace_context)) {
    scans_abort(
      "The exported dsprrr {.field trace_context} column must be a list.",
      class = "scans_error_dsprrr_source",
      call = call
    )
  }
  invisible(x)
}

dsprrr_trace_bundle <- function(
  trace,
  index,
  trajectory_id,
  source_uri,
  metadata,
  source_class
) {
  artifact_id <- dsprrr_artifact_id(trace$program_artifact_id[[1L]])
  if (is.null(trajectory_id)) {
    trajectory_id <- dsprrr_trajectory_id(artifact_id, index)
  }
  context <- dsprrr_trace_context(trace$trace_context[[1L]])
  run_id <- dsprrr_context_run_id(context)
  agent <- dsprrr_context_string(context, "agent_name")
  model <- dsprrr_string(trace$model[[1L]])
  turns <- dsprrr_trace_turns(trace)

  base <- if (length(turns) > 0L && dsprrr_are_ellmer_turns(turns)) {
    as_trajectory_ellmer(
      turns,
      trajectory_id = trajectory_id,
      run_id = run_id,
      source_id = artifact_id,
      source_uri = source_uri,
      agent = agent,
      model = model,
      metadata = list()
    )
  } else {
    dsprrr_empty_base(
      trajectory_id,
      run_id,
      artifact_id,
      source_uri,
      agent,
      model
    )
  }

  ids <- trajectory_ids(trajectory_id)
  source_metadata <- dsprrr_trace_metadata(
    trace,
    index,
    artifact_id,
    context,
    metadata,
    source_class
  )
  safe_metadata <- trajectory_sanitize_metadata(
    source_metadata,
    "metadata",
    ids
  )
  source_losses <- c(
    safe_metadata$losses,
    dsprrr_trace_losses(trace, ids)
  )
  event <- dsprrr_trace_event(
    trace,
    trajectory_id,
    nrow(trajectory_events(base)),
    safe_metadata$value[c("program_artifact_id", "trace_context")]
  )

  info <- trajectory_info(base)
  info$source_type <- "dsprrr"
  info$started_at <- dsprrr_trace_started_at(trace)
  info$completed_at <- dsprrr_timestamp(trace$timestamp[[1L]])
  info$metadata <- list(safe_metadata$value)

  events <- trajectory_bind_rows(list(trajectory_events(base), event))
  losses <- trajectory_bind_rows(list(
    trajectory_losses(base),
    trajectory_loss_table(source_losses)
  ))

  TrajectoryBundle(
    info,
    trajectory_turns(base),
    events,
    losses = losses
  )
}

dsprrr_empty_base <- function(
  trajectory_id,
  run_id,
  source_id,
  source_uri,
  agent,
  model
) {
  safe_uri <- trajectory_sanitize_uri(
    source_uri,
    "source_uri",
    trajectory_ids(trajectory_id)
  )
  TrajectoryBundle(
    tibble::tibble(
      trajectory_id = trajectory_id,
      run_id = run_id,
      source_type = "dsprrr",
      source_id = source_id,
      source_uri = safe_uri$value,
      agent = agent,
      model = model,
      # The trace completed; only its turns were not exported, and the loss
      # table says so.
      status = "completed"
    ),
    NULL,
    NULL,
    losses = trajectory_loss_table(safe_uri$losses)
  )
}

dsprrr_trace_losses <- function(trace, ids) {
  losses <- list()
  artifact_id <- dsprrr_artifact_id(trace$program_artifact_id[[1L]])
  if (is.na(artifact_id)) {
    losses <- c(
      losses,
      list(trajectory_new_loss(
        ids,
        "program_artifact_id",
        "unsupported",
        "No valid dsprrr program artifact ID was available for source correlation"
      ))
    )
  }

  if (is.na(dsprrr_timestamp(trace$timestamp[[1L]]))) {
    losses <- c(
      losses,
      list(trajectory_new_loss(
        ids,
        "timestamp",
        "unsupported",
        "No valid completion timestamp was available in the dsprrr trace"
      ))
    )
  }
  metric_fields <- c(
    "latency_ms",
    "input_tokens",
    "cached_input_tokens",
    "output_tokens",
    "total_tokens",
    "cost"
  )
  for (field in metric_fields) {
    if (is.na(dsprrr_nonnegative_number(trace[[field]][[1L]]))) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          field,
          "unsupported",
          paste("No valid nonnegative", field, "value was available")
        ))
      )
    }
  }
  if (is.na(dsprrr_string(trace$model[[1L]]))) {
    losses <- c(
      losses,
      list(trajectory_new_loss(
        ids,
        "model",
        "unsupported",
        "No valid model label was available in the dsprrr trace"
      ))
    )
  }

  context <- trace$trace_context[[1L]]
  if (!trajectory_is_named_list(context)) {
    losses <- c(
      losses,
      list(trajectory_new_loss(
        ids,
        "trace_context",
        "unsupported",
        "The dsprrr trace context was not a uniquely named list"
      ))
    )
  } else {
    for (field in c("run_id", "deputy_run_id")) {
      if (field %in% names(context) && is.na(dsprrr_string(context[[field]]))) {
        losses <- c(
          losses,
          list(trajectory_new_loss(
            ids,
            paste0("trace_context$", field),
            "unsupported",
            paste("The caller-supplied", field, "was not a valid string")
          ))
        )
      }
    }
    run_id <- dsprrr_context_string(context, "run_id")
    deputy_run_id <- dsprrr_context_string(context, "deputy_run_id")
    if (!is.na(run_id) && !is.na(deputy_run_id) && run_id != deputy_run_id) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          "trace_context$run_id",
          "unsupported",
          paste(
            "Conflicting caller-supplied run IDs were retained as metadata",
            "but not promoted"
          )
        ))
      )
    }
  }

  turns <- dsprrr_trace_value(trace, "turns")
  valid_turns <- is.list(turns) &&
    length(turns) > 0L &&
    dsprrr_are_ellmer_turns(turns)
  if (!valid_turns) {
    losses <- c(
      losses,
      list(trajectory_new_loss(
        ids,
        "turns",
        "unsupported",
        paste(
          "Model-visible turns were unavailable; export traces with outputs",
          "enabled and retain ellmer turn objects"
        )
      ))
    )
  }
  losses
}

dsprrr_trace_metadata <- function(
  trace,
  index,
  artifact_id,
  context,
  metadata,
  source_class
) {
  fields <- c(
    "prompt",
    "prompt_markdown",
    "prompt_html",
    "response",
    "response_text",
    "response_markdown",
    "response_html",
    "output"
  )
  content <- lapply(fields, \(field) dsprrr_trace_value(trace, field))
  names(content) <- fields
  content <- Filter(Negate(is.null), content)

  list(
    dsprrr_version = dsprrr_version(),
    source_class = source_class,
    trace_index = as.integer(index),
    program_artifact_id = artifact_id,
    program_artifact_assurance = "integrity",
    program_artifact_authenticity = "not_asserted",
    trace_context = context,
    metrics = dsprrr_trace_metrics(trace),
    prompt_length = dsprrr_nonnegative_number(trace$prompt_length[[1L]]),
    content = content,
    source_fields = dsprrr_source_fields(trace, fields),
    application = metadata
  )
}

dsprrr_source_fields <- function(trace, content_fields) {
  core_fields <- c(
    "timestamp",
    "latency_ms",
    "input_tokens",
    "cached_input_tokens",
    "output_tokens",
    "total_tokens",
    "cost",
    "model",
    "prompt_length",
    "program_artifact_id",
    "trace_context",
    "turns",
    content_fields
  )
  fields <- setdiff(names(trace), core_fields)
  values <- lapply(fields, \(field) dsprrr_trace_value(trace, field))
  stats::setNames(values, fields)
}

dsprrr_trace_event <- function(
  trace,
  trajectory_id,
  offset,
  metadata
) {
  tibble::tibble(
    trajectory_id = trajectory_id,
    event_id = paste0(trajectory_id, "/dsprrr-event-000001"),
    event_index = as.integer(offset + 1L),
    turn_id = NA_character_,
    content_index = NA_integer_,
    parent_event_id = NA_character_,
    event_type = "dsprrr:trace",
    content_type = NA_character_,
    name = dsprrr_context_string(
      dsprrr_trace_context(trace$trace_context[[1L]]),
      "agent_name"
    ),
    call_id = NA_character_,
    text = NA_character_,
    value = list(dsprrr_trace_metrics(trace)),
    timestamp = dsprrr_timestamp(trace$timestamp[[1L]]),
    duration = dsprrr_latency_seconds(trace$latency_ms[[1L]]),
    status = "completed",
    error = NA_character_,
    metadata = list(metadata)
  )
}

dsprrr_trace_metrics <- function(trace) {
  list(
    input_tokens = dsprrr_nonnegative_number(trace$input_tokens[[1L]]),
    cached_input_tokens = dsprrr_nonnegative_number(
      trace$cached_input_tokens[[1L]]
    ),
    output_tokens = dsprrr_nonnegative_number(trace$output_tokens[[1L]]),
    total_tokens = dsprrr_nonnegative_number(trace$total_tokens[[1L]]),
    cost = dsprrr_nonnegative_number(trace$cost[[1L]]),
    latency_ms = dsprrr_nonnegative_number(trace$latency_ms[[1L]])
  )
}

dsprrr_trace_turns <- function(trace) {
  if (!"turns" %in% names(trace)) {
    return(list())
  }
  turns <- trace$turns[[1L]]
  if (!is.list(turns)) list() else turns
}

dsprrr_are_ellmer_turns <- function(x) {
  rlang::is_installed("ellmer", version = "0.4.2") &&
    length(x) > 0L &&
    all(vapply(x, ellmer_is_turn, logical(1)))
}

dsprrr_trace_context <- function(x) {
  if (!trajectory_is_named_list(x)) list() else x
}

dsprrr_context_run_id <- function(context) {
  run_id <- dsprrr_context_string(context, "run_id")
  deputy_run_id <- dsprrr_context_string(context, "deputy_run_id")
  if (!is.na(run_id) && !is.na(deputy_run_id) && run_id != deputy_run_id) {
    return(NA_character_)
  }
  if (!is.na(run_id)) run_id else deputy_run_id
}

dsprrr_context_string <- function(context, field) {
  if (!field %in% names(context)) {
    return(NA_character_)
  }
  dsprrr_string(context[[field]])
}

dsprrr_trace_value <- function(trace, field) {
  if (!field %in% names(trace)) {
    return(NULL)
  }
  trace[[field]][[1L]]
}

dsprrr_trace_started_at <- function(trace) {
  completed_at <- dsprrr_timestamp(trace$timestamp[[1L]])
  duration <- dsprrr_latency_seconds(trace$latency_ms[[1L]])
  if (is.na(completed_at) || is.na(duration) || duration < 0) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  completed_at - duration
}

dsprrr_timestamp <- function(x) {
  if (!inherits(x, "POSIXct") || length(x) != 1L || is.na(x)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  attr(x, "tzone") <- "UTC"
  x
}

dsprrr_latency_seconds <- function(x) {
  value <- dsprrr_nonnegative_number(x)
  if (is.na(value)) NA_real_ else value / 1000
}

dsprrr_artifact_id <- function(x) {
  value <- dsprrr_string(x)
  if (is.na(value) || !grepl("^sha256:[0-9a-f]{64}$", value)) {
    return(NA_character_)
  }
  value
}

dsprrr_string <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }
  x
}

dsprrr_number <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    return(NA_real_)
  }
  as.double(x)
}

dsprrr_nonnegative_number <- function(x) {
  value <- dsprrr_number(x)
  if (is.na(value) || value < 0) NA_real_ else value
}

dsprrr_trajectory_id <- function(artifact_id, index) {
  prefix <- if (is.na(artifact_id)) {
    "dsprrr"
  } else {
    paste0("dsprrr/", utils::URLencode(artifact_id, reserved = TRUE))
  }
  paste0(prefix, "/trace-", sprintf("%06d", index))
}

dsprrr_version <- function() {
  if (!rlang::is_installed("dsprrr", version = "0.0.0.9000")) {
    return(NA_character_)
  }
  as.character(utils::packageVersion("dsprrr"))
}

dsprrr_is_module <- function(x) {
  rlang::is_installed("dsprrr", version = "0.0.0.9000") &&
    inherits(x, "Module") &&
    inherits(x, "R6") &&
    is.function(x$get_traces)
}
