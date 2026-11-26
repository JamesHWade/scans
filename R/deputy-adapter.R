#' Convert completed Deputy agent results
#'
#' `as_trajectory_deputy()` snapshots one or more completed
#' `deputy::AgentResult` objects into a canonical [TrajectoryBundle]. It
#' delegates model-visible turns to [as_trajectory_ellmer()] and adds ordered,
#' namespaced Deputy lifecycle events. Results and events are read through
#' their public S7 properties.
#'
#' Run, session, agent, delegation, usage, ending, and immutable run-context
#' values are retained as sanitized data. When a result's parent run occurs in
#' the same input, the adapter resolves `parent_trajectory_id`. Otherwise it
#' retains the parent run ID as metadata and records an adapter loss. Missing
#' run identity is also explicit. Source objects, providers, tools, callbacks,
#' and credentials are never retained.
#'
#' `as_trajectory()` dispatches to this adapter for Deputy results and non-empty
#' lists containing only Deputy results when Deputy is installed.
#'
#' @details
#' # Run ending
#'
#' The trajectory `status` follows the result's `RunEnding`:
#' `RunEndingAnswered` and `RunEndingCutOff` are `"completed"`,
#' `RunEndingStopped` and `RunEndingLimit` are `"interrupted"`, and
#' `RunEndingFailed` is `"failed"`. A cut-off answer keeps `"completed"`
#' because Deputy completed the run; the provider truncated its final
#' response. Check `metadata$ending$cut_off` whenever a whole answer matters.
#'
#' `trajectory_info(bundle)$metadata[[i]]$ending` always has these fields, in
#' this order, with `NA` where the ending does not supply one:
#'
#' - `class` (character): `"RunEndingAnswered"`, `"RunEndingStopped"`,
#'   `"RunEndingLimit"`, `"RunEndingCutOff"`, or `"RunEndingFailed"`.
#' - `reason` (character): the stop, limit, or failure reason.
#' - `by` (character): who stopped a `RunEndingStopped` run, such as
#'   `"caller"`, `"hook"`, or `"abandoned"`.
#' - `field` (character), `actual` (double), `limit` (double): the
#'   `UsageLimits` field reached, the figure reached, and the figure allowed.
#'   `actual` is also `NA` when Deputy could not know it.
#' - `wrapped_up` (logical): for a limit, whether the model gave a final
#'   answer in the extra request allowed by `on_exceed = "wrap_up"`.
#' - `phase` (character): where a failed run failed.
#' - `finish_reason` (character): the provider's finish reason for the
#'   final response.
#' - `cut_off` (logical): whether the provider cut that response short. It is
#'   `TRUE` for every `RunEndingCutOff` and for a truncated wrap-up answer.
#'
#' The failure condition itself is projected by the `deputy:run_error` event.
#'
#' # Events
#'
#' Each `deputy::AgentEvent` becomes a `deputy:<type>` event, in source
#' order. Fields that are not projected into a column stay in the event's
#' sanitized `metadata`, with Deputy S7 values and conditions as plain lists.
#'
#' - `tool_start`, `approval`: `value` is `tool_input`.
#' - `tool_end`: `value` is `tool_result`, and `error` is `tool_error`.
#' - `content`: `value` is `content`.
#' - `trusted_result`: `value` is the trusted value; `result_id`,
#'   `result_type`, and `tool_fingerprint` stay in `metadata`.
#' - `structured_attempt`: `value` is the attempted value; `valid`,
#'   `feedback`, and `attempt` stay in `metadata`.
#' - `request_error`, `run_error`, `compaction_error`: `error` is the
#'   condition message.
#'
#' `status` is `"failed"` for `request_error`, `run_error`,
#' `compaction_error`, a `tool_end` with an error, a denied `permission`, and
#' an invalid `structured_attempt`; `"pending"` for an `approval` awaiting a
#' decision; and `"completed"` otherwise. `name` and `call_id` carry
#' `tool_name` and `tool_call_id`; `text` carries `text`, a `start` task, or a
#' `warning` message.
#'
#' @param x A completed `deputy::AgentResult`, or a non-empty list of completed
#'   results.
#' @param trajectory_id A bundle-unique trajectory ID. When `NULL`, a stable ID
#'   is derived from the Deputy run ID. It must be `NULL` when `x` contains
#'   multiple results.
#' @param source_uri An optional source locator. Credentials, query parameters,
#'   and fragments are removed.
#' @param metadata A uniquely named list of safe application metadata, applied
#'   to every result. Sensitive fields are redacted and recorded as losses.
#'
#' @returns A [TrajectoryBundle].
#' @export
#'
#' @examples
#' if (
#'   rlang::is_installed("deputy", version = "0.0.0.9007") &&
#'     rlang::is_installed("ellmer", version = "0.5.0")
#' ) {
#'   result <- deputy::AgentResult(
#'     turns = list(
#'       ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
#'       ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
#'     ),
#'     run_id = "run-001",
#'     agent_name = "assistant"
#'   )
#'
#'   bundle <- as_trajectory_deputy(result)
#'   trajectory_events(bundle)
#'   trajectory_info(bundle)$metadata[[1]]$ending
#' }
as_trajectory_deputy <- function(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
) {
  call <- rlang::caller_env()
  deputy_check_installed(call)
  rlang::check_string(trajectory_id, allow_null = TRUE, call = call)
  rlang::check_string(source_uri, allow_null = TRUE, call = call)
  metadata <- deputy_check_metadata(metadata, call)

  results <- deputy_source_results(x, call)
  if (length(results) > 1L) {
    if (!is.null(trajectory_id)) {
      scans_abort(
        "{.arg trajectory_id} must be {.code NULL} for multiple Deputy results.",
        class = "scans_error_deputy_argument",
        call = call
      )
    }
    return(deputy_results_bundle(results, source_uri, metadata, call))
  }
  x <- results[[1L]]

  snapshot <- deputy_result_snapshot(x, call)
  run_id <- deputy_source_string(snapshot$run_id)
  if (is.null(trajectory_id)) {
    trajectory_id <- deputy_trajectory_id(run_id)
  }

  base <- as_trajectory_ellmer(
    snapshot$turns,
    trajectory_id = trajectory_id,
    run_id = deputy_canonical_string(run_id),
    source_id = deputy_canonical_string(run_id),
    source_uri = source_uri,
    agent = deputy_agent(snapshot),
    metadata = list()
  )

  ids <- trajectory_ids(trajectory_id)
  safe_metadata <- trajectory_sanitize_metadata(
    deputy_result_metadata(snapshot, metadata),
    "metadata",
    ids
  )
  event_tables <- deputy_event_tables(
    snapshot$events,
    trajectory_id,
    call
  )

  info <- trajectory_info(base)
  info$source_type <- "deputy"
  info$started_at <- deputy_boundary_time(
    snapshot$events,
    "start",
    first = TRUE
  )
  info$completed_at <- deputy_boundary_time(
    snapshot$events,
    "stop",
    first = FALSE
  )
  info$status <- deputy_status(snapshot)
  info$metadata <- list(safe_metadata$value)

  events <- deputy_merge_events(
    trajectory_events(base),
    event_tables$events
  )
  if (nrow(events) > 0L) {
    events$event_index <- seq_len(nrow(events))
  }

  source_losses <- c(
    safe_metadata$losses,
    deputy_correlation_losses(snapshot, ids),
    event_tables$losses
  )
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

deputy_merge_events <- function(content_events, lifecycle_events) {
  if (nrow(lifecycle_events) == 0L) {
    return(content_events)
  }
  if (nrow(content_events) == 0L) {
    return(lifecycle_events)
  }

  start <- lifecycle_events$event_type == "deputy:start"
  stop <- lifecycle_events$event_type == "deputy:stop"
  trajectory_bind_rows(list(
    lifecycle_events[start, , drop = FALSE],
    content_events,
    lifecycle_events[!start & !stop, , drop = FALSE],
    lifecycle_events[stop, , drop = FALSE]
  ))
}

deputy_check_installed <- function(call = rlang::caller_env()) {
  if (!rlang::is_installed("deputy", version = "0.0.0.9007")) {
    scans_abort(
      c(
        "Package {.pkg deputy} 0.0.0.9007 or later is required.",
        "i" = "Install it from {.url https://github.com/JamesHWade/deputy}."
      ),
      class = "scans_error_missing_dependency",
      call = call
    )
  }
  ellmer_check_installed(call)
}

deputy_check_metadata <- function(x, call) {
  if (!trajectory_is_named_list(x)) {
    scans_abort(
      "{.arg metadata} must be a uniquely named list.",
      class = "scans_error_deputy_argument",
      call = call
    )
  }
  x
}

deputy_check_result <- function(x, call) {
  if (!deputy_is_result(x)) {
    scans_abort(
      c(
        "{.arg x} must be a completed {.cls deputy::AgentResult}.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_deputy_source",
      call = call
    )
  }
  invisible(x)
}

deputy_source_results <- function(x, call) {
  if (deputy_is_result(x)) {
    return(list(x))
  }
  if (!is.list(x) || length(x) == 0L) {
    deputy_check_result(x, call)
  }

  valid <- vapply(x, deputy_is_result, logical(1))
  if (!all(valid)) {
    positions <- which(!valid)
    scans_abort(
      c(
        "{.arg x} must contain only completed {.cls deputy::AgentResult} objects.",
        "x" = "Elements {.val {positions}} are not Deputy results."
      ),
      class = "scans_error_deputy_source",
      call = call
    )
  }
  unname(x)
}

deputy_results_bundle <- function(results, source_uri, metadata, call) {
  bundles <- lapply(seq_along(results), function(index) {
    run_id <- deputy_source_string(results[[index]]$run_id)
    trajectory_id <- if (is.na(run_id)) {
      sprintf("trajectory-%06d", index)
    } else {
      NULL
    }
    as_trajectory_deputy(
      results[[index]],
      trajectory_id = trajectory_id,
      source_uri = source_uri,
      metadata = metadata
    )
  })
  info <- trajectory_bind_rows(lapply(bundles, trajectory_info))
  duplicate <- !is.na(info$run_id) & duplicated(info$run_id)
  if (any(duplicate)) {
    rows <- which(duplicate)
    scans_abort(
      c(
        "Each Deputy result must identify a distinct run.",
        "x" = "Rows {.val {rows}} duplicate an earlier {.field run_id}."
      ),
      class = "scans_error_deputy_identity",
      call = call
    )
  }

  info <- deputy_resolve_parents(info)
  losses <- trajectory_bind_rows(lapply(bundles, trajectory_losses))
  resolved <- !is.na(info$parent_trajectory_id)
  if (!is.null(losses) && any(resolved)) {
    resolved_ids <- info$trajectory_id[resolved]
    unresolved_parent <- losses$field == "parent_run_id" &
      losses$trajectory_id %in% resolved_ids
    losses <- losses[!unresolved_parent, , drop = FALSE]
  }

  TrajectoryBundle(
    info,
    trajectory_bind_rows(lapply(bundles, trajectory_turns)),
    trajectory_bind_rows(lapply(bundles, trajectory_events)),
    losses = losses
  )
}

deputy_resolve_parents <- function(info) {
  run_map <- stats::setNames(info$trajectory_id, info$run_id)
  for (index in seq_len(nrow(info))) {
    parent_run_id <- deputy_source_string(
      info$metadata[[index]]$parent_run_id
    )
    if (!is.na(parent_run_id) && parent_run_id %in% names(run_map)) {
      info$parent_trajectory_id[[index]] <- run_map[[parent_run_id]]
    }
  }
  info
}

deputy_is_result <- function(x) {
  deputy_is_s7_value(x, "AgentResult")
}

deputy_is_result_list <- function(x) {
  is.list(x) &&
    length(x) > 0L &&
    all(vapply(x, deputy_is_result, logical(1)))
}

deputy_result_snapshot <- function(x, call) {
  fields <- c(
    "turns",
    "events",
    "cost",
    "duration",
    "stop_reason",
    "structured_output",
    "session_id",
    "run_id",
    "agent_id",
    "agent_name",
    "parent_agent_id",
    "parent_run_id",
    "delegation_id",
    "run_context",
    "usage",
    "response",
    "ending",
    "finish_reason",
    "cut_off"
  )
  values <- tryCatch(
    lapply(fields, \(field) S7::prop(x, field)),
    error = function(error) {
      scans_abort(
        c(
          "Can't snapshot the {.cls deputy::AgentResult}.",
          "x" = conditionMessage(error)
        ),
        class = "scans_error_deputy_source",
        call = call,
        parent = error
      )
    }
  )
  snapshot <- stats::setNames(values, fields)
  deputy_check_snapshot(snapshot, call)
  snapshot$events <- lapply(snapshot$events, deputy_event_snapshot)
  snapshot
}

deputy_check_snapshot <- function(snapshot, call) {
  if (!is.list(snapshot$turns)) {
    scans_abort(
      c(
        "The Deputy result has invalid turns.",
        "x" = "{.field turns} must be a list, not {.obj_type_friendly {snapshot$turns}}."
      ),
      class = "scans_error_deputy_source",
      call = call
    )
  }
  valid_turns <- vapply(snapshot$turns, ellmer_is_turn, logical(1))
  if (!all(valid_turns)) {
    positions <- which(!valid_turns)
    scans_abort(
      c(
        "The Deputy result contains invalid turns.",
        "x" = "Elements {.val {positions}} do not inherit from {.cls ellmer::Turn}."
      ),
      class = "scans_error_deputy_source",
      call = call
    )
  }
  invisible(snapshot)
}

deputy_result_metadata <- function(snapshot, metadata) {
  list(
    deputy_version = as.character(utils::packageVersion("deputy")),
    source_class = "AgentResult",
    session_id = snapshot$session_id,
    agent_id = snapshot$agent_id,
    parent_agent_id = snapshot$parent_agent_id,
    parent_run_id = snapshot$parent_run_id,
    delegation_id = snapshot$delegation_id,
    run_context = deputy_plain_value(snapshot$run_context),
    usage = deputy_plain_value(snapshot$usage),
    cost = deputy_plain_value(snapshot$cost),
    duration = snapshot$duration,
    stop_reason = snapshot$stop_reason,
    ending = deputy_ending_metadata(
      snapshot$ending,
      snapshot$finish_reason,
      snapshot$cut_off
    ),
    structured_output = deputy_plain_value(snapshot$structured_output),
    application = metadata
  )
}

deputy_ending_classes <- c(
  "RunEndingAnswered",
  "RunEndingStopped",
  "RunEndingLimit",
  "RunEndingCutOff",
  "RunEndingFailed"
)

deputy_ending_class <- function(x) {
  for (name in deputy_ending_classes) {
    if (deputy_is_s7_value(x, name)) {
      return(name)
    }
  }
  NA_character_
}

# One stable shape for every ending, so consumers can read any field without
# checking the class first. See the `@details` of as_trajectory_deputy().
deputy_ending_metadata <- function(ending, finish_reason, cut_off) {
  class <- deputy_ending_class(ending)
  props <- if (is.na(class)) list() else S7::props(ending)
  string <- \(name) deputy_canonical_string(props[[name]])
  number <- \(name) {
    value <- props[[name]]
    if (is.numeric(value) && length(value) == 1L) as.double(value) else NA_real_
  }
  list(
    class = class,
    reason = string("reason"),
    by = string("by"),
    field = string("field"),
    actual = number("actual"),
    limit = number("limit"),
    wrapped_up = if (rlang::is_bool(props$wrapped_up)) {
      props$wrapped_up
    } else {
      NA
    },
    phase = string("phase"),
    finish_reason = deputy_canonical_string(
      finish_reason %||% props$finish_reason
    ),
    cut_off = if (rlang::is_bool(cut_off)) cut_off else NA
  )
}

deputy_status <- function(snapshot) {
  switch(
    deputy_ending_class(snapshot$ending),
    RunEndingAnswered = ,
    RunEndingCutOff = "completed",
    RunEndingStopped = ,
    RunEndingLimit = "interrupted",
    RunEndingFailed = "failed",
    trajectory_canonical_status(snapshot$stop_reason)
  )
}

deputy_correlation_losses <- function(snapshot, ids) {
  losses <- list()
  run_id <- deputy_source_string(snapshot$run_id)
  if (is.na(run_id)) {
    losses <- list(trajectory_new_loss(
      ids,
      "run_id",
      "unsupported",
      "No valid Deputy run ID was available for source correlation"
    ))
  }

  parent_run_id <- deputy_source_string(snapshot$parent_run_id)
  if (is.na(parent_run_id)) {
    return(losses)
  }
  c(
    losses,
    list(trajectory_new_loss(
      ids,
      "parent_run_id",
      "unsupported",
      paste(
        "The parent run ID was retained as metadata because no parent trajectory",
        "snapshot was supplied"
      )
    ))
  )
}

deputy_event_tables <- function(events, trajectory_id, call) {
  if (length(events) == 0L) {
    return(list(events = tibble::tibble(), losses = list()))
  }

  rows <- vector("list", length(events))
  losses <- list()
  for (index in seq_along(events)) {
    event <- events[[index]]
    event_id <- deputy_event_id(trajectory_id, index)
    ids <- trajectory_ids(trajectory_id, event_id = event_id)
    event_type <- deputy_event_type(event$type, index, call)
    value_field <- deputy_event_value_field(event$type)
    value <- trajectory_sanitize_value(
      deputy_plain_value(if (!is.null(value_field)) event[[value_field]]),
      paste0("events[[", index, "]]$value"),
      ids
    )
    text <- deputy_event_text(event, index, ids)
    error <- deputy_event_error(deputy_event_condition(event), index, ids)
    metadata <- event[setdiff(
      names(event),
      c(
        "type",
        "timestamp",
        "tool_call_id",
        "tool_name",
        "text",
        "duration",
        "tool_error",
        "turn",
        value_field
      )
    )]
    safe_metadata <- trajectory_sanitize_metadata(
      deputy_plain_value(metadata),
      paste0("events[[", index, "]]$metadata"),
      ids
    )
    losses <- c(
      losses,
      value$losses,
      text$losses,
      error$losses,
      safe_metadata$losses
    )

    rows[[index]] <- tibble::tibble(
      trajectory_id = trajectory_id,
      event_id = event_id,
      event_index = as.integer(index),
      turn_id = NA_character_,
      content_index = NA_integer_,
      parent_event_id = NA_character_,
      event_type = paste0("deputy:", event_type),
      content_type = NA_character_,
      name = deputy_canonical_string(event$tool_name),
      call_id = deputy_canonical_string(event$tool_call_id),
      text = text$value,
      value = list(value$value),
      timestamp = deputy_event_timestamp(event$timestamp),
      duration = deputy_canonical_number(event$duration),
      status = deputy_event_status(event),
      error = error$value,
      metadata = list(safe_metadata$value)
    )
  }

  list(events = trajectory_bind_rows(rows), losses = losses)
}

deputy_event_value_field <- function(type) {
  switch(
    type,
    tool_start = ,
    approval = "tool_input",
    tool_end = "tool_result",
    content = "content",
    trusted_result = ,
    structured_attempt = "value",
    NULL
  )
}

deputy_event_failed <- function(event) {
  switch(
    event$type,
    tool_end = !is.null(event$tool_error),
    request_error = ,
    run_error = ,
    compaction_error = TRUE,
    permission = identical(event$decision, "deny"),
    structured_attempt = isFALSE(event$valid),
    FALSE
  )
}

# The condition or message a failed event reports as its error.
deputy_event_condition <- function(event) {
  if (!deputy_event_failed(event)) {
    return(NULL)
  }
  switch(
    event$type,
    tool_end = event$tool_error,
    structured_attempt = event$condition %||% event$feedback,
    event$condition
  )
}

deputy_event_type <- function(x, index, call) {
  valid <- is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!valid) {
    scans_abort(
      "Deputy event {index} has no valid event type.",
      class = "scans_error_deputy_source",
      call = call
    )
  }
  type <- tolower(gsub("[^A-Za-z0-9._-]", "_", x))
  if (!grepl("^[a-z]", type)) {
    type <- paste0("event_", type)
  }
  type
}

deputy_event_text <- function(event, index, ids) {
  value <- if (!is.null(event$text)) {
    event$text
  } else if (identical(event$type, "warning")) {
    event$message
  } else if (identical(event$type, "start")) {
    event$task
  } else {
    NULL
  }
  trajectory_sanitize_text(value, paste0("events[[", index, "]]$text"), ids)
}

deputy_event_timestamp <- function(x) {
  if (!inherits(x, "POSIXct") || length(x) != 1L || is.na(x)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  attr(x, "tzone") <- "UTC"
  x
}

deputy_event_status <- function(event) {
  if (deputy_event_failed(event)) {
    return("failed")
  }
  if (identical(event$type, "approval") && identical(event$status, "pending")) {
    return("pending")
  }
  "completed"
}

deputy_event_error <- function(x, index, ids) {
  if (is.null(x)) {
    return(list(value = NA_character_, losses = list()))
  }
  value <- if (inherits(x, "condition")) {
    deputy_condition_message(x)
  } else if (is.character(x)) {
    paste(x, collapse = "\n")
  } else {
    "Tool execution failed"
  }
  out <- trajectory_sanitize_text(
    value,
    paste0("events[[", index, "]]$error"),
    ids
  )
  if (!inherits(x, "condition") && !is.character(x)) {
    out$losses <- c(
      out$losses,
      list(trajectory_new_loss(
        ids,
        paste0("events[[", index, "]]$error"),
        "unsupported",
        "A non-condition Deputy error was not retained exactly"
      ))
    )
  }
  out
}

deputy_boundary_time <- function(events, type, first) {
  if (!is.list(events)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  matching <- Filter(
    \(event) is.list(event) && identical(event$type, type),
    events
  )
  if (length(matching) == 0L) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  times <- lapply(matching, \(event) deputy_event_timestamp(event$timestamp))
  valid <- !vapply(times, is.na, logical(1))
  times <- times[valid]
  if (length(times) == 0L) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  if (first) times[[1L]] else times[[length(times)]]
}

deputy_trajectory_id <- function(run_id) {
  if (is.na(run_id)) {
    return("trajectory-000001")
  }
  paste0("deputy/", utils::URLencode(run_id, reserved = TRUE))
}

deputy_event_id <- function(trajectory_id, index) {
  paste0(trajectory_id, "/deputy-event-", sprintf("%06d", index))
}

deputy_agent <- function(snapshot) {
  agent_name <- deputy_source_string(snapshot$agent_name)
  if (!is.na(agent_name)) {
    return(agent_name)
  }
  deputy_canonical_string(deputy_source_string(snapshot$agent_id))
}

deputy_source_string <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }
  x
}

deputy_canonical_string <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  as.character(x)
}

deputy_canonical_number <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    return(NA_real_)
  }
  as.double(x)
}

deputy_is_s7_value <- function(x, name) {
  if (!inherits(x, "S7_object")) {
    return(FALSE)
  }
  if (!rlang::is_installed("deputy")) {
    return(FALSE)
  }
  class <- getExportedValue("deputy", name)
  inherits(class, "S7_class") && S7::S7_inherits(x, class)
}

deputy_event_snapshot <- function(x) {
  c(
    list(type = S7::prop(x, "type"), timestamp = S7::prop(x, "timestamp")),
    S7::prop(x, "data")
  )
}

# Deputy's read-only value classes that results and events carry. Their
# properties are plain data; subclasses of the abstract families keep their
# class name because the class is what they mean.
deputy_value_classes <- c("AgentUsage", "UsageLimits")
deputy_value_families <- c("RunEnding", "PermissionResult", "HookResult")

deputy_plain_value <- function(x) {
  if (inherits(x, "condition")) {
    return(list(class = class(x), message = deputy_condition_message(x)))
  }
  if (inherits(x, "S7_object")) {
    x <- deputy_s7_plain_value(x)
  }
  if (!is.list(x) || is.data.frame(x)) {
    return(x)
  }
  out <- lapply(x, deputy_plain_value)
  names(out) <- names(x)
  out
}

# Other S7 objects are returned unchanged, so sanitization records them.
deputy_s7_plain_value <- function(x) {
  is_class <- \(names) any(vapply(names, deputy_is_s7_value, logical(1), x = x))
  if (is_class(deputy_value_classes)) {
    return(S7::props(x))
  }
  if (is_class(deputy_value_families)) {
    return(c(list(class = S7::S7_class(x)@name), S7::props(x)))
  }
  x
}

deputy_condition_message <- function(x) {
  tryCatch(
    conditionMessage(x),
    error = \(error) "The condition message could not be read"
  )
}
