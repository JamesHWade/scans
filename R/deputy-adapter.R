#' Convert completed Deputy agent results
#'
#' `as_trajectory_deputy()` snapshots one or more completed
#' `deputy::AgentResult` objects into a canonical [TrajectoryBundle]. It
#' delegates model-visible turns to [as_trajectory_ellmer()] and adds ordered,
#' namespaced Deputy lifecycle events.
#'
#' Run, session, agent, delegation, usage, and immutable run-context values are
#' retained as sanitized data. When a result's parent run occurs in the same
#' input, the adapter resolves `parent_trajectory_id`. Otherwise it retains the
#' parent run ID as metadata and records an adapter loss. Missing run identity
#' is also explicit. The live R6 results, providers, tools, callbacks, and
#' credentials are never retained.
#'
#' `as_trajectory()` dispatches to this adapter for Deputy results and non-empty
#' lists containing only Deputy results when Deputy is installed.
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
#'   rlang::is_installed("deputy", version = "0.0.0.9000") &&
#'     rlang::is_installed("ellmer", version = "0.4.2")
#' ) {
#'   result <- deputy::AgentResult$new(
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

  ids <- ellmer_ids(trajectory_id)
  safe_metadata <- ellmer_sanitize_metadata(
    deputy_result_metadata(snapshot, metadata),
    "metadata",
    ids
  )
  event_tables <- deputy_event_tables(
    snapshot$events,
    trajectory_id,
    nrow(trajectory_events(base)),
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
  info$status <- deputy_result_status(snapshot$stop_reason)
  info$metadata <- list(safe_metadata$value)

  events <- ellmer_bind_rows(list(trajectory_events(base), event_tables$events))
  if (nrow(events) > 0L) {
    events$event_index <- seq_len(nrow(events))
  }

  source_losses <- c(
    safe_metadata$losses,
    deputy_correlation_losses(snapshot, ids),
    event_tables$losses
  )
  losses <- deputy_bind_tables(list(
    trajectory_losses(base),
    ellmer_loss_table(source_losses)
  ))

  TrajectoryBundle(
    info,
    trajectory_turns(base),
    events,
    losses = losses
  )
}

deputy_check_installed <- function(call = rlang::caller_env()) {
  if (!rlang::is_installed("deputy", version = "0.0.0.9000")) {
    scans_abort(
      c(
        "Package {.pkg deputy} 0.0.0.9000 or later is required.",
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
  info <- deputy_bind_tables(lapply(bundles, trajectory_info))
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
  losses <- deputy_bind_tables(lapply(bundles, trajectory_losses))
  resolved <- !is.na(info$parent_trajectory_id)
  if (!is.null(losses) && any(resolved)) {
    resolved_ids <- info$trajectory_id[resolved]
    unresolved_parent <- losses$field == "parent_run_id" &
      losses$trajectory_id %in% resolved_ids
    losses <- losses[!unresolved_parent, , drop = FALSE]
  }

  TrajectoryBundle(
    info,
    deputy_bind_tables(lapply(bundles, trajectory_turns)),
    deputy_bind_tables(lapply(bundles, trajectory_events)),
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

deputy_bind_tables <- function(tables) {
  tables <- Filter(\(table) !is.null(table) && nrow(table) > 0L, tables)
  if (length(tables) == 0L) {
    return(NULL)
  }
  tibble::as_tibble(do.call(rbind, tables))
}

deputy_is_result <- function(x) {
  if (
    !rlang::is_installed("deputy", version = "0.0.0.9000") ||
      !inherits(x, "AgentResult") ||
      !inherits(x, "R6") ||
      !is.function(x$n_turns)
  ) {
    return(FALSE)
  }

  environment <- environment(x$n_turns)
  namespace <- asNamespace("deputy")
  while (is.environment(environment) && !identical(environment, emptyenv())) {
    if (identical(environment, namespace)) {
      return(TRUE)
    }
    environment <- parent.env(environment)
  }
  FALSE
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
    "usage"
  )
  values <- tryCatch(
    lapply(fields, \(field) x[[field]]),
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
  if (!is.list(snapshot$events)) {
    scans_abort(
      c(
        "The Deputy result has invalid events.",
        "x" = "{.field events} must be a list, not {.obj_type_friendly {snapshot$events}}."
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
    structured_output = deputy_plain_value(snapshot$structured_output),
    application = metadata
  )
}

deputy_correlation_losses <- function(snapshot, ids) {
  losses <- list()
  run_id <- deputy_source_string(snapshot$run_id)
  if (is.na(run_id)) {
    losses <- list(ellmer_new_loss(
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
    list(ellmer_new_loss(
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

deputy_event_tables <- function(events, trajectory_id, offset, call) {
  if (!is.list(events)) {
    scans_abort(
      "{.cls deputy::AgentResult} events must be a list.",
      class = "scans_error_deputy_source",
      call = call
    )
  }
  if (length(events) == 0L) {
    return(list(events = tibble::tibble(), losses = list()))
  }

  rows <- vector("list", length(events))
  losses <- list()
  for (index in seq_along(events)) {
    event <- events[[index]]
    event_id <- deputy_event_id(trajectory_id, index)
    ids <- ellmer_ids(trajectory_id, event_id = event_id)
    if (!inherits(event, "AgentEvent") || !is.list(event)) {
      scans_abort(
        "Deputy event {index} is not an {.cls AgentEvent}.",
        class = "scans_error_deputy_source",
        call = call
      )
    }

    event_type <- deputy_event_type(event$type, index, call)
    value <- ellmer_sanitize_value(
      deputy_plain_value(deputy_event_value(event)),
      paste0("events[[", index, "]]$value"),
      ids
    )
    text <- deputy_event_text(event, index, ids)
    error <- deputy_event_error(event$tool_error, index, ids)
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
        "tool_input",
        "tool_result",
        "content",
        "turn"
      )
    )]
    safe_metadata <- ellmer_sanitize_metadata(
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
      event_index = as.integer(offset + index),
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

  list(events = ellmer_bind_rows(rows), losses = losses)
}

deputy_event_value <- function(event) {
  switch(
    event$type,
    tool_start = event$tool_input,
    tool_end = event$tool_result,
    content = event$content,
    NULL
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
  ellmer_sanitize_text(value, paste0("events[[", index, "]]$text"), ids)
}

deputy_event_timestamp <- function(x) {
  if (!inherits(x, "POSIXct") || length(x) != 1L || is.na(x)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  attr(x, "tzone") <- "UTC"
  x
}

deputy_event_status <- function(event) {
  if (identical(event$type, "tool_end") && !is.null(event$tool_error)) {
    return("failed")
  }
  "completed"
}

deputy_event_error <- function(x, index, ids) {
  if (is.null(x)) {
    return(list(value = NA_character_, losses = list()))
  }
  value <- if (inherits(x, "condition")) {
    conditionMessage(x)
  } else if (is.character(x)) {
    paste(x, collapse = "\n")
  } else {
    "Tool execution failed"
  }
  out <- ellmer_sanitize_text(
    value,
    paste0("events[[", index, "]]$error"),
    ids
  )
  if (!inherits(x, "condition") && !is.character(x)) {
    out$losses <- c(
      out$losses,
      list(ellmer_new_loss(
        ids,
        paste0("events[[", index, "]]$error"),
        "unsupported",
        "A non-condition Deputy tool error was not retained exactly"
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

deputy_result_status <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    if (x %in% c("complete", "completed", "success")) {
      return("completed")
    }
    if (x %in% c("error", "failed", "failure")) {
      return("failed")
    }
  }
  "interrupted"
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

deputy_plain_value <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    return(x)
  }
  out <- lapply(x, deputy_plain_value)
  names(out) <- names(x)
  out
}
