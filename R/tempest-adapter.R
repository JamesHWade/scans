#' Convert completed Tempest trajectory reviews
#'
#' `as_trajectory_tempest()` consumes the authoritative closed projection from
#' `tempest::tempest_trajectory_review_data()` and converts it into a canonical
#' [TrajectoryBundle]. It accepts the already-created review and never reaches
#' back into a `TempestResult`, session, workspace, provider, or other live
#' object.
#'
#' One review becomes one product trajectory. Authoritative stage order is
#' preserved before the review's canonical-set lanes. Product stages, Deputy
#' agent runs, programs, knowledge, evidence identities, join proofs, and
#' findings remain distinguishable through namespaced events and structured
#' values. In particular, `authority_validated`, `exact_identity`, and
#' `correlation_only` proof kinds are retained without reinterpretation.
#'
#' The Tempest review deliberately excludes prompts, responses, source
#' content, local paths, credentials, and live runtime objects. Those omissions
#' are recorded as adapter losses rather than reconstructed from private state.
#'
#' @param x A completed value returned by
#'   `tempest::tempest_trajectory_review()`.
#' @param trajectory_id A bundle-unique trajectory ID. When `NULL`, a stable ID
#'   is derived from the Tempest research run ID.
#' @param source_uri An optional source locator. Credentials, query parameters,
#'   and fragments are removed.
#' @param metadata A uniquely named list of safe application metadata.
#'   Sensitive fields are redacted and recorded as losses.
#'
#' @returns A [TrajectoryBundle].
#' @export
#'
#' @examples
#' \dontrun{
#' review <- tempest::tempest_trajectory_review(result)
#' bundle <- as_trajectory_tempest(review)
#'
#' filter_trajectory_events(bundle, event_type = "tempest:join")
#' }
as_trajectory_tempest <- function(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
) {
  call <- rlang::caller_env()
  derived_trajectory_id <- is.null(trajectory_id)
  rlang::check_string(
    trajectory_id,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = call
  )
  rlang::check_string(source_uri, allow_null = TRUE, call = call)
  if (!trajectory_is_named_list(metadata)) {
    scans_abort(
      "{.arg metadata} must be a uniquely named list.",
      class = "scans_error_tempest_argument",
      call = call
    )
  }
  metadata <- trajectory_check_metadata_paths(
    metadata,
    "metadata$application",
    call,
    "scans_error_tempest_argument"
  )

  review <- tempest_review_snapshot(x, call)
  tempest_review_validate_run_id(review$product$research_run_id, call)
  if (is.null(trajectory_id)) {
    trajectory_id <- paste0(
      "tempest/",
      utils::URLencode(review$product$research_run_id, reserved = TRUE)
    )
  }
  tempest_review_validate_trajectory_id(
    trajectory_id,
    derived_trajectory_id,
    call
  )
  ids <- trajectory_ids(trajectory_id)

  source_uri <- trajectory_sanitize_uri(source_uri, "source_uri", ids)
  source_metadata <- trajectory_sanitize_metadata(
    list(
      source = "tempest_trajectory_review",
      schema_version = review$schema_version,
      review_id = review$review_id,
      product = review$product,
      lanes = tempest_review_lane_summaries(review),
      application = metadata
    ),
    "metadata",
    ids
  )
  projected <- tempest_review_events(review, trajectory_id, call)
  stages_complete <- identical(review$stages$omitted, 0L)
  starts_complete <- stages_complete && !anyNA(projected$started_at)
  completions_complete <- stages_complete && !anyNA(projected$completed_at)
  started_at <- if (starts_complete) {
    tempest_review_min_time(projected$started_at)
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  completed_at <- if (completions_complete) {
    tempest_review_max_time(projected$completed_at)
  } else {
    as.POSIXct(NA, tz = "UTC")
  }

  info <- tibble::tibble(
    trajectory_id = trajectory_id,
    run_id = review$product$research_run_id,
    source_type = "tempest",
    source_id = review$review_id,
    source_uri = source_uri$value,
    agent = review$product$mode,
    started_at = started_at,
    completed_at = completed_at,
    status = tempest_review_status(review$product$status),
    metadata = list(source_metadata$value)
  )
  losses <- trajectory_loss_table(c(
    source_uri$losses,
    source_metadata$losses,
    projected$losses,
    tempest_review_omission_losses(review, ids)
  ))

  TrajectoryBundle(
    info,
    NULL,
    projected$events,
    losses = losses
  )
}

tempest_review_validate_trajectory_id <- function(value, derived, call) {
  if (!nzchar(trimws(value))) {
    if (derived) {
      tempest_review_abort(
        "The derived Tempest trajectory ID must contain non-whitespace characters.",
        call
      )
    }
    scans_abort(
      "{.arg trajectory_id} must contain non-whitespace characters.",
      class = "scans_error_tempest_argument",
      call = call
    )
  }
  event_suffix_bytes <- nchar("/tempest-event-000001", type = "bytes")
  maximum <- trajectory_payload_max_bytes - event_suffix_bytes
  if (nchar(value, type = "bytes") <= maximum) {
    return(invisible(value))
  }
  if (derived) {
    tempest_review_abort(
      "The derived Tempest trajectory ID leaves no room for event identities.",
      call
    )
  }
  scans_abort(
    "{.arg trajectory_id} must not exceed {maximum} bytes for Tempest events.",
    class = "scans_error_tempest_argument",
    call = call
  )
}

tempest_review_validate_run_id <- function(value, call) {
  if (nchar(value, type = "bytes") > trajectory_payload_max_bytes) {
    tempest_review_abort(
      "The Tempest research run ID exceeds the canonical payload limit.",
      call
    )
  }
  invisible(value)
}

tempest_review_snapshot <- function(x, call) {
  tempest_review_check_installed(call)
  review <- tryCatch(
    tempest::tempest_trajectory_review_data(x),
    error = function(error) {
      tempest_review_abort(
        c(
          "Can't read the Tempest trajectory review.",
          "x" = conditionMessage(error)
        ),
        call,
        parent = error
      )
    }
  )
  if (!identical(review$schema_version, 1L)) {
    tempest_review_abort(
      "Tempest trajectory review schema {review$schema_version} is unsupported.",
      call
    )
  }
  review
}

tempest_review_check_installed <- function(call = rlang::caller_env()) {
  if (!tempest_review_accessor_available()) {
    scans_abort(
      c(
        paste(
          "A {.pkg tempest} development version that exports",
          "{.fn tempest_trajectory_review_data} is required."
        ),
        "i" = paste(
          "Install it from",
          "{.url https://github.com/JamesHWade/tempest}."
        )
      ),
      class = "scans_error_missing_dependency",
      call = call
    )
  }
  invisible(NULL)
}

tempest_review_accessor_available <- function() {
  if (!rlang::is_installed("tempest")) {
    return(FALSE)
  }
  tryCatch(
    "tempest_trajectory_review_data" %in% getNamespaceExports("tempest"),
    error = function(error) FALSE
  )
}

tempest_review_abort <- function(
  message,
  call,
  parent = NULL,
  .envir = rlang::caller_env()
) {
  scans_abort(
    message,
    class = "scans_error_tempest_source",
    call = call,
    parent = parent,
    .envir = .envir
  )
}

tempest_review_lane_summaries <- function(review) {
  lanes <- c("stages", "agent_runs", "evidence", "joins", "findings")
  stats::setNames(
    lapply(lanes, \(lane) {
      review[[lane]][c("total", "retained", "omitted", "digest")]
    }),
    lanes
  )
}

tempest_review_events <- function(review, trajectory_id, call) {
  rows <- list()
  losses <- list()
  started_at <- as.POSIXct(character(), tz = "UTC")
  completed_at <- as.POSIXct(character(), tz = "UTC")
  reference_keys <- character()
  reference_events <- character()

  add <- function(
    event_type,
    name = NA_character_,
    call_id = NA_character_,
    value = NULL,
    timestamp = as.POSIXct(NA, tz = "UTC"),
    duration = NA_real_,
    status = "completed",
    error = NA_character_,
    metadata = list(),
    parent_event_id = NA_character_,
    field
  ) {
    index <- length(rows) + 1L
    projected <- tempest_review_event(
      trajectory_id,
      index,
      event_type,
      name,
      call_id,
      value,
      timestamp,
      duration,
      status,
      error,
      metadata,
      parent_event_id,
      field,
      call
    )
    rows[[index]] <<- projected$row
    losses <<- c(losses, projected$losses)
    projected$row$event_id[[1L]]
  }

  for (index in seq_along(review$stages$items)) {
    stage <- review$stages$items[[index]]
    ids <- trajectory_ids(trajectory_id)
    start <- tempest_review_time(
      stage$started_at,
      paste0("stages$items[[", index, "]]$started_at"),
      ids
    )
    completion <- tempest_review_time(
      stage$completed_at,
      paste0("stages$items[[", index, "]]$completed_at"),
      ids,
      nullable = TRUE
    )
    losses <- c(losses, start$losses, completion$losses)
    started_at <- c(started_at, start$value)
    completed_at <- c(completed_at, completion$value)
    timestamp <- if (is.na(completion$value)) start$value else completion$value
    duration <- if (is.na(start$value) || is.na(completion$value)) {
      NA_real_
    } else {
      max(
        0,
        as.numeric(difftime(
          completion$value,
          start$value,
          units = "secs"
        ))
      )
    }
    event_id <- add(
      event_type = paste0("tempest:stage_", stage$status),
      name = stage$stage,
      value = stage$output,
      timestamp = timestamp,
      duration = duration,
      status = tempest_review_status(stage$status),
      error = stage$failure_class %||% NA_character_,
      metadata = stage[setdiff(names(stage), "output")],
      field = paste0("stages$items[[", index, "]]")
    )
    reference_keys <- c(
      reference_keys,
      tempest_review_reference_key("stage_attempt", stage$attempt_id)
    )
    reference_events <- c(reference_events, event_id)
  }

  for (index in seq_along(review$agent_runs$items)) {
    agent <- review$agent_runs$items[[index]]
    event_id <- add(
      event_type = paste0("tempest:", agent$trace_type),
      name = agent$role,
      call_id = agent$tool_call_id %||% NA_character_,
      status = tempest_review_status(agent$status),
      metadata = agent,
      field = paste0("agent_runs$items[[", index, "]]")
    )
    reference_keys <- c(
      reference_keys,
      tempest_review_reference_key("deputy_run", agent$deputy_run_id)
    )
    reference_events <- c(reference_events, event_id)
  }

  for (stage in names(review$programs)) {
    add(
      event_type = "tempest:program",
      name = stage,
      value = review$programs[[stage]],
      field = paste0("programs$", stage)
    )
  }

  knowledge <- review$knowledge
  revisions <- list()
  if (!is.null(knowledge$acceptance)) {
    revisions <- knowledge$acceptance$record_revisions$items
    knowledge$acceptance$record_revisions <-
      knowledge$acceptance$record_revisions[
        c("total", "retained", "omitted", "digest")
      ]
  }
  add(
    event_type = "tempest:knowledge",
    name = knowledge$promotion_state,
    value = knowledge,
    field = "knowledge"
  )
  for (index in seq_along(revisions)) {
    add(
      event_type = "tempest:accepted_revision",
      name = revisions[[index]]$class,
      value = revisions[[index]],
      field = paste0(
        "knowledge$acceptance$record_revisions$items[[",
        index,
        "]]"
      )
    )
  }

  for (index in seq_along(review$evidence$items)) {
    evidence <- review$evidence$items[[index]]
    add(
      event_type = "tempest:evidence",
      name = evidence$record_type,
      value = evidence,
      field = paste0("evidence$items[[", index, "]]")
    )
  }

  for (index in seq_along(review$joins$items)) {
    join <- review$joins$items[[index]]
    add(
      event_type = "tempest:join",
      name = join$relation,
      value = join,
      field = paste0("joins$items[[", index, "]]")
    )
  }

  for (index in seq_along(review$findings$items)) {
    finding <- review$findings$items[[index]]
    parent <- tempest_review_reference_event(
      finding$ref_type,
      finding$ref_id,
      reference_keys,
      reference_events
    )
    add(
      event_type = "tempest:finding",
      name = finding$code,
      value = finding,
      status = "completed",
      parent_event_id = parent,
      field = paste0("findings$items[[", index, "]]")
    )
  }

  list(
    events = trajectory_bind_rows(rows),
    losses = losses,
    started_at = started_at,
    completed_at = completed_at
  )
}

tempest_review_event <- function(
  trajectory_id,
  index,
  event_type,
  name,
  call_id,
  value,
  timestamp,
  duration,
  status,
  error,
  metadata,
  parent_event_id,
  field,
  call
) {
  event_id <- paste0(
    trajectory_id,
    "/tempest-event-",
    sprintf("%06d", index)
  )
  ids <- trajectory_ids(trajectory_id, event_id = event_id)
  tempest_review_check_payload_size(value, paste0(field, "$value"), call)
  tempest_review_check_payload_size(
    metadata,
    paste0(field, "$metadata"),
    call
  )
  value <- trajectory_sanitize_value(value, paste0(field, "$value"), ids)
  metadata <- trajectory_sanitize_metadata(
    metadata,
    paste0(field, "$metadata"),
    ids
  )
  error <- trajectory_sanitize_text(error, paste0(field, "$error"), ids)

  list(
    row = tibble::tibble(
      trajectory_id = trajectory_id,
      event_id = event_id,
      event_index = as.integer(index),
      turn_id = NA_character_,
      content_index = NA_integer_,
      parent_event_id = parent_event_id,
      event_type = event_type,
      content_type = NA_character_,
      name = name,
      call_id = call_id,
      text = NA_character_,
      value = list(value$value),
      timestamp = timestamp,
      duration = duration,
      status = status,
      error = error$value,
      metadata = list(metadata$value)
    ),
    losses = c(value$losses, metadata$losses, error$losses)
  )
}

tempest_review_check_payload_size <- function(value, field, call) {
  if (trajectory_serialized_bytes(value) > trajectory_payload_max_bytes) {
    tempest_review_abort(
      "Tempest {.field {field}} must not exceed 65,536 bytes when serialized.",
      call
    )
  }
  invisible(value)
}

tempest_review_time <- function(x, field, ids, nullable = FALSE) {
  if (is.null(x) && nullable) {
    return(list(value = as.POSIXct(NA, tz = "UTC"), losses = list()))
  }
  formats <- c(
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS"
  )
  parsed <- as.POSIXct(NA, tz = "UTC")
  if (rlang::is_string(x) && !is.na(x)) {
    for (format in formats) {
      parsed <- suppressWarnings(as.POSIXct(x, format = format, tz = "UTC"))
      if (!is.na(parsed)) {
        break
      }
    }
  }
  if (!is.na(parsed)) {
    return(list(value = parsed, losses = list()))
  }
  list(
    value = as.POSIXct(NA, tz = "UTC"),
    losses = list(trajectory_new_loss(
      ids,
      field,
      "unsupported",
      "A Tempest stage timestamp could not be represented as POSIXct"
    ))
  )
}

tempest_review_status <- function(x) {
  trajectory_canonical_status(x)
}

tempest_review_reference_key <- function(type, id) {
  paste(type, id, sep = "\037")
}

tempest_review_reference_event <- function(
  type,
  id,
  reference_keys,
  reference_events
) {
  matches <- which(reference_keys == tempest_review_reference_key(type, id))
  if (length(matches) != 1L) {
    return(NA_character_)
  }
  reference_events[[matches]]
}

tempest_review_min_time <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) as.POSIXct(NA, tz = "UTC") else min(x)
}

tempest_review_max_time <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) as.POSIXct(NA, tz = "UTC") else max(x)
}

tempest_review_omission_losses <- function(review, ids) {
  metadata <- list(source = "tempest_trajectory_review")
  losses <- list(
    trajectory_new_loss(
      ids,
      "turns",
      "unsupported",
      "TempestTrajectoryReview intentionally excludes prompts and responses",
      metadata
    ),
    trajectory_new_loss(
      ids,
      "source_content",
      "unsupported",
      "TempestTrajectoryReview intentionally excludes source content",
      metadata
    ),
    trajectory_new_loss(
      ids,
      "runtime",
      "unsupported",
      paste(
        "TempestTrajectoryReview intentionally excludes local paths,",
        "credentials, and live objects"
      ),
      metadata
    )
  )
  lanes <- c("stages", "agent_runs", "evidence", "joins", "findings")
  for (lane in lanes) {
    collection <- review[[lane]]
    if (collection$omitted > 0L) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          paste0(lane, "$items"),
          "truncated",
          paste0(
            "TempestTrajectoryReview retained ",
            collection$retained,
            " of ",
            collection$total,
            " ",
            lane,
            " records"
          ),
          list(
            source = "tempest_trajectory_review",
            complete_digest = collection$digest
          )
        ))
      )
    }
  }
  acceptance <- review$knowledge$acceptance
  if (!is.null(acceptance)) {
    revisions <- acceptance$record_revisions
    if (revisions$omitted > 0L) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          "knowledge$acceptance$record_revisions$items",
          "truncated",
          paste0(
            "TempestTrajectoryReview retained ",
            revisions$retained,
            " of ",
            revisions$total,
            " accepted revision records"
          ),
          list(
            source = "tempest_trajectory_review",
            complete_digest = revisions$digest
          )
        ))
      )
    }
  }
  losses
}
