#' Convert completed commons conversations
#'
#' `as_trajectory_commons()` converts the already-read result of
#' `commons::trajectory_read()` into one canonical [TrajectoryBundle]. Each
#' named conversation becomes one trajectory, and its model-visible turns are
#' delegated to [as_trajectory_ellmer()]. The adapter does not read files,
#' connect to Posit Connect, or parse trace records itself.
#'
#' Conversation provenance and the outer source descriptor are retained as
#' sanitized metadata. Each exchange provenance record also becomes a
#' `"commons:provenance"` event attached to the final turn in that exchange.
#' Missing or malformed source facts are reported as adapter losses rather than
#' inferred.
#'
#' Because `commons::trajectory_read()` returns an ordinary list without a
#' discriminating class, use this explicit adapter instead of
#' [as_trajectory()].
#'
#' @param x An already-read, named list returned by
#'   `commons::trajectory_read()`.
#' @param trajectory_id A bundle-unique trajectory ID. When `NULL`, a stable ID
#'   is derived from the conversation name. It must be `NULL` when `x` contains
#'   multiple conversations.
#' @param metadata A uniquely named list of safe application metadata, applied
#'   to every conversation. Sensitive fields are redacted and recorded as
#'   losses. Names used by commons source metadata are reserved.
#'
#' @returns A [TrajectoryBundle].
#' @export
#'
#' @examples
#' if (rlang::is_installed("ellmer", version = "0.4.2")) {
#'   conversation <- list(
#'     ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
#'     ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
#'   )
#'   attr(conversation, "last_active") <- as.POSIXct(
#'     "2026-08-23 12:00:00",
#'     tz = "UTC"
#'   )
#'   attr(conversation, "provenance") <- list(list(
#'     provenance_tag = "A",
#'     citation_decisions = list()
#'   ))
#'
#'   source <- list(`conversation-001` = conversation)
#'   attr(source, "source") <- list(kind = "local", path = "/traces")
#'   as_trajectory_commons(source)
#' }
as_trajectory_commons <- function(
  x,
  trajectory_id = NULL,
  metadata = list()
) {
  call <- rlang::caller_env()
  commons_check_trajectory_id(trajectory_id, call)
  if (!trajectory_is_named_list(metadata)) {
    scans_abort(
      "{.arg metadata} must be a uniquely named list.",
      class = "scans_error_commons_argument",
      call = call
    )
  }
  reserved <- intersect(
    names(metadata),
    c(
      "source",
      "provenance",
      "source_attributes",
      "conversation_attributes"
    )
  )
  if (length(reserved) > 0L) {
    scans_abort(
      "{.arg metadata} must not use reserved name{?s}: {.field {reserved}}.",
      class = "scans_error_commons_argument",
      call = call
    )
  }
  commons_check_source(x, call)

  if (length(x) == 0L) {
    return(TrajectoryBundle(NULL, NULL, NULL))
  }
  if (length(x) > 1L && !is.null(trajectory_id)) {
    scans_abort(
      "{.arg trajectory_id} must be {.code NULL} for multiple commons conversations.",
      class = "scans_error_commons_argument",
      call = call
    )
  }
  ellmer_check_installed(call)

  source <- attr(x, "source", exact = TRUE)
  source_attributes <- commons_extra_attributes(x, c("names", "source"))
  bundles <- lapply(seq_along(x), function(index) {
    id <- if (length(x) == 1L) trajectory_id else NULL
    commons_conversation_bundle(
      x[[index]],
      names(x)[[index]],
      id,
      source,
      source_attributes,
      metadata,
      call
    )
  })

  TrajectoryBundle(
    commons_bind_tables(lapply(bundles, trajectory_info)),
    commons_bind_tables(lapply(bundles, trajectory_turns)),
    commons_bind_tables(lapply(bundles, trajectory_events)),
    losses = commons_bind_tables(lapply(bundles, trajectory_losses))
  )
}

commons_check_trajectory_id <- function(x, call) {
  tryCatch(
    rlang::check_string(
      x,
      allow_null = TRUE,
      allow_empty = FALSE,
      call = call
    ),
    error = function(error) {
      scans_abort(
        conditionMessage(error),
        class = "scans_error_commons_argument",
        call = call,
        parent = error
      )
    }
  )
  if (
    !is.null(x) &&
      nchar(x, type = "bytes") > trajectory_payload_max_bytes
  ) {
    scans_abort(
      "{.arg trajectory_id} must not exceed 65,536 bytes.",
      class = "scans_error_commons_argument",
      call = call
    )
  }
  invisible(x)
}

commons_check_source <- function(x, call) {
  if (!is.list(x) || is.data.frame(x)) {
    scans_abort(
      c(
        "{.arg x} must be an already-read commons trajectory list.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_commons_source",
      call = call
    )
  }
  if (length(x) == 0L) {
    return(invisible(x))
  }
  conversation_ids <- names(x)
  valid_names <- !is.null(conversation_ids) &&
    all(!is.na(conversation_ids) & nzchar(trimws(conversation_ids))) &&
    all(
      nchar(conversation_ids, type = "bytes") <= trajectory_payload_max_bytes
    ) &&
    !anyDuplicated(conversation_ids)
  if (!valid_names) {
    scans_abort(
      "The commons trajectory list must have unique, non-empty conversation names.",
      class = "scans_error_commons_source",
      call = call
    )
  }
  valid_conversations <- vapply(x, is.list, logical(1))
  if (!all(valid_conversations)) {
    scans_abort(
      "Each commons conversation must be a list of ellmer turns.",
      class = "scans_error_commons_source",
      call = call
    )
  }
  invisible(x)
}

commons_conversation_bundle <- function(
  conversation,
  conversation_id,
  trajectory_id,
  source,
  source_attributes,
  metadata,
  call
) {
  if (is.null(trajectory_id)) {
    trajectory_id <- paste0(
      "commons/",
      utils::URLencode(conversation_id, reserved = TRUE)
    )
  }
  provenance <- attr(conversation, "provenance", exact = TRUE)
  last_active <- attr(conversation, "last_active", exact = TRUE)
  conversation_attributes <- commons_extra_attributes(
    conversation,
    c("names", "provenance", "last_active")
  )
  valid_turns <- vapply(conversation, ellmer_is_turn, logical(1))
  if (!all(valid_turns)) {
    positions <- which(!valid_turns)
    scans_abort(
      c(
        "The commons conversation contains invalid ellmer turns.",
        "x" = "Invalid position{?s}: {positions}."
      ),
      class = "scans_error_commons_source",
      call = call
    )
  }
  ids <- ellmer_ids(trajectory_id)
  source_metadata <- c(
    metadata,
    list(source = source, provenance = provenance)
  )
  if (length(source_attributes) > 0L) {
    source_metadata$source_attributes <- source_attributes
  }
  if (length(conversation_attributes) > 0L) {
    source_metadata$conversation_attributes <- conversation_attributes
  }

  base <- as_trajectory_ellmer(
    conversation,
    trajectory_id = trajectory_id,
    run_id = conversation_id,
    source_id = conversation_id,
    metadata = list()
  )
  safe_metadata <- ellmer_sanitize_metadata(
    source_metadata,
    "metadata",
    ids
  )
  safe_source <- commons_sanitize_source(safe_metadata$value$source, ids)
  safe_metadata$value$source <- safe_source$value
  rounds <- commons_round_indices(conversation)
  source_losses <- commons_source_losses(source, ids)
  active <- commons_last_active(last_active, ids)
  turns <- trajectory_turns(base)
  turns$round_index <- rounds
  provenance_tables <- commons_provenance_events(
    provenance,
    trajectory_id,
    turns,
    nrow(trajectory_events(base))
  )

  info <- trajectory_info(base)
  info$source_type <- "commons"
  info$completed_at <- active$value
  info$metadata <- list(safe_metadata$value)

  losses <- commons_bind_tables(list(
    trajectory_losses(base),
    ellmer_loss_table(c(
      safe_metadata$losses,
      safe_source$losses,
      source_losses,
      active$losses,
      provenance_tables$losses
    ))
  ))

  TrajectoryBundle(
    info,
    turns,
    commons_bind_tables(list(
      trajectory_events(base),
      provenance_tables$events
    )),
    losses = losses
  )
}

commons_extra_attributes <- function(x, excluded) {
  attributes <- attributes(x)
  attributes[setdiff(names(attributes), excluded)]
}

commons_round_indices <- function(turns) {
  rounds <- rep(NA_integer_, length(turns))
  round <- 0L
  for (index in seq_along(turns)) {
    role <- S7::prop(turns[[index]], "role")
    if (identical(role, "user") && !commons_has_tool_result(turns[[index]])) {
      round <- round + 1L
    }
    if (round > 0L) {
      rounds[[index]] <- round
    }
  }
  rounds
}

commons_has_tool_result <- function(turn) {
  contents <- S7::prop(turn, "contents")
  any(vapply(
    contents,
    function(content) {
      inherits(content, "S7_object") &&
        S7::S7_inherits(content, ellmer::ContentToolResult)
    },
    logical(1)
  ))
}

commons_provenance_events <- function(
  provenance,
  trajectory_id,
  turns,
  offset
) {
  if (is.null(provenance)) {
    return(list(
      events = NULL,
      losses = list(ellmer_new_loss(
        ellmer_ids(trajectory_id),
        "provenance",
        "unsupported",
        "The commons conversation has no provenance records"
      ))
    ))
  }
  if (!is.list(provenance)) {
    return(list(
      events = NULL,
      losses = list(ellmer_new_loss(
        ellmer_ids(trajectory_id),
        "provenance",
        "unsupported",
        "The commons provenance attribute is not a list"
      ))
    ))
  }

  rows <- vector("list", length(provenance))
  losses <- list()
  exchanges <- commons_exchange_count(turns$round_index)
  for (index in seq_along(provenance)) {
    turn_rows <- which(turns$round_index == index)
    turn_id <- if (length(turn_rows) == 0L) {
      NA_character_
    } else {
      turns$turn_id[[turn_rows[[length(turn_rows)]]]]
    }
    event_id <- ellmer_event_id(trajectory_id, offset + index)
    ids <- ellmer_ids(trajectory_id, turn_id, event_id)
    value <- ellmer_sanitize_value(
      provenance[[index]],
      paste0("provenance[[", index, "]]"),
      ids
    )
    losses <- c(
      losses,
      value$losses,
      commons_provenance_record_losses(
        provenance[[index]],
        index,
        exchanges,
        ids
      )
    )
    rows[[index]] <- tibble::tibble(
      trajectory_id = trajectory_id,
      event_id = event_id,
      event_index = as.integer(offset + index),
      turn_id = turn_id,
      content_index = NA_integer_,
      parent_event_id = NA_character_,
      event_type = "commons:provenance",
      content_type = NA_character_,
      name = NA_character_,
      call_id = NA_character_,
      text = NA_character_,
      value = list(value$value),
      timestamp = as.POSIXct(NA, tz = "UTC"),
      duration = NA_real_,
      status = "completed",
      error = NA_character_,
      metadata = list(list(exchange_index = as.integer(index)))
    )
  }
  if (length(provenance) < exchanges) {
    for (index in seq.int(length(provenance) + 1L, exchanges)) {
      losses <- c(
        losses,
        list(ellmer_new_loss(
          ellmer_ids(trajectory_id),
          paste0("provenance[[", index, "]]"),
          "unsupported",
          "The commons exchange has no provenance record"
        ))
      )
    }
  }
  list(events = ellmer_bind_rows(rows), losses = losses)
}

commons_sanitize_source <- function(source, ids) {
  out <- list(value = source, losses = list())
  if (
    trajectory_is_named_list(out$value) &&
      identical(out$value$kind, "connect") &&
      commons_source_string(out$value$server)
  ) {
    server <- ellmer_sanitize_uri(out$value$server, "source$server", ids)
    out$value$server <- server$value
    out$losses <- c(out$losses, server$losses)
  }
  out
}

commons_exchange_count <- function(rounds) {
  known <- rounds[!is.na(rounds)]
  if (length(known) == 0L) 0L else max(known)
}

commons_provenance_record_losses <- function(record, index, exchanges, ids) {
  field <- paste0("provenance[[", index, "]]")
  if (index > exchanges) {
    return(list(ellmer_new_loss(
      ids,
      field,
      "unsupported",
      "The provenance record has no matching commons exchange"
    )))
  }
  if (!trajectory_is_named_list(record)) {
    return(list(ellmer_new_loss(
      ids,
      field,
      "unsupported",
      "The commons provenance record is not a uniquely named list"
    )))
  }

  losses <- list()
  tag <- record$provenance_tag
  valid_tag <- is.character(tag) && length(tag) == 1L && !is.na(tag)
  if (!valid_tag) {
    losses <- c(
      losses,
      list(ellmer_new_loss(
        ids,
        paste0(field, "$provenance_tag"),
        "unsupported",
        "No unambiguous commons provenance tag was available"
      ))
    )
  }
  decisions <- record$citation_decisions
  if (!is.list(decisions) || is.data.frame(decisions)) {
    losses <- c(
      losses,
      list(ellmer_new_loss(
        ids,
        paste0(field, "$citation_decisions"),
        "unsupported",
        "The commons citation decisions are missing or malformed"
      ))
    )
  } else if (
    length(decisions) > 0L &&
      !all(vapply(decisions, trajectory_is_named_list, logical(1)))
  ) {
    losses <- c(
      losses,
      list(ellmer_new_loss(
        ids,
        paste0(field, "$citation_decisions"),
        "unsupported",
        "A commons citation decision is not a uniquely named list"
      ))
    )
  }
  losses
}

commons_source_losses <- function(source, ids) {
  valid <- trajectory_is_named_list(source)
  if (valid) {
    kind <- source$kind
    valid_kind <- is.character(kind) &&
      length(kind) == 1L &&
      !is.na(kind)
    if (valid_kind && identical(kind, "local")) {
      valid <- commons_source_string(source$path) &&
        setequal(names(source), c("kind", "path"))
    } else if (valid_kind && identical(kind, "connect")) {
      valid <- commons_source_string(source$server) &&
        commons_source_string(source$content_guid) &&
        setequal(names(source), c("kind", "server", "content_guid"))
    } else {
      valid <- FALSE
    }
  }
  if (valid) {
    return(list())
  }
  list(ellmer_new_loss(
    ids,
    "source",
    "unsupported",
    "The commons source descriptor is missing, malformed, or conflicting"
  ))
}

commons_source_string <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    nzchar(trimws(x))
}

commons_last_active <- function(x, ids) {
  if (
    !inherits(x, "POSIXct") ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(as.numeric(x))
  ) {
    return(list(
      value = as.POSIXct(NA, tz = "UTC"),
      losses = list(ellmer_new_loss(
        ids,
        "last_active",
        "unsupported",
        "The commons last-active timestamp is missing or malformed"
      ))
    ))
  }
  attr(x, "tzone") <- "UTC"
  list(value = x, losses = list())
}

commons_bind_tables <- function(tables) {
  tables <- Filter(\(table) !is.null(table) && nrow(table) > 0L, tables)
  if (length(tables) == 0L) {
    return(NULL)
  }
  tibble::as_tibble(do.call(rbind, tables))
}
