#' Convert completed commons conversations
#'
#' `as_trajectory_commons()` converts the already-read result of
#' `commons::trajectory_read()` into one canonical [TrajectoryBundle]. Each
#' named conversation becomes one trajectory. Both current records with `turns`,
#' `last_active`, and `user` fields and older attributed turn lists are
#' accepted. Model-visible turns are delegated to [as_trajectory_ellmer()]. The
#' adapter does not read files, connect to Posit Connect, or parse trace records
#' itself.
#'
#' Provenance stays attached to the turn list. Each source provenance record
#' becomes a `"commons:provenance"` event whose `exchange_index` matches the
#' `round_index` of its turns. Rounds follow commons' exchange rule: a user
#' turn that is neither a tool result nor an internal reminder opens a round.
#' Internal reminder turns, which commons injects as hidden follow-ups, belong
#' to the round they follow, keep their turn rows, and are marked with
#' `commons_internal = TRUE` in turn metadata. Because the public commons
#' result does not expose an exchange-to-turn key, provenance events do not
#' infer a `turn_id`.
#'
#' The conversation `user`, the Posit Connect user recorded by commons, is
#' retained as `metadata$otel$user`, the slot [as_trajectory_otel()] uses for
#' the same span-derived identity. A missing user (`NA`) is not a loss.
#'
#' Commons substitutes placeholder turns when a trace omitted input history or
#' tool results. Those turns are kept as recorded, and each one is reported as
#' a `capture$input` or `capture$tool_result` loss on that turn, so
#' [assess_trajectory_scans()] does not treat the transcript as complete.
#'
#' Current commons results carry no outer source descriptor, and its absence
#' is not a loss. An outer `source` attribute supplied by an older reader or by
#' the caller is retained as sanitized metadata, and a malformed or conflicting
#' one is reported as a loss. Missing or malformed provenance and activity
#' times are adapter losses. Extra record fields and turn attributes are
#' retained as sanitized metadata.
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
#' @param source_uri An optional locator for where the conversations were read
#'   from, applied to every conversation. Credentials, query parameters, and
#'   fragments are removed.
#' @param metadata A uniquely named list of safe application metadata, applied
#'   to every conversation. Sensitive fields are redacted and recorded as
#'   losses. Names used by commons source metadata are reserved.
#'
#' @returns A [TrajectoryBundle].
#' @export
#'
#' @examples
#' if (rlang::is_installed("ellmer", version = "0.5.0")) {
#'   turns <- list(
#'     ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
#'     ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
#'   )
#'   attr(turns, "provenance") <- list(list(
#'     provenance_tag = "A",
#'     citation_decisions = list()
#'   ))
#'   conversation <- list(
#'     turns = turns,
#'     last_active = as.POSIXct("2026-08-23 12:00:00", tz = "UTC"),
#'     user = "analyst"
#'   )
#'
#'   as_trajectory_commons(
#'     list(`conversation-001` = conversation),
#'     source_uri = "file:///traces"
#'   )
#' }
as_trajectory_commons <- function(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
) {
  call <- rlang::caller_env()
  commons_check_string(trajectory_id, "trajectory_id", call)
  commons_check_string(source_uri, "source_uri", call)
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
      "otel",
      "source_attributes",
      "conversation_attributes",
      "conversation_fields",
      "turn_attributes"
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
      source_uri,
      source,
      source_attributes,
      metadata,
      call
    )
  })

  TrajectoryBundle(
    trajectory_bind_rows(lapply(bundles, trajectory_info)),
    trajectory_bind_rows(lapply(bundles, trajectory_turns)),
    trajectory_bind_rows(lapply(bundles, trajectory_events)),
    losses = trajectory_bind_rows(lapply(bundles, trajectory_losses))
  )
}

commons_check_string <- function(x, arg, call) {
  tryCatch(
    rlang::check_string(
      x,
      allow_null = TRUE,
      allow_empty = FALSE,
      arg = arg,
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
      "{.arg {arg}} must not exceed 65,536 bytes.",
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
  source_uri,
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
  record <- "turns" %in%
    names(conversation) &&
    !ellmer_is_turn(conversation[["turns"]])
  conversation_fields <- list()
  turn_attributes <- list()
  user <- NULL
  if (record) {
    if (
      !trajectory_is_named_list(conversation) ||
        !is.list(conversation$turns) ||
        is.data.frame(conversation$turns)
    ) {
      scans_abort(
        "A commons conversation record must contain a list of ellmer turns in {.field turns} and have unique field names.",
        class = "scans_error_commons_source",
        call = call
      )
    }
    last_active <- conversation[["last_active"]]
    user <- conversation[["user"]]
    conversation_fields <- conversation[setdiff(
      names(conversation),
      c("turns", "last_active", "user")
    )]
    conversation_attributes <- commons_extra_attributes(conversation, "names")
    conversation <- conversation$turns
    turn_attributes <- commons_extra_attributes(
      conversation,
      c("names", "provenance")
    )
  } else {
    last_active <- attr(conversation, "last_active", exact = TRUE)
    conversation_attributes <- commons_extra_attributes(
      conversation,
      c("names", "provenance", "last_active")
    )
  }
  provenance <- attr(conversation, "provenance", exact = TRUE)
  valid_turns <- vapply(conversation, ellmer_is_turn, logical(1))
  if (!all(valid_turns)) {
    positions <- which(!valid_turns)
    scans_abort(
      c(
        "The commons conversation contains invalid ellmer turns.",
        "x" = "Invalid turn positions: {.val {positions}}."
      ),
      class = "scans_error_commons_source",
      call = call
    )
  }
  ids <- trajectory_ids(trajectory_id)
  safe_user <- commons_user(user, ids)
  source_metadata <- c(
    metadata,
    list(source = source, provenance = provenance)
  )
  if (!is.null(safe_user$value)) {
    source_metadata$otel <- list(user = safe_user$value)
  }
  if (length(source_attributes) > 0L) {
    source_metadata$source_attributes <- source_attributes
  }
  if (length(conversation_attributes) > 0L) {
    source_metadata$conversation_attributes <- conversation_attributes
  }

  if (length(conversation_fields) > 0L) {
    source_metadata$conversation_fields <- conversation_fields
  }
  if (length(turn_attributes) > 0L) {
    source_metadata$turn_attributes <- turn_attributes
  }

  base <- as_trajectory_ellmer(
    conversation,
    trajectory_id = trajectory_id,
    run_id = conversation_id,
    source_id = conversation_id,
    source_uri = source_uri,
    metadata = list()
  )
  safe_metadata <- trajectory_sanitize_metadata(
    source_metadata,
    "metadata",
    ids
  )
  safe_source <- commons_sanitize_source(safe_metadata$value$source, ids)
  safe_metadata$value$source <- safe_source$value
  source_losses <- commons_source_losses(source, ids)
  active <- commons_last_active(last_active, ids)
  turns <- trajectory_turns(base)
  turns$round_index <- commons_round_indices(conversation)
  internal <- vapply(conversation, commons_is_internal_turn, logical(1))
  turns$metadata[internal] <- lapply(
    turns$metadata[internal],
    function(metadata) c(metadata, list(commons_internal = TRUE))
  )
  provenance_tables <- commons_provenance_events(
    provenance,
    trajectory_id,
    nrow(trajectory_events(base))
  )

  info <- trajectory_info(base)
  info$source_type <- "commons"
  info$completed_at <- active$value
  info$metadata <- list(safe_metadata$value)

  losses <- trajectory_bind_rows(list(
    trajectory_losses(base),
    trajectory_loss_table(c(
      safe_metadata$losses,
      safe_source$losses,
      source_losses,
      safe_user$losses,
      active$losses,
      commons_placeholder_losses(conversation, turns$turn_id, trajectory_id),
      provenance_tables$losses
    ))
  ))

  TrajectoryBundle(
    info,
    turns,
    trajectory_bind_rows(list(
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

# Mirrors commons' split_exchanges(): a user turn opens an exchange unless it
# carries a tool result or is an internal reminder. Other turns, including
# internal reminders, join the open exchange; turns before the first exchange
# have no round.
commons_round_indices <- function(turns) {
  rounds <- rep(NA_integer_, length(turns))
  round <- 0L
  for (index in seq_along(turns)) {
    turn <- turns[[index]]
    if (
      identical(S7::prop(turn, "role"), "user") &&
        !commons_has_tool_result(turn) &&
        !commons_is_internal_turn(turn)
    ) {
      round <- round + 1L
    }
    if (round > 0L) {
      rounds[[index]] <- round
    }
  }
  rounds
}

# The rule from commons' is_internal_turn() in pkg-r/R/trajectory-read.R:
# a user turn whose every content is text wrapped in one <system-instruction>
# or legacy <reminder> element. Recorded traces keep only the text.
commons_is_internal_turn <- function(turn) {
  identical(S7::prop(turn, "role"), "user") &&
    all(vapply(
      S7::prop(turn, "contents"),
      function(content) {
        S7::S7_inherits(content, ellmer::ContentText) &&
          grepl(
            "(?s)^<(system-instruction|reminder)>.*</\\1>$",
            S7::prop(content, "text"),
            perl = TRUE
          )
      },
      logical(1)
    ))
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

# Placeholders written by commons' missing_input_turn() and
# missing_tool_results() in pkg-r/R/trajectory-read.R.
commons_missing_input <- "(Input content was not captured.)"
commons_missing_tool_result <- "(Tool result was not captured.)"

commons_placeholder_losses <- function(turns, turn_ids, trajectory_id) {
  losses <- list()
  for (index in seq_along(turns)) {
    turn <- turns[[index]]
    if (!identical(S7::prop(turn, "role"), "user")) {
      next
    }
    ids <- trajectory_ids(trajectory_id, turn_ids[[index]])
    contents <- S7::prop(turn, "contents")
    missing_input <- length(contents) == 1L &&
      S7::S7_inherits(contents[[1L]], ellmer::ContentText) &&
      identical(S7::prop(contents[[1L]], "text"), commons_missing_input)
    if (missing_input) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          "capture$input",
          "unsupported",
          "The trace omitted this input; commons substituted a placeholder turn"
        ))
      )
    }
    missing_results <- vapply(
      contents,
      function(content) {
        S7::S7_inherits(content, ellmer::ContentToolResult) &&
          identical(S7::prop(content, "value"), commons_missing_tool_result)
      },
      logical(1)
    )
    if (any(missing_results)) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          "capture$tool_result",
          "unsupported",
          "The trace omitted tool results; commons substituted placeholders",
          metadata = list(content_index = which(missing_results))
        ))
      )
    }
  }
  losses
}

commons_user <- function(user, ids) {
  if (is.null(user) || identical(user, NA_character_)) {
    return(list(value = NULL, losses = list()))
  }
  if (is.character(user) && !is.na(otel_string(user))) {
    return(list(value = user, losses = list()))
  }
  list(
    value = NULL,
    losses = list(trajectory_new_loss(
      ids,
      "user",
      "unsupported",
      "The commons conversation user is malformed"
    ))
  )
}

commons_provenance_events <- function(
  provenance,
  trajectory_id,
  offset
) {
  if (is.null(provenance)) {
    return(list(
      events = NULL,
      losses = list(trajectory_new_loss(
        trajectory_ids(trajectory_id),
        "provenance",
        "unsupported",
        "The commons conversation has no provenance records"
      ))
    ))
  }
  if (!is.list(provenance)) {
    return(list(
      events = NULL,
      losses = list(trajectory_new_loss(
        trajectory_ids(trajectory_id),
        "provenance",
        "unsupported",
        "The commons provenance attribute is not a list"
      ))
    ))
  }

  rows <- vector("list", length(provenance))
  losses <- list()
  for (index in seq_along(provenance)) {
    event_id <- trajectory_event_id(trajectory_id, offset + index)
    ids <- trajectory_ids(trajectory_id, event_id = event_id)
    value <- trajectory_sanitize_value(
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
        ids
      )
    )
    rows[[index]] <- tibble::tibble(
      trajectory_id = trajectory_id,
      event_id = event_id,
      event_index = as.integer(offset + index),
      turn_id = NA_character_,
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
  list(events = trajectory_bind_rows(rows), losses = losses)
}

commons_sanitize_source <- function(source, ids) {
  out <- list(value = source, losses = list())
  if (
    trajectory_is_named_list(out$value) &&
      identical(out$value$kind, "connect") &&
      commons_source_string(out$value$server)
  ) {
    server <- trajectory_sanitize_uri(out$value$server, "source$server", ids)
    out$value$server <- server$value
    out$losses <- c(out$losses, server$losses)
  }
  out
}

commons_provenance_record_losses <- function(record, index, ids) {
  field <- paste0("provenance[[", index, "]]")
  if (!trajectory_is_named_list(record)) {
    return(list(trajectory_new_loss(
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
      list(trajectory_new_loss(
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
      list(trajectory_new_loss(
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
      list(trajectory_new_loss(
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
  if (is.null(source)) {
    return(list())
  }
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
  list(trajectory_new_loss(
    ids,
    "source",
    "unsupported",
    "The commons source descriptor is malformed or conflicting"
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
      losses = list(trajectory_new_loss(
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
