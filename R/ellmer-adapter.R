#' Convert completed ellmer conversations
#'
#' `as_trajectory_ellmer()` snapshots a completed ellmer `Chat`, a list of
#' exported ellmer turn objects, or one ellmer turn into a canonical
#' [TrajectoryBundle]. It never sends a prompt or retains the live chat,
#' provider, tools, callbacks, or credentials.
#'
#' `as_trajectory()` dispatches to this adapter for ellmer chats, turns, and
#' lists of turns when ellmer is installed.
#'
#' Text, thinking, tool requests, tool results, remote images, inline images,
#' and PDFs receive canonical event representations. Tool results retain the
#' request ID and point to the matching request event when it is present.
#' Provider response JSON is excluded by default, inline binary content is
#' externalized, sensitive named fields are redacted, and unknown content
#' classes produce a namespaced custom event plus an explicit loss row.
#'
#' Ellmer does not provide stable IDs for chats, turns, or content blocks. The
#' adapter therefore generates fixed-width IDs from source order. Re-converting
#' an unchanged snapshot produces the same IDs, while inserting or reordering
#' turns or content may change them.
#'
#' @param x A completed ellmer `Chat`, a list of ellmer turns, or one ellmer
#'   turn.
#' @param trajectory_id A bundle-unique trajectory ID. When `NULL`, the adapter
#'   uses the deterministic snapshot-local ID `"trajectory-000001"`.
#' @param run_id,source_id,task_id,sample_id Optional correlation identities.
#' @param source_uri An optional source locator. Credentials, query parameters,
#'   and fragments are removed.
#' @param epoch An optional positive evaluation epoch.
#' @param agent An optional agent label.
#' @param model An optional model label. For a `Chat`, the public
#'   `get_model()` value is used by default.
#' @param metadata A named list of safe application metadata. Sensitive keys
#'   are redacted and recorded in the bundle's losses table.
#' @param include_system_prompt Whether a `Chat` system prompt should be
#'   included as a system turn.
#'
#' @returns A [TrajectoryBundle].
#' @export
#'
#' @examples
#' if (rlang::is_installed("ellmer", version = "0.4.2")) {
#'   turns <- list(
#'     ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
#'     ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
#'   )
#'
#'   bundle <- as_trajectory_ellmer(turns)
#'   trajectory_events(bundle)
#' }
as_trajectory_ellmer <- function(
  x,
  trajectory_id = NULL,
  run_id = NULL,
  source_id = NULL,
  source_uri = NULL,
  task_id = NULL,
  sample_id = NULL,
  epoch = NULL,
  agent = NULL,
  model = NULL,
  metadata = list(),
  include_system_prompt = TRUE
) {
  call <- rlang::caller_env()
  ellmer_check_installed(call)

  if (is.null(trajectory_id)) {
    trajectory_id <- "trajectory-000001"
  }
  trajectory_id <- ellmer_optional_string(
    trajectory_id,
    "trajectory_id",
    call,
    required = TRUE
  )
  run_id <- ellmer_optional_string(run_id, "run_id", call)
  source_id <- ellmer_optional_string(source_id, "source_id", call)
  source_uri <- ellmer_optional_string(source_uri, "source_uri", call)
  task_id <- ellmer_optional_string(task_id, "task_id", call)
  sample_id <- ellmer_optional_string(sample_id, "sample_id", call)
  epoch <- ellmer_optional_epoch(epoch, call)
  agent <- ellmer_optional_string(agent, "agent", call)
  model <- ellmer_optional_string(model, "model", call)
  metadata <- ellmer_check_metadata(metadata, call)
  include_system_prompt <- ellmer_check_flag(
    include_system_prompt,
    "include_system_prompt",
    call
  )

  source <- ellmer_source_turns(x, include_system_prompt, call)
  if (is.na(model)) {
    model <- source$model
  }

  ids <- ellmer_ids(trajectory_id)
  metadata$ellmer_version <- as.character(utils::packageVersion("ellmer"))
  metadata$source_class <- source$source_class
  safe_metadata <- ellmer_sanitize_metadata(metadata, "metadata", ids)
  safe_uri <- ellmer_sanitize_uri(source_uri, "source_uri", ids)

  tables <- ellmer_turn_tables(source$turns, trajectory_id, call)
  losses <- c(
    safe_metadata$losses,
    safe_uri$losses,
    tables$losses
  )
  status <- if (tables$interrupted) "interrupted" else "completed"

  trajectories <- tibble::tibble(
    trajectory_id = trajectory_id,
    run_id = run_id,
    source_type = "ellmer",
    source_id = source_id,
    source_uri = safe_uri$value,
    task_id = task_id,
    sample_id = sample_id,
    epoch = epoch,
    agent = agent,
    model = model,
    status = status,
    metadata = list(safe_metadata$value)
  )

  TrajectoryBundle(
    trajectories,
    tables$turns,
    tables$events,
    losses = ellmer_loss_table(losses)
  )
}

ellmer_source_turns <- function(x, include_system_prompt, call) {
  if (ellmer_is_chat(x)) {
    turns <- tryCatch(
      x$get_turns(include_system_prompt = include_system_prompt),
      error = function(error) {
        scans_abort(
          c(
            "Can't read turns from the ellmer {.cls Chat}.",
            "x" = conditionMessage(error)
          ),
          class = "scans_error_ellmer_source",
          call = call,
          parent = error
        )
      }
    )
    model <- tryCatch(x$get_model(), error = function(...) NULL)
    model <- ellmer_optional_string(model, "model", call)
    source_class <- "Chat"
  } else if (ellmer_is_turn(x)) {
    turns <- list(x)
    model <- NA_character_
    source_class <- class(x)[[1L]]
  } else if (is.list(x)) {
    turns <- x
    model <- NA_character_
    source_class <- "list"
  } else {
    scans_abort(
      c(
        "{.arg x} must be an ellmer {.cls Chat}, turn, or list of turns.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_ellmer_source",
      call = call
    )
  }

  valid <- vapply(turns, ellmer_is_turn, logical(1))
  if (!all(valid)) {
    positions <- which(!valid)
    scans_abort(
      c(
        "The ellmer source contains invalid turns.",
        "x" = "Elements {.val {positions}} do not inherit from {.cls ellmer::Turn}."
      ),
      class = "scans_error_ellmer_source",
      call = call
    )
  }
  list(turns = turns, model = model, source_class = source_class)
}

ellmer_is_chat <- function(x) {
  inherits(x, "Chat") &&
    inherits(x, "R6") &&
    is.function(x$get_turns) &&
    is.function(x$get_model)
}

ellmer_is_turn <- function(x) {
  inherits(x, "S7_object") && S7::S7_inherits(x, ellmer::Turn)
}

ellmer_is_turn_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0L) {
    return(TRUE)
  }
  any(vapply(x, ellmer_is_turn, logical(1)))
}

ellmer_turn_tables <- function(turns, trajectory_id, call) {
  turn_rows <- vector("list", length(turns))
  event_rows <- list()
  loss_rows <- list()
  call_events <- character()
  event_index <- 0L
  interrupted <- FALSE

  for (turn_index in seq_along(turns)) {
    turn <- turns[[turn_index]]
    turn_id <- ellmer_turn_id(trajectory_id, turn_index)
    ids <- ellmer_ids(trajectory_id, turn_id)
    contents <- S7::prop(turn, "contents")
    turn_error <- ellmer_turn_error(contents, ids)
    partial <- S7::S7_inherits(turn, ellmer::AssistantPartialTurn)
    interrupted <- interrupted || partial
    partial_reason <- if (partial) {
      ellmer_sanitize_text(
        S7::prop(turn, "reason"),
        paste0("turns[[", turn_index, "]]$reason"),
        ids
      )
    } else {
      list(value = NA_character_, losses = list())
    }
    finish_reason <- ellmer_sanitize_text(
      ellmer_prop(turn, "finish_reason", NA_character_),
      paste0("turns[[", turn_index, "]]$finish_reason"),
      ids
    )

    metadata <- if (partial) {
      list(interruption_reason = partial_reason$value)
    } else {
      list()
    }
    safe_metadata <- ellmer_sanitize_metadata(
      metadata,
      paste0("turns[[", turn_index, "]]$metadata"),
      ids
    )
    loss_rows <- c(
      loss_rows,
      partial_reason$losses,
      finish_reason$losses,
      safe_metadata$losses,
      turn_error$losses
    )

    if (S7::prop_exists(turn, "json") && length(S7::prop(turn, "json"))) {
      loss_rows <- c(
        loss_rows,
        list(ellmer_new_loss(
          ids,
          paste0("turns[[", turn_index, "]]$json"),
          "redacted",
          "Provider response JSON is excluded by default"
        ))
      )
    }

    status <- if (partial) {
      partial_reason$value
    } else if (!is.na(turn_error$value)) {
      "failed"
    } else {
      "completed"
    }
    turn_rows[[turn_index]] <- ellmer_turn_row(
      turn,
      trajectory_id,
      turn_id,
      turn_index,
      status,
      turn_error$value,
      finish_reason$value,
      safe_metadata$value
    )

    for (content_index in seq_along(contents)) {
      event_index <- event_index + 1L
      event_id <- ellmer_event_id(trajectory_id, event_index)
      event_ids <- ellmer_ids(trajectory_id, turn_id, event_id)
      mapped <- ellmer_content_event(
        contents[[content_index]],
        event_ids,
        event_index,
        content_index,
        call
      )
      event <- mapped$row

      if (
        identical(event$event_type, "tool_result") &&
          !is.na(event$call_id) &&
          event$call_id %in% names(call_events)
      ) {
        event$parent_event_id <- unname(call_events[[event$call_id]])
      }
      if (
        identical(event$event_type, "tool_call") &&
          !is.na(event$call_id)
      ) {
        call_events[event$call_id] <- event_id
      }

      event_rows[[length(event_rows) + 1L]] <- event
      loss_rows <- c(loss_rows, mapped$losses)
    }
  }

  list(
    turns = ellmer_bind_rows(turn_rows),
    events = ellmer_bind_rows(event_rows),
    losses = loss_rows,
    interrupted = interrupted
  )
}

ellmer_turn_row <- function(
  turn,
  trajectory_id,
  turn_id,
  turn_index,
  status,
  error,
  finish_reason,
  metadata
) {
  tokens <- if (S7::prop_exists(turn, "tokens")) {
    S7::prop(turn, "tokens")
  } else {
    rep(NA_real_, 3L)
  }

  tibble::tibble(
    trajectory_id = trajectory_id,
    turn_id = turn_id,
    turn_index = as.integer(turn_index),
    role = S7::prop(turn, "role"),
    input_tokens = tokens[[1L]],
    output_tokens = tokens[[2L]],
    cached_input_tokens = tokens[[3L]],
    cost = ellmer_prop(turn, "cost", NA_real_),
    duration = ellmer_prop(turn, "duration", NA_real_),
    finish_reason = finish_reason,
    status = status,
    error = error,
    metadata = list(metadata)
  )
}

ellmer_content_event <- function(
  content,
  ids,
  event_index,
  content_index,
  call
) {
  if (
    !inherits(content, "S7_object") ||
      !S7::S7_inherits(content, ellmer::Content)
  ) {
    scans_abort(
      "An ellmer turn contains an invalid content object.",
      class = "scans_error_ellmer_source",
      call = call
    )
  }

  mapped <- if (S7::S7_inherits(content, ellmer::ContentText)) {
    ellmer_text_event(content, ids)
  } else if (S7::S7_inherits(content, ellmer::ContentThinking)) {
    ellmer_thinking_event(content, ids)
  } else if (S7::S7_inherits(content, ellmer::ContentToolRequest)) {
    ellmer_tool_request_event(content, ids)
  } else if (S7::S7_inherits(content, ellmer::ContentToolResult)) {
    ellmer_tool_result_event(content, ids)
  } else if (S7::S7_inherits(content, ellmer::ContentImageRemote)) {
    ellmer_remote_image_event(content, ids)
  } else if (S7::S7_inherits(content, ellmer::ContentImageInline)) {
    ellmer_inline_image_event(content, ids)
  } else if (S7::S7_inherits(content, ellmer::ContentPDF)) {
    ellmer_pdf_event(content, ids)
  } else {
    ellmer_unknown_content_event(content, ids)
  }

  mapped$row$trajectory_id <- ids$trajectory_id
  mapped$row$event_id <- ids$event_id
  mapped$row$event_index <- as.integer(event_index)
  mapped$row$turn_id <- ids$turn_id
  mapped$row$content_index <- as.integer(content_index)
  mapped$row <- mapped$row[c(
    "trajectory_id",
    "event_id",
    "event_index",
    "turn_id",
    "content_index",
    setdiff(
      names(mapped$row),
      c(
        "trajectory_id",
        "event_id",
        "event_index",
        "turn_id",
        "content_index"
      )
    )
  )]
  mapped
}

ellmer_text_event <- function(content, ids) {
  text <- ellmer_sanitize_text(
    S7::prop(content, "text"),
    "contents$text",
    ids
  )
  list(
    row = ellmer_event_row(
      event_type = "content",
      content_type = "text",
      text = text$value
    ),
    losses = text$losses
  )
}

ellmer_thinking_event <- function(content, ids) {
  text <- ellmer_sanitize_text(
    S7::prop(content, "thinking"),
    "contents$thinking",
    ids
  )
  metadata <- ellmer_sanitize_metadata(
    S7::prop(content, "extra"),
    "contents$extra",
    ids
  )
  list(
    row = ellmer_event_row(
      event_type = "content",
      content_type = "thinking",
      text = text$value,
      metadata = metadata$value
    ),
    losses = c(text$losses, metadata$losses)
  )
}

ellmer_tool_request_event <- function(content, ids) {
  name <- ellmer_sanitize_text(
    S7::prop(content, "name"),
    "contents$name",
    ids
  )
  call_id <- ellmer_sanitize_text(
    S7::prop(content, "id"),
    "contents$id",
    ids
  )
  arguments <- ellmer_sanitize_value(
    S7::prop(content, "arguments"),
    "contents$arguments",
    ids
  )
  metadata <- ellmer_sanitize_metadata(
    S7::prop(content, "extra"),
    "contents$extra",
    ids
  )
  losses <- c(
    name$losses,
    call_id$losses,
    arguments$losses,
    metadata$losses
  )
  if (!is.null(S7::prop(content, "tool"))) {
    losses <- c(
      losses,
      list(ellmer_new_loss(
        ids,
        "contents$tool",
        "unsupported",
        "The live ellmer tool definition was not retained"
      ))
    )
  }

  list(
    row = ellmer_event_row(
      event_type = "tool_call",
      name = name$value,
      call_id = call_id$value,
      value = arguments$value,
      metadata = metadata$value
    ),
    losses = losses
  )
}

ellmer_tool_result_event <- function(content, ids) {
  value <- ellmer_sanitize_value(
    S7::prop(content, "value"),
    "contents$value",
    ids
  )
  metadata <- ellmer_sanitize_metadata(
    S7::prop(content, "extra"),
    "contents$extra",
    ids
  )
  error <- ellmer_error_text(S7::prop(content, "error"), ids)
  request <- S7::prop(content, "request")
  has_request <- !is.null(request) &&
    inherits(request, "S7_object") &&
    S7::S7_inherits(request, ellmer::ContentToolRequest)
  call_id <- if (has_request) {
    ellmer_sanitize_text(
      S7::prop(request, "id"),
      "contents$request$id",
      ids
    )
  } else {
    list(value = NA_character_, losses = list())
  }
  name <- if (has_request) {
    ellmer_sanitize_text(
      S7::prop(request, "name"),
      "contents$request$name",
      ids
    )
  } else {
    list(value = NA_character_, losses = list())
  }
  text <- if (is.character(value$value) && length(value$value) == 1L) {
    value$value
  } else {
    NA_character_
  }

  list(
    row = ellmer_event_row(
      event_type = "tool_result",
      name = name$value,
      call_id = call_id$value,
      text = text,
      value = value$value,
      status = if (is.na(error$value)) "completed" else "failed",
      error = error$value,
      metadata = metadata$value
    ),
    losses = c(
      value$losses,
      metadata$losses,
      error$losses,
      call_id$losses,
      name$losses
    )
  )
}

ellmer_remote_image_event <- function(content, ids) {
  uri <- ellmer_sanitize_uri(
    S7::prop(content, "url"),
    "contents$url",
    ids
  )
  detail <- ellmer_sanitize_text(
    S7::prop(content, "detail"),
    "contents$detail",
    ids
  )
  list(
    row = ellmer_event_row(
      event_type = "content",
      content_type = "image",
      value = list(url = uri$value, detail = detail$value)
    ),
    losses = c(uri$losses, detail$losses)
  )
}

ellmer_inline_image_event <- function(content, ids) {
  data <- S7::prop(content, "data")
  type <- ellmer_sanitize_text(
    S7::prop(content, "type"),
    "contents$type",
    ids
  )
  size <- if (is.null(data)) 0 else nchar(data, type = "bytes")
  losses <- type$losses
  if (!is.null(data)) {
    losses <- c(
      losses,
      list(ellmer_new_loss(
        ids,
        "contents$data",
        "externalized",
        "Inline image data was not embedded"
      ))
    )
  }
  list(
    row = ellmer_event_row(
      event_type = "content",
      content_type = "image",
      value = list(
        mime_type = type$value,
        encoding = "base64",
        size_bytes = size
      )
    ),
    losses = losses
  )
}

ellmer_pdf_event <- function(content, ids) {
  data <- S7::prop(content, "data")
  type <- ellmer_sanitize_text(
    S7::prop(content, "type"),
    "contents$type",
    ids
  )
  filename <- ellmer_sanitize_text(
    S7::prop(content, "filename"),
    "contents$filename",
    ids
  )
  size <- nchar(data, type = "bytes")
  list(
    row = ellmer_event_row(
      event_type = "content",
      content_type = "pdf",
      value = list(
        mime_type = type$value,
        filename = filename$value,
        encoding = "base64",
        size_bytes = size
      )
    ),
    losses = c(
      type$losses,
      filename$losses,
      list(ellmer_new_loss(
        ids,
        "contents$data",
        "externalized",
        "Inline PDF data was not embedded"
      ))
    )
  )
}

ellmer_unknown_content_event <- function(content, ids) {
  source_class <- class(content)[[1L]]
  kind <- sub("^.*::", "", source_class)
  kind <- tolower(gsub("[^A-Za-z0-9._-]", "_", kind))
  if (!grepl("^[a-z]", kind)) {
    kind <- paste0("content_", kind)
  }

  list(
    row = ellmer_event_row(
      event_type = "custom",
      content_type = paste0("ellmer:", kind),
      value = list(),
      metadata = list(source_class = source_class)
    ),
    losses = list(ellmer_new_loss(
      ids,
      "contents",
      "unsupported",
      "An unsupported ellmer content class was not retained",
      metadata = list(source_class = source_class)
    ))
  )
}

ellmer_event_row <- function(
  event_type,
  content_type = NA_character_,
  name = NA_character_,
  call_id = NA_character_,
  text = NA_character_,
  value = NULL,
  status = "completed",
  error = NA_character_,
  metadata = list()
) {
  tibble::tibble(
    parent_event_id = NA_character_,
    event_type = event_type,
    content_type = content_type,
    name = name,
    call_id = call_id,
    text = text,
    value = list(value),
    status = status,
    error = error,
    metadata = list(metadata)
  )
}

ellmer_turn_error <- function(contents, ids) {
  errors <- lapply(contents, function(content) {
    if (
      inherits(content, "S7_object") &&
        S7::S7_inherits(content, ellmer::ContentToolResult)
    ) {
      S7::prop(content, "error")
    } else {
      NULL
    }
  })
  errors <- Filter(Negate(is.null), errors)
  if (length(errors) == 0L) {
    return(list(value = NA_character_, losses = list()))
  }
  ellmer_error_text(errors[[1L]], ids)
}

ellmer_error_text <- function(error, ids) {
  if (is.null(error)) {
    return(list(value = NA_character_, losses = list()))
  }
  text <- if (inherits(error, "condition")) {
    conditionMessage(error)
  } else if (is.character(error)) {
    paste(error, collapse = "\n")
  } else {
    "Tool execution failed"
  }
  ellmer_sanitize_text(text, "contents$error", ids)
}

ellmer_prop <- function(x, name, default) {
  if (S7::prop_exists(x, name)) S7::prop(x, name) else default
}

ellmer_bind_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(tibble::tibble())
  }
  do.call(rbind, rows)
}

ellmer_ids <- function(
  trajectory_id,
  turn_id = NA_character_,
  event_id = NA_character_
) {
  list(
    trajectory_id = trajectory_id,
    turn_id = turn_id,
    event_id = event_id
  )
}

ellmer_turn_id <- function(trajectory_id, index) {
  sprintf("%s/turn-%06d", trajectory_id, index)
}

ellmer_event_id <- function(trajectory_id, index) {
  sprintf("%s/event-%06d", trajectory_id, index)
}
