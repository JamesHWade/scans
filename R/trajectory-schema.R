trajectory_schema_version <- 1L
trajectory_payload_max_bytes <- 65536L

trajectory_table_schemas <- function() {
  datetime <- as.POSIXct(character(), tz = "UTC")

  list(
    trajectories = list(
      trajectory_id = character(),
      run_id = character(),
      parent_trajectory_id = character(),
      source_type = character(),
      source_id = character(),
      source_uri = character(),
      task_id = character(),
      sample_id = character(),
      epoch = integer(),
      agent = character(),
      model = character(),
      started_at = datetime,
      completed_at = datetime,
      status = character(),
      error = character(),
      metadata = list()
    ),
    turns = list(
      trajectory_id = character(),
      turn_id = character(),
      turn_index = integer(),
      round_index = integer(),
      role = character(),
      input_tokens = double(),
      output_tokens = double(),
      cached_input_tokens = double(),
      cost = double(),
      duration = double(),
      finish_reason = character(),
      status = character(),
      error = character(),
      metadata = list()
    ),
    events = list(
      trajectory_id = character(),
      event_id = character(),
      event_index = integer(),
      turn_id = character(),
      content_index = integer(),
      parent_event_id = character(),
      event_type = character(),
      content_type = character(),
      name = character(),
      call_id = character(),
      text = character(),
      value = list(),
      timestamp = datetime,
      duration = double(),
      status = character(),
      error = character(),
      metadata = list()
    ),
    evaluations = list(
      trajectory_id = character(),
      evaluation_id = character(),
      task_id = character(),
      sample_id = character(),
      epoch = integer(),
      scorer = character(),
      value = list(),
      target = list(),
      explanation = character(),
      metadata = list()
    ),
    losses = list(
      trajectory_id = character(),
      turn_id = character(),
      event_id = character(),
      field = character(),
      reason = character(),
      detail = character(),
      metadata = list()
    )
  )
}

trajectory_required_input_columns <- function(table) {
  switch(
    table,
    trajectories = c("trajectory_id", "source_type"),
    turns = c("trajectory_id", "turn_id", "turn_index", "role"),
    events = c(
      "trajectory_id",
      "event_id",
      "event_index",
      "event_type"
    ),
    evaluations = c("trajectory_id", "evaluation_id"),
    losses = c("field", "reason", "detail")
  )
}

normalize_trajectory_table <- function(x, table, call) {
  schema <- trajectory_table_schemas()[[table]]

  if (is.null(x)) {
    x <- data.frame()
  }
  if (!is.data.frame(x)) {
    trajectory_schema_abort(
      c(
        "{.arg {table}} must be a data frame.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  if (anyDuplicated(names(x))) {
    trajectory_schema_abort(
      "{.arg {table}} must have unique column names.",
      call = call
    )
  }

  out <- tibble::as_tibble(x, .name_repair = "minimal")
  required <- trajectory_required_input_columns(table)
  missing <- setdiff(required, names(out))
  if (nrow(out) > 0L && length(missing) > 0L) {
    trajectory_schema_abort(
      c(
        "{.arg {table}} is missing required columns.",
        "x" = "Missing: {.field {missing}}."
      ),
      call = call
    )
  }

  for (column in setdiff(names(schema), names(out))) {
    out[[column]] <- trajectory_default_column(
      table,
      column,
      schema[[column]],
      nrow(out)
    )
  }
  for (column in names(schema)) {
    out[[column]] <- trajectory_cast_column(
      out[[column]],
      schema[[column]],
      table,
      column,
      call
    )
  }

  canonical <- names(schema)
  out[c(canonical, setdiff(names(out), canonical))]
}

trajectory_default_column <- function(table, column, prototype, size) {
  if (identical(column, "metadata")) {
    return(rep(list(list()), size))
  }
  if (is.list(prototype)) {
    return(rep(list(NULL), size))
  }
  prototype[rep(NA_integer_, size)]
}

trajectory_cast_column <- function(
  x,
  prototype,
  table,
  column,
  call
) {
  type <- trajectory_prototype_type(prototype)
  valid <- TRUE

  if (length(x) == 0L) {
    return(prototype)
  }
  if (is.logical(x) && all(is.na(x))) {
    return(trajectory_default_column(table, column, prototype, length(x)))
  }

  out <- switch(
    type,
    character = {
      valid <- is.character(x) || is.factor(x)
      if (valid) as.character(x) else x
    },
    integer = {
      valid <- is.numeric(x) &&
        all(
          is.na(x) |
            (is.finite(x) &
              x == floor(x) &
              x >= -.Machine$integer.max - 1 &
              x <= .Machine$integer.max)
        )
      if (valid) as.integer(x) else x
    },
    double = {
      valid <- is.numeric(x)
      if (valid) as.double(x) else x
    },
    POSIXct = {
      valid <- inherits(x, "POSIXct")
      if (valid) {
        attr(x, "tzone") <- "UTC"
      }
      x
    },
    list = {
      valid <- is.list(x) && !is.data.frame(x)
      x
    }
  )

  if (!valid) {
    trajectory_schema_abort(
      c(
        "{.arg {table}} has an invalid column type.",
        "x" = "{.field {column}} must be {trajectory_type_label(type)}, not {.obj_type_friendly {x}}."
      ),
      call = call
    )
  }
  out
}

trajectory_prototype_type <- function(x) {
  if (inherits(x, "POSIXct")) {
    return("POSIXct")
  }
  typeof(x)
}

trajectory_type_label <- function(type) {
  switch(
    type,
    character = "a character vector",
    integer = "an integer vector",
    double = "a numeric vector",
    POSIXct = "a POSIXct vector",
    list = "a list"
  )
}

trajectory_schema_abort <- function(
  message,
  ...,
  call,
  .envir = rlang::caller_env()
) {
  scans_abort(
    message,
    ...,
    class = "scans_error_trajectory_schema",
    call = call,
    .envir = .envir
  )
}

trajectory_bundle_validation_problems <- function(self) {
  data <- S7::props(self)
  trajectory_bundle_data_validation_problems(data)
}

trajectory_bundle_data_validation_problems <- function(data) {
  schemas <- trajectory_table_schemas()
  problems <- character()

  if (!identical(data$schema_version, trajectory_schema_version)) {
    problems <- c(problems, "@schema_version must be 1L")
  }
  for (table in names(schemas)) {
    problems <- c(
      problems,
      trajectory_table_structure_problems(
        data[[table]],
        schemas[[table]],
        table
      )
    )
  }
  if (length(problems) > 0L) {
    return(unique(problems))
  }

  problems <- c(
    problems,
    trajectory_identity_problems(data),
    trajectory_order_problems(data),
    trajectory_reference_problems(data),
    trajectory_vocabulary_problems(data),
    trajectory_value_problems(data)
  )

  problems <- unique(problems)
  if (length(problems) == 0L) NULL else problems
}

trajectory_table_structure_problems <- function(x, schema, table) {
  label <- paste0("@", table)
  if (!inherits(x, "tbl_df")) {
    return(paste0(label, " must be a tibble"))
  }
  missing <- setdiff(names(schema), names(x))
  if (length(missing) > 0L) {
    return(paste0(
      label,
      " is missing canonical columns: ",
      paste(missing, collapse = ", ")
    ))
  }

  problems <- character()
  for (column in names(schema)) {
    expected <- trajectory_prototype_type(schema[[column]])
    observed <- trajectory_prototype_type(x[[column]])
    if (!identical(observed, expected)) {
      problems <- c(
        problems,
        paste0(label, "$", column, " must be ", expected)
      )
    }
  }
  problems
}

trajectory_identity_problems <- function(data) {
  problems <- character()
  identities <- list(
    trajectories = "trajectory_id",
    turns = "turn_id",
    events = "event_id",
    evaluations = "evaluation_id"
  )

  for (table in names(identities)) {
    column <- identities[[table]]
    value <- data[[table]][[column]]
    label <- paste0("@", table, "$", column)
    if (any(is.na(value) | !nzchar(trimws(value)))) {
      problems <- c(problems, paste0(label, " must not be missing or empty"))
    }
    if (anyDuplicated(value)) {
      problems <- c(problems, paste0(label, " must be unique"))
    }
  }

  required_text <- list(
    trajectories = "source_type",
    turns = "role",
    events = "event_type",
    losses = c("field", "reason", "detail")
  )
  for (table in names(required_text)) {
    for (column in required_text[[table]]) {
      value <- data[[table]][[column]]
      if (any(is.na(value) | !nzchar(trimws(value)))) {
        problems <- c(
          problems,
          paste0("@", table, "$", column, " must not be missing or empty")
        )
      }
    }
  }

  optional_ids <- list(
    trajectories = c(
      "run_id",
      "parent_trajectory_id",
      "source_id",
      "task_id",
      "sample_id"
    ),
    events = c("turn_id", "parent_event_id", "call_id"),
    evaluations = c("task_id", "sample_id"),
    losses = c("trajectory_id", "turn_id", "event_id")
  )
  for (table in names(optional_ids)) {
    for (column in optional_ids[[table]]) {
      value <- data[[table]][[column]]
      if (any(!is.na(value) & !nzchar(trimws(value)))) {
        problems <- c(
          problems,
          paste0("@", table, "$", column, " must be missing or non-empty")
        )
      }
    }
  }
  problems
}

trajectory_order_problems <- function(data) {
  problems <- character()
  required_indices <- list(
    turns = c("trajectory_id", "turn_index"),
    events = c("trajectory_id", "event_index")
  )
  for (table in names(required_indices)) {
    parent <- required_indices[[table]][[1]]
    index <- required_indices[[table]][[2]]
    value <- data[[table]][[index]]
    if (any(is.na(value) | value < 1L)) {
      problems <- c(
        problems,
        paste0("@", table, "$", index, " must contain positive integers")
      )
    }
    key <- data[[table]][c(parent, index)]
    if (anyDuplicated(key) > 0L) {
      problems <- c(
        problems,
        paste0("@", table, "$", index, " must be unique within ", parent)
      )
    }
  }

  optional_indices <- list(
    trajectories = "epoch",
    turns = "round_index",
    events = "content_index",
    evaluations = "epoch"
  )
  for (table in names(optional_indices)) {
    column <- optional_indices[[table]]
    value <- data[[table]][[column]]
    if (any(!is.na(value) & value < 1L)) {
      problems <- c(
        problems,
        paste0("@", table, "$", column, " must be missing or positive")
      )
    }
  }
  event_content <- data$events[!is.na(data$events$content_index), ]
  if (anyNA(event_content$turn_id)) {
    problems <- c(problems, "@events$content_index requires turn_id")
  }
  if (anyDuplicated(event_content[c("turn_id", "content_index")]) > 0L) {
    problems <- c(
      problems,
      "@events$content_index must be unique within turn_id"
    )
  }

  nonnegative <- list(
    turns = c(
      "input_tokens",
      "output_tokens",
      "cached_input_tokens",
      "cost",
      "duration"
    ),
    events = "duration"
  )
  for (table in names(nonnegative)) {
    for (column in nonnegative[[table]]) {
      value <- data[[table]][[column]]
      if (any(!is.na(value) & (!is.finite(value) | value < 0))) {
        problems <- c(
          problems,
          paste0("@", table, "$", column, " must be missing or nonnegative")
        )
      }
    }
  }

  known_starts <- !is.na(data$trajectories$started_at)
  known_completions <- !is.na(data$trajectories$completed_at)
  if (
    any(
      known_starts & !is.finite(as.numeric(data$trajectories$started_at))
    )
  ) {
    problems <- c(problems, "@trajectories$started_at must be finite")
  }
  if (
    any(
      known_completions &
        !is.finite(as.numeric(data$trajectories$completed_at))
    )
  ) {
    problems <- c(problems, "@trajectories$completed_at must be finite")
  }

  known_bounds <- known_starts & known_completions
  if (
    any(
      data$trajectories$completed_at[known_bounds] <
        data$trajectories$started_at[known_bounds]
    )
  ) {
    problems <- c(
      problems,
      "@trajectories$completed_at must not precede started_at"
    )
  }
  known_event_times <- !is.na(data$events$timestamp)
  if (
    any(
      known_event_times & !is.finite(as.numeric(data$events$timestamp))
    )
  ) {
    problems <- c(problems, "@events$timestamp must be finite")
  }
  problems
}

trajectory_reference_problems <- function(data) {
  problems <- character()
  trajectory_ids <- data$trajectories$trajectory_id

  for (table in c("turns", "events", "evaluations")) {
    references <- data[[table]]$trajectory_id
    if (!all(references %in% trajectory_ids)) {
      problems <- c(
        problems,
        paste0("@", table, "$trajectory_id must reference @trajectories")
      )
    }
  }
  loss_trajectories <- data$losses$trajectory_id
  if (
    any(
      !is.na(loss_trajectories) & !loss_trajectories %in% trajectory_ids
    )
  ) {
    problems <- c(
      problems,
      "@losses$trajectory_id must reference @trajectories when present"
    )
  }

  parent_trajectory <- data$trajectories$parent_trajectory_id
  if (
    any(
      !is.na(parent_trajectory) & !parent_trajectory %in% trajectory_ids
    )
  ) {
    problems <- c(
      problems,
      "@trajectories$parent_trajectory_id must reference @trajectories"
    )
  } else if (
    trajectory_reference_has_cycle(trajectory_ids, parent_trajectory)
  ) {
    problems <- c(
      problems,
      "@trajectories$parent_trajectory_id must not form a cycle"
    )
  }

  event_turns <- data$events$turn_id
  turn_match <- match(event_turns, data$turns$turn_id)
  known_turns <- !is.na(event_turns)
  if (any(known_turns & is.na(turn_match))) {
    problems <- c(
      problems,
      "@events$turn_id must reference @turns when present"
    )
  } else if (
    any(
      known_turns &
        data$events$trajectory_id != data$turns$trajectory_id[turn_match]
    )
  ) {
    problems <- c(
      problems,
      "@events$turn_id must reference a turn in the same trajectory"
    )
  }

  parent_event <- data$events$parent_event_id
  event_match <- match(parent_event, data$events$event_id)
  known_events <- !is.na(parent_event)
  if (any(known_events & is.na(event_match))) {
    problems <- c(
      problems,
      "@events$parent_event_id must reference @events when present"
    )
  } else if (
    any(
      known_events &
        data$events$trajectory_id != data$events$trajectory_id[event_match]
    )
  ) {
    problems <- c(
      problems,
      "@events$parent_event_id must reference an event in the same trajectory"
    )
  } else if (
    trajectory_reference_has_cycle(data$events$event_id, parent_event)
  ) {
    problems <- c(
      problems,
      "@events$parent_event_id must not form a cycle"
    )
  }

  problems <- c(
    problems,
    trajectory_loss_reference_problems(data$losses, data$turns, data$events)
  )
  problems
}

trajectory_loss_reference_problems <- function(losses, turns, events) {
  problems <- character()
  turn_match <- match(losses$turn_id, turns$turn_id)
  known_turns <- !is.na(losses$turn_id)
  if (any(known_turns & is.na(turn_match))) {
    problems <- c(problems, "@losses$turn_id must reference @turns")
  }

  event_match <- match(losses$event_id, events$event_id)
  known_events <- !is.na(losses$event_id)
  if (any(known_events & is.na(event_match))) {
    problems <- c(problems, "@losses$event_id must reference @events")
  }

  known_loss_trajectory <- !is.na(losses$trajectory_id)
  if (
    any(
      known_turns &
        known_loss_trajectory &
        !is.na(turn_match) &
        losses$trajectory_id != turns$trajectory_id[turn_match]
    )
  ) {
    problems <- c(
      problems,
      "@losses$turn_id must belong to the referenced trajectory"
    )
  }
  if (
    any(
      known_events &
        known_loss_trajectory &
        !is.na(event_match) &
        losses$trajectory_id != events$trajectory_id[event_match]
    )
  ) {
    problems <- c(
      problems,
      "@losses$event_id must belong to the referenced trajectory"
    )
  }
  problems
}

trajectory_reference_has_cycle <- function(ids, parents) {
  if (length(ids) == 0L || anyDuplicated(ids)) {
    return(FALSE)
  }
  parent_map <- stats::setNames(parents, ids)

  for (id in ids) {
    seen <- id
    parent <- parent_map[[id]]
    while (!is.null(parent) && !is.na(parent)) {
      if (parent %in% seen) {
        return(TRUE)
      }
      seen <- c(seen, parent)
      parent <- parent_map[[parent]]
    }
  }
  FALSE
}

trajectory_vocabulary_problems <- function(data) {
  problems <- character()
  vocabularies <- list(
    turns = list(
      column = "role",
      core = c("system", "user", "assistant", "tool"),
      optional = FALSE
    ),
    events_type = list(
      table = "events",
      column = "event_type",
      core = c("content", "tool_call", "tool_result", "error", "custom"),
      optional = FALSE
    ),
    events_content = list(
      table = "events",
      column = "content_type",
      core = c(
        "text",
        "thinking",
        "citation",
        "image",
        "pdf",
        "audio",
        "document",
        "json"
      ),
      optional = TRUE
    ),
    losses = list(
      column = "reason",
      core = c("unsupported", "redacted", "truncated", "externalized"),
      optional = FALSE
    )
  )

  for (name in names(vocabularies)) {
    vocabulary <- vocabularies[[name]]
    table <- vocabulary$table %||% name
    value <- data[[table]][[vocabulary$column]]
    if (vocabulary$optional) {
      value <- value[!is.na(value)]
    }
    valid <- value %in%
      vocabulary$core |
      grepl("^[a-z][a-z0-9._-]*:[a-z][a-z0-9._-]*$", value)
    if (!all(valid)) {
      problems <- c(
        problems,
        paste0(
          "@",
          table,
          "$",
          vocabulary$column,
          " must use a core or namespaced value"
        )
      )
    }
  }
  problems
}

trajectory_value_problems <- function(data) {
  problems <- character()

  for (table in c(
    "trajectories",
    "turns",
    "events",
    "evaluations",
    "losses"
  )) {
    value <- data[[table]]
    for (column in names(value)) {
      if (is.list(value[[column]])) {
        problems <- c(
          problems,
          trajectory_list_column_problems(
            value[[column]],
            paste0("@", table, "$", column)
          )
        )
      } else if (is.character(value[[column]])) {
        bytes <- nchar(value[[column]], type = "bytes", allowNA = TRUE)
        if (any(!is.na(bytes) & bytes > trajectory_payload_max_bytes)) {
          problems <- c(
            problems,
            paste0(
              "@",
              table,
              "$",
              column,
              " values must not exceed 65,536 bytes"
            )
          )
        }
      }
    }
  }

  metadata <- list(
    trajectories = data$trajectories$metadata,
    turns = data$turns$metadata,
    events = data$events$metadata,
    evaluations = data$evaluations$metadata,
    losses = data$losses$metadata
  )
  for (table in names(metadata)) {
    valid <- vapply(metadata[[table]], trajectory_is_named_list, logical(1))
    if (!all(valid)) {
      problems <- c(
        problems,
        paste0(
          "@",
          table,
          "$metadata elements must be uniquely named lists"
        )
      )
    }
  }
  problems
}

trajectory_list_column_problems <- function(x, label) {
  problems <- character()
  for (index in seq_along(x)) {
    value <- x[[index]]
    if (!trajectory_value_is_safe(value)) {
      problems <- c(
        problems,
        paste0(label, "[[", index, "]] must contain serializable data only")
      )
      next
    }
    bytes <- tryCatch(
      length(serialize(value, NULL, xdr = FALSE)),
      error = function(...) Inf
    )
    if (bytes > trajectory_payload_max_bytes) {
      problems <- c(
        problems,
        paste0(label, "[[", index, "]] must not exceed 65,536 bytes")
      )
    }
  }
  problems
}

trajectory_value_is_safe <- function(x, depth = 0L) {
  if (depth > 50L) {
    return(FALSE)
  }
  if (
    is.environment(x) ||
      is.function(x) ||
      isS4(x) ||
      inherits(x, "connection") ||
      typeof(x) %in% c("externalptr", "weakref", "promise", "language") ||
      S7::S7_inherits(x)
  ) {
    return(FALSE)
  }
  if (is.list(x)) {
    safe <- vapply(
      x,
      \(element) trajectory_value_is_safe(element, depth + 1L),
      logical(1)
    )
    if (!all(safe)) {
      return(FALSE)
    }
  }

  attributes <- attributes(x)
  if (!is.null(attributes)) {
    safe_attributes <- vapply(
      attributes,
      \(attribute) trajectory_value_is_safe(attribute, depth + 1L),
      logical(1)
    )
    if (!all(safe_attributes)) {
      return(FALSE)
    }
  }
  TRUE
}

trajectory_is_named_list <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    return(FALSE)
  }
  if (length(x) == 0L) {
    return(TRUE)
  }
  names <- names(x)
  !is.null(names) &&
    all(!is.na(names) & nzchar(trimws(names))) &&
    !anyDuplicated(names)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
