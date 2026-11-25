# Route and provenance analysis over canonical bundle tables. These functions
# are internal until a second consumer proves the interface; see
# docs/design/route-analysis.md for the classification rules, signature
# canonicalization, ranking rule, and limits.

route_classes <- c(
  "trusted",
  "cited",
  "untrusted",
  "unclassified",
  "not_applicable"
)

# commons' provenance_display: A = Trusted, B = Cited, C = Untrusted.
route_commons_tags <- c(A = "trusted", B = "cited", C = "untrusted")

route_no_tools <- "(none)"

#' Classify and route every exchange
#'
#' One row per recorded exchange. An exchange is a turn's `round_index` when
#' the trajectory records rounds; otherwise a user turn opens an exchange
#' unless it carries a tool result or is marked `commons_internal`.
#'
#' Columns:
#' - `trajectory_id`, `exchange_index`, `exchange_id`: identity.
#'   `exchange_basis` is `"round_index"`, `"derived"`, or `"provenance_only"`
#'   (a provenance record names an exchange with no retained turns).
#' - `source_type`, `user` (`metadata$otel$user`, `NA` when unknown),
#'   `trajectory_completed_at` (conversation time, not answer time), and
#'   `timestamp` (earliest recorded event time in the exchange, often `NA`).
#' - `turn_ids`, `n_turns`, `n_assistant_turns`.
#' - `provenance_class`: one of `route_classes`. `provenance_reason`:
#'   `"recorded"`, `"missing_event"`, `"missing_tag"`, `"conflicting_tags"`,
#'   `"unrecognized_tag"`, or `"no_provenance_source"`. `provenance_tag` is the
#'   single recorded tag, otherwise `NA`. `provenance_event_ids` lists the
#'   contributing `commons:provenance` events.
#' - `route_signature`, `route_steps`, `n_steps`, `route_truncated`,
#'   `n_tool_calls`, `tool_call_event_ids`, `tool_result_event_ids`. The
#'   signature is `"(none)"` for an exchange with retained turns and no tool
#'   calls, and `NA` when no turns were retained.
#' - `n_capture_losses`: losses recorded on the exchange's turns or events.
#'
#' @param x A [TrajectoryBundle].
#' @param max_steps Maximum canonical steps kept in a signature.
#' @returns A tibble.
#' @noRd
route_exchanges <- function(x, max_steps = 12L) {
  check_trajectory_bundle(x)
  info <- trajectory_info(x)
  tables <- route_call_table(x)
  turns <- tables$turns
  events <- trajectory_events(x)
  calls <- tables$calls
  losses <- trajectory_losses(x)
  losses$trajectory_id <- scan_loss_trajectory_ids(losses, turns, events)

  turn_groups <- split(seq_len(nrow(turns)), turns$trajectory_id)
  event_groups <- split(seq_len(nrow(events)), events$trajectory_id)
  call_groups <- split(seq_len(nrow(calls)), calls$trajectory_id)
  loss_groups <- split(seq_len(nrow(losses)), losses$trajectory_id)
  rows <- lapply(seq_len(nrow(info)), function(i) {
    id <- info$trajectory_id[[i]]
    route_trajectory_exchanges(
      info[i, ],
      turns[turn_groups[[id]] %||% integer(), ],
      events[event_groups[[id]] %||% integer(), ],
      calls[call_groups[[id]] %||% integer(), ],
      losses[loss_groups[[id]] %||% integer(), ],
      max_steps
    )
  })
  vctrs::vec_rbind(!!!rows, .ptype = route_exchange_template())
}

route_exchange_template <- function() {
  tibble::tibble(
    trajectory_id = character(),
    exchange_index = integer(),
    exchange_id = character(),
    exchange_basis = character(),
    source_type = character(),
    user = character(),
    trajectory_completed_at = as.POSIXct(character(), tz = "UTC"),
    timestamp = as.POSIXct(character(), tz = "UTC"),
    turn_ids = list(),
    n_turns = integer(),
    n_assistant_turns = integer(),
    provenance_class = character(),
    provenance_reason = character(),
    provenance_tag = character(),
    provenance_event_ids = list(),
    route_signature = character(),
    route_steps = list(),
    n_steps = integer(),
    route_truncated = logical(),
    n_tool_calls = integer(),
    tool_call_event_ids = list(),
    tool_result_event_ids = list(),
    n_capture_losses = integer()
  )
}

route_trajectory_exchanges <- function(
  info,
  turns,
  events,
  calls,
  losses,
  max_steps
) {
  id <- info$trajectory_id[[1L]]
  provenance <- route_provenance_records(events, turns)
  provenance_source <- route_is_provenance_source(events, losses)
  indices <- sort(unique(c(
    stats::na.omit(turns$exchange_index),
    stats::na.omit(provenance$exchange_index)
  )))
  if (!length(indices)) {
    return(NULL)
  }
  user <- scans_app_metadata_user(info$metadata[[1L]])

  rows <- lapply(indices, function(index) {
    in_exchange <- turns$exchange_index %in% index
    exchange_turns <- turns[in_exchange, ]
    exchange_turns <- exchange_turns[order(exchange_turns$turn_index), ]
    turn_events <- events[events$turn_id %in% exchange_turns$turn_id, ]
    records <- provenance[provenance$exchange_index %in% index, ]
    exchange_calls <- calls[calls$exchange_index %in% index, ]
    route <- if (nrow(exchange_turns)) {
      route_signature(exchange_calls, max_steps)
    } else {
      list(
        signature = NA_character_,
        steps = character(),
        truncated = NA
      )
    }
    class <- route_exchange_class(records, provenance_source)
    event_ids <- c(turn_events$event_id, records$event_id)
    timestamps <- c(turn_events$timestamp, records$timestamp)
    tibble::tibble(
      trajectory_id = id,
      exchange_index = as.integer(index),
      exchange_id = sprintf("%s/exchange-%06d", id, index),
      exchange_basis = if (nrow(exchange_turns)) {
        exchange_turns$exchange_basis[[1L]]
      } else {
        "provenance_only"
      },
      source_type = info$source_type[[1L]],
      user = user,
      trajectory_completed_at = info$completed_at[[1L]],
      timestamp = if (all(is.na(timestamps))) {
        as.POSIXct(NA, tz = "UTC")
      } else {
        min(timestamps, na.rm = TRUE)
      },
      turn_ids = list(exchange_turns$turn_id),
      n_turns = nrow(exchange_turns),
      n_assistant_turns = sum(exchange_turns$role %in% "assistant"),
      provenance_class = class$class,
      provenance_reason = class$reason,
      provenance_tag = class$tag,
      provenance_event_ids = list(records$event_id),
      route_signature = route$signature,
      route_steps = list(route$steps),
      n_steps = length(route$steps),
      route_truncated = route$truncated,
      n_tool_calls = if (nrow(exchange_turns)) {
        nrow(exchange_calls)
      } else {
        NA_integer_
      },
      tool_call_event_ids = list(exchange_calls$event_id),
      tool_result_event_ids = list(
        exchange_calls$result_event_id[!is.na(exchange_calls$result_event_id)]
      ),
      n_capture_losses = sum(
        losses$turn_id %in%
          exchange_turns$turn_id |
          losses$event_id %in% event_ids
      )
    )
  })
  vctrs::vec_rbind(!!!rows)
}

# A trajectory is a provenance source when it records provenance events or its
# adapter reported the provenance record itself as missing (commons: a loss on
# field "provenance"). Unknown provenance is then unclassified, not absent.
route_is_provenance_source <- function(events, losses) {
  any(events$event_type %in% "commons:provenance") ||
    any(losses$field %in% "provenance")
}

# Provenance events keyed by exchange: `metadata$exchange_index`, falling back
# to the exchange of the event's turn when an adapter records one.
route_provenance_records <- function(events, turns) {
  records <- events[events$event_type %in% "commons:provenance", ]
  index <- vapply(
    records$metadata,
    function(metadata) {
      value <- if (is.list(metadata)) metadata$exchange_index else NULL
      if (
        is.numeric(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          value >= 1 &&
          value == floor(value)
      ) {
        as.integer(value)
      } else {
        NA_integer_
      }
    },
    integer(1)
  )
  from_turn <- turns$exchange_index[match(records$turn_id, turns$turn_id)]
  index[is.na(index)] <- from_turn[is.na(index)]
  records$exchange_index <- index
  records$tag <- lapply(records$value, function(value) {
    if (trajectory_is_named_list(value)) value$provenance_tag else NULL
  })
  records
}

route_exchange_class <- function(records, provenance_source) {
  if (!provenance_source) {
    return(list(
      class = "not_applicable",
      reason = "no_provenance_source",
      tag = NA_character_
    ))
  }
  unclassified <- function(reason) {
    list(class = "unclassified", reason = reason, tag = NA_character_)
  }
  if (!nrow(records)) {
    return(unclassified("missing_event"))
  }
  tags <- records$tag
  if (any(lengths(tags) > 1L)) {
    return(unclassified("conflicting_tags"))
  }
  missing <- vapply(
    tags,
    function(tag) is.atomic(tag) && length(tag) == 1L && is.na(tag),
    logical(1)
  )
  if (all(missing)) {
    return(unclassified("missing_tag"))
  }
  if (any(missing)) {
    return(unclassified("conflicting_tags"))
  }
  if (!all(vapply(tags, rlang::is_string, logical(1)))) {
    return(unclassified("unrecognized_tag"))
  }
  tags <- unique(unlist(tags))
  if (length(tags) > 1L) {
    return(unclassified("conflicting_tags"))
  }
  if (!tags %in% names(route_commons_tags)) {
    return(unclassified("unrecognized_tag"))
  }
  list(
    class = unname(route_commons_tags[[tags]]),
    reason = "recorded",
    tag = tags
  )
}

# Assigns each turn to an exchange and links each tool call to its first
# recorded result, by parent event and then by unambiguous call identifier.
route_call_table <- function(x) {
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  exchanges <- route_turn_exchanges(turns, events)
  turns$exchange_index <- exchanges$index
  turns$exchange_basis <- exchanges$basis

  calls <- events[events$event_type %in% "tool_call", ]
  results <- events[events$event_type %in% "tool_result", ]
  call_keys <- paste(calls$trajectory_id, calls$event_id, sep = "\r")
  owner <- match(
    paste(results$trajectory_id, results$parent_event_id, sep = "\r"),
    call_keys
  )
  owner[is.na(results$parent_event_id)] <- NA_integer_
  # A call identifier links a result only when one call in the trajectory
  # carries it; ambiguous identifiers leave the result unlinked.
  call_id_keys <- paste(calls$trajectory_id, calls$call_id, sep = "\r")
  call_id_keys[
    is.na(calls$call_id) |
      call_id_keys %in% call_id_keys[duplicated(call_id_keys)]
  ] <- NA_character_
  by_call_id <- match(
    paste(results$trajectory_id, results$call_id, sep = "\r"),
    call_id_keys,
    incomparables = NA_character_
  )
  owner[is.na(owner)] <- by_call_id[is.na(owner)]
  result <- match(seq_len(nrow(calls)), owner)

  turn <- match(
    paste(calls$trajectory_id, calls$turn_id, sep = "\r"),
    paste(turns$trajectory_id, turns$turn_id, sep = "\r")
  )
  turn[is.na(calls$turn_id)] <- NA_integer_
  out <- tibble::tibble(
    trajectory_id = calls$trajectory_id,
    exchange_index = turns$exchange_index[turn],
    turn_id = calls$turn_id,
    turn_index = turns$turn_index[turn],
    event_id = calls$event_id,
    event_index = calls$event_index,
    tool = calls$name,
    input = calls$value,
    input_text = calls$text,
    result_event_id = results$event_id[result],
    result_text = results$text[result],
    result_value = results$value[result],
    result_status = results$status[result],
    result_error = results$error[result]
  )
  out <- out[
    order(out$trajectory_id, out$turn_index, out$event_index, method = "radix"),
  ]
  list(turns = turns, calls = out)
}

route_turn_exchanges <- function(turns, events) {
  index <- rep(NA_integer_, nrow(turns))
  basis <- rep(NA_character_, nrow(turns))
  result_turns <- paste(
    events$trajectory_id,
    events$turn_id,
    sep = "\r"
  )[events$event_type %in% "tool_result" & !is.na(events$turn_id)]
  internal <- vapply(
    turns$metadata,
    function(metadata) is.list(metadata) && isTRUE(metadata$commons_internal),
    logical(1)
  )
  for (rows in split(seq_len(nrow(turns)), turns$trajectory_id)) {
    rows <- rows[order(turns$turn_index[rows])]
    if (!all(is.na(turns$round_index[rows]))) {
      index[rows] <- turns$round_index[rows]
      basis[rows] <- "round_index"
      next
    }
    opens <- turns$role[rows] %in%
      "user" &
      !paste(turns$trajectory_id[rows], turns$turn_id[rows], sep = "\r") %in%
        result_turns &
      !internal[rows]
    opened <- cumsum(opens)
    index[rows] <- ifelse(opened > 0L, opened, NA_integer_)
    basis[rows] <- "derived"
  }
  list(index = as.integer(index), basis = basis)
}

# Canonical steps: one step per turn holding tool calls, in turn order. Calls
# in one turn are parallel, so their distinct names are sorted (C locale) and
# joined with " + ". Immediately repeated steps collapse to one, so retries do
# not create new routes. Signatures longer than `max_steps` end in "...".
route_signature <- function(calls, max_steps) {
  if (!nrow(calls)) {
    return(list(
      signature = route_no_tools,
      steps = character(),
      truncated = FALSE
    ))
  }
  tools <- calls$tool
  tools[is.na(tools) | !nzchar(tools)] <- "(unnamed)"
  # Calls without a turn cannot be ordered against other turns; each one is
  # kept as its own step after the recorded event order.
  step_keys <- ifelse(is.na(calls$turn_id), calls$event_id, calls$turn_id)
  steps <- vapply(
    unique(step_keys),
    function(key) {
      paste(
        sort(unique(tools[step_keys == key]), method = "radix"),
        collapse = " + "
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
  steps <- steps[c(TRUE, steps[-1L] != steps[-length(steps)])]
  truncated <- length(steps) > max_steps
  if (truncated) {
    steps <- steps[seq_len(max_steps)]
  }
  signature <- paste(c(steps, if (truncated) "..."), collapse = " > ")
  list(signature = signature, steps = steps, truncated = truncated)
}

#' Summarize routes and provenance with explicit denominators
#'
#' `by = "overall"` returns one row per provenance class, including zero
#' counts, with the scope's denominators and capture windows. `by =
#' "trajectory"` returns one row per trajectory in scope, including those with
#' no exchanges. `by = "signature"` returns one row per route signature with
#' its provenance breakdown, ranked like candidates.
#'
#' The event window covers recorded exchange event timestamps; the trajectory
#' window covers recorded trajectory bounds. Neither is the reader's query
#' window, which the bundle does not retain.
#'
#' @param x A [TrajectoryBundle].
#' @param by `"overall"`, `"trajectory"`, or `"signature"`.
#' @param trajectory_ids Optional scope; `NULL` uses every trajectory.
#' @param exchanges `route_exchanges(x)`, supplied to reuse a computed table.
#' @returns A tibble.
#' @noRd
route_summary <- function(
  x,
  by = c("overall", "trajectory", "signature"),
  trajectory_ids = NULL,
  exchanges = route_exchanges(x)
) {
  by <- rlang::arg_match(by)
  info <- route_scope_info(x, trajectory_ids)
  exchanges <- exchanges[exchanges$trajectory_id %in% info$trajectory_id, ]
  switch(
    by,
    overall = route_summary_overall(info, exchanges),
    trajectory = route_summary_trajectories(x, info, exchanges),
    signature = route_summary_signatures(exchanges)
  )
}

route_scope_info <- function(x, trajectory_ids) {
  info <- trajectory_info(x)
  if (!is.null(trajectory_ids)) {
    info <- info[info$trajectory_id %in% trajectory_ids, ]
  }
  info$user <- vapply(info$metadata, scans_app_metadata_user, character(1))
  info
}

route_summary_overall <- function(info, exchanges) {
  users <- unique(stats::na.omit(info$user))
  bounds <- c(info$started_at, info$completed_at)
  window <- function(x, fun) {
    if (all(is.na(x))) as.POSIXct(NA, tz = "UTC") else fun(x, na.rm = TRUE)
  }
  n_exchanges <- vapply(
    route_classes,
    function(class) sum(exchanges$provenance_class == class),
    integer(1),
    USE.NAMES = FALSE
  )
  tibble::tibble(
    provenance_class = route_classes,
    n_exchanges = n_exchanges,
    n_trajectories = vapply(
      route_classes,
      function(class) {
        length(unique(exchanges$trajectory_id[
          exchanges$provenance_class == class
        ]))
      },
      integer(1),
      USE.NAMES = FALSE
    ),
    n_users = vapply(
      route_classes,
      function(class) {
        length(unique(stats::na.omit(exchanges$user[
          exchanges$provenance_class == class
        ])))
      },
      integer(1),
      USE.NAMES = FALSE
    ),
    exchange_share = if (nrow(exchanges)) {
      n_exchanges / nrow(exchanges)
    } else {
      NA_real_
    },
    total_exchanges = nrow(exchanges),
    total_trajectories = nrow(info),
    total_users = length(users),
    n_unknown_user_trajectories = sum(is.na(info$user)),
    n_timestamped_exchanges = sum(!is.na(exchanges$timestamp)),
    event_window_start = window(exchanges$timestamp, min),
    event_window_end = window(exchanges$timestamp, max),
    trajectory_window_start = window(bounds, min),
    trajectory_window_end = window(bounds, max)
  )
}

route_summary_trajectories <- function(x, info, exchanges) {
  calls <- route_call_table(x)$calls
  events <- trajectory_events(x)
  losses <- trajectory_losses(x)
  losses$trajectory_id <- scan_loss_trajectory_ids(
    losses,
    trajectory_turns(x),
    events
  )
  counts <- lapply(route_classes, function(class) {
    vapply(
      info$trajectory_id,
      function(id) {
        sum(exchanges$trajectory_id == id & exchanges$provenance_class == class)
      },
      integer(1),
      USE.NAMES = FALSE
    )
  })
  names(counts) <- paste0("n_", route_classes)
  routed <- !is.na(exchanges$route_signature) &
    exchanges$route_signature != route_no_tools
  tibble::tibble(
    trajectory_id = info$trajectory_id,
    source_type = info$source_type,
    user = info$user,
    completed_at = info$completed_at,
    provenance_source = vapply(
      info$trajectory_id,
      function(id) {
        route_is_provenance_source(
          events[events$trajectory_id %in% id, ],
          losses[losses$trajectory_id %in% id, ]
        )
      },
      logical(1),
      USE.NAMES = FALSE
    ),
    n_exchanges = vapply(
      info$trajectory_id,
      function(id) sum(exchanges$trajectory_id == id),
      integer(1),
      USE.NAMES = FALSE
    ),
    !!!counts,
    n_tool_calls = vapply(
      info$trajectory_id,
      function(id) sum(calls$trajectory_id == id),
      integer(1),
      USE.NAMES = FALSE
    ),
    n_unassigned_tool_calls = vapply(
      info$trajectory_id,
      function(id) sum(calls$trajectory_id == id & is.na(calls$exchange_index)),
      integer(1),
      USE.NAMES = FALSE
    ),
    n_signatures = vapply(
      info$trajectory_id,
      function(id) {
        length(unique(exchanges$route_signature[
          routed & exchanges$trajectory_id == id
        ]))
      },
      integer(1),
      USE.NAMES = FALSE
    )
  )
}

route_summary_signatures <- function(exchanges) {
  groups <- split(
    seq_len(nrow(exchanges)),
    factor(exchanges$route_signature, exclude = NULL)
  )
  groups <- groups[lengths(groups) > 0L]
  rows <- lapply(groups, function(rows) {
    part <- exchanges[rows, ]
    counts <- lapply(route_classes, function(class) {
      sum(part$provenance_class == class)
    })
    names(counts) <- paste0("n_", route_classes)
    tibble::tibble(
      route_signature = part$route_signature[[1L]],
      n_steps = part$n_steps[[1L]],
      n_exchanges = nrow(part),
      n_trajectories = length(unique(part$trajectory_id)),
      n_users = length(unique(stats::na.omit(part$user))),
      !!!counts
    )
  })
  out <- vctrs::vec_rbind(
    !!!unname(rows),
    .ptype = route_signature_template()
  )
  out[
    order(
      -out$n_trajectories,
      -out$n_exchanges,
      out$route_signature,
      method = "radix"
    ),
  ]
}

route_signature_template <- function() {
  counts <- rep(list(integer()), length(route_classes))
  names(counts) <- paste0("n_", route_classes)
  tibble::tibble(
    route_signature = character(),
    n_steps = integer(),
    n_exchanges = integer(),
    n_trajectories = integer(),
    n_users = integer(),
    !!!counts
  )
}

# Exploratory or fallback exchanges: recorded untrusted or cited answers with a
# tool route, or multi-step routes from sources that record no provenance.
# Unclassified exchanges are never eligible.
route_candidate_basis <- function(exchanges) {
  routed <- !is.na(exchanges$route_signature) &
    exchanges$route_signature != route_no_tools
  basis <- rep(NA_character_, nrow(exchanges))
  basis[
    routed & exchanges$provenance_class %in% c("untrusted", "cited")
  ] <- "provenance"
  basis[
    routed &
      exchanges$provenance_class == "not_applicable" &
      exchanges$n_steps >= 2L
  ] <- "unclassified_source"
  basis
}

#' Rank repeated exploratory routes as measure-promotion candidates
#'
#' Candidates group eligible exchanges by `route_signature` and
#' `candidate_basis`: `"provenance"` for recorded untrusted or cited answers
#' with a tool route, and `"unclassified_source"` for multi-step routes from
#' trajectories with no provenance evidence (`not_applicable`). Unclassified
#' exchanges never make a route eligible.
#'
#' Ranking: distinct trajectories (desc), eligible exchanges (desc),
#' `"provenance"` before `"unclassified_source"`, then signature in C-locale
#' order. There is no composite score.
#'
#' Columns: `rank`, `route_signature`, `candidate_basis`, `n_steps`;
#' eligible frequency `n_trajectories`, `n_exchanges`, `n_users`,
#' `n_unknown_user_trajectories`; the route's breakdown across every exchange
#' in scope with the signature, `n_trusted` through `n_not_applicable`;
#' evidence `trajectory_ids` and `evidence` (one row per eligible exchange with
#' `turn_ids` and `event_ids`); `snippets` (at most `max_snippets` recorded
#' tool inputs and outputs, calculation tools first; see `route_snippets()`);
#' and `resources`, a per-measure summary of `measure_trajectories()` values
#' over the candidate's trajectories. Measures have trajectory grain: a value
#' covers every exchange in that trajectory, not only this route.
#'
#' @param x A [TrajectoryBundle].
#' @param trajectory_ids Optional scope.
#' @param exchanges `route_exchanges(x)`.
#' @param measures `measure_trajectories(x)`, or `NULL` to omit resources.
#' @param min_exchanges Minimum eligible exchanges for a candidate.
#' @param max_snippets,max_chars Bounds for `snippets`.
#' @returns A tibble.
#' @noRd
route_candidates <- function(
  x,
  trajectory_ids = NULL,
  exchanges = route_exchanges(x),
  measures = measure_trajectories(x),
  min_exchanges = 2L,
  max_snippets = 5L,
  max_chars = 1000L
) {
  info <- route_scope_info(x, trajectory_ids)
  exchanges <- exchanges[exchanges$trajectory_id %in% info$trajectory_id, ]
  routes <- route_summary_signatures(exchanges)
  exchanges$candidate_basis <- route_candidate_basis(exchanges)
  eligible <- exchanges[!is.na(exchanges$candidate_basis), ]
  groups <- split(
    seq_len(nrow(eligible)),
    paste(eligible$candidate_basis, eligible$route_signature, sep = "\r")
  )
  groups <- groups[lengths(groups) >= min_exchanges]
  calls <- if (length(groups)) route_call_table(x)$calls
  rows <- lapply(unname(groups), function(rows) {
    part <- eligible[rows, ]
    route <- routes[match(part$route_signature[[1L]], routes$route_signature), ]
    ids <- unique(part$trajectory_id)
    users <- info$user[match(ids, info$trajectory_id)]
    tibble::tibble(
      route_signature = part$route_signature[[1L]],
      candidate_basis = part$candidate_basis[[1L]],
      n_steps = part$n_steps[[1L]],
      n_trajectories = length(ids),
      n_exchanges = nrow(part),
      n_users = length(unique(stats::na.omit(users))),
      n_unknown_user_trajectories = sum(is.na(users)),
      n_trusted = route$n_trusted,
      n_cited = route$n_cited,
      n_untrusted = route$n_untrusted,
      n_unclassified = route$n_unclassified,
      n_not_applicable = route$n_not_applicable,
      trajectory_ids = list(ids),
      evidence = list(route_evidence(part)),
      snippets = list(utils::head(
        route_snippet_rows(calls, part, max_chars, calculation_first = TRUE),
        max_snippets
      )),
      resources = list(route_resources(measures, ids))
    )
  })
  out <- vctrs::vec_rbind(!!!rows, .ptype = route_candidate_template())
  out <- out[
    order(
      -out$n_trajectories,
      -out$n_exchanges,
      match(out$candidate_basis, c("provenance", "unclassified_source")),
      out$route_signature,
      method = "radix"
    ),
  ]
  out$rank <- seq_len(nrow(out))
  out[c("rank", setdiff(names(out), "rank"))]
}

route_candidate_template <- function() {
  tibble::tibble(
    route_signature = character(),
    candidate_basis = character(),
    n_steps = integer(),
    n_trajectories = integer(),
    n_exchanges = integer(),
    n_users = integer(),
    n_unknown_user_trajectories = integer(),
    n_trusted = integer(),
    n_cited = integer(),
    n_untrusted = integer(),
    n_unclassified = integer(),
    n_not_applicable = integer(),
    trajectory_ids = list(),
    evidence = list(),
    snippets = list(),
    resources = list()
  )
}

route_evidence <- function(exchanges) {
  tibble::tibble(
    trajectory_id = exchanges$trajectory_id,
    exchange_id = exchanges$exchange_id,
    exchange_index = exchanges$exchange_index,
    provenance_class = exchanges$provenance_class,
    turn_ids = exchanges$turn_ids,
    event_ids = lapply(seq_len(nrow(exchanges)), function(i) {
      unique(c(
        exchanges$tool_call_event_ids[[i]],
        exchanges$tool_result_event_ids[[i]],
        exchanges$provenance_event_ids[[i]]
      ))
    })
  )
}

route_resources <- function(measures, trajectory_ids) {
  template <- tibble::tibble(
    measure = character(),
    unit = character(),
    grain = character(),
    n_total = integer(),
    n_recorded = integer(),
    n_complete = integer(),
    median_recorded = double(),
    median_complete = double()
  )
  if (is.null(measures)) {
    return(template)
  }
  measures <- measures[measures$trajectory_id %in% trajectory_ids, ]
  rows <- lapply(unique(measures$measure), function(name) {
    part <- measures[measures$measure == name, ]
    known <- part$value[!is.na(part$value)]
    complete <- part$value[!is.na(part$value) & part$coverage == "complete"]
    tibble::tibble(
      measure = name,
      unit = part$unit[[1L]],
      grain = "trajectory",
      n_total = nrow(part),
      n_recorded = length(known),
      n_complete = length(complete),
      median_recorded = scans_app_median(known),
      median_complete = scans_app_median(complete)
    )
  })
  vctrs::vec_rbind(!!!rows, .ptype = template)
}

#' Recorded tool inputs and outputs for exchanges
#'
#' One row per tool call in the supplied exchanges, in exchange and step
#' order. `input` and `output` render the recorded arguments and result as
#' text, cut to `max_chars`; `input_chars` and `output_chars` give the full
#' rendered length so truncation is visible. Captured SQL and R are rendered,
#' never evaluated. `calculation` marks `run_sql`, `run_r`, and `call_*`
#' tools.
#'
#' @param x A [TrajectoryBundle].
#' @param exchanges Rows of `route_exchanges(x)`.
#' @param max_chars Maximum characters per rendered field.
#' @returns A tibble.
#' @noRd
route_snippets <- function(x, exchanges, max_chars = 1000L) {
  route_snippet_rows(route_call_table(x)$calls, exchanges, max_chars)
}

route_snippet_rows <- function(
  calls,
  exchanges,
  max_chars,
  calculation_first = FALSE
) {
  keys <- paste(exchanges$trajectory_id, exchanges$exchange_index, sep = "\r")
  selected <- paste(calls$trajectory_id, calls$exchange_index, sep = "\r")
  owner <- match(selected, keys)
  calls <- calls[!is.na(owner), ]
  owner <- owner[!is.na(owner)]
  calls <- calls[order(owner, method = "radix"), ]
  owner <- sort(owner, method = "radix")
  step <- stats::ave(seq_along(owner), owner, FUN = seq_along)
  input <- vapply(
    seq_len(nrow(calls)),
    function(i) route_render(calls$input_text[[i]], calls$input[[i]]),
    character(1)
  )
  output <- vapply(
    seq_len(nrow(calls)),
    function(i) {
      route_render(calls$result_text[[i]], calls$result_value[[i]])
    },
    character(1)
  )
  input_chars <- nchar(input)
  output_chars <- nchar(output)
  out <- tibble::tibble(
    trajectory_id = calls$trajectory_id,
    exchange_id = exchanges$exchange_id[owner],
    exchange_index = calls$exchange_index,
    provenance_class = exchanges$provenance_class[owner],
    step = as.integer(step),
    turn_id = calls$turn_id,
    event_id = calls$event_id,
    result_event_id = calls$result_event_id,
    tool = calls$tool,
    calculation = route_is_calculation(calls$tool),
    input = substr(input, 1L, max_chars),
    input_chars = input_chars,
    output = substr(output, 1L, max_chars),
    output_chars = output_chars,
    result_status = calls$result_status,
    result_error = calls$result_error
  )
  if (calculation_first) {
    out <- out[order(!out$calculation, seq_len(nrow(out)), method = "radix"), ]
  }
  out
}

route_is_calculation <- function(tool) {
  !is.na(tool) & (tool %in% c("run_sql", "run_r") | startsWith(tool, "call_"))
}

route_render <- function(text, value) {
  if (!is.na(text)) {
    return(text)
  }
  if (
    is.null(value) ||
      (is.atomic(value) && length(value) == 1L && is.na(value))
  ) {
    return(NA_character_)
  }
  if (is.character(value) && length(value) == 1L) {
    return(value)
  }
  if (rlang::is_installed("jsonlite")) {
    return(as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      force = TRUE
    )))
  }
  paste(deparse(value), collapse = "")
}

#' Optional normalized features of recorded tool arguments
#'
#' One row per extracted value. Features never feed signatures or counts.
#' `describe_table` contributes its recorded `table` argument and `call_*`
#' tools their recorded `name`; both have `heuristic = FALSE`. `run_sql`
#' contributes `sql_table` values found after `FROM` or `JOIN` by a
#' conservative regular expression over the recorded `sql` argument, with
#' comments, string literals, common table expression names, and
#' `EXTRACT`/`TRIM`/`SUBSTRING`/`IS DISTINCT FROM` forms removed; these have
#' `heuristic = TRUE`. Only named-list arguments are read; SQL and R are never
#' executed.
#'
#' @param x A [TrajectoryBundle].
#' @returns A tibble.
#' @noRd
route_features <- function(x) {
  calls <- route_call_table(x)$calls
  rows <- lapply(seq_len(nrow(calls)), function(i) {
    values <- route_call_features(calls$tool[[i]], calls$input[[i]])
    if (!length(values$value)) {
      return(NULL)
    }
    tibble::tibble(
      trajectory_id = calls$trajectory_id[[i]],
      exchange_index = calls$exchange_index[[i]],
      event_id = calls$event_id[[i]],
      tool = calls$tool[[i]],
      feature = values$feature,
      value = values$value,
      heuristic = values$heuristic
    )
  })
  vctrs::vec_rbind(
    !!!rows,
    .ptype = tibble::tibble(
      trajectory_id = character(),
      exchange_index = integer(),
      event_id = character(),
      tool = character(),
      feature = character(),
      value = character(),
      heuristic = logical()
    )
  )
}

route_call_features <- function(tool, arguments) {
  none <- list(
    feature = character(),
    value = character(),
    heuristic = logical()
  )
  string <- function(name) {
    value <- arguments[[name]]
    if (rlang::is_string(value) && !is.na(value) && nzchar(value)) value
  }
  if (is.na(tool) || !trajectory_is_named_list(arguments)) {
    return(none)
  }
  if (identical(tool, "describe_table") && !is.null(string("table"))) {
    return(list(feature = "table", value = string("table"), heuristic = FALSE))
  }
  if (startsWith(tool, "call_") && !is.null(string("name"))) {
    return(list(feature = "measure", value = string("name"), heuristic = FALSE))
  }
  if (identical(tool, "run_sql") && !is.null(string("sql"))) {
    tables <- route_sql_tables(string("sql"))
    return(list(
      feature = rep("sql_table", length(tables)),
      value = tables,
      heuristic = rep(TRUE, length(tables))
    ))
  }
  none
}

route_sql_tables <- function(sql) {
  sql <- gsub("--[^\n]*|/\\*.*?\\*/", " ", sql, perl = TRUE)
  sql <- gsub("'(?:[^']|'')*'", "''", sql, perl = TRUE)
  sql <- gsub(
    "(?i)\\b(?:extract|trim|substring)\\s*\\([^()]*\\)|\\bdistinct\\s+from\\b",
    " ",
    sql,
    perl = TRUE
  )
  name <- "(?:\"[^\"]+\"|`[^`]+`|\\[[^]]+\\]|[A-Za-z_][A-Za-z0-9_$]*)"
  ctes <- regmatches(
    sql,
    gregexpr(
      paste0(
        "(?i)(?:\\bwith(?:\\s+recursive)?|,)\\s*(",
        name,
        ")\\s+as\\s*\\("
      ),
      sql,
      perl = TRUE
    )
  )[[1L]]
  ctes <- sub("(?i)^(?:with(?:\\s+recursive)?|,)\\s*", "", ctes, perl = TRUE)
  ctes <- sub("(?i)\\s+as\\s*\\($", "", ctes, perl = TRUE)
  matches <- regmatches(
    sql,
    gregexpr(
      paste0(
        "(?i)\\b(?:from|join)\\s+(",
        name,
        "(?:\\s*\\.\\s*",
        name,
        ")*)"
      ),
      sql,
      perl = TRUE
    )
  )[[1L]]
  tables <- sub("(?i)^(?:from|join)\\s+", "", matches, perl = TRUE)
  clean <- function(x) tolower(gsub("[\"`]|\\[|\\]|\\s", "", x))
  tables <- clean(tables)
  unique(tables[!tables %in% clean(ctes)])
}
