# Disclosed Deputy delegation views: the list `deputy::delegation_history()`
# returns, or `LeadAgent$inspect_subagents()`. They are plain lists with no
# class of their own, so `as_trajectory()` never dispatches on them.

deputy_is_delegation_source <- function(x) {
  deputy_is_delegation_export(x) ||
    (is.list(x) &&
      !is.object(x) &&
      length(x) > 0L &&
      is.null(names(x)) &&
      all(vapply(x, deputy_is_delegation_view, logical(1))))
}

deputy_is_delegation_view <- function(x) {
  trajectory_is_named_list(x) && is.list(x$outcome)
}

deputy_is_delegation_export <- function(x) {
  trajectory_is_named_list(x) &&
    all(c("schema_version", "settled", "scope", "children") %in% names(x))
}

deputy_source_views <- function(x, arg, call) {
  # Replaying an export without deputy::delegation_history() would skip the
  # second DelegationDisclosure check that Deputy requires on every read.
  if (deputy_is_delegation_export(x)) {
    scans_abort(
      c(
        "{.arg {arg}} is an unread {.code $export_subagents()} snapshot.",
        "i" = "Read it with {.fn deputy::delegation_history} and your {.cls deputy::DelegationDisclosure} first."
      ),
      class = "scans_error_deputy_source",
      call = call
    )
  }
  valid <- is.list(x) &&
    !is.object(x) &&
    length(x) > 0L &&
    is.null(names(x))
  if (!valid) {
    scans_abort(
      c(
        "{.arg {arg}} must be a non-empty list of Deputy delegation views.",
        "i" = "Use the value of {.fn deputy::delegation_history} or {.code $inspect_subagents(transcript = TRUE)}.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_deputy_source",
      call = call
    )
  }

  for (index in seq_along(x)) {
    view <- x[[index]]
    problem <- if (!deputy_is_delegation_view(view)) {
      "is not a delegation view with an {.field outcome}"
    } else if (!is.list(view$outcome$runtime)) {
      "has no {.field outcome$runtime} record"
    } else if (!is.null(view$transcript) && is.null(view$turns)) {
      "has transcript records that were not replayed into turns"
    } else if (
      !is.null(view$turns) &&
        (!is.list(view$turns) ||
          !all(vapply(view$turns, ellmer_is_turn, logical(1))))
    ) {
      "has {.field turns} that are not ellmer turns"
    }
    if (!is.null(problem)) {
      scans_abort(
        c(
          paste0("Element {index} of {.arg {arg}} ", problem, "."),
          "i" = "Use the value of {.fn deputy::delegation_history} or {.code $inspect_subagents(transcript = TRUE)}."
        ),
        class = "scans_error_deputy_source",
        call = call
      )
    }
  }
  x
}

deputy_view_trajectory_id <- function(view, index) {
  runtime <- view$outcome$runtime
  run_id <- deputy_source_string(runtime$run_id)
  if (!is.na(run_id)) {
    return(deputy_trajectory_id(run_id))
  }
  delegation_id <- deputy_source_string(runtime$delegation_id)
  if (!is.na(delegation_id)) {
    return(paste0(
      "deputy/delegation/",
      utils::URLencode(delegation_id, reserved = TRUE)
    ))
  }
  sprintf("trajectory-%06d", index)
}

deputy_view_bundle <- function(
  view,
  trajectory_id,
  source_uri,
  metadata,
  call
) {
  runtime <- view$outcome$runtime
  run_id <- deputy_source_string(runtime$run_id)
  agent <- deputy_source_string(runtime$agent_name)
  if (is.na(agent)) {
    agent <- deputy_source_string(runtime$agent_id)
  }
  model <- deputy_source_string(view$manifest$model)
  turns <- view$turns %||% list()

  base <- as_trajectory_ellmer(
    turns,
    trajectory_id = trajectory_id,
    run_id = deputy_canonical_string(run_id),
    source_id = deputy_canonical_string(run_id),
    source_uri = source_uri,
    agent = deputy_canonical_string(agent),
    model = if (is.na(model)) NULL else model,
    metadata = list()
  )

  ids <- trajectory_ids(trajectory_id)
  safe_metadata <- trajectory_sanitize_metadata(
    deputy_view_metadata(view, metadata),
    "metadata",
    ids
  )

  info <- trajectory_info(base)
  info$source_type <- "deputy"
  info$status <- deputy_delegation_status(runtime$status)
  info$metadata <- list(safe_metadata$value)

  source_losses <- c(
    safe_metadata$losses,
    deputy_view_lineage_losses(runtime, ids),
    deputy_transcript_losses(view, ids),
    deputy_outcome_losses(runtime, ids),
    deputy_offloaded_result_losses(turns, base)
  )
  losses <- trajectory_bind_rows(list(
    trajectory_losses(base),
    trajectory_loss_table(source_losses)
  ))

  TrajectoryBundle(
    info,
    trajectory_turns(base),
    trajectory_events(base),
    losses = losses
  )
}

deputy_view_metadata <- function(view, metadata) {
  outcome <- view$outcome
  runtime <- outcome$runtime
  list(
    deputy_version = as.character(utils::packageVersion("deputy")),
    source_class = "delegation_view",
    session_id = runtime$session_id,
    agent_id = runtime$agent_id,
    parent_agent_id = runtime$parent_agent_id,
    parent_run_id = runtime$parent_run_id,
    parent_delegation_id = runtime$parent_delegation_id,
    delegation_id = runtime$delegation_id,
    usage = view$usage,
    stop_reason = runtime$stop_reason,
    ending = deputy_ending_metadata(
      NULL,
      runtime$finish_reason,
      runtime$cut_off
    ),
    delegation = list(
      task = view$task,
      answer = outcome$answer,
      claims = list(
        missing_evidence = deputy_claim(outcome$claims$missing_evidence),
        unresolved_work = deputy_claim(outcome$claims$unresolved_work)
      ),
      references = outcome$references,
      manifest = deputy_manifest_metadata(view$manifest),
      runtime = runtime,
      cumulative_usage = view$cumulative_usage,
      errors = Filter(Negate(is.null), view$errors %||% list()),
      retention = view$retention
    ),
    application = metadata
  )
}

# Deputy joins a subagent's claim entries into one bounded string and leaves
# the claim NULL when the subagent did not report it.
deputy_claim <- function(x) {
  if (is.character(x) && length(x) == 1L) x else NA_character_
}

deputy_manifest_fields <- c(
  "schema_version",
  "definition_id",
  "provider",
  "model",
  "params",
  "tools",
  "policies",
  "size"
)

# The task, evidence, prompt, and message text stay out: the transcript
# carries what the subagent was sent, and metadata holds its configuration.
deputy_manifest_metadata <- function(manifest) {
  if (!is.list(manifest)) {
    return(NULL)
  }
  manifest$definition_id <- manifest$definition_id %||% manifest$definition$id
  stats::setNames(
    lapply(deputy_manifest_fields, \(field) manifest[[field]]),
    deputy_manifest_fields
  )
}

deputy_delegation_status <- function(status) {
  if (!is.character(status) || length(status) != 1L || is.na(status)) {
    return(trajectory_canonical_status(status))
  }
  switch(
    status,
    completed = "completed",
    failed = "failed",
    stopped = ,
    suspended = "interrupted",
    not_started = "cancelled",
    trajectory_canonical_status(status)
  )
}

deputy_view_lineage_losses <- function(runtime, ids) {
  losses <- list()
  if (is.na(deputy_source_string(runtime$run_id))) {
    losses <- list(trajectory_new_loss(
      ids,
      "run_id",
      "unsupported",
      "The delegation view has no Deputy run ID for source correlation"
    ))
  }
  for (field in deputy_parent_fields) {
    if (!is.na(deputy_source_string(runtime[[field]]))) {
      return(c(
        losses,
        list(trajectory_new_loss(
          ids,
          field,
          "unsupported",
          paste(
            "The parent lineage was retained as metadata because no parent",
            "trajectory snapshot was supplied"
          )
        ))
      ))
    }
  }
  losses
}

deputy_transcript_losses <- function(view, ids) {
  retention <- view$retention$transcript
  if (
    !is.null(view$transcript) &&
      (is.null(retention) || identical(retention, "included"))
  ) {
    return(list())
  }
  if (identical(retention, "not_requested")) {
    return(list(trajectory_new_loss(
      ids,
      "capture$transcript",
      "unsupported",
      "The delegation view was read without its transcript"
    )))
  }
  list(trajectory_new_loss(
    ids,
    "capture$transcript",
    "redacted",
    paste(
      "Deputy disclosure withheld the subagent transcript; its turns, tool",
      "calls, and tool results are absent"
    ),
    metadata = list(retention = deputy_canonical_string(retention))
  ))
}

deputy_outcome_losses <- function(runtime, ids) {
  losses <- list()
  if (isTRUE(runtime$answer_truncated)) {
    losses <- list(trajectory_new_loss(
      ids,
      "metadata$delegation$answer",
      "truncated",
      "Deputy cut the subagent's answer to 8 KiB in the delegation outcome"
    ))
  }
  for (name in c("missing_evidence", "unresolved_work")) {
    omission <- runtime$claim_omissions[[name]]
    omitted <- deputy_canonical_number(omission$omitted_entries)
    if (isTRUE(omitted > 0) || isTRUE(omission$text_truncated)) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          paste0("metadata$delegation$claims$", name),
          "truncated",
          "Deputy bounded the subagent's claim entries in the delegation outcome"
        ))
      )
    }
  }
  losses
}

# A tool result that Deputy moved out of the model context holds only a
# preview and a `deputy://tool-result/` reference. It is never dereferenced.
deputy_offloaded_result_losses <- function(turns, base) {
  events <- trajectory_events(base)
  if (nrow(events) == 0L) {
    return(list())
  }
  trajectory_id <- trajectory_info(base)$trajectory_id[[1L]]
  losses <- list()
  for (turn_index in seq_along(turns)) {
    contents <- S7::prop(turns[[turn_index]], "contents")
    turn_id <- ellmer_turn_id(trajectory_id, turn_index)
    for (content_index in seq_along(contents)) {
      content <- contents[[content_index]]
      if (!inherits(content, "ellmer::ContentToolResult")) {
        next
      }
      reference <- deputy::tool_result_reference(content)
      if (is.null(reference)) {
        next
      }
      row <- which(
        events$turn_id == turn_id &
          events$content_index == content_index &
          events$event_type == "tool_result"
      )
      event_id <- if (length(row) == 1L) {
        events$event_id[[row]]
      } else {
        NA_character_
      }
      losses <- c(
        losses,
        list(trajectory_new_loss(
          trajectory_ids(trajectory_id, turn_id, event_id),
          "contents$value",
          "externalized",
          paste(
            "Deputy offloaded this tool result; only a preview and a",
            "reference were captured"
          ),
          metadata = list(reference = reference)
        ))
      )
    }
  }
  losses
}
