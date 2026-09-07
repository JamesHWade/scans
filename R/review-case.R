#' Review exact trajectory evidence
#'
#' `review_case()` freezes one trajectory from an investigation for human
#' review. It retains the original snapshot and revision identifiers and a
#' selected investigation containing only that trajectory and relevant losses.
#' `review_decision()` appends a decision without changing earlier decisions.
#' These functions never call a model or select content for an example set.
#'
#' A scanner judgment describes whether a finding is valid. An application
#' expectation describes what a future execution should do. They are separate:
#' the rationale for an old output is not a score for a new prediction.
#'
#' Decisions have content identifiers. Independent decisions remain active
#' until an adjudicator explicitly supersedes them. Multiple active decisions
#' are disputed, even if they agree. Uncertain and insufficient-capture
#' decisions cannot become evaluation targets. Reviewer names are supplied
#' labels, not authenticated identities. Hashes detect edits, not authorship.
#'
#' @param x An [investigation_snapshot()] for `review_case()`, or a review case
#'   for `review_decision()` and `review_status()`.
#' @param trajectory_id One trajectory in the investigation.
#' @param finding_id Optional finding in that trajectory. Leave `NULL` to
#'   review an unflagged trajectory or record a missed finding.
#' @param scan Optional scanner name from the saved analysis. Required to
#'   record a scanner target for an unflagged trajectory. Inferred from a finding.
#' @param event_ids,turn_ids Optional evidence references within the trajectory.
#' @param judgment One of `"confirmed"`, `"rejected"`, `"uncertain"`,
#'   `"missed"`, or `"insufficient_capture"`.
#' @param reviewer One non-empty reviewer name.
#' @param rationale One non-empty explanation about the reviewed evidence.
#' @param expected_behavior Optional description of the intended future behavior.
#' @param supersedes Decision identifiers to adjudicate. Only currently active
#'   decisions can be superseded. Defaults to none, creating an independent review.
#'
#' @returns `review_case()` and `review_decision()` return a `scans_review_case`
#'   value with selected evidence, provenance, and append-only decision history.
#'   `review_status()` returns a list with `status` and active `decision_ids`.
#' @name review_case
#' @export
review_case <- function(
  x,
  trajectory_id,
  finding_id = NULL,
  scan = NULL,
  event_ids = character(),
  turn_ids = character()
) {
  investigation_check_dependencies()
  investigation_validate(x)
  review_string(trajectory_id, "trajectory_id")
  if (!trajectory_id %in% trajectory_info(x$bundle)$trajectory_id) {
    review_abort("The trajectory is absent from this investigation.")
  }
  if (!is.null(finding_id)) {
    review_string(finding_id, "finding_id")
    finding <- x$analysis$findings[
      x$analysis$findings$finding_id == finding_id &
        x$analysis$findings$trajectory_id == trajectory_id,
      ,
      drop = FALSE
    ]
    if (nrow(finding) != 1L) {
      review_abort("The finding is absent from this trajectory.")
    }
    if (!is.null(scan) && !identical(scan, finding$scan[[1L]])) {
      review_abort("The scanner disagrees with the finding.")
    }
    scan <- finding$scan[[1L]]
    event_ids <- unique(c(
      event_ids,
      finding$event_ids[[1L]],
      stats::na.omit(finding$event_id)
    ))
    event_ids <- as.character(event_ids)
    turn_ids <- as.character(unique(c(
      turn_ids,
      stats::na.omit(finding$turn_id)
    )))
  }
  if (!is.null(scan)) {
    review_string(scan, "scan")
    if (!scan %in% x$settings$scans) {
      review_abort("The scanner is absent from this investigation.")
    }
  }
  review_references(
    event_ids,
    trajectory_events(x$bundle),
    "event_id",
    trajectory_id
  )
  review_references(
    turn_ids,
    trajectory_turns(x$bundle),
    "turn_id",
    trajectory_id
  )
  selected <- investigation_build(
    x$bundle,
    trajectory_id,
    x$manifest$application,
    x$manifest$source,
    x$settings,
    list(),
    x$analysis,
    previous = x,
    package_version = x$manifest$package_version
  )
  out <- list(
    format_version = 1L,
    origin = list(
      snapshot_id = x$manifest$snapshot_id,
      revision_id = x$manifest$revision_id
    ),
    evidence = investigation_payload(selected),
    trajectory_id = trajectory_id,
    finding_id = finding_id,
    scan = scan,
    event_ids = event_ids,
    turn_ids = turn_ids,
    decisions = list()
  )
  out$case_id <- investigation_hash(out[setdiff(names(out), "decisions")])
  structure(out, class = "scans_review_case")
}

#' @rdname review_case
#' @export
review_decision <- function(
  x,
  judgment,
  reviewer,
  rationale,
  expected_behavior = NULL,
  supersedes = character()
) {
  review_validate(x)
  review_choice(
    judgment,
    c("confirmed", "rejected", "uncertain", "missed", "insufficient_capture"),
    "judgment"
  )
  review_string(reviewer, "reviewer")
  review_string(rationale, "rationale")
  if (!is.null(expected_behavior)) {
    review_string(expected_behavior, "expected_behavior")
  }
  active <- review_active(x$decisions)
  if (
    !is.character(supersedes) ||
      anyNA(supersedes) ||
      anyDuplicated(supersedes) ||
      !all(supersedes %in% active)
  ) {
    review_abort("Superseded decisions must be active in this case.")
  }
  if (identical(judgment, "missed") && !is.null(x$finding_id)) {
    review_abort(
      "A missed finding must be reviewed without a finding identifier."
    )
  }
  decision <- list(
    case_id = x$case_id,
    judgment = judgment,
    reviewer = reviewer,
    rationale = rationale,
    expected_behavior = expected_behavior,
    supersedes = supersedes,
    sequence = length(x$decisions) + 1L,
    created_at = as.POSIXct(Sys.time(), tz = "UTC")
  )
  decision$decision_id <- investigation_hash(decision)
  x$decisions <- c(x$decisions, list(decision))
  x
}

#' @rdname review_case
#' @export
review_status <- function(x) {
  review_validate(x)
  ids <- review_active(x$decisions)
  status <- if (!length(ids)) {
    "unreviewed"
  } else if (length(ids) > 1L) {
    "disputed"
  } else {
    x$decisions[[match(
      ids,
      vapply(x$decisions, `[[`, "", "decision_id")
    )]]$judgment
  }
  list(status = status, decision_ids = ids)
}

review_active <- function(decisions) {
  setdiff(
    vapply(decisions, `[[`, "", "decision_id"),
    unlist(lapply(decisions, `[[`, "supersedes"), use.names = FALSE)
  )
}

review_evidence <- function(payload) {
  if (
    !trajectory_is_named_list(payload) ||
      !is.list(payload$bundle) ||
      !identical(payload$bundle$schema_version, trajectory_schema_version)
  ) {
    review_abort("Unsupported review evidence schema.")
  }
  payload$bundle <- do.call(
    TrajectoryBundle,
    payload$bundle[setdiff(names(payload$bundle), "schema_version")]
  )
  out <- structure(payload, class = "scans_investigation")
  investigation_validate(out)
  out
}

review_validate <- function(x) {
  investigation_check_dependencies()
  fields <- c(
    "format_version",
    "origin",
    "evidence",
    "trajectory_id",
    "finding_id",
    "scan",
    "event_ids",
    "turn_ids",
    "decisions",
    "case_id"
  )
  if (
    !inherits(x, "scans_review_case") ||
      !is.list(x) ||
      !setequal(names(x), fields) ||
      !identical(x$format_version, 1L)
  ) {
    review_abort("Expected a review case with a supported format.")
  }
  review_string(x$trajectory_id, "trajectory_id")
  review_string(x$case_id, "case_id")
  if (
    !trajectory_is_named_list(x$origin) ||
      !identical(names(x$origin), c("snapshot_id", "revision_id")) ||
      !all(vapply(
        x$origin,
        function(id) {
          rlang::is_string(id) &&
            !is.na(id) &&
            grepl("^sha256:[0-9a-f]{64}$", id)
        },
        logical(1)
      ))
  ) {
    review_abort("Invalid original investigation identifiers.")
  }
  saved <- review_evidence(x$evidence)
  if (!identical(saved$manifest$parent_revision_id, x$origin$revision_id)) {
    review_abort(
      "The selected evidence does not refer to its original revision."
    )
  }
  if (
    !identical(trajectory_info(saved$bundle)$trajectory_id, x$trajectory_id)
  ) {
    review_abort("A review case must contain only its selected trajectory.")
  }
  if (!is.null(x$scan)) {
    review_string(x$scan, "scan")
    if (!x$scan %in% saved$settings$scans) {
      review_abort("Unknown saved scanner.")
    }
  }
  if (!is.null(x$finding_id)) {
    review_string(x$finding_id, "finding_id")
    findings <- saved$analysis$findings
    row <- match(x$finding_id, findings$finding_id)
    if (is.na(row) || !identical(x$scan, findings$scan[[row]])) {
      review_abort("Invalid finding reference.")
    }
  }
  review_references(
    x$event_ids,
    trajectory_events(saved$bundle),
    "event_id",
    x$trajectory_id
  )
  review_references(
    x$turn_ids,
    trajectory_turns(saved$bundle),
    "turn_id",
    x$trajectory_id
  )
  if (
    !identical(
      x$case_id,
      investigation_hash(unclass(x)[setdiff(
        names(x),
        c("case_id", "decisions")
      )])
    )
  ) {
    review_abort("The case identifier does not match its evidence.")
  }
  if (!is.list(x$decisions)) {
    review_abort("Invalid decision history.")
  }
  prior <- list()
  for (i in seq_along(x$decisions)) {
    d <- x$decisions[[i]]
    if (
      !trajectory_is_named_list(d) ||
        !setequal(
          names(d),
          c(
            "case_id",
            "judgment",
            "reviewer",
            "rationale",
            "expected_behavior",
            "supersedes",
            "sequence",
            "created_at",
            "decision_id"
          )
        )
    ) {
      review_abort("Invalid decision record.")
    }
    review_choice(
      d$judgment,
      c("confirmed", "rejected", "uncertain", "missed", "insufficient_capture"),
      "judgment"
    )
    review_string(d$reviewer, "reviewer")
    review_string(d$rationale, "rationale")
    if (!is.null(d$expected_behavior)) {
      review_string(d$expected_behavior, "expected_behavior")
    }
    if (
      !identical(d$case_id, x$case_id) ||
        !identical(d$sequence, i) ||
        !inherits(d$created_at, "POSIXct") ||
        length(d$created_at) != 1L ||
        is.na(d$created_at) ||
        !is.character(d$supersedes) ||
        anyNA(d$supersedes) ||
        anyDuplicated(d$supersedes) ||
        !all(d$supersedes %in% review_active(prior)) ||
        (d$judgment == "missed" && !is.null(x$finding_id)) ||
        !identical(
          d$decision_id,
          investigation_hash(d[setdiff(names(d), "decision_id")])
        )
    ) {
      review_abort(
        "The decision history has invalid references or identifiers."
      )
    }
    prior <- c(prior, list(d))
  }
  invisible(x)
}

review_references <- function(ids, rows, column, trajectory_id) {
  if (
    !is.character(ids) ||
      anyNA(ids) ||
      anyDuplicated(ids) ||
      !all(ids %in% rows[[column]][rows$trajectory_id == trajectory_id])
  ) {
    review_abort("Evidence references must belong to the selected trajectory.")
  }
}

review_string <- function(x, name) {
  if (!rlang::is_string(x) || is.na(x) || !nzchar(trimws(x))) {
    review_abort("{.arg {name}} must be one non-empty string.")
  }
}

review_choice <- function(x, choices, name) {
  review_string(x, name)
  if (!x %in% choices) {
    review_abort("{.arg {name}} must be one of {.val {choices}}.")
  }
}

review_abort <- function(message) {
  scans_abort(
    message,
    class = "scans_error_review",
    call = rlang::caller_env(),
    .envir = rlang::caller_env()
  )
}
