#' Validate a custom scanner against reviewed cases
#'
#' `validate_scanner()` runs a [scanner()] on the retained evidence of each
#' reviewed case that targets it and compares the result with the adjudicated
#' judgment. It reports development and test cases separately, and it keeps
#' cases that cannot be scored out of every rate. It never calls a model and
#' changes neither the saved findings nor the review decisions.
#'
#' A case is reviewed positive when its active decision is `"confirmed"` or
#' `"missed"`, and reviewed negative when it is `"rejected"`. A case is scored
#' only when it has one such decision and the scanner assessed the trajectory.
#' The other cases are counted separately and are never treated as negatives:
#'
#' - `scanner_unassessed`: the scanner reported `insufficient_evidence` or
#'   `not_applicable`, or failed (`execution_failure`). Each has its own count.
#'   This category comes first, whatever the review says, so every scanner
#'   failure is counted.
#' - `reviewer_uncertain` and `reviewer_insufficient_capture`: the reviewer
#'   could not judge the evidence.
#' - `not_adjudicated`: the case is unreviewed, or has several active
#'   decisions that nobody has adjudicated.
#'
#' Every case has exactly one outcome, so the scored and unscored counts add
#' up to `n_cases`.
#'
#' @section Unit of agreement:
#' The unit is the reviewed case. With `unit = "evidence"` (the default) the
#' scanner agrees with a case only through findings that cite the case's
#' evidence: a finding counts when it shares an event with the case's
#' `event_ids` and, when the case names turns, lies in one of its `turn_ids`.
#' A case that cites no evidence is a whole-trajectory judgment, and any
#' finding in the trajectory counts. Findings elsewhere in the trajectory are
#' not scored for that case. `unreviewed_findings` counts the findings in the
#' split's trajectories that agree with no scored case: no positive or
#' negative judgment covers them, either because no case cites them or
#' because the cases that do are uncertain, insufficient, unreviewed, or
#' disputed. They are neither true nor false positives. With
#' `unit = "trajectory"` any finding in the trajectory counts for
#' every case, whatever evidence the case cites. Use it only when each
#' trajectory has one case, since two cases on one trajectory would both be
#' scored against the same flag.
#'
#' @section Development and test cases:
#' Development cases are for building the scanner: read its false positives
#' and false negatives, change the function or its settings, and run again.
#' Test cases are held out. Look at them once the scanner is settled, and do
#' not change the scanner in response to them; a test result that has guided
#' a change is a development result. The function reports both and tunes
#' nothing. A rate measures agreement with these reviewed cases only. It is
#' not an estimate for unreviewed traffic, and it says nothing about the
#' quality of the application's answers.
#'
#' @param scanner A [scanner()].
#' @param examples A scanner example set from [review_examples()]. Cases that
#'   target another scanner are left out.
#' @param unit `"evidence"` (the default) or `"trajectory"`.
#'
#' @returns A list with two tibbles.
#'
#' `summary` has one row per split, always `development` then `test`:
#' `scan`, `scan_version`, `n_cases`, `scored`, `true_positive`,
#' `false_positive`, `false_negative`, `true_negative`, `precision` with
#' `precision_denominator` (flagged scored cases), `recall` with
#' `recall_denominator` (reviewed-positive scored cases), `precision_note` and
#' `recall_note`, `scanner_unassessed` with its parts `insufficient_evidence`,
#' `not_applicable`, and `execution_failure`, `reviewer_uncertain`,
#' `reviewer_insufficient_capture`, `not_adjudicated`, and
#' `unreviewed_findings`. A rate with a zero denominator is `NA` and its note
#' says why.
#'
#' `cases` has one row per case: `case` (its position in `examples$cases`),
#' `case_id`, `split`, `group`, `trajectory_id`, `judgment`, `reviewed`
#' (`"positive"`, `"negative"`, or `NA`), `assessment` and
#' `assessment_reason` from the scanner, `flagged`, `outcome`, `count` of
#' agreeing findings, the case's `operator`, `target`, and whether its
#' predicate `passed`, `scan_version` and the `reviewed_scan_version` the
#' reviewer saw, the review evidence (`review_finding_id`, `review_event_ids`,
#' `review_turn_ids`, `decision_ids`, `snapshot_id`, `revision_id`), and what
#' the scanner cited (`finding_ids` for the agreeing findings,
#' `other_finding_ids` for its other findings in the trajectory, and
#' `scanner_event_ids` for every event it cited there). A rerun finding that
#' cites the same turn and events as a saved finding of the scan carries
#' that saved identifier, the one `review_finding_id` uses; a finding the
#' saved analysis does not hold, such as one a new version adds, keeps an
#' identifier from the rerun that starts with `"rerun/"`. `outcome` is `true_positive`, `false_positive`,
#' `false_negative`, `true_negative`, or one of the unscored categories.
#' @export
validate_scanner <- function(scanner, examples, unit = "evidence") {
  if (!inherits(scanner, "scans_scanner")) {
    review_abort("{.arg scanner} must be created with {.fn scanner}.")
  }
  review_choice(unit, c("evidence", "trajectory"), "unit")
  review_examples_validate(examples)
  if (examples$purpose != "scanner") {
    review_abort("Scanner validation requires scanner examples.")
  }
  rows <- which(vapply(
    examples$cases,
    function(case) identical(case$scan, scanner$name),
    logical(1)
  ))
  if (!length(rows)) {
    name <- scanner$name
    review_abort("No reviewed case targets the scanner {.val {name}}.")
  }
  cases <- trajectory_bind_rows(lapply(rows, function(i) {
    validate_scanner_case(scanner, examples, i, unit)
  }))
  list(
    summary = trajectory_bind_rows(lapply(
      c("development", "test"),
      function(split) {
        validate_scanner_summary(scanner, cases[cases$split == split, ], split)
      }
    )),
    cases = cases
  )
}

validate_scanner_case <- function(scanner, examples, i, unit) {
  item <- structure(examples$cases[[i]], class = "scans_review_case")
  saved <- review_evidence(item$evidence)
  status <- review_status(item)
  judgment <- status$status
  reviewed <- if (judgment %in% c("confirmed", "missed")) {
    "positive"
  } else if (judgment == "rejected") {
    "negative"
  } else {
    NA_character_
  }
  result <- assess_trajectory_scans(
    saved$bundle,
    scan_id = "rerun",
    scans = scanner$name,
    scanners = scanner
  )
  assessment <- result$assessments$status[[1L]]
  assessed <- assessment %in%
    c("assessed_with_findings", "assessed_no_findings")
  findings <- result$findings
  findings$finding_id <- validate_scanner_saved_ids(
    findings,
    saved$analysis$findings
  )
  agreeing <- rep(TRUE, nrow(findings))
  if (unit == "evidence") {
    if (length(item$event_ids)) {
      agreeing <- vapply(
        findings$event_ids,
        function(ids) any(ids %in% item$event_ids),
        logical(1)
      )
    }
    if (length(item$turn_ids)) {
      agreeing <- agreeing & findings$turn_id %in% item$turn_ids
    }
  }
  count <- if (assessed) sum(agreeing) else NA_integer_
  flagged <- if (assessed) count > 0L else NA
  # A scanner that could not run is counted first, so no failure is hidden
  # behind an unresolved review.
  outcome <- if (!assessed) {
    "scanner_unassessed"
  } else if (judgment %in% c("unreviewed", "disputed")) {
    "not_adjudicated"
  } else if (judgment == "uncertain") {
    "reviewer_uncertain"
  } else if (judgment == "insufficient_capture") {
    "reviewer_insufficient_capture"
  } else if (reviewed == "positive") {
    if (flagged) "true_positive" else "false_negative"
  } else {
    if (flagged) "false_positive" else "true_negative"
  }
  predicate <- examples$predicates[[i]]
  passed <- if (assessed && !is.na(reviewed)) {
    switch(
      predicate$operator,
      eq = count == predicate$value,
      gte = count >= predicate$value,
      lte = count <= predicate$value
    )
  } else {
    NA
  }
  versions <- saved$manifest$scanner_versions
  tibble::tibble(
    case = i,
    case_id = item$case_id,
    split = examples$splits[[i]],
    group = examples$groups[[i]],
    trajectory_id = item$trajectory_id,
    judgment = judgment,
    reviewed = reviewed,
    assessment = assessment,
    assessment_reason = result$assessments$reason[[1L]],
    flagged = flagged,
    outcome = outcome,
    count = count,
    operator = predicate$operator,
    target = as.integer(predicate$value),
    passed = passed,
    scan = scanner$name,
    scan_version = scanner$version,
    reviewed_scan_version = versions$scan_version[
      match(scanner$name, versions$scan)
    ],
    review_finding_id = item$finding_id %||% NA_character_,
    review_event_ids = list(item$event_ids),
    review_turn_ids = list(item$turn_ids),
    decision_ids = list(status$decision_ids),
    snapshot_id = item$origin$snapshot_id,
    revision_id = item$origin$revision_id,
    finding_ids = list(findings$finding_id[agreeing]),
    scanner_event_ids = list(
      unique(unlist(
        findings$event_ids,
        use.names = FALSE
      )) %||%
        character()
    ),
    other_finding_ids = list(findings$finding_id[!agreeing])
  )
}

validate_scanner_summary <- function(scanner, cases, split) {
  n <- function(outcome) sum(cases$outcome == outcome)
  unassessed <- function(status) {
    sum(cases$outcome == "scanner_unassessed" & cases$assessment == status)
  }
  scored_outcomes <- c(
    "true_positive",
    "false_positive",
    "false_negative",
    "true_negative"
  )
  tp <- n("true_positive")
  fp <- n("false_positive")
  fn <- n("false_negative")
  tn <- n("true_negative")
  tibble::tibble(
    split = split,
    scan = scanner$name,
    scan_version = scanner$version,
    n_cases = nrow(cases),
    scored = tp + fp + fn + tn,
    true_positive = tp,
    false_positive = fp,
    false_negative = fn,
    true_negative = tn,
    precision = if (tp + fp > 0L) tp / (tp + fp) else NA_real_,
    precision_denominator = tp + fp,
    precision_note = if (tp + fp > 0L) {
      NA_character_
    } else {
      "No scored case was flagged, so precision is undefined."
    },
    recall = if (tp + fn > 0L) tp / (tp + fn) else NA_real_,
    recall_denominator = tp + fn,
    recall_note = if (tp + fn > 0L) {
      NA_character_
    } else {
      "No scored case was reviewed as positive, so recall is undefined."
    },
    scanner_unassessed = n("scanner_unassessed"),
    insufficient_evidence = unassessed("insufficient_evidence"),
    not_applicable = unassessed("not_applicable"),
    execution_failure = unassessed("execution_failure"),
    reviewer_uncertain = n("reviewer_uncertain"),
    reviewer_insufficient_capture = n("reviewer_insufficient_capture"),
    not_adjudicated = n("not_adjudicated"),
    unreviewed_findings = length(setdiff(
      c(
        validate_scanner_keys(cases, "other_finding_ids"),
        validate_scanner_keys(cases, "finding_ids")
      ),
      validate_scanner_keys(
        cases[cases$outcome %in% scored_outcomes, ],
        "finding_ids"
      )
    ))
  )
}

# Saved identifiers are unique within a snapshot and rerun identifiers within
# a trajectory, so a finding is identified with both.
validate_scanner_keys <- function(cases, column) {
  unique(unlist(
    lapply(seq_len(nrow(cases)), function(i) {
      ids <- cases[[column]][[i]]
      if (length(ids)) {
        paste(cases$snapshot_id[[i]], cases$trajectory_id[[i]], ids)
      }
    }),
    use.names = FALSE
  ))
}

# The rerun numbers its findings within this case's evidence, while the
# review cites the saved snapshot's identifiers. A rerun finding takes the
# identifier of the saved finding of the same scan that cites the same turn
# and events; repeats are paired in order. A finding the saved analysis does
# not hold keeps its rerun identifier, which starts with "rerun/".
validate_scanner_saved_ids <- function(findings, saved) {
  key <- function(x) {
    evidence <- vapply(
      x$event_ids,
      function(ids) paste(sort(ids, method = "radix"), collapse = "\r"),
      character(1)
    )
    base <- paste(x$scan, x$turn_id, x$event_id, evidence, sep = "\n")
    paste(base, stats::ave(seq_along(base), base, FUN = seq_along))
  }
  ids <- findings$finding_id
  if (!length(ids)) {
    return(ids)
  }
  match <- match(key(findings), key(saved))
  ids[!is.na(match)] <- saved$finding_id[match[!is.na(match)]]
  ids
}
