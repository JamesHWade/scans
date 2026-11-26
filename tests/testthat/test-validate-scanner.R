test_that("each split reports its own agreement, denominators, and unscored cases", {
  result <- validate_scanner(
    scanner_keyword_fixture(),
    scanner_validation_fixture()
  )
  cases <- result$cases

  expect_identical(
    stats::setNames(cases$outcome, cases$trajectory_id),
    c(
      "dev-two" = "true_positive",
      "dev-two" = "false_positive",
      "dev-clean" = "true_negative",
      "dev-unsure" = "reviewer_uncertain",
      "dev-disputed" = "not_adjudicated",
      "test-missed" = "false_negative",
      "test-hit" = "true_positive",
      "test-empty" = "scanner_unassessed",
      "test-unreviewed" = "not_adjudicated"
    )
  )
  summary <- result$summary
  expect_identical(summary$split, c("development", "test"))
  expect_identical(summary$n_cases, c(5L, 4L))
  expect_identical(summary$scored, c(3L, 2L))
  expect_identical(summary$true_positive, c(1L, 1L))
  expect_identical(summary$false_positive, c(1L, 0L))
  expect_identical(summary$false_negative, c(0L, 1L))
  expect_identical(summary$true_negative, c(1L, 0L))
  expect_identical(summary$precision, c(0.5, 1))
  expect_identical(summary$precision_denominator, c(2L, 1L))
  expect_identical(summary$recall, c(1, 0.5))
  expect_identical(summary$recall_denominator, c(1L, 2L))
  expect_identical(summary$scanner_unassessed, c(0L, 1L))
  expect_identical(summary$insufficient_evidence, c(0L, 1L))
  expect_identical(summary$reviewer_uncertain, c(1L, 0L))
  expect_identical(summary$not_adjudicated, c(1L, 1L))
  # The uncertain and disputed cases' findings have no scored judgment.
  expect_identical(summary$unreviewed_findings, c(2L, 0L))
  expect_identical(
    summary$scored +
      summary$scanner_unassessed +
      summary$reviewer_uncertain +
      summary$reviewer_insufficient_capture +
      summary$not_adjudicated,
    summary$n_cases
  )
})

test_that("the case table links each outcome to review evidence and cited events", {
  examples <- scanner_validation_fixture()
  cases <- validate_scanner(scanner_keyword_fixture(), examples)$cases
  missed <- cases[cases$outcome == "false_negative", ]
  wrong <- cases[cases$outcome == "false_positive", ]

  expect_identical(missed$review_event_ids, list("test-missed-e2"))
  expect_identical(missed$finding_ids, list(character()))
  expect_identical(missed$scanner_event_ids, list(character()))
  expect_identical(missed$judgment, "missed")
  expect_false(missed$passed)

  expect_identical(wrong$review_event_ids, list("dev-two-e3"))
  expect_identical(wrong$scanner_event_ids, list(c("dev-two-e2", "dev-two-e3")))
  expect_identical(
    wrong$finding_ids,
    list("scan-000001/toy_keyword/finding-000002")
  )
  expect_identical(wrong$review_finding_id, wrong$finding_ids[[1L]])
  # Saved identifiers are numbered across the whole snapshot, so a case on a
  # later trajectory checks that the rerun's findings take the saved ones.
  hit <- cases[cases$trajectory_id == "test-hit", ]
  expect_identical(
    hit$review_finding_id,
    "scan-000001/toy_keyword/finding-000005"
  )
  expect_identical(hit$finding_ids, list(hit$review_finding_id))
  expect_identical(
    examples$cases[[wrong$case]]$case_id,
    wrong$case_id
  )
  expect_identical(
    wrong$revision_id,
    examples$cases[[wrong$case]]$origin$revision_id
  )
  expect_length(wrong$decision_ids[[1L]], 1L)

  unassessed <- cases[cases$outcome == "scanner_unassessed", ]
  expect_identical(unassessed$assessment, "insufficient_evidence")
  expect_identical(
    unassessed$assessment_reason,
    "No conversation events are recorded."
  )
  expect_true(is.na(unassessed$flagged))
  expect_true(is.na(unassessed$count))
  expect_true(is.na(unassessed$passed))
})

test_that("a new scanner version is scored on the same reviewed cases", {
  examples <- scanner_validation_fixture()
  stricter <- scanner_keyword_fixture(
    "(?<!not )guaranteed|promise",
    version = "2"
  )
  stricter$fn <- function(trajectory, settings) {
    events <- trajectory$events
    if (nrow(events) == 0L) {
      return(scanner_unassessed("insufficient_evidence", "No events."))
    }
    hits <- events[
      events$event_index > 1L &
        grepl(settings$keyword, events$text, ignore.case = TRUE, perl = TRUE),
    ]
    scanner_findings(
      event_id = hits$event_id,
      label = rep("Keyword", nrow(hits)),
      explanation = rep("Keyword.", nrow(hits))
    )
  }
  result <- validate_scanner(stricter, examples)

  expect_identical(result$summary$false_positive, c(0L, 0L))
  expect_identical(result$summary$false_negative, c(0L, 0L))
  expect_identical(result$summary$precision, c(1, 1))
  expect_identical(unique(result$cases$scan_version), "2")
  expect_identical(unique(result$cases$reviewed_scan_version), "1")
  missed <- result$cases[result$cases$trajectory_id == "test-missed", ]
  expect_identical(
    missed$finding_ids,
    list("rerun/toy_keyword/finding-000001")
  )
})

test_that("a throwing scanner is counted as unassessed, never as a negative", {
  examples <- scanner_validation_fixture()
  broken <- scanner_keyword_fixture()
  inner <- broken$fn
  broken$fn <- function(trajectory, settings) {
    if (
      trajectory$trajectory_id %in% c("test-hit", "dev-clean", "dev-unsure")
    ) {
      stop("database password=hunter2")
    }
    inner(trajectory, settings)
  }
  result <- validate_scanner(broken, examples)

  expect_identical(result$summary$execution_failure, c(2L, 1L))
  expect_identical(result$summary$scanner_unassessed, c(2L, 2L))
  expect_identical(result$summary$true_negative, c(0L, 0L))
  expect_identical(result$summary$reviewer_uncertain, c(0L, 0L))
  expect_identical(result$summary$recall, c(1, 0))
  expect_identical(result$summary$recall_denominator, c(1L, 1L))
  expect_true(is.na(result$summary$precision[[2L]]))
  expect_identical(
    result$summary$precision_note[[2L]],
    "No scored case was flagged, so precision is undefined."
  )
  expect_false(any(grepl("hunter2", unlist(result), fixed = TRUE)))
})

test_that("a rate with no denominator is NA with its reason", {
  examples <- scanner_validation_fixture()
  silent <- validate_scanner(scanner_keyword_fixture("zzz"), examples)$summary
  expect_identical(silent$precision, c(NA_real_, NA_real_))
  expect_identical(silent$precision_denominator, c(0L, 0L))
  expect_identical(silent$recall, c(0, 0))
  expect_false(anyNA(silent$precision_note))
  expect_true(all(is.na(silent$recall_note)))

  negatives <- examples$cases[c(2L, 3L)]
  negatives <- lapply(negatives, structure, class = "scans_review_case")
  only_negative <- review_examples(
    negatives,
    "scanner",
    groups = c("dev-two", "dev-clean"),
    splits = c("development", "development"),
    predicates = rep(list(list(operator = "eq", value = 0L)), 2L)
  )
  summary <- validate_scanner(scanner_keyword_fixture(), only_negative)$summary
  expect_identical(summary$recall, c(NA_real_, NA_real_))
  expect_identical(
    summary$recall_note,
    rep("No scored case was reviewed as positive, so recall is undefined.", 2L)
  )
  expect_identical(summary$n_cases, c(2L, 0L))
  expect_identical(summary$unreviewed_findings, c(1L, 0L))
})

test_that("cases from two revisions of one snapshot count a finding once", {
  bundle <- scanner_chat_bundle(list("dev-a" = "It is guaranteed today."))
  first <- investigation_snapshot(
    bundle,
    scans = "toy_keyword",
    scanners = scanner_keyword_fixture(),
    application = "Parcel support"
  )
  second <- investigation_snapshot(
    bundle,
    scans = "toy_keyword",
    scanners = scanner_keyword_fixture("zzz"),
    application = "Parcel support",
    previous = first
  )
  expect_identical(first$manifest$snapshot_id, second$manifest$snapshot_id)
  expect_identical(nrow(second$analysis$findings), 0L)
  examples <- review_examples(
    list(
      review_case(
        first,
        "dev-a",
        first$analysis$findings$finding_id[[1L]]
      ),
      review_case(second, "dev-a", scan = "toy_keyword")
    ),
    "scanner",
    groups = c("a", "b"),
    splits = rep("development", 2L),
    predicates = rep(list(list(operator = "gte", value = 1L)), 2L)
  )
  result <- validate_scanner(scanner_keyword_fixture(), examples)
  expect_false(identical(
    result$cases$finding_ids[[1L]],
    result$cases$finding_ids[[2L]]
  ))
  expect_identical(result$summary$unreviewed_findings, c(1L, 0L))
})

test_that("the trajectory unit ignores which evidence a case cites", {
  examples <- scanner_validation_fixture()
  elsewhere <- scanner_keyword_fixture("parcel")
  elsewhere$fn <- function(trajectory, settings) {
    events <- trajectory$events
    if (nrow(events) == 0L) {
      return(scanner_unassessed("insufficient_evidence", "No events."))
    }
    scanner_findings(
      event_id = events$event_id[[1L]],
      label = "Question",
      explanation = "Flags the customer's question."
    )
  }
  by_evidence <- validate_scanner(elsewhere, examples)
  by_trajectory <- validate_scanner(elsewhere, examples, unit = "trajectory")

  expect_identical(by_evidence$summary$true_positive, c(0L, 0L))
  expect_identical(by_evidence$summary$false_negative, c(1L, 2L))
  expect_identical(by_evidence$summary$false_positive, c(1L, 0L))
  expect_identical(by_evidence$summary$unreviewed_findings, c(3L, 3L))
  expect_identical(by_trajectory$summary$true_positive, c(1L, 2L))
  expect_identical(by_trajectory$summary$false_positive, c(2L, 0L))
  expect_identical(by_trajectory$summary$unreviewed_findings, c(2L, 1L))
})

test_that("validation needs scanner cases that target the scanner", {
  examples <- scanner_validation_fixture()
  other <- scanner_toy_fixture("toy_other", function(trajectory, settings) {
    scanner_findings()
  })
  expect_error(validate_scanner(other, examples), class = "scans_error_review")
  expect_error(
    validate_scanner("toy_keyword", examples),
    class = "scans_error_review"
  )
  expect_error(
    validate_scanner(scanner_keyword_fixture(), examples, unit = "finding"),
    class = "scans_error_review"
  )
})

test_that("predicate checks run a supplied custom scanner", {
  examples <- scanner_validation_fixture()
  checked <- validate_review_examples(
    examples,
    "test",
    scanners = scanner_keyword_fixture()
  )
  expect_identical(
    checked$assessment,
    c(
      "assessed_no_findings",
      "assessed_with_findings",
      "insufficient_evidence",
      "assessed_no_findings"
    )
  )
  expect_identical(checked$passed, c(FALSE, TRUE, NA, NA))
  expect_identical(unique(checked$scan_version), "1")
  expect_identical(unique(checked$reviewed_scan_version), "1")

  newer <- validate_review_examples(
    examples,
    "test",
    scanners = scanner_keyword_fixture(version = "2")
  )
  expect_identical(unique(newer$scan_version), "2")
  expect_identical(unique(newer$reviewed_scan_version), "1")

  missing <- validate_review_examples(examples, "test")
  expect_identical(unique(missing$assessment), "scanner_not_supplied")
  expect_identical(unique(missing$reviewed_scan_version), "1")
  expect_true(all(is.na(missing$passed)))
  thrower <- scanner_toy_fixture("toy_keyword", function(trajectory, settings) {
    stop("boom")
  })
  expect_identical(
    unique(validate_review_examples(examples, "test", thrower)$assessment),
    "execution_failure"
  )
})
