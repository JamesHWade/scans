# Validate a custom scanner against reviewed cases

`validate_scanner()` runs a
[`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
on the retained evidence of each reviewed case that targets it and
compares the result with the adjudicated judgment. It reports
development and test cases separately, and it keeps cases that cannot be
scored out of every rate. It never calls a model and changes neither the
saved findings nor the review decisions.

## Usage

``` r
validate_scanner(scanner, examples, unit = "evidence")
```

## Arguments

- scanner:

  A
  [`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md).

- examples:

  A scanner example set from
  [`review_examples()`](https://jameshwade.github.io/scans/reference/review_examples.md).
  Cases that target another scanner are left out.

- unit:

  `"evidence"` (the default) or `"trajectory"`.

## Value

A list with two tibbles.

`summary` has one row per split, always `development` then `test`:
`scan`, `scan_version`, `n_cases`, `scored`, `true_positive`,
`false_positive`, `false_negative`, `true_negative`, `precision` with
`precision_denominator` (flagged scored cases), `recall` with
`recall_denominator` (reviewed-positive scored cases), `precision_note`
and `recall_note`, `scanner_unassessed` with its parts
`insufficient_evidence`, `not_applicable`, and `execution_failure`,
`reviewer_uncertain`, `reviewer_insufficient_capture`,
`not_adjudicated`, and `unreviewed_findings`. A rate with a zero
denominator is `NA` and its note says why.

`cases` has one row per case: `case` (its position in `examples$cases`),
`case_id`, `split`, `group`, `trajectory_id`, `judgment`, `reviewed`
(`"positive"`, `"negative"`, or `NA`), `assessment` and
`assessment_reason` from the scanner, `flagged`, `outcome`, `count` of
agreeing findings, the case's `operator`, `target`, and whether its
predicate `passed`, `scan_version` and the `reviewed_scan_version` the
reviewer saw, the review evidence (`review_finding_id`,
`review_event_ids`, `review_turn_ids`, `decision_ids`, `snapshot_id`,
`revision_id`), and what the scanner cited (`finding_ids` for the
agreeing findings, `other_finding_ids` for its other findings in the
trajectory, and `scanner_event_ids` for every event it cited there, and
`finding_keys` and `other_finding_keys`, the revision-independent
identities the summary counts by). A rerun finding that cites the same
turn and events as a saved finding of the scan carries that saved
identifier, the one `review_finding_id` uses; a finding the saved
analysis does not hold, such as one a new version adds, keeps an
identifier from the rerun that starts with `"rerun/"`. `outcome` is
`true_positive`, `false_positive`, `false_negative`, `true_negative`, or
one of the unscored categories.

## Details

A case is reviewed positive when its active decision is `"confirmed"` or
`"missed"`, and reviewed negative when it is `"rejected"`. A case is
scored only when it has one such decision and the scanner assessed the
trajectory. The other cases are counted separately and are never treated
as negatives:

- `scanner_unassessed`: the scanner reported `insufficient_evidence` or
  `not_applicable`, or failed (`execution_failure`). Each has its own
  count. This category comes first, whatever the review says, so every
  scanner failure is counted.

- `reviewer_uncertain` and `reviewer_insufficient_capture`: the reviewer
  could not judge the evidence.

- `not_adjudicated`: the case is unreviewed, or has several active
  decisions that nobody has adjudicated.

Every case has exactly one outcome, so the scored and unscored counts
add up to `n_cases`.

## Unit of agreement

The unit is the reviewed case. With `unit = "evidence"` (the default)
the scanner agrees with a case only through findings that cite the
case's evidence: a finding counts when it shares an event with the
case's `event_ids` and, when the case names turns, lies in one of its
`turn_ids`. A case that cites no evidence is a whole-trajectory
judgment, and any finding in the trajectory counts. Findings elsewhere
in the trajectory are not scored for that case. `unreviewed_findings`
counts the findings in the split's trajectories that agree with no
scored case: no positive or negative judgment covers them, either
because no case cites them or because the cases that do are uncertain,
insufficient, unreviewed, or disputed. They are neither true nor false
positives. With `unit = "trajectory"` any finding in the trajectory
counts for every case, whatever evidence the case cites. Use it only
when each trajectory has one case, since two cases on one trajectory
would both be scored against the same flag.

## Development and test cases

Development cases are for building the scanner: read its false positives
and false negatives, change the function or its settings, and run again.
Test cases are held out. Look at them once the scanner is settled, and
do not change the scanner in response to them; a test result that has
guided a change is a development result. The function reports both and
tunes nothing. A rate measures agreement with these reviewed cases only.
It is not an estimate for unreviewed traffic, and it says nothing about
the quality of the application's answers.
