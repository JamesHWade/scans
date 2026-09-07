# Review exact trajectory evidence

`review_case()` freezes one trajectory from an investigation for human
review. It retains the original snapshot and revision identifiers and a
selected investigation containing only that trajectory and relevant
losses. `review_decision()` appends a decision without changing earlier
decisions. These functions never call a model or select content for an
example set.

## Usage

``` r
review_case(
  x,
  trajectory_id,
  finding_id = NULL,
  scan = NULL,
  event_ids = character(),
  turn_ids = character()
)

review_decision(
  x,
  judgment,
  reviewer,
  rationale,
  expected_behavior = NULL,
  supersedes = character()
)

review_status(x)
```

## Arguments

- x:

  An
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
  for `review_case()`, or a review case for `review_decision()` and
  `review_status()`.

- trajectory_id:

  One trajectory in the investigation.

- finding_id:

  Optional finding in that trajectory. Leave `NULL` to review an
  unflagged trajectory or record a missed finding.

- scan:

  Optional scanner name from the saved analysis. Required to record a
  scanner target for an unflagged trajectory. Inferred from a finding.

- event_ids, turn_ids:

  Optional evidence references within the trajectory.

- judgment:

  One of `"confirmed"`, `"rejected"`, `"uncertain"`, `"missed"`, or
  `"insufficient_capture"`.

- reviewer:

  One non-empty reviewer name.

- rationale:

  One non-empty explanation about the reviewed evidence.

- expected_behavior:

  Optional description of the intended future behavior.

- supersedes:

  Decision identifiers to adjudicate. Only currently active decisions
  can be superseded. Defaults to none, creating an independent review.

## Value

`review_case()` and `review_decision()` return a `scans_review_case`
value with selected evidence, provenance, and append-only decision
history. `review_status()` returns a list with `status` and active
`decision_ids`.

## Details

A scanner judgment describes whether a finding is valid. An application
expectation describes what a future execution should do. They are
separate: the rationale for an old output is not a score for a new
prediction.

Case identifiers exclude the evidence copy's creation time, so reviewing
the same evidence selection again addresses the same case. Decisions
have content identifiers. Independent decisions remain active until an
adjudicator explicitly supersedes them. Multiple active decisions are
disputed, even if they agree. Uncertain and insufficient-capture
decisions cannot become evaluation targets. Reviewer names are supplied
labels, not authenticated identities. Hashes detect edits, not
authorship.
