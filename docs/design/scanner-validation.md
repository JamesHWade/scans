# Validating a scanner against reviewed cases

Issue #43 asks that a detector be validated on held-out reviewed cases, with
false positives, false negatives, precision, recall, and unassessed cases
reported with their denominators. `validate_scanner(scanner, examples)` does
that for a custom scanner (see `custom-scanners.md`) and a scanner example set
from the review contract. It returns two ordinary tibbles, `summary` and
`cases`.

`validate_review_examples()` already checks each case's count predicate. It
stays as it is, and gains a `scanners` argument so its predicates can run a
custom scanner. It answers "does each target hold"; the new function answers
"how often does the scanner agree with the reviewers, and on which cases not".

## Decisions

**The reviewed label comes from the adjudicated judgment.** `confirmed` and
`missed` are reviewed positives. `rejected` is a reviewed negative, whether it
rejects a finding or a whole unflagged trajectory. The count predicate was
rejected as the source of the label: a predicate such as `eq 2` or `lte 3`
has no binary class. Each case's predicate result is still reported in the
`passed` column.

**The unit of agreement is the reviewed case, matched on its evidence.** A
case is a trajectory, a scanner, and optionally the events and turns the
reviewer cited. By default the scanner agrees with a case only through
findings that share an event with the case's `event_ids` and, when the case
names turns, lie in its `turn_ids`. A case with no references is a
whole-trajectory judgment, so any finding counts. This is the rule
`validate_review_examples()` already uses to count findings, so the two
functions cannot disagree about what a case covers.

A trajectory-level flag as the only unit was rejected for two reasons. A
scanner that flags the right trajectory for the wrong reason would be credited
with a true positive. And one trajectory can hold a confirmed finding and a
rejected one; with a single flag per trajectory both cases get the same answer
and one of them is necessarily scored wrongly. `unit = "trajectory"` is
offered as an option for example sets with one whole-trajectory case each,
where the reviewer judged the run rather than a span of it.

**Unreviewed findings are neither true nor false positives.** A finding that
agrees with no case in the split was never reviewed. It is counted in
`unreviewed_findings` and listed per case in `other_finding_ids`, so the
reviewer can open it and add a case. Counting it as a false positive would
punish a scanner for finding something nobody had looked at.

**Unscored cases are a partition, and scanner failure comes first.** Every
case has one outcome. `scanner_unassessed` (with separate counts for
`insufficient_evidence`, `not_applicable`, and `execution_failure`) is decided
before the review status, so a scanner that fails on an uncertain case still
shows up as a failure. Then `not_adjudicated` (unreviewed or disputed),
`reviewer_uncertain`, and `reviewer_insufficient_capture`. Only the remaining
cases enter the four scored cells. None of the unscored cases is a negative.

**Undefined rates are `NA` with a note.** Precision with no flagged scored
case, or recall with no reviewed-positive scored case, is `NA`, and
`precision_note` or `recall_note` says which denominator was zero. The
denominators are columns of their own.

**Both splits are always reported, and nothing is tuned.** `summary` has a
`development` row and a `test` row even when one is empty, so a missing
held-out set is visible as zero cases rather than absent. There is no helper
that searches settings: choosing them is the author's work on development
cases, and the test row is only meaningful if it did not guide that work.

**The scanner object is the version under test.** Cases carry the evidence
bundle the reviewer saw, not scanner code. Validation runs the supplied
scanner on that bundle and reports its version beside the version recorded in
the case's saved analysis, so a result for version 2 on cases reviewed against
version 1 is labelled as such.

## Not included

- Validating built-in scans this way. Their cases can already be checked with
  predicates; a confusion matrix for them needs a decision about thresholds
  that this issue does not ask for.
- Confidence intervals. The reviewed sets this is meant for are small, and an
  interval would suggest a sampling design that reviewed cases do not have.
- Any combined score.
