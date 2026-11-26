# Matched comparison

Pairing and scorer contract for #42, settled against vitals 0.4.1.
vitals runs and scores evaluations. scans reads two completed records and
explains what changed between them. Nothing here runs a solver, a scorer, or
a model.

## What vitals records

Checked through `Task$get_samples()` and `as_trajectory_vitals()`:

| Fact | Recorded | Where |
| --- | --- | --- |
| Sample identity | Yes | `id`, kept as `sample_id` |
| Epoch | Yes when the task ran more than one | `epoch`; the column is absent for one epoch and the adapter uses `1L` |
| Task identity | No | The caller passes `task_id` to `as_trajectory_vitals()` |
| Dataset version | No | Not in the samples or the `Task` fields |
| Scorer label | Yes | `scorer`: the deparsed expression passed as `scorer`. A function held in a variable named `scorer` is recorded as `"scorer"`; an inline function is recorded as its source text |
| Scorer version | No | |
| Scale and direction | Only for ordered factors | Built-in scorers return an ordered factor (`I < C`, or `I < P < C`); the saved value keeps the levels and their order. Numeric, logical, and character scores carry no direction |

The scorer label names an expression, not a scorer: two different functions
bound to the same variable name share a label, and one function under two
names does not. So the label alone cannot establish identity across runs.

## Cases and sides

A case is one sample of one dataset version: `(dataset version, task,
sample)`. An epoch is a repeat of a case, not a different case.

A baseline and a candidate are saved investigations
(`investigation_snapshot()` or `read_investigation()`). A bare
`TrajectoryBundle` is rejected. A comparison cites evidence, so both sides
need a snapshot identifier, and it compares assessments and measures, so both
sides need the frozen analysis and the scanner settings that produced it. The
pairing records both snapshot and revision identifiers.

## Pairing key

`pair_trajectories(baseline, candidate, by, dataset_version, scorers)` has no
default key. `by` must include `"sample_id"` and may add `"task_id"` and
`"epoch"`. The dataset version is always part of the key and is declared by
the caller, because vitals does not record it: one string for both sides, or
one per side. Cases pair only when both sides declare the same version.

Every trajectory of both sides lands in exactly one table:

| Table | Contents |
| --- | --- |
| `pairs` | Key matched by exactly one trajectory on each side |
| `only_baseline`, `only_candidate` | Key present on one side |
| `duplicates` | Every trajectory, on either side, whose key occurs more than once on a side, with both counts |
| `missing_key` | Trajectories lacking a key value, and which value. An undeclared dataset version puts the whole side here |

Keys compare by recorded identity. Row order is never used. A duplicated key
is not resolved by taking the first or latest trajectory.

### Epochs

Decision: with `"epoch"` in the key, epoch `k` pairs with epoch `k`. Without
it, a sample that ran several epochs is a duplicate and is reported.

Epochs of one sample are exchangeable repeats; vitals shares no seed between
epoch `k` of two runs, so the alignment by epoch number is a convention. It
is kept because each row then holds one trajectory per side that a reviewer
can open, which a sample-level row holding two sets of trajectories does not
give. The convention is kept harmless in two ways. The caller must ask for it
by naming `"epoch"`. And when any case has more than one pair, summaries are
counts of epoch pairs with no interval or test (see
[Paired summaries](#paired-summaries)).

Rejected: sample-level pairs with epochs as a replicate set. Every per-case
change would need a reduction rule (any epoch, majority, mean), and each rule
is a second analysis choice hidden inside pairing. Also rejected: pairing a
single-epoch side against the first epoch of a multi-epoch side, which is a
positional match.

## Scorers

Outcomes pair only when scorer identity, version, scale, and direction agree.

`comparison_scorer(scorer, version, scale, direction, levels, id)` declares
what the record lacks for one recorded label. `id` defaults to the label and
lets the caller state that two labels are one scorer. `scale` is `binary`,
`ordered`, or `numeric`; `levels` runs from lowest to highest; `direction`
says which end is better. Declarations apply to both sides, or to each side
separately when the scorer changed.

What is taken from the record without a declaration: the label as identity;
for ordered factor scores, the levels, a binary or ordered scale, and
direction `higher` (the vitals convention that levels run from worst to
best, so a default rather than a recorded fact: a declaration may set
`direction = "lower"` for a scorer whose levels run from best to worst, and
its `levels` must still list the recorded levels in their recorded order); for numeric or logical scores, the scale only. These are read from the
non-missing scores; a missing score records nothing, so one `NA` neither
establishes a scale nor hides one from the check below. When one side records
conflicting scores under one label (ordered factors with different levels, or
an ordered factor beside scores of another type), its scale stays unknown, and a
declaration for that label is an error: no single declaration matches every
record. A mix of other types (numbers, logicals, text) records no scale; a
declaration applies, and values off the declared scale are counted as off-scale. Version is never
recorded. So an undeclared vitals scorer is `unknown`, not compatible: the
caller has to say that the scorer did not change.

A declaration is an assertion scans cannot verify. It can be checked against
the record, and a contradiction (declared numeric, recorded ordered factor;
different levels) is an error. A declared label that neither side recorded is
an error too, so a typo cannot silently leave a scorer undeclared.

`outcome_pairs` has one row per pair and scorer:

| `compatibility` | Meaning |
| --- | --- |
| `compatible` | Same identity, version, scale, direction, and levels |
| `incompatible` | A known attribute differs, or the two sides were scored by different scorers (see below). The reason names the attribute |
| `unknown` | An attribute is neither recorded nor declared, the scorer label is absent, or either trajectory of the pair has several evaluations from one scorer (both sides are then `unknown` for it) |
| `missing` | One side, or both, has no evaluation from this scorer |

A known difference outranks an unknown attribute: levels recorded on both
sides that differ make the row `incompatible` even when the version is
undeclared.

Evaluations of a pair match by scorer identity. When exactly one evaluation
is left on each side, under two different identities, the row is
`incompatible` ("Scorer identity differs") only if neither identity is
recorded anywhere on the other side: the sides then used different scorers.
If either identity is recorded on the other side, that scorer simply has no
evaluation of this case there, and each leftover is `missing`. Pairing the
two leftovers anyway would match them by position.

Only `compatible` rows can carry an outcome change. Nothing is coerced: a
score of `"C"` under one scorer is not compared with `1` under another.

Rejected: treating equal labels as the same scorer (the label is a variable
name); inferring direction for numeric scores from a metric name; a registry
of known scorers (scans would then own scoring facts that belong to vitals).

## Per-case changes

`compare_trajectories(pairing)` reads the saved evaluations, assessments, and
measures of both investigations and returns three families. They are never
combined into one score, and every row carries both trajectory identities.

**Outcomes**, per case and scorer. Only a `compatible` scorer pair whose two
values are on the scale gets a `change` (`improved`, `regressed`,
`unchanged`), read through the scorer's direction. `delta` is candidate minus
baseline for numeric and binary scales and `NA` for ordered scales, whose
steps are not known to be equal.

**Patterns**, per case and scan, from the saved assessments. Every pair of
saved statuses has one change; rows are the baseline, columns the candidate:

| Baseline \ Candidate | with findings | no findings | not applicable | insufficient evidence | execution failure |
| --- | --- | --- | --- | --- | --- |
| with findings | `persisted` | `resolved` | `no_longer_applicable` | `not_comparable` | `not_comparable` |
| no findings | `appeared` | `absent` | `absent` | `not_comparable` | `not_comparable` |
| not applicable | `newly_applicable` | `absent` | `absent` | `not_comparable` | `not_comparable` |
| insufficient evidence | `not_comparable` | `not_comparable` | `not_comparable` | `not_comparable` | `not_comparable` |
| execution failure | `not_comparable` | `not_comparable` | `not_comparable` | `not_comparable` | `not_comparable` |

A scan not selected on a side, a missing assessment, or a different scanner
version or thresholds is `not_comparable` whatever the statuses.

`not_applicable` is an assessment: the scanner determined that the pattern
cannot occur in that capture. Against no findings on the other side it is
`absent`, since neither side has the pattern. Against findings on the other
side it is not a fix or a regression. A pattern found on the baseline whose
scan does not apply to the candidate's capture is `no_longer_applicable`:
the pattern can no longer occur there, which may mean the capture changed
shape (for example, the candidate stopped calling tools) rather than that
anything was repaired. The mirror case, not applicable on the baseline and
found on the candidate, is `newly_applicable`. Both are counted on their own
in `cases`, in the summaries, and in print, and neither feeds
`share_resolved` or its sign test, which compare only `resolved` with
`appeared`.

Rejected: counting `no_longer_applicable` as `resolved` (it would credit a
change with removing a pattern it only made impossible to observe), and
counting it as `not_comparable` (both sides were assessed, and the finding
on the baseline is evidence a reviewer should see).

An unassessed side is never read as clean. Rows carry the finding
identifiers and the event identifiers those findings cite on each side.

**Resources**, per case and measure, from `measure_trajectories()`. A delta
needs, on both sides, a recorded value with `complete` coverage, and the same
unit, aggregation, source, measurement contract version, and cache inclusion.
For `input_tokens` the inclusion must also be known. Otherwise the delta is
`NA` with the reason, and both values, units, coverages, denominators,
sources, and cache-inclusion flags stay on the row. `unit` is the unit the
two sides share and `NA` when they differ, so a row never shows both values
under one side's unit. A summary's median delta needs one unit across its
cases. A partial sum is a lower bound, so a
difference of two of them has no sign that can be trusted. A resource change
is `increased` or `decreased`, never better or worse.

`cases` has one row per pair with the count of its changes in each family,
so a caller or the app can filter to regressions, improvements, pattern
changes, and resource changes without recomputing anything.

## Paired summaries

Estimand: among paired cases from the same dataset version, how many changed
in each direction.

For each scorer, scan, and measure the summary counts cases by direction. A
scorer, scan, or measure with nothing comparable keeps its row, with zero
compared and its not-comparable count, so it is not mistaken for one never
evaluated; a scorer never compatible on any pair has an unknown version. It
adds uncertainty only when both conditions hold:

1. Every case has one pair, so pairs are independent cases.
2. At least six cases changed. A two-sided exact sign test on five changed
   cases cannot go below `p = 0.0625`, so fewer are reported as counts.

Then it reports the share of changed cases that moved in the named direction
(improved, resolved, increased), an exact Clopper-Pearson interval for that
share, and the exact two-sided sign test against an even split. For a binary
scorer this is the exact McNemar test. The sign test uses only direction, so
it is also valid for ordered and numeric scales. P-values are per row and
unadjusted.

Two descriptive numbers carry no interval and so need neither condition on
the number of changed cases. For a binary scorer without repeated epochs,
`paired_difference` is the difference in the proportion at the better level,
(improved - regressed) / n, reported whatever the number of changes. Numeric
outcomes and resources get the median delta: nothing recorded justifies a
distribution for it, so it has no interval either.

Under repeated epochs the pairs of one case are not independent and their
alignment is a convention, so summaries count epoch pairs and report no
interval or test. A case-level reduction for repeated epochs is future work.

When no case pairs (different or undeclared dataset versions, or disjoint
task mixes) the design is `unpaired`. Summaries then describe each side
separately: counts by outcome value (for a numeric scorer, the count and
median of finite numeric values, and a count of the other values as off the
scale; a logical `TRUE` is not read as `1`), trajectories with, without, and without
assessed findings per scan, and recorded values per measure. Every table is
labelled `unpaired` and none has an estimate, interval, or test.

`print()` lists what was not compared before any change: missing-key,
duplicated, and one-sided trajectories, incompatible, unknown, and missing
outcomes, pattern checks that were not comparable, and resource values
recorded on one side without a delta.

Rejected: a t-interval or bootstrap for mean deltas (assumes a distribution,
and small evaluation sets are the norm); an asymptotic McNemar test; a
single improvement score across the three families; resource totals summed
across cases (partial coverage would be mixed into the sum).

## Review app

`scans_app()` accepts a comparison. Its baseline and candidate investigations
become the two applications in the switcher, labelled `Baseline: ...` and
`Candidate: ...`, and the overview leads with the comparison: what was not
compared, then the summary, then matched cases with a filter for regressions,
improvements, pattern changes, resource changes, and cases with something not
comparable. Each case links to both trajectories and to the events its
findings cite, through the app's existing `#application/trajectory?event=`
addresses, so opening a link switches side and reveals the event.

The side labels are for display. Each side keeps the application its
investigation records: annotations are read and written under it, and a
snapshot saved from a side records it, so annotations made in the plain app
appear in a comparison and the reverse. The annotation store keys on
application and trajectory identifier only, so when both sides record the
same application and a case has the same trajectory identifier on both, an
annotation made on one side shows on the other too, as it does across any
two snapshots of one application.

"Pattern changes" selects cases with a pattern that appeared, resolved, no
longer applies, or newly applies; each is shown under its own name. A
resource row whose sides differ in unit shows each value with its own unit.

The view reads `compare_trajectories()` output only. The filter selects pairs
by the counts in `cases`; the Shiny code computes no change, count, or test.
The view is capped at 100 cases and 100 excluded trajectories and says so
when it truncates.

Rejected: a `comparison =` argument beside two separately supplied sources
(the comparison already holds both investigations, and a second copy could
disagree with it); a third value of the app's view switch (saved view state
allows only `application` and `trajectory`, and several observers treat any
other value as an open trajectory).

No comparison tool was added to `scans_tools()`. Its tools are built around
one bundle, one snapshot identity, and one permitted trajectory set; a
comparison has two of each, so it does not fit that context without changing
the evidence-reference contract. A model can still read either side through
the existing tools.

## What a result can establish

- A re-scored copy of the same outputs has a different scorer version, so
  its outcomes are `incompatible` with the original. Comparing them tests the
  scorer change, and scans reports them as not comparable rather than as an
  improvement.
- An application or prompt change needs new outputs for the same cases,
  scored by the same scorer version. Then a per-case change is a recorded
  difference between two runs of one case.
- A recorded difference is not a cause. One run per case cannot separate the
  change from run-to-run variation; repeated epochs can show that variation.
- Cases that pair are the cases both runs kept. Unmatched, duplicated, and
  missing-key cases are outside every paired number and are reported first.
- Different dataset versions, or an undeclared one, give no pairs. The
  comparison is then descriptive and unpaired.
