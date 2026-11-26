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
give. The convention is made harmless in two ways. The caller must ask for it
by naming `"epoch"`. And paired summaries take the case as the unit and use
only statistics that do not depend on how epochs were aligned (see
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
scores of different types), its scale stays unknown, and a declaration for that
label is an error: no single declaration matches every record. Version is never
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
