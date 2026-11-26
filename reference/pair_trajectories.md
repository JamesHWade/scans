# Pair baseline and candidate trajectories on an explicit key

`pair_trajectories()` matches the trajectories of two saved
investigations case by case. It reads recorded identities only: it never
runs a solver, a scorer, or a model, and it never matches by position.
Every trajectory of both investigations appears in exactly one of the
returned tables.

## Usage

``` r
pair_trajectories(
  baseline,
  candidate,
  by,
  dataset_version = NULL,
  scorers = NULL
)
```

## Arguments

- baseline, candidate:

  Saved investigations from
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
  or
  [`read_investigation()`](https://jameshwade.github.io/scans/reference/investigation_files.md).
  A saved investigation carries the snapshot identity and the frozen
  analysis that a comparison cites.

- by:

  Key columns: `"sample_id"`, optionally with `"task_id"` and `"epoch"`.
  There is no default.

- dataset_version:

  The dataset version of both sides as one string, or a character vector
  named `baseline` and `candidate`. `NULL` leaves it undeclared.

- scorers:

  Scorer declarations from
  [`comparison_scorer()`](https://jameshwade.github.io/scans/reference/comparison_scorer.md):
  a list applied to both sides, or a list with `baseline` and
  `candidate` lists when the sides differ.

## Value

A `scans_pairing` value: a list of ordinary tibbles and the two
investigations.

- `key`: `by` and the declared dataset versions.

- `snapshots`: one row per side with snapshot and revision identifiers.

- `pairs`: one row per matched case with both trajectory identities.

- `only_baseline`, `only_candidate`: cases present on one side.

- `duplicates`: every trajectory whose key occurs more than once on
  either side, with the count on each side.

- `missing_key`: trajectories lacking a key value, and which one.

- `scorers`: recorded and declared scorer attributes per side.

- `outcome_pairs`: one row per pair and scorer with `compatibility`
  (`"compatible"`, `"incompatible"`, `"unknown"`, or `"missing"`) and
  the reason.

## Cases and keys

A case is one sample of one dataset version. `by` names the recorded
identity columns of
[`trajectory_info()`](https://jameshwade.github.io/scans/reference/trajectory_accessors.md)
that identify it and must include `"sample_id"`. Add `"task_id"` when an
investigation holds several tasks. Add `"epoch"` when a sample was run
more than once: epoch `k` of the baseline then pairs with epoch `k` of
the candidate. Without `"epoch"` in the key, repeated epochs of a sample
are duplicates and are reported, not reduced to one.

vitals does not record a dataset version, so the caller declares it.
Cases pair only when both sides declare the same version. A side without
a declared version has every trajectory reported as missing a key value.

## Scorers

Outcomes pair only when scorer identity, version, scale, and direction
agree on both sides. vitals records a scorer label (the expression
passed as `scorer`) and, for an ordered factor score, its levels in
order. It records no scorer version and no direction for numeric or
logical scores. Declare what the record lacks with
[`comparison_scorer()`](https://jameshwade.github.io/scans/reference/comparison_scorer.md).
An undeclared attribute stays unknown, and an unknown or disagreeing
scorer pair is reported in `outcome_pairs` with its reason. It is never
coerced.

## See also

[`compare_trajectories()`](https://jameshwade.github.io/scans/reference/compare_trajectories.md)
for per-case changes.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(
    trajectory_id = c("a", "b"),
    sample_id = c("case-1", "case-2"),
    source_type = "manual",
    status = "completed"
  ),
  data.frame(),
  data.frame()
)
if (rlang::is_installed(c("digest", "jsonlite"))) {
  baseline <- investigation_snapshot(bundle, application = "Baseline")
  candidate <- investigation_snapshot(
    bundle,
    trajectory_ids = "a",
    application = "Candidate"
  )
  pairing <- pair_trajectories(
    baseline,
    candidate,
    by = "sample_id",
    dataset_version = "2026-11"
  )
  pairing$pairs
  pairing$only_baseline
}
#> # A tibble: 1 × 5
#>   dataset_version task_id sample_id epoch trajectory_id
#>   <chr>           <chr>   <chr>     <int> <chr>        
#> 1 2026-11         NA      case-2       NA b            
```
