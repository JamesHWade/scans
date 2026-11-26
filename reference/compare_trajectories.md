# Compare a change on matched cases

`compare_trajectories()` reads a pairing from
[`pair_trajectories()`](https://jameshwade.github.io/scans/reference/pair_trajectories.md)
and reports what changed between the baseline and the candidate for each
matched case. It reads the saved evaluations, scanner assessments, and
resource measures of the two investigations. It never runs a solver, a
scorer, a scanner, or a model.

## Usage

``` r
compare_trajectories(pairing, conf_level = 0.95)
```

## Arguments

- pairing:

  A `scans_pairing` value from
  [`pair_trajectories()`](https://jameshwade.github.io/scans/reference/pair_trajectories.md).
  The investigations it holds must be valid and carry the snapshot and
  revision identifiers the pairing recorded.

- conf_level:

  Confidence level for intervals, between 0 and 1.

## Value

A `scans_comparison` value: a list with the `pairing`, the `design`
(`type` and `reason`), per-case tibbles `outcomes`, `patterns`, and
`resources`, a `cases` tibble with one row per pair counting its changes
in each family, and `summaries` (a list of `outcomes`, `patterns`, and
`resources` tibbles). Printing leads with what could not be compared.

## Details

Changes come in three families that are never combined into one score:

- `outcomes`: one row per case and scorer. A change is reported only for
  a compatible scorer pair whose values are on the declared scale.

- `patterns`: one row per case and scan: `"appeared"`, `"resolved"`,
  `"persisted"`, `"absent"`, or `"not_comparable"` when either side was
  not assessed, did not run the scan, or ran it under a different
  version or settings. A pattern found on one side whose scan does not
  apply to the other side's capture is `"no_longer_applicable"` (found
  on the baseline) or `"newly_applicable"` (found on the candidate): the
  pattern could not occur in the other capture, which is not a fix or a
  regression. These are counted on their own and never feed the share
  resolved or its test. A scan that does not apply, with no finding on
  the other side, is absent.

- `resources`: one row per case and measure. A delta is reported only
  when both sides have a recorded value with complete coverage under the
  same unit, aggregation, source, measurement contract, and cache
  inclusion. Otherwise the delta is `NA` and `reason` says why. A higher
  resource value is not labelled better or worse.

Every row carries both trajectory identities. Pattern rows also carry
the finding and event identities on each side.

## Summaries

The estimand is a change among paired cases from the same dataset
version. `summaries` counts cases by direction for each scorer, scan,
and measure. A scorer with no comparable outcome keeps a row with `n`
zero and its `n_not_comparable` count. When each case has one pair and
at least six cases changed, it adds the share of changed cases that
moved in the named direction, an exact (Clopper-Pearson) interval for
that share, and an exact two-sided sign test of an even split. For a
binary scorer this is the exact McNemar test. With fewer changed cases,
or with repeated epochs, summaries are counts only and `note` says why.
P-values are per row and unadjusted. Two descriptive numbers have no
interval and are reported whatever the number of changed cases:
`paired_difference`, the difference in the proportion at the better
level for a binary scorer without repeated epochs, and `median_delta`
for numeric outcomes and resources.

When no case pairs, for example under different dataset versions, the
design is `"unpaired"`: `summaries` describes each side separately,
every table is labelled `"unpaired"`, and no paired estimate, interval,
or test is produced.

A recorded difference between two runs does not establish that the
change caused it. See
[`vignette("matched-comparison")`](https://jameshwade.github.io/scans/articles/matched-comparison.md)
for what a result supports.

## Examples

``` r
bundle <- function(score) {
  TrajectoryBundle(
    data.frame(
      trajectory_id = "run-1",
      sample_id = "case-1",
      source_type = "manual",
      status = "completed"
    ),
    data.frame(),
    data.frame(),
    evaluations = tibble::tibble(
      trajectory_id = "run-1",
      evaluation_id = "run-1/evaluation",
      scorer = "judge",
      value = list(score)
    )
  )
}
if (rlang::is_installed(c("digest", "jsonlite"))) {
  pairing <- pair_trajectories(
    investigation_snapshot(bundle(0), application = "Baseline"),
    investigation_snapshot(bundle(1), application = "Candidate"),
    by = "sample_id",
    dataset_version = "2026-11",
    scorers = list(comparison_scorer("judge", "1", "numeric"))
  )
  comparison <- compare_trajectories(pairing)
  comparison$outcomes[c("sample_id", "baseline_value", "candidate_value", "change")]
  comparison
}
#> <scans_comparison> Baseline -> Candidate
#> Key: sample_id; dataset version 2026-11
#> Baseline snapshot:
#> sha256:51ef3e8b6884d9ab1a9d12fca5034026c051220b35800543d230c005a26fc558
#> Candidate snapshot:
#> sha256:af07fec2b9250b0b2e7ea2286eedc0e36dfc1c58103dc566c868f25770120237
#> 
#> ── Not compared 
#> ! 8 pattern checks not comparable (not assessed, not run, or changed scanner)
#> 
#> ── Paired changes in 1 matched case 
#> • Outcome judge (version 1): 1 improved, 0 regressed, 0 unchanged; counts only
```
