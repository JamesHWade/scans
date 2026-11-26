# Compare a change on matched cases

You changed a prompt, a tool, or a model, and ran your evaluation again.
This article compares the two runs case by case: which outcomes changed,
which diagnostic patterns appeared or went away, and what the change
cost.

vitals runs and scores the evaluation. scans reads the two completed
records and reports the recorded differences. It does not run a solver,
a scorer, or a model.

## Two runs of the same reviewed cases

The installed recipe builds everything offline. It runs an earlier
version of an order-support application on four order-tracking
questions. In each trajectory the order lookup times out and the reply
asks the customer to try again. A reviewer confirms each one, citing the
failed lookup and the reply, and expects the application to say that the
order service is unavailable.
[`review_examples()`](https://jameshwade.github.io/scans/reference/review_examples.md)
turns the four reviews into versioned application examples (one
development case, three held-out cases), and the recipe runs a baseline
and a candidate version of the application through vitals with one
scorer. Every model reply is a fixed transcript, so the numbers below
show the workflow, not a real improvement.

``` r

source(system.file("examples", "matched-comparison.R", package = "scans"))
runs <- build_matched_comparison()
runs$baseline$manifest[c("application", "snapshot_id")]
#> $application
#> [1] "Order support (baseline)"
#> 
#> $snapshot_id
#> [1] "sha256:6ec0a858a65b2adeedbc41173c2ec2acc7a931965984974f6a545eb5ef3a14bf"
runs$candidate$manifest[c("application", "snapshot_id")]
#> $application
#> [1] "Order support (candidate)"
#> 
#> $snapshot_id
#> [1] "sha256:dd08cdd626c545f946d634c21b211f70efa445a057fd8d95b0165f00286146d2"
```

Each side is a saved investigation. That gives the comparison a snapshot
identity to cite and the frozen scanner assessments and resource
measures it compares.

[`review_dataset()`](https://jameshwade.github.io/scans/reference/review_dataset.md)
used each review case identifier as the vitals sample `id`, so the
recorded `sample_id` leads back to the review:

``` r

provenance <- attr(runs$held_out, "scans_provenance")
provenance[c("group", "split", "expected_behavior")]
#> # A tibble: 3 × 3
#>   group   split expected_behavior                           
#>   <chr>   <chr> <chr>                                       
#> 1 order-B test  State that the order service is unavailable.
#> 2 order-C test  State that the order service is unavailable.
#> 3 order-D test  State that the order service is unavailable.
```

## Restrict both runs to held-out cases

Both runs also covered the development case. A change that was tuned on
the development case must be judged on cases it never saw, so keep only
the held-out samples on each side. Selecting trajectories creates a new
snapshot that records the omission.

``` r

held_out_only <- function(investigation) {
  info <- trajectory_info(investigation$bundle)
  investigation_snapshot(
    investigation$bundle,
    trajectory_ids = info$trajectory_id[info$sample_id %in% runs$held_out$id],
    application = investigation$manifest$application
  )
}
baseline <- held_out_only(runs$baseline)
candidate <- held_out_only(runs$candidate)
```

The candidate run also contained a smoke-test sample that is not in the
dataset. It is not a held-out case, so this selection leaves it out.
Pairing the unrestricted runs reports it as present only in the
candidate:

``` r

unrestricted <- pair_trajectories(
  runs$baseline,
  runs$candidate,
  by = "sample_id",
  dataset_version = runs$examples$version_id
)
unrestricted$only_candidate$sample_id
#> [1] "smoke-test"
```

## Pair on an explicit key

Name the key. Nothing is matched by position, and there is no default.

vitals does not record a dataset version or a scorer version, so you
declare both. The version of the example set is the dataset version. The
scorer declaration states that the label `expected_answer` means the
same scorer, at version 1, on both sides. vitals recorded that label
because the recipe passed a function named `expected_answer` as the
scorer.

``` r

scorer <- comparison_scorer(
  "expected_answer",
  version = "1",
  scale = "binary",
  direction = "higher",
  levels = c("I", "C")
)
pairing <- pair_trajectories(
  baseline,
  candidate,
  by = "sample_id",
  dataset_version = runs$examples$version_id,
  scorers = list(scorer)
)
pairing$pairs[c("pair_id", "baseline_trajectory_id", "candidate_trajectory_id")]
#> # A tibble: 2 × 3
#>   pair_id     baseline_trajectory_id                      candidate_trajectory…¹
#>   <chr>       <chr>                                       <chr>                 
#> 1 pair-000001 vitals/sha256%3A4bdaed157c85446df075adcc5e… vitals/sha256%3A4bdae…
#> 2 pair-000002 vitals/sha256%3Aacac4c6fed6f5fcb1f497fd8c0… vitals/sha256%3Aacac4…
#> # ℹ abbreviated name: ¹​candidate_trajectory_id
pairing$only_baseline["trajectory_id"]
#> # A tibble: 1 × 1
#>   trajectory_id                                                                 
#>   <chr>                                                                         
#> 1 vitals/sha256%3A4f1e685c2c86129425bb114354ae27ca3329c29fee90859b2ed76a7f103a7…
nrow(pairing$only_candidate)
#> [1] 0
```

The candidate run stopped before the last held-out case, so that case
exists only in the baseline. It is reported, and it is outside every
paired number below. `pairing$duplicates` and `pairing$missing_key` list
the other ways a trajectory can fail to pair; both are empty here.

If the samples had run for several epochs, add `"epoch"` to `by`.
Without it, repeated epochs of a sample are reported as duplicates.

## Per-case changes

``` r

comparison <- compare_trajectories(pairing)
group <- function(table) {
  provenance$group[match(table$sample_id, provenance$case_id)]
}
```

Outcomes, patterns, and resources stay in three tables. They are not
combined into a score.

``` r

outcomes <- comparison$outcomes
data.frame(
  group = group(outcomes),
  outcomes[c("baseline_value", "candidate_value", "change")]
)
#>     group baseline_value candidate_value    change
#> 1 order-B              I               C  improved
#> 2 order-C              C               I regressed
```

One case improved and one regressed. The patterns table says what the
scanners saw on each side. A side that could not be assessed is
`not_comparable`; it is never read as clean. A pattern found on one side
whose scan does not apply to the other side’s capture is
`no_longer_applicable` or `newly_applicable`, not `resolved` or
`appeared`: the pattern could not occur in that capture, which says
nothing about a fix.

``` r

patterns <- comparison$patterns
changed <- patterns[patterns$change != "absent", ]
data.frame(
  group = group(changed),
  changed[c("scan", "change", "baseline_status", "candidate_status")]
)
#>     group        scan   change      baseline_status       candidate_status
#> 1 order-C event_error appeared assessed_no_findings assessed_with_findings
#> 2 order-C  turn_error appeared assessed_no_findings assessed_with_findings
```

The regression came with a failed tool call that the baseline did not
have.

``` r

resources <- comparison$resources
tokens <- resources[resources$measure == "input_tokens", ]
data.frame(
  group = group(tokens),
  tokens[c(
    "baseline_value", "candidate_value", "delta",
    "baseline_coverage", "candidate_coverage",
    "baseline_includes_cached_input", "candidate_includes_cached_input"
  )]
)
#>     group baseline_value candidate_value delta baseline_coverage
#> 1 order-B             20             120   100          complete
#> 2 order-C             20             120   100          complete
#>   candidate_coverage baseline_includes_cached_input
#> 1           complete                           TRUE
#> 2           complete                           TRUE
#>   candidate_includes_cached_input
#> 1                            TRUE
#> 2                            TRUE
```

Both changed cases used more input tokens, including the one that
improved. A delta appears only when both sides have a complete recorded
value under the same unit, source, and cache-inclusion rule. Otherwise
`delta` is `NA` and `reason` says why:

``` r

unique(resources$reason[!resources$comparable])
#> [1] "No recorded value on either side."
```

## Open both trajectories

Every row carries both trajectory identities, and pattern rows carry the
finding and event identities on each side. Use them to read the
evidence:

``` r

appeared <- changed[changed$scan == "event_error", ]
events <- trajectory_events(comparison$pairing$candidate$bundle)
events[
  events$event_id %in% appeared$candidate_event_ids[[1L]],
  c("event_type", "name", "status", "text")
]
#> # A tibble: 1 × 4
#>   event_type  name         status text 
#>   <chr>       <chr>        <chr>  <chr>
#> 1 tool_result order_status failed NA
```

Or open the comparison in the review app. The overview lists what could
not be compared first, then the matched cases, with links from each case
to both trajectories and to the cited events.

``` r

scans_app(comparison)
```

## Link each case to its review

The sample identity is the review case identity, so each compared case
joins back to the decision, the expected behavior, and the snapshot the
reviewer looked at.

``` r

reviewed <- merge(
  comparison$outcomes[c("sample_id", "change")],
  provenance,
  by.x = "sample_id",
  by.y = "case_id"
)
reviewed[c("group", "change", "expected_behavior")]
#>     group    change                            expected_behavior
#> 1 order-B  improved State that the order service is unavailable.
#> 2 order-C regressed State that the order service is unavailable.
reviewed$origin[[1L]]$snapshot_id
#> [1] "sha256:eb85b2f74c12c0b925d53d229116d93adad12c272aa13a6fe09265bbaaf6a1f9"
```

`runs$examples$cases` holds the reviewed evidence itself: the original
trajectory, its findings, the events the reviewer cited, and the full
decision history.

## The paired summary

Printing leads with what was not compared.

``` r

comparison
#> <scans_comparison> Order support (baseline) -> Order support (candidate)
#> Key: sample_id; dataset version
#> sha256:b99966426b995ed721a1aca646fd902a594226ae6506b9b603aaa015cb398db8
#> Baseline snapshot:
#> sha256:29e10475d6ea144580a6269c788ad8de75a96147fdea117c3d16083cf0d7bf0c
#> Candidate snapshot:
#> sha256:ce3addcc0d1f050803e73e9b61b0a859081b458d117b9b35298f43ed82ed3dae
#> 
#> ── Not compared
#> ! 1 trajectory only in the baseline
#> 
#> ── Paired changes in 2 matched cases
#> • Outcome expected_answer (version 1): 1 improved, 1 regressed, 0 unchanged;
#>   counts only
#> • Pattern event_error: appeared in 1, resolved in 0, persisted in 0; counts
#>   only
#> • Pattern turn_error: appeared in 1, resolved in 0, persisted in 0; counts only
#> • Resource input_tokens: increased in 2, decreased in 0, median change 100
#>   tokens; counts only
#> • Resource output_tokens: increased in 2, decreased in 0, median change 24
#>   tokens; counts only
#> • Resource turn_duration: increased in 2, decreased in 0, median change 0.4 s;
#>   counts only
```

The summary counts cases by direction. It adds an exact sign test and an
interval only when each case has one pair and at least six cases
changed. Two changed cases cannot separate a change from chance, so this
summary is counts only and its `note` says so:

``` r

comparison$summaries$outcomes[c("n", "n_improved", "n_regressed", "method", "note")]
#> # A tibble: 1 × 5
#>       n n_improved n_regressed method note                                      
#>   <int>      <int>       <int> <chr>  <chr>                                     
#> 1     2          1           1 counts Fewer than 6 changed cases: an exact sign…
```

If no case had paired, for example because the two runs used different
dataset versions, the summaries would describe each side separately,
labelled `unpaired`, with no paired estimate.

## What this establishes

- For each held-out case that both runs completed, the recorded outcome,
  findings, and resource values of the two runs, and how they differ.
- That the outcomes were compared under one scorer at the version you
  declared, and that you declared the same dataset version for both
  runs. scans records the versions you declare; it does not check the
  cases against them.
- Which cases are outside the comparison, and why.

## What it does not establish

**That the change caused the difference.** Each case ran once per side.
A model that gives different answers on repeated runs can produce the
same table with no change at all. Run several epochs to see that
variation.

**Anything about a re-scored run.** Scoring the same saved outputs again
with a new scorer tests the scorer. scans reports those outcomes as
incompatible, because the scorer version differs:

``` r

rescored <- pair_trajectories(
  baseline,
  candidate,
  by = "sample_id",
  dataset_version = runs$examples$version_id,
  scorers = list(
    baseline = list(scorer),
    candidate = list(comparison_scorer(
      "expected_answer", version = "2", scale = "binary", levels = c("I", "C")
    ))
  )
)
unique(rescored$outcome_pairs[c("compatibility", "reason")])
#> # A tibble: 1 × 2
#>   compatibility reason                 
#>   <chr>         <chr>                  
#> 1 incompatible  Scorer version differs.
```

To test an application or prompt change, generate new outputs for the
same cases and score both runs with the same scorer version.

**Performance on cases you tuned against.** Use the development split to
change the application and the held-out split to judge it. Once you
adjust the application in response to held-out results, those cases are
no longer held out; review new cases and make a new version of the
example set.

**Complete resource use.** Coverage describes the loaded records. A
complete measure does not prove the source captured every call.

**A property of production traffic.** Three reviewed cases describe
three reviewed cases.
