# Capture an investigation for saving and reopening

`investigation_snapshot()` freezes selected trajectory evidence together
with its summaries, findings, scanner assessments, resource
measurements, and analysis settings. It calls only deterministic package
diagnostics. Use
[`write_investigation()`](https://jameshwade.github.io/scans/reference/investigation_files.md)
to save the result and
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
to view it.

## Usage

``` r
investigation_snapshot(
  x,
  trajectory_ids = NULL,
  application = "Trajectories",
  source = list(),
  scans = NULL,
  repeat_threshold = 2L,
  loop_threshold = 3L,
  view = list(),
  previous = NULL
)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

- trajectory_ids:

  Trajectory identifiers to include. `NULL` includes all trajectories.
  [`character()`](https://rdrr.io/r/base/character.html) creates an
  empty selection. Selection preserves the bundle's row order.

- application:

  One non-empty application name.

- source:

  A named list of source identity and capture details, such as the
  `read_info` attribute from
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md).
  It must contain data only; never supply credentials or a loader
  function.

- scans:

  Which detectors to run, as a character vector of names from
  [`scan_registry()`](https://jameshwade.github.io/scans/reference/scan_registry.md).
  `NULL` (the default) runs all of them. Selecting a subset narrows the
  findings without changing how any of them are computed, so a finding
  is the same whether or not its neighbours ran.

- repeat_threshold:

  The minimum number of calls with the same tool name and arguments that
  produces a `repeated_tool_call` finding.

- loop_threshold:

  The minimum consecutive calls with the same tool name and arguments
  that produces a `suspicious_tool_loop` finding. Tool results between
  calls do not break a call sequence.

- view:

  A named list of browser settings. Supported names are `query`,
  `source_type` (`NULL` means all), `status` (`NULL` means all, `NA`
  means unknown), `findings_only`, `annotated_only`, `annotation_ids`,
  `pattern`, `sort` (`"newest"`, `"oldest"`, `"findings"`, or
  `"longest"`), `selected_trajectory_id`, `tab` (`"application"` or
  `"trajectory"`), and `priority` (`"elapsed"`, `"tokens"`, or
  `"findings"`). Annotation IDs record filter membership only, not
  judgments or reviewer text.

- previous:

  An optional earlier investigation. Its revision identifier is recorded
  as the parent; its settings are not inherited. When `x` is the earlier
  investigation's bundle, its prior omission count is retained and any
  newly excluded trajectories are added to that count.

## Value

A `scans_investigation` value. Its identifiers are verified when writing
or opening it; use this constructor to create a changed revision rather
than editing its fields.

## Details

The result is an ordinary list with class `scans_investigation`. Its
`bundle` is a
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md);
`analysis` holds four tibbles named `summaries`, `findings`,
`assessments`, and `measures`. `settings` records scanner selection and
thresholds. `view` records browser state. `manifest` records the
application, capture details, package/scanner versions, content policy,
snapshot identifier, and revision identifier.

The snapshot identifier covers selected evidence, application, and
capture details. The revision identifier also covers analysis, settings,
view, and an optional parent revision. Repeating the same capture is
stable; creation time does not affect either identifier. Changing
evidence creates a new snapshot. Changing settings or view creates a new
revision of that snapshot.

## Content policy

The initial `retained-v1` policy includes all retained fields of
selected trajectories: text, tool arguments/results, evaluations, and
metadata. Existing adapter redactions and loss records remain intact.
Saving does not perform additional anonymization or guarantee that
sensitive text is absent.

Unselected trajectory records and annotation history are omitted.
Unassigned capture-wide losses remain included. If a selected
trajectory's parent is excluded, the link becomes an explicit loss
retaining its original identity. The JSON format supports base vectors,
lists, data frames, factors, dates, and times. Unsupported objects are
rejected with a field path.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "run-1", source_type = "manual",
             status = "completed"),
  data.frame(), data.frame()
)
saved <- investigation_snapshot(bundle, application = "Support assistant")
saved$analysis$assessments[c("scan", "status")]
#> # A tibble: 9 × 2
#>   scan                       status               
#>   <chr>                      <chr>                
#> 1 ambiguous_tool_correlation insufficient_evidence
#> 2 unresolved_tool_call       insufficient_evidence
#> 3 unmatched_tool_result      insufficient_evidence
#> 4 repeated_tool_call         insufficient_evidence
#> 5 suspicious_tool_loop       insufficient_evidence
#> 6 event_error                insufficient_evidence
#> 7 error_chain                insufficient_evidence
#> 8 turn_error                 insufficient_evidence
#> 9 trajectory_error           assessed_no_findings 
saved$manifest$content_policy
#> $id
#> [1] "retained-v1"
#> 
#> $included
#> [1] "All retained fields of selected trajectories, their turns, events, evaluations, and relevant losses."
#> 
#> $excluded
#> [1] "Unselected trajectory records, annotation history, loader credentials, and live connections."
#> 
#> $redaction
#> [1] "Existing adapter redactions are preserved. No additional anonymization is performed."
#> 
#> $omitted_trajectories
#> [1] 0
#> 
```
