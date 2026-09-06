# Assess built-in trajectory scans and their evidence coverage

`assess_trajectory_scans()` runs the built-in diagnostics and explains
whether each selected scanner could assess each trajectory. It uses the
same detectors as
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
and never calls a model or tool.

## Usage

``` r
assess_trajectory_scans(
  x,
  scan_id = "scan-000001",
  repeat_threshold = 2L,
  loop_threshold = 3L,
  scans = NULL
)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

- scan_id:

  A non-empty identifier for this diagnostic run.

- repeat_threshold:

  The minimum number of calls with the same tool name and arguments that
  produces a `repeated_tool_call` finding.

- loop_threshold:

  The minimum consecutive calls with the same tool name and arguments
  that produces a `suspicious_tool_loop` finding. Tool results between
  calls do not break a call sequence.

- scans:

  Which detectors to run, as a character vector of names from
  [`scan_registry()`](https://jameshwade.github.io/scans/reference/scan_registry.md).
  `NULL` (the default) runs all of them. Selecting a subset narrows the
  findings without changing how any of them are computed, so a finding
  is the same whether or not its neighbours ran.

## Value

A list with two tibbles:

- `findings`: the findings from assessable scanners, with the same
  columns and identities as
  [`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md).
  Candidates from scanners with insufficient evidence are excluded. The
  findings-only API still returns those raw candidates.

- `assessments`: one row per selected scanner and trajectory, with
  `scan_id`, `trajectory_id`, `scan`, `scan_version`, `status`,
  `required_evidence`, `reason`, and list columns `settings`,
  `finding_ids`, `limitations`, and `loss_rows`. `loss_rows` indexes
  [`trajectory_losses()`](https://jameshwade.github.io/scans/reference/trajectory_accessors.md)
  in the input bundle, including unassigned losses that may affect any
  trajectory.

Status is one of `assessed_no_findings`, `assessed_with_findings`,
`insufficient_evidence`, `not_applicable`, or `execution_failure`. An
assessment describes only the retained records, never complete capture
or overall success. A positive error observation remains assessable when
other records lack status; limitations explain that negative conclusions
are unavailable. Missing semantic records do not establish
inapplicability.

Tool comparisons require recorded identities or arguments as
appropriate. Known losses in required fields prevent assessment. Empty
argument lists are valid; missing arguments are not. Recorded text
conversations without tool activity make tool scans inapplicable to that
capture. Status scans require explicit status or failure evidence.
Recognized status labels are `completed`, `failed`, `cancelled`,
`interrupted`, `running`, and `pending`; other labels do not establish
status coverage. Multiple failed events without recorded parent links
cannot establish the absence of an error chain.

Built-ins execute in tool, event, and record groups. A group failure
affects its selected scanners on that trajectory; other groups continue.
Failure reasons omit arbitrary condition messages. Finding identifiers
are local to the run and can change if a previously failed group later
succeeds.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "span-1", source_type = "manual",
             status = "completed"),
  data.frame(), data.frame()
)
result <- assess_trajectory_scans(bundle)
result$assessments[c("scan", "status", "reason")]
#> # A tibble: 9 × 3
#>   scan                       status                reason                       
#>   <chr>                      <chr>                 <chr>                        
#> 1 ambiguous_tool_correlation insufficient_evidence No tool exchange or fully in…
#> 2 unresolved_tool_call       insufficient_evidence No tool exchange or fully in…
#> 3 unmatched_tool_result      insufficient_evidence No tool exchange or fully in…
#> 4 repeated_tool_call         insufficient_evidence No tool exchange or fully in…
#> 5 suspicious_tool_loop       insufficient_evidence No tool exchange or fully in…
#> 6 event_error                insufficient_evidence Events with diagnostic statu…
#> 7 error_chain                insufficient_evidence Events with diagnostic statu…
#> 8 turn_error                 insufficient_evidence Semantic turns are not recor…
#> 9 trajectory_error           assessed_no_findings  Required evidence is recorde…
```
