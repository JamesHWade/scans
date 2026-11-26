# Assess trajectory scans and their evidence coverage

`assess_trajectory_scans()` runs the built-in diagnostics, and any
custom `scanners`, and explains whether each selected scanner could
assess each trajectory. It uses the same detectors as
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
and never calls a model or tool.

## Usage

``` r
assess_trajectory_scans(
  x,
  scan_id = "scan-000001",
  repeat_threshold = 2L,
  loop_threshold = 3L,
  scans = NULL,
  scanners = NULL
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
  is the same whether or not its neighbours ran. Names of custom
  `scanners` can be selected too.

- scanners:

  Optional custom scanners for this call: one
  [`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
  or a list of them. `NULL` (the default) runs built-in scans only.
  Custom findings follow the built-in findings, which are identical
  whether or not custom scanners are supplied. A custom scanner that
  reports a trajectory as not assessed contributes no findings; one that
  fails raises an error here. Use `assess_trajectory_scans()` to record
  both.

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

The Deputy scans apply to trajectories that record Deputy run evidence:
a run ending in `metadata$ending`, a delegation in
`metadata$delegation`, any `deputy:*` event, or a `"deputy"` source
type. Other trajectories are `not_applicable`. `cut_off_answer` needs
the ending's `cut_off` or `finish_reason`; `run_intervention` needs
recorded `deputy:*` events, with a recorded status on each permission
and approval event. A capture with events but none of an intervention's
types is assessed with no findings; a capture without any lifecycle
events has insufficient evidence unless the run ending itself shows a
limit. A delegation child has no lifecycle events by design: it is
assessed from its reported claims, and has insufficient evidence when
the subagent reported neither. `settings` records the fixed
repeated-compaction threshold.

Built-ins execute in tool, event, record, and Deputy groups. A group
failure affects its selected scanners on that trajectory; other groups
continue. Failure reasons omit arbitrary condition messages. Finding
identifiers are local to the run and can change if a previously failed
group later succeeds.

A custom
[`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
has one assessment per trajectory, after the built-in rows. It declares
`insufficient_evidence` or `not_applicable` itself. A scanner that
throws, returns something other than a result, or cites a turn or event
that is not in the trajectory is an `execution_failure` for that scanner
and trajectory, with a fixed reason and none of its findings. Built-in
findings and assessments are identical whether or not custom scanners
are supplied.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "span-1", source_type = "manual",
             status = "completed"),
  data.frame(), data.frame()
)
result <- assess_trajectory_scans(bundle)
result$assessments[c("scan", "status", "reason")]
#> # A tibble: 11 × 3
#>    scan                       status                reason                      
#>    <chr>                      <chr>                 <chr>                       
#>  1 ambiguous_tool_correlation insufficient_evidence No tool exchange or fully i…
#>  2 unresolved_tool_call       insufficient_evidence No tool exchange or fully i…
#>  3 unmatched_tool_result      insufficient_evidence No tool exchange or fully i…
#>  4 repeated_tool_call         insufficient_evidence No tool exchange or fully i…
#>  5 suspicious_tool_loop       insufficient_evidence No tool exchange or fully i…
#>  6 event_error                insufficient_evidence Events with diagnostic stat…
#>  7 error_chain                insufficient_evidence Events with diagnostic stat…
#>  8 turn_error                 insufficient_evidence Semantic turns are not reco…
#>  9 trajectory_error           assessed_no_findings  Required evidence is record…
#> 10 cut_off_answer             not_applicable        No Deputy run ending or dep…
#> 11 run_intervention           not_applicable        No Deputy run ending or dep…
```
