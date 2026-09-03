# Scan trajectories for diagnostic findings

`scan_trajectories()` analyzes a completed
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
without calling a model or tool. It detects ambiguous, unresolved, and
unmatched tool correlation; repeated calls and suspicious consecutive
loops; failed events; and causal error chains. Named tool-argument
mappings are compared without regard to key order.

## Usage

``` r
scan_trajectories(
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

A tibble with one row per finding and these columns:

- `finding_id`: deterministic identifier within `scan_id`; `scan_id`:
  the caller-supplied diagnostic-run identifier.

- `scan`: one of `ambiguous_tool_correlation`, `unresolved_tool_call`,
  `unmatched_tool_result`, `repeated_tool_call`, `suspicious_tool_loop`,
  `event_error`, `error_chain`, `turn_error`, or `trajectory_error`;
  `scan_version`: detector version.

- `trajectory_id`, `turn_id`, and `event_id`: primary evidence
  identities; `event_ids`: character vector of every supporting event
  identifier. A `trajectory_error` finding has no turn or event, and a
  `turn_error` finding no event, because the failure is recorded on the
  record itself.

- `severity`: `warning` for tool-pattern findings or `error` for
  failures; `label` and `explanation`: concise human-readable
  descriptions.

- `value`: scan-specific structured details; `metadata`: a named
  extension list, currently empty for built-in scans.

No findings returns a typed zero-row tibble with the same columns.

## Details

Finding identifiers are deterministic for an unchanged bundle, argument
set, and `scan_id`. The default `scan_id` is snapshot-local and is not
intended as a globally unique persistent identifier.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "trajectory-1", source_type = "manual"),
  data.frame(),
  data.frame()
)

scan_trajectories(bundle)
#> # A tibble: 0 × 13
#> # ℹ 13 variables: finding_id <chr>, scan_id <chr>, scan <chr>,
#> #   scan_version <chr>, trajectory_id <chr>, turn_id <chr>, event_id <chr>,
#> #   event_ids <list>, severity <chr>, label <chr>, value <list>,
#> #   explanation <chr>, metadata <list>
```
