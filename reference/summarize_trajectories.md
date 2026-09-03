# Summarize trajectories

`summarize_trajectories()` reduces each trajectory in a
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
to one row of identity, structure, diagnostic, and usage measures.
Counts are zero when no matching records exist. Usage totals are `NA`
when every source value contributing to that total is unknown.

## Usage

``` r
summarize_trajectories(x)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Value

A tibble with one row per trajectory and these columns:

- Identity: `trajectory_id`, `run_id`, `parent_trajectory_id`,
  `source_type`, `status`, `model`, `agent`, `task_id`, `sample_id`,
  `epoch`, `started_at`, and `completed_at` retain the corresponding
  trajectory fields, so summaries can be grouped by model, agent, or
  task without a join back to
  [`trajectory_info()`](https://jameshwade.github.io/scans/reference/trajectory_accessors.md).

- Structure: `trajectory_depth` counts parent trajectories from a root
  at zero; `max_event_depth` counts the longest event-parent path;
  `n_turns`, `n_rounds`, and `n_events` count semantic records.

- Tool correlation: `n_tool_calls`, `n_tool_results`,
  `n_unresolved_tool_calls`, `n_unmatched_tool_results`, and
  `n_ambiguous_tool_correlations` count tool activity and correlation
  gaps.

- Failures and losses: `n_error_events`, `n_failed_turns`, and
  `n_losses`.

- Usage: `input_tokens`, `output_tokens`, and `cached_input_tokens` sum
  known token counts; `cost` sums known source costs without currency
  conversion; `turn_duration` sums known turn durations in seconds;
  `elapsed` is wall time in seconds from `started_at` through
  `completed_at`.

Depths, counts, and `epoch` are integers. Usage, cost, and duration
columns are doubles; `started_at` and `completed_at` are `POSIXct`.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "trajectory-1", source_type = "manual"),
  data.frame(),
  data.frame()
)

summarize_trajectories(bundle)
#> # A tibble: 1 × 31
#>   trajectory_id run_id parent_trajectory_id source_type status model agent
#>   <chr>         <chr>  <chr>                <chr>       <chr>  <chr> <chr>
#> 1 trajectory-1  NA     NA                   manual      NA     NA    NA   
#> # ℹ 24 more variables: task_id <chr>, sample_id <chr>, epoch <int>,
#> #   started_at <dttm>, completed_at <dttm>, trajectory_depth <int>,
#> #   max_event_depth <int>, n_turns <int>, n_rounds <int>, n_events <int>,
#> #   n_tool_calls <int>, n_tool_results <int>, n_unresolved_tool_calls <int>,
#> #   n_unmatched_tool_results <int>, n_ambiguous_tool_correlations <int>,
#> #   n_error_events <int>, n_failed_turns <int>, n_losses <int>,
#> #   input_tokens <dbl>, output_tokens <dbl>, cached_input_tokens <dbl>, …
```
