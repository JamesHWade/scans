# A canonical agent trajectory bundle

`TrajectoryBundle()` creates a validated, source-neutral snapshot of one
or more completed agent trajectories. Its properties are ordinary
tibbles for trajectories, turns, events, evaluations, and adapter
losses.

## Usage

``` r
TrajectoryBundle(
  trajectories,
  turns,
  events,
  evaluations = NULL,
  losses = NULL
)
```

## Arguments

- trajectories:

  A data frame with one row per logical agent trajectory. Non-empty
  inputs require `trajectory_id` and `source_type`.

- turns:

  A data frame with one row per semantic turn. Non-empty inputs require
  `trajectory_id`, `turn_id`, `turn_index`, and `role`.

- events:

  A data frame with one row per content or execution event. Non-empty
  inputs require `trajectory_id`, `event_id`, `event_index`, and
  `event_type`.

- evaluations:

  An optional data frame with outcome judgments. Non-empty inputs
  require `trajectory_id` and `evaluation_id`.

- losses:

  An optional data frame recording source data that was unsupported,
  redacted, truncated, or externalized. Non-empty inputs require
  `field`, `reason`, and `detail`.

## Value

A `TrajectoryBundle` S7 object.

## Details

Missing optional canonical columns are filled with typed missing values.
Empty `evaluations` and `losses` tables are created when those arguments
are `NULL`. Unknown extra columns are preserved.

## Additional properties

- `@schema_version`:

  The integer trajectory schema version. Version 1 is the only currently
  supported value.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(
    trajectory_id = "trajectory-1",
    source_type = "manual"
  ),
  data.frame(),
  data.frame()
)

trajectory_info(bundle)
#> # A tibble: 1 × 16
#>   trajectory_id run_id parent_trajectory_id source_type source_id source_uri
#>   <chr>         <chr>  <chr>                <chr>       <chr>     <chr>     
#> 1 trajectory-1  NA     NA                   manual      NA        NA        
#> # ℹ 10 more variables: task_id <chr>, sample_id <chr>, epoch <int>,
#> #   agent <chr>, model <chr>, started_at <dttm>, completed_at <dttm>,
#> #   status <chr>, error <chr>, metadata <list>
```
