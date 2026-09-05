# Measure recorded trajectory resources

`measure_trajectories()` reports resource values with their recording
denominators and source semantics. It prefers captured OpenTelemetry
calls over reconstructed turns.
[`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md)
retains its original turn-based sums, including raw source costs.

## Usage

``` r
measure_trajectories(x)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Value

A tibble with one row per trajectory and measure:

- `trajectory_id`, `parent_trajectory_id`: attribution; children are not
  pooled with parents or subtracted from inclusive parent measurements.

- `measure`, `value`, `unit`, `aggregation`: the quantity and its
  operation. Durations are seconds (`s`); usage is `tokens`.

- `n_recorded`, `n_total`: eligible records with a valid value and
  eligible records in the loaded snapshot, respectively. Unknown counts
  are `NA`.

- `coverage`: `complete`, `partial`, `unavailable`, or `unknown` (an
  older conversation total without a denominator). Complete recording
  does not establish complete capture of an application's work.

- `source`, `source_unit`, `adapter`, `contract_version`: interpretation
  provenance. Contract version 1 describes this measurement interface.

- `includes_cached_input`: `TRUE` for input totals known to include
  cache reads, otherwise `NA` (unknown or not an input-token measure).

- `currency`, `price_basis`: missing until supported cost semantics
  exist.

- `limitation`: the boundary on interpreting the value.

## Measures

`elapsed` is time between trajectory bounds. New OTel captures require
valid bounds for every captured span; incomplete bounds are unavailable.
`input_tokens`, `output_tokens`, and `cached_input_tokens` sum known
captured call values, otherwise canonical assistant turns and other
turns explicitly carrying usage. For ellmer turns, including those
projected by Deputy, dsprrr, Commons, and vitals, input totals combine
uncached and cached input per turn. Both components must be recorded for
a turn to contribute a known input total; missing cache usage is not
zero. Other canonical sources retain their input counts with unknown
cache inclusion. Do not add cached usage again when
`includes_cached_input` is `TRUE`. `turn_duration` sums known canonical
turn durations without asserting one turn equals one model call.

`model_duration` sums captured OTel chat durations. `tool_duration` sums
OTel execute-tool durations, otherwise canonical tool-call durations.
Both include nested activity. `recorded_work` unions captured chat and
tool intervals, counting overlap once. `unattributed_elapsed` is elapsed
minus this union only when all captured intervals are valid. It does not
prove idle time. Do not sum per-trajectory intervals to claim
application wall time.

`first_chunk_latency` is the median recorded OTel time to first chunk;
chunks may contain no token. `first_token_latency` and `cost` are
unavailable because supported sources do not establish these semantics.
Raw cost sums remain accessible through
[`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md).
Missing values never become zero; partial sums describe only recorded
components.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "example", source_type = "manual"),
  data.frame(), data.frame()
)
measure_trajectories(bundle)
#> # A tibble: 12 × 17
#>    trajectory_id parent_trajectory_id measure value unit  aggregation n_recorded
#>    <chr>         <chr>                <chr>   <dbl> <chr> <chr>            <int>
#>  1 example       NA                   elapsed    NA s     interval             0
#>  2 example       NA                   input_…    NA toke… sum                  0
#>  3 example       NA                   output…    NA toke… sum                  0
#>  4 example       NA                   cached…    NA toke… sum                  0
#>  5 example       NA                   turn_d…    NA s     sum                  0
#>  6 example       NA                   model_…    NA s     sum                 NA
#>  7 example       NA                   tool_d…    NA s     sum                  0
#>  8 example       NA                   record…    NA s     union               NA
#>  9 example       NA                   unattr…    NA s     difference          NA
#> 10 example       NA                   first_…    NA s     median              NA
#> 11 example       NA                   first_…    NA s     unavailable         NA
#> 12 example       NA                   cost       NA NA    unavailable         NA
#> # ℹ 10 more variables: n_total <int>, coverage <chr>, source <chr>,
#> #   source_unit <chr>, adapter <chr>, contract_version <int>,
#> #   includes_cached_input <lgl>, currency <chr>, price_basis <chr>,
#> #   limitation <chr>
```
