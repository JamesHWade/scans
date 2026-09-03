# Test for or extract data from a trajectory bundle

These functions provide the ordinary tibble analysis surface for a
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
Accessors return the stored canonical table without modifying it.

## Usage

``` r
is_trajectory_bundle(x)

trajectory_info(x)

trajectory_turns(x)

trajectory_events(x)

trajectory_evaluations(x)

trajectory_losses(x)
```

## Arguments

- x:

  An object to test or a
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
  to access.

## Value

- `is_trajectory_bundle()` returns one logical value.

- The accessors return a tibble.
