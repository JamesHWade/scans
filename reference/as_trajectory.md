# Convert an object to a trajectory bundle

`as_trajectory()` is an S7 generic used by source adapters. Methods
snapshot completed upstream objects into a canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
The core package provides an identity method for existing bundles;
integration packages provide methods for their own source classes.

## Usage

``` r
as_trajectory(x, ...)
```

## Arguments

- x:

  An object containing completed agent trajectory data.

- ...:

  Arguments passed to a source-specific method.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
