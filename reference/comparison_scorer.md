# Declare what a recorded scorer measures

A vitals result records a scorer label and, for an ordered factor score,
its levels. It does not record a scorer version, and it records no
direction for numeric, logical, or character scores.
`comparison_scorer()` declares those facts so
[`pair_trajectories()`](https://jameshwade.github.io/scans/reference/pair_trajectories.md)
can decide whether two recorded outcomes came from the same scorer. A
declaration is the caller's assertion; scans cannot verify it. A
declaration that contradicts the record is an error.

## Usage

``` r
comparison_scorer(
  scorer,
  version,
  scale = c("binary", "ordered", "numeric"),
  direction = c("higher", "lower"),
  levels = NULL,
  id = scorer
)
```

## Arguments

- scorer:

  The scorer label recorded in `trajectory_evaluations()$scorer`.

- version:

  The scorer version: any string that changes whenever the scorer's
  code, prompt, grading model, or rubric changes.

- scale:

  `"binary"`, `"ordered"`, or `"numeric"`.

- direction:

  Whether a `"higher"` or `"lower"` value is better. An undeclared
  ordered factor score is read as `"higher"`, the vitals convention that
  levels run from worst to best; declare `"lower"` when a scorer's
  levels run the other way. The declared `levels` must still match the
  recorded level order.

- levels:

  For binary and ordered scales, the possible values from lowest to
  highest. Not used for a numeric scale.

- id:

  The scorer's identity. Defaults to the recorded label. Supply one
  shared `id` when the same scorer was recorded under different labels.

## Value

A `scans_comparison_scorer` value.

## Examples

``` r
comparison_scorer(
  "detect_exact()",
  version = "vitals-0.4.1",
  scale = "binary",
  direction = "higher",
  levels = c("I", "C")
)
#> $scorer
#> [1] "detect_exact()"
#> 
#> $id
#> [1] "detect_exact()"
#> 
#> $version
#> [1] "vitals-0.4.1"
#> 
#> $scale
#> [1] "binary"
#> 
#> $direction
#> [1] "higher"
#> 
#> $levels
#> [1] "I" "C"
#> 
#> attr(,"class")
#> [1] "scans_comparison_scorer"
```
