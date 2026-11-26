# Check reviewed scanner targets against retained evidence

Runs the installed deterministic scanner on each selected case using its
saved thresholds, then evaluates its explicit count predicate. Saved
review decisions and findings are never overwritten. Unresolved reviews
or an assessment with insufficient evidence, inapplicability, or
execution failure produce an unknown result (`NA`), not a negative
label.

## Usage

``` r
validate_review_examples(x, split, scanners = NULL)
```

## Arguments

- x:

  A scanner example set from
  [`review_examples()`](https://jameshwade.github.io/scans/reference/review_examples.md).

- split:

  One of `"development"` or `"test"`.

- scanners:

  Optional custom scanners that the cases target: one
  [`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
  or a list of them. A case whose custom scanner is not supplied has
  assessment `"scanner_not_supplied"` and an unknown result. See
  [`validate_scanner()`](https://jameshwade.github.io/scans/reference/validate_scanner.md)
  for false positives, false negatives, precision, and recall.

## Value

A tibble with case identity, judgment, the `scan_version` that ran
beside the `reviewed_scan_version` recorded in the case's saved
analysis, assessment, observed count, predicate, and `passed`. A pass
validates the scanner predicate on retained evidence; it does not
establish application quality.
