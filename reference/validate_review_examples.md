# Check reviewed scanner targets against retained evidence

Runs the installed deterministic scanner on each selected case using its
saved thresholds, then evaluates its explicit count predicate. Saved
review decisions and findings are never overwritten. Unresolved reviews
or an assessment with insufficient evidence, inapplicability, or
execution failure produce an unknown result (`NA`), not a negative
label.

## Usage

``` r
validate_review_examples(x, split)
```

## Arguments

- x:

  A scanner example set from
  [`review_examples()`](https://jameshwade.github.io/scans/reference/review_examples.md).

- split:

  One of `"development"` or `"test"`.

## Value

A tibble with case identity, judgment, scanner/assessment version,
observed count, predicate, and `passed`. A pass validates the scanner
predicate on retained evidence; it does not establish application
quality.
