# Build explicitly mapped evaluation or optimization data

`review_dataset()` emits application inputs and targets for a consumer.
It never infers a runnable input from captured messages, calls a model,
or turns an old reviewer rationale into a new prediction score. Use a
scorer that evaluates each fresh output against its target and expected
behavior.

## Usage

``` r
review_dataset(x, format, split)
```

## Arguments

- x:

  An application example set from
  [`review_examples()`](https://jameshwade.github.io/scans/reference/review_examples.md).

- format:

  Either `"dsprrr"` or `"vitals"`.

- split:

  Exactly one of `"development"` or `"test"`. Required so a training
  call cannot silently include held-out cases.

## Value

A tibble of explicitly mapped cases with provenance attached.

## Details

The `"dsprrr"` format contains the named input and target columns
required by a module signature. The `"vitals"` format contains `id`,
`input`, `target`, and `metadata` list columns; use a solver/scorer that
accepts the explicit mappings. Provenance, expected behavior, groups,
and version are available in the `scans_provenance` attribute of either
format. Do not include that attribute or old reviewer rationale as
optimization targets by accident.
