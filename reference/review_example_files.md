# Save or reopen selected review examples

Files contain selected evidence and review history, including retained
text and explicit input/target mappings. Inspect the example set's
`content_policy` and cases before sharing. The versioned JSON format
accepts data only and verifies content identifiers and relational
references on read and write.

## Usage

``` r
write_review_examples(x, path, overwrite = FALSE)

read_review_examples(path, max_bytes = 50 * 1024^2)
```

## Arguments

- x:

  A value from
  [`review_examples()`](https://jameshwade.github.io/scans/reference/review_examples.md).

- path:

  Local JSON file path.

- overwrite:

  Whether to replace an existing file.

- max_bytes:

  Maximum file size to read, in bytes. Defaults to 50 MiB.

## Value

`write_review_examples()` returns `x` invisibly;
`read_review_examples()` returns the verified example set.
