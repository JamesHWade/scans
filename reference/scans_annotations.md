# Store trajectory annotations

`scans_annotations()` creates the annotation store the app writes to
when a reviewer labels or comments on a trajectory. Pass it to
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
or
[`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
through their `annotations` argument; without one the app stays strictly
read-only and shows no annotation controls.

Annotations are an append-only log: saving never rewrites or deletes an
earlier record, and an application's trajectory annotations are every
record written against that pair, newest first. The application
namespace matters because trajectory identifiers are unique only within
one bundle. A log rather than a mutable field means two reviewers
working at once cannot overwrite each other, and that a judgement made
about a trajectory remains visible after it is revised.

## Usage

``` r
scans_annotations(
  path = NULL,
  labels = c("looks right", "wrong answer", "tool failure", "follow up")
)
```

## Arguments

- path:

  Path to the log file. Each line is one JSON record. Defaults to
  `annotations.jsonl` under `tools::R_user_dir("scans", "data")`. On
  Posit Connect, point this at a directory every viewer's process can
  write to; the per-user default is not shared.

- labels:

  The labels a reviewer may apply, as a character vector.

## Value

A `scans_annotations` object: a list of `read()`,
[`append()`](https://rdrr.io/r/base/append.html), and `labels` for the
app to use. [`append()`](https://rdrr.io/r/base/append.html) requires
both an application namespace and a trajectory identifier; `read()` can
filter by either. Labels outside the store's configured vocabulary are
rejected; a missing label remains valid for note-only annotations.

## Examples

``` r
store <- scans_annotations(path = tempfile(fileext = ".jsonl"))
store$append(
  "Support assistant",
  "trajectory-1",
  label = "follow up",
  note = "Tool loop here."
)
store$read("Support assistant", "trajectory-1")
#> # A tibble: 1 × 6
#>   application       trajectory_id label     note      author created_at         
#>   <chr>             <chr>         <chr>     <chr>     <chr>  <dttm>             
#> 1 Support assistant trajectory-1  follow up Tool loo… runner 2026-09-04 21:27:24
```
