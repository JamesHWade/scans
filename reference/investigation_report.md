# Report a saved investigation

`investigation_report()` computes a shareable report from a saved
investigation without starting the app. It applies the saved view's
filters to the saved evidence and analysis, exactly as
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
does when it opens the same investigation, and returns the resulting
tables. `write_investigation_report()` renders them as one standalone
HTML file or as Markdown;
[`format()`](https://rdrr.io/r/base/format.html) returns the same text.

## Usage

``` r
investigation_report(
  x,
  reviews = NULL,
  app_url = NULL,
  include_users = FALSE,
  include_errors = FALSE,
  verify = TRUE
)

write_investigation_report(
  x,
  path,
  format = NULL,
  max_evidence = 100L,
  overwrite = FALSE
)

# S3 method for class 'scans_investigation_report'
format(x, format = c("markdown", "html"), max_evidence = 100L, ...)
```

## Arguments

- x:

  A saved investigation from
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
  or
  [`read_investigation()`](https://jameshwade.github.io/scans/reference/investigation_files.md).
  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
  is captured first with
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
  and its defaults. For `write_investigation_report()` and
  [`format()`](https://rdrr.io/r/base/format.html), a report.

- reviews:

  Optional list of
  [`review_case()`](https://jameshwade.github.io/scans/reference/review_case.md)
  values made from this investigation's snapshot. Their adjudicated
  status is reported beside the evidence they refer to.

- app_url:

  Optional address of a
  [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
  deployment: an `http://` or `https://` host and path, with no
  credentials, query, or fragment. Without it the report has anchors
  only.

- include_users:

  Whether to print user identifiers and the values of free-form source
  details. Defaults to `FALSE`.

- include_errors:

  Whether to print the most frequent recorded error text per tool.
  Defaults to `FALSE`, which prints the count only.

- verify:

  Whether to recompute the snapshot and revision identifiers and refuse
  an investigation they no longer describe. Defaults to `TRUE`. Use
  `FALSE` only for a value just returned by
  [`read_investigation()`](https://jameshwade.github.io/scans/reference/investigation_files.md)
  or
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md).

- path:

  File to write. The extension chooses the format unless `format` is
  given: `.html` or `.htm` for HTML, `.md` for Markdown.

- format:

  `"html"` or `"markdown"`.

- max_evidence:

  Largest number of trajectories listed per pattern. The rendered report
  says when it lists fewer than it counted; the `evidence` table always
  holds them all.

- overwrite:

  Whether to replace an existing file. Defaults to `FALSE`.

- ...:

  Unused.

## Value

`investigation_report()` returns a `scans_investigation_report`: a list
of ordinary tibbles and scope details.

- `scope`: counts in scope, `filters` in words, the time `window`, the
  `source` read, snapshot identifiers, scanner `settings`, and what
  links cannot restore.

- `headline`: one row of counts with their denominators, including
  `n_assessed` and `n_not_assessed`.

- `patterns`: one row per finding type with affected `trajectories`, the
  number the scan `assessed`, the `share` of those affected (`NA` when
  it assessed none), and first and last occurrence.

- `tools`, `models`, and, when requested, `users`: breakdowns. Findings
  in a model or user are counted over its `n_assessed` trajectories.

- `measures`: each resource measure by recording coverage.

- `coverage`: trajectories per scanner and assessment status.

- `losses`: capture losses by field and reason.

- `trajectories`: the cohort, one row per trajectory, with its class.

- `evidence`: one row per finding with trajectory, turn, and event
  identities, its anchor in the rendered report, and links.

- `reviews`: one row per supplied review case.

  `write_investigation_report()` returns the report invisibly.
  [`format()`](https://rdrr.io/r/base/format.html) returns Markdown
  lines or one HTML string.

## Details

The report states its scope (application, time window, every active
filter, and what the source read covered), headline counts with their
denominators, patterns with reach, tool failures, resource measurements
with recording coverage, capture limits and losses, and the evidence
behind each pattern. It never runs a scanner, calls a model, or contacts
the source.

## What the report can and cannot establish

Findings describe recorded behavior. A trajectory with no findings is
reported as clean only when a selected scan could assess it; one that no
scan could assess is counted as not assessed. A measurement that was not
recorded is printed as unknown, never as zero. No findings does not
establish success, and complete recording of the loaded records does not
establish that the source captured everything.

## Content

The report contains identities and scanner explanations, not transcript
text: no message text, tool arguments, or tool results. A
[`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
writes its own explanations, which the report prints as written, so a
custom scanner that quotes the transcript puts that text in the report;
the report then says which scans' explanations it prints. Tool names,
model names, and trajectory, turn, and event identifiers do appear.
Recorded error strings are often tool-result text, so the tool table
counts errors without their text unless `include_errors = TRUE`. User
identifiers are omitted unless `include_users = TRUE`; a text filter
that matches a user identifier is then described without its term.
Review rationales and expected behavior are free text and are never
printed.

Source details a caller recorded with the investigation print their
values only for the fields a source reader records about its read
(`read_at`, `from`, `to`, `n`, `spans`, `spans_total`, `max_spans`,
`truncated`, `incomplete`, `conversations_found`, `conversations`,
`files`, and `lines_dropped`). Any other field is free-form and may name
a person, so it is listed by name with its value withheld unless
`include_users = TRUE`.

## Integrity

The report checks the investigation's structure and evidence references,
and recomputes the snapshot and revision identifiers to check that they
still describe the content, so an investigation edited in R after it was
read is refused rather than reported under its original identifiers.
Recomputing them hashes the whole investigation, which on a large one
takes longer than the report. A caller that has just read or built the
value can pass `verify = FALSE`; the report then labels its identifiers
as not re-verified, and says so beside any review cases matched by them.
Build a changed revision with
[`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
rather than editing a saved value.

## Links

With `app_url`, each trajectory and event links to that app. A link sets
the filters the report names and clears every other filter in the app
that opens it. It reopens exactly the reported cohort when that app
serves this saved investigation; an app reading the live source applies
the same filters to whatever it holds then. The report prints this, and
names any filter a link cannot carry, under its scope.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "run-1", source_type = "manual",
             status = "failed"),
  data.frame(), data.frame()
)
saved <- investigation_snapshot(bundle, application = "Support assistant")
report <- investigation_report(saved)
report$headline[c("n", "n_assessed", "n_findings")]
#> # A tibble: 1 × 3
#>       n n_assessed n_findings
#>   <int>      <int>      <int>
#> 1     1          1          1
report$evidence[c("scan", "trajectory_id")]
#> # A tibble: 1 × 2
#>   scan             trajectory_id
#>   <chr>            <chr>        
#> 1 trajectory_error run-1        

if (requireNamespace("htmltools", quietly = TRUE)) {
  path <- tempfile(fileext = ".html")
  write_investigation_report(report, path)
}
```
