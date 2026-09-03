# Review Posit Connect traces with the scans app

`scans_app_connect()` creates a
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
whose application switcher reads completed conversations from Posit
Connect's content observability store. Each application is read when it
is first selected in any session, and the snapshot is then shared by
every session of the deployment until it is thirty minutes old or
**Reload traces** fetches it again.

## Usage

``` r
scans_app_connect(
  x,
  n = 100L,
  from = NULL,
  to = NULL,
  reader = c("otel", "commons"),
  annotations = NULL,
  jobs = TRUE
)
```

## Arguments

- x:

  A named character vector or list of Posit Connect content GUIDs. Names
  appear in the application switcher. Sources must belong to the
  `CONNECT_SERVER` that supplies the review app's API key.

- n:

  The maximum number of recent conversations to read per application.
  The default is 100. Use `NULL` to read all available conversations.

- from, to:

  Optional lower-inclusive and upper-exclusive activity bounds passed to
  the reader. When both are omitted, each load reads the seven days
  ending at load time. Supply either bound as `NULL` explicitly to leave
  that side of the window open.

- reader:

  Which trace reader to use: `"otel"` (the default) for this package's
  native reader, or `"commons"` for
  [`commons::trajectory_read()`](https://posit-dev.github.io/commons/reference/trajectory_read.html).

- annotations:

  Optional
  [`scans_annotations()`](https://jameshwade.github.io/scans/reference/scans_annotations.md)
  store, forwarded to
  [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md).

- jobs:

  Passed to
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md):
  whether to also read the retained per-job trace stores. `FALSE` makes
  loads noticeably faster on deployments with many past jobs. Ignored by
  the `"commons"` reader.

## Value

A [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object.

## Details

Two readers are available. The default, `"otel"`, uses this package's
own
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
with
[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md):
it needs no other package, falls back to Connect's per-job trace
endpoint when the aggregate one fails, and keeps the per-call tool
timings and failures the span history carries. `"commons"` routes
through
[`commons::trajectory_read()`](https://posit-dev.github.io/commons/reference/trajectory_read.html)
and
[`as_trajectory_commons()`](https://jameshwade.github.io/scans/reference/as_trajectory_commons.md)
instead, which suits agents built with commons because it also recovers
their provenance records.

Connect trace access requires `CONNECT_SERVER`, `CONNECT_API_KEY`, and
editor access to every selected content item. Content observability must
be enabled on the server and on each application. Because the review app
reads with its own Connect API key, deploy it only to viewers who may
inspect all configured applications and their potentially sensitive
conversations.

## Examples

``` r
if (interactive()) {
  scans_app_connect(c(
    "Support assistant" = "11111111-1111-4111-8111-111111111111",
    "Research assistant" = "22222222-2222-4222-8222-222222222222"
  ))
}
```
