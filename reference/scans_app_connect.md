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
  jobs = TRUE,
  investigations = FALSE,
  reviews = FALSE,
  chat_factory = NULL,
  days = 7,
  scanners = NULL
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
  the reader. When both are omitted, each load reads the `days` days
  ending at load time. Supply either bound as `NULL` explicitly to leave
  that side of the window open.

- reader:

  Which trace reader to use: `"otel"` (the default) for this package's
  native reader, or `"commons"` for
  [`commons::trajectory_read()`](https://posit-dev.github.io/commons/r/reference/trajectory_read.html).

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

- investigations:

  Whether to enable investigation save/open controls, forwarded to
  [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md).
  Defaults to `FALSE`.

- reviews:

  Whether to enable session-local review/example controls, forwarded to
  [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md).
  Defaults to `FALSE`.

- chat_factory:

  Optional zero-argument function returning an ellmer chat, for example
  `function() ellmer::chat_openai()`. Defaults to `NULL`, which omits
  Ask. Tools configured on this client are replaced with the bounded
  scans tools; its system prompt is retained with evidence instructions.

- days:

  Length in days of the rolling window read when `from` and `to` are
  both omitted. The default is 7.

- scanners:

  Optional custom scanners: one
  [`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
  or a list of them. They appear with the built-in scans in Options, run
  with them on the loaded bundle, and their findings and assessment
  status are shown the same way. A saved investigation shows its saved
  custom results without them; applying current scanners reruns only the
  scanners supplied here. Scanners that could never run are refused
  rather than ignored: with a single saved investigation opened with
  `investigations = FALSE`, and with a comparison, whose sides are the
  saved analyses it compares (rerun each side with
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
  and compare again). Custom scanners are deterministic functions, so
  browsing never calls a model.

## Value

A [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object.

## Details

The app is described in
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md):
an overview with facets, a review queue with one-key dispositions when
an annotation store is supplied, exports, and Deputy and provenance
sections when the traces record them. Exports and the R snippet never
contain the Connect server address or API key; the snippet names the
content GUID and window so the read can be repeated.

Two readers are available. The default, `"otel"`, uses this package's
own
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
with
[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md):
it needs no other package, falls back to Connect's per-job trace
endpoint when the aggregate one fails, and keeps the per-call tool
timings and failures the span history carries. `"commons"` routes
through
[`commons::trajectory_read()`](https://posit-dev.github.io/commons/r/reference/trajectory_read.html)
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
