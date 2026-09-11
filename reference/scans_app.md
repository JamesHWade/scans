# Explore trajectory diagnostics with the scans app

`scans_app()` launches a Shiny app for exploring one or more
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
snapshots or saved
[`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
values. A named list creates an application switcher; each entry can be
a bundle, saved investigation, or a zero-argument loader that returns
either. Lazy loaders make it practical to review snapshots from multiple
deployed apps without downloading every snapshot when the review app
starts.

## Usage

``` r
scans_app(
  x,
  annotations = NULL,
  investigations = inherits(x, "scans_investigation"),
  reviews = FALSE,
  chat_factory = NULL
)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md),
  a saved
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md),
  or a named list of application sources. Each source can be either
  value or a zero-argument function that returns one. Source names are
  shown in the application switcher.

- annotations:

  Optional
  [`scans_annotations()`](https://jameshwade.github.io/scans/reference/scans_annotations.md)
  store. When supplied, the app shows an annotation panel for the
  selected trajectory and appends what reviewers write to that store.
  The store also supplies current annotation filter membership when
  opening a saved investigation. Without a store, saved membership is
  used and the app writes only explicitly requested investigation
  downloads.

- investigations:

  Whether to enable investigation save, upload, and rescan controls.
  Defaults to `TRUE` for a single saved investigation and `FALSE` for
  other inputs. Set explicitly for named sources or lazy loaders.

- reviews:

  Whether to enable session-local evidence review and example export
  controls. Defaults to `FALSE`. Download selected examples to keep them
  after closing the session; no shared review store is written.

- chat_factory:

  Optional zero-argument function returning an ellmer chat, for example
  `function() ellmer::chat_openai()`. Defaults to `NULL`, which omits
  Ask. Tools configured on this client are replaced with the bounded
  scans tools; its system prompt is retained with evidence instructions.

## Value

A [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object. Calling `scans_app()` at the console launches the app; the
returned object can also be served from an `app.R`.

## Details

The scans app keeps the canonical bundle as its data boundary. Ordinary
inspection does not call a model, replay tools, or modify a bundle.
Caller-supplied loaders are invoked only when their application is first
selected in a session or explicitly reloaded.

The app opens with an application overview of the filtered trajectories:
recurring findings, elapsed-time quantiles, recorded token usage, and
evidence coverage. Selecting a pattern filters the browser; selecting a
trajectory opens its evidence. Elapsed time includes any pauses between
user messages and is not model response latency. Unknown measurements
remain unknown. Token counts use preserved conversation totals when
available, otherwise sum known turn values, and may be partial.

The app also provides filters and a trajectory browser, a source-neutral
transcript and event stream, and the findings, evaluations, and adapter
losses associated with the selected trajectory. Built-in findings are
computed with
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
when each application snapshot is first loaded.

## Ask about the evidence

Supply `chat_factory` to enable an optional Ask tab beside Findings. It
must return an ellmer chat configured with your chosen provider. Each
session clones the client, clears its prior turns and tools, and
registers only
[`scans_tools()`](https://jameshwade.github.io/scans/reference/scans_tools.md).
Provider requests begin when the user submits a question. The permitted
trajectories and snapshot are fixed at submission; browsing while a
response streams cannot change them. Changing scope starts fresh model
context while keeping earlier answers and drafts visible. Links in older
answers open their retained snapshot, with a return-to-source action.
Chat history and evidence references last only for the current session.
The provider receives the submitted question and any retained evidence
its tools read. Existing redactions remain intact; no new anonymization
occurs. A recent shinychat with `page_chat_theme()` and
`chat_server()$set_client()` is required for this surface.

## Saved investigations

Set `investigations = TRUE` to enable save/open controls in a regular
app. They are enabled by default when `x` is a single saved
investigation. Save visible trajectories downloads the selected retained
evidence, analysis, scanner settings, and browser state as JSON. The
preview describes the content policy before download. Open an
investigation restores a file in the current session. Saved diagnostics
remain unchanged until the reviewer chooses Apply current scanners;
saving again records a parent revision. Opening does not contact the
original source. The default upload limit is 50 MiB. An explicit
`shiny.maxRequestSize` option sets both the app upload ceiling and its
investigation reader limit; `Inf` disables the size limit. See
[`investigation_files()`](https://jameshwade.github.io/scans/reference/investigation_files.md).

## Posit Connect

Use
[`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
when Connect content observability is enabled. It reads native OTLP
traces for named deployed applications and supplies the lazy loaders
used by the application switcher. Custom loaders remain useful for
completed snapshots held outside Connect's trace store.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(
    trajectory_id = "trajectory-1",
    source_type = "manual"
  ),
  data.frame(),
  data.frame()
)

if (interactive()) {
  scans_app(bundle)

  scans_app(list(
    "Support assistant" = bundle,
    "Research assistant" = function() readRDS("research-bundle.rds")
  ))
}
```
