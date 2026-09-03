# Explore trajectory diagnostics with the scans app

`scans_app()` launches a read-only Shiny app for exploring one or more
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
snapshots. A named list creates an application switcher; each entry can
be a bundle or a zero-argument loader that returns one. Lazy loaders
make it practical to review snapshots from multiple deployed apps
without downloading every snapshot when the review app starts.

## Usage

``` r
scans_app(x, annotations = NULL)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md),
  or a named list of application sources. Each source must be a
  `TrajectoryBundle` or a zero-argument function that returns one.
  Source names are shown in the application switcher.

- annotations:

  Optional
  [`scans_annotations()`](https://jameshwade.github.io/scans/reference/scans_annotations.md)
  store. When supplied, the app shows an annotation panel for the
  selected trajectory and appends what reviewers write to that store.
  Without one the app makes no writes at all.

## Value

A [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object. Calling `scans_app()` at the console launches the app; the
returned object can also be served from an `app.R`.

## Details

The scans app keeps the canonical bundle as its data boundary. It does
not call a model, run tools, modify a bundle, or infer missing source
facts. Caller-supplied loaders are invoked only when their application
is first selected in a session or explicitly reloaded.

The app provides filters and a trajectory browser, a source-neutral
transcript and event stream, and the findings, evaluations, and adapter
losses associated with the selected trajectory. Built-in findings are
computed with
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
when each application snapshot is first loaded.

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
