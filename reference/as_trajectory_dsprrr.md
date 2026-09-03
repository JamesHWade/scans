# Convert completed dsprrr traces

`as_trajectory_dsprrr()` snapshots one or more rows returned by
[`dsprrr::export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.html),
or a dsprrr module through that public exporter, into a canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
Each trace row becomes one trajectory. Model-visible turns are delegated
to
[`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md)
when present.

## Usage

``` r
as_trajectory_dsprrr(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
)
```

## Arguments

- x:

  A dsprrr module, or a data frame returned by
  [`dsprrr::export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.html).

- trajectory_id:

  A bundle-unique trajectory ID. When `NULL`, a stable ID is derived
  from the program artifact ID and trace order. It must be `NULL` when
  `x` contains multiple traces.

- source_uri:

  An optional source locator. Credentials, query parameters, and
  fragments are removed.

- metadata:

  A uniquely named list of safe application metadata, applied to every
  trace. Sensitive fields are redacted and recorded as losses.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

The program artifact digest becomes the source identity and is retained
as an integrity assurance, not an authenticity or trust claim. A
caller-supplied `run_id` or `deputy_run_id` in `trace_context` may
become the canonical `run_id`; product identifiers such as
`research_run_id` remain correlation metadata. The adapter never
manufactures Deputy or Tempest ownership.

[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
dispatches to this adapter for authenticated dsprrr module objects when
dsprrr is installed. Exported trace data frames use this explicit
adapter because they have no discriminating class.

## Examples

``` r
traces <- tibble::tibble(
  timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
  latency_ms = 10,
  input_tokens = 2L,
  cached_input_tokens = 0L,
  output_tokens = 1L,
  total_tokens = 3L,
  cost = 0,
  model = "fixture-model",
  prompt_length = 5L,
  prompt = "Hello",
  response = "Hi",
  program_artifact_id = paste0("sha256:", strrep("a", 64L)),
  trace_context = list(list(run_id = "run-001"))
)
as_trajectory_dsprrr(traces)
#> <TrajectoryBundle> schema 1
#> trajectories: 1
#> turns: 0
#> events: 1
#> evaluations: 0
#> losses: 1
#> sources: dsprrr
```
