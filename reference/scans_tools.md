# Create tools for exploring retained trajectories

`scans_tools()` creates six read-only ellmer tools backed by
deterministic scans queries. The tools inspect a fixed copy of the
supplied evidence; they cannot fetch sources, replay recorded tools,
execute R, or write reviews. Register the returned tools with an ellmer
chat using `chat$set_tools()`.

## Usage

``` r
scans_tools(x, trajectory_ids = NULL, max_rows = 100L, max_chars = 4000L)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
  or saved
  [`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md).

- trajectory_ids:

  Permitted trajectory identifiers. `NULL` permits all retained
  trajectories; [`character()`](https://rdrr.io/r/base/character.html)
  permits none. Model arguments can narrow this scope but cannot broaden
  it.

- max_rows:

  Maximum rows returned by each query, from 1 to 1000.

- max_chars:

  Maximum characters per field, from 100 to 16000. Larger fields are
  returned as explicit slices. Repeat the same paginated query with
  `content_offset` to retrieve subsequent slices.

## Value

A named list of ellmer tools: `scans_list_trajectories`,
`scans_summarize`, `scans_read_trajectory`, `scans_find_events`,
`scans_get_findings`, and `scans_measure`.

## Details

Tools return row-oriented JSON with data, snapshot and analysis
identities, evidence references, coverage limitations, and explicit
pagination. Counts and descriptive statistics are computed over the
requested population in R, not over the returned page. Missing values
remain JSON `null`.

A saved investigation retains its saved diagnostics and scanner
settings. To recompute them, explicitly create a new
[`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md)
from its bundle and supply that separate investigation.

## Examples

``` r
if (rlang::is_installed("ellmer", version = "0.5.0")) {
  bundle <- TrajectoryBundle(
    data.frame(trajectory_id = "run-1", source_type = "manual"),
    data.frame(), data.frame()
  )
  tools <- scans_tools(bundle)
}
```
