# Convert completed Deputy agent results

`as_trajectory_deputy()` snapshots one or more completed
[`deputy::AgentResult`](https://jameshwade.github.io/deputy/reference/AgentResult.html)
objects into a canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
It delegates model-visible turns to
[`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md)
and adds ordered, namespaced Deputy lifecycle events.

## Usage

``` r
as_trajectory_deputy(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
)
```

## Arguments

- x:

  A completed
  [`deputy::AgentResult`](https://jameshwade.github.io/deputy/reference/AgentResult.html),
  or a non-empty list of completed results.

- trajectory_id:

  A bundle-unique trajectory ID. When `NULL`, a stable ID is derived
  from the Deputy run ID. It must be `NULL` when `x` contains multiple
  results.

- source_uri:

  An optional source locator. Credentials, query parameters, and
  fragments are removed.

- metadata:

  A uniquely named list of safe application metadata, applied to every
  result. Sensitive fields are redacted and recorded as losses.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

Run, session, agent, delegation, usage, and immutable run-context values
are retained as sanitized data. When a result's parent run occurs in the
same input, the adapter resolves `parent_trajectory_id`. Otherwise it
retains the parent run ID as metadata and records an adapter loss.
Missing run identity is also explicit. The live R6 results, providers,
tools, callbacks, and credentials are never retained.

[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
dispatches to this adapter for Deputy results and non-empty lists
containing only Deputy results when Deputy is installed.

## Examples

``` r
if (
  rlang::is_installed("deputy", version = "0.0.0.9000") &&
    rlang::is_installed("ellmer", version = "0.4.2")
) {
  result <- deputy::AgentResult$new(
    turns = list(
      ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
      ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
    ),
    run_id = "run-001",
    agent_name = "assistant"
  )

  bundle <- as_trajectory_deputy(result)
  trajectory_events(bundle)
}
#> # A tibble: 2 × 17
#>   trajectory_id  event_id      event_index turn_id content_index parent_event_id
#>   <chr>          <chr>               <int> <chr>           <int> <chr>          
#> 1 deputy/run-001 deputy/run-0…           1 deputy…             1 NA             
#> 2 deputy/run-001 deputy/run-0…           2 deputy…             1 NA             
#> # ℹ 11 more variables: event_type <chr>, content_type <chr>, name <chr>,
#> #   call_id <chr>, text <chr>, value <list>, timestamp <dttm>, duration <dbl>,
#> #   status <chr>, error <chr>, metadata <list>
```
