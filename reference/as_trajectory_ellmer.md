# Convert completed ellmer conversations

`as_trajectory_ellmer()` snapshots a completed ellmer `Chat`, a list of
exported ellmer turn objects, or one ellmer turn into a canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
It never sends a prompt or retains the live chat, provider, tools,
callbacks, or credentials.

## Usage

``` r
as_trajectory_ellmer(
  x,
  trajectory_id = NULL,
  run_id = NULL,
  source_id = NULL,
  source_uri = NULL,
  task_id = NULL,
  sample_id = NULL,
  epoch = NULL,
  agent = NULL,
  model = NULL,
  metadata = list(),
  include_system_prompt = TRUE
)
```

## Arguments

- x:

  A completed ellmer `Chat`, a list of ellmer turns, or one ellmer turn.

- trajectory_id:

  A bundle-unique trajectory ID. When `NULL`, the adapter uses the
  deterministic snapshot-local ID `"trajectory-000001"`.

- run_id, source_id, task_id, sample_id:

  Optional correlation identities.

- source_uri:

  An optional source locator. Credentials, query parameters, and
  fragments are removed.

- epoch:

  An optional positive evaluation epoch.

- agent:

  An optional agent label.

- model:

  An optional model label. For a `Chat`, the public `get_model()` value
  is used by default.

- metadata:

  A uniquely named list of safe application metadata. Sensitive keys are
  redacted and recorded in the bundle's losses table.

- include_system_prompt:

  Whether a `Chat` system prompt should be included as a system turn.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
dispatches to this adapter for ellmer chats, turns, and lists of turns
when ellmer is installed.

Text, thinking, tool requests, tool results, remote images, inline
images, and PDFs receive canonical event representations. Tool results
retain the request ID and point to the matching request event when it is
present. Provider response JSON is excluded by default, inline binary
content is externalized, sensitive named fields are redacted, and
unknown content classes produce a namespaced custom event containing
their sanitized properties. A loss is recorded only for properties that
cannot be retained exactly.

Ellmer does not provide stable IDs for chats, turns, or content blocks.
The adapter therefore generates fixed-width IDs from source order.
Re-converting an unchanged snapshot produces the same IDs, while
inserting or reordering turns or content may change them.

## Examples

``` r
if (rlang::is_installed("ellmer", version = "0.4.2")) {
  turns <- list(
    ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
  )

  bundle <- as_trajectory_ellmer(turns)
  trajectory_events(bundle)
}
#> # A tibble: 2 × 17
#>   trajectory_id     event_id   event_index turn_id content_index parent_event_id
#>   <chr>             <chr>            <int> <chr>           <int> <chr>          
#> 1 trajectory-000001 trajector…           1 trajec…             1 NA             
#> 2 trajectory-000001 trajector…           2 trajec…             1 NA             
#> # ℹ 11 more variables: event_type <chr>, content_type <chr>, name <chr>,
#> #   call_id <chr>, text <chr>, value <list>, timestamp <dttm>, duration <dbl>,
#> #   status <chr>, error <chr>, metadata <list>
```
