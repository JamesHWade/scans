# Adapters and building your own

An adapter snapshots a completed run from one framework into the
`TrajectoryBundle` contract. All adapters share three rules:

- **Post-run and read-only.** An adapter reads a settled value. It never
  calls a provider, replays a tool, or reaches into live objects.
- **No invention.** When the source lacks something the contract can
  express, the adapter records a **loss** naming the field and the
  reason (`unsupported`, `redacted`, `truncated`, or `externalized`)
  rather than filling the gap.
- **Sanitized.** Sensitive named fields in tool arguments, results, and
  metadata are redacted before they enter a bundle, and the redaction is
  itself a loss.

[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
dispatches on the input’s class. Sources that arrive as unclassed lists
or tibbles use an explicit `as_trajectory_*()` function so scans never
guesses what a bare list is.

## ellmer

[`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md)
converts a completed `Chat` or a list of public turn objects. It is also
the foundation the other adapters reuse for model-visible turns.

``` r

request <- ellmer::ContentToolRequest("call-1", "lookup", list(id = 7L))
turns <- list(
  ellmer::UserTurn("Look up record 7"),
  ellmer::AssistantTurn(list(request)),
  ellmer::UserTurn(list(ellmer::ContentToolResult("found", request = request))),
  ellmer::AssistantTurn("Record 7 was found.")
)
bundle <- as_trajectory(turns)
trajectory_events(bundle)[, c("event_type", "name", "call_id", "status")]
#> # A tibble: 4 × 4
#>   event_type  name   call_id status   
#>   <chr>       <chr>  <chr>   <chr>    
#> 1 content     NA     NA      completed
#> 2 tool_call   lookup call-1  completed
#> 3 tool_result lookup call-1  completed
#> 4 content     NA     NA      completed
```

Text, tool requests and results, thinking, and token usage become
events. Tool results link to their request through `call_id`; an
ambiguous match is left unlinked and reported by the
`ambiguous_tool_correlation` scan rather than guessed.

## OpenTelemetry GenAI spans

[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
accepts the conversations returned by
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md),
or a flat list of spans that it groups itself. The most recent `chat`
span of a conversation carries the whole message history; `execute_tool`
spans add each call’s duration and failure. Non-GenAI span and resource
attributes are kept in trajectory metadata. See
[`vignette("connect")`](https://jameshwade.github.io/scans/articles/connect.md).

## vitals

[`as_trajectory_vitals()`](https://jameshwade.github.io/scans/reference/as_trajectory_vitals.md)
converts an evaluated vitals `Task` or the samples returned by
[`vitals::vitals_log_read()`](https://vitals.tidyverse.org/reference/vitals_log_read.html).
Each sample and epoch becomes a trajectory with a stable identifier, the
solver chat is converted through the ellmer adapter, and scores join as
evaluations:

``` r

bundle <- as_trajectory_vitals(task)
trajectory_evaluations(bundle)

samples <- vitals::vitals_log_read(log_path)
bundle <- as_trajectory_vitals(samples, source_uri = log_path)
```

## Deputy

A completed Deputy result adds run lifecycle events, tool timing, usage,
immutable run context, and delegation identity to its ellmer turns.
Passing a list of results keeps each run as its own trajectory, and a
child is linked through `parent_trajectory_id` only when its parent is
in the same snapshot:

``` r

bundle <- as_trajectory_deputy(list(parent_result, child_result))
trajectory_info(bundle)[, c("trajectory_id", "run_id", "parent_trajectory_id")]
```

Unresolved parent identifiers and missing run identifiers are recorded
as losses.

## dsprrr

A dsprrr module converts through its public trace exporter; an already
exported trace tibble uses the explicit function because it has no class
of its own:

``` r

bundle <- as_trajectory(program)

traces <- dsprrr::export_traces(program, include_prompts = TRUE, include_outputs = TRUE)
bundle <- as_trajectory_dsprrr(traces)
```

Each trace becomes a trajectory with token, cost, latency, model, and
output context. The program artifact digest is the source identity; it
establishes integrity, not authenticity.

## commons

commons reconstructs conversations and their provenance from
OpenTelemetry traces. Read through commons, then convert the settled
value:

``` r

conversations <- commons::trajectory_read("path/to/otel-traces")
bundle <- as_trajectory_commons(conversations)
filter_trajectory_events(bundle, event_type = "commons:provenance")
```

Model-visible turns reuse the ellmer adapter. Activity time, the local
or Connect source descriptor, trust tags, and citation decisions stay
inspectable as `commons:provenance` events. Missing or conflicting
provenance becomes a loss; scans never infers a stronger trust claim.

## Tempest

Tempest exposes a bounded, deterministic review of a completed STORM or
Co-STORM product. The adapter uses that authoritative projection rather
than re-deriving it:

``` r

review <- tempest::tempest_trajectory_review(result)
bundle <- as_trajectory_tempest(review)
filter_trajectory_events(bundle, event_type = "tempest:join")
```

The product is one trajectory. Stage order is authoritative; agent runs,
programs, knowledge, evidence identities, joins, and findings keep their
source order. Prompts, responses, source content, paths, and credentials
are excluded and recorded as losses.

## Build a bundle from your own framework

[`TrajectoryBundle()`](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
accepts rectangular data directly. Non-empty tables need only a few
columns; optional canonical columns are filled with typed missing values
and extra columns are kept.

| Table          | Required columns                                         |
|----------------|----------------------------------------------------------|
| `trajectories` | `trajectory_id`, `source_type`                           |
| `turns`        | `trajectory_id`, `turn_id`, `turn_index`, `role`         |
| `events`       | `trajectory_id`, `event_id`, `event_index`, `event_type` |
| `evaluations`  | `trajectory_id`, `evaluation_id`                         |
| `losses`       | `field`, `reason`, `detail`                              |

The scans rely on a handful of event columns: `event_type` values
`tool_call` and `tool_result`, the tool `name`, a `call_id` shared by a
call and its result, the canonical `value` list-column for tool-call
arguments and tool-result payloads, and `status` or `error` for
failures.

``` r

trajectories <- data.frame(
  trajectory_id = "run-1",
  source_type = "my-framework",
  status = "completed"
)

turns <- data.frame(
  trajectory_id = "run-1",
  turn_id = c("turn-1", "turn-2"),
  turn_index = 1:2,
  role = c("assistant", "user")
)

events <- tibble::tibble(
  trajectory_id = "run-1",
  turn_id = c("turn-1", "turn-2", "turn-2"),
  event_id = c("event-1", "event-2", "event-3"),
  event_index = 1:3,
  event_type = c("tool_call", "tool_result", "tool_result"),
  name = "search",
  call_id = c("call-1", "call-1", "call-2"),
  value = list(list(query = "Detroit"), "sunny", NULL),
  status = c("completed", "completed", "failed"),
  error = c(NA, NA, "timeout")
)

losses <- data.frame(
  trajectory_id = "run-1",
  field = "latency",
  reason = "unsupported",
  detail = "The framework does not record per-call latency."
)

bundle <- TrajectoryBundle(trajectories, turns, events, losses = losses)
scan_trajectories(bundle)[, c("severity", "label", "event_ids")]
#> # A tibble: 2 × 3
#>   severity label                 event_ids
#>   <chr>    <chr>                 <list>   
#> 1 warning  Unmatched tool result <chr [1]>
#> 2 error    Event error           <chr [1]>
trajectory_losses(bundle)
#> # A tibble: 1 × 7
#>   trajectory_id turn_id event_id field   reason      detail             metadata
#>   <chr>         <chr>   <chr>    <chr>   <chr>       <chr>              <list>  
#> 1 run-1         NA      NA       latency unsupported The framework doe… <list>
```

Validation errors name the table and column so a new adapter can be
built up incrementally. Once the tables are right, wrap the conversion
in a function and, if your framework’s objects carry a class, register
an
[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
method for it with
[`S7::method()`](https://rconsortium.github.io/S7/reference/method.html).
