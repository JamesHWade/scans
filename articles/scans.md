# Get started with scans

scans works on the record an agent leaves behind. It never calls a model
or runs a tool. This article introduces the trajectory model, the tour
from a completed run to a finding, and what each built-in scan means.

## The trajectory model

A **trajectory** is one agent run: a chat, a Deputy result, a Connect
conversation. A **bundle** holds one or more trajectories and is the
unit every scans function accepts.

Inside a trajectory:

- **Turns** are the conversational units, each with a role such as
  `user`, `assistant`, or `system`.
- **Events** are what happened within turns: text content, tool calls,
  tool results, thinking, errors, and source-specific records. Every
  event has an identifier, a position in canonical order, and optional
  timing, usage, and status.
- **Evaluations** are outcome judgements joined to a trajectory, for
  example a vitals score.
- **Losses** record what an adapter could not carry over from its
  source. A loss is explicit so a reviewer knows what is missing rather
  than guessing.

A **finding** is the output of a scan: a deterministic statement about a
trajectory that names the events supporting it.

## From a completed run to a finding

``` r

library(scans)

first_request <- ellmer::ContentToolRequest(
  "call-weather-1",
  "weather",
  list(city = "Detroit")
)
second_request <- ellmer::ContentToolRequest(
  "call-weather-2",
  "weather",
  list(city = "Detroit")
)

turns <- list(
  ellmer::UserTurn("What's the weather in Detroit?"),
  ellmer::AssistantTurn(list(first_request)),
  ellmer::UserTurn(list(ellmer::ContentToolResult(
    "Cloudy",
    request = first_request
  ))),
  ellmer::AssistantTurn(list(second_request))
)

bundle <- as_trajectory(turns)
bundle
#> <TrajectoryBundle> schema 1
#> trajectories: 1
#> turns: 4
#> events: 4
#> evaluations: 0
#> losses: 0
#> sources: ellmer
```

[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
dispatches on the input’s class. An ellmer `Chat` works the same way, as
do the classed results of the other supported frameworks.

### Look at the tables

Every accessor returns an ordinary tibble:

``` r

trajectory_info(bundle)
#> # A tibble: 1 × 16
#>   trajectory_id     run_id parent_trajectory_id source_type source_id source_uri
#>   <chr>             <chr>  <chr>                <chr>       <chr>     <chr>     
#> 1 trajectory-000001 NA     NA                   ellmer      NA        NA        
#> # ℹ 10 more variables: task_id <chr>, sample_id <chr>, epoch <int>,
#> #   agent <chr>, model <chr>, started_at <dttm>, completed_at <dttm>,
#> #   status <chr>, error <chr>, metadata <list>
trajectory_turns(bundle)[, c("turn_id", "role", "turn_index")]
#> # A tibble: 4 × 3
#>   turn_id                       role      turn_index
#>   <chr>                         <chr>          <int>
#> 1 trajectory-000001/turn-000001 user               1
#> 2 trajectory-000001/turn-000002 assistant          2
#> 3 trajectory-000001/turn-000003 user               3
#> 4 trajectory-000001/turn-000004 assistant          4
trajectory_events(bundle)[, c("event_id", "event_type", "name", "status")]
#> # A tibble: 4 × 4
#>   event_id                       event_type  name    status   
#>   <chr>                          <chr>       <chr>   <chr>    
#> 1 trajectory-000001/event-000001 content     NA      completed
#> 2 trajectory-000001/event-000002 tool_call   weather completed
#> 3 trajectory-000001/event-000003 tool_result weather completed
#> 4 trajectory-000001/event-000004 tool_call   weather completed
```

[`filter_trajectory_events()`](https://jameshwade.github.io/scans/reference/filter_trajectory_events.md)
selects events by trajectory, role, type, tool, status, or metadata and
adds the containing turn’s context in dot-prefixed columns:

``` r

filter_trajectory_events(bundle, event_type = "tool_call")[
  ,
  c("event_id", "name", ".turn_role")
]
#> # A tibble: 2 × 3
#>   event_id                       name    .turn_role
#>   <chr>                          <chr>   <chr>     
#> 1 trajectory-000001/event-000002 weather assistant 
#> 2 trajectory-000001/event-000004 weather assistant
```

### Summarize

[`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md)
returns one row per trajectory with counts of turns, events, tool
activity, unresolved calls, failures, token usage, and duration, so a
fleet of runs can be sorted and filtered before anyone opens a
transcript:

``` r

summarize_trajectories(bundle)[
  ,
  c("trajectory_id", "n_tool_calls", "n_unresolved_tool_calls")
]
#> # A tibble: 1 × 3
#>   trajectory_id     n_tool_calls n_unresolved_tool_calls
#>   <chr>                    <int>                   <int>
#> 1 trajectory-000001            2                       1
```

### Scan

[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
runs the built-in detectors and returns one finding per problem:

``` r

findings <- scan_trajectories(bundle)
findings[, c("severity", "label", "explanation")]
#> # A tibble: 2 × 3
#>   severity label                explanation                                     
#>   <chr>    <chr>                <chr>                                           
#> 1 warning  Repeated tool call   The same tool and arguments were requested repe…
#> 2 warning  Unresolved tool call No subsequent tool result can be paired with th…
findings$event_ids
#> [[1]]
#> [1] "trajectory-000001/event-000002" "trajectory-000001/event-000004"
#> 
#> [[2]]
#> [1] "trajectory-000001/event-000004"
```

Each finding carries `trajectory_id`. Event-backed findings also carry
`event_id` and `event_ids`, so those findings join directly back to
events. Record-level findings instead identify the failed trajectory or
turn through `trajectory_id` or `turn_id`. Finding identifiers are
stable across runs of the same bundle.

## What the built-in scans mean

``` r

scan_registry()
#> # A tibble: 9 × 3
#>   scan                       severity description                               
#>   <chr>                      <chr>    <chr>                                     
#> 1 ambiguous_tool_correlation warning  A call identifier matches more than one c…
#> 2 unresolved_tool_call       warning  A tool was called and no result came back.
#> 3 unmatched_tool_result      warning  A tool result arrived with no call to mat…
#> 4 repeated_tool_call         warning  The same tool was called with the same ar…
#> 5 suspicious_tool_loop       warning  Consecutive identical tool calls suggest …
#> 6 event_error                error    An event recorded a failure.              
#> 7 error_chain                error    A failure followed an earlier one within …
#> 8 turn_error                 error    A turn ended with a failed status, an err…
#> 9 trajectory_error           error    The trajectory ended with a failed status…
```

| Scan | It fires when |
|----|----|
| `unresolved_tool_call` | A tool was called and no result came back. |
| `unmatched_tool_result` | A result arrived with no preceding call to match it. |
| `ambiguous_tool_correlation` | A call identifier matches more than one call or result, so the pairing cannot be trusted. |
| `repeated_tool_call` | The same tool was called with the same arguments at least `repeat_threshold` times (default 2). Argument order does not matter. |
| `suspicious_tool_loop` | At least `loop_threshold` consecutive identical calls (default 3) suggest the agent is stuck. |
| `event_error` | An event recorded a failure. |
| `error_chain` | A failure followed an earlier one in the same trajectory, which often means the agent did not recover. |
| `turn_error` | A turn has a failed status, error text, or a truncating finish reason such as a token limit. |
| `trajectory_error` | The run itself ended with a failed status or an error, even when no event recorded it. |

Run a subset with `scans =`, and adjust the thresholds for agents whose
normal behaviour includes retries:

``` r

scan_trajectories(
  bundle,
  scans = c("repeated_tool_call", "event_error"),
  repeat_threshold = 3L
)
#> # A tibble: 0 × 13
#> # ℹ 13 variables: finding_id <chr>, scan_id <chr>, scan <chr>,
#> #   scan_version <chr>, trajectory_id <chr>, turn_id <chr>, event_id <chr>,
#> #   event_ids <list>, severity <chr>, label <chr>, value <list>,
#> #   explanation <chr>, metadata <list>
```

## Open the review app

[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
opens an application overview of the same bundle. It summarizes the
trajectories matching the browser filters, ranks recurring finding
patterns by affected trajectories, and shows elapsed-time quantiles,
recorded tokens, and evidence coverage. Select a pattern to narrow the
browser, then open a trajectory to see its transcript and evidence
links. The **Clear** link beside the active pattern restores the broader
selection.

Elapsed time runs from the recorded start to completion; in a
conversation it may include pauses between user messages. It is not
model response latency. Recorded tokens use preserved conversation
totals when available and otherwise sum known turn values. Usage may be
partial. The app shows coverage counts and does not replace unknown
values with zeros. Parent and delegated trajectories remain separate
observations. Finding prevalence describes captured trajectories and
does not measure answer quality.

``` r

scans_app(bundle)
```

Pass a named list to review more than one application. Entries may be
bundles or zero-argument functions that load one when the application is
first selected:

``` r

scans_app(list(
  "Support assistant" = support_bundle,
  "Research assistant" = function() load_research_bundle()
))
```

To let reviewers record labels and notes, pass a
[`scans_annotations()`](https://jameshwade.github.io/scans/reference/scans_annotations.md)
store. Records append to a JSON-lines log and are never rewritten, so
two reviewers cannot overwrite each other and a revised judgement keeps
the one before it:

``` r

store <- scans_annotations(
  "reviews/annotations.jsonl",
  labels = c("looks right", "wrong answer", "tool failure", "follow up")
)
scans_app(bundle, annotations = store)

store$read()
```

## Where to next

- [`vignette("connect")`](https://jameshwade.github.io/scans/articles/connect.md)
  covers reading traces from Posit Connect and deploying the review app
  there.
- [`vignette("adapters")`](https://jameshwade.github.io/scans/articles/adapters.md)
  describes what each adapter preserves and how to build a bundle from
  another framework.
