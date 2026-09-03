# Convert OpenTelemetry GenAI spans

`as_trajectory_otel()` builds a
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
from OpenTelemetry spans that follow the GenAI semantic conventions, as
emitted by ellmer and recorded by Posit Connect's content observability
store. Read those spans with
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md).

Each conversation is reconstructed from its most recent `chat` span:
provider instrumentation records the whole message history on every
model call, so the last call carries the complete exchange.
`execute_tool` spans contribute the timing and failure of each tool
call, which the message history alone does not record.

Spans are grouped into conversations by the `gen_ai.conversation.id`
attribute, walking up the span's ancestry to find it. A conversation
without that attribute anywhere in its ancestry falls back to its trace
id, which groups a single model call rather than a session.

## Usage

``` r
as_trajectory_otel(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
)

otel_group_conversations(spans)
```

## Arguments

- x:

  A list of conversations, as returned by
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md),
  or a flat list of spans to be grouped.

- trajectory_id:

  Optional trajectory identifier. Must be `NULL` when `x` holds more
  than one conversation.

- source_uri:

  Optional URI recording where the spans were read from.

- metadata:

  Optional named list attached to every trajectory.

- spans:

  A flat list of parsed spans.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## See also

[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
to fetch spans from Posit Connect.
