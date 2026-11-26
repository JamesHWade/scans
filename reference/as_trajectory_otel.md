# Convert OpenTelemetry GenAI spans

`as_trajectory_otel()` builds a
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
from OpenTelemetry spans that follow the GenAI semantic conventions, as
emitted by ellmer and recorded by Posit Connect's content observability
store or written to local OTLP files. Read those spans with
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
or
[`read_otel_traces()`](https://jameshwade.github.io/scans/reference/read_otel_traces.md).

Each conversation is reconstructed from its most recent `chat` span:
provider instrumentation records the whole message history on every
model call, so the last call carries the complete exchange.
`execute_tool` spans contribute the timing and failure of each tool
call, which the message history alone does not record.

A shinychat conversation-titling call (system instructions beginning
"You title chat conversations.") is never chosen over another model
call, even when it started last. Its usage stays in the conversation's
totals in `metadata$otel`, and `metadata$otel$title_calls` reports how
many title calls there were and their tokens.

When the chosen span records its output but omits
`gen_ai.input.messages`, as ellmer does when a turn holds content it
cannot serialize, the history is recovered from the latest earlier model
call in the conversation that recorded its input, followed by the output
of each later call. A span with neither attribute had message capture
turned off and is left contentless. Messages that no span recorded are
reported as `capture$input` or `capture$tool_result` losses, so
[`assess_trajectory_scans()`](https://jameshwade.github.io/scans/reference/assess_trajectory_scans.md)
does not treat the transcript as complete.

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
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
  or
  [`read_otel_traces()`](https://jameshwade.github.io/scans/reference/read_otel_traces.md),
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

## commons agents

commons wraps each turn in a `commons_conversation_turn` span and
records each answer's provenance in a `commons_provenance` span. These
spans are kept with their conversation, never counted against a reader's
span budget, and never used as a transcript, so token totals, model
calls, and resource measures are unchanged by them. Each exchange of the
transcript becomes a `"commons:provenance"` event in the form
[`as_trajectory_commons()`](https://jameshwade.github.io/scans/reference/as_trajectory_commons.md)
gives: `value` holds `provenance_tag` and the full `citation_decisions`
parsed from `commons.citation.candidates` (sanitized and bounded by the
payload limit, with a loss when truncated), `metadata$exchange_index`
numbers the exchange, and each turn's `round_index` is its exchange. As
in commons, a user turn opens an exchange unless it returns tool results
or is an internal reminder; reminder turns carry
`commons_internal = TRUE` in their metadata. Unlike the commons reader,
`timestamp` is the time the record's span ended. A record attaches to an
exchange only when the history its turn recorded matches the transcript,
and conflicting records for one exchange leave it unclassified with a
loss. The Connect user commons records (`commons.connect.user`) is kept
as `metadata$otel$user`. commons setup spans such as
`commons_agent_create` belong to no conversation and are ignored.

## Deputy agents

Deputy wraps each agent run in a `deputy.run` span. Like commons' spans
it is kept with its conversation, never counted against a span budget,
and never used as a transcript or a measure. A conversation's root runs,
the turns of one chat, stay one trajectory. Each subagent run (one with
`deputy.parent.run.id`) becomes its own trajectory, holding the model
and tool calls under its run span, with the ID
`otel/<conversation>/<run ID>` and `parent_trajectory_id` set when its
parent run is in the same input. Otherwise the parent run ID stays in
metadata with a `parent_run_id` loss. A model or tool call that no run's
ancestry reaches stays with the lead and is reported with a `deputy.run`
loss.

The latest run speaks for its trajectory. `run_id` is its
`deputy.run.id`, and metadata carries `session_id`, `agent_id`,
`parent_run_id`, `delegation_id`, `stop_reason`, and the ten-field
`ending` described in
[`as_trajectory_deputy()`](https://jameshwade.github.io/scans/reference/as_trajectory_deputy.md).
The span records only the stop reason, the final finish reason, and
whether the answer was cut off, so `by`, `field`, `actual`, `limit`,
`wrapped_up`, and `phase` are `NA`. `class` is `"RunEndingAnswered"` or
`"RunEndingCutOff"` for stop reason `"complete"`, `"RunEndingStopped"`
for any other reason Deputy exports by name except a limit or failure
reason, and `NA` for those and for `"custom"`, because a caller or hook
can stop a run with a limit's or failure's own word. The trajectory
`status` then follows the Deputy adapter's mapping from `class`, or from
the stop reason when `class` is `NA`.

Deputy exports nine event types as span events, each with only
identifiers and decisions. They become events named as the Deputy
adapter names the same events, timestamped from the span event:

|  |  |  |
|----|----|----|
| Span event | Bundle event | Notes |
| `deputy.permission` | `deputy:permission` | `call_id` from `tool_call_id`; `"failed"` when `decision` is `"deny"` |
| `deputy.structured_attempt` | `deputy:structured_attempt` | `"failed"` when `valid` is false; no attempted value |
| `deputy.fallback` | `deputy:fallback` | `fallback_index` |
| `deputy.compaction` | `deputy:compaction` | successful compactions only |
| `deputy.hook` | `deputy:hook` | `hook`, `decision` |
| `deputy.file_checkpoint` | `deputy:file_checkpoint` | `checkpoint_id` |
| `deputy.delegation` | `deputy:delegation` | `delegation_id` |
| `deputy.warning` | `deputy:warning` | `code`; no message |
| `deputy.stop` | `deputy:stop` |  |

Other attributes stay in event `metadata`. Deputy does not export its
other event types, so OTel bundles have no `deputy:approval`,
`deputy:limit_wrap_up`, `deputy:compaction_error`, tool, request, or
content events, and no tool names, inputs, results, or error messages. A
run span's first 1,000 span events are read; a span with more is
reported with a `capture$events` loss counting the events left out.
Conversations without a `deputy.run` span are unaffected.

Spans are grouped into conversations by the `gen_ai.conversation.id`
attribute, walking up the span's ancestry to find it. A conversation
without that attribute anywhere in its ancestry falls back to its trace
id, which groups a single model call rather than a session.

## See also

[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
to fetch spans from Posit Connect, and
[`read_otel_traces()`](https://jameshwade.github.io/scans/reference/read_otel_traces.md)
to read local trace files.
