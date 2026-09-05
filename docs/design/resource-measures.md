# Resource and capture measures

Contract version 1, implementing #40. `measure_trajectories()` is the shared
resource interface for scripts and the app. It returns a long tibble with one
row per trajectory and measure. `summarize_trajectories()` keeps its existing
turn-based sums, including raw costs; its columns do not change meaning.

## Meaning and coverage

Every row carries a value, unit, aggregation, source, source unit,
`n_recorded`, `n_total`, coverage, and a limitation. Counts refer to records in
the loaded bundle, never to all calls made by an application. `complete` means
every eligible captured record supplies this measure; `partial` means some
do; `unavailable` means none do or the measure is unsupported. `unknown` is
reserved for older conversation totals whose recording denominator was not
preserved. A known zero is a value; an empty or unmeasured set is `NA`.

| Measure | Value and denominator |
| --- | --- |
| `elapsed` | Seconds between trajectory bounds; one pair for canonical sources. For new OTel captures, all captured spans must have valid bounds, with the span count as denominator. May include user pauses. |
| `input_tokens`, `output_tokens`, `cached_input_tokens` | Sum of known counts across captured OTel chat calls, falling back to canonical assistant turns (plus any other turns explicitly carrying usage). Each component has its own coverage. Cached tokens are already part of input tokens and must not be added again. |
| `turn_duration` | Sum of recorded canonical turn durations; all turns with duration or assistant role are eligible. Does not assert that a turn equals one model call. |
| `model_duration` | Sum of captured OTel chat span durations. Inclusive of nested activity; not wall time. |
| `tool_duration` | Sum of captured OTel execute-tool spans, or canonical tool-call event durations. Inclusive of nested work; not wall time. |
| `recorded_work` | Union of valid captured chat/tool intervals, counting overlap once. Partial capture is a lower bound on recorded work. |
| `unattributed_elapsed` | Elapsed minus interval union, only when all captured chat/tool intervals are valid. This is time outside recorded work, not proof of idleness or user think time. |
| `first_chunk_latency` | Median of recorded `gen_ai.response.time_to_first_chunk` values across captured chat calls, in seconds. A first chunk may contain no token. |
| `first_token_latency` | Unavailable: supported adapters do not supply a proven per-request first-token timestamp or duration. |
| `cost` | Unavailable: current canonical costs lack a shared currency, price basis, and inclusion contract. Raw source costs remain in `summarize_trajectories()`. |

`currency` and `price_basis` remain missing while cost is unavailable. Future
cost comparisons must require compatible currency, price/version basis,
billing scope, and explicit inclusion of child work before producing deltas.
Neither a model name nor a numeric cost establishes these facts.

## Attribution and source preservation

Rows retain trajectory and parent identity. No measure pools parent and child
trajectories or subtracts children from inclusive parent durations. Overlapping
intervals within an OTel conversation are unioned before raw spans are discarded.
Never sum these per-trajectory unions to claim application wall time.

OTel message history is reconstructed once from the latest chat span. Resource
measures instead use each distinct captured span once. Nanosecond span bounds
are converted to seconds, with the original unit recorded. Negative, missing,
or nonfinite measurements are unavailable. The adapter retains bounded
aggregates and denominators in `metadata$otel$measures`, tagged with adapter
`as_trajectory_otel` and measurement contract version 1. Older bundles retain
their token-total preference, with unknown coverage; unsupported newer metadata
versions are not interpreted as version 1. Canonical timing remains in seconds.

Source read windows, capped loads, sampling, and disabled content capture can
omit work before conversion. Keep `read_connect_traces()` read information with
the investigation; even complete measurement coverage cannot establish capture
completeness. Contentless model spans can supply timing and usage but cannot
support transcript-based findings.

The OpenTelemetry [GenAI attributes](https://github.com/open-telemetry/semantic-conventions/blob/main/docs/registry/attributes/gen-ai.md)
define token components and time to first chunk. Its
[GenAI metrics](https://github.com/open-telemetry/semantic-conventions/blob/main/docs/gen-ai/gen-ai-metrics.md)
distinguish server time to first token. These conventions are evolving; this
contract supports the named attributes, without treating absent instrumentation
as zero or inventing a semantic-convention version.

## Delivery and verification

Prove repeated histories, duplicate spans, overlapping and nested intervals,
partial usage, model-only capture, invalid clocks, known zeros, empty bundles,
and legacy metadata through public adapter and measurement calls. Use the same
results for the app and the provider-free investigation recipe (#45). The
existing public Tempest review fixture demonstrates unsupported timing and
usage without inventing a complete capture. A deployed-app pilot with a second
R user remains tracked by #39; offline fixtures do not complete it.
