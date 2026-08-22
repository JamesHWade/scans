# v0 trajectory contract

## Status

Accepted by the maintainer on 2026-08-22 for implementation in issue #4.

## Purpose

A scans trajectory bundle is a completed, source-neutral snapshot of one or
more logical agent paths. It preserves the ordered semantic records needed for
post-run diagnostics and the evaluation context needed to interpret those
records.

It does not execute an agent, stream live events, score task outcomes, or
reproduce a chat interface.

The terms run, trajectory, turn, event, evaluation, scan, and finding use the
definitions in `trajectory-concept-map.md`. In particular, evaluations are
inputs from an outcome-evaluation system; findings are outputs from scans.

## Public API

The initial core API should contain:

```r
TrajectoryBundle(
  trajectories,
  turns,
  events,
  evaluations = NULL,
  losses = NULL
)

as_trajectory(x, ...)
is_trajectory_bundle(x)

trajectory_info(x)
trajectory_turns(x)
trajectory_events(x)
trajectory_evaluations(x)
trajectory_losses(x)
```

`TrajectoryBundle()` is a customized S7 constructor. It accepts data frames,
coerces them to tibbles, and constructs correctly typed empty `evaluations` and
`losses` properties when those arguments are `NULL`. The class object is
exported so downstream packages can identify the formal class and register
methods against it.

`as_trajectory()` is an S7 generic for source adapters. It can dispatch on S7,
S3, R6, and base source classes. Adapter-specific arguments belong on the
applicable method or an explicit source function, not on the core constructor.

The five accessors return the bundle's tibble properties. Callers do not need
to use `@` or depend on property storage. A compact S7 print method reports
table counts, source types, run count, and statuses without printing event
values or metadata.

The contract does not initially include mutation verbs. A user may extract a
table for ordinary analysis. Functions that later return a modified bundle
must replace all affected properties as one operation and pass S7 validation.

## Bundle invariants

`TrajectoryBundle` has six formal properties: the five tibbles and
`schema_version = 1L`. Every bundle must satisfy these rules:

1. All five table properties are tibbles with their canonical columns, including
   zero-row components.
2. `trajectory_id`, `turn_id`, `event_id`, and `evaluation_id` are unique in
   their respective tables within one bundle.
3. Every turn, event, evaluation, and loss trajectory reference resolves to a
   row in `trajectories` when the reference is present.
4. Every event `turn_id` resolves to a turn in the same trajectory.
5. Every parent trajectory and parent event reference resolves within the
   bundle and does not form a cycle.
6. Indices are positive integers and unique within their parent scope. They do
   not need to remain contiguous after a documented selection operation.
7. List-column elements are bounded, serializable values. They cannot contain
   live environments, functions, connections, external pointers, R6 objects,
   or reactives.
8. Stable optional fields are represented by typed missing values, not by
   removing canonical columns.

S7 checks each property's declared class and runs the whole-object validator at
construction and after property replacement. `S7::validate()` is the explicit
manual validation entry point. `TrajectoryBundle()` runs the same validator
before S7 construction so invalid input raises a stable scans validation
condition. Direct property replacement uses S7's native validation error.

Tool calls and tool results use `call_id` for correlation. An unmatched call or
result remains valid data because unresolved tool activity is itself diagnostic
evidence.

## Identity and ordering

Canonical IDs are unique within one bundle. They are not promised to be
globally unique and must not be joined across bundles without source identity
or an application-owned key.

Adapters preserve an upstream ID in both the canonical ID and the appropriate
`source_id` or metadata field when it already satisfies bundle uniqueness.
When the source has no ID, adapters generate deterministic snapshot-local IDs:

```text
trajectory-000001
trajectory-000001/turn-000001
trajectory-000001/event-000001
trajectory-000001/evaluation-000001
```

The fixed-width ordinal is based on explicit source order. The exact string is
opaque to users, but the version-1 adapter must reproduce it for an unchanged
snapshot. Inserting or reordering upstream records may change generated IDs;
adapters must not imply cross-snapshot stability where the source provides
none.

Row order is never the ordering contract. `turn_index`, `event_index`, and
`content_index` carry the relevant order explicitly.

## Canonical tables

All canonical columns are always present. Adapters may preserve additional
columns, but package functions cannot require them until they become part of a
later schema version.

### `trajectories`

| Column | R type | Meaning |
|---|---|---|
| `trajectory_id` | character | Bundle-unique logical path ID. |
| `run_id` | character | Producer execution ID shared by related paths. |
| `parent_trajectory_id` | character | Parent path for delegated or nested agents. |
| `source_type` | character | Source adapter name. |
| `source_id` | character | Upstream path identity. |
| `source_uri` | character | Sanitized file or store locator. |
| `task_id` | character | Evaluation task identity. |
| `sample_id` | character | Evaluation sample identity. |
| `epoch` | integer | Repeated-evaluation epoch. |
| `agent` | character | Primary agent label. |
| `model` | character | Primary model label. |
| `started_at` | POSIXct | Known start time, normalized to UTC. |
| `completed_at` | POSIXct | Known completion time, normalized to UTC. |
| `status` | character | Completion or termination state. |
| `error` | character | Safe terminal error summary. |
| `metadata` | list | Named lists of sanitized source-specific data. |

`trajectory_id` and `source_type` cannot be missing. `metadata` elements are
uniquely named lists, using `list()` when empty.

### `turns`

| Column | R type | Meaning |
|---|---|---|
| `trajectory_id` | character | Parent trajectory ID. |
| `turn_id` | character | Bundle-unique turn ID. |
| `turn_index` | integer | Total order within the trajectory. |
| `round_index` | integer | Derived conversational round when known. |
| `role` | character | Semantic author role. |
| `input_tokens` | double | Input tokens when available. |
| `output_tokens` | double | Output tokens when available. |
| `cached_input_tokens` | double | Cached input tokens when available. |
| `cost` | double | Source-reported cost when available. |
| `duration` | double | Elapsed seconds when available. |
| `finish_reason` | character | Source completion reason. |
| `status` | character | Turn completion state. |
| `error` | character | Safe turn error summary. |
| `metadata` | list | Named lists of sanitized turn-specific data. |

`trajectory_id`, `turn_id`, `turn_index`, and `role` cannot be missing.

### `events`

| Column | R type | Meaning |
|---|---|---|
| `trajectory_id` | character | Parent trajectory ID. |
| `event_id` | character | Bundle-unique event ID. |
| `event_index` | integer | Total order within the trajectory. |
| `turn_id` | character | Containing semantic turn. |
| `content_index` | integer | Content order within the turn. |
| `parent_event_id` | character | Containment or causal parent event. |
| `event_type` | character | Stable event kind. |
| `content_type` | character | Stable content kind when applicable. |
| `name` | character | Tool or source event name. |
| `call_id` | character | Tool request/result correlation ID. |
| `text` | character | Safe searchable text when appropriate. |
| `value` | list | Typed, bounded payload; an element may be `NULL`. |
| `timestamp` | POSIXct | Known event time, normalized to UTC. |
| `duration` | double | Elapsed seconds when available. |
| `status` | character | Event completion state. |
| `error` | character | Safe event error summary. |
| `metadata` | list | Named lists of sanitized event-specific data. |

`trajectory_id`, `event_id`, `event_index`, `event_type`, `value`, and
`metadata` are required columns. Events outside semantic turns have missing
`turn_id` and `content_index`.

### `evaluations`

| Column | R type | Meaning |
|---|---|---|
| `trajectory_id` | character | Evaluated trajectory ID. |
| `evaluation_id` | character | Bundle-unique evaluation ID. |
| `task_id` | character | Evaluation task identity. |
| `sample_id` | character | Evaluation sample identity. |
| `epoch` | integer | Repeated-evaluation epoch. |
| `scorer` | character | Scorer or judge identity. |
| `value` | list | Outcome judgment. |
| `target` | list | Expected value or grading guidance. |
| `explanation` | character | Safe scorer rationale. |
| `metadata` | list | Named lists of metrics and scorer context. |

`trajectory_id`, `evaluation_id`, `value`, `target`, and `metadata` are
required columns. A bundle can have no evaluations or several evaluations for
one trajectory.

### `losses`

| Column | R type | Meaning |
|---|---|---|
| `trajectory_id` | character | Affected trajectory when known. |
| `turn_id` | character | Affected turn when known. |
| `event_id` | character | Affected event when known. |
| `field` | character | Canonical or source field affected. |
| `reason` | character | Loss category. |
| `detail` | character | Safe human-readable description. |
| `metadata` | list | Named lists of bounded loss context. |

Core loss reasons are `unsupported`, `redacted`, `truncated`, and
`externalized`. Loss rows must not repeat the sensitive or discarded value.

## Vocabularies and extensions

Schema version 1 defines these core values:

- roles: `system`, `user`, `assistant`, and `tool`;
- event types: `content`, `tool_call`, `tool_result`, `error`, and `custom`;
- content types: `text`, `thinking`, `citation`, `image`, `pdf`, `audio`,
  `document`, and `json`;
- loss reasons: `unsupported`, `redacted`, `truncated`, and `externalized`.

An adapter retains an unknown stable kind using a namespaced extension such as
`ellmer:content_foo` or `tempest:delegation`. A colon-free unknown value is
invalid. The adapter records a loss only when it cannot preserve the source
meaning or payload; using a faithful extension is not itself a loss.

Statuses and finish reasons remain open character vocabularies in v0 because
their useful meanings are producer-specific.

## Redaction and size limits

Adapters exclude provider request and response JSON by default. They replace
values for known sensitive keys with `"<redacted>"` while retaining the key and
recording a `redacted` loss. Sensitive keys include authorization and proxy
authorization headers, cookies, passwords, API keys, access and refresh
tokens, secrets, and credentials. Token-usage fields are not sensitive merely
because their names contain `token`. The same rule applies to named atomic
vectors and attributes, not only list elements.

Raw binary content is not embedded. It is represented by a safe reference and
an `externalized` loss when the representation differs from the source.

The default maximum retained size for one source payload is 65,536 UTF-8 bytes.
Adapters truncate or externalize larger payloads, retain a bounded summary when
safe, and add a loss row. An adapter may expose a stricter limit. Increasing or
disabling the default requires an explicit argument and must not bypass secret
redaction.

Source URIs may retain a scheme, host, and safe path. They must remove embedded
credentials and secret query parameters.

## Worked ellmer mapping

Given this completed semantic sequence:

```text
UserTurn: "What is the weather?"
AssistantTurn: ContentToolRequest(id = "call_1", name = "weather")
UserTurn: ContentToolResult(request_id = "call_1", value = "Sunny")
AssistantTurn: ContentText("It is sunny.")
```

the adapter creates one trajectory and four turns. The event table contains:

| `event_index` | `turn_id` | `event_type` | `content_type` | `name` | `call_id` | `text` |
|---:|---|---|---|---|---|---|
| 1 | `trajectory-000001/turn-000001` | `content` | `text` | NA | NA | `What is the weather?` |
| 2 | `trajectory-000001/turn-000002` | `tool_call` | NA | `weather` | `call_1` | NA |
| 3 | `trajectory-000001/turn-000003` | `tool_result` | NA | `weather` | `call_1` | `Sunny` |
| 4 | `trajectory-000001/turn-000004` | `content` | `text` | NA | NA | `It is sunny.` |

The structured tool arguments and result remain in `value`. Assistant usage,
duration, and finish reason belong to their turn rows. Provider-specific extras
are excluded unless a caller explicitly requests sanitized metadata.

Call identifiers are correlation evidence, not unique keys. When a snapshot
reuses one for multiple calls or results, adapters leave `parent_event_id`
missing rather than inventing a unique causal parent. Scans report the
ambiguity and pair only as many call and result records as the observed counts
support.

## Initial integration scope

The first ellmer adapter accepts a completed `Chat` or a completed list of
public ellmer turns. It never sends a prompt.

The first vitals adapter should accept both an evaluated `Task`, through its
public `get_samples()` method, and a data frame returned by
`vitals_log_read()`. scans should not parse the Inspect-compatible log file
itself in v0. Run and task IDs remain missing when the public input does not
contain them, unless the user supplies those identities explicitly.

A shinychat-specific adapter is deferred. Its semantic ellmer client already
uses the ellmer adapter, while rendered UI state and branching require a later
presentation-aware extension.

## Compatibility and evolution

- Schema version 1 fixes canonical column names, key meanings, core vocabulary,
  and deterministic ID rules.
- New optional canonical columns may be added without changing schema version
  when old readers can preserve them as extra columns.
- Removing a column, changing its type or meaning, tightening a previously
  valid invariant, or changing generated IDs requires a new schema version.
- Unknown extra columns and namespaced vocabulary values must survive
  construction, validation, access, and supported serialization round trips.
- Adapter changes that discard additional source data require a documented loss
  and a user-facing changelog entry.
- Dedicated cross-language serialization is deferred. Schema-1 values must be
  safe for R serialization and must not depend on live process state.

Until scans reaches 1.0, the package version may evolve quickly, but stored
bundles identify their schema independently of the package version.

## Decisions resolved from the concept map

1. The class and customized constructor are S7 `TrajectoryBundle`; adapters use
   the S7 `as_trajectory()` generic.
2. Canonical IDs are bundle-unique, not globally namespaced.
3. Core role, event, content, and loss vocabularies are small; source additions
   use namespaced extensions.
4. Missing IDs use fixed-width, snapshot-local ordinal paths.
5. The first vitals adapter accepts evaluated task samples and
   `vitals_log_read()` output, but does not parse log files.
6. A shinychat presentation adapter is deferred beyond v0.
7. Provider JSON is excluded by default, sensitive values are always redacted,
   binary values are externalized, and retained source payloads default to a
   65,536-byte limit.
