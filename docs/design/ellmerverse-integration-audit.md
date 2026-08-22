# Ellmerverse+ integration audit

## Purpose

This audit identifies the first integration seams beyond ellmer and vitals.
It is pinned to upstream development commits on 2026-08-22 and considers only
public, settled, post-run state. Live clients, private R6 fields, network reads,
and provider calls are outside the adapter seam.

## Source inventory

| Package | Development snapshot | Public post-run source | Information beyond ellmer | Decision |
| --- | --- | --- | --- | --- |
| shinychat 0.4.0.9000 | [`c6121f1`](https://github.com/posit-dev/shinychat/tree/c6121f15192472d058241a9ad987d1b35a84dbbc) | `chat_server()` returns the current client, status, last error, and last turn. `ConversationStore` documents a branched record containing turns and rendered UI. | Rendered messages, attachments, errors, conversation identity, active branch, inactive branches, and application state. | Documentation only. The record is not available through a public settled snapshot accessor, and its semantic turns remain ellmer-owned. |
| commons 0.0.0.9002 | [`4b2cd5a`](https://github.com/posit-dev/commons/tree/4b2cd5a92e573506dbd9341d61072de6780eb84a) | `trajectory_read()` returns named conversations of ellmer turns with source, activity time, and exchange provenance attributes. | Conversation identity, OpenTelemetry provenance, trust tags, and citation decisions. | Complete explicit adapter. Accept already-read output and perform no Connect or filesystem I/O. |
| dsprrr 0.0.0.9000 | [`0215110`](https://github.com/JamesHWade/dsprrr/tree/0215110b73a823fa1a3891b5b57d54f873d1ee47) | `export_traces()` snapshots module traces; `inspect_history()` exposes recent calls; run methods accept `.trace_context`; `program_artifact_id()` supplies verified executable identity. | Module identity, prompts and outputs, token/cost/latency measures, program artifact identity, and caller-supplied correlation context. | Complete source adapter. Prefer `export_traces(..., include_outputs = TRUE)` and retain trace context without treating artifact integrity as authenticity. |
| deputy 0.0.0.9000 | [`c474452`](https://github.com/JamesHWade/deputy/tree/c474452dadd387c5c5f14874cc3789f244b54d32) | `AgentResult` contains settled turns, ordered `AgentEvent`s, usage, session/run/agent identity, delegation identity, and immutable run context. | Run lifecycle, tool timings and failures, budgets, delegation hierarchy, stable run identity, and product context. | Complete source adapter and first implementation target. It has the deepest public source interface and a distinct class. |
| tempest 0.3.0.9000 | [`2e38fc8`](https://github.com/JamesHWade/tempest/tree/2e38fc8f43efa56019998374981cb0b58534883f) | `tempest_trajectory_review()` returns a deterministic bounded review of stages, agent runs, programs, knowledge, evidence, joins, and findings. | Product and stage identity, Deputy/dsprrr correlation, program identity, evidence authority, explicit join strength, and product findings. | Complete explicit adapter. Preserve the review's authority distinctions and record its excluded prompts, responses, and source content as losses. |

## Package findings

### shinychat

The public `ConversationStore` interface documents the useful settled record:
conversation identity, a node graph, a current leaf, serialized ellmer turns,
rendered UI, timestamps, client information, and application values. However,
standard callers cannot construct the non-exported partition required by
`ConversationStore$get()`, and `chat_server()` exposes only live reactive state
plus the semantic ellmer client.

scans must not inspect the internal `HistoryController`, synthesize a
partition, or substitute rendered messages for model-visible turns. The
integration path is therefore:

1. Continue using `as_trajectory_ellmer()` for the semantic client snapshot.
2. Ask shinychat to expose a public immutable conversation-record snapshot.
3. Once available, add rendered messages and branch identity as a supplement,
   retaining inactive branches without changing the active semantic path.

### commons

`trajectory_read()` already performs the difficult OpenTelemetry reconstruction
and returns oldest-first named conversations. Each value is a list of ellmer
turns with `last_active` and exchange `provenance`; the outer list identifies
its local or Connect source.

The result has no discriminating class, so scans should provide an explicit
`as_trajectory_commons()` rather than extend broad list-shape guessing in
`as_trajectory()`. The adapter accepts an already-read value, delegates turn
normalization to the ellmer adapter, then retains conversation IDs, source,
trust tags, and citation decisions. Reading Connect traces remains commons'
responsibility.

### dsprrr

`export_traces()` is the stable snapshot seam. With outputs enabled it can
retain prompts, results, reconstructed turns, model usage, latency, cost,
`program_artifact_id`, and validated `trace_context`. The context may correlate
a trace to Deputy or Tempest, but dsprrr deliberately does not manufacture a
host run identity.

A dsprrr adapter can produce one trajectory per exported trace and record the
verified program artifact ID in source metadata. A host run ID may be promoted
only when the caller supplied it explicitly in trace context. The digest is an
integrity identity, not an authenticity claim.

### deputy

`AgentResult` is the best next adapter source. It snapshots the exact ellmer
turns alongside ordered start, tool, text, turn, usage, warning, checkpoint,
and stop events. Run-boundary and tool events carry agent, session, run,
parent-run, delegation, tool-call, and immutable run-context fields.

The adapter should delegate semantic turn conversion to the ellmer adapter,
then add lifecycle events and promote only Deputy-owned identifiers. A child
`AgentResult` becomes a child trajectory when `parent_run_id` identifies a
parent result included in the same input. Otherwise the parent ID is retained
as metadata and the missing parent is recorded without inventing a trajectory.

### tempest

`tempest_trajectory_review()` is already a purpose-built post-run projection.
It is deterministic, bounded, credential-free, and explicit about the strength
of each join. Its stage order is authoritative, while agent runs and other
lanes are canonical sets. Correlation-only joins never imply causation.

The adapter should create a product trajectory, stage and agent-run events,
and diagnostic losses/findings. Program, evidence, and knowledge identities
remain structured metadata until scans has a shared semantic table for them.
Prompts, responses, source content, and live objects are intentionally absent;
the adapter records that omission and never reaches back into `TempestResult`.

## Correlation fixture

The `ellmerverse_correlation` fixture models four independent source views of
one research step:

- Tempest owns `research-run-001` and references a Deputy run and dsprrr
  program artifact.
- Deputy owns `deputy-run-001`, tool lifecycle, and immutable product context.
- dsprrr owns the verified program artifact and carries the host IDs in
  caller-supplied trace context.
- ellmer owns the model-visible turns and tool call content.

The fixture deliberately leaves `parent_trajectory_id` empty. These are source
views correlated by exact retained identifiers, not an agent delegation tree.
Tests require the Deputy run, dsprrr program artifact, Tempest references, and
ellmer tool-call identity to agree without collapsing their source identities.

## Implementation order

1. Implement the Deputy `AgentResult` adapter in #18.
2. Implement the dsprrr module/trace adapter in #19 and verify Deputy
   run-context correlation.
3. Implement the explicit commons adapter from already-read trajectories in
   #20.
4. Implement the explicit Tempest review adapter using its closed projection
   in #21.
5. Revisit shinychat only after a public settled history snapshot exists.

The first four adapters remain optional. Each gets a focused child issue and
provider-free conformance fixtures before implementation.
