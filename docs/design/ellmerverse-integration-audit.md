# Ellmerverse+ integration audit

## Purpose

This audit identifies the first integration seams beyond ellmer and vitals.
It was first pinned to upstream development commits on 2026-08-22. The deputy
and commons sections and inventory rows were refreshed on 2026-11-25; the
shinychat, dsprrr, and tempest rows still reflect the 2026-08-22 pins. It
considers only public, settled, post-run state. Live clients, private R6 fields, network reads,
and provider calls are outside the adapter seam.

## Source inventory

| Package | Development snapshot | Public post-run source | Information beyond ellmer | Decision |
| --- | --- | --- | --- | --- |
| shinychat 0.4.0.9000 | [`c6121f1`](https://github.com/posit-dev/shinychat/tree/c6121f15192472d058241a9ad987d1b35a84dbbc) | `chat_server()` returns the current client, status, last error, and last turn. `ConversationStore` documents a branched record containing turns and rendered UI. | Rendered messages, attachments, errors, conversation identity, active branch, inactive branches, and application state. | Documentation only. The record is not available through a public settled snapshot accessor, and its semantic turns remain ellmer-owned. |
| commons 0.2.0.9000 | [`820659f`](https://github.com/posit-dev/commons/tree/820659fca10f0ae6284d0a76016c186a3e63f420) (2026-11-24) | `trajectory_read()` returns named conversations, each a list of `turns` (ellmer turns), `last_active`, and `user`; the turns carry a `provenance` attribute of per-exchange records. | Conversation identity, authenticated Connect user, OpenTelemetry provenance, trust tags, and citation decisions. | Complete explicit adapter. Accept already-read output and perform no Connect or filesystem I/O. |
| dsprrr 0.0.0.9000 | [`0215110`](https://github.com/JamesHWade/dsprrr/tree/0215110b73a823fa1a3891b5b57d54f873d1ee47) | `export_traces()` snapshots module traces; `inspect_history()` exposes recent calls; run methods accept `.trace_context`; `program_artifact_id()` supplies verified executable identity. | Module identity, prompts and outputs, token/cost/latency measures, program artifact identity, and caller-supplied correlation context. | Complete source adapter. Prefer `export_traces(..., include_outputs = TRUE)` and retain trace context without treating artifact integrity as authenticity. |
| deputy 0.0.0.9007 | [`bfc7747`](https://github.com/JamesHWade/deputy/tree/bfc77473acc26a76f5f4f4310cdb78d1f2a298b6) (2026-11-25, on `feat/mcp-console-0.4-267`; not yet on `main`) | The S7 `AgentResult` contains settled turns, ordered S7 `AgentEvent`s, usage, session/run/agent identity, delegation identity, immutable run context, and a `RunEnding` with `finish_reason` and `cut_off`. `delegation_history()` replays a saved `$export_subagents()` snapshot. | Run lifecycle and how each run ended, tool timings and failures, budgets, delegation hierarchy, stable run identity, and product context. | Complete source adapter and first implementation target. It has the deepest public source interface and a distinct class. |
| tempest 0.3.0.9000 | [`2e38fc8`](https://github.com/JamesHWade/tempest/tree/2e38fc8f43efa56019998374981cb0b58534883f) | `tempest_trajectory_review()` returns a deterministic bounded review of stages, agent runs, programs, knowledge, evidence, joins, and findings. | Product and stage identity, Deputy/dsprrr correlation, program identity, evidence authority, explicit join strength, and product findings. | Complete explicit adapter. Preserve the review's authority distinctions and record its excluded prompts, responses, and source content as losses. |

## Package findings

### shinychat

This section still reflects the 2026-08-22 pin (shinychat 0.4.0.9000,
`c6121f1`) and has not been re-audited. Issue #9 tracks the refresh.

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

Audited at posit-dev/commons `820659f` (`pkg-r/DESCRIPTION` version
0.2.0.9000), 2026-11-25. `trajectory_read()` (`pkg-r/R/trajectory-read.R`)
performs the OpenTelemetry reconstruction and returns oldest-first conversations
named by conversation ID. Each conversation is a list with:

- `turns`: ellmer turns. Their `provenance` attribute holds one record per
  exchange, built from the `commons_provenance` span
  (`provenance_tag`, `citation_decisions`). An exchange with no record in the
  read window has an empty record.
- `last_active`: a `POSIXct` of the most recent chat activity.
- `user`: the authenticated Shiny session user from the earliest recorded
  non-title chat (span attribute `commons.connect.user`), or `NA_character_`
  for anonymous sessions and older logs.

commons records internal reminder turns: user turns whose content is only
`<system-instruction>...</system-instruction>` (or the older `<reminder>`
form), created as `ContentTurnReminder` (`pkg-r/R/turn-reminder.R`). The reader
leaves them in `turns`; it only refuses to start a new exchange on one
(`split_exchanges()`), so provenance is attached to the real user question.
A consumer that treats every user turn as a person's message will overcount.

Commons emits `commons_provenance` and `commons_conversation_turn` spans from
tracer `co.posit.r-package.commons` (`pkg-r/R/tracing.R`), and reads Connect
traces or local OTLP NDJSON files. Connect reading requires Connect
2026.09.0 or later (`trajectory_read()` documentation).

The result still has no discriminating class, so scans provides an explicit
`as_trajectory_commons()` rather than extending list-shape guessing in
`as_trajectory()`. The adapter accepts an already-read value, delegates turn
normalization to the ellmer adapter, then retains conversation IDs, source,
trust tags, and citation decisions. As of this audit the adapter does not
retain `user` or distinguish reminder turns from user messages; both are
candidates for follow-up work. Reading Connect traces remains commons'
responsibility.

The scans app may compose that reader with the explicit adapter without moving
the I/O seam into `as_trajectory_commons()`: `scans_app_connect()` creates one
lazy source per allow-listed Connect content item, calls
`commons::trajectory_read()` only when selected, and then converts the completed
result. The Connect endpoint compatibility logic therefore remains localized in
commons while scans owns review, caching, and application switching.

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

Audited at JamesHWade/deputy `bfc7747` (`DESCRIPTION` version 0.0.0.9007),
2026-11-25. That commit is on `origin/feat/mcp-console-0.4-267` and not on
`main` at the time of audit.

`AgentResult` (`R/agent-result.R`) is an exported, read-only S7 class and the
best adapter source. It snapshots the exact ellmer turns alongside ordered
`AgentEvent`s (also S7) and usage, with session, run, agent, parent-agent,
parent-run, delegation, and `run_context` fields. Fields are read with `$`.

Two properties describe how a run ended:

- `ending` is a `RunEnding` (`R/run-ending.R`), one of `RunEndingAnswered`,
  `RunEndingStopped`, `RunEndingLimit`, `RunEndingCutOff`, or
  `RunEndingFailed`. A host need not parse `stop_reason`.
- `finish_reason` is the provider's finish reason for the final response, and
  `cut_off` is `TRUE` when the provider truncated it. A cut-off response
  without another stop reason keeps `stop_reason = "complete"`, so
  `result_is_success()` is `TRUE`; a truncated limit wrap-up keeps
  `RunEndingLimit` and also sets `cut_off`.

`AgentEvent` types (documented in `R/agent-result.R`) are `start`,
`request_start`, `request_end`, `request_error`, `text`, `text_complete`,
`content`, `tool_start`, `tool_end`, `turn`, `permission`, `hook`, `approval`,
`trusted_result`, `compaction_start`, `compaction`, `compaction_error`,
`limit_wrap_up`, `fallback`, `structured_attempt`, `file_checkpoint`,
`run_error`, `usage`, and `stop`. All but `text`, `text_complete`, and
`content` carry agent, session, and run identity, plus parent and delegation
IDs in subagent runs. `structured_attempt` and `request_error` may hold
sensitive data or original conditions.

Delegation history is public for settled runs. `$export_subagents()` on an
`Agent` or `LeadAgent` (`R/agent.R`) returns a plain, versioned list of
finished delegations, and the exported `delegation_history()`
(`R/delegation-inspection.R`) replays it as redacted views with ellmer turns.
Replay re-checks a `DelegationDisclosure`, never runs tools or a model, and
marks artifact references `availability = "unresolved"`. It is a separate
source of subagent conversations from the `parent_run_id` links on results.

Deputy records OpenTelemetry spans in `R/agent-tracing.R`: a `deputy.run` span
per run from tracer `co.posit.r-package.deputy`, with `deputy.run.id`,
`deputy.agent.id`, `deputy.parent.run.id`, `deputy.delegation.id`,
`deputy.session.id`, and `gen_ai.conversation.id` (the host's conversation ID
when set, else the session), plus `deputy.stop_reason`, `deputy.finish_reason`,
and `deputy.cut_off` at the end. Governance events (checkpoint, warning, stop,
permission, fallback, structured attempt, hook, compaction, delegation) are
span events with an allowlisted set of identifier fields only; prompts, tool
inputs and results, and run context are deliberately not exported. These spans
are an alternative route to a deputy run through the OTel adapter, not a
replacement for `AgentResult`.

The adapter should delegate semantic turn conversion to the ellmer adapter,
then add lifecycle events and promote only Deputy-owned identifiers. A child
`AgentResult` becomes a child trajectory when `parent_run_id` identifies a
parent result included in the same input. Otherwise the parent ID is retained
as metadata and the missing parent is recorded without inventing a trajectory.
`ending`, `finish_reason`, and `cut_off` are candidates to retain as run
outcome metadata.

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

`tools/rebuild-tempest-fixture.R` still uses obsolete graft calls, tracked by
JamesHWade/graft#91. Until that lands, the Tempest fixture cannot be rebuilt
with the script as written.

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
