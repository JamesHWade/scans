# Ellmerverse+ integration audit

## Purpose

This audit identifies the first integration seams beyond ellmer and vitals.
It was first pinned to upstream development commits on 2026-08-22. The deputy
and commons sections and inventory rows were refreshed on 2026-11-25, and the
shinychat section and row on 2026-11-26; the dsprrr and tempest rows still
reflect the 2026-08-22 pins. It considers only public, settled, post-run
state. Live clients, private R6 fields, network reads, and provider calls are
outside the adapter seam.

## Source inventory

| Package | Development snapshot | Public post-run source | Information beyond ellmer | Decision |
| --- | --- | --- | --- | --- |
| shinychat 0.5.0 (pin), 0.5.0.9000 (`main`) | [`826c799`](https://github.com/posit-dev/shinychat/tree/826c799994c32611629236bbc73516dbc14ff2ab) (2026-09-09, pin); [`2a1d456`](https://github.com/posit-dev/shinychat/tree/2a1d456cb640692d58a51c8f58d87df8aa043d39) (2026-11-10, `main`) | `chat_server()` returns the current client, status, last error, last turn, and a reactive conversation ID. `ConversationStore` documents the top level of a branched record containing turns and rendered UI. | Inactive branches, other saved conversations, conversation identity and title, attachments, and application state. Displayed messages on the active path are derived from the ellmer turns. | Documentation only, re-confirmed 2026-11-26. The record is not available through a public settled snapshot accessor, its node shape is undocumented and due to change, and its semantic turns remain ellmer-owned. |
| commons 0.2.0.9000 | [`820659f`](https://github.com/posit-dev/commons/tree/820659fca10f0ae6284d0a76016c186a3e63f420) (2026-11-24) | `trajectory_read()` returns named conversations, each a list of `turns` (ellmer turns), `last_active`, and `user`; the turns carry a `provenance` attribute of per-exchange records. | Conversation identity, authenticated Connect user, OpenTelemetry provenance, trust tags, and citation decisions. | Complete explicit adapter. Accept already-read output and perform no Connect or filesystem I/O. |
| dsprrr 0.0.0.9000 | [`0215110`](https://github.com/JamesHWade/dsprrr/tree/0215110b73a823fa1a3891b5b57d54f873d1ee47) | `export_traces()` snapshots module traces; `inspect_history()` exposes recent calls; run methods accept `.trace_context`; `program_artifact_id()` supplies verified executable identity. | Module identity, prompts and outputs, token/cost/latency measures, program artifact identity, and caller-supplied correlation context. | Complete source adapter. Prefer `export_traces(..., include_outputs = TRUE)` and retain trace context without treating artifact integrity as authenticity. |
| deputy 0.0.0.9007 | [`bfc7747`](https://github.com/JamesHWade/deputy/tree/bfc77473acc26a76f5f4f4310cdb78d1f2a298b6) (2026-11-25, on `feat/mcp-console-0.4-267`; not yet on `main`) | The S7 `AgentResult` contains settled turns, ordered S7 `AgentEvent`s, usage, session/run/agent identity, delegation identity, immutable run context, and a `RunEnding` with `finish_reason` and `cut_off`. `delegation_history()` replays a saved `$export_subagents()` snapshot. | Run lifecycle and how each run ended, tool timings and failures, budgets, delegation hierarchy, stable run identity, and product context. | Complete source adapter and first implementation target. It has the deepest public source interface and a distinct class. |
| tempest 0.3.0.9000 | [`2e38fc8`](https://github.com/JamesHWade/tempest/tree/2e38fc8f43efa56019998374981cb0b58534883f) | `tempest_trajectory_review()` returns a deterministic bounded review of stages, agent runs, programs, knowledge, evidence, joins, and findings. | Product and stage identity, Deputy/dsprrr correlation, program identity, evidence authority, explicit join strength, and product findings. | Complete explicit adapter. Preserve the review's authority distinctions and record its excluded prompts, responses, and source content as losses. |

## Package findings

### shinychat

#### Refresh of 2026-11-26

Decision: the gap continues. No supported public shinychat interface returns
a settled snapshot of the displayed conversation. scans keeps the ellmer path
and adds no shinychat adapter or supplement.

Versions audited:

| What | Version | Commit | Date |
| --- | --- | --- | --- |
| Pin in `DESCRIPTION` `Remotes:` | 0.5.0 | [`826c799`](https://github.com/posit-dev/shinychat/tree/826c799994c32611629236bbc73516dbc14ff2ab) | 2026-09-09 |
| Latest R release (CRAN 0.5.0, tag `r/v0.5.0`) | 0.5.0 | `6cb1417` | 2026-09-13 |
| Upstream `main` | 0.5.0.9000 | [`2a1d456`](https://github.com/posit-dev/shinychat/tree/2a1d456cb640692d58a51c8f58d87df8aa043d39) | 2026-11-10 |
| Installed and tested here | 0.5.0 | not recorded by the install | built 2026-09-18 |

The pin is the release candidate: the release tag differs from it under
`pkg-r/` only in `pkgdown/_pkgdown.yml`. The pin is 33 commits behind `main`,
6 of which touch `pkg-r/R`. `pkg-r/NAMESPACE` is byte-identical at the pin, in
the installed package, and on `main` (33 exports). The installed package has
no remote metadata, so its commit is inferred from that match and from
`chat_server()` lacking `history$title()`, which only `main` has. File and
line references below are to `pkg-r/` at `2a1d456`.

What was searched: every export in `NAMESPACE`; all 32 help topics in
`pkg-r/man`; `NEWS.md`; both vignettes; the value `chat_server()` returns
(read from the function in the installed package and from `R/chat_app.R`);
`R/chat_history.R`, `R/chat_history_store.R`, `R/chat_history_types.R`,
`R/chat_history_client.R`, and `R/chat_restore.R`; and the upstream design
note `memory-bank/exchange-tree-history.md`. Exports and help topics were
also searched for names containing message, history, turn, transcript,
branch, snapshot, export, record, conversation, store, or restore.

| Public surface | Reference | Displayed messages | Tool activity | Branch state | Identity and order |
| --- | --- | --- | --- | --- | --- |
| `chat_server()` return: `client`, `last_turn`, `last_input`, `status`, `last_error` | `R/chat_app.R:84-160`, `:996-1014`; `?chat_app` | None. `client` is the ellmer `Chat`; the rest are live reactives of the latest exchange. | Only what the `Chat` turns hold. | None. The `Chat` holds the active path only. | ellmer turn order. |
| `chat_server()$history`: `conversation_id()`, `title()` (`main` only), `save()`, `on_save()`, `on_restore()` | `R/chat_app.R:127-158`, `:1022-1076` | None. | None. | None. | `conversation_id()` is the saved record's ID and is set on the client as `conversation_id`, which ellmer records as `gen_ai.conversation.id`. It is reactive, so it must be read inside the session. |
| `chat_enable_history()`, `history_options()` | `R/chat_history.R:740`, `:792`; `?chat_enable_history` | None. Returns a cancel function. `on_save` and `on_restore` receive only the application `values` list. | None. | None. | None. |
| `ConversationStore`, `FileConversationStore` | `R/chat_history_store.R:24-108`, `:338`, `:499`; `?ConversationStore` | Inside the record that `get()` returns, as `nodes[[id]]$ui`. | Inside `ui`, as rendered blocks. | Inside the record: `nodes`, `current_leaf`. | Record `id`; node IDs; no per-node timestamps. See below for why this is not a usable accessor. |
| `chat_restore()` | `R/chat_restore.R:74`; `?chat_restore` | None. Bookmarks the `Chat` state and replays it into the UI. | None. | None. It does not know about multiple conversations. | None. |
| `chat_append()`, `chat_append_message()`, `chat_clear()`, `update_chat_user_input()`, greeting functions | `R/chat.R`; help topics of the same names | Write-only. `chat_get_greeting()` reads the greeting, not messages. | None. | None. | None. |
| `contents_shinychat()`, `tool_result_display()` | `R/contents_shinychat.R`; `?contents_shinychat` | Renders ellmer content for display. It reads nothing back from a session. | Controls how a tool result looks. | None. | None. |
| Documented inputs: `input$<id>_user_input`, `input$<id>_greeting_dismissed`, `input$<id>_page` | `NEWS.md` (0.5.0) | The latest submission only. | None. | None. | None. |

The conversation record is the only place branch state exists, and it is not
reachable through a supported accessor:

- `ConversationStore$get(partition, id)` and `$list(partition)` need a
  partition. `conversation_partition()` is not exported, and the source says
  why: custom stores "only ever receive a partition from the framework" and
  "never need to construct one" (`R/chat_history_store.R:1-15`). In the
  installed package, `FileConversationStore$new(dir)$list()` with a plain
  list aborts with "`partition` must be a shinychat conversation partition."
- A host can see a record only by being the storage backend: a
  `ConversationStore` subclass receives each record in `put()`. That is a
  write-side persistence contract used during the run. It is not a post-run
  read of a completed session, and it fires on every save, title change, and
  branch navigation, with nothing marking a record as final.
- The documented record shape stops at the top level. `?ConversationStore`
  lists `schema_version`, `id`, `title`, `title_source`, `response_count`,
  `created_at`, `updated_at`, `client_info`, `nodes`, `current_leaf`,
  `values`, and `bookmark_state_id`, and describes `nodes` only as "a named
  list of turn nodes forming the conversation tree". The node fields
  (`parent`, `children`, `turns`, `ui`, `selected_child`) and the stored UI
  message format are undocumented.
- The shape is about to change. Upstream's design note says the Python
  package already uses record schema version 2 (an exchange tree) and "R has
  not yet been ported" (`memory-bank/exchange-tree-history.md:5`). The Python
  changelog for that rewrite says apps with custom stores "must support the
  new record format". R is still at schema version 1
  (`R/chat_history_types.R:60-61`).
- `FileConversationStore` writes `record.json`, `turns.jsonl`, and
  `ui.jsonl`. That layout is undocumented, and reading it would be filesystem
  I/O against a private format.

Two facts narrow what a snapshot would add in R today:

- Displayed messages in the record are derived on the server from the ellmer
  turns, through `contents_shinychat()`
  (`derive_stored_ui_messages()`, `R/chat_history_types.R:284`), and are
  re-derived from the turns when missing. For the active path they are a
  rendering of the same turns the `Chat` holds, not independent evidence of
  what the browser showed. Tool activity is likewise a rendering of the tool
  requests and results in those turns. Messages an app adds with
  `chat_append()` are not recorded.
- After an edit, a branch switch, or a conversation switch, shinychat resets
  the client to the turns on the active path
  (`R/chat_history.R:235`, `:500-541`; `R/chat_history_client.R:5`). A `Chat`
  read after the session therefore never contains turns the user edited away
  or other conversations from the history drawer.

So the evidence a public snapshot would add beyond the `Chat` is: inactive
branches, the other saved conversations, the conversation ID and title,
record timestamps, attachment metadata, and the application `values`.

What changed since 2026-08-22 (0.4.0.9000, `c6121f1`):

- `chat_server()$history$conversation_id()` is new. It gives a stable
  conversation ID and hands it to the ellmer client. The earlier audit had no
  public conversation identity.
- `history$title()` is new on `main` and not in 0.5.0.
- `page_chat()`, `chat_ui_history()`, the drawer functions, and `new_chat()`
  are new. None reads conversation content.
- Editing a sent message now forks the conversation in the UI (0.5.0 NEWS,
  #269), so inactive branches are now something users create routinely.
- Still true: the record is reachable only through a store, and the partition
  constructor is not exported.

What would unblock a presentation supplement. Any one of the first two, plus
the rest:

1. An exported accessor for the active or a named conversation that needs no
   partition, for example a `history$record()` on the `chat_server()` return
   value, or an exported reader that takes a store, chat ID, and scope.
2. Or an exported `conversation_partition()` constructor, with `get()`
   documented as a supported read path.
3. A documented node schema: node ID, parent, children, which child is
   active, the recorded ellmer turns, and the displayed messages, with a
   schema version that upstream commits to. The schema 2 port to R is the
   natural point.
4. A way to tell a settled record from one written mid-run, such as a
   terminal status per exchange.
5. An identifier that ties each displayed message to the turns it renders,
   so the join to ellmer turns does not rest on position.

The ellmer path was checked against the audited version. With ellmer 0.5.2
and shinychat 0.5.0 installed, a `Chat` whose turns went through
`ellmer::contents_record()` and `ellmer::contents_replay()`, which is what
shinychat does on save and restore, converts with `as_trajectory()` to the
same events as the original turns, with no losses. The titling prompt at the
pin and on `main` (`R/chat_history_title.R:1`) still begins with the text
`as_trajectory_otel()` uses to set title calls aside. One gap in scans, not
in shinychat: `as_trajectory_ellmer()` does not retain `Chat$conversation_id`,
so a trajectory read from the client cannot yet be joined to the OTel
conversation or a future shinychat record by that ID.

The integration path is unchanged:

1. Continue using `as_trajectory_ellmer()` for the semantic client snapshot,
   or `as_trajectory_otel()` for recorded spans.
2. Ask shinychat to expose a public immutable conversation-record snapshot,
   as listed above.
3. Once available, add inactive branches and presentation state as a
   supplement, without changing the active semantic path.

#### Findings of 2026-08-22

Kept as history. These describe shinychat 0.4.0.9000 at `c6121f1`.

The public `ConversationStore` interface documents the useful settled record:
conversation identity, a node graph, a current leaf, serialized ellmer turns,
rendered UI, timestamps, client information, and application values. However,
standard callers cannot construct the non-exported partition required by
`ConversationStore$get()`, and `chat_server()` exposes only live reactive state
plus the semantic ellmer client.

scans must not inspect the internal `HistoryController`, synthesize a
partition, or substitute rendered messages for model-visible turns.

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

#### Delegation views, jobs, and approvals (deputy 0.0.0.9007, #66)

`LeadAgent$export_subagents()` returns a plain list (`schema_version = 1L`,
`settled`, `scope`, `children`) whose children hold ellmer content records,
not turns. `delegation_history()` checks a `DelegationDisclosure` again and
replays them as ellmer `turns`, marking every reference `"unresolved"`.
`as_trajectory_deputy()` therefore accepts the views that
`delegation_history()` or `$inspect_subagents(transcript = TRUE)` return, on
their own or through `delegations =` beside the lead `AgentResult`, and
rejects a raw export rather than replaying it without that check. Neither
value has a class, so `as_trajectory()` does not dispatch on them.

Parents resolve from each view's `outcome$runtime`: `parent_run_id` names the
delegating run, and `parent_delegation_id` names the parent delegation of a
nested child. Views carry no timestamps or lifecycle events. A withheld
transcript is a `capture$transcript` loss, and a tool result that holds only
a `tool_result_reference()` preview is an externalized `contents$value` loss.

`AgentJob` (`job_read()`) and `ApprovalContinuation` (`approval_read()`) stay
deferred. A job's `result` is a summary of its `AgentResult` without turns or
events, and its status can still be running, so a trajectory from it would
have no semantic record to review. An approval continuation is a paused run,
not a completed one; once resumed, the run's `AgentResult` already converts.
A job adapter becomes worthwhile when Deputy persists a public, settled turn
record for the job (as the delegation transcript does), and an approval one
when an investigation needs pending tool calls as review evidence.

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
   The 2026-11-26 refresh found none; the shinychat section lists what would
   unblock it.

The first four adapters remain optional. Each gets a focused child issue and
provider-free conformance fixtures before implementation.
