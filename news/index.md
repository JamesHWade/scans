# Changelog

## scans 0.0.0.9000

- A bundled, provider-free support-assistant investigation demonstrates
  repeated failures, overlapping tools, conversation pauses, and
  incomplete capture through public functions. The new article includes
  a capture table and instructions for using the same evidence in the
  app ([\#45](https://github.com/JamesHWade/scans/issues/45)).
- [`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
  retains bounded resource aggregates across distinct captured calls,
  including coverage and a versioned adapter contract. Duplicate spans
  in pre-grouped conversations no longer inflate resources
  ([\#40](https://github.com/JamesHWade/scans/issues/40)).
- [`measure_trajectories()`](https://jameshwade.github.io/scans/reference/measure_trajectories.md)
  reports resource values with recording denominators, source units,
  attribution, and limitations while preserving
  [`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md)
  semantics. It normalizes ellmer’s separate input/cache components when
  both are recorded and identifies known cache-inclusive totals. It
  distinguishes inclusive model/tool durations, overlap-aware recorded
  work, and time outside recorded work; unsupported first-token latency
  and comparable cost remain unavailable
  ([\#40](https://github.com/JamesHWade/scans/issues/40)).
- [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
  uses the public resource measurements, shows where recorded time went,
  and exposes individual values and coverage in Resource measurements.
  Incomplete captured timestamps cannot establish conversation elapsed
  time, and partial recorded-work values are excluded from the
  overview’s timing medians
  ([\#40](https://github.com/JamesHWade/scans/issues/40)).

### Review app and Connect reader

- [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
  opens with an application overview that follows the browser filters,
  counts distinct trajectories affected by each diagnostic pattern,
  summarizes elapsed time and recorded tokens with coverage counts, and
  opens ranked trajectories in the evidence inspector. Selecting a
  pattern narrows the browser and a visible Clear control restores the
  cohort ([\#39](https://github.com/JamesHWade/scans/issues/39)).
- [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
  no longer fails when Connect omits the `X-Total-Count` header,
  converts `Date` and ISO 8601 string bounds instead of mis-scaling
  them, keeps the pages it has already read when a later page fails
  (with a warning and `read_info$incomplete = TRUE`, distinct from the
  `truncated` flag the span ceiling sets), and counts a span once even
  when it appears in two stores or on two pages. A conversation is
  selected by a model call inside the window and then keeps its earlier
  history, so a tool failure before `from` is not lost.
- [`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
  attaches tool spans that carry no conversation id to the conversation
  of their trace, which is how ellmer emits them, so tool timing and
  failures reach the transcript. A conversation’s status follows its
  latest model call, tool results link to their calls through
  `parent_event_id`, and HTTP transport attributes and URL query strings
  no longer reach trajectory metadata.
- `trajectory_sensitive_name()` matched credential fields only when they
  were lowercase, so `API_KEY`, `Authorization`, and `PASSWORD` reached
  bundles unredacted through every adapter. Fields now match regardless
  of case, and common credential shapes (`X-Api-Key`, `GITHUB_TOKEN`,
  `client_secret`) are covered. Unknown ellmer content classes have
  their `uri` scrubbed like remote images.
- The trajectory list marks annotated trajectories with their latest
  label and gains an **Annotated** filter, and the URL hash now
  identifies the selected application and trajectory so a reviewer can
  share a link straight to it.
- Tool arguments, results, and evaluation values render as themselves
  (scalars plainly, flat lists as `key: value`, nested values as JSON)
  instead of [`str()`](https://rdrr.io/r/utils/str.html) output; payload
  text is bounded at 20,000 characters with a marker; list titles drop
  markdown markers.
- Fixed three review-app bugs: the transcript sanitizer let raw-text
  elements such as `<xmp>` smuggle live markup; the cache refresh timer
  reset the selection, filters, and annotation draft every thirty
  minutes; and a failed reload in one session was shown as a cached
  failure to every other session. Annotation store failures are no
  longer echoed to the browser.
- Loading Connect traces is much faster and shows progress.
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
  fetches pages several at a time, parses each once, counts only GenAI
  spans against `max_spans`, and reads retained per-job stores only when
  they predate the content-wide store and overlap the window
  (`jobs = FALSE` skips them). The app caches each loaded snapshot for
  every session of the deployment, refreshed after thirty minutes or on
  **Reload traces**.
- The app says what a Connect read found: when it loaded, how many spans
  and conversations it read, the window it covered, and whether the
  `max_spans` ceiling or the `n` limit left conversations out. Non-GenAI
  span and resource attributes – such as `enduser.id` or a session id –
  are kept in trajectory metadata, shown in the Context panel, and the
  user surfaces as a badge and a search term.
- The trajectory browser can be ordered by time, findings, or length,
  and stepped through with the header buttons or the arrow keys; the
  selected entry is highlighted without re-rendering the list. Summary
  counts moved from value boxes into a stat strip in the workspace
  header, annotations sit beside the evidence, and the transcript has
  expand/collapse controls for tool activity.
- [`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
  and
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
  read OpenTelemetry GenAI spans from Posit Connect’s content
  observability store without going through commons. A conversation is
  reconstructed from its most recent `chat` span, which carries the
  whole message history, and `execute_tool` spans supply each call’s
  duration and failure. The reader merges Connect’s current content-wide
  store with retained per-job traces, and uses the per-job endpoint
  alone whenever the aggregate one fails.
  [`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
  gains `reader`, defaulting to `"otel"`; pass `reader = "commons"` for
  agents built with commons, whose provenance records only that reader
  recovers.
- [`scan_registry()`](https://jameshwade.github.io/scans/reference/scan_registry.md)
  lists the available detectors, and
  [`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
  gains `scans` to run a subset. Finding identifiers do not shift with
  the selection: a finding keeps its id whether or not its neighbours
  ran. The app grows a scanner panel that re-scans the loaded bundle as
  the selection changes, without refetching traces.
- [`scans_annotations()`](https://jameshwade.github.io/scans/reference/scans_annotations.md)
  creates an append-only store for reviewer labels and notes, passed to
  [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
  or
  [`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
  through `annotations`. Saving never rewrites an earlier record, so two
  reviewers cannot overwrite each other and a revised judgement does not
  erase the one before it. Each store enforces its configured label
  vocabulary while still allowing note-only records. Without a store the
  app makes no writes and shows no annotation controls.
- The transcript renders as a chat: markdown is rendered rather than
  shown as source, user turns sit in a trailing bubble, tool activity
  collapses into compact rows, and the system prompt is behind a
  disclosure. Model-authored HTML is rebuilt against an element and
  attribute allowlist; markup that carries no readable children is shown
  as escaped source rather than dropped, so a model emitting a
  `<script>` tag is visible to whoever is diagnosing it and inert in the
  page.
- An application whose traces fail to load now says so, and says why.
  The failure was previously swallowed, which rendered an empty app with
  nothing in the log and read as “this application has no
  conversations”.

### Trajectory contract and adapters

- Adapters share one trajectory status vocabulary (`completed`,
  `failed`, `interrupted`, `cancelled`), so a Deputy run that hit a
  budget limit is classified the same way whether it arrived through
  Deputy or Tempest. A dsprrr trace exported without turns is
  `completed` rather than `interrupted`; Tempest finding severities live
  in the event value instead of the event status; the ellmer adapter
  records the omitted provider JSON as `unsupported` rather than
  `redacted`.
- [`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
  gains `trajectory_error` and `turn_error`, so a run that died with a
  failed status, or a turn cut off at a token limit, produces a finding
  even when no event recorded the failure. Such findings carry no
  `event_id`. `suspicious_tool_loop` now tolerates tool results and the
  assistant’s own narration between identical calls, which is how real
  retry loops look; content from a user turn, or any other event, ends
  the run.
- [`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md)
  carries `model`, `agent`, `task_id`, `sample_id`, `epoch`,
  `started_at`, and `completed_at`, so grouped summaries no longer need
  a join back to
  [`trajectory_info()`](https://jameshwade.github.io/scans/reference/trajectory_accessors.md).
- Bundle validation, tool correlation, and row binding are linear in the
  number of events. Constructing a bundle with a 3,000-event parent
  chain took minutes and scanning 60,000 events took ten seconds; both
  now take seconds at most.
- Initial package scaffold.
- Tool results now resolve only calls that precede them in canonical
  event order, and causal mismatch findings distinguish missing
  preceding calls from missing subsequent results. Deputy lifecycle
  boundaries retain their causal position, and source-wide dsprrr and
  Commons provenance events no longer claim an unsupported turn
  association.
- Optional R6 adapters now accept valid upstream subclasses and method
  overrides through their public class contracts.
- [`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
  no longer treats an empty list as an ellmer source only because ellmer
  is installed ([\#14](https://github.com/JamesHWade/scans/issues/14)).
- [`as_trajectory_commons()`](https://jameshwade.github.io/scans/reference/as_trajectory_commons.md)
  snapshots already-read commons conversations, preserving source
  identity, activity time, trust tags, and citation decisions without
  performing filesystem or Connect I/O
  ([\#20](https://github.com/JamesHWade/scans/issues/20)).
- [`as_trajectory_deputy()`](https://jameshwade.github.io/scans/reference/as_trajectory_deputy.md)
  snapshots completed Deputy results, lifecycle events, run context,
  usage, and delegation correlation into canonical trajectories
  ([\#18](https://github.com/JamesHWade/scans/issues/18)).
- [`as_trajectory_dsprrr()`](https://jameshwade.github.io/scans/reference/as_trajectory_dsprrr.md)
  snapshots dsprrr modules and exported traces, preserving semantic
  turns, program artifact identity, metrics, and caller-supplied run
  correlation without treating artifact integrity as authenticity
  ([\#19](https://github.com/JamesHWade/scans/issues/19)).
- [`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md)
  converts completed ellmer chats and turns into canonical trajectory
  bundles without provider calls
  ([\#6](https://github.com/JamesHWade/scans/issues/6)).
- [`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md)
  now redacts sensitive named-vector elements and attributes, preserves
  sanitized properties from unknown content classes, and avoids
  ambiguous tool-result parent links
  ([\#14](https://github.com/JamesHWade/scans/issues/14)).
- [`as_trajectory_tempest()`](https://jameshwade.github.io/scans/reference/as_trajectory_tempest.md)
  converts Tempest’s authoritative closed review projection into
  product, stage, agent-run, program, knowledge, evidence, join, and
  finding events while preserving authority distinctions and explicit
  source omissions. Tempest owns review validity; scans validates only
  its generated identities and projected-record bounds
  ([\#21](https://github.com/JamesHWade/scans/issues/21)).
- [`as_trajectory_vitals()`](https://jameshwade.github.io/scans/reference/as_trajectory_vitals.md)
  snapshots evaluated vitals tasks and sample data, including the
  development `vitals_log_read()` output, into solver trajectories
  joined to canonical evaluations
  ([\#8](https://github.com/JamesHWade/scans/issues/8)).
- [`filter_trajectory_events()`](https://jameshwade.github.io/scans/reference/filter_trajectory_events.md)
  selects events by trajectory, role, type, tool, status, and metadata
  ([\#7](https://github.com/JamesHWade/scans/issues/7)). It preserves
  source columns and exposes containing-turn context in reserved
  dot-prefixed columns
  ([\#14](https://github.com/JamesHWade/scans/issues/14)).
- [`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
  emits deterministic findings for ambiguous, unresolved, unmatched,
  repeated, looping, and failed agent activity, and compares named tool
  arguments independently of key order
  ([\#7](https://github.com/JamesHWade/scans/issues/7),
  [\#14](https://github.com/JamesHWade/scans/issues/14)).
- [`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
  launches a read-only Shiny app for filtering trajectories, browsing
  canonical turns and events, and inspecting linked findings,
  evaluations, losses, and source context. Named bundle or loader lists
  add a lazy, reloadable application switcher suited to reviewing
  multiple deployments on Posit Connect. Loaders must be callable
  without arguments, evaluation cards show their canonical IDs, and
  source context includes canonical task identities plus bounded
  metadata ([\#2](https://github.com/JamesHWade/scans/issues/2)).
- [`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md)
  reports activity, failures, usage, and execution depth for each
  trajectory with grouped linear-time aggregation and an
  ambiguous-correlation count
  ([\#7](https://github.com/JamesHWade/scans/issues/7),
  [\#14](https://github.com/JamesHWade/scans/issues/14)).
- [`TrajectoryBundle()`](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md)
  provides the validated S7 trajectory contract, source-adapter generic,
  and tibble accessors
  ([\#4](https://github.com/JamesHWade/scans/issues/4)). Metadata
  entries must have unique names
  ([\#14](https://github.com/JamesHWade/scans/issues/14)).
