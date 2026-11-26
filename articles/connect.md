# Reviewing Posit Connect traces

Posit Connect can retain the OpenTelemetry traces that deployed
applications emit. ellmer follows the GenAI semantic conventions, so a
Connect deployment that uses ellmer leaves a complete record of every
model call, its message history, and each tool execution. scans reads
that record directly and turns it into trajectories you can scan and
review, with no changes to the producing application beyond enabling
capture.

## Prerequisites on the producing application

1.  **Server permission.** A Connect administrator enables OpenTelemetry
    and allows content instrumentation:

    ``` ini
    [OpenTelemetry]
    Enabled = true
    AllowContentInstrumentation = true
    ```

2.  **Content observability** is turned on for each application under
    **Settings \> Monitoring**, and the application is restarted
    afterwards.

3.  **Message content is captured.** ellmer records prompts and
    responses in its spans only when the process runs with
    `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true`. Set it as
    a Connect Var. Without it the reader finds spans but has no messages
    to reconstruct.

4.  **Conversations are identified.** Spans are grouped by the
    `gen_ai.conversation.id` attribute, found by walking up each span’s
    ancestry. commons agents set it; other ellmer apps should set the
    chat’s conversation identifier or open a parent span carrying the
    attribute. Without it, each model call becomes its own one-turn
    trajectory.

Connect retains captured message content. Reading traces copies that
content into the R process running scans, which may be on a local
computer or Connect. The deployed review app reads with its deployment
owner’s credentials.

## Read traces from R

[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
needs `CONNECT_SERVER` and `CONNECT_API_KEY`, and the key’s user must
own or collaborate on the content. It accepts a content GUID, a content
URL, or a dashboard URL.

``` r

library(scans)

conversations <- read_connect_traces(
  "11111111-1111-4111-8111-111111111111",
  n = 200,
  from = Sys.Date() - 30
)
attr(conversations, "read_info")

bundle <- as_trajectory_otel(conversations)
summarize_trajectories(bundle)
scan_trajectories(bundle)
```

### Windows and limits

- `from` and `to` select conversations: a conversation is kept when one
  of its model calls started in `[from, to)`. A kept conversation then
  retains its earlier spans as far back as the read reached, which is an
  hour before `from`, so token totals, tool failures, and the start time
  describe the conversation, not just the window. Spans at or after `to`
  are dropped. Widen `from` if you need the timing and usage of calls
  made earlier than that hour. When both are omitted the reader takes
  the seven days ending now. Pass `NULL` explicitly to leave one side
  open. Bounds may be POSIXct, Date, or ISO 8601 strings; a string
  without a zone is read as UTC.
- `n` keeps the most recent conversations; `NULL` keeps all of them.
- `max_spans` caps how many GenAI spans are read. Only spans with
  `gen_ai.*` attributes count, so a Shiny application’s reactive spans
  do not exhaust the budget before the conversations are reached.
- `jobs` controls whether retained per-job trace stores are read as
  well. Connect did not migrate traces recorded before its content-wide
  store, so the default reads both and de-duplicates. `jobs = FALSE`
  skips per-job requests. Use it after verifying that the content-wide
  store holds the records needed for the investigation.

The `read_info` attribute records the window, how many spans and
conversations were found, whether the span ceiling or `n` left
conversations out, and whether a page failed to load. The review app
shows the same information so a reviewer can tell a quiet week from a
truncated or incomplete read.

### How a conversation is reconstructed

scans reconstructs messages from the most recent `chat` span of a
conversation. The transcript is limited to the history captured in that
span; disabled message capture leaves content unavailable.
`execute_tool` spans contribute recorded tool duration and failure.
Non-GenAI span and resource attributes such as `enduser.id` or a session
identifier are kept in trajectory metadata and surface in the app as
badges and search terms.

shinychat titles each conversation with a separate model call under the
same conversation identifier, and that call often finishes last. It is
never used as the transcript. Its tokens stay in the conversation’s
usage totals and are reported separately in `metadata$otel$title_calls`.

ellmer leaves out a call’s message history when the turn holds content
it cannot serialize. scans then rebuilds the history from the latest
earlier call that recorded one, adds each later call’s reply, and
records a `capture$input` or `capture$tool_result` loss for the messages
no span kept.
[`assess_trajectory_scans()`](https://jameshwade.github.io/scans/reference/assess_trajectory_scans.md)
reports scans that depend on those messages as having insufficient
evidence.

## Read local trace files

Outside Connect, the same spans can be written to local OTLP NDJSON
files by otelsdk’s file exporter.
[`read_otel_traces()`](https://jameshwade.github.io/scans/reference/read_otel_traces.md)
reads them and returns conversations in the same form as
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md),
with the same `n`, `from`, `to`, and `max_spans` arguments. It reads
every file unless you give a window.

A commons agent logs locally when tracing goes to a file:

``` r

Sys.setenv(
  OTEL_TRACES_EXPORTER = "otlp/file",
  OTEL_EXPORTER_OTLP_TRACES_FILE = "traces/trace-%N.jsonl",
  OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "true"
)
# Set these before ellmer and commons load, then:
agent <- commons::agent(client, commons, log = TRUE)
```

A deputy agent, or any ellmer application, is configured the same way:
set the exporter variables before ellmer loads, and deputy’s
`deputy.run` spans land in the same files as ellmer’s model calls.

Pass the directory, one or more files, or nothing when
`OTEL_EXPORTER_OTLP_TRACES_FILE` is set in the session doing the
reading:

``` r

conversations <- read_otel_traces("traces")
bundle <- as_trajectory_otel(conversations)
```

The exporter rotates through `trace-0.jsonl`, `trace-1.jsonl`, and so
on, and keeps `trace-latest.jsonl` pointing at the newest file. Each
span is read once, whether that alias is a link or a copy. The
`read_info` attribute also records how many files were read and how many
lines could not be parsed; a file being written while you read can end
in a partial line.

## Deploy the review app

[`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
builds a review app whose application switcher reads from Connect. The
repository’s `deploy/connect/` directory contains a ready `app.R`
configured through Connect Vars, or call the function from your own:

``` r

library(scans)

scans_app_connect(
  c(
    "Support assistant" = "11111111-1111-4111-8111-111111111111",
    "Research assistant" = "22222222-2222-4222-8222-222222222222"
  ),
  n = 100,
  jobs = FALSE
)
```

Only content GUIDs are accepted, so every source stays on the hosting
`CONNECT_SERVER` and a content URL cannot redirect the injected API key
elsewhere. Each application is read when first selected in any session.
The snapshot is then shared by every session of the deployment until it
is thirty minutes old or a reviewer presses **Reload traces**.

The app depends on shiny, bslib, htmltools, httr2, and jsonlite. Attach
them in `app.R` (as `deploy/connect/app.R` does) so Posit Publisher or
rsconnect bundles them.

### Review the conversations

The app opens on an overview of the conversations in view: counts with
their denominators, activity by day, patterns, tools, models, and users.
Selecting a bar, pattern, tool, user, or model narrows the overview and
the list together, and each active filter is a removable chip.

With an annotation store (next section) the list becomes a review queue.
It orders conversations by “Needs review”: unreviewed first, then those
with error findings, warnings, nothing a scan could assess, and no
findings, newest first. Open a conversation and mark it with one of the
store’s labels by button or key (`1`-`9`), or press `E` to mark it
reviewed without a label; the next unreviewed conversation opens. `U`
jumps to the next unreviewed conversation and `?` lists the shortcuts.

The Export menu on the overview gives the conversations and findings in
view as CSV, a Markdown report for the application’s developers, and an
R snippet that repeats the read with the same content GUID, window, and
scan settings. The report carries counts and links only. It has no
transcript text, and user identifiers only when you ask for them. Links
reopen the same filters. No export contains the Connect server address
or API key.

Deputy runs show their recorded ending and run interventions, and
commons agents show answer provenance and recurring exploratory routes;
see the sections on each below.

### Annotations on Connect

Pass a
[`scans_annotations()`](https://jameshwade.github.io/scans/reference/scans_annotations.md)
store to record reviewer labels and notes. Point it at a file on storage
that survives redeployment and that every process of the app can write
to, and run the app as a single process so all reviewers append to one
log:

``` r

store <- scans_annotations("/data/scans-reviews/annotations.jsonl")
scans_app_connect(sources, annotations = store)
```

The bundled `app.R` reads `SCANS_ANNOTATIONS_DIR` for this, and
`SCANS_ANNOTATION_LABELS` for the labels reviewers can apply. Its other
Vars set the window (`SCANS_CONNECT_DAYS`), the reader
(`SCANS_CONNECT_READER`), whether per-job stores are read
(`SCANS_CONNECT_JOBS`), and the optional `SCANS_REVIEWS` and
`SCANS_INVESTIGATIONS` controls; a value it does not understand stops
the app with a message naming the Var. Without a store the app makes no
writes and shows no annotation controls.

## Security model

- **The app reads as its owner.** Connect injects an ephemeral API key
  for the deployment owner. Every viewer of the app therefore sees every
  configured application’s conversations, including prompts, responses,
  tool arguments, and results. Restrict the app’s viewers accordingly.
- **Failures are not echoed.** A source that fails to load is reported
  in the app as a failure with a fixed message; the underlying error,
  which may include hostnames or paths, goes to the server log.
- **Transcripts are sanitized.** Model-authored HTML is rebuilt against
  an element and attribute allowlist. Markup that cannot be shown safely
  appears as escaped source so the reviewer can still see what the model
  emitted.
- **Secrets are redacted before they reach a bundle.** Adapters redact
  sensitive named fields in tool arguments, results, and metadata. The
  transcript bounds every payload so an oversized result cannot stall
  the browser.
- **Nothing is written without a store.** The app never modifies a
  bundle. With an annotation store it appends records and nothing else.

## Agents built with commons

commons agents add their own spans to each conversation: a turn wrapper
that records the Connect user, and a provenance record of each answer’s
tag and citation decisions.
[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
keeps both. Each exchange becomes a `commons:provenance` event, in the
same form
[`as_trajectory_commons()`](https://jameshwade.github.io/scans/reference/as_trajectory_commons.md)
gives, with the full citation decisions and the time the record was
made, and the user is kept with the trajectory’s metadata. The same read
also keeps every model and tool call’s timing and usage, which the
commons reader does not return, so
[`measure_trajectories()`](https://jameshwade.github.io/scans/reference/measure_trajectories.md)
and answer-time trends work on commons conversations. A record that
conflicts with another for the same exchange, as after an edited or
restored session, leaves the exchange unclassified with a loss rather
than guessing.

Read through commons instead when you want the transcript exactly as
commons rebuilds it, including its placeholder turns, or its own
resolution of vanity URLs and deployment records. Convert that result
with
[`as_trajectory_commons()`](https://jameshwade.github.io/scans/reference/as_trajectory_commons.md),
or pass `reader = "commons"` to
[`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md):

``` r

conversations <- commons::trajectory_read("path-or-guid")
bundle <- as_trajectory_commons(conversations)

scans_app_connect(sources, reader = "commons")
```

## Agents built with Deputy

Deputy adds a `deputy.run` span around each agent run, with the run’s
identity, how it ended, and its governance decisions as span events.
[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
keeps these too. Each trajectory carries the run ID, session, agent, and
delegation in metadata, with the same ten-field `ending` that
[`as_trajectory_deputy()`](https://jameshwade.github.io/scans/reference/as_trajectory_deputy.md)
records, as far as the span says it: whether the answer was cut off, the
finish reason, and the stop reason. Permission decisions, fallbacks,
compactions, hooks, checkpoints, warnings, and structured attempts
become `deputy:*` events with their times. Deputy files a subagent’s run
under its lead’s conversation, and scans gives that run its own
trajectory, linked to the lead through `parent_trajectory_id`, so the
two transcripts never replace each other.

The span leaves out what Deputy keeps private: prompts, tool inputs and
results, error messages, approvals waiting for a decision, and failed
compactions. Convert the `AgentResult` with
[`as_trajectory_deputy()`](https://jameshwade.github.io/scans/reference/as_trajectory_deputy.md)
when you have it and need those.
