# scans

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml)
[![format-check](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml)
[![jarl-check](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml)
<!-- badges: end -->

Understand how your R agent reached its result—and where it went wrong.

An evaluation score tells you whether an agent produced the right outcome.
{scans} helps explain the path that produced it. It turns completed agent runs
into evidence-linked trajectories that you can search, compare, scan for common
failure patterns, and inspect without calling the model or its tools again.

Use {scans} when a pass/fail score is not enough: a tool failed somewhere in a
long run, an agent repeated the same request, token or cost usage jumped, or a
reviewer needs to see the exact events behind a finding.

## From outcome to explanation

| Question | What {scans} provides |
|---|---|
| Which runs need attention? | One-row summaries of failures, tool activity, execution depth, usage, cost, and recorded data loss. |
| What looks wrong? | Deterministic findings for repeated and unresolved tool calls, suspicious loops, failed events, and causal error chains. |
| What exactly happened? | A read-only Shiny app that links each finding and evaluation to the relevant transcript events. |
| Can I do my own analysis? | Ordinary tibbles for trajectories, turns, events, evaluations, and adapter losses. |
| Can I compare different agent systems? | Optional adapters that normalize their completed runs into the same validated structure. |

[{vitals}](https://vitals.tidyverse.org/) answers, "Did the agent succeed?"
{scans} answers, "What did it do, what deserves attention, and where is the
evidence?" Use them together to connect an evaluation outcome to the behavior
that produced it.

## Installation

Install the development version from GitHub:

```r
pak::pak("JamesHWade/scans")
```

## A thirty-second tour

After an ellmer chat has run, snapshot it and work entirely from the completed
record:

```r
library(scans)

bundle <- as_trajectory(chat)

summarize_trajectories(bundle)

findings <- scan_trajectories(bundle)
findings[, c("severity", "label", "explanation", "event_ids")]

scans_app(bundle)
```

Every finding carries the trajectory and event identifiers that support it.
Open the same bundle in `scans_app()` to move from a fleet-level summary to the
relevant turn, tool call, result, error, evaluation, or adapter loss.

Already evaluating with vitals? Read the persisted log and keep the outcomes
and execution evidence joined by `trajectory_id`:

```r
samples <- vitals::vitals_log_read(log_path)
bundle <- as_trajectory_vitals(samples, source_uri = log_path)
```

{scans} is post-run and read-only. It does not execute an agent, replay tools,
or score answer quality.

## Bring completed runs from your existing stack

{scans} uses tidy, rectangular trajectory data and small, composable analysis
functions. Current optional adapters support
[{ellmer}](https://ellmer.tidyverse.org/),
[{vitals}](https://vitals.tidyverse.org/),
[{deputy}](https://github.com/JamesHWade/deputy),
[{dsprrr}](https://github.com/JamesHWade/dsprrr),
[{commons}](https://github.com/posit-dev/commons),
and [{tempest}](https://github.com/JamesHWade/tempest). The core analysis layer
does not require those packages, and `TrajectoryBundle()` accepts already
rectangular data from other agent frameworks.

The package is in early development. Its shared trajectory contract is a
validated S7 object, while its analysis properties remain ordinary tibbles.
{scans} is independently maintained. It borrows development conventions and
interface principles from tidyverse packages, but it is not part of the
tidyverse organization. Its design is inspired by Meridian Labs'
[inspect_scout](https://meridianlabs-ai.github.io/inspect_scout/).

## Start with an ellmer trajectory

Completed ellmer chats and public turn objects can be snapshotted and inspected
without a provider call. Here the agent repeats a tool request after receiving
a result:

```r
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
  ellmer::AssistantTurn(list(first_request)),
  ellmer::UserTurn(list(ellmer::ContentToolResult(
    "Cloudy",
    request = first_request
  ))),
  ellmer::AssistantTurn(list(second_request))
)

bundle <- as_trajectory(turns)

bundle |>
  summarize_trajectories()

bundle |>
  scan_trajectories()
```

The scan returns evidence-linked findings for both the repeated request and its
missing result. It never calls the model or tool.

## Connect vitals outcomes

Evaluated vitals tasks can be converted through their public sample interface:

```r
bundle <- as_trajectory_vitals(task)

outcomes <- trajectory_evaluations(bundle)
diagnostics <- scan_trajectories(bundle)
```

The development version of vitals can also reconstruct persisted eval logs.
Its output uses the same adapter:

```r
samples <- vitals::vitals_log_read(log_path)
bundle <- as_trajectory_vitals(samples, source_uri = log_path)
```

Each sample and epoch receives a stable trajectory identity within the bundle,
so outcomes, summaries, and evidence-linked findings join by `trajectory_id`.
The adapter snapshots the solver chat and never reaches into private vitals
state.

## Inspect Deputy execution

A completed Deputy result adds run lifecycle, tool timing, usage, immutable run
context, and delegation identity to its model-visible ellmer turns:

```r
bundle <- as_trajectory(agent_result)

trajectory_info(bundle)
trajectory_events(bundle)
```

Passing a list of results preserves each run as a separate trajectory. A child
is linked through `parent_trajectory_id` only when its parent run is included
in that same snapshot:

```r
bundle <- as_trajectory_deputy(list(parent_result, child_result))

trajectory_info(bundle)[
  , c("trajectory_id", "run_id", "parent_trajectory_id")
]
```

Unresolved parent IDs and missing run IDs remain explicit adapter losses; the
adapter does not invent related executions.

## Inspect dsprrr traces

A dsprrr module can be converted after execution through its public trace
exporter. Already-exported trace tibbles use the explicit adapter because they
do not carry a package-specific class:

```r
bundle <- as_trajectory(program)

traces <- dsprrr::export_traces(
  program,
  include_prompts = TRUE,
  include_outputs = TRUE
)
bundle <- as_trajectory_dsprrr(traces)
```

Each trace becomes a trajectory. The adapter reuses the ellmer conversion for
model-visible turns, keeps token, cost, latency, model, output, and trace
context data, and uses the program artifact digest as its source identity. The
digest establishes integrity, not authenticity. A caller-supplied
`deputy_run_id` may correlate the trace to a Deputy run; Tempest product
identifiers remain metadata rather than ownership claims.

## Inspect commons provenance

commons reconstructs conversations and their OpenTelemetry provenance. Read
the trajectory through commons, then pass that settled value to the explicit
adapter:

```r
conversations <- commons::trajectory_read("path/to/otel-traces")
bundle <- as_trajectory_commons(conversations)

trajectory_info(bundle)
bundle |>
  filter_trajectory_events(event_type = "commons:provenance")
```

Each named conversation becomes a trajectory. Model-visible turns reuse the
ellmer adapter, while `last_active`, the local or Connect source descriptor,
trust tags, and citation decisions remain inspectable. Missing or conflicting
provenance produces an explicit loss; scans does not infer a stronger trust
claim. The input has no package-specific class, so `as_trajectory_commons()`
is intentionally explicit and performs no filesystem or Connect I/O.

## Inspect Tempest product reviews

Tempest exposes a bounded, deterministic review of a completed STORM or
Co-STORM product. Create that review with Tempest, then pass the closed value to
the explicit adapter. The adapter uses Tempest's authoritative review
projection instead of reimplementing its source invariants:

```r
review <- tempest::tempest_trajectory_review(result)
bundle <- as_trajectory_tempest(review)

bundle |>
  filter_trajectory_events(event_type = "tempest:join")
```

The product becomes one trajectory. Stage order remains authoritative, while
agent runs, programs, knowledge, evidence identities, joins, and findings keep
their deterministic source order. Join values preserve
`authority_validated`, `exact_identity`, and `correlation_only` proof kinds.
Prompts, responses, source content, paths, credentials, and live objects remain
excluded and are recorded as losses; scans never reaches back into the live
Tempest result.

## Compose trajectory diagnostics

Core diagnostics return ordinary tibbles and compose with the base pipe:

```r
bundle |>
  filter_trajectory_events(role = "assistant")

bundle |>
  summarize_trajectories()

bundle |>
  scan_trajectories()
```

Built-in scans identify unresolved or unmatched tool activity, repeated calls,
ambiguous correlation, suspicious tool loops, failed events, and causal error
chains. Every finding retains the trajectory and event identifiers that support
it.

## Explore trajectories with the scans app

The scans app turns the same detached bundle into a read-only Shiny app. Use its
left sidebar to search and filter trajectories, browse canonical turns and
events in the main canvas, and inspect linked findings, evaluations, adapter
losses, and source context in the evidence rail.

```r
scans_app(bundle)
```

Pass a named list to review more than one application. Entries may be detached
bundles or zero-argument loader functions. Loaders run only when their
application is first selected in a browser session, and the **Reload traces**
button fetches that application again.

```r
scans_app(list(
  "Support assistant" = support_bundle,
  "Research assistant" = function() load_research_bundle()
))
```

The scans app computes the built-in deterministic scans when each snapshot is
first loaded. It does not call a model, run tools, modify a bundle, or infer
missing source facts. The app depends on the optional {shiny}, {bslib}, and
{htmltools} packages.

### Deploy a multi-application reviewer to Posit Connect

When [content observability](https://docs.posit.co/connect/user/content-settings/#content-observability)
is enabled, Connect retains the OpenTelemetry traces emitted by compatible R
libraries. `{commons}` already reconstructs its logged `ellmer` conversations
from that store, so the review app can read the native traces directly. Use a
named allow-list of content GUIDs in `app.R`:

```r
library(scans)

scans_app_connect(c(
  "Support assistant" = "11111111-1111-4111-8111-111111111111",
  "Research assistant" = "22222222-2222-4222-8222-222222222222"
))
```

Each application is fetched only when selected, cached for that browser
session, and fetched again by **Reload traces**. The default keeps the 100 most
recent conversations from the seven-day window ending at each load; use `n`,
`from`, and `to` to choose another window. Supplying a bound as `NULL`
explicitly leaves that side open. `{commons}` isolates Connect's current
content-wide trace endpoint and its legacy per-job fallback; scans requires the
current trajectory reader instead of duplicating that version-sensitive
transport.

The Connect administrator must enable OpenTelemetry and allow content
instrumentation. Turn on **Monitoring > Content Observability** for every source
application, redeploy with compatible versions of
[ellmer and otelsdk](https://docs.posit.co/connect/user/traces/compatible-libraries.html),
and run the commons agent with trajectory logging enabled. The reviewer uses
Connect's injected `CONNECT_SERVER` and ephemeral owner `CONNECT_API_KEY`; its
owner must own or collaborate on every configured source application.

Only content GUIDs are accepted. All sources therefore stay on the hosting
`CONNECT_SERVER`; a content URL cannot redirect the injected API key to another
server.

That key acts as the deployment owner, not the person currently viewing the
review app. Restrict the reviewer app to people permitted to inspect every
configured application's potentially sensitive prompts, responses, tool
arguments, and results. Pins and custom lazy loaders remain useful as explicit
offline import/export paths, but they are no longer the primary Connect
transport.

## Construct a bundle directly

Adapters normalize other sources into the same validated relational boundary.
`TrajectoryBundle()` is also available when data is already rectangular. Empty
semantic tables are valid, so incomplete or interrupted paths do not require
invented events.

```r
bundle <- TrajectoryBundle(
  data.frame(
    trajectory_id = "trajectory-1",
    source_type = "manual"
  ),
  data.frame(),
  data.frame()
)

trajectory_info(bundle)
```

## Development

The repository uses [Air](https://posit-dev.github.io/air/) for formatting and
[Jarl](https://jarl.etiennebacher.com/) for linting.

```sh
air format .
jarl check .
Rscript -e 'devtools::test()'
Rscript -e 'devtools::check()'
```

## Code of conduct

Please note that the scans project is released with a
[Contributor Code of Conduct](https://github.com/JamesHWade/scans/blob/main/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
