# scans <img src="man/figures/logo.png" align="right" height="139" alt="Scans hex sticker: a raccoon inspecting a trail of pawprints with a magnifying glass." />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml)
[![format-check](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml)
[![jarl-check](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml)
<!-- badges: end -->

Inspect completed R agent and chat runs to find failed tools, repeated
requests, and changes in recorded resource use. scans imports the captured
record, returns summaries and diagnostic findings as tibbles, and opens the
same evidence in a Shiny review app. It does not call the model or tools again.

Use scans to investigate how a run unfolded. Use outcome evaluations from
vitals to assess the result. Findings identify behavior to review; an empty
findings table does not establish that a run succeeded or was fully captured.

scans is experimental. Its trajectory contract is stable enough to build on,
but function signatures and finding vocabularies may still change.

## Installation

```r
pak::pak("JamesHWade/scans")
```

The core analysis layer depends on cli, rlang, S7, tibble, and vctrs. Adapters
and the review app use optional packages that are loaded when you need them.

## Try the offline example

Start without a provider account using the bundled support-assistant example:

```r
library(scans)
path <- system.file("extdata", "support-investigation.json", package = "scans")
bundle <- as_trajectory_otel(jsonlite::read_json(path))
scan_trajectories(bundle)
measure_trajectories(bundle)
scans_app(list("Support assistant example" = bundle))
```

The [offline investigation](https://jameshwade.github.io/scans/articles/investigate.html)
walks from three failed order lookups to their exact events, separates
recorded work from conversation elapsed time, and makes incomplete capture
visible. It includes optional-package installation instructions and a capture
table for bringing your own evidence.

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

`summarize_trajectories()` gives one row per trajectory: turns, tool calls,
unresolved calls, failures, token usage, and duration. `scan_trajectories()`
runs the deterministic detectors listed by `scan_registry()` and returns one
finding per detected pattern. Findings identify their supporting trajectory,
turn, or events. `scans_app()` opens the same bundle and links findings to
that evidence.

The following runnable example builds a trajectory in which the agent repeats a
tool request after receiving its result:

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
scan_trajectories(bundle)
```

The scan reports both the repeated request and its missing result.

## Review deployed apps on Posit Connect

When Posit Connect's content observability is enabled, it retains the
OpenTelemetry traces that ellmer emits. scans reads those traces directly and
rebuilds each conversation, including per-call tool timing and failures:

```r
conversations <- read_connect_traces("11111111-1111-4111-8111-111111111111")
bundle <- as_trajectory_otel(conversations)
```

To review several deployed applications from one place, deploy the app in
`deploy/connect/` or call `scans_app_connect()` from your own `app.R` with a
named allow-list of content GUIDs:

```r
scans_app_connect(c(
  "Support assistant" = "11111111-1111-4111-8111-111111111111",
  "Research assistant" = "22222222-2222-4222-8222-222222222222"
))
```

Each application is read when first selected, shared by every session of the
deployment, and refreshed after thirty minutes or with **Reload traces**. The
producing apps must run with message capture enabled and set a conversation
identifier; the review app reads with its deployment owner's Connect API key,
so restrict its viewers to people allowed to see every configured
conversation. `vignette("connect")` walks through the setup, the reader's
window and span limits, and the security model.

## Where runs come from

Adapters normalize completed runs from different frameworks into the same
validated `TrajectoryBundle`. `as_trajectory()` dispatches on classed inputs;
the explicit functions accept unclassed data.

| Source | Function | What it preserves |
|---|---|---|
| ellmer chats and turns | `as_trajectory()`, `as_trajectory_ellmer()` | Messages, tool calls and results, token usage, redacted sensitive fields |
| OpenTelemetry GenAI spans (Posit Connect) | `read_connect_traces()`, `as_trajectory_otel()` | Whole conversations, tool span timing and failures, user and session attributes |
| vitals evaluations | `as_trajectory_vitals()` | Solver trajectories joined to scores by `trajectory_id` |
| Deputy agent results | `as_trajectory_deputy()` | Run lifecycle, tool timing, delegation between runs |
| dsprrr modules and traces | `as_trajectory_dsprrr()` | Semantic turns, metrics, program artifact identity |
| commons conversations | `as_trajectory_commons()` | Provenance tags and citation decisions |
| Tempest product reviews | `as_trajectory_tempest()` | Stage order, agent runs, evidence joins, findings |
| Your own rectangular data | `TrajectoryBundle()` | Whatever you supply; empty tables are valid |

Every adapter records what it could not carry over as an explicit loss in
`trajectory_losses()` rather than inventing events. `vignette("adapters")`
describes each adapter's contract and how to build one for another framework.

## The reviewer workflow

- **Understand the application** in the overview: see recurring finding
  patterns, median and 95th-percentile elapsed time, recorded token usage,
  and evidence coverage for the trajectories matching your browser filters.
  Select a pattern to narrow the list, or open a ranked trajectory to inspect
  its evidence. Unknown measurements stay visible as missing; conversation
  elapsed time may include pauses between user messages.
- **Triage** in the app's left rail: search by identifier, user, model, or
  transcript text; filter by source and status; sort by time, findings, or
  length; step through trajectories with the arrow keys.
- **Diagnose** in the transcript: turns render as chat, tool activity collapses
  into compact rows, and each finding links to the events behind it.
- **Choose the scans** that matter for your agent with the scanner panel, or in
  code with `scan_trajectories(bundle, scans = c("event_error", "error_chain"))`.
- **Record judgments** by passing a `scans_annotations()` store to the app.
  Labels and notes append to a JSON-lines log that never rewrites earlier
  records. Earlier judgments remain available to your own analysis. See the
  [Connect guide](https://jameshwade.github.io/scans/articles/connect.html)
  for storage and single-process deployment requirements.

Core diagnostics return ordinary tibbles and compose with the base pipe:

```r
bundle |>
  filter_trajectory_events(role = "assistant", event_type = "tool_call")

bundle |>
  scan_trajectories(scans = "repeated_tool_call")
```

## Scope

scans is post-run and read-only. It does not execute an agent, replay tools,
or score answer quality. Use [vitals](https://vitals.tidyverse.org/) to ask
"Did the agent succeed?" and scans to ask "What did it do, what deserves
attention, and where is the evidence?"

Its design is inspired by Meridian Labs'
[inspect_scout](https://meridianlabs-ai.github.io/inspect_scout/). scans is
independently maintained and borrows conventions from tidyverse packages
without being part of the tidyverse.

## Getting help and contributing

See [SUPPORT.md](https://github.com/JamesHWade/scans/blob/main/.github/SUPPORT.md)
for where to ask questions and
[CONTRIBUTING.md](https://github.com/JamesHWade/scans/blob/main/.github/CONTRIBUTING.md)
for the development workflow. Please note that scans is released with a
[Contributor Code of Conduct](https://github.com/JamesHWade/scans/blob/main/.github/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
