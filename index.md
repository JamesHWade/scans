# scans

Understand how your R agent reached its result, and where it went wrong.

An evaluation score says whether an agent produced the right outcome.
scans explains the path that produced it. It turns completed agent runs
into evidence-linked trajectories that you can summarize, scan for
common failure patterns, and inspect in a read-only review app, without
calling the model or its tools again.

Reach for scans when a pass/fail score is not enough: a tool failed
somewhere in a long run, an agent repeated the same request, token usage
jumped, or a reviewer needs to see the exact events behind a finding.

scans is experimental. Its trajectory contract is stable enough to build
on, but function signatures and finding vocabularies may still change.

## Installation

``` r

pak::pak("JamesHWade/scans")
```

The core analysis layer depends only on cli, rlang, S7, and tibble.
Adapters and the review app use optional packages that are loaded when
you need them.

## A thirty-second tour

After an ellmer chat has run, snapshot it and work entirely from the
completed record:

``` r

library(scans)

bundle <- as_trajectory(chat)

summarize_trajectories(bundle)

findings <- scan_trajectories(bundle)
findings[, c("severity", "label", "explanation", "event_ids")]

scans_app(bundle)
```

[`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md)
gives one row per trajectory: turns, tool calls, unresolved calls,
failures, token usage, and duration.
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
runs the deterministic detectors listed by
[`scan_registry()`](https://jameshwade.github.io/scans/reference/scan_registry.md)
and returns one finding per problem, each carrying the trajectory and
event identifiers that support it.
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
opens the same bundle so you can move from a finding to the transcript
event behind it.

The following runnable example builds a trajectory in which the agent
repeats a tool request after receiving its result:

``` r

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

When Posit Connect’s content observability is enabled, it retains the
OpenTelemetry traces that ellmer emits. scans reads those traces
directly and rebuilds each conversation, including per-call tool timing
and failures:

``` r

conversations <- read_connect_traces("11111111-1111-4111-8111-111111111111")
bundle <- as_trajectory_otel(conversations)
```

To review several deployed applications from one place, deploy the app
in `deploy/connect/` or call
[`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
from your own `app.R` with a named allow-list of content GUIDs:

``` r

scans_app_connect(c(
  "Support assistant" = "11111111-1111-4111-8111-111111111111",
  "Research assistant" = "22222222-2222-4222-8222-222222222222"
))
```

Each application is read when first selected, shared by every session of
the deployment, and refreshed after thirty minutes or with **Reload
traces**. The producing apps must run with message capture enabled and
set a conversation identifier; the review app reads with its deployment
owner’s Connect API key, so restrict its viewers to people allowed to
see every configured conversation.
[`vignette("connect")`](https://jameshwade.github.io/scans/articles/connect.md)
walks through the setup, the reader’s window and span limits, and the
security model.

## Where runs come from

Adapters normalize completed runs from different frameworks into the
same validated `TrajectoryBundle`.
[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
dispatches on classed inputs; the explicit functions accept unclassed
data.

| Source | Function | What it preserves |
|----|----|----|
| ellmer chats and turns | [`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md), [`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md) | Messages, tool calls and results, token usage, redacted sensitive fields |
| OpenTelemetry GenAI spans (Posit Connect) | [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md), [`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md) | Whole conversations, tool span timing and failures, user and session attributes |
| vitals evaluations | [`as_trajectory_vitals()`](https://jameshwade.github.io/scans/reference/as_trajectory_vitals.md) | Solver trajectories joined to scores by `trajectory_id` |
| Deputy agent results | [`as_trajectory_deputy()`](https://jameshwade.github.io/scans/reference/as_trajectory_deputy.md) | Run lifecycle, tool timing, delegation between runs |
| dsprrr modules and traces | [`as_trajectory_dsprrr()`](https://jameshwade.github.io/scans/reference/as_trajectory_dsprrr.md) | Semantic turns, metrics, program artifact identity |
| commons conversations | [`as_trajectory_commons()`](https://jameshwade.github.io/scans/reference/as_trajectory_commons.md) | Provenance tags and citation decisions |
| Tempest product reviews | [`as_trajectory_tempest()`](https://jameshwade.github.io/scans/reference/as_trajectory_tempest.md) | Stage order, agent runs, evidence joins, findings |
| Your own rectangular data | [`TrajectoryBundle()`](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md) | Whatever you supply; empty tables are valid |

Every adapter records what it could not carry over as an explicit loss
in
[`trajectory_losses()`](https://jameshwade.github.io/scans/reference/trajectory_accessors.md)
rather than inventing events.
[`vignette("adapters")`](https://jameshwade.github.io/scans/articles/adapters.md)
describes each adapter’s contract and how to build one for another
framework.

## The reviewer workflow

- **Triage** in the app’s left rail: search by identifier, user, model,
  or transcript text; filter by source and status; sort by time,
  findings, or length; step through trajectories with the arrow keys.
- **Diagnose** in the transcript: turns render as chat, tool activity
  collapses into compact rows, and each finding links to the events
  behind it.
- **Choose the scans** that matter for your agent with the scanner
  panel, or in code with
  `scan_trajectories(bundle, scans = c("event_error", "error_chain"))`.
- **Record judgements** by passing a
  [`scans_annotations()`](https://jameshwade.github.io/scans/reference/scans_annotations.md)
  store to the app. Labels and notes append to a JSON-lines log that
  never rewrites earlier records, so reviewers cannot overwrite each
  other and every record stays available to your own analysis.

Core diagnostics return ordinary tibbles and compose with the base pipe:

``` r

bundle |>
  filter_trajectory_events(role = "assistant", event_type = "tool_call")

bundle |>
  scan_trajectories(scans = "repeated_tool_call")
```

## Scope

scans is post-run and read-only. It does not execute an agent, replay
tools, or score answer quality. Use
[vitals](https://vitals.tidyverse.org/) to ask “Did the agent succeed?”
and scans to ask “What did it do, what deserves attention, and where is
the evidence?”

Its design is inspired by Meridian Labs’
[inspect_scout](https://meridianlabs-ai.github.io/inspect_scout/). scans
is independently maintained and borrows conventions from tidyverse
packages without being part of the tidyverse.

## Getting help and contributing

See
[SUPPORT.md](https://github.com/JamesHWade/scans/blob/main/.github/SUPPORT.md)
for where to ask questions and
[CONTRIBUTING.md](https://github.com/JamesHWade/scans/blob/main/.github/CONTRIBUTING.md)
for the development workflow. Please note that scans is released with a
[Contributor Code of
Conduct](https://github.com/JamesHWade/scans/blob/main/.github/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
