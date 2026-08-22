# scans

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml)
[![format-check](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml)
[![jarl-check](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml)
<!-- badges: end -->

Trajectory diagnostics for AI agents in R.

{scans} complements [{vitals}](https://vitals.tidyverse.org/) by examining
the trajectories that produce evaluation outcomes. Its design is inspired by
Meridian Labs' [inspect_scout](https://meridianlabs-ai.github.io/inspect_scout/)
and is intended for agents built with the ellmerverse and related R packages.

The package is in early development. Its canonical trajectory contract is a
validated S7 object whose analysis properties remain ordinary tibbles.

{scans} is independently maintained. It borrows development conventions and
interface principles from tidyverse packages, but it is not part of the
tidyverse organization.

## Ecosystem

{scans} is being designed around tidy, rectangular trajectory data and small,
composable analysis functions. Integrations with
[{ellmer}](https://ellmer.tidyverse.org/),
[{vitals}](https://vitals.tidyverse.org/),
[{deputy}](https://github.com/JamesHWade/deputy),
[{dsprrr}](https://github.com/JamesHWade/dsprrr), and
[{shinychat}](https://posit-dev.github.io/shinychat/r/) will remain optional so
the core analysis layer can also inspect trajectories from other agent
frameworks.

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

## Installation

You can install the development version from GitHub with:

```r
pak::pak("JamesHWade/scans")
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
