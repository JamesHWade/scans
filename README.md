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
[{dsprrr}](https://github.com/JamesHWade/dsprrr),
[{commons}](https://github.com/posit-dev/commons), and
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

A Connect pin provides a simple durable handoff between trace collection and
review. Publish one versioned `TrajectoryBundle` pin per deployed application,
then use this `app.R` for the reviewer:

```r
library(scans)

board <- pins::board_connect()

pin_loader <- function(name) {
  force(name)
  function() pins::pin_read(board, name)
}

scans_app(list(
  "Support assistant" = pin_loader("agents/support-traces"),
  "Research assistant" = pin_loader("agents/research-traces")
))
```

On Connect, `pins::board_connect()` can use the server and ephemeral API-key
environment variables supplied to the content process. The content owner must
have access to each pin; installations that disable those automatic variables
need an administrator-approved credential configuration. Deploy the directory
containing `app.R` with `rsconnect::deployApp()`.

Use a scheduled collector or another single-writer process to update each pin.
Connect application working directories are not persistent across deployments,
and pins are not a concurrent telemetry database. For high-volume applications,
keep traces in a database or object store and replace each `pin_loader()` with a
zero-argument loader that returns a detached `TrajectoryBundle`.

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
