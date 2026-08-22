# scans

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/R-CMD-check.yaml)
[![format-check](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/format-check.yaml)
[![jarl-check](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml/badge.svg)](https://github.com/JamesHWade/scans/actions/workflows/jarl-check.yaml)
<!-- badges: end -->

Trajectory diagnostics for AI agents in R.

{scans} will complement [{vitals}](https://vitals.tidyverse.org/) by examining
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
[{vitals}](https://vitals.tidyverse.org/), and
[{shinychat}](https://posit-dev.github.io/shinychat/r/) will remain optional so
the core analysis layer can also inspect trajectories from other agent
frameworks.

## Trajectory bundles

`TrajectoryBundle()` creates a detached snapshot with explicit relational
tables. Empty semantic tables are valid, so adapters can preserve incomplete
or interrupted agent paths without inventing events.

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
