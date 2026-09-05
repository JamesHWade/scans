# Contributing to scans

Thanks for helping improve scans.

## Before making a change

For a typo or small documentation correction, a focused pull request is
welcome. For a new feature or substantial behavior change, open an issue
first so that we can agree on the problem and the shape of the solution.

Bug reports should include a minimal
[reprex](https://reprex.tidyverse.org/) whenever possible.

## Pull request process

1.  Fork and clone the package. If you use usethis, run
    `usethis::create_from_github("JamesHWade/scans", fork = TRUE)`.
2.  Install development dependencies with
    `devtools::install_dev_deps()`.
3.  Create a branch with a short, descriptive name.
4.  Make the change, test affected behavior, and update documentation
    where needed.
5.  Run the local verification commands below.
6.  Open a pull request that explains the problem, the approach, and the
    verification performed.

For user-facing behavior changes, add a concise bullet under the
development heading in `NEWS.md`. Small documentation corrections do not
need a bullet.

## Development style

scans is independently maintained. We use the [tidyverse style
guide](https://style.tidyverse.org/) and [code review
principles](https://code-review.tidyverse.org/) as useful engineering
references, not as a statement of project affiliation.

- Format R code with `air format .`.
- Lint with `jarl check .`.
- Use roxygen2 with Markdown for documentation.
- Use testthat for deterministic tests that do not require credentials
  or live model calls.
- Use ordinary R data structures. Extract a helper when it names a
  useful operation or keeps a shared rule in one place.

## Writing and review

Describe what users can do and what the recorded evidence supports. Keep
limitations next to the feature they qualify. Remove repeated
explanations, unsupported performance claims, and wording that praises
the implementation.

Comments should explain constraints, source conventions, or non-obvious
failure modes. Remove comments that restate the code or describe an
abandoned design. Keep current priorities in the
[roadmap](https://github.com/JamesHWade/scans/issues/2) and durable
design decisions in `docs/`; avoid copying issue acceptance lists into
design documents.

Tests should protect a behavior, invariant, or known failure. Prefer
expected outcomes from public source objects. An unexpectedly empty
fixture result should fail, not skip. Reserve skips for unavailable
optional dependencies or explicitly unsupported environments. Avoid
tests of incidental prose or the structure of an internal helper.

Before deleting a helper, inspect its callers. Preserve shared rules for
identity, missing values, and sanitization. A short function or a single
caller is not enough reason to remove it. Explain the maintenance cost
removed by a cleanup; test counts and line reductions are not goals.

## Local checks

Run the full local verification suite with:

``` sh
air format . --check
jarl check .
Rscript -e 'devtools::test()'
Rscript -e 'pkgdown::check_pkgdown()'
Rscript -e 'devtools::check()'
```

## Code of conduct

By participating, you agree to follow the [Contributor Code of
Conduct](https://jameshwade.github.io/scans/CODE_OF_CONDUCT.md).
