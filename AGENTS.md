# Agent instructions

## Agent skills

### Issue tracker

Issues and specs are tracked in GitHub Issues. See
`docs/agents/issue-tracker.md`.

### Triage labels

Use the canonical triage label vocabulary. See
`docs/agents/triage-labels.md`.

### Domain docs

This is a single-context repository. See `docs/agents/domain.md`.

## Package development

- Use the tidyverse style guide as the default code-style reference and
  use the base pipe, `|>`. This is an independent package, not a
  tidyverse package.
- Run `air format .` after changing R code.
- Run `jarl check .` before committing.
- Use `cli` for user-facing conditions and messages, and `rlang` for
  condition and call handling.
- Keep ellmer, vitals, and shinychat integrations optional until a
  public API requires one as a hard dependency.
- Keep tests deterministic. Do not require API keys, network access, or
  live model responses.

### Verification

``` sh
Rscript -e 'devtools::test()'
Rscript -e 'pkgdown::check_pkgdown()'
Rscript -e 'devtools::check()'
```
