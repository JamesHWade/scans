# Domain docs

How the engineering skills should consume this repository's domain
documentation when exploring the codebase.

## Before exploring, read these

- `CONTEXT.md` at the repository root, or
- `CONTEXT-MAP.md` at the repository root if it exists; it points to the
  relevant context documents.
- ADRs in `docs/adr/` that touch the area being changed.

If these files do not exist, proceed silently. Domain-modeling skills create
them lazily when terms or decisions are resolved.

## File structure

This is a single-context repository:

```text
/
├── CONTEXT.md
├── docs/adr/
└── R/
```

## Use the glossary's vocabulary

When output names a domain concept in an issue, proposal, hypothesis, or test,
use the term defined in `CONTEXT.md`. Do not drift to synonyms that the
glossary explicitly avoids.

If a needed concept is absent, reconsider whether the language belongs in the
project or note the gap for domain modeling.

## Flag ADR conflicts

If output contradicts an existing ADR, surface the conflict explicitly rather
than silently overriding it.
