# ADR 0002: Keep source adapters separate from trajectory supplements

## Status

Proposed on 2026-08-22 as part of issue #17.

## Context

The first scans integrations snapshot ellmer chats and vitals evaluation
samples into a `TrajectoryBundle`. The wider ellmerverse+ contains two
different kinds of post-run input:

1. Sources that can independently describe a completed execution, including
   semantic turns, execution events, or a complete product-level review.
2. Sources that only add another view of an execution already represented by a
   bundle, such as rendered UI state or conversation branches.

Treating both kinds as one generic enrichment interface would require callers
to learn match rules, conflict precedence, mutation order, and partial-failure
behavior before two usable supplement sources exist. Treating every source as
a complete adapter would instead encourage presentation state to replace the
semantic model-visible record.

## Decision

Keep `as_trajectory()` as the interface for complete source adapters. Add an
explicit package adapter when the upstream value has no safely dispatchable
class. Each adapter returns a new, self-contained `TrajectoryBundle` and does
not mutate another bundle.

Classify the audited packages as follows:

- deputy: complete source adapter from an `AgentResult`.
- dsprrr: complete source adapter from a module or exported trace snapshot.
- commons: complete explicit adapter from `trajectory_read()` output.
- tempest: complete explicit adapter from `tempest_trajectory_review()`
  output. Its deliberate omissions are recorded as losses rather than filled
  from private product state.
- shinychat: documentation only until shinychat exposes a public settled
  history snapshot. A future integration will be a supplement because
  semantic turns remain ellmer-owned.

Do not add a general `augment_trajectory()` or similar generic now. Revisit a
supplement interface when a public shinychat snapshot or a second
supplement-only source makes that seam concrete.

Cross-source correlation follows these rules:

- `run_id` names the execution identified by that source. Equal-looking IDs
  from different systems are not silently substituted.
- `parent_trajectory_id` represents an actual agent delegation hierarchy. It
  never means that two package records are different views of the same work.
- Source-specific identifiers remain in sanitized, namespaced metadata until
  at least two integrations demonstrate shared semantics.
- A correlation identifier is grouping evidence, not proof of causation,
  authorship, or authority.
- Adapter losses state when an upstream public snapshot omits semantic content
  that scans cannot reconstruct.

## Consequences

- Callers continue to learn one small source-adapter interface.
- Adapter output is deterministic and testable without installing every
  ellmerverse+ package.
- scans does not need conflict-resolution or in-place mutation semantics.
- Cross-package analysis initially uses stable identifiers retained in
  metadata. A later composition interface can promote fields only after their
  semantics are proven across sources.
- Tempest product review and dsprrr execution identity remain useful without
  weakening their upstream authority or integrity claims.
- shinychat presentation and branch data remain unavailable through scans
  until they can be captured without private fields or live reactive objects.

## Rejected alternatives

### One enrichment generic for every package

This makes adapter order and conflict resolution part of the public interface.
Most audited packages can already produce a self-contained bundle, so the
extra interface would be shallow.

### Treat shinychat as another ellmer adapter

shinychat rendered messages can differ from what the model saw, and its
conversation graph can contain inactive branches. Replacing ellmer turns with
that state would conflate semantic and presentation records.

### Reach into private upstream state

Private R6 fields, non-exported history controllers, and Tempest product
internals may expose more data today, but they do not form stable source
contracts. scans records a loss or defers the integration instead.
