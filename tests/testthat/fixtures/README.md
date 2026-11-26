# Trajectory adapter fixtures

`trajectory-fixtures.R` is the shared, provider-free corpus for scans adapter
tests. Each named fixture is a validated `TrajectoryBundle` that represents the
canonical output an adapter must produce.

The corpus covers simple conversation, parallel tool calls, tool failure,
repeated tool use, delegated activity, missing source data, and an evaluated
trajectory. The `ellmerverse_correlation` fixture adds independent Tempest,
Deputy, dsprrr, and ellmer source views whose exact identifiers correlate
without being collapsed into one source identity. Together the fixtures
populate every canonical schema column and exercise every core loss reason.

## Add a source fixture

1. Add a small constructor to `trajectory-fixtures.R` and include it in
   `trajectory_fixtures()`.
2. Use synthetic values only. Do not include credentials, provider payloads,
   private conversations, API calls, or generated fixture IDs that vary across
   runs.
3. Preserve explicit source identity, event order, call correlation, and any
   adapter loss in the expected bundle.
4. In the adapter test, obtain the source through the upstream package's public
   constructor, runner, reader, or exporter and call
   `expect_adapter_conforms(source, expected, adapter)`. The helper runs the
   adapter twice, compares canonical output independently of physical row order,
   validates serialization, and checks the full expected bundle.
5. If the source is malformed, add it to `malformed_trajectory_fixtures()` with
   the stable scans condition class the adapter must raise.

Fixtures are loaded through `testthat::test_path()`, so adapter tests work from
the source tree and during `R CMD check` without network access.

`tempest-review.rds` was produced through Tempest's public
`tempest_trajectory_review()` function using its provider-free STORM promotion
fixture. It was rebuilt on 2026-11-15 with Tempest source commit
`de2d3ddde4022648f70e6cd985d114690b1ff67a`, pinned in `Remotes`. The producer
publishes the exact promotion bundle through Graft API4, accepts the retained
selection with a host-supplied predecessor and review metadata, and records the
exact selection and decision in the review. This build used Graft source commit
`ad0ab921a02df602bb3687649e48b2e470cf23d6` and dsprrr source commit
`a0a372d515cc32ab9296fad8f91440bba84d187e`; their `Remotes` entries remain
unpinned. Tests read the authoritative review and project it through Tempest's
exported accessor; they do not reproduce Tempest classes or digest contracts.
Install the Tempest revision and current Graft and dsprrr versions into an
isolated R library, then run from the scans root:

```sh
Rscript tools/rebuild-tempest-fixture.R /path/to/matching/tempest-checkout
```

The fixture includes a host decision accepting an exact artifact selection.
It uses Tempest trajectory schema 3, which contains no native graph snapshot.
Tempest's promotion-bundle format changed after the previous fixture was
generated, while the trajectory review remains schema 3. Before rebuilding,
verify that the installed Tempest source revision matches the supported
producer. Do not repair digests inside serialized reviews or skip a test
because a producer rejects the value.

The Tempest fixture constructors assign current `created_at` values to the
synthetic claim and evidence span. Those provenance values contribute to the
stage-output digests, so rebuilding can change the bundle, selection, decision,
review IDs, and RDS bytes while preserving the same semantic fixture. Tests
check the review fields and exact selection/decision binding within each
fixture; they do not compare serialized bytes across rebuilds. Do not replace
the creation times or rewrite their digests to make the file byte-identical.

`commons/trace-0.jsonl` is a static OTLP envelope. Commons tests obtain their
source by passing its directory to `commons::trajectory_read()`.

`commons/current/trace-0.jsonl` is a static OTLP envelope written by hand on
2026-11-25 against commons 0.2.0.9000 (source commit `820659f`), following the
span layout in commons' `pkg-r/R/trajectory-read.R`. Commons tests read it
through `commons::trajectory_read()`. It holds two conversations:
`conversation-current` has two `commons_conversation_turn` spans with
provenance tags, a `commons.connect.user` attribute, a tool exchange, and a
hidden `<system-instruction>` follow-up turn; `conversation-recovered` has chat
spans without `gen_ai.input.messages`, so commons rebuilds it with its
missing-input and missing-tool-result placeholder turns. It lives in its own
directory so reads of `commons/` stay unchanged. The dsprrr
module fixture similarly uses `dsprrr::module_fn()` and `dsprrr::run()` instead
of mutating module state.
