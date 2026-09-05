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

`tempest-review.rds` was produced by the installed Tempest 0.0.0.9000 public
`tempest_trajectory_review()` function from Tempest's provider-free STORM
promotion fixture. Tests read that authoritative result and project it through
Tempest's exported accessor; they do not reproduce Tempest classes or digest
contracts. After installing a matching Tempest checkout, run from the scans root:

```sh
Rscript tools/rebuild-tempest-fixture.R /path/to/matching/tempest-checkout
```

The fixture includes a host-accepted promotion receipt from a real Graft store.

`commons/trace-0.jsonl` is a static OTLP envelope. Commons tests obtain their
source by passing its directory to `commons::trajectory_read()`. The dsprrr
module fixture similarly uses `dsprrr::module_fn()` and `dsprrr::run()` instead
of mutating module state.
