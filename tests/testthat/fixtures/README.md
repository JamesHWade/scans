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
4. In the adapter test, construct the source object locally and call
   `expect_adapter_conforms(source, expected, adapter)`. The helper runs the
   adapter twice, compares canonical output independently of physical row order,
   validates serialization, and checks the full expected bundle.
5. If the source is malformed, add it to `malformed_trajectory_fixtures()` with
   the stable scans condition class the adapter must raise.

Fixtures are loaded through `testthat::test_path()`, so adapter tests work from
the source tree and during `R CMD check` without network access.

`helper-tempest-adapter.R` constructs a bounded, provider-free review using
Tempest's exact internal review class and digest contract. It also converts one
review into a plain structural lookalike to verify that the adapter accepts only
authoritative Tempest reviews; production code uses Tempest's exported
projection accessor.
