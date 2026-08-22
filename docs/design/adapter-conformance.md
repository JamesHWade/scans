# Adapter conformance contract

## Purpose

Every scans adapter converts a completed source object into the same validated
`TrajectoryBundle` contract. The conformance suite makes that boundary
repeatable across ellmer and later ellmerverse integrations without making test
helpers part of the user-facing package API.

## Required behavior

Given an unchanged completed source object, an adapter must:

1. return a schema-version-1 `TrajectoryBundle` without network access;
2. produce deterministic canonical IDs and table values;
3. preserve explicit source identity and provenance;
4. represent order with canonical indices rather than relying on row order;
5. preserve tool request and result correlation through `call_id`;
6. retain supported source data in canonical fields or safe metadata;
7. record unsupported, redacted, truncated, or externalized data in `losses`;
8. reject malformed source data with a stable `scans_error` subclass; and
9. produce a bundle that survives ordinary R serialization without retaining a
   live source object.

The expected bundle may use any physical row order. Conformance comparisons
order entity tables by their canonical identities and indices before checking
them. Repeated conversion of the same source must still be byte-for-byte
deterministic at the property level.

## Test interface

Adapter tests use the helpers loaded from
`tests/testthat/helper-adapter-conformance.R`:

```r
expected <- trajectory_fixture("multiple_tools")

expect_adapter_conforms(
  source = completed_source,
  expected = expected,
  adapter = as_trajectory
)
```

`expect_adapter_rejects()` checks the stable condition hierarchy for malformed
sources. The shared fixtures and contributor instructions live in
`tests/testthat/fixtures/`.

These helpers are intentionally test-only. The public extension point remains
the S7 `as_trajectory()` generic, and the public result remains
`TrajectoryBundle`.
