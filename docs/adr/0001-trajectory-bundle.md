# ADR 0001: Represent trajectories as a validated relational bundle

## Status

Proposed for maintainer review in issue #4.

## Context

scans needs one source-neutral object for completed agent trajectories. It must
retain ordered turns and events, connect evaluations to the path that produced
them, represent multi-agent relationships, and remain comfortable for R users
who work with tidy data.

The concept map in `docs/design/trajectory-concept-map.md` established four
canonical entity tables: trajectories, turns, events, and evaluations. The
remaining architectural choice is how to expose and validate those tables.

The main options are:

1. Return unrelated tibbles and rely on each function to validate them.
2. Define an S3 list that owns the related tibbles and validates their keys.
3. Define an S7 object with typed properties for every table.
4. Collapse the model into one deeply nested object or one denormalized table.

## Decision

Use a lightweight S3 class, `scans_trajectory_bundle`, containing four entity
tables and one audit table:

- `trajectories`
- `turns`
- `events`
- `evaluations`
- `losses`

The first four tables are the public relational data model. `losses` is an
audit table, not another agent-domain entity. It records source data that an
adapter redacted, truncated, externalized, or could not represent exactly.

The public constructor will be `trajectory_bundle()`. It will coerce inputs to
tibbles, create typed empty optional tables where appropriate, validate the
relational contract, and return the S3 object. An internal
`new_trajectory_bundle()` will construct an already validated object without
user-facing checks.

`as_trajectory()` will be the adapter generic. Source-specific methods or
explicit adapter functions will snapshot live upstream objects and return a
bundle. The bundle will never retain a live chat, reactive, provider client,
R6 controller, connection, environment, or external pointer.

Public accessors will return ordinary tibbles:

- `trajectory_info()`
- `trajectory_turns()`
- `trajectory_events()`
- `trajectory_evaluations()`
- `trajectory_losses()`

`validate_trajectory()` will return its input invisibly when valid and raise a
classed scans error otherwise. The constructor and adapters will always call
it; users should not need to call it in an ordinary workflow.

## Why S3

The bundle needs relational validation and print behavior, but it does not need
mutable state or a new scalar type. S3 adds the smallest useful boundary while
keeping every analysis table directly usable with base R and tidy tools.

S7 would make property declarations more explicit, but it would add an object
system dependency without improving the rectangular analysis surface. Bare
tibbles would be maximally familiar but would repeatedly lose the fact that
the tables form one referentially consistent snapshot.

The S3 wrapper is deliberately shallow. It is not a workflow, query language,
database, or execution runtime.

## Consequences

- R users can extract and pipe ordinary tibbles without learning a custom data
  manipulation API.
- Adapter authors get one validator for identity, ordering, references, and
  serializability.
- The package will import `tibble`; `dplyr` remains optional.
- The object is immutable by convention and R copy-on-modify behavior, not by
  an active binding or private state.
- Extra source columns can be preserved, but stable shared fields must use the
  canonical columns.
- Validation has a cost proportional to the snapshot size. The internal
  constructor prevents repeated validation inside trusted package code.
- A future lazy or database-backed representation may implement the same
  accessors, but it is not part of schema version 1.

## Rejected alternatives

### Independent tibbles

Independent tables are easy to create but provide no durable place for schema
version, adapter losses, cross-table validation, or concise printing.

### S7 properties

S7 is appropriate for validated value objects, but the useful values here are
already tibbles. An S7 shell would expose essentially the same list of tables
with more dependency and dispatch machinery.

### One denormalized table

A single event table would duplicate trajectory, turn, and evaluation data. It
would also make multiple evaluations and event-free turns awkward to express.

### Deeply nested messages

Nested objects resemble provider payloads but work poorly for filtering,
joining, summarizing, serialization, and cross-source conformance tests.
