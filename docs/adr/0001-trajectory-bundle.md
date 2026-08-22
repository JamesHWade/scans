# ADR 0001: Represent trajectories as an S7 relational bundle

## Status

Accepted by the maintainer on 2026-08-22 as part of issue #4.

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
3. Define an S7 value object with typed properties for every table.
4. Collapse the model into one deeply nested object or one denormalized table.

## Decision

Use one S7 class, `TrajectoryBundle`, containing four entity-table properties,
one audit-table property, and an integer schema-version property:

- `trajectories`
- `turns`
- `events`
- `evaluations`
- `losses`
- `schema_version`

The first four tables are the public relational data model. `losses` is an
audit table, not another agent-domain entity. It records source data that an
adapter redacted, truncated, externalized, or could not represent exactly.

The exported S7 class object will provide the formal type, user constructor,
and extension point. Its customized `TrajectoryBundle()` constructor will
coerce inputs to tibbles and create typed empty optional tables where
appropriate. The class validator will enforce the complete relational contract
during construction and after property replacement.

`as_trajectory()` will be an S7 generic. Its methods can dispatch on S7 source
objects such as ellmer turns as well as S3, R6, and base classes exposed by
other integrations. Source-specific methods or explicit adapter functions will
snapshot live upstream objects and return a bundle. The bundle will never
retain a live chat, reactive, provider client, R6 controller, connection,
environment, or external pointer.

Public accessors will return ordinary tibbles:

- `trajectory_info()`
- `trajectory_turns()`
- `trajectory_events()`
- `trajectory_evaluations()`
- `trajectory_losses()`

The constructor and property replacement will validate automatically.
`S7::validate()` is the manual validation entry point; scans will not add a
redundant validation generic. Adapter and input-shape failures will continue to
use stable scans condition classes.

## Why S7

The bundle is a validated value boundary: five tables and one schema version
are meaningful only when their types, keys, ordering, and references agree.
S7 makes those properties and their whole-object validator part of the formal
class definition. It also reruns validation when a property is replaced,
instead of relying on every package function to remember an S3 convention.

This formal boundary does not change the rectangular analysis surface. Each
property is still a tibble, and the public accessors return those tibbles for
base R, dplyr, plotting, and reporting. Individual trajectories, turns, events,
evaluations, and losses are rows, not separate S7 objects.

S7 also fits the initial integration boundary. ellmer represents turns and
content with S7, while S7 generics can register methods for S7, S3, and base
classes. The class remains a shallow value object, not a workflow, query
language, database, or execution runtime.

## Consequences

- R users can extract and pipe ordinary tibbles without learning a custom data
  manipulation API.
- Adapter authors get one validator for identity, ordering, references, and
  serializability.
- The package will import `S7` and `tibble`; `dplyr` remains optional.
- Replacing one or several properties creates an updated R value and triggers
  S7 validation. scans will not provide in-place mutation verbs.
- Extra source columns can be preserved, but stable shared fields must use the
  canonical columns.
- Validation has a cost proportional to the snapshot size. Trusted package
  code can use S7's supported deferred-validation tools when changing several
  properties as one operation.
- S7 is a newer dependency with less ecosystem usage than S3. scans accepts
  that pre-1.0 tradeoff because formal invariants and ellmer alignment are more
  important at this boundary.
- A future lazy or database-backed representation may implement the same
  accessors, but it is not part of schema version 1.

## Rejected alternatives

### Independent tibbles

Independent tables are easy to create but provide no durable place for schema
version, adapter losses, cross-table validation, or concise printing.

### An S3 list wrapper

An S3 wrapper would keep the same tidy analysis surface with one fewer
dependency. It would not formally declare the table properties, automatically
validate property replacement, or provide the same adapter-dispatch path
across S7 and S3 sources. Those omissions matter more than the smaller object
system footprint.

### One denormalized table

A single event table would duplicate trajectory, turn, and evaluation data. It
would also make multiple evaluations and event-free turns awkward to express.

### Deeply nested messages

Nested objects resemble provider payloads but work poorly for filtering,
joining, summarizing, serialization, and cross-source conformance tests.
