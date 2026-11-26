# Convert completed commons conversations

`as_trajectory_commons()` converts the already-read result of
[`commons::trajectory_read()`](https://posit-dev.github.io/commons/r/reference/trajectory_read.html)
into one canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
Each named conversation becomes one trajectory. Both current records
with `turns`, `last_active`, and `user` fields and older attributed turn
lists are accepted. Model-visible turns are delegated to
[`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md).
The adapter does not read files, connect to Posit Connect, or parse
trace records itself.

## Usage

``` r
as_trajectory_commons(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
)
```

## Arguments

- x:

  An already-read, named list returned by
  [`commons::trajectory_read()`](https://posit-dev.github.io/commons/r/reference/trajectory_read.html).

- trajectory_id:

  A bundle-unique trajectory ID. When `NULL`, a stable ID is derived
  from the conversation name. It must be `NULL` when `x` contains
  multiple conversations.

- source_uri:

  An optional locator for where the conversations were read from,
  applied to every conversation. Credentials, query parameters, and
  fragments are removed.

- metadata:

  A uniquely named list of safe application metadata, applied to every
  conversation. Sensitive fields are redacted and recorded as losses.
  Names used by commons source metadata are reserved.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

Provenance stays attached to the turn list. Each source provenance
record becomes a `"commons:provenance"` event whose `exchange_index`
matches the `round_index` of its turns. Rounds follow commons' exchange
rule: a user turn that is neither a tool result nor an internal reminder
opens a round. Internal reminder turns, which commons injects as hidden
follow-ups, belong to the round they follow, keep their turn rows, and
are marked with `commons_internal = TRUE` in turn metadata. Because the
public commons result does not expose an exchange-to-turn key,
provenance events do not infer a `turn_id`.

The conversation `user`, the Posit Connect user recorded by commons, is
retained as `metadata$otel$user`, the slot
[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
uses for the same span-derived identity. A missing user (`NA`) is not a
loss.

Commons substitutes placeholder turns when a trace omitted input history
or tool results. Those turns are kept as recorded, and each one is
reported as a `capture$input` or `capture$tool_result` loss on that
turn, so
[`assess_trajectory_scans()`](https://jameshwade.github.io/scans/reference/assess_trajectory_scans.md)
does not treat the transcript as complete.

Current commons results carry no outer source descriptor, and its
absence is not a loss. An outer `source` attribute supplied by an older
reader or by the caller is retained as sanitized metadata, and a
malformed or conflicting one is reported as a loss. Missing or malformed
provenance and activity times are adapter losses. Extra record fields
and turn attributes are retained as sanitized metadata.

Because
[`commons::trajectory_read()`](https://posit-dev.github.io/commons/r/reference/trajectory_read.html)
returns an ordinary list without a discriminating class, use this
explicit adapter instead of
[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md).

## Examples

``` r
if (rlang::is_installed("ellmer", version = "0.5.0")) {
  turns <- list(
    ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
  )
  attr(turns, "provenance") <- list(list(
    provenance_tag = "A",
    citation_decisions = list()
  ))
  conversation <- list(
    turns = turns,
    last_active = as.POSIXct("2026-08-23 12:00:00", tz = "UTC"),
    user = "analyst"
  )

  as_trajectory_commons(
    list(`conversation-001` = conversation),
    source_uri = "file:///traces"
  )
}
#> <TrajectoryBundle> schema 1
#> trajectories: 1
#> turns: 2
#> events: 3
#> evaluations: 0
#> losses: 0
#> sources: commons
```
