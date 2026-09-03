# Convert completed commons conversations

`as_trajectory_commons()` converts the already-read result of
[`commons::trajectory_read()`](https://posit-dev.github.io/commons/reference/trajectory_read.html)
into one canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
Each named conversation becomes one trajectory, and its model-visible
turns are delegated to
[`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md).
The adapter does not read files, connect to Posit Connect, or parse
trace records itself.

## Usage

``` r
as_trajectory_commons(x, trajectory_id = NULL, metadata = list())
```

## Arguments

- x:

  An already-read, named list returned by
  [`commons::trajectory_read()`](https://posit-dev.github.io/commons/reference/trajectory_read.html).

- trajectory_id:

  A bundle-unique trajectory ID. When `NULL`, a stable ID is derived
  from the conversation name. It must be `NULL` when `x` contains
  multiple conversations.

- metadata:

  A uniquely named list of safe application metadata, applied to every
  conversation. Sensitive fields are redacted and recorded as losses.
  Names used by commons source metadata are reserved.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

Conversation provenance and the outer source descriptor are retained as
sanitized metadata. Each source provenance record also becomes a
`"commons:provenance"` event. Because the public commons result does not
expose an exchange-to-turn key, provenance events retain their source
index without inferring a `turn_id`. Missing or malformed source facts
are reported as adapter losses.

Because
[`commons::trajectory_read()`](https://posit-dev.github.io/commons/reference/trajectory_read.html)
returns an ordinary list without a discriminating class, use this
explicit adapter instead of
[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md).

## Examples

``` r
if (rlang::is_installed("ellmer", version = "0.4.2")) {
  conversation <- list(
    ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
  )
  attr(conversation, "last_active") <- as.POSIXct(
    "2026-08-23 12:00:00",
    tz = "UTC"
  )
  attr(conversation, "provenance") <- list(list(
    provenance_tag = "A",
    citation_decisions = list()
  ))

  source <- list(`conversation-001` = conversation)
  attr(source, "source") <- list(kind = "local", path = "/traces")
  as_trajectory_commons(source)
}
#> <TrajectoryBundle> schema 1
#> trajectories: 1
#> turns: 2
#> events: 3
#> evaluations: 0
#> losses: 0
#> sources: commons
```
