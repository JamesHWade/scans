# Filter trajectory events

`filter_trajectory_events()` selects canonical events using event
fields, containing-turn context, or exact top-level metadata values.
Filters compose with AND across arguments and OR within each character
vector.

## Usage

``` r
filter_trajectory_events(
  x,
  trajectory_id = NULL,
  role = NULL,
  event_type = NULL,
  content_type = NULL,
  tool = NULL,
  status = NULL,
  metadata = NULL
)
```

## Arguments

- x:

  A
  [TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

- trajectory_id, role, event_type, content_type, tool, status:

  Optional character vectors. An event must match one value in every
  supplied filter.

- metadata:

  An optional uniquely named list of top-level event metadata values to
  match with exact R equality.

## Value

A tibble containing the selected canonical event rows and any extra
source columns, followed by these derived turn-context columns:

- Identity and order: character `trajectory_id`, `event_id`, and
  optional `turn_id` and `parent_event_id`; integer `event_index` and
  optional `content_index`.

- Event description: character `event_type`, optional `content_type`,
  `name`, `call_id`, and `text`; list-column `value` for structured
  payloads.

- Outcome and extensions: optional character `status` and `error`, plus
  the named-list `metadata` column.

- `.turn_role`: character role of the containing turn.

- `.turn_index`: integer position of the containing turn.

- `.round_index`: optional integer round of the containing turn.

Rows follow trajectory order and then `event_index`. A valid filter with
no matches returns a typed zero-row tibble with the same columns.

## Details

The returned event columns are not modified. Three derived columns are
added after `turn_id`: `.turn_role`, `.turn_index`, and `.round_index`.
These names are reserved so source columns are never overwritten; a
source event table that already uses one of them produces a
`scans_error_scan_column_collision` error.

## Examples

``` r
bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "trajectory-1", source_type = "manual"),
  data.frame(
    trajectory_id = "trajectory-1",
    turn_id = "turn-1",
    turn_index = 1L,
    role = "assistant"
  ),
  data.frame(
    trajectory_id = "trajectory-1",
    event_id = "event-1",
    event_index = 1L,
    turn_id = "turn-1",
    event_type = "content",
    content_type = "text",
    text = "Done"
  )
)

filter_trajectory_events(bundle, role = "assistant")
#> # A tibble: 1 × 20
#>   trajectory_id event_id event_index turn_id .turn_role .turn_index .round_index
#>   <chr>         <chr>          <int> <chr>   <chr>            <int>        <int>
#> 1 trajectory-1  event-1            1 turn-1  assistant            1           NA
#> # ℹ 13 more variables: content_index <int>, parent_event_id <chr>,
#> #   event_type <chr>, content_type <chr>, name <chr>, call_id <chr>,
#> #   text <chr>, value <list>, timestamp <dttm>, duration <dbl>, status <chr>,
#> #   error <chr>, metadata <list>
```
