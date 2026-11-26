# Route and provenance analysis

Implements #70. `R/route-analysis.R` finds repeated exploratory or fallback
tool routes in completed trajectories and ranks them as candidates for a
governed measure or context document. The functions are internal until a
second consumer, such as the app view or a saved report, proves the
interface. `scans_tools()` exposes the same results to Ask and coding agents
through `scans_route_counts` and `scans_route_candidates`.

The analysis reads canonical bundle tables only. It never calls a model,
replays a tool, or evaluates captured SQL or R.

## Exchanges

An exchange is the turn's `round_index` when the trajectory records rounds,
as `as_trajectory_commons()` does. Otherwise a user turn opens an exchange
unless it holds a `tool_result` event or carries `commons_internal = TRUE` in
turn metadata; turns before the first such user turn belong to no exchange.
`exchange_basis` records which rule applied. Tool calls in unassigned turns,
or with no turn, are reported as `n_unassigned_tool_calls` per trajectory
rather than dropped silently.

## Provenance classes

Classes come only from `commons:provenance` events. Each event is attached to
`metadata$exchange_index`, or to the exchange of its `turn_id` when that index
is absent. The tag is `value$provenance_tag`. Following commons'
`provenance_display` and its trajectory review guide, tags map as:

| Recorded evidence | `provenance_class` | `provenance_reason` |
| --- | --- | --- |
| One tag `A` | `trusted` | `recorded` |
| One tag `B` | `cited` | `recorded` |
| One tag `C` | `untrusted` | `recorded` |
| No event for the exchange | `unclassified` | `missing_event` |
| Tag `NA` in every event | `unclassified` | `missing_tag` |
| Several tags, or events that disagree | `unclassified` | `conflicting_tags` |
| Any other value | `unclassified` | `unrecognized_tag` |
| Trajectory has no provenance evidence | `not_applicable` | `no_provenance_source` |

A trajectory has provenance evidence when it contains a `commons:provenance`
event or a loss with field `provenance` (the commons adapter's record for a
conversation whose provenance attribute is missing or malformed). Such a
trajectory's untagged exchanges are `unclassified`: the provenance is
unknown, not absent. Plain ellmer, Deputy, and OTel bundles without these
events are `not_applicable`, so their exchanges stay in denominators without
being counted as unclassified.

Classes describe the recorded answer path, not correctness. They are never
inferred from answer text or from per-tool `commons_tag` values in tool
result metadata, and `unclassified` is never merged into `untrusted`.

## Route signatures

Every exchange with retained turns gets a signature, with or without
provenance:

- One step per turn that holds tool calls, in turn order.
- Calls within one turn are parallel; their distinct tool names are sorted in
  C-locale order and joined with ` + `.
- Immediately repeated steps collapse, so `run_sql > run_sql` is `run_sql`.
  `n_tool_calls` keeps the raw count.
- Signatures keep at most `max_steps` steps (default 12) and then end in
  ` > ...`, with `route_truncated = TRUE`.
- An exchange with turns but no tool calls has signature `(none)`. An
  exchange known only from a provenance record has signature `NA`.

`route_features()` adds normalized argument values joinable by `event_id`:
`describe_table` tables and `call_*` measure names as recorded, and
`run_sql` table names found after `FROM`/`JOIN` by a conservative regular
expression, flagged `heuristic = TRUE`. Features never feed signatures,
counts, or ranking.

## Cohort summaries

`route_summary(x, by)` returns tibbles:

- `overall`: one row per class, including zeros, with `total_exchanges`,
  `total_trajectories`, `total_users`, `n_unknown_user_trajectories`, and two
  windows. The event window covers recorded event timestamps in exchanges;
  commons conversations have none, so it is usually missing. The trajectory
  window covers recorded trajectory bounds; for commons this is
  `last_active`, a conversation time rather than an answer time. Neither is
  the reader's query window or `n` limit, which the bundle does not retain.
- `trajectory`: one row per trajectory in scope, including those with no
  exchanges.
- `signature`: one row per signature with its class breakdown.

Users come from `metadata$otel$user`; missing users are counted as unknown,
not as a distinct user.

## Promotion candidates

An exchange is eligible when it has a tool route and either records an
`untrusted` or `cited` answer (`candidate_basis = "provenance"`), or comes
from a trajectory with no provenance evidence and has at least two steps
(`"unclassified_source"`). Unclassified exchanges are counted in a route's
breakdown but never make it eligible. Candidates group eligible exchanges by
basis and signature and keep groups with at least `min_exchanges` (default
2).

Ranking is distinct trajectories (descending), eligible exchanges
(descending), `provenance` before `unclassified_source`, then signature in
C-locale order. There is no composite score.

Each candidate keeps the trajectory ids, one evidence row per eligible
exchange (`turn_ids`, and `event_ids` for its tool calls, results, and
provenance events), up to `max_snippets` recorded tool inputs and outputs
(`run_sql`, `run_r`, and `call_*` first, each cut to `max_chars` with the
full length kept), and a `resources` summary of `measure_trajectories()`
over its trajectories. Resource measures have trajectory grain: a token or
duration total covers every exchange in the trajectory, not only the
candidate route. `n_total`, `n_recorded`, and `n_complete` show coverage;
no per-exchange cost is derived.

## Review

A candidate's evidence row passes directly to `review_case()`:

```r
candidate <- route_candidates(bundle)[1, ]
evidence <- candidate$evidence[[1]][1, ]
case <- review_case(
  investigation_snapshot(bundle),
  evidence$trajectory_id,
  event_ids = evidence$event_ids[[1]],
  turn_ids = evidence$turn_ids[[1]]
)
case <- review_decision(
  case,
  "confirmed",
  "analyst",
  "new: no governed measure covers this. trajectory analysis (2026-11-25)",
  expected_behavior = "Answer with call_measure(\"revenue_share_by_region\")."
)
```

`review_examples()` and `review_dataset(format = "vitals")` then export the
case as a regression example with explicit inputs and targets, for a
before/after comparison once the measure exists.

The review contract has no structured slot for commons' triage labels
(`new`, `duplicate`, `extension`, `conflict`) or for the
`trajectory analysis (<yyyy-mm-dd>)` provenance; today they live in the
rationale text. A review case also covers one trajectory, so a candidate
spanning several trajectories needs one case per trajectory. Adding a
promotion-triage field would change the review contract and is left for a
separate decision.

## Dependencies and limits

The analysis depends on these canonical fields: turn `round_index`, `role`,
`turn_index`, and `metadata$commons_internal`; event `event_type`
(`tool_call`, `tool_result`, `commons:provenance`), `turn_id`, `name`,
`call_id`, `parent_event_id`, `text`, `value`, `status`, `error`,
`timestamp`, and `metadata$exchange_index`; provenance `value$provenance_tag`;
trajectory `metadata$otel$user`, `started_at`, and `completed_at`; and loss
field `provenance`. An OTel bundle that emits the same provenance events
works unchanged, provided its exchange indices agree with its turns: without
`round_index` or `commons_internal` markers, an injected reminder turn would
open its own exchange.

Results are linked to calls by `parent_event_id`, then by a call id that is
unique in the trajectory; ambiguous ids leave the result unlinked. Coverage
refers to loaded records; recent conversations are not a representative
sample of all usage.
