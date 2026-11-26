# Convert completed Deputy agent results and delegations

`as_trajectory_deputy()` snapshots one or more completed
[`deputy::AgentResult`](https://jameshwade.github.io/deputy/reference/AgentResult.html)
objects, and the disclosed delegation views of a
[`deputy::LeadAgent`](https://jameshwade.github.io/deputy/reference/LeadAgent.html),
into a canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
It delegates model-visible turns to
[`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md)
and adds ordered, namespaced Deputy lifecycle events. Results and events
are read through their public S7 properties.

## Usage

``` r
as_trajectory_deputy(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list(),
  delegations = NULL
)
```

## Arguments

- x:

  A completed
  [`deputy::AgentResult`](https://jameshwade.github.io/deputy/reference/AgentResult.html),
  a non-empty list of completed results, or a list of disclosed
  delegation views on its own.

- trajectory_id:

  A bundle-unique trajectory ID. When `NULL`, a stable ID is derived
  from the Deputy run ID (or, for a delegation that never ran, its
  delegation ID). It must be `NULL` when the input holds several runs.

- source_uri:

  An optional source locator. Credentials, query parameters, and
  fragments are removed.

- metadata:

  A uniquely named list of safe application metadata, applied to every
  trajectory. Sensitive fields are redacted and recorded as losses.

- delegations:

  Optional disclosed delegation views to convert beside the results in
  `x`, usually the lead result's own delegations. Use the value of
  [`deputy::delegation_history()`](https://jameshwade.github.io/deputy/reference/delegation_history.html)
  or of `lead$inspect_subagents(transcript = TRUE)`. A raw
  `$export_subagents()` snapshot is an error: read it with
  [`deputy::delegation_history()`](https://jameshwade.github.io/deputy/reference/delegation_history.html)
  first, so Deputy checks your `DelegationDisclosure` again.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

Run, session, agent, delegation, usage, ending, and immutable
run-context values are retained as sanitized data. When a run's parent
occurs in the same input, the adapter resolves `parent_trajectory_id`
from Deputy's lineage: the parent run ID, then, for a nested delegation,
the parent delegation ID. Otherwise it retains the parent IDs as
metadata, leaves `parent_trajectory_id` missing, and records an adapter
loss. Missing run identity is also explicit. Source objects, providers,
tools, callbacks, and credentials are never retained.

[`as_trajectory()`](https://jameshwade.github.io/scans/reference/as_trajectory.md)
dispatches to this adapter for Deputy results and non-empty lists
containing only Deputy results when Deputy is installed. Delegation
views are plain lists without a class of their own, so they need this
function.

## Run ending

The trajectory `status` follows the result's `RunEnding`:
`RunEndingAnswered` and `RunEndingCutOff` are `"completed"`,
`RunEndingStopped` and `RunEndingLimit` are `"interrupted"`, and
`RunEndingFailed` is `"failed"`. A cut-off answer keeps `"completed"`
because Deputy completed the run; the provider truncated its final
response. Check `metadata$ending$cut_off` whenever a whole answer
matters.

`trajectory_info(bundle)$metadata[[i]]$ending` always has these fields,
in this order, with `NA` where the ending does not supply one:

- `class` (character): `"RunEndingAnswered"`, `"RunEndingStopped"`,
  `"RunEndingLimit"`, `"RunEndingCutOff"`, or `"RunEndingFailed"`.

- `reason` (character): the stop, limit, or failure reason.

- `by` (character): who stopped a `RunEndingStopped` run, such as
  `"caller"`, `"hook"`, or `"abandoned"`.

- `field` (character), `actual` (double), `limit` (double): the
  `UsageLimits` field reached, the figure reached, and the figure
  allowed. `actual` is also `NA` when Deputy could not know it.

- `wrapped_up` (logical): for a limit, whether the model gave a final
  answer in the extra request allowed by `on_exceed = "wrap_up"`.

- `phase` (character): where a failed run failed.

- `finish_reason` (character): the provider's finish reason for the
  final response.

- `cut_off` (logical): whether the provider cut that response short. It
  is `TRUE` for every `RunEndingCutOff` and for a truncated wrap-up
  answer.

The failure condition itself is projected by the `deputy:run_error`
event.

## Events

Each
[`deputy::AgentEvent`](https://jameshwade.github.io/deputy/reference/AgentEvent.html)
becomes a `deputy:<type>` event, in source order. Fields that are not
projected into a column stay in the event's sanitized `metadata`, with
Deputy S7 values and conditions as plain lists.

- `tool_start`, `approval`: `value` is `tool_input`.

- `tool_end`: `value` is `tool_result`, and `error` is `tool_error`.

- `content`: `value` is `content`.

- `trusted_result`: `value` is the trusted value; `result_id`,
  `result_type`, and `tool_fingerprint` stay in `metadata`.

- `structured_attempt`: `value` is the attempted value; `valid`,
  `feedback`, and `attempt` stay in `metadata`.

- `request_error`, `run_error`, `compaction_error`: `error` is the
  condition message.

`status` is `"failed"` for `request_error`, `run_error`,
`compaction_error`, a `tool_end` with an error, a denied `permission`,
and an invalid `structured_attempt`; `"pending"` for an `approval`
awaiting a decision; and `"completed"` otherwise. `name` and `call_id`
carry `tool_name` and `tool_call_id`; `text` carries `text`, a `start`
task, or a `warning` message.

## Delegation views

Each view becomes one trajectory with `source_type = "deputy"`. Its
turns are the view's replayed ellmer `turns`; a view has no lifecycle
events, timestamps, or run context. `status` follows
`outcome$runtime$status`: `"completed"` and `"failed"` keep their names,
`"stopped"` and `"suspended"` are `"interrupted"`, and `"not_started"`
is `"cancelled"`. As with results, `"completed"` means the run ended
normally, not that the task succeeded. `metadata$ending` has the ten
fields above, with only `finish_reason` and `cut_off` set.

The view's metadata keeps the result keys `session_id`, `agent_id`,
`parent_agent_id`, `parent_run_id`, `delegation_id`, `usage` (the
delegation's `AgentUsage` fields), and `stop_reason`, adds
`parent_delegation_id`, and sets `source_class` to `"delegation_view"`.
`metadata$delegation` holds:

- `task` (character): the delegated task label.

- `answer` (character): the subagent's final reply, which Deputy cuts to
  8 KiB.

- `claims$missing_evidence`, `claims$unresolved_work` (each
  `character(1)`): what the subagent reported in its structured output,
  entries joined by newlines. `""` means it reported none; `NA` means it
  did not report, not that nothing is missing.

- `references` (list): one record per saved artifact, with `reference`,
  `source` (`"tool_result"` or `"delegation_answer"`), `tool_call_id`,
  and `availability`: `"available"` or `"missing"` from a live
  `$inspect_subagents()` view, or `"unresolved"` from
  `delegation_history()`, whose saved status may be stale. References
  are never read.

- `manifest` (list or `NULL`): the `DelegationManifest` fields
  `schema_version`, `definition_id`, `provider`, `model`, `params`,
  `tools`, `policies`, and `size`. The manifest's task, evidence,
  prompt, and message text are not copied.

- `runtime` (list): Deputy's whole runtime record, including `depth`,
  `root_agent_id`, `answer_truncated`, and `claim_omissions`.

- `cumulative_usage`, `errors`, and `retention`, as in the view.

## Delegation losses

- `capture$transcript` is `"redacted"` when disclosure withheld a
  delegation's transcript, so its turns and tool activity are absent,
  and `"unsupported"` when the view was read without one.

- `contents$value` is `"externalized"` on each tool result, in a result
  or a view, that Deputy moved out of the model context: the event holds
  only a preview, and the loss metadata holds its `reference`.

- `metadata$delegation$answer` and `metadata$delegation$claims$<name>`
  are `"truncated"` when Deputy bounded them.

- `parent_run_id` or `parent_delegation_id` is `"unsupported"` when the
  parent is not in the same call.

## Examples

``` r
if (
  rlang::is_installed("deputy", version = "0.0.0.9007") &&
    rlang::is_installed("ellmer", version = "0.5.0")
) {
  result <- deputy::AgentResult(
    turns = list(
      ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
      ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
    ),
    run_id = "run-001",
    agent_name = "assistant"
  )

  bundle <- as_trajectory_deputy(result)
  trajectory_events(bundle)
  trajectory_info(bundle)$metadata[[1]]$ending

  # With a LeadAgent `lead`, convert its delegations beside its result:
  # history <- deputy::delegation_history(
  #   lead$export_subagents("reviewer"), "reviewer", disclosure, scope
  # )
  # as_trajectory_deputy(result, delegations = history)
}
#> $class
#> [1] "RunEndingAnswered"
#> 
#> $reason
#> [1] NA
#> 
#> $by
#> [1] NA
#> 
#> $field
#> [1] NA
#> 
#> $actual
#> [1] NA
#> 
#> $limit
#> [1] NA
#> 
#> $wrapped_up
#> [1] NA
#> 
#> $phase
#> [1] NA
#> 
#> $finish_reason
#> [1] NA
#> 
#> $cut_off
#> [1] FALSE
#> 
```
