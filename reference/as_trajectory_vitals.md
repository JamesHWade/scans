# Convert vitals evaluation samples

`as_trajectory_vitals()` snapshots completed solver trajectories and
their evaluation outcomes from [vitals](https://vitals.tidyverse.org/).
It accepts a vitals `Task` or the sample data frame returned by
`Task$get_samples()`. It also accepts the data frame returned by the
development
[`vitals::vitals_log_read()`](https://vitals.tidyverse.org/reference/vitals_log_read.html)
interface.

## Usage

``` r
as_trajectory_vitals(
  x,
  run_id = NULL,
  task_id = NULL,
  source_uri = NULL,
  include_system_prompt = TRUE
)
```

## Arguments

- x:

  A vitals `Task`, or a data frame from `Task$get_samples()` or
  [`vitals::vitals_log_read()`](https://vitals.tidyverse.org/reference/vitals_log_read.html).
  Sample data must contain unique `(id, epoch)` pairs and an ellmer
  `Chat` in `solver_chat` for every row. When `epoch` is absent, it
  defaults to `1L`.

- run_id, task_id:

  Optional correlation identities applied to every trajectory.

- source_uri:

  An optional source locator, such as the path to a vitals eval log.
  Credentials, query parameters, and fragments are removed.

- include_system_prompt:

  Whether solver system prompts should be included as system turns.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

Each `(id, epoch)` sample becomes one trajectory. Its `solver_chat` is
converted through the ellmer adapter, and a non-missing `score` becomes
one evaluation row. Sample input, result, solver metadata, and custom
columns are retained as sanitized trajectory metadata. Scorer metadata
is retained on the evaluation. A `scorer_chat`, when present, is
recorded as an explicit adapter loss because schema version 1 represents
the evaluated solver path, not the evaluator's separate model
trajectory.

The adapter only uses the public `Task$get_samples()` method. It does
not retain the live task, chats, providers, tools, callbacks, or
credentials.

## Examples

``` r
if (
  rlang::is_installed("ellmer", version = "0.4.2") &&
    rlang::is_installed("vitals", version = "0.3.0")
) {
  task <- vitals::Task$new(
    data.frame(input = "Say hello", target = "Hi"),
    solver = function(inputs) {
      chat <- ellmer::chat_openai(
        model = "gpt-4o-mini",
        credentials = function() "not-used"
      )
      chat$set_turns(list(
        ellmer::UserTurn(list(ellmer::ContentText(inputs[[1L]]))),
        ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
      ))
      list(result = "Hi", solver_chat = list(chat))
    },
    scorer = function(samples) list(score = "C"),
    dir = tempdir()
  )
  task$solve()
  task$score()

  bundle <- as_trajectory_vitals(task)
  trajectory_evaluations(bundle)
}
#> # A tibble: 1 × 10
#>   trajectory_id        evaluation_id task_id sample_id epoch scorer value target
#>   <chr>                <chr>         <chr>   <chr>     <int> <chr>  <lis> <list>
#> 1 vitals/1/epoch-0000… vitals/1/epo… NA      1             1 "func… <chr> <chr> 
#> # ℹ 2 more variables: explanation <chr>, metadata <list>
```
