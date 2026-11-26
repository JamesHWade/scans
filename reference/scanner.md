# Define a custom deterministic scanner

`scanner()` wraps a plain function and the facts the assessment contract
needs: a name, a version, the evidence it requires, and its settings.
Pass scanners to
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md),
[`assess_trajectory_scans()`](https://jameshwade.github.io/scans/reference/assess_trajectory_scans.md),
[`investigation_snapshot()`](https://jameshwade.github.io/scans/reference/investigation_snapshot.md),
[`scans_tools()`](https://jameshwade.github.io/scans/reference/scans_tools.md),
or
[`scans_app()`](https://jameshwade.github.io/scans/reference/scans_app.md)
through their `scanners` argument. Nothing is registered globally: a
scanner exists only for the calls it is passed to.

## Usage

``` r
scanner(
  name,
  fn,
  description,
  required_evidence,
  version = "1",
  severity = "warning",
  settings = list()
)

scanner_findings(
  event_id = NA_character_,
  label = character(),
  explanation = character(),
  turn_id = NA_character_,
  event_ids = NULL,
  value = NULL
)

scanner_unassessed(status, reason)
```

## Arguments

- name:

  One scanner name: lower-case letters, digits, and underscores,
  starting with a letter. It must not be the name of a built-in scan in
  [`scan_registry()`](https://jameshwade.github.io/scans/reference/scan_registry.md).

- fn:

  A function of `trajectory` and `settings`. It must be deterministic
  and must not call a model or tool.

- description:

  One sentence describing what a finding means.

- required_evidence:

  One sentence naming the recorded evidence the scanner needs before it
  can assess a trajectory.

- version:

  One non-empty version string. Change it whenever `fn` or its meaning
  changes; it is recorded with every finding and assessment.

- severity:

  `"warning"` or `"error"`, applied to every finding.

- settings:

  A named list of data passed to `fn` and recorded with each assessment,
  such as tool names or phrases the application owner chose. It must
  hold only data a saved investigation can store: no functions,
  environments, or objects of other classes.

- event_id, turn_id:

  Primary evidence identities for each finding, from the scanned
  trajectory. `NA` means the finding has none. A finding with an event
  and no turn takes the event's turn; one with both must name the
  event's own turn.

- label, explanation:

  A short label and a one-sentence explanation for each finding. All
  arguments are recycled to a common size with the vctrs rules: only
  length one recycles, and other mismatches are an error.

- event_ids:

  Optional list with every supporting event identifier for each finding.
  The primary event is always included.

- value:

  Optional list of scan-specific structured details, one per finding.
  Like `settings`, it must hold only data a saved investigation can
  store; otherwise the result is an execution failure.

- status:

  `"insufficient_evidence"` or `"not_applicable"`.

- reason:

  One sentence, at most 500 characters, saying what is missing or why
  the scan does not apply. It is shown with the assessment.

## Value

`scanner()` returns a `scans_scanner` value.

`scanner_findings()` returns a tibble of findings of class
`scans_scanner_findings`; call it with no arguments for a clean
negative. `scanner_unassessed()` returns a value that marks the
trajectory as not assessed.

## Details

`fn` is called once per trajectory as `fn(trajectory, settings)`.
`trajectory` is a list holding one trajectory's rows from the bundle
accessors: `trajectory_id`, `info` (one row of
[`trajectory_info()`](https://jameshwade.github.io/scans/reference/trajectory_accessors.md)),
`turns` (in turn order), `events` (in event order), `evaluations`, and
`losses` (the
[`trajectory_losses()`](https://jameshwade.github.io/scans/reference/trajectory_accessors.md)
rows assigned to this trajectory or to no trajectory). It must return
one of:

- `scanner_findings()`: the scanner assessed the trajectory. Return
  `scanner_findings()` with no arguments to say "assessed, no findings"
  (`assessed_no_findings`).

- `scanner_unassessed()`: the scanner could not assess the trajectory,
  because the evidence it needs is not recorded
  (`"insufficient_evidence"`) or the scan does not apply to this capture
  (`"not_applicable"`).

Anything else, including `NULL` and a data frame not made by
`scanner_findings()` (even an empty one), is an execution failure rather
than a clean negative, so a scanner that forgets to return cannot report
"no findings". Rows can be dropped from a `scanner_findings()` result
with `[`. The scanner decides whether recorded losses prevent an
assessment; the framework lists them in the assessment's limitations.

The framework checks every finding. A turn or event reference that is
not in the scanned trajectory, a primary event outside the finding's
turn, or a malformed finding, makes the whole result an
`execution_failure` for that scanner and trajectory, and none of its
findings are kept. A scanner that throws an error, or raises any other
condition with [`stop()`](https://rdrr.io/r/base/stop.html), is an
`execution_failure` for that scanner and trajectory only. Warnings and
messages are not failures and pass through.
[`assess_trajectory_scans()`](https://jameshwade.github.io/scans/reference/assess_trajectory_scans.md)
records a fixed reason and never copies the condition message;
[`scan_trajectories()`](https://jameshwade.github.io/scans/reference/scan_trajectories.md)
re-signals the failure with the original condition as its parent.

Finding identifiers are `<scan_id>/<name>/finding-<n>`, numbered per
scanner in bundle trajectory order, then by the primary event, then in
the order returned. They do not change when other scanners are added,
removed, or deselected, and built-in identifiers never change because a
custom scanner ran.

## Examples

``` r
no_apology <- scanner(
  "apology_without_action",
  function(trajectory, settings) {
    events <- trajectory$events
    roles <- trajectory$turns$role[
      match(events$turn_id, trajectory$turns$turn_id)
    ]
    answers <- events[roles %in% "assistant" & events$event_type == "content", ]
    if (nrow(answers) == 0L) {
      return(scanner_unassessed(
        "insufficient_evidence",
        "No assistant text is recorded."
      ))
    }
    hits <- answers[grepl(settings$phrase, answers$text, ignore.case = TRUE), ]
    scanner_findings(
      event_id = hits$event_id,
      label = rep("Apology", nrow(hits)),
      explanation = rep("The assistant apologised.", nrow(hits))
    )
  },
  description = "The assistant apologised.",
  required_evidence = "Recorded assistant text.",
  settings = list(phrase = "sorry")
)

bundle <- TrajectoryBundle(
  data.frame(trajectory_id = "run-1", source_type = "manual"),
  data.frame(trajectory_id = "run-1", turn_id = "turn-1", turn_index = 1L,
             role = "assistant"),
  data.frame(trajectory_id = "run-1", event_id = "event-1", event_index = 1L,
             turn_id = "turn-1", event_type = "content",
             text = "Sorry about that.")
)
result <- assess_trajectory_scans(
  bundle,
  scans = "apology_without_action",
  scanners = list(no_apology)
)
result$assessments[c("scan", "scan_version", "status")]
#> # A tibble: 1 × 3
#>   scan                   scan_version status                
#>   <chr>                  <chr>        <chr>                 
#> 1 apology_without_action 1            assessed_with_findings
result$findings[c("finding_id", "event_id", "label")]
#> # A tibble: 1 × 3
#>   finding_id                                        event_id label  
#>   <chr>                                             <chr>    <chr>  
#> 1 scan-000001/apology_without_action/finding-000001 event-1  Apology
```
