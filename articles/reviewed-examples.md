# Turn a review into an improvement example

A reviewer may decide that a retry finding is valid, then ask the
application to handle the timeout differently. Those are two separate
targets: the scanner should detect the pattern, and a future application
execution should produce a better response. Scans preserves the evidence
and judgment for both uses. vitals runs and scores new outputs; dsprrr
optimizes a program against the selected application examples.

## Record a decision against exact evidence

This offline investigation contains retries, concurrent work, and
incomplete capture. Freeze it before reviewing so scanner settings and
evidence identities remain available when the source changes.

``` r

path <- system.file("extdata", "support-investigation.json", package = "scans")
saved <- investigation_snapshot(
  as_trajectory_otel(jsonlite::read_json(path)),
  application = "Order support"
)
findings <- saved$analysis$findings
finding <- findings$finding_id[findings$scan == "suspicious_tool_loop"][[1L]]
retry <- review_case(saved, "otel/retry", finding_id = finding)
retry <- review_decision(
  retry, "confirmed", reviewer = "Alex",
  rationale = "The same failed call was repeated without new information.",
  expected_behavior = "Bound retries and explain that the service is unavailable."
)
review_status(retry)
#> $status
#> [1] "confirmed"
#> 
#> $decision_ids
#> [1] "sha256:4220f1547ec9dc5ecc30d0376977650749d727fc064bc1ca47549d8690a2bba3"
```

The case contains only the selected trajectory and relevant losses,
alongside the original snapshot and revision IDs. Its copied analysis
retains the scanner version and settings. Capture-wide metadata and
unassigned losses remain part of the evidence; review those too before
sharing.

A second review appends to the history. It does not silently replace the
first:

``` r

retry <- review_decision(
  retry, "uncertain", reviewer = "Sam",
  rationale = "I need to check whether a retry changed its arguments."
)
review_status(retry)$status
#> [1] "disputed"
retry <- review_decision(
  retry, "confirmed", reviewer = "Alex",
  rationale = "Checked the referenced calls: the arguments are unchanged.",
  expected_behavior = "Bound retries and explain that the service is unavailable.",
  supersedes = review_status(retry)$decision_ids
)
review_status(retry)$status
#> [1] "confirmed"
```

`supersedes` explicitly adjudicates the listed active decisions.
Unreviewed, disputed, uncertain, and insufficient-capture cases can be
retained in example files, but cannot become application targets.
Scanner validation reports their result as unknown. Reviewer names and
hashes preserve attribution labels and integrity; they do not
authenticate the author.

## Select scanner-validation cases

Review unflagged trajectories as well as findings. A `"missed"` judgment
records a pattern the scanner did not find. A `"rejected"` judgment can
identify a false positive or a whole-trajectory negative case. Supply
the scanner name when no finding is selected.

``` r

parallel <- review_case(saved, "otel/parallel", scan = "suspicious_tool_loop")
parallel <- review_decision(
  parallel, "rejected", reviewer = "Alex",
  rationale = "These are distinct concurrent operations, not a retry loop."
)
scanner_set <- review_examples(
  list(retry, parallel), purpose = "scanner",
  groups = c("retry-task", "parallel-task"),
  splits = c("development", "test"),
  predicates = list(
    list(operator = "gte", value = 1L),
    list(operator = "eq", value = 0L)
  )
)
validate_review_examples(scanner_set, split = "test")[
  c("judgment", "assessment", "count", "passed")
]
#> # A tibble: 1 × 4
#>   judgment assessment           count passed
#>   <chr>    <chr>                <int> <lgl> 
#> 1 rejected assessed_no_findings     0 TRUE
```

Predicates check finding counts for the saved scanner. A case with event
references counts only findings intersecting those events; turn
references further restrict the count. Validation runs the installed
deterministic scanner with the saved thresholds and records the current
scanner version. It leaves saved findings and decisions intact.
Insufficient capture, inapplicability, and scanner execution failure
produce `NA`, even when no finding was returned.

Group repeated executions and revisions of the same task together. Scans
rejects a group or repeated application/trajectory identity that crosses
splits, including when creating a new version with `previous`. You must
supply the grouping for related tasks with different source IDs; Scans
cannot infer semantic relatedness from arbitrary transcripts.

## Map an application example explicitly

An arbitrary conversation is not necessarily a runnable application
input. Choose the input fields your solver accepts and the target fields
your scorer needs. A rationale about an old output is not a reusable
score.

``` r

application_set <- review_examples(
  list(retry), purpose = "application",
  groups = "retry-task", splits = "development",
  inputs = list(list(question = "Track order A after the order service times out")),
  targets = list(list(answer = "Service unavailable"))
)
training <- review_dataset(application_set, format = "dsprrr", split = "development")
training
#> # A tibble: 1 × 2
#>   question                                        answer             
#>   <chr>                                           <chr>              
#> 1 Track order A after the order service times out Service unavailable
attr(training, "scans_provenance")[c("group", "expected_behavior")]
#> # A tibble: 1 × 2
#>   group      expected_behavior                                         
#>   <chr>      <chr>                                                     
#> 1 retry-task Bound retries and explain that the service is unavailable.
```

The dsprrr dataset contains only the explicitly mapped input and target
columns. The vitals format contains `id`, named `input` and `target`
lists, and metadata; use a solver and scorer that understand those
fields. Both carry case, decision, set-version, split, and origin
provenance. Expected behavior stays in that provenance unless you
explicitly map it to a target field for your scorer.

Inspect the content before exporting. Selection includes the selected
case’s full decision history and retained evidence, not just its latest
judgment.

``` r

application_set$content_policy
#> $id
#> [1] "selected-review-v1"
#> 
#> $included
#> [1] "Selected trajectory evidence, relevant losses, complete selected case histories, explicit mappings, and split identity hashes."
#> 
#> $excluded
#> [1] "Unselected trajectories and review cases. No live producers or credentials are added."
#> 
#> $redaction
#> [1] "Existing redactions remain. No additional anonymization is performed."
file <- tempfile(fileext = ".json")
write_review_examples(application_set, file)
reopened <- read_review_examples(file)
identical(reopened$version_id, application_set$version_id)
#> [1] TRUE
```

Use `previous = reopened` when constructing the next set version. Its
identifier covers content and history, and prior split assignments
remain reserved even if a case is omitted from the new version. Export
does not redact retained content further or automatically select any
other production data.

## Run the complete offline workflow

The installed recipe creates original executions through a public dsprrr
module, records reviews, exports and reopens two selected application
cases, then runs GEPA with a score-plus-feedback metric on the
development case. A vitals Task evaluates the held-out case through
dsprrr’s public solver adapter. The resulting Task is imported back into
Scans and saved for inspection.

``` r

source(system.file("examples", "reviewed-improvement.R", package = "scans"))
result <- run_reviewed_improvement()
result$task$get_samples()
scans_app(result$traces)
```

All provider responses in the default recipe are mocked. The fake
provider returns a fixed response, so fixture scores demonstrate the
data flow, not prompt improvement. Automated tests run the public
producers, optimizer, Task, and Scans adapters without API keys or
network access.

For a real run, supply your own ellmer chat and explicit
[`dsprrr::optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.html)
limits via the recipe’s `chat` and `control` arguments. Set
provider-call, metric-call, token, cost, or elapsed-time caps to suit
your budget. dsprrr owns enforcement, including its documented
postflight overshoot and handling of unknown usage. Those controls cover
optimization; the recipe then runs one held-out solver case. Configure
your chat’s response limits for that evaluation too. Re-evaluate a
baseline and candidate on the same held-out cases before concluding that
a change helped. General matched-comparison UI remains a separate
feature.

Manual application changes use the same regression cases: replace the
optimized program in the recipe’s held-out Task with the revised
application solver. Always generate fresh outputs after changing an
application or prompt. Re-scoring retained outputs answers a different
question about the scorer.

## Use optional app controls

``` r

scans_app(saved, reviews = TRUE)
# Or enable them on a hosted app:
# scans_app_connect(c("Support" = "content-guid"), reviews = TRUE)
```

In the Trajectory view, open **Review and create example**, choose a
finding or whole trajectory, and record a reviewer, judgment, rationale,
and expected behavior. Independent decisions remain visible; select
**Adjudicate all active decisions** when resolving them. Choose a task
group, split, purpose, and explicit input/target or count predicate,
then **Select and preview example** before downloading.

The app exports one selected case per file. Its text-field mapper
creates one input and one target field; use the R API for multi-field
mappings and sets of many cases. **Open reviewed examples** accepts
either kind of set and lets you choose a case to inspect. Exporting an
imported case creates a new single-case version and retains the imported
set’s split history. Reviews are session-local; download before closing
the app. Opening an older case preserves newer compatible session
decisions. Divergent histories are rejected without replacing either;
keep both files and inspect them in separate sessions. Existing
annotations remain a separate feature.

## Design references

The separation between feedback, expectations, and optimization follows
the [MLflow assessment
model](https://mlflow.org/docs/latest/genai/assessments/feedback/).
Explicit predicates, validation splits, and review of unflagged cases
draw on [Inspect Scout
validation](https://meridianlabs-ai.github.io/inspect_scout/validation.html).
The distinction between re-scoring stored outputs and generating fresh
outputs follows [Inspect’s scoring
workflow](https://inspect.aisi.org.uk/scoring-workflow.html). These are
design references, not runtime dependencies.
