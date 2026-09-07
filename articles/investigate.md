# Investigate a support assistant offline

An order lookup keeps failing. Another conversation takes two minutes.
Which deserves attention first? This example takes you from an
application snapshot to the supporting events, then shows what the
capture cannot tell you.

Everything runs locally without credentials or provider calls. The four
conversations are authored examples, not production records. They
include repeated message histories, overlapping tools, a delayed
follow-up, and a conversation with content capture disabled and one
missing end timestamp.

## Load the snapshot

Install the package and the small JSON reader once:

``` r

pak::pak(c("JamesHWade/scans", "jsonlite"))
```

``` r

library(scans)
path <- system.file("extdata", "support-investigation.json", package = "scans")
bundle <- as_trajectory_otel(jsonlite::read_json(path))
trajectory_info(bundle)[, c("trajectory_id", "model")]
#> # A tibble: 4 × 2
#>   trajectory_id        model                
#>   <chr>                <chr>                
#> 1 otel/retry           example-support-model
#> 2 otel/follow-up       example-support-model
#> 3 otel/parallel        example-support-model
#> 4 otel/limited-capture NA
```

The fixture’s source is `data-raw/investigation-fixture.R` in the
repository. The installed JSON is sufficient to repeat every calculation
below.

## Find a pattern worth investigating

``` r

findings <- scan_trajectories(bundle)
findings[, c("trajectory_id", "scan", "label")]
#> # A tibble: 5 × 3
#>   trajectory_id scan                 label               
#>   <chr>         <chr>                <chr>               
#> 1 otel/retry    repeated_tool_call   Repeated tool call  
#> 2 otel/retry    suspicious_tool_loop Suspicious tool loop
#> 3 otel/retry    event_error          Event error         
#> 4 otel/retry    event_error          Event error         
#> 5 otel/retry    event_error          Event error
```

Only `otel/retry` is flagged. It has three tool errors, a repeated
request, and a suspicious loop. Those five findings describe **one
affected conversation**, not five failing conversations. They identify
behavior to review; they do not establish that the final answer was
wrong.

Inspect the events that support the loop finding:

``` r

loop <- findings[findings$scan == "suspicious_tool_loop", ]
events <- trajectory_events(bundle)
evidence <- events[events$event_id %in% loop$event_ids[[1]], ]
evidence[, c("event_id", "event_type", "name", "duration", "error")]
#> # A tibble: 3 × 5
#>   event_id                event_type name         duration error                
#>   <chr>                   <chr>      <chr>           <dbl> <chr>                
#> 1 otel/retry/event-000002 tool_call  lookup_order        5 order_service_timeout
#> 2 otel/retry/event-000004 tool_call  lookup_order        6 order_service_timeout
#> 3 otel/retry/event-000006 tool_call  lookup_order        9 order_service_timeout
```

All three calls ask `lookup_order` for the same order. Each ends with
`order_service_timeout`. The transcript’s final answer acknowledges that
it could not retrieve the shipment. A useful engineering question is
whether the workflow should stop repeating that request or use a
recovery route. This record alone does not tell us whether a retry
policy was intentional, or whether the order service was failing for
other users.

## Distinguish no findings from missing evidence

``` r

assessment <- assess_trajectory_scans(bundle)
coverage <- assessment$assessments
coverage[coverage$scan == "repeated_tool_call",
         c("trajectory_id", "scan", "status", "reason")]
#> # A tibble: 4 × 4
#>   trajectory_id        scan               status                 reason         
#>   <chr>                <chr>              <chr>                  <chr>          
#> 1 otel/retry           repeated_tool_call assessed_with_findings Findings are s…
#> 2 otel/follow-up       repeated_tool_call not_applicable         The retained t…
#> 3 otel/parallel        repeated_tool_call assessed_no_findings   Required evide…
#> 4 otel/limited-capture repeated_tool_call insufficient_evidence  No tool exchan…
```

The retry conversation has assessed repetition findings. A contentless
capture cannot support that comparison, even if its spans record elapsed
time and token counts. A captured text conversation with no tool
activity instead reports `not_applicable`. These states must not be
counted as assessed negative results.

`assessment$findings` retains the evidence identities used above. The
assessment rows also carry `scan_version`, `settings`,
`required_evidence`, `finding_ids`, `limitations`, and `loss_rows` (row
references into `trajectory_losses(bundle)`). The app shows the same
information in **Scanner assessments**, and counts each state separately
for the selected scanners and filtered trajectories in **Scanner
coverage**. An execution failure remains visible there while the
unaffected detector groups continue. Conclusions always concern retained
records.

## Separate waiting from recorded work

``` r

measures <- measure_trajectories(bundle)
timing <- measures[measures$measure %in% c(
  "elapsed", "model_duration", "tool_duration",
  "recorded_work", "unattributed_elapsed"
), ]
timing[, c("trajectory_id", "measure", "value", "n_recorded", "n_total", "coverage")]
#> # A tibble: 20 × 6
#>    trajectory_id        measure              value n_recorded n_total coverage  
#>    <chr>                <chr>                <dbl>      <int>   <int> <chr>     
#>  1 otel/retry           elapsed                 33          7       7 complete  
#>  2 otel/retry           model_duration          13          4       4 complete  
#>  3 otel/retry           tool_duration           20          3       3 complete  
#>  4 otel/retry           recorded_work           33          7       7 complete  
#>  5 otel/retry           unattributed_elapsed     0          7       7 complete  
#>  6 otel/follow-up       elapsed                126          2       2 complete  
#>  7 otel/follow-up       model_duration           6          2       2 complete  
#>  8 otel/follow-up       tool_duration           NA          0       0 unavailab…
#>  9 otel/follow-up       recorded_work            6          2       2 complete  
#> 10 otel/follow-up       unattributed_elapsed   120          2       2 complete  
#> 11 otel/parallel        elapsed                 10          4       4 complete  
#> 12 otel/parallel        model_duration           4          2       2 complete  
#> 13 otel/parallel        tool_duration           10          2       2 complete  
#> 14 otel/parallel        recorded_work           10          4       4 complete  
#> 15 otel/parallel        unattributed_elapsed     0          4       4 complete  
#> 16 otel/limited-capture elapsed                 NA          1       2 unavailab…
#> 17 otel/limited-capture model_duration           4          1       2 partial   
#> 18 otel/limited-capture tool_duration           NA          0       0 unavailab…
#> 19 otel/limited-capture recorded_work            4          1       2 partial   
#> 20 otel/limited-capture unattributed_elapsed    NA          1       2 unavailab…
```

The follow-up conversation spans **126 seconds**, with **6 seconds** of
recorded model work and **120 seconds** outside recorded work. Its
elapsed time does not establish a slow model. The parallel example
records **4 seconds** in model calls and **10 seconds** in tools, but
**10 seconds** of work after overlapping intervals are counted once.
Adding its inclusive durations would wrongly claim 14 seconds of wall
time.

The limited capture has one valid four-second interval and one span
without an end timestamp. Recorded work is therefore partial. Its
elapsed time and time outside recorded work are unavailable. No
transcript findings on that conversation do not establish a successful
or well-behaved interaction.

## Check the recording denominator

``` r

usage <- measures[measures$measure %in% c("input_tokens", "output_tokens"), ]
usage[, c("trajectory_id", "measure", "value", "n_recorded", "n_total", "coverage")]
#> # A tibble: 8 × 6
#>   trajectory_id        measure       value n_recorded n_total coverage   
#>   <chr>                <chr>         <dbl>      <int>   <int> <chr>      
#> 1 otel/retry           input_tokens   1000          4       4 complete   
#> 2 otel/retry           output_tokens    95          4       4 complete   
#> 3 otel/follow-up       input_tokens    240          2       2 complete   
#> 4 otel/follow-up       output_tokens    55          2       2 complete   
#> 5 otel/parallel        input_tokens    350          2       2 complete   
#> 6 otel/parallel        output_tokens    75          2       2 complete   
#> 7 otel/limited-capture input_tokens     75          1       2 partial    
#> 8 otel/limited-capture output_tokens    NA          0       2 unavailable
```

The retry conversation consumed 1,000 recorded input tokens across four
model calls. Its reconstructed final assistant turn carries only the
latest 400 input tokens.
[`summarize_trajectories()`](https://jameshwade.github.io/scans/reference/summarize_trajectories.md)
keeps its original turn-based usage sums; use
[`measure_trajectories()`](https://jameshwade.github.io/scans/reference/measure_trajectories.md)
for the source-aware resource view shared with the app. These OTel input
counts include cache reads. The new interface also combines ellmer’s
separate uncached and cached input per turn, requiring both components
to be recorded. `includes_cached_input` marks these inclusive totals.
Other sources can retain unknown cache inclusion, so inspect the source
before combining input and cached-input counts.

`complete` means every eligible source record in the **loaded snapshot**
supplied a valid value. Depending on the measure, these records are
spans, turns, tool-call events, or a pair of trajectory bounds. It
cannot prove that a source recorded every call, retained every trace, or
loaded the whole time window. The limited capture supplies input usage
for one of two calls and no output usage. Unknown usage is never
converted to zero.

``` r

measures[measures$trajectory_id == "otel/retry" &
  measures$measure %in% c("first_chunk_latency", "first_token_latency", "cost"),
  c("measure", "value", "coverage", "limitation")]
#> # A tibble: 3 × 4
#>   measure             value coverage    limitation                              
#>   <chr>               <dbl> <chr>       <chr>                                   
#> 1 first_chunk_latency    NA unavailable Median time to first chunk; a chunk may…
#> 2 first_token_latency    NA unavailable No supported per-request first-token me…
#> 3 cost                   NA unavailable Raw costs lack currency, price basis, a…
```

This fixture has no first-chunk measurement. Where an OTel producer
records `gen_ai.response.time_to_first_chunk`, scans reports its median
in seconds. A chunk need not contain a token, so it is not labelled
first-token latency. Comparable cost is unavailable without currency, a
price basis, and known inclusion of child work. The old raw cost column
remains available for source-specific analysis.

## Open the same investigation in the app

Install the optional UI dependencies once, then open the same bundle:

``` r

pak::pak(c("shiny", "bslib", "htmltools", "commonmark", "xml2"))
scans_app(list("Support assistant example" = bundle), investigations = TRUE)
```

The application overview uses these measurements. Select **suspicious
tool loop**, then open `otel/retry`. The transcript and findings retain
the event identifiers shown above. Expand **Resource measurements** to
inspect each quantity, recording denominator, source, and limitation.
Clear the pattern filter to inspect the other three conversations.
Filter changes reuse the loaded snapshot.

The overview’s recorded-work medians exclude trajectories with partial
timing; individual resource details still show their known intervals.
Elapsed medians exclude unavailable bounds. Recorded token medians can
include partial sums, so inspect coverage before interpreting a
difference as an improvement.

## Save and reopen the evidence

Install the optional file-format dependencies once:

``` r

pak::pak(c("jsonlite", "digest"))
```

Save the retry conversation alongside the incomplete capture. Their
findings and scanner assessments travel with the evidence, so the
incomplete capture remains distinguishable from an assessed conversation
with no findings.

``` r

saved <- investigation_snapshot(
  bundle,
  trajectory_ids = c("otel/retry", "otel/limited-capture"),
  application = "Support assistant example",
  source = list(kind = "bundled example", incomplete = TRUE),
  view = list(selected_trajectory_id = "otel/retry", tab = "trajectory")
)
path <- tempfile(fileext = ".scans.json")
write_investigation(saved, path)
reopened <- read_investigation(path)
stopifnot(identical(saved, reopened))
reopened$analysis$assessments[c("trajectory_id", "scan", "status")]
#> # A tibble: 18 × 3
#>    trajectory_id        scan                       status                
#>    <chr>                <chr>                      <chr>                 
#>  1 otel/retry           ambiguous_tool_correlation assessed_no_findings  
#>  2 otel/retry           unresolved_tool_call       assessed_no_findings  
#>  3 otel/retry           unmatched_tool_result      assessed_no_findings  
#>  4 otel/retry           repeated_tool_call         assessed_with_findings
#>  5 otel/retry           suspicious_tool_loop       assessed_with_findings
#>  6 otel/retry           event_error                assessed_with_findings
#>  7 otel/retry           error_chain                insufficient_evidence 
#>  8 otel/retry           turn_error                 assessed_no_findings  
#>  9 otel/retry           trajectory_error           assessed_no_findings  
#> 10 otel/limited-capture ambiguous_tool_correlation insufficient_evidence 
#> 11 otel/limited-capture unresolved_tool_call       insufficient_evidence 
#> 12 otel/limited-capture unmatched_tool_result      insufficient_evidence 
#> 13 otel/limited-capture repeated_tool_call         insufficient_evidence 
#> 14 otel/limited-capture suspicious_tool_loop       insufficient_evidence 
#> 15 otel/limited-capture event_error                insufficient_evidence 
#> 16 otel/limited-capture error_chain                insufficient_evidence 
#> 17 otel/limited-capture turn_error                 insufficient_evidence 
#> 18 otel/limited-capture trajectory_error           assessed_no_findings
reopened$manifest$content_policy
#> $id
#> [1] "retained-v1"
#> 
#> $included
#> [1] "All retained fields of selected trajectories, their turns, events, evaluations, and relevant losses."
#> 
#> $excluded
#> [1] "Unselected trajectory records, annotation history, loader credentials, and live connections."
#> 
#> $redaction
#> [1] "Existing adapter redactions are preserved. No additional anonymization is performed."
#> 
#> $omitted_trajectories
#> [1] 2
```

``` r

scans_app(reopened)
```

In the app, **Save or open** offers the same workflow for the visible
selection. The preview names its content policy before downloading:
selected retained text, tool arguments and results, evaluations,
metadata, and capture limits. Existing redactions remain, but the file
performs no additional anonymization. Annotation history and unselected
records are excluded. Review the selected content before sharing it.

Close the app and reopen the file with the code above, or upload it
through **Open an investigation** in a new app session. Confirm that the
retry evidence and incomplete-capture assessments are still present. The
saved query, filters, ordering, focused trajectory, and scanner settings
are restored. The upload belongs to that session and leaves other
reviewers’ source snapshots alone.

Opening uses the saved diagnostic results. Changing scanner controls
does not replace them until you choose **Apply current scanners**.
Saving that analysis creates a revision with the earlier revision as its
parent. From R:

``` r

revised <- investigation_snapshot(
  reopened$bundle,
  application = reopened$manifest$application,
  source = reopened$manifest$source,
  repeat_threshold = 4L,
  previous = reopened
)
stopifnot(
  identical(revised$manifest$snapshot_id, reopened$manifest$snapshot_id),
  identical(revised$manifest$parent_revision_id, reopened$manifest$revision_id)
)
```

Snapshot and revision identifiers detect changed content; they do not
certify author identity. Unsupported file versions, altered identifiers,
and invalid evidence references fail explicitly. The JSON reader
restores supported data types without deserializing R code or
reconnecting to the original source.

## Bring your own completed evidence

| Source | Public entry point | Resource and capture boundary |
|----|----|----|
| ellmer chat or turns (\>= 0.4.2) | [`as_trajectory_ellmer()`](https://jameshwade.github.io/scans/reference/as_trajectory_ellmer.md) | Canonical turn usage and duration where supplied. A turn duration is not asserted to be one model call. |
| OTel GenAI / Connect | [`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md) / [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md) | Call-level usage and intervals when recorded. Message capture and conversation IDs determine whether a meaningful transcript can be reconstructed. Keep read limits with the snapshot. |
| vitals evaluation (\>= 0.3.0) | [`as_trajectory_vitals()`](https://jameshwade.github.io/scans/reference/as_trajectory_vitals.md) | Outcome evaluations joined to solver trajectories; resource coverage follows the supplied turns. Reading development vitals logs requires its supported development API. |
| Deputy and dsprrr public results | [`as_trajectory_deputy()`](https://jameshwade.github.io/scans/reference/as_trajectory_deputy.md) / [`as_trajectory_dsprrr()`](https://jameshwade.github.io/scans/reference/as_trajectory_dsprrr.md) | Source execution and trace measurements remain distinct from model-call timing. dsprrr millisecond latency is normalized to canonical seconds. |
| Tempest public review | [`as_trajectory_tempest()`](https://jameshwade.github.io/scans/reference/as_trajectory_tempest.md) | Product review evidence is not a full call trace. The bundled public review fixture has no canonical token counts or model durations. |
| Commons conversations | [`as_trajectory_commons()`](https://jameshwade.github.io/scans/reference/as_trajectory_commons.md) | Already-read conversation evidence; availability depends on what the upstream reader retained. |

Optional integration versions are specified in `DESCRIPTION` and
exercised through public producer objects in the test suite. This is a
capture table, not a claim that every release or every deployment
records every field. Continue with [plain ellmer and
adapters](https://jameshwade.github.io/scans/articles/adapters.md),
[Connect
capture](https://jameshwade.github.io/scans/articles/connect.md), or the
[core scans tour](https://jameshwade.github.io/scans/articles/scans.md).

This recipe covers inspection. Reviewed-example export and its runnable
recipe are tracked in
[\#41](https://github.com/JamesHWade/scans/issues/41).
[\#45](https://github.com/JamesHWade/scans/issues/45) tracks first-use
testing with another R user; later feature recipes belong to their
delivery issues.
