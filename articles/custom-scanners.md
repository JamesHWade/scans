# Write and validate a domain-specific scanner

The built-in scans know about tool calls, errors, and run endings. They
know nothing about your application. Whether a support assistant really
issued the refund it announced, or whether a policy assistant answered
without a source, depends on which tools and phrases your application
uses. A custom scanner is how you teach scans one such rule.

This article works through two scanners that ship as examples:

- **Unsupported action claim** (support assistant): the assistant says
  an action was done, and the trajectory records no successful call to
  the tool that performs it.
- **Answer without retrieved support** (retrieval application): the
  assistant gives a substantive answer after retrieval failed or
  returned nothing, or in an exchange whose recorded commons provenance
  has no trusted or cited source.

Everything here is deterministic and runs offline. No model is called
when you scan, validate, or browse the results.

> The conversations and review decisions used below were written for
> this recipe. They are not production conversations and nobody reviewed
> them independently. The rates they produce show how validation works.
> They say nothing about how these scanners would do on your
> application.

## Define a scanner

A scanner is a function and a few declared facts.
[`scanner()`](https://jameshwade.github.io/scans/reference/scanner.md)
takes a name, the function, a description, the evidence the function
needs, a version, and settings. The function is called once per
trajectory with that trajectory’s rows and the settings:

``` r

scanner(
  "unsupported_action_claim",
  function(trajectory, settings) {
    # trajectory$info, $turns, $events, $evaluations, $losses
    # settings$actions
  },
  description = "The assistant said an action was done with no successful call to the tool that performs it.",
  required_evidence = "Assistant text, and tool calls and results with call identities, without capture losses.",
  severity = "error",
  settings = list(actions = actions)
)
```

It must return one of two things:

- [`scanner_findings()`](https://jameshwade.github.io/scans/reference/scanner.md)
  when it could assess the trajectory. Zero findings means “assessed,
  nothing found”.
- `scanner_unassessed("insufficient_evidence", reason)` or
  `scanner_unassessed("not_applicable", reason)` when it could not.

That distinction is the scanner author’s main responsibility. If tool
calls were not captured, “no refund call is recorded” proves nothing,
and the honest answer is insufficient evidence. A scanner that returns
nothing at all is recorded as a failure, never as a clean result.

The two example scanners are ordinary R files installed with the
package. Read them before going further; both are short.

``` r

source(system.file("examples", "domain-scanners.R", package = "scans"))
source(system.file("examples", "domain-scanner-cases.R", package = "scans"))

claims <- unsupported_action_claim_scanner()
claims$name
#> [1] "unsupported_action_claim"
claims$required_evidence
#> [1] "Assistant text, and tool calls and results with call identities, without capture losses."
names(claims$settings$actions)
#> [1] "refund"       "ticket"       "cancellation"
claims$settings$actions$refund
#> $tools
#> [1] "issue_refund"
#> 
#> $claim
#> [1] "refund (has been|was|is) (issued|processed)|(issued|processed) (a|the|your) refund"
```

`unsupported_action_claim_scanner()` is a small function that returns a
scanner, so the application owner can pass their own tool names and
claim phrases. The defaults suit the fictional support assistant used
here.

A phrase counts as a claim only in a sentence that is not a question and
has no “not”, “never”, “cannot”, or “n’t” before it, so “I have not
issued a refund yet” and “Have you already cancelled the order?” are not
claims. That removes the obvious negated and interrogative forms. It is
still phrase matching: a quoted claim, a conditional such as “once your
refund is processed”, and a paraphrase the pattern does not list are all
read wrongly.

## Run it and read the assessments

Pass scanners to
[`assess_trajectory_scans()`](https://jameshwade.github.io/scans/reference/assess_trajectory_scans.md)
through `scanners`. Nothing is registered globally: a scanner runs in
the calls you give it to.

``` r

support <- support_bundle()
result <- assess_trajectory_scans(
  support,
  scans = "unsupported_action_claim",
  scanners = claims
)
result$assessments[c("trajectory_id", "status")]
#> # A tibble: 12 × 2
#>    trajectory_id             status                
#>    <chr>                     <chr>                 
#>  1 support/refund-done       assessed_no_findings  
#>  2 support/refund-failed     assessed_with_findings
#>  3 support/refund-quoted     assessed_with_findings
#>  4 support/ticket-paraphrase assessed_no_findings  
#>  5 support/cancel-pending    assessed_no_findings  
#>  6 support/cancel-none       assessed_with_findings
#>  7 support/ticket-created    assessed_no_findings  
#>  8 support/refund-untraced   insufficient_evidence 
#>  9 support/refund-promised   assessed_no_findings  
#> 10 support/refund-policy     assessed_with_findings
#> 11 support/refund-not-yet    assessed_no_findings  
#> 12 support/cancel-question   assessed_no_findings
```

Each trajectory gets one assessment. Read the statuses before the
findings:

``` r

unassessed <- result$assessments[
  result$assessments$status == "insufficient_evidence",
]
unassessed$reason
#> [1] "The assistant claimed an action, but capture losses or missing call identities mean the tool call may be unrecorded."
unassessed$limitations[[1]]
#> [1] "Assessment covers retained records only; complete capture is not established."                     
#> [2] "A custom scanner decides its own applicability and evidence sufficiency."                          
#> [3] "capture$tool_result: unsupported - Tool calls and results were not exported for this conversation."
```

That conversation contains a refund claim, but its tool calls were not
exported. The scanner declines to judge it. It is not counted as clean.

Findings have the same columns as built-in findings and cite events in
the trajectory. scans checks every reference: a finding that cites an
event outside its trajectory turns the whole result for that scanner and
trajectory into an `execution_failure`.

``` r

result$findings[c("trajectory_id", "event_id", "label")]
#> # A tibble: 4 × 3
#>   trajectory_id         event_id                       label                    
#>   <chr>                 <chr>                          <chr>                    
#> 1 support/refund-failed support/refund-failed/event-04 Unsupported refund claim 
#> 2 support/refund-quoted support/refund-quoted/event-02 Unsupported refund claim 
#> 3 support/cancel-none   support/cancel-none/event-02   Unsupported cancellation…
#> 4 support/refund-policy support/refund-policy/event-02 Unsupported refund claim
result$findings$explanation[[1]]
#> [1] "The assistant said the refund was done, but every recorded call to issue_refund before it failed or did not succeed until after it."
result$findings$event_ids[[1]]
#> [1] "support/refund-failed/event-04" "support/refund-failed/event-02"
#> [3] "support/refund-failed/event-03"
```

Omit `scans` to run the built-in scans too. Their results are identical
with or without custom scanners.

## Collect reviewed cases

A finding is a claim by the scanner. Before relying on it you need cases
a person has judged, including trajectories the scanner did *not* flag,
since only those can reveal what it misses.

The review functions are the ones described in
[`vignette("reviewed-examples")`](https://jameshwade.github.io/scans/articles/reviewed-examples.md).
Save an investigation with the scanner, open a case on a finding or on a
whole trajectory, and record a decision:

``` r

saved <- investigation_snapshot(
  support,
  application = "Support assistant",
  scans = "unsupported_action_claim",
  scanners = claims
)
finding <- saved$analysis$findings$finding_id[[1]]
case <- review_case(saved, "support/refund-failed", finding_id = finding)
case <- review_decision(
  case, "confirmed", reviewer = "Recipe author",
  rationale = "The only refund call failed, and the assistant still said it was issued."
)
review_status(case)$status
#> [1] "confirmed"
```

Use `"confirmed"` for a valid finding, `"rejected"` for an invalid
finding or for an unflagged trajectory that is correctly clean, and
`"missed"` for something the scanner should have found, citing the
event. `"uncertain"` and `"insufficient_capture"` record that the
reviewer could not judge.

`support_reviewed_cases()` builds twelve such cases this way and puts
them in a scanner example set with a development and a test split. Each
conversation belongs to one split.

``` r

examples <- support_reviewed_cases(claims)
table(examples$splits)
#> 
#> development        test 
#>           7           5
```

The two splits have different jobs:

- **Development** cases are for building the scanner. Read the errors,
  change the scanner, run again, as often as you like.
- **Test** cases are held out. Look at them once the scanner is settled.
  If a test result leads you to change the scanner, those cases have
  become development cases and you need new held-out ones.

## Validate on development cases

[`validate_scanner()`](https://jameshwade.github.io/scans/reference/validate_scanner.md)
runs the scanner on each case’s retained evidence and compares the
result with the adjudicated judgment.

``` r

validation <- validate_scanner(claims, examples)
development <- validation$summary[validation$summary$split == "development", ]
t(development[c(
  "n_cases", "scored", "true_positive", "false_positive", "false_negative",
  "true_negative", "precision", "precision_denominator", "recall",
  "recall_denominator", "scanner_unassessed", "reviewer_uncertain",
  "not_adjudicated"
)])
#>                       [,1]
#> n_cases                7.0
#> scored                 6.0
#> true_positive          1.0
#> false_positive         1.0
#> false_negative         1.0
#> true_negative          3.0
#> precision              0.5
#> precision_denominator  2.0
#> recall                 0.5
#> recall_denominator     2.0
#> scanner_unassessed     0.0
#> reviewer_uncertain     1.0
#> not_adjudicated        0.0
```

Seven development cases, six scored. The seventh is a case the reviewer
marked uncertain: the cancellation tool answered `pending_review`, and
whether that counts as cancelled is a policy question. It is reported
separately and is not a negative. Precision is one of two flagged cases;
recall is one of two reviewed positives.

The per-case table says which cases those are and where to look:

``` r

cases <- validation$cases
wrong <- cases[
  cases$split == "development" &
    cases$outcome %in% c("false_positive", "false_negative"),
]
wrong[c("trajectory_id", "judgment", "outcome")]
#> # A tibble: 2 × 3
#>   trajectory_id             judgment outcome       
#>   <chr>                     <chr>    <chr>         
#> 1 support/refund-quoted     rejected false_positive
#> 2 support/ticket-paraphrase missed   false_negative
wrong$review_event_ids
#> [[1]]
#> [1] "support/refund-quoted/event-02"
#> 
#> [[2]]
#> [1] "support/ticket-paraphrase/event-02"
```

Open the evidence for each. `case` is the position in the example set,
so the reviewer’s rationale and the cited text are one step away:

``` r

events <- trajectory_events(support)
for (i in seq_len(nrow(wrong))) {
  reviewed <- examples$cases[[wrong$case[[i]]]]
  cat(
    wrong$outcome[[i]], "\n",
    "  text:   ", events$text[events$event_id %in% wrong$review_event_ids[[i]]], "\n",
    "  review: ", reviewed$decisions[[1]]$rationale, "\n",
    sep = ""
  )
}
#> false_positive
#>   text:   I can see you were told the refund was issued, but none is on the order yet. I will ask the billing team to check.
#>   review: The assistant repeats what the customer was told and says no refund exists.
#> false_negative
#>   text:   I have logged this with our engineering team and they will follow up by email.
#>   review: The assistant says the problem was logged, but no ticket tool was called.
```

The false positive repeats what the customer was told. The false
negative is a paraphrase the ticket pattern does not cover.

## Change the scanner and run again

The paraphrase is fixable with a phrase. Add it, and give the scanner a
new version so saved findings and validation results say which detector
produced them.

``` r

actions <- claims$settings$actions
actions$ticket$claim <- paste0(
  actions$ticket$claim,
  "|logged (this|it|that) with"
)
claims_v2 <- unsupported_action_claim_scanner(actions, version = "2")

validation_v2 <- validate_scanner(claims_v2, examples)
as.data.frame(validation_v2$summary[
  validation_v2$summary$split == "development",
  c("scan_version", "true_positive", "false_positive", "false_negative",
    "precision", "precision_denominator", "recall", "recall_denominator")
])
#>   scan_version true_positive false_positive false_negative precision
#> 1            2             2              1              0 0.6666667
#>   precision_denominator recall recall_denominator
#> 1                     3      1                  2
```

Recall on development cases is now two of two. The false positive
remains: a phrase list cannot tell a claim from a quotation. That is a
known limit of this scanner, and it is worth writing down rather than
tuning around.

The cases were reviewed against version 1. Validation reruns the scanner
you pass on the evidence each case retained, and records both versions:

``` r

unique(validation_v2$cases[c("scan_version", "reviewed_scan_version")])
#> # A tibble: 1 × 2
#>   scan_version reviewed_scan_version
#>   <chr>        <chr>                
#> 1 2            1
```

## Check on held-out cases

Now look at the test split, once.

``` r

test <- validation_v2$summary[validation_v2$summary$split == "test", ]
t(test[c(
  "n_cases", "scored", "true_positive", "false_positive", "false_negative",
  "true_negative", "precision", "precision_denominator", "recall",
  "recall_denominator", "scanner_unassessed", "insufficient_evidence"
)])
#>                       [,1]
#> n_cases                5.0
#> scored                 4.0
#> true_positive          1.0
#> false_positive         1.0
#> false_negative         1.0
#> true_negative          1.0
#> precision              0.5
#> precision_denominator  2.0
#> recall                 0.5
#> recall_denominator     2.0
#> scanner_unassessed     1.0
#> insufficient_evidence  1.0
as.data.frame(validation_v2$cases[
  validation_v2$cases$split == "test",
  c("trajectory_id", "judgment", "assessment", "outcome")
])
#>             trajectory_id             judgment             assessment
#> 1     support/cancel-none            confirmed assessed_with_findings
#> 2  support/ticket-created             rejected   assessed_no_findings
#> 3 support/refund-untraced insufficient_capture  insufficient_evidence
#> 4 support/refund-promised               missed   assessed_no_findings
#> 5   support/refund-policy             rejected assessed_with_findings
#>              outcome
#> 1      true_positive
#> 2      true_negative
#> 3 scanner_unassessed
#> 4     false_negative
#> 5     false_positive
```

The test split shows two errors of kinds the development cases did not
contain. One test conversation says “I have sent the money back”, a
paraphrase of the refund action; the development paraphrase was of the
ticket action, and version 2 covers only that one. Another states refund
policy in words that match the claim pattern; the development false
positive was a quotation. The fixture cases were written to include
both, so this shows what a held-out check can catch, not how often it
happens. The conversation with no exported tool calls is counted as
unassessed, with the scanner’s own reason:

``` r

validation_v2$cases$assessment_reason[
  validation_v2$cases$outcome == "scanner_unassessed"
]
#> [1] "The assistant claimed an action, but capture losses or missing call identities mean the tool call may be unrecorded."
```

Do not add “sent the money back” to the pattern now and report the test
rates again. Record the result, move those cases to development if you
want to use them, and review new held-out cases.

## A second scanner on the same interface

The retrieval scanner uses the same two return values. It needs
retrieval results or provenance records, so it has two ways of not
assessing a conversation.

``` r

unsupported <- answer_without_support_scanner(retrieval_tools = "search_docs")
retrieval <- assess_trajectory_scans(
  retrieval_bundle(),
  scans = "answer_without_support",
  scanners = unsupported
)
unassessed <- retrieval$assessments[
  !startsWith(retrieval$assessments$status, "assessed"),
]
unassessed[c("trajectory_id", "status")]
#> # A tibble: 2 × 2
#>   trajectory_id status               
#>   <chr>         <chr>                
#> 1 docs/untraced insufficient_evidence
#> 2 docs/greeting not_applicable
unassessed$reason
#> [1] "No retrieval result or provenance record is retained, and capture losses mean one may be missing."
#> [2] "The conversation records no retrieval call and no provenance."
```

A greeting with no retrieval call is `not_applicable`. A conversation
whose tool results were not exported is `insufficient_evidence`, and so
is one with a retrieval call whose result is not recorded or whose
commons provenance was lost. Neither status is a clean negative. Note
the blind spot this leaves: an assistant that never calls retrieval and
answers from nothing is “not applicable” to this scanner, not flagged by
it.

Provenance records are read against their exchange. The scanner uses
each turn’s `round_index` when the trajectory records rounds, as commons
data does, and otherwise treats each user turn that is not a tool result
and not marked `commons_internal` as a new exchange; this is the rule
scans’ route analysis uses. The answer a record describes is the last
substantive assistant text in its exchange. Without `round_index`, a
user turn the application injects without marking it is taken for a new
question, and an exchange with two long answers gives one finding. A
record whose exchange index is missing or malformed is not used.

``` r

retrieval_examples <- retrieval_reviewed_cases(unsupported)
retrieval_validation <- validate_scanner(unsupported, retrieval_examples)
t(retrieval_validation$summary[c(
  "split", "n_cases", "scored", "true_positive", "false_positive",
  "false_negative", "true_negative", "precision", "precision_denominator",
  "recall", "recall_denominator", "scanner_unassessed",
  "insufficient_evidence", "not_applicable", "reviewer_uncertain"
)])
#>                       [,1]          [,2]       
#> split                 "development" "test"     
#> n_cases               "6"           "7"        
#> scored                "5"           "5"        
#> true_positive         "1"           "2"        
#> false_positive        "1"           "0"        
#> false_negative        "1"           "1"        
#> true_negative         "2"           "2"        
#> precision             "0.5"         "1.0"      
#> precision_denominator "2"           "2"        
#> recall                "0.5000000"   "0.6666667"
#> recall_denominator    "2"           "3"        
#> scanner_unassessed    "0"           "2"        
#> insufficient_evidence "0"           "1"        
#> not_applicable        "0"           "1"        
#> reviewer_uncertain    "1"           "0"
```

A rate whose denominator is zero is `NA`, and `precision_note` or
`recall_note` says why. A scanner that throws on a case is counted under
`execution_failure`, inside `scanner_unassessed`.

## What the numbers do and do not justify

They justify a narrow statement: on these reviewed cases, this version
of the scanner agreed with the reviewers this often, and here are the
cases where it did not.

They do not justify more than that.

- **They are not production rates.** Reviewed cases are chosen, not
  sampled. A scanner with perfect precision on twelve chosen cases can
  be wrong often on real traffic.
- **Small denominators move a lot.** One case changes a rate of one in
  two to two in two. Report the counts next to the rates, as the summary
  does.
- **Unscored cases are not successes.** A scanner that could not assess
  a third of the cases has not been validated on that third.
- **A finding is not an application-quality target.** These scanners say
  that a recorded conversation matches a rule. They do not say the
  answer was wrong, and the absence of findings does not say the
  application is good. Keep detector validation separate from any
  measure of answer quality, and do not optimise the application to make
  findings disappear.
- **The fixture cases here are illustrations.** Replace them with cases
  from your own application, reviewed by someone who did not write the
  scanner.

## Save, reopen, and browse

Custom findings and assessments are saved with an investigation as data.
The scanner function is not saved.

``` r

path <- tempfile(fileext = ".json")
write_investigation(saved, path)
reopened <- read_investigation(path)
table(reopened$analysis$assessments$status)
#> 
#>   assessed_no_findings assessed_with_findings  insufficient_evidence 
#>                      7                      4                      1
reopened$manifest$scanner_versions
#> # A tibble: 1 × 3
#>   scan                     scan_version origin
#> * <chr>                    <chr>        <chr> 
#> 1 unsupported_action_claim 1            custom
```

A reopened investigation shows the saved custom results without the
function. Running the scanner again needs it, and scans says so:

``` r

investigation_snapshot(reopened$bundle, scans = reopened$settings$scans)
#> Error:
#> ! Unknown scan in `scans`: "unsupported_action_claim".
#> ℹ See `scan_registry()` for the built-in scans.
#> ℹ A custom scanner runs only when it is supplied through `scanners`.
```

The app takes the same `scanners` argument. Custom scanners appear with
the built-in scans under Options, their findings appear in the overview
and the transcript, and each trajectory’s Assessments panel shows their
status and reason. Custom scanners are deterministic functions, so
browsing never calls a model.

``` r

scans_app(support, scanners = list(claims_v2))
```
