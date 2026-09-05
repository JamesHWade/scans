# Make scans useful for improving an application

Accepted direction, 2026-09-05. Repository baseline: `f471110`.
The [roadmap](https://github.com/JamesHWade/scans/issues/2) tracks the work in
[#39–#45](https://github.com/JamesHWade/scans/issues/2#issue-body). This document
distinguishes the initial app implementation from outstanding backlog work.
Model-backed scanning remains a later, explicit scope decision.

## The product promise

**Understand where an R agent or chat workflow struggles, inspect the evidence,
and carry what you learn into the next evaluation.**

The primary user is the person responsible for a deployed application or an
agent under development. Their question is concrete: “Search started failing
after I changed a tool; which conversations were affected, what happened, and
what examples should protect the next release?” A transcript viewer answers
the middle of that question. scans should support the complete investigation.

There are three distinct meanings of performance: execution behavior (errors,
retries, tool selection), resource use (latency, tokens, cost), and task quality
(correctness, groundedness, useful completion). Show them together when the
evidence permits, but preserve their meanings. A completed trace can contain a
wrong answer. A successful answer can follow an inefficient path. An absent
trace tells us neither.

The R opportunity is analysis that composes: ordinary tibbles for grouping,
plotting, joining, and reporting; a Shiny app over the same calculations;
public adapters; and reusable, reviewed examples. A user should benefit with
one ellmer chat or one vitals result, without adopting the rest of this
repository's package ecosystem.

## What exists and what the backlog misses

At the baseline, the package already had eight input paths, a validated S7
bundle, nine deterministic detectors, Connect ingestion and caching, linked
evaluations, a transcript inspector, and append-only trajectory annotations.
`summarize_trajectories()` exposes identity, tool counts, usage, durations,
failures, and losses. The app presented these mainly one trajectory at a time.

At the initial GitHub inventory, only
[#2](https://github.com/JamesHWade/scans/issues/2) and
[#9](https://github.com/JamesHWade/scans/issues/9) were open. They described
adapter expansion as the frontier and grouped landed integrations with the
unresolved shinychat snapshot question. The updated roadmap now follows the
investigation workflow below. #9 records landed adapters and calls for a
refresh of the public shinychat contract against the pinned integration audit.

The code also exposes measurement limits that a dashboard must respect:

- `elapsed` is trajectory wall time; conversation pauses may dominate it.
- Token totals sum known values. They do not prove that all calls were captured.
- `cost` lacks a canonical currency and pricing basis. Cross-run currency
  totals or estimated savings would be premature.
- Parent and child trajectories may describe overlapping work. Summing their
  durations or usage can double-count it.
- Detector findings are positive records. No finding does not distinguish
  “examined and absent” from “not assessable with the available evidence.”
- Annotations label a trajectory, but do not yet identify a detector version,
  frozen snapshot, evidence selection, or adjudicated validation target.
- Source read limits exist beside the bundle. A reproducible report needs to
  retain those limits alongside the selected identities and scan settings.

## The first app change

The initial implementation adds an **Application overview** alongside the
existing **Trajectory** inspector. It follows the browser filters and reports
affected trajectories per pattern, median and p95 elapsed time, median recorded
tokens, and evaluation/loss coverage. A pattern narrows the browser with a
visible clear action; ranked trajectories open the existing evidence view.
Unknown usage remains unknown. The app does not invent outcome scores or cost.

The calculations reuse canonical summaries, preserved conversation usage, and
findings. Both app views prefer valid conversation token totals over turn
summaries because reconstructed turns may omit earlier model calls. The #40
implementation now supplies a public, per-trajectory `measure_trajectories()`
interface shared with the offline investigation recipe, including coverage and
source semantics. The [resource contract](resource-measures.md) preserves the
existing summary columns. Cohort analysis remains internal; the report/export
work below still earns that separate public interface. Neither interface should
depend on Shiny inputs.

Validation uses a deterministic cohort with known, partial, and missing
measurements, public ellmer turn objects, and the existing public Tempest
review fixture. This proves a
source-neutral consumer, not deployed Tempest capture completeness. A live
application pilot remains necessary before claiming production usefulness.

The Tempest fixture has 82 product-review events and zero semantic turns. It
provides elapsed time but no canonical token totals. The nine generic scans
produce no findings on that fixture. That does not establish that the research
was good or that all relevant behaviors were assessed; it makes the need for
explicit scanner applicability and source-specific evidence interpretation
concrete.

## The intended workflow

1. **Choose the evidence.** Select the application, capture window, and relevant
   model or release. Show the loaded snapshot's age, limits, unknown fields,
   and exact cohort size.
2. **Find a question worth investigating.** Rank repeated failure patterns and
   unusual resource use. Keep execution errors, outcome evaluations, and
   detector judgements distinct. Every aggregate leads to its contributing
   trajectories.
3. **Explain an example.** Inspect model calls, tools, and delegated work in
   recorded order. Show timing only where capture supports it. Preserve
   ambiguous correlations and withheld content as explicit limitations.
4. **Record a judgement.** Accept or reject a proposed pattern, cite evidence,
   and record uncertainty. Preserve earlier reviewers' decisions.
5. **Protect the next change.** Export explicitly reviewed examples for vitals,
   compare matched cases, and inspect improved and regressed trajectories.

## Backlog changes, in dependency order

#2 is the map, with native sub-issues for the seven workstreams below and the
remaining integration inventory in #9. Closed issues were checked before
creating new tickets. #42 is blocked by #40; #43 is blocked by #41. Other
dependencies apply to specific delivery stages, so they should not prevent
independent work on the first-run recipe or basic report.

| Issue | Workstream | Next action |
| --- | --- | --- |
| [#39](https://github.com/JamesHWade/scans/issues/39) | Application overview | Land this implementation, then run the pilot and realistic example. |
| [#40](https://github.com/JamesHWade/scans/issues/40) | Resource and capture measures | Define the contract against supported producer fixtures. |
| [#41](https://github.com/JamesHWade/scans/issues/41) | Reviewed validation examples | Establish immutable review and export boundaries. |
| [#42](https://github.com/JamesHWade/scans/issues/42) | Matched comparisons | Refine pairing and scorer contracts; resource comparison depends on #40. |
| [#43](https://github.com/JamesHWade/scans/issues/43) | Custom scanners | Use #41 validation cases to prove two concrete detectors. |
| [#44](https://github.com/JamesHWade/scans/issues/44) | Reproducible investigations | Prove a report consumer after the overview lands; judged exports need #41. |
| [#45](https://github.com/JamesHWade/scans/issues/45) | Community onboarding | Start the provider-free recipe now; add review/comparison stages as their APIs land. |

### A. Understand a captured application before opening a trajectory

Priority: now. The initial implementation advances #39; the ticket remains
open for its deployment pilot and realistic onboarding example.

Acceptance: the same cohort drives overview and browser; pattern prevalence
deduplicates trajectories; every row opens exact evidence; unknown values and
source read limits are visible; empty, failed, and capped loads are distinct;
filter changes do not refetch. Prove the journey with the public Tempest
fixture and an ellmer or OTEL producer fixture, plus a real deployment pilot.

Follow-up within this ticket: replace the small synthetic verification cohort
with a realistic offline onboarding example and document what a user can
actually conclude from each measure. Do not interpret the UI change as proof
of capture quality.

### B. Define trustworthy resource and capture measures

Priority: next; blocks quantitative resource comparisons in D.

Acceptance: distinguish conversation elapsed time, model-call duration, tool
duration, first-token latency when present, and periods with no recorded work;
define denominators and missingness for each. Preserve source time units,
usage attribution, and inclusive/exclusive cost semantics. Currency and price
basis must accompany cost comparisons. Missing capture must not become zero.
Parallel spans must not be added and labelled wall time.

Test with repeated histories from OTEL, overlapping parent/child work, partial
turn usage, model-only traces, and unsupported timing. An analysis may state
“unavailable”; that is a valid result. Reuse OpenTelemetry semantics where
they fit, while retaining adapter-version evidence.

### C. Turn reviewed findings into validation examples

Priority: next; independent of B after A establishes the review journey.

Acceptance: record the exact snapshot and trajectory, optional event/turn
references, detector and version, judgement, reviewer, and rationale. Support
confirmed, rejected, and uncertain findings; capture false negatives from
unflagged examples too. Keep immutable prior decisions and explicit
adjudication. Export only the examples the user selects, with an inspectable
content/redaction policy. Re-import preserves identities and provenance.

Keep detector validation targets separate from answer-quality targets for
vitals. No automatic promotion of private production transcripts into a shared
test set. Demonstrate one reviewed retry-pattern set and one vitals-compatible
task example through public objects.

### D. Compare a change on matched cases

Priority: after B; can begin with existing outcome evaluations before cost is
available.

Acceptance: select explicit baseline/candidate snapshots and a pairing key
with task/sample/epoch and dataset-version semantics; report unmatched,
duplicate, and missing cases. Preserve scorer identity, version, scale, and
direction. Show per-case changes in outcomes, diagnostic patterns, and
supported resource measures, with links to both trajectories. Never pool
incompatible scorers into one average.

Use paired summaries and uncertainty where justified. Production cohorts with
different task mixes remain descriptive and are labelled unpaired; a lower
error rate alone does not establish that the release caused an improvement.
The acceptance fixture must contain a quality improvement that costs more, a
regression, and unmatched cases. vitals owns executing and scoring the runs.

### E. Let users define and validate domain-specific scanners

Priority: after C; avoid an LLM-scanner feature ahead of validation.

Acceptance: a small public function contract consumes completed evidence and
returns bounded findings with valid references. Record applicability,
abstention, execution errors, configuration, and version separately from
positive findings. A failed scanner must not look like a clean trajectory.
Provide two useful custom detectors through the same contract before adding
an extension framework.

Validate against held-out reviewed cases; report false positives, false
negatives, precision, recall, and abstentions with denominators. A later
optional ellmer-backed scanner needs explicit execution and budget controls,
persisted provenance, and reference validation. Browsing an app must never
silently call a model. This would extend today's deterministic-analysis scope
and deserves a separate design decision.

### F. Save and share a reproducible investigation

Priority: after A; use C when exporting judgements.

Acceptance: save application/source identity, snapshot fingerprint, selected
trajectory IDs, filters, scanner versions/settings, capture limits, findings,
review decisions, and package version. A local HTML or Quarto report and the
app must agree on the same analysis result. Save a new investigation revision
instead of mutating an old one. A redacted export must list what it omits.

This is the second consumer that earns a public cohort-analysis interface.
Do not require a hosted database, new query language, or general artifact
runtime. Profile filtering and rendering on a fixed 10,000-trajectory fixture;
set performance budgets from that baseline, then add paging/caching where the
measurements justify it.

### G. Prove the community onboarding path

Priority: alongside A, then extend with C and D.

Acceptance: a fresh R session can install scans, run a provider-free example,
explain one finding, inspect a limitation, and export a reviewed example using
documented public functions. Follow with recipes for plain ellmer, a chat app
on Connect, and vitals; Tempest is an integration proof, not a prerequisite.
Execute documentation in CI and test current supported upstream public
objects. Publish a small compatibility/capture table and reproducible local
fixtures, not unsupported “works with everything” claims.

## What to defer

Additional adapters need a concrete source that adds evidence and a user
journey needing it. Generic orchestration, live tool replay, a telemetry
backend, automatic production-to-training pipelines, and a composite agent
health score would distract from the current opportunity. Keep source-owned
privacy, execution, and authority semantics intact.

Defer a general multi-agent graph until two real sources demonstrate the
necessary parent and timing semantics. A correlated identifier is useful for
navigation; it is not proof of causation.

## How to judge progress

Use a task-based pilot with one maintained app and a second R user. Record how
long it takes to identify a recurring problem, reach the supporting event,
record a judgement, and recover the same investigation after a refresh. Record
which questions remain unanswerable because capture is incomplete. The next
milestone is one confirmed issue becoming a reusable regression case, followed
by a matched before/after evaluation. Targets should follow the pilot baseline;
do not claim adoption or performance gains from synthetic tests.

## External reference points

These are design inputs reviewed on 2026-09-05, not promises that scans already
implements their capabilities.

- [vitals](https://vitals.tidyverse.org/) already runs solvers and scorers,
  writes evaluation logs, and supports assessing changes in models, prompts,
  and tools. scans should connect production diagnosis to that evaluation
  workflow through stable identities and reviewed cases.
- [Inspect Scout](https://meridianlabs-ai.github.io/inspect_scout/) combines
  transcript inspection with custom scanners and persisted results. The
  transferable product lesson is an investigation workflow rather than a
  growing list of importers.
- [Scout validation](https://meridianlabs-ai.github.io/inspect_scout/validation.html)
  connects reviewer labels to scanner validation and exposes precision and
  recall. That motivates treating detector quality as a product capability,
  distinct from the quality of the agent's answer.
- [OpenTelemetry GenAI conventions](https://github.com/open-telemetry/semantic-conventions-genai)
  provide the upstream vocabulary to check before promoting timing and usage
  fields into comparable scans measures. Confirm supported versions against
  actual producer output; the existence of a convention does not prove capture.
