# Application diagnostics direction

Accepted 2026-09-05; revised after #46 and #47 merged.
The [roadmap](https://github.com/JamesHWade/scans/issues/2) owns current
priorities, dependencies, and delivery status. This document records the
product and design decisions behind them.

## Purpose

Help an R developer find a problem in a completed application run, inspect its
evidence, and turn a reviewed example into a regression case. The next
milestone is one real investigation followed by a matched before/after
comparison. The developer should be able to start with one ellmer chat or
one vitals result.

scans reads completed records and diagnoses execution behavior. vitals owns
evaluation execution and scoring. Keep three kinds of evidence distinct:
execution behavior, resource use, and task quality. A failed tool can appear
in a successful run; a completed run can contain a wrong answer.

## Shared analysis

Return ordinary tibbles for filtering, joining, plotting, and reporting. The
Shiny app consumes the same summaries, findings, and resource measurements
available to an R script. App controls select evidence and settings; they
should not carry separate diagnostic logic.

The overview and trajectory browser use the same filtered identities. Pattern
prevalence counts affected trajectories, and every aggregate must lead to its
contributing evidence. Preserve source identities and capture limitations
through filtering, review, and export.

The [resource contract](resource-measures.md) defines `measure_trajectories()`.
It preserves the existing meaning of `summarize_trajectories()`. Conversation
elapsed time may include user pauses; inclusive model and tool durations may
overlap; token totals may be partial. Unsupported cost or latency measures
remain unavailable. Resource coverage refers to the loaded records and does
not prove that the source captured every call.

Keep cohort analysis internal until a second consumer, such as a saved report,
establishes which calculations need a public interface. That interface must
work without Shiny.

## Assessment and review

A zero-row findings table does not distinguish a clean assessed case from
unusable evidence. Scanner coverage must identify whether each selected scan
could assess a trajectory and why it could not. Execution failure must remain
distinct from an assessed case with no finding. This applies to built-in scans
before custom extensions are added.

The public Tempest review fixture illustrates the gap: it contains product
review events but no semantic turns. Generic transcript scans emit no findings
on it. That says nothing about the quality of the research.

A reviewed example needs an exact snapshot, trajectory and optional evidence
references, detector version and settings, reviewer, judgment, and rationale.
Preserve prior decisions and uncertainty. Include reviewed unflagged cases so
scanner validation can measure missed findings as well as false positives.
Keep detector-validation labels separate from answer-quality targets.

Saving an investigation must retain the evidence and analysis settings needed
to reopen it. Export only selected examples under an inspectable content and
redaction policy. Record omitted content. Prove this with a local round trip
before adding a report format or storage abstraction.

## Comparing changes

Compare explicit baseline and candidate snapshots on matched cases. Preserve
task, sample, epoch, dataset version, and scorer identity; report duplicate,
missing, and unmatched cases. Show per-case changes and both trajectories.
Resource comparisons must respect capture coverage and source semantics.

Different production task mixes support descriptive comparisons only.
Compatible scorer scales and directions are required for aggregate outcome
comparisons. A lower observed failure rate alone does not establish that a
release caused the improvement.

## Delivery standards

Each feature needs a runnable recipe and a check that demonstrates its intended
use. Keep the initial offline walkthrough independent of later export and
comparison work. Use one maintained application and a second R user to test
whether the workflow helps someone identify and explain a recurring problem.
Record time spent, wrong turns, and questions the capture cannot answer.
Fixture results and production-pilot evidence remain separate.

Review writing, comments, tests, and helpers as part of each change. Keep prose
specific to supported behavior, comments focused on constraints, tests tied to
useful failures, and helpers justified by their callers. The
[contribution guide](../../.github/CONTRIBUTING.md) contains the review criteria.

## Deferred work

Custom scanners must first prove useful on two domain problems with reviewed
validation cases. An optional model-backed scanner would extend the current
deterministic scope and needs a separate decision on explicit execution,
budgets, provenance, and reference validation.

Add adapters when a public completed source supplies evidence needed for an
investigation. Keep integrations optional. A general multi-agent graph needs
two sources with compatible parent and timing semantics. Correlation alone
cannot establish delegation or causation.

Generic orchestration, live tool replay, a telemetry backend, automatic
production-to-training pipelines, and composite agent-health scores remain
outside this roadmap.

## Design sources

The initial direction drew on these sources, reviewed on 2026-09-05:

- [vitals](https://vitals.tidyverse.org/): evaluation execution and scoring.
- [Inspect Scout](https://meridianlabs-ai.github.io/inspect_scout/): transcript
  investigation and custom scanners.
- [Scout validation](https://meridianlabs-ai.github.io/inspect_scout/validation.html):
  reviewed labels and scanner validation.
- [OpenTelemetry GenAI conventions](https://github.com/open-telemetry/semantic-conventions-genai):
  source vocabulary for timing and usage. Supported capture must still be
  checked against actual producer output.
