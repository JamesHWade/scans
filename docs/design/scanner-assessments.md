# Scanner assessments

Issue #52 needs one row per selected built-in scanner and trajectory, alongside
findings. `assess_trajectory_scans()` returns an ordinary list with `findings`
and `assessments`; it introduces no detector registry or execution framework.
`scan_trajectories()` retains its findings-only contract and error behavior.

Assessment statuses are `assessed_no_findings`, `assessed_with_findings`,
`insufficient_evidence`, `not_applicable`, and `execution_failure`. Each row
records scanner version, thresholds used, required evidence, reason, finding
identities, and capture limitations with row references into `trajectory_losses()`.
All conclusions concern the retained capture, not the completeness or success
of the original run. An observed error establishes a positive error finding even
when other records lack status; it cannot establish the absence of other errors.

Tool correlation requires recorded tool identities. Argument comparisons require
recorded names and arguments; empty argument lists are valid. Loop checks also
need roles for intervening content. Known relevant losses prevent negative
conclusions and comparisons of incomplete arguments. Status scans need explicit
recorded status or failure evidence. Error chains need parent relationships when
multiple failed events exist. Missing semantic records are insufficient evidence;
a captured text conversation without tool activity can establish that a tool
scan does not apply to that capture. Nonempty product events alone cannot.

The findings-only API remains available for raw detector output. The new API
retains its finding identities but excludes candidates from unassessed scans;
a missing call identity or redacted arguments must not become a diagnosed tool
failure. Successful, assessable findings have exactly the legacy representation.
Built-ins currently execute in three existing groups (tool, event, record).
Failure in a group marks its selected scanners as failed for that trajectory,
leaves other groups and trajectories available, and displays a fixed reason
without copying arbitrary condition messages into the result or browser.

Acceptance examples, all offline:

| Capture | Expected distinction |
| --- | --- |
| Public ellmer tool exchange | Tool scans assessed with no findings |
| Recorded repeated failing requests | Repetition and error findings with original event identities |
| Contentless OTel spans | Semantic scans insufficient, retained span status assessable |
| Public Tempest product review | Tool/turn scans insufficient despite product events; product status remains usable |
| Injected built-in execution error | Visible execution failure, other groups still usable |

The app uses the same public result for findings, per-trajectory assessment
details, and overview coverage. Coverage counts scanner/trajectory pairs in the
current cohort and never treats unassessed rows as negative results. Changing
selection or thresholds recomputes from the cached bundle. Saving reviews,
custom/model-backed scanners, and judging upstream capture completeness are
outside this slice.
