# Test and helper audit

This audit addresses #50 and #51 against `b900431` on 2026-09-05. It starts
with the complete test/fixture inventory and internal function definitions,
then traces questionable assertions and helper candidates to their callers.
Changes are justified by behavior, not by assertion counts or function length.

## Test decisions

| Changed test | Decision and protected behavior |
| --- | --- |
| `test-package.R`: package metadata | Delete. DESCRIPTION keyword checks enforce incidental prose; package identity and metadata validity are checked by R CMD check. |
| `test-scan-registry.R`: registry | Assert the name-to-severity mapping and unique complete membership. Keep nonempty descriptions but remove the particular phrase about truncation. Truncated turns are exercised by the detector tests. |
| `test-scan-registry.R`: selection | Replace output-dependent scanner selection and the no-findings skip with the two known `event_error` findings, including exact trajectory, turn, primary event, and supporting event identities. |
| `test-scan-registry.R`: stable finding IDs | Require all four known findings and the third/fourth IDs when only event errors are selected. An empty result or renumbering now fails. Keep the full-row comparison to ensure selection preserves the rest of the finding. |
| `test-scan-registry.R`: empty selection, positional arguments, invalid names | Use the existing public bundle directly instead of extracting and reconstructing it. Preserve all assertions. Positional/named equivalence checks API compatibility rather than an independently calculated result. |
| `test-adapter-conformance.R`: canonical fixture round trips | Delete two tests that passed already-normalized bundles back through the same constructor, including a reordered copy. These primarily tested the expectation helper. The independent expected ellmer bundle still checks every canonical table against a public producer object. Constructor validation and serialization remain in `test-trajectory-bundle.R`. |
| `test-adapter-conformance.R`: malformed fixtures | Keep the specific error classes for malformed sources. Remove the broad wording regex, which added no constraint beyond the error contract. |
| `test-adapter-conformance.R`: shared statuses | Keep independently expected completed, failed, cancelled, interrupted, and missing-value mappings. Remove equality between two pass-through wrappers; both could agree on the same mistake. Public Deputy and Tempest adapter tests retain source-to-status assertions. |
| `test-deputy-adapter.R`: unresolved parent | Assert an `unsupported` loss on `parent_run_id` instead of searching explanatory prose. Keep the missing canonical parent and retained source identities. |
| `test-ellmer-adapter.R`: credential fields and unknown content locations | Add the missing optional ellmer guards. Keep the redaction, loss, and URI assertions. |
| `test-ellmer-adapter.R`: tool-result correlation | Rename the test around unambiguous earlier calls. Replace the large-case nonmissing-parent count with every expected parent identity and every unmatched call row. A wrong parent with the right total must fail. |
| `test-otel-adapter.R`: duplicate spans at the ceiling | Capture the expected ceiling warning in a reviewed snapshot. Keep the distinct span IDs and duplicate span accounting. No reader behavior changes. |
| `test-scans-annotations.R`: append order | Remove the wall-clock sleep. The store explicitly uses file order to break tied timestamps, which is also covered by a fixed-timestamp test. Preserve both decisions and newest-first order. |
| `test-scans-annotations.R`: scoped reads | Replace count-only assertions with the expected label, trajectory IDs, and application names. Retain aggregate/empty counts. |
| `test-scans-annotations.R`: malformed lines | Require both surrounding notes, rather than only two rows. |
| `test-scans-annotations.R`: absent store | Allocate a unique temporary filename instead of assuming a shared `absent.jsonl` path does not exist. |
| `test-scans-app.R`: Connect application switching | Guard httr2 and jsonlite, which the native reader actually uses, instead of requiring commons. Preserve request, lazy-load, and application-selection checks. |
| `test-scans-app-performance.R`: Tempest overview | Use the shared public-review fixture accessor so the optional dependency checks and fixture ownership stay consistent. |

The Tempest fixture repair is a separate commit: the old serialized proposal
failed the installed producer's schema-digest validation. The replacement was
created through public Tempest/Graft APIs, including actual host acceptance in
an in-memory Graft store. No digest was patched or validator bypassed. The
fixture README records the producer commit and CI compatibility check.

## Coverage retained across the suite

| Test files | Behavior retained and reason |
| --- | --- |
| `test-trajectory-bundle.R`, `test-conditions.R` | S7 table types, optional columns, references, duplicate IDs, cycles, chronology, loss ownership, bounded values, property replacement, serialization, and stable diagnostic conditions. Error/print snapshots represent intentional public output. |
| `test-adapter-conformance.R` | Corpus scenarios and populated columns, cross-source correlation without invented parents, malformed-source conditions, and the shared status vocabulary. Column population is a fixture completeness check, not a coverage claim. |
| `test-ellmer-adapter.R` | Public turns/Chat snapshots, caller identity, tool correlation, partial turns, redaction, duplicate/deep metadata, binary/unknown content, truncation, and unsupported shapes. Keep direct sensitive-name and correlation tests because they isolate broad failure families. |
| `test-deputy-adapter.R` | Public AgentResult objects, lifecycle/tool failures, subclass behavior, identity/delegation joins, explicit unresolved parents, source errors, safe serialization, and source limits. |
| `test-dsprrr-adapter.R` | Public module execution/export, deterministic synthetic trace rows, semantic ellmer turns, program/run identities, missing metrics, content losses, correlation, and unsupported snapshots. |
| `test-commons-adapter.R` | Public local reader, rounds, provenance gaps/conflicts, stable conversation identity, source attributes, sanitization, unsupported provenance, and explicit empty reads. |
| `test-tempest-adapter.R` | Public product reviews, authoritative-accessor rejection, stage order, accepted knowledge versus correlation, source identities, bounded lanes, unknown execution bounds, metadata redaction, and payload limits. Projection mocks isolate scans-owned conversion after the public accessor; they do not forge producer authority. |
| `test-vitals-adapter.R` | Public Task solve/score and samples, task/sample/epoch identities, row-order independence, score values, unscored samples, scorer-chat loss, source sanitization, and optional development log round trips. |
| `test-scan-primitives.R`, `test-scan-registry.R` | Public filtering, summaries, repeat/loop thresholds, correlation ambiguity, chronology, canonicalized tool arguments, error chains, truncated/failed records, unknown usage, stable selection, and exact evidence. Keep deterministic large-input cases that protect indexed joins; they make no timing-budget claim. |
| `test-measure-trajectories.R` | Complete/partial/unavailable denominators, known zeros, unsupported costs, overlapping/nested intervals, cache inclusion, deduplication, parent attribution, source fallbacks, and producer-level resource semantics. |
| `test-investigation-example.R` | Installed offline fixture conclusions, distinct retry evidence, resource arithmetic, app/script agreement, partial capture, and exact selection through `scans_app()`. Agreement is accompanied by independently expected values. |
| `test-scans-annotations.R` | Append-only decisions, reviewer/application/trajectory scope, tied timestamps, malformed lines, scalar validation, bounded notes, absent files, and public app writes. |
| `test-scans-app.R` | Lazy loaders, errors versus empty reads, bounded Connect loading, cohort filtering, exact evidence anchors, transcript chronology, eventless turns, lost evidence, source context, reload/selection retention, shared cache expiry/failure, reviewer polling, and deep links. TestServer access to internal reactives is retained where it observes session behavior unavailable from a static render. |
| `test-scans-app-markdown.R` | Rendered Markdown structure and escaped hostile/unknown markup, attributes, protocols, declarations, comments, raw-text nodes, and text preservation. HTML assertions protect evidence visibility and escaping, not incidental writing. |
| `test-scans-app-performance.R`, `test-scans-app-resources.R` | Distinct-trajectory counts, missing resource values, ranking, filter/selection agreement, source coverage, escaped labels, resource identity under reorder, pattern reset, and threshold changes. |

## Intentional defects

Each defect below was applied alone in a disposable copy, followed by
`devtools::test(filter = "^<file>$", reporter = "summary", stop_on_failure = TRUE)`.
Every run failed on a behavioral assertion. The original source was restored
between runs; these modifications are not part of the package.

| Defect | Modification | Retained test that failed |
| --- | --- | --- |
| Silent detector loss | Subset `scan_bind_findings()` output to zero rows | Registry selection and stable finding IDs |
| Selection renumbers findings | Replace selected `finding_id` values with a sequence starting at one | Stable finding IDs |
| Cached input counted twice | Add twice `cached_input_tokens` to canonical input totals | Public ellmer usage, ellmer/OTel equivalence, and dsprrr cache semantics |
| Missing capture becomes zero | Return zero for no valid values in `scans_measure_values()` | Tempest unsupported resources and missing ellmer cache components |
| Cross-application review leakage | Omit the application filter in `annotations_read()` | Scoped store reads and application-namespaced app writes |
| Unsafe HTML passes through | Return input unchanged from `scans_app_sanitize_html()` | Opaque markup visibility and unsafe event handlers/link schemes |
| Findings link to the wrong event | Replace the evidence-event match with row one | Canonical text and exact evidence anchor in the app |
| Credentials retained | Return false from `trajectory_sensitive_name()` | Public ellmer metadata/tool redaction and named-vector attribute redaction |
| Expired snapshots never reload | Return zero from `scans_app_cache_age()` | Active-session expiry, selected-trajectory rematching, and injected-time age |

## Remaining boundaries

- These are representative defect checks, not exhaustive mutation testing.
- The development-only vitals log test remains skipped locally with vitals
  0.3.0; it requires 0.3.0.9001. This audit does not weaken that version guard.
- The large app and OTel test files still contain substantial case-specific
  setup. Shared objects that would hide the important input differences were
  not introduced merely to shorten those files.
- Missing detector assessment states belong to #52. A zero-row finding table
  still cannot establish that every scanner assessed the captured evidence.
- TestServer and public fixtures do not replace the second-user deployment
  and first-use observations in #39 and #45.
