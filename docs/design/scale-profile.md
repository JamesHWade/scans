# Scale profile

The third delivery for #44 measures scans on a fixed 10,000-trajectory cohort
before any paging or caching is added. This note records the environment,
the fixture, the baseline, the budgets chosen from it, and what was and was
not changed as a result. Recorded 2026-11-26.

## How to repeat it

```sh
Rscript tools/scale-fixture.R            # prints the fixture's shape
Rscript tools/scale-profile.R out.csv    # about 20 minutes
```

`tools/scale-fixture.R` builds the cohort from a fixed seed; nothing is
shipped as data. `tools/scale-profile.R` runs each step five times, three
times when it takes over five seconds, and once when it takes over a minute,
and reports the median, the range, and the peak memory R allocated above what
was in use when the step started. Rendering times are the server's work to
produce HTML and exclude the browser's layout.

The test suite asserts no times. The script is the regression guard: rerun it
and compare with the table below. Each fix made here has a test that pins its
behavior, and two of them count calls rather than seconds.

## Environment

- Apple M2 Pro, 10 cores, 16 GB memory; macOS (Darwin 27.3.0, arm64).
- R 4.6.1.
- scans 0.0.0.9000, S7 1.0.0, tibble 3.3.1, vctrs 0.7.3, rlang 1.3.0,
  cli 3.6.6, jsonlite 2.0.0, digest 0.6.39, htmltools 0.5.9, shiny 1.15.0,
  bslib 0.12.0.

## Fixture

`scale_fixture(10000, seed = 4410)` is shaped like a deployed support
assistant read from OpenTelemetry. All text is synthetic.

| | |
| --- | --- |
| Trajectories | 10,000 over 14 days (2026-09-01 to 2026-09-14 UTC), with a daily and weekly rhythm |
| Turns and events | 103,410 each; per trajectory: median 8, 90th percentile 22, 99th 44, maximum 88 |
| Empty trajectories | 42 (no turns or events) |
| Tool calls | 25,887 across 8 tools; 2,576 failed, including a two-day outage of one tool with retry loops |
| Models | 4 (3,747 / 3,039 / 2,687 / 527), with a model change on day 8 |
| Users | 399 with a long tail; 495 trajectories record no user |
| Status | 9,545 completed, 181 failed, 274 not recorded |
| Capture gaps | 105 without an end time, 211 recorded without content, 399 with missing usage and a loss record, 5 capture-wide losses |
| Findings | 4,226: 2,576 event errors, 755 repeated calls, 498 loops, 181 failed runs, 120 unresolved calls, 96 cut-off turns |
| In memory | 87 MB |

## Baseline

Medians on the fixture, after the fixes below. `n` is the number of
trajectories the step worked on.

| Group | Step | n | Runs | Median (s) | Range (s) | Peak memory (MB) |
| --- | --- | --- | --- | --- | --- | --- |
| bundle | generate fixture and construct | 10,000 | 3 | 17.516 | 16.914–19.122 | 227 |
| bundle | `TrajectoryBundle()` from tables | 10,000 | 3 | 16.118 | 16.071–16.654 | 202 |
| bundle | validate | 10,000 | 3 | 8.605 | 7.490–8.798 | 196 |
| diagnostics | `scan_trajectories()` | 10,000 | 3 | 7.750 | 7.731–8.684 | 192 |
| diagnostics | `assess_trajectory_scans()` | 10,000 | 3 | 25.185 | 23.606–25.856 | 193 |
| diagnostics | `summarize_trajectories()` | 10,000 | 5 | 1.030 | 1.025–1.121 | 221 |
| diagnostics | `measure_trajectories()` | 10,000 | 3 | 21.388 | 20.763–21.693 | 223 |
| app data | `scans_app_data()`, diagnostics included | 10,000 | 3 | 49.462 | 48.772–54.153 | 297 |
| app data | `scans_app_data()`, diagnostics supplied | 10,000 | 5 | 3.642 | 3.479–3.783 | 242 |
| app data | review queue for the snapshot | 10,000 | 5 | 0.731 | 0.703–0.790 | 194 |
| cohort | overview data, all trajectories | 10,000 | 5 | 0.589 | 0.569–0.647 | 343 |
| cohort | overview HTML, all trajectories | 10,000 | 5 | 0.517 | 0.497–0.653 | 162 |
| filter: select rows | facet: model | 2,687 | 5 | 0.006 | 0.005–0.027 | 4 |
| filter: overview data | facet: model | 2,687 | 5 | 0.372 | 0.369–0.374 | 242 |
| filter: select rows | facet: tool failed | 602 | 5 | 0.009 | 0.009–0.010 | 6 |
| filter: overview data | facet: tool failed | 602 | 5 | 0.246 | 0.242–0.254 | 209 |
| filter: select rows | facet: user | 1,417 | 5 | 0.006 | 0.005–0.006 | 4 |
| filter: overview data | facet: user | 1,417 | 5 | 0.078 | 0.078–0.080 | 99 |
| filter: select rows | time window: one day | 238 | 5 | 0.011 | 0.010–0.035 | 5 |
| filter: overview data | time window: one day | 238 | 5 | 0.127 | 0.125–0.134 | 180 |
| filter: select rows | text search | 5,753 | 5 | 0.063 | 0.061–0.064 | 4 |
| filter: overview data | text search | 5,753 | 5 | 0.529 | 0.484–0.700 | 319 |
| filter: select rows | text search, no match | 0 | 5 | 0.060 | 0.060–0.061 | 3 |
| filter: overview data | text search, no match | 0 | 5 | 0.028 | 0.027–0.029 | 43 |
| filter: select rows | findings only | 1,406 | 5 | 0.004 | 0.003–0.009 | 3 |
| filter: overview data | findings only | 1,406 | 5 | 0.328 | 0.324–0.333 | 231 |
| filter: select rows | pattern | 458 | 5 | 0.004 | 0.004–0.014 | 3 |
| filter: overview data | pattern | 458 | 5 | 0.195 | 0.193–0.203 | 207 |
| filter: overview data | facet: model, with its wider scope | 2,687 | 5 | 0.458 | 0.446–0.509 | 318 |
| browser list | entries HTML, as the app lists them | 100 | 5 | 0.153 | 0.152–0.162 | 155 |
| browser list | entries HTML, as the app lists them | 500 | 5 | 0.777 | 0.769–0.794 | 163 |
| browser list | entries HTML, as the app lists them | 10,000 | 5 | 0.776 | 0.768–0.971 | 162 |
| browser list | entries HTML, every entry rendered | 2,000 | 5 | 3.179 | 3.151–3.354 | 268 |
| browser list | order by newest | 10,000 | 5 | 0.001 | 0.001–0.006 | 2 |
| app session | start to first overview and list, data prepared | 10,000 | 3 | 7.375 | 7.184–7.772 | 317 |
| report | `report_build()`, the app's report, all trajectories | 10,000 | 5 | 0.597 | 0.574–0.614 | 539 |
| investigation, 1,000 | `investigation_snapshot()` of 1,000 from the bundle | 1,000 | 1 | 86.554 | | 609 |
| investigation, 1,000 | verify identifiers and references | 1,000 | 3 | 26.329 | 26.181–27.109 | 441 |
| investigation, 1,000 | `investigation_report(verify = FALSE)` | 1,000 | 5 | 1.784 | 1.775–1.797 | 372 |
| investigation, 1,000 | `format()`, Markdown | 1,000 | 5 | 0.068 | 0.068–0.070 | 88 |
| investigation, 1,000 | `format()`, HTML | 1,000 | 5 | 0.041 | 0.039–0.048 | 52 |
| investigation, 1,000 | `write_investigation()` | 1,000 | 3 | 48.015 | 47.682–48.539 | 545 |
| investigation, 1,000 | `read_investigation()` | 1,000 | 3 | 45.540 | 44.921–46.722 | 726 |

The 1,000-trajectory snapshot runs the diagnostics on the whole bundle
before selecting, which is 48 of its 87 seconds. Its file is 62 MB; its
report is 0.05 MB of Markdown or 0.07 MB of HTML.

Saved-investigation steps for all 10,000 trajectories were run once
(`SCANS_SCALE_SAVED=1000,10000`), in an earlier run of the same code:

| Step | Seconds |
| --- | --- |
| `investigation_snapshot()` | 579 |
| verify identifiers and references | 358 |
| `investigation_report()`, not verifying | 27 |
| `format()`, Markdown / HTML / HTML listing every trajectory | 0.22 / 0.14 / 0.84 |
| `write_investigation()` | did not finish |
| `read_investigation()` | not reached |

`write_investigation()` was stopped after 20 minutes: the process had used
the machine's 16 GB and was swapping.

### In a browser

One session against `scans_app()` serving the fixture, on the same machine,
timed from the page with `performance.now()`: first overview 53 s after
opening the page (the data preparation above); model facet on 0.68 s and off
0.88 s; text search 1.24 s including the input's debounce; pattern 0.52 s;
clearing every filter to a redrawn overview and list 2.30 s; opening a listed
trajectory 0.42 s; the report dialog for all 10,000 trajectories 4.6 s, and
its HTML download (0.34 MB) 0.25 s. These are single observations, not
medians.

## Fixes made, before and after

Only hotspots that were local and had an exact equivalent were changed. The
"before" figures are medians from a first run of the script, which was
stopped before it printed ranges, except where noted.

| Step | Before (s) | After (s) | Cause and fix | Pinned by |
| --- | --- | --- | --- | --- |
| `assess_trajectory_scans()` | 168.1 | 25.2 | A one-row tibble was built per trajectory and scan (110,000) and bound at the end, and every finding was searched per pair. Columns are now filled as vectors and findings grouped by trajectory once. Results are identical. | A cohort's assessments equal each trajectory's own, in bundle order |
| `scans_app_data()`, diagnostics included | 184.2 | 49.5 | The same. | |
| Browser list, 10,000 visible | 25.2 (6.9 MB) | 0.78 (0.4 MB) | htmltools renders about a millisecond and a kilobyte per entry, and superlinearly for long lists. The list now renders the first 500 entries and says how many it left out. | The list shows a bounded head and names the rest |
| App session start, data prepared | 53.0 | 7.4 | A click observer was created per trajectory. They are now created when an entry is first listed. | An entry opens its trajectory once it is listed |
| Overview data, all trajectories | 1.66 | 0.59 | The ancestry walk behind the token total ran a fresh `match()` per trajectory and step. Parents are resolved to rows once. | Ancestry over gaps, cycles, and unknown ids |
| `investigation_build()`, 10,000 | about 750 (one profiled run) | about 530 (the 579 s snapshot less its diagnostics) | Replacing one element of a tibble list column per assessment copied the column each time, and the build re-hashed the payload twice to confirm identifiers it had just computed. | Loss rows for an omitted parent; a snapshot fingerprints once |
| `investigation_report()`, 10,000 | 27 plus the 358 s verification | 27 s with `verify = FALSE`; plus verification otherwise | The report re-verifies identifiers by default, so an edited value is refused. `verify = FALSE` skips the re-hash for a value just read or built, and the report then says its identifiers were not re-verified. | A report verifies identifiers unless told the value is fresh |

## Budgets

For this fixture on this machine. They are the numbers to compare a rerun
against, chosen with headroom of roughly a quarter over the baseline, not
promises about other hardware.

| Interaction | Budget | Baseline |
| --- | --- | --- |
| Narrow or widen the cohort: select rows, assemble and render the overview, render the list | 2.5 s | About 1.9 s for the whole cohort (0.59 + 0.52 + 0.78) and for the widest filter measured (text search, 5,753 rows: 0.06 + 0.53, then the same rendering) |
| Select rows for any filter | 0.1 s | 0.063 s (text search) |
| Start a session on prepared data | 10 s | 7.4 s |
| Prepare a snapshot's data, diagnostics included | 60 s | 49.5 s |
| Each diagnostic from a script (`scan_`, `assess_`, `measure_`) | 30 s | 7.8, 25.2, 21.4 s |
| Construct and validate a bundle | 20 s | 16.1 s |
| The app's report for the cohort in view | 1 s to assemble, 1 s to render | 0.60 s, 0.22 s |
| `investigation_report()` on a saved investigation, `verify = FALSE` | 30 s at 10,000; 2.5 s at 1,000 | 27 s; 1.8 s |
| Save or reopen a selection of 1,000 trajectories | 60 s each | 48 s write, 46 s read |

Saving or reopening all 10,000 trajectories has no budget because it does
not work on this machine. That is the main result of this profile, and it is
a property of the version 1 format, discussed below.

## Paging and caching

No cache was added. Every filter path recomputes its cohort from the
snapshot's tables in under 0.6 s and selects rows in under 0.1 s. A cache of
cohort tables per filter state would save at most that and would need
invalidating on every annotation, rescan, and reload.

No paging control was added. One bound was: the browser list renders the
first 500 entries in the current order. Rendering every entry took 25 s and
7 MB of HTML on each filter change, which is unusable, and the cost is in
htmltools rather than in scans, so it could not be removed by a local fix.
Five hundred keeps a list redraw under a second (0.78 s). This is a bound,
not paging: there is no page state, the count and every aggregate still
cover the whole cohort, next and previous step through all of it, and the
list says how many entries it left out and how to reach them. An open
trajectory past the first 500 (from a link, or by stepping) is shown after
that note with its position, marked as open, so the list never hides it.

## What remains slow, and why it was left

- **The saved-investigation fingerprint.** `investigation_hash()` encodes
  every scalar as its own JSON node and serializes the tree through
  jsonlite. One hash costs about 13 s per 1,000 trajectories of this shape,
  the file is 62 MB per 1,000, and at 10,000 writing exhausts 16 GB. Every
  step that verifies identifiers pays two hashes. The format and its
  identifiers were delivered earlier and are not redesigned here. A writer
  that produces the same bytes directly, checked against the current encoder
  on every fixture, would remove most of this without changing an
  identifier; a version 2 encoding is the alternative. Until then the
  supported use is a selected investigation of up to about 1,000
  trajectories.
- **Data preparation is per session.** The app shares the loaded bundle
  between sessions but derives `scans_app_data()` in each one, so each
  session waits 50 s at this size, and a change of scan settings recomputes
  summaries and measurements (22 s) that do not depend on them. Sharing the
  derived data per snapshot and scan settings is the first caching to add if
  cohorts of this size become routine. It was not added because one wait per
  session with a progress message is slow rather than unusable, and the
  issue asks for a measurement first.
- **`TrajectoryBundle()` validates twice**: once to build its own error and
  once in S7 (16.1 s against 8.6 s for one validation).
- **`measure_trajectories()`** (21 s) and the evidence checks inside
  `assess_trajectory_scans()` (about 10 of its 25 s) were not profiled
  further.
- **Click observers** are still one per listed entry. A single delegated
  input would remove them, at the cost of a change to the page script.
