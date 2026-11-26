# Custom scanners

Issue #43 asks for two domain-specific detectors built through the smallest
common function interface, reusing the #52 assessment contract. This note
records that interface. Validation against reviewed cases and the two
detectors are separate notes and slices.

## The interface

A custom scanner is a plain function plus the facts an assessment row needs:

```r
scanner(name, fn, description, required_evidence,
        version = "1", severity = "warning", settings = list())
```

`fn(trajectory, settings)` is called once per trajectory and returns either
`scanner_findings(...)` or `scanner_unassessed(status, reason)`. Scanners are
passed per call through a `scanners` argument on `scan_trajectories()`,
`assess_trajectory_scans()`, `investigation_snapshot()`, `scans_tools()`, and
`scans_app()`. There is no registration step and no global state. A scanner
exists for the calls it is passed to and nowhere else.

Three exported functions are the whole surface. Everything else a custom
finding needs (identities, scan name, version, severity, trajectory) is filled
in by the framework so that an author cannot get it wrong.

## Decisions

**Unassessed is a different return value, not an empty table.** A zero-row
`scanner_findings()` means "assessed, no findings". `scanner_unassessed()`
carries `"insufficient_evidence"` or `"not_applicable"` and a reason. Any
other value, including `NULL`, is an execution failure. A function that falls
off the end of an `if` therefore cannot report a clean negative by accident.
`scanner_findings()` marks its result with a class, and only a value with
that class is a findings result: a data frame built by hand, even an empty
one, is an execution failure, because an empty table from a failed query
looks exactly like a clean negative. Both result classes can be set by hand,
so the framework checks the fields again when a result comes back rather
than trusting the class.
The author decides which recorded losses matter, because only the author knows
which fields the detector reads; the framework lists the trajectory's losses
in the assessment's limitations either way.

**One trajectory at a time.** The function receives a list with
`trajectory_id`, `info`, `turns`, `events`, `evaluations`, and `losses`: the
rows the public accessors return, restricted to one trajectory, in turn and
event order, with unassigned losses included. This is what makes failure
isolation and assessments per scanner and trajectory possible. A whole-bundle
scanner was rejected: neither detector in #43 needs to compare trajectories,
and a bundle-level function has no natural per-trajectory failure boundary. A
one-trajectory `TrajectoryBundle` was rejected because constructing one needs
a parent-link decision (see saved investigations) that has no meaning here.

**Findings carry only what the author knows.** `label` and `explanation` are
required. `event_id`, `turn_id`, `event_ids`, and `value` are optional. A
finding with an event and no turn takes the event's turn; the primary event
always appears in `event_ids`. Severity belongs to the scanner, not the
finding, as it does for every built-in but one.

**The framework checks references.** Every `turn_id` and event identifier
must be in the scanned trajectory, a finding that names both a turn and a
primary event must name the event's own turn, and a `trajectory_id` column,
if present, must name the trajectory. One bad reference makes that scanner's whole result for that
trajectory an `execution_failure` and discards its findings. Dropping only the
bad finding was rejected: a scanner that cites records that do not exist is
wrong in a way that makes its other findings on that trajectory untrustworthy,
and a silently shorter findings table hides that.

**Failures stay inside one scanner and trajectory.** In
`assess_trajectory_scans()` a thrown error becomes `execution_failure` with a
fixed reason. The condition message is never copied into results, the app, or
a saved file, since it can contain anything. `scan_trajectories()` has no
place to record a failure, so it raises `scans_error_scanner_failure` with the
original condition as parent. That is the function to call while debugging a
scanner.

**Built-in results do not move.** Custom findings and assessments are appended
after the built-in rows. Built-in finding identifiers keep their existing
`<scan_id>/finding-<n>` sequence. Custom identifiers are
`<scan_id>/<name>/finding-<n>`, numbered per scanner in bundle trajectory
order, then by primary event, then in returned order. They do not shift when
another scanner is added, removed, or deselected. Like built-in identifiers
they are local to a run: they can shift if the bundle changes or a trajectory
that failed starts succeeding. The version is recorded on every finding and
assessment but is not part of the identifier, so a reviewer can compare two
versions on the same evidence.

**Names.** A name is lower-case snake case and must not be a built-in scan
name. That is checked when the scanner is created and again on every call, so
a later scans release that adds a built-in with the same name fails loudly
rather than mixing two detectors' results under one name. Two scanners with
the same name in one call are an error. A mandatory namespace prefix was
considered and rejected as more ceremony than two detectors justify; the
documentation suggests an application prefix.

**The registry is a listing, not a store.** `scan_registry()` still returns
the built-ins. `scan_registry(scanners)` returns the same three columns with
the given scanners appended, which is what the app uses to build its scan
selection. `scans = NULL` selects the built-ins plus every scanner passed.

## Consumers

A saved investigation stores custom findings, assessments, names, versions,
and settings as data. It never stores the function. Reopening shows the saved
results without it. Recomputing needs the scanner again:
`investigation_snapshot()` with a saved custom name and no `scanners` stops
with an error that says so, and the app names the saved scanners it cannot
rerun, both before and after "Apply current scanners".

A trajectory is assessed when any selected scan assessed it, so a scanner
that fails everywhere leaves trajectories "clean" if the built-ins assessed
them. The class stays as it is; instead the review queue lists "Execution
failure" as a reason, and the overview tile and the report headline count
the trajectories with at least one failed scan, built-in or custom. The
report is `investigation_report()`, which the app's Export > Report also
builds, so both read the count from `cohort_headline()` as the tile does.

**The report prints custom explanations as written.** It labels custom
patterns and evidence with the scan name in title case, as the overview
does, and escapes explanations in both its Markdown and HTML writers, so
one cannot add headings, table cells, links, or tags. Escaping does not
remove content: a custom scanner that quotes the transcript in an
explanation puts that text in the report. The report therefore names the
custom scans whose explanations it prints and qualifies its statement that
transcript text is omitted; with only built-in scans it is unchanged. A
saved investigation's custom results are reported from the saved analysis
whether or not the scanner is supplied, since the report never reruns a
scanner; saying which scans cannot be rerun is left to the app, where
rerunning is possible.

**Saved scans record their origin.** A name alone does not say which
detector produced a saved result: a later release could add a built-in with
the name of a saved custom scan, and the app would then show and rerun the
saved custom findings as the built-in. `manifest$scanner_versions` therefore
gains an `origin` column, `"builtin"` or `"custom"`, beside `scan` and
`scan_version`. The app counts a saved custom scan as rerunnable only when a
scanner of that name is supplied again, and a saved built-in only when it is
still installed; anything else is named as unavailable and is not selected
for "Apply current scanners".

The change is additive and leaves the format version and identifier rules
alone. The revision identifier hashes the saved payload as written, so a new
file's hash covers its `origin` column and an older file, which has none,
still hashes to the identifier stored in it. Validation accepts the
manifest's versions table with or without the column. For a file without it,
origin is inferred, never written back: a name is built-in when it is in the
registry of the release that wrote the file, otherwise custom. Every such
file was written by scans 0.0.0.9000, whose registry never held a name
outside the eleven built-ins present when origins started being saved, and a
custom scanner could never take a built-in name, so that fixed list is the
registry for all of them. It is a literal, not `scan_registry()`, so a
built-in added later does not change how an older file reads. A scan with no
assessment rows, which happens only with no selected trajectories, has no
row to record its origin and falls back to the same inference.

`scans_tools()` accepts `scanners` for a bundle and refuses them for a saved
investigation, whose analysis is fixed. The findings tool accepts any scan
name present in the analysis. The app's copied R snippet says that custom
scanners must be supplied, since the generated call stops without them.

## Not included

- Model-backed scanners. The interface says `fn` must be deterministic and
  must not call a model or tool; nothing enforces it. That scope decision is
  still open in #43.
- A positive result with scanner-declared limitations. Built-ins can report a
  finding while saying other findings cannot be ruled out. Neither detector
  needs it yet.
- Per-finding severity and per-finding metadata.
- Configuring a custom scanner's settings from the app. Settings are fixed
  when the scanner is created.
