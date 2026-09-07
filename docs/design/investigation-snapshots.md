# Saved investigations

The first delivery for #44 lets an R user save a selected investigation and
reopen it without contacting its source or rerunning diagnostics. Reviewed
judgments, report rendering, and a persistence service remain separate work.

## Public boundary

`investigation_snapshot()` takes a `TrajectoryBundle`, explicit trajectory
selection, application name, capture details, scanner settings, and view state.
It returns a `scans_investigation` value containing the selected bundle,
settings, saved analysis, view, and manifest. `write_investigation()` and
`read_investigation()` write and validate that value. `scans_app()` accepts it
alongside existing bundle sources. These are ordinary snapshot values, not a
live store or a new execution layer.

The saved analysis includes summaries, findings, assessments, and resource
measurements. Opening uses those results even if installed scanner code has
changed. Changing scan settings explicitly derives a current analysis; saving
that result creates a new revision and records its parent revision. Scanner
versions and package version travel with the analysis.

## Identity and format

Version 1 is a JSON envelope with a small typed-data encoding. It supports
base atomic vectors, lists, data frames, factors, dates, and UTC times,
including typed missing values and empty vectors. Double values use C99
hexadecimal strings to preserve their exact binary value across JSON round trips;
raw bytes use base64. Attributes are encoded in sorted name order. Unsupported objects fail
with a field path; no source fields are silently discarded. Reading validates
the envelope and allowed types before constructing the canonical bundle. It
does not deserialize R code, load classes from the file, or fetch sources.

A SHA-256 snapshot identifier covers the selected evidence, application, and
source capture details. A separate revision identifier also covers saved
analysis, settings, view, content policy, and parent revision. Write time is
informational and does not change either identifier. The encoding and hashing
rules are versioned together. Integrity checks detect edits, not authorship.
Unsupported versions and invalid references fail explicitly.

Only selected trajectory records and their turns, events, and evaluations
enter the file. Relevant losses, including unassigned capture-wide losses,
remain visible. A selected child whose parent is excluded retains the parent
identity as an explicit selection loss instead of importing unselected data.
Finding and loss references are remapped consistently within the snapshot.

## Content and reopening

The initial policy is retained content: selected text, arguments, results,
evaluations, and metadata are included as held in the bundle. Existing adapter
redactions and losses remain intact. Saving does not promise to identify all
sensitive text. The app shows the selected count and this policy before a
user downloads the file. Unselected records, credentials held by loaders,
live connections, and annotation history are outside the file.

View state records filters, ordering, focused trajectory, and the selected
cohort. Annotation-filter membership can be retained as trajectory identities;
judgments and reviewer text are not exported by this delivery. Reopening is
session-local in the app and does not replace another user's source cache.
Malformed uploads leave the current investigation available.

The acceptance recipe saves complete and partial captures, closes the app,
and reopens the file to the same cohort and evidence. Tests cover saved
analysis, content omission, identity stability, changed evidence/settings,
unsupported versions/types, tampering, and app state restoration. A report
consumer and a measured large-cohort baseline remain follow-up work in #44.
