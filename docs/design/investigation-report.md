# Investigation report

The second delivery for #44 is a report that an R script can render from a
saved investigation and that agrees with the app. It is the second consumer
the [direction](application-diagnostics-direction.md) asked for before cohort
analysis gained a public interface. Save and reopen are unchanged; see
[saved investigations](investigation-snapshots.md).

## Public boundary

`investigation_report(x, reviews, app_url, include_users, include_errors)`
takes a saved
investigation (or a bundle, which it snapshots with the defaults) and returns
a `scans_investigation_report`: scope details and ordinary tibbles named
`headline`, `patterns`, `tools`, `models`, `users`, `measures`, `coverage`,
`losses`, `trajectories`, `evidence`, and `reviews`.
`write_investigation_report()` writes one standalone HTML file or Markdown;
`format()` returns the same text. Nothing else is exported.

The report value is the public cohort interface. A script that wants the
overview's numbers reads them from it:

```r
report <- investigation_report(read_investigation("support.scans.json"))
report$headline[c("n", "n_assessed", "n_not_assessed", "n_findings")]
report$patterns[c("scan", "trajectories", "n", "assessed")]
report$evidence[c("scan", "trajectory_id", "event_ids")]
```

The individual cohort functions stay internal. The report showed which
results a second consumer needs, but not that callers need to compose the
steps themselves: every table depends on the same filtered identities, and a
caller who computed one table from different identities than another would
produce numbers that disagree with the app. One function that fixes the
cohort and returns every table rules that out.

### Alternatives rejected

- Export `cohort_headline()`, `cohort_patterns()`, and the rest. They take the
  app's internal data list and precomputed intermediate tables. Exporting
  them would freeze that list as a contract and let two tables be computed
  for different cohorts.
- A general `analyze_cohort(bundle, filters = ...)`. The only filters with a
  defined meaning are the ones a saved view records. A second filter language
  would need its own validation and would have no consumer.
- A report function that returns only text. Then a script that wants one
  number would parse Markdown, and the agreement test could only compare
  strings.
- A Quarto template. It adds a system dependency for a document with no
  executable content, and the app could not produce the same bytes without
  Quarto installed on the server.

## One computation

`investigation_report()` builds the same data the app builds when it opens
the investigation (`scans_app_data()` with the saved analysis), selects rows
with the saved view through `scans_app_view_indices()`, and takes every count
from `scans_app_performance_data()`, the function the overview renders from.
The report adds no count of its own. Three pieces of logic that lived in
Shiny reactives or inline in the overview assembly moved down so both callers
share them:

- which rows a view selects (`scans_app_view_indices()`), and the review
  filter (`scans_app_review_indices()`);
- measurement coverage (`cohort_measures()`), which the overview's work
  medians now read;
- capture losses by field and reason (`cohort_losses()`).

`patterns$assessed` is the number of trajectories in scope each scan could
assess, from `cohort_patterns()`, and a pattern's reach (`share`) is over
that number, as the overview states it: a trajectory the scan could not
assess is not an unaffected one. Model and user breakdowns count findings
over their `n_assessed` trajectories and print "not assessed" when that is
zero.

The cohort is the saved selection narrowed by the saved view. Facets and
review-state filters are session state in the app and are not part of a saved
view; a selection saved while they were active already reflects them, and the
report states how many trajectories the selection left out of its source.

## The app's report

The overview's Export menu builds its report through the same assembly
(`report_build()`), passing the session's data, visible rows, filters, and
address, and offers it as HTML and as Markdown. For an opened investigation
the downloaded files equal the ones `write_investigation_report()` writes for
that investigation and address, apart from the generation time; a test holds
this. A live view has no snapshot identifiers and can carry facets and review
filters, which the report names. After "Apply current scanners" the report
keeps the snapshot identifier and says the analysis is not a saved revision.
When the filters on screen differ from the saved view (a changed source,
status, text, pattern, or annotation switch, or any facet or review filter),
the report likewise keeps the snapshot identifier, which still identifies the
evidence, and omits the revision identifier, saying the view was changed
since the save: that revision covers a different view.

The annotation filter of an opened investigation has two sources. Without an
annotation store the app uses the membership saved with the investigation,
and its report is the script's ("Annotated when saved"). With a store it uses
the store's annotations as they are when the report is made. The same
investigation can then report a different cohort, so the app's report names
the filter as the store's ("Annotated in this app's annotation store at
export"), omits the revision identifier, and says a link reopens that cohort
only where the same annotations are recorded. A script never sees a store and
always reports the saved membership. Making the app ignore its store for a
saved view was rejected: the switch on screen filters by the store, and the
report must describe the cohort on screen.
The earlier in-app Markdown builder is removed. The app does not pass its
session-local review cases to the report yet.

## Scope, unknowns, and limits

The scope names every active filter in words, including source, status, text,
and annotation filters, which the earlier in-app Markdown report left out. It
states the window of the trajectories in scope separately from what the
source read covered. A cap is reported when the read recorded one, "no cap"
only when the read recorded that it had none, and unknown otherwise.

A trajectory no selected scan could assess is counted as not assessed, listed
under Evidence, and never folded into the clean count. When nothing was
assessed the headline says findings are unknown. A measurement that was not
recorded prints as unknown. Measurement medians cover completely recorded
values only, with the coverage counts beside them.

## Evidence and links

Each pattern lists the trajectories it counted and, per finding, the exact
turn and event identities, with an anchor per finding. With `app_url` the
identities link to that trajectory and event in the app.

A link carries filter values, not trajectory identities. Link scope gained
`source`, `status`, `status_unknown`, `query`, and `annotated`, and the app
now applies them, so a link from the app no longer reopens a wider cohort
than the report it came from.

Every link the report or an export writes names its scope, and a scope with
no filters is written `?scope=none`. Opening a link with a scope replaces
every filter in the app, so a link never inherits filters the opening app
happened to have set. A hash without `?scope=`, which is what the address bar
shows while browsing and what older links carry, sets no filters, so reload
and back-and-forward keep the reader's filters. The app's own "Copy link"
and the CSV link columns use the first form too. A
link filter the opening app cannot apply (a source, status, pattern, or
facet value it does not hold, or an annotation or review filter without a
store) is dropped with a notice naming it.

Three limits remain, and the report prints each one that applies:

- A link reopens exactly the reported cohort only when the app serves the
  same saved investigation. Against a live source the same filters select
  from whatever it holds then.
- The annotation and review filters depend on annotations where the link is
  opened. An app that cannot apply one says so and names the wider cohort.
- A user filter, and a text filter whose term matches a user identifier, are
  left out of links unless user identifiers were requested. The link still
  clears every other filter, so it opens the cohort the remaining filters
  select, and the report says that is wider than the one it describes.

## Content

The default is identities and scanner explanations, not content. The report
never prints message text, tool arguments, or tool results, whatever the
saved content policy retains, so it is always within that policy. It does
print tool names, model names, and identifiers. Recorded error strings are
often tool-result text, so the tool table counts each tool's most frequent
error without its text unless `include_errors = TRUE`; printing them by
default would contradict the report's own statement that it holds no tool
results.

Source details a script records with an investigation are free-form and
can name a person (`user_id`, an account, a host). Their values are printed
only for the fields a source reader records about its read (window, cap,
counts, files); any other field is named with its value withheld unless
`include_users = TRUE`. An allow-list is the smallest rule that cannot
leak: a deny-list of identifying names would miss the next one.
Explanations are scanner-written sentences; a custom scanner that quotes
transcript text in an explanation would put that text in the report.

User identifiers are omitted unless `include_users = TRUE`. The user count
per pattern is a count and stays. A text filter is described without its term
when the term contains, or is contained in, a user identifier recorded in the
snapshot; this can also hide an unrelated short term, which is the safer
error.

Recorded text reaches the output only as plain text or code spans in a block
layout that each writer escapes for its format. The HTML file has inline CSS,
no script, and no external asset. `app_url` must be an `http` or `https`
base address: host, optional port, and a path limited to characters neither
writer reads as markup, with no credentials, query, or fragment, since every
link copies it.

## Review decisions

A saved investigation carries no review decisions; they live in
`review_case()` values that record the snapshot they were made from. The
report takes those values through `reviews`, rejects a case from another
snapshot, and shows each case's `review_status()`: its single active
judgment, disputed, or unreviewed. It links a case to its finding's evidence
and marks a case made on another revision. Rationales and expected behavior
are free text and are not printed; reviewer names appear only with
`include_users = TRUE`.

## Not done

- No charts. The activity series is in the app; a static chart would need a
  second rendering of it.
- The rendered evidence lists at most `max_evidence` trajectories per pattern
  and says when it lists fewer than it counted. The `evidence` table is
  always complete.
- No per-pattern review tallies. The supplied cases are listed; aggregating
  them would be a new count the app does not show.
