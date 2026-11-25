# Investigation workspace preview

From the package checkout, run:

```r
shiny::runApp("tools/ux-preview", port = 7452, launch.browser = TRUE)
```

The preview serves two sources, a deterministic chat fixture, and the real scans
tools. The Ask pane labels its responses as fixture output; it makes no model
requests.

- **Support assistant** is the demo cohort from `cohort.R`: 240 authored,
  provider-free conversations over a fortnight, shaped like the OpenTelemetry
  GenAI spans a deployed ellmer app writes to Posit Connect. Its overview should
  make three things obvious: an order-service outage on Nov 19 and 20
  (`lookup_order` timeouts and retry loops), an intermittently failing
  `refund_status`, and a model change on Nov 18 that raises token use. Select a
  bar, a tool, a user, or a model to narrow the overview and the browser
  together; the chips above the trajectory list remove each filter.
- **Small example** is the bundled support-assistant recording. It exercises
  native shinychat streaming, suggestions, tool disclosure, and evidence
  navigation, and shows how the overview degrades for a handful of trajectories.

For a configured provider, load the checkout and use the public API:

```r
devtools::load_all()
bundle <- as_trajectory_otel(jsonlite::read_json(system.file(
  "extdata", "support-investigation.json", package = "scans"
)))
scans_app(
  list("Support assistant example" = bundle),
  chat_factory = function() ellmer::chat_openai()
)
```

Omit `chat_factory` for ordinary inspection without Ask. The app requires a recent
shinychat with `page_chat_theme()`; Ask also requires `chat_server()$set_client()`.

Start on Overview, select a pattern or inspect the suggested trajectory, then
follow a finding or an Ask citation. At narrow widths, use the Trajectories and
Findings / Ask drawers. A citation closes the drawer and focuses the recorded
event, including results nested inside a paired tool call.

The package also exports `scans_tools(bundle)` for use in an ellmer chat outside
the app. It binds a copy of the evidence and computes summaries over the selected
population, independently of result pagination.

The full package check used a temporary library containing the matching Tempest
source: the normally installed Tempest has an older promotion schema digest and
cannot validate the repository's existing review fixture. No regular library or
other checkout was modified.
