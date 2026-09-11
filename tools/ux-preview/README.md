# Investigation workspace preview

From the package checkout, run:

```r
shiny::runApp("tools/ux-preview", port = 7452, launch.browser = TRUE)
```

This uses the bundled support-assistant recording, the real scans tools, and a
deterministic chat fixture. The Ask pane labels its responses as fixture output;
it makes no model requests. It exercises native shinychat streaming, suggestions,
tool disclosure, and evidence navigation.

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

Validation evidence and visual comparisons are in `../../artifacts/ux-review/`.
The full package check used a temporary library containing the matching Tempest
source: the normally installed Tempest has an older promotion schema digest and
cannot validate the repository's existing review fixture. No regular library or
other checkout was modified.
