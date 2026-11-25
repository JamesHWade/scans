# Run from the package checkout: shiny::runApp("tools/ux-preview", port = 7452)
# This preview uses deterministic responses and the real scans tools. No model
# or source provider is contacted. To use a provider, pass your own chat_factory
# to scans_app() instead of scans_test_chat().
local({
  root <- normalizePath(file.path(getwd(), "../.."))
  devtools::load_all(root, quiet = TRUE)
  source(file.path(root, "tests/testthat/helper-scans-tools.R"), local = TRUE)
  source(file.path(root, "tools/ux-preview/cohort.R"), local = TRUE)
  cohort <- as_trajectory_otel(
    demo_cohort_spans(),
    metadata = list(application = "Support assistant")
  )
  scans_app(
    list(
      "Support assistant" = cohort,
      "Small example" = function() scans_support_bundle()
    ),
    annotations = scans_annotations(tempfile(fileext = ".jsonl")),
    investigations = TRUE,
    reviews = TRUE,
    chat_factory = function() scans_test_chat(delay = 0.8)
  )
})
