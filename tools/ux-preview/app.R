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
  source(file.path(root, "tools/ux-preview/commons.R"), local = TRUE)
  deputy <- as_trajectory_otel(
    read_otel_traces(file.path(root, "tests/testthat/fixtures/deputy/otel")),
    metadata = list(application = "Deputy agent")
  )
  commons <- as_trajectory_commons(
    demo_commons_cohort(),
    metadata = list(application = "Commons analyst")
  )
  scans_app(
    list(
      "Support assistant" = cohort,
      "Deputy agent (captured trace)" = deputy,
      "Commons analyst" = commons,
      "Small example" = function() scans_support_bundle()
    ),
    annotations = scans_annotations(
      file.path(tempdir(), "scans-preview-annotations.jsonl")
    ),
    investigations = TRUE,
    reviews = TRUE,
    chat_factory = function() scans_test_chat(delay = 0.8)
  )
})
