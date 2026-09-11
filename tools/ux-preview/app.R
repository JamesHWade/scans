# Run from the package checkout: shiny::runApp("tools/ux-preview", port = 7452)
# This preview uses deterministic responses and the real scans tools. No model
# or source provider is contacted. To use a provider, pass your own chat_factory
# to scans_app() instead of scans_test_chat().
local({
  root <- normalizePath(file.path(getwd(), "../.."))
  devtools::load_all(root, quiet = TRUE)
  source(file.path(root, "tests/testthat/helper-scans-tools.R"), local = TRUE)
  scans_app(
    list("Support assistant example" = scans_support_bundle()),
    investigations = TRUE,
    chat_factory = function() scans_test_chat(delay = 0.8)
  )
})
