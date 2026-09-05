test_that("resource labels follow measure identity when rows are reordered or extended", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("xml2")
  data <- scans_app_data(performance_fixture())
  rows <- data$measures[data$measures$trajectory_id == "fast", ]
  data$measures <- rows[
    match(c("output_tokens", "elapsed", "input_tokens"), rows$measure),
  ]
  html <- as.character(scans_app_resources_ui(data, 1L))
  summaries <- xml2::xml_find_all(
    xml2::read_html(html),
    "//details/details/summary"
  )
  expect_identical(
    stats::setNames(
      xml2::xml_text(xml2::xml_find_all(summaries, "./strong")),
      xml2::xml_text(xml2::xml_find_all(summaries, "./span"))
    ),
    c(
      "Output tokens" = "5",
      "Conversation elapsed" = "10.0 s",
      "Input tokens" = "10"
    )
  )

  data$measures <- rows[rows$measure == "elapsed", ]
  data$measures$measure <- "additional_duration"
  html <- as.character(scans_app_resources_ui(data, 1L))
  expect_match(html, "<span>additional_duration</span>", fixed = TRUE)
  expect_no_match(html, "Conversation elapsed", fixed = TRUE)
})
