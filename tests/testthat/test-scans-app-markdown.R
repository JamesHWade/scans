skip_if_no_markdown <- function() {
  skip_if_not_installed("commonmark")
  skip_if_not_installed("xml2")
  skip_if_not_installed("htmltools")
}

render <- function(text) {
  as.character(htmltools::renderTags(scans_app_markdown(text))$html)
}

test_that("markdown structure is rendered, not shown as source", {
  skip_if_no_markdown()
  html <- render("## Heading\n\n- one\n- two\n\n**bold** and `code`")
  expect_match(html, "<h2>Heading</h2>", fixed = TRUE)
  expect_match(html, "<li>one</li>", fixed = TRUE)
  expect_match(html, "<strong>bold</strong>", fixed = TRUE)
  expect_match(html, "<code>code</code>", fixed = TRUE)
  expect_no_match(html, "## Heading", fixed = TRUE)
})

test_that("tables render, since assistants answer with them", {
  skip_if_no_markdown()
  html <- render("| a | b |\n|---|---|\n| 1 | 2 |")
  expect_match(html, "<table>", fixed = TRUE)
  expect_match(html, "<th>a</th>", fixed = TRUE)
  expect_match(html, "<td>2</td>", fixed = TRUE)
})

test_that("opaque markup is shown as text rather than dropped or executed", {
  skip_if_no_markdown()
  html <- render("Before <script>alert(1)</script> after.")
  # Visible to whoever is diagnosing the trajectory...
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  # ...but never live markup, and the surrounding prose survives.
  expect_no_match(html, "<script", fixed = TRUE)
  expect_match(html, "Before", fixed = TRUE)
  expect_match(html, "after.", fixed = TRUE)
})

test_that("event handlers and unsafe link schemes do not survive", {
  skip_if_no_markdown()
  html <- render(
    "<img src=x onerror=alert(1)>\n\n[bad](javascript:evil()) [ok](https://example.com)"
  )
  doc <- xml2::read_html(html)
  attrs <- unlist(lapply(
    xml2::xml_find_all(doc, "//*"),
    function(node) names(xml2::xml_attrs(node))
  ))
  expect_false(any(grepl("^on", attrs)))
  expect_match(html, "https://example.com", fixed = TRUE)
  expect_no_match(html, "javascript:", fixed = TRUE)
})

test_that("unknown containers keep their text", {
  skip_if_no_markdown()
  html <- render("<div class=\"wrapper\">kept text</div>")
  expect_match(html, "kept text", fixed = TRUE)
  expect_no_match(html, "wrapper", fixed = TRUE)
})

test_that("HTML comments remain visible as escaped source", {
  skip_if_no_markdown()
  html <- render("Before <!-- diagnostic detail --> after.")
  expect_match(html, "&lt;!-- diagnostic detail --&gt;", fixed = TRUE)
  expect_no_match(html, "<!-- diagnostic detail -->", fixed = TRUE)

  html <- scans_app_sanitize_html(
    "<p>Before</p><?review hidden?><p>After</p>"
  )
  expect_match(html, "&lt;!--?review hidden?--&gt;", fixed = TRUE)
})

test_that("empty unknown elements remain visible as escaped source", {
  skip_if_no_markdown()
  html <- render("Before <citation id=\"source-1\"></citation> after.")
  expect_match(html, "&lt;citation id=\"source-1\"&gt;", fixed = TRUE)
  expect_match(html, "&lt;/citation&gt;", fixed = TRUE)
})

test_that("raw closing tags cannot hide later model output", {
  skip_if_no_markdown()
  closing_tags <- c(
    "</div>",
    "</body>",
    "</html>",
    "</scans-sanitizer-root/>",
    "</scans-sanitizer-root x>"
  )

  for (closing_tag in closing_tags) {
    html <- scans_app_sanitize_html(paste0(
      "<p>Before</p>",
      closing_tag,
      "<p>After</p>"
    ))
    expect_match(html, "Before", fixed = TRUE)
    expect_match(html, "After", fixed = TRUE)
  }
})

test_that("empty and missing text render nothing", {
  skip_if_no_markdown()
  expect_null(scans_app_markdown(NA_character_))
  expect_null(scans_app_markdown(""))
  expect_null(scans_app_markdown(NULL))
})
