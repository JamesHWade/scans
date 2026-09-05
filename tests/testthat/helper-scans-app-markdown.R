skip_if_no_markdown <- function() {
  skip_if_not_installed("commonmark")
  skip_if_not_installed("xml2")
  skip_if_not_installed("htmltools")
}

render_markdown <- function(text) {
  as.character(htmltools::renderTags(scans_app_markdown(text))$html)
}
