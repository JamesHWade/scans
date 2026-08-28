scans_app_context_ui <- function(info) {
  fields <- c(
    "Trajectory" = info$trajectory_id[[1L]],
    "Run" = info$run_id[[1L]],
    "Parent" = info$parent_trajectory_id[[1L]],
    "Source" = info$source_type[[1L]],
    "Source ID" = info$source_id[[1L]],
    "Source URI" = info$source_uri[[1L]],
    "Task" = info$task_id[[1L]],
    "Sample" = info$sample_id[[1L]],
    "Epoch" = info$epoch[[1L]],
    "Agent" = info$agent[[1L]],
    "Model" = info$model[[1L]],
    "Started" = scans_app_time_string(info$started_at[[1L]]),
    "Completed" = scans_app_time_string(info$completed_at[[1L]]),
    "Error" = info$error[[1L]]
  )
  fields <- fields[!is.na(fields) & nzchar(fields)]
  metadata <- info$metadata[[1L]]
  has_metadata <- is.list(metadata) && length(metadata) > 0L
  if (length(fields) == 0L && !has_metadata) {
    return(scans_app_empty_ui(
      "No additional context is recorded.",
      compact = TRUE
    ))
  }
  htmltools::tags$dl(
    class = "scans-app-context",
    htmltools::tagList(Map(
      function(label, value) {
        htmltools::tagList(
          htmltools::tags$dt(label),
          htmltools::tags$dd(value)
        )
      },
      names(fields),
      fields
    )),
    if (has_metadata) {
      htmltools::tagList(
        htmltools::tags$dt("Metadata"),
        htmltools::tags$dd(
          htmltools::tags$pre(
            htmltools::tags$code(scans_app_value_text(metadata))
          )
        )
      )
    }
  )
}

scans_app_labeled_value <- function(label, value) {
  text <- scans_app_value_text(value)
  if (is.null(text)) {
    return(NULL)
  }
  htmltools::div(
    class = "scans-app-labeled-value",
    htmltools::tags$strong(label),
    htmltools::tags$pre(htmltools::tags$code(text))
  )
}

scans_app_badge <- function(text, tone = "quiet") {
  if (!scans_app_has_string(text)) {
    return(NULL)
  }
  htmltools::tags$span(
    class = paste0(
      "scans-app-badge scans-app-badge-",
      scans_app_css_token(tone)
    ),
    text
  )
}

scans_app_status_badge <- function(status) {
  if (!scans_app_has_string(status)) {
    return(NULL)
  }
  tone <- if (status %in% c("completed", "succeeded", "success", "passed")) {
    "success"
  } else if (status %in% c("failed", "error")) {
    "danger"
  } else {
    "quiet"
  }
  scans_app_badge(scans_app_title_case(status), tone)
}

scans_app_empty_ui <- function(text, compact = FALSE) {
  htmltools::div(
    class = paste(
      "scans-app-empty",
      if (compact) "scans-app-empty-compact" else ""
    ),
    text
  )
}

scans_app_value_text <- function(value, max_chars = 4000L) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }
  text <- paste(
    utils::capture.output(
      utils::str(
        value,
        max.level = 4L,
        give.attr = FALSE,
        strict.width = "cut",
        width = 80L
      )
    ),
    collapse = "\n"
  )
  scans_app_truncate(text, max_chars)
}

scans_app_truncate <- function(text, max_chars) {
  if (is.na(text) || nchar(text) <= max_chars) {
    return(text)
  }
  paste0(substr(text, 1L, max_chars - 1L), "\u2026")
}

scans_app_has_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

scans_app_title_case <- function(x) {
  if (!scans_app_has_string(x)) {
    return("")
  }
  x <- gsub("_", " ", x)
  paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

scans_app_css_token <- function(x) {
  token <- tolower(scans_app_first_string(x, "quiet"))
  gsub("[^a-z0-9-]+", "-", token)
}

scans_app_event_dom_id <- function(row) {
  paste0("scans-app-event-", row)
}

scans_app_time_string <- function(x) {
  if (is.na(x)) {
    return(NA_character_)
  }
  format(x, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
}

scans_app_dependency <- function(package_version = utils::packageVersion) {
  htmltools::htmlDependency(
    name = "scans-app",
    version = as.character(package_version("scans")),
    src = c(
      file = system.file("www", "scans-app", package = "scans")
    ),
    stylesheet = "scans-app.css"
  )
}

# Render model-authored markdown for the transcript.
#
# Model output is untrusted: it can contain raw HTML, and commonmark 2.0
# dropped the `sanitize` argument that used to strip it. So the rendered
# HTML is parsed and rebuilt against an allowlist of elements and
# attributes -- a node filter rather than a regex, because regexes do not
# reliably see tag boundaries. Without commonmark or xml2 the text is shown
# verbatim, which is honest rather than merely unformatted-looking.
scans_app_markdown <- function(text) {
  if (!scans_app_has_string(text)) {
    return(NULL)
  }
  usable <- requireNamespace("commonmark", quietly = TRUE) &&
    requireNamespace("xml2", quietly = TRUE)
  if (!usable) {
    return(htmltools::div(
      class = "scans-app-prose scans-app-prose-plain",
      text
    ))
  }
  html <- commonmark::markdown_html(
    text,
    smart = TRUE,
    extensions = c("table", "strikethrough", "autolink")
  )
  htmltools::div(
    class = "scans-app-prose",
    htmltools::HTML(scans_app_sanitize_html(html))
  )
}

scans_app_allowed_tags <- c(
  "p",
  "br",
  "hr",
  "em",
  "strong",
  "del",
  "code",
  "pre",
  "blockquote",
  "ul",
  "ol",
  "li",
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",
  "a",
  "span",
  "table",
  "thead",
  "tbody",
  "tr",
  "th",
  "td"
)

# Elements that carry a payload rather than readable children. Unwrapping
# these would delete what the model actually emitted, and a transcript that
# quietly drops content is worse than useless for diagnosis -- so they are
# shown as their own source text instead.
scans_app_opaque_tags <- c(
  "script",
  "style",
  "iframe",
  "object",
  "embed",
  "template",
  "svg",
  "math",
  "img",
  "input",
  "link",
  "meta",
  "base",
  "form",
  "video",
  "audio",
  "source"
)

scans_app_allowed_attrs <- list(
  a = c("href", "title"),
  th = c("align", "colspan", "rowspan"),
  td = c("align", "colspan", "rowspan")
)

# Keep only allowlisted elements and attributes. Nothing is deleted: an
# element with readable children is unwrapped so its text survives, and an
# opaque one is rewritten as inline code showing its markup verbatim. The
# result is that a model emitting `<script>` is visible in the transcript as
# text -- which is exactly what someone inspecting a trajectory needs to see
# -- while never being live markup in the page.
scans_app_sanitize_html <- function(html) {
  root_name <- "scans-sanitizer-root"
  root_close <- paste0("(?i)</\\s*", root_name, "\\s*>")
  html <- gsub(
    root_close,
    paste0("&lt;/", root_name, "&gt;"),
    html,
    perl = TRUE
  )
  doc <- xml2::read_html(paste0(
    "<",
    root_name,
    ">",
    html,
    "</",
    root_name,
    ">"
  ))
  root <- xml2::xml_find_first(doc, paste0("//", root_name))
  query <- paste0(
    ".//*[not(self::",
    paste(scans_app_allowed_tags, collapse = " or self::"),
    ")]"
  )
  # Bounded because rewriting a node re-queries the tree; a node that somehow
  # resisted both branches would otherwise spin here.
  for (pass in seq_len(20L)) {
    bad <- xml2::xml_find_all(root, query)
    if (length(bad) == 0L) {
      break
    }
    for (node in bad) {
      if (xml2::xml_name(node) %in% scans_app_opaque_tags) {
        scans_app_escape_node(node)
      } else {
        scans_app_unwrap_node(node)
      }
    }
  }
  for (node in xml2::xml_find_all(root, ".//*")) {
    name <- xml2::xml_name(node)
    keep <- scans_app_allowed_attrs[[name]] %||% character()
    for (attr in setdiff(names(xml2::xml_attrs(node)), keep)) {
      xml2::xml_attr(node, attr) <- NULL
    }
    if (identical(name, "a")) {
      href <- xml2::xml_attr(node, "href")
      if (!is.na(href) && !grepl("^(https?:|mailto:|#|/|\\.)", href)) {
        xml2::xml_attr(node, "href") <- NULL
      }
    }
  }
  paste(
    vapply(xml2::xml_contents(root), as.character, character(1)),
    collapse = ""
  )
}

# Rewrite a node as inline code holding its own serialized markup.
#
# The replacement is parsed from pre-escaped text rather than renamed in
# place: libxml2 keeps script and style content as raw text, so a renamed
# node would serialize its payload back out as live markup.
scans_app_escape_node <- function(node) {
  raw <- as.character(node)
  fragment <- xml2::read_html(paste0(
    "<div><code>",
    htmltools::htmlEscape(raw),
    "</code></div>"
  ))
  xml2::xml_replace(node, xml2::xml_find_first(fragment, "//code"))
  invisible(NULL)
}

scans_app_unwrap_node <- function(node) {
  for (child in rev(xml2::xml_contents(node))) {
    xml2::xml_add_sibling(node, child, .where = "after")
  }
  xml2::xml_remove(node)
}
