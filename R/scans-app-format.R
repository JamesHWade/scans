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
        htmltools::tags$dt(class = "scans-app-context-section", "Metadata"),
        htmltools::tags$dd(
          class = "scans-app-context-metadata",
          scans_app_metadata_ui(metadata)
        )
      )
    }
  )
}

# Flatten metadata to dotted keys and bound each displayed value.
scans_app_metadata_ui <- function(metadata, max_chars = 300L) {
  entries <- scans_app_flatten_metadata(metadata)
  if (length(entries) == 0L) {
    return(scans_app_empty_ui("No metadata recorded.", compact = TRUE))
  }
  htmltools::tags$dl(
    class = "scans-app-metadata",
    htmltools::tagList(Map(
      function(key, value) {
        htmltools::tagList(
          htmltools::tags$dt(key),
          htmltools::tags$dd(scans_app_truncate(value, max_chars))
        )
      },
      names(entries),
      entries
    ))
  )
}

scans_app_flatten_metadata <- function(value, prefix = "", depth = 0L) {
  if (depth > 6L) {
    return(stats::setNames(list(scans_app_value_text(value)), prefix))
  }
  if (is.list(value)) {
    if (length(value) == 0L) {
      return(list())
    }
    keys <- names(value)
    if (is.null(keys)) {
      keys <- as.character(seq_along(value))
    }
    blank <- is.na(keys) | !nzchar(keys)
    keys[blank] <- as.character(which(blank))
    entries <- Map(
      function(key, item) {
        scans_app_flatten_metadata(
          item,
          prefix = if (nzchar(prefix)) paste(prefix, key, sep = ".") else key,
          depth = depth + 1L
        )
      },
      keys,
      value
    )
    return(do.call(c, unname(entries)))
  }
  if (is.null(value) || length(value) == 0L) {
    return(list())
  }
  if (is.atomic(value) && length(value) == 1L && is.na(value)) {
    return(list())
  }
  text <- if (is.atomic(value) && length(value) == 1L) {
    if (is.numeric(value)) {
      format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
    } else {
      as.character(value)
    }
  } else if (is.atomic(value)) {
    paste(as.character(value), collapse = ", ")
  } else {
    scans_app_value_text(value)
  }
  stats::setNames(list(text), prefix)
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

# Format scalars and flat named lists directly; use JSON for nested values
# and str() for objects JSON cannot represent.
scans_app_value_text <- function(value, max_chars = 4000L) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }
  text <- scans_app_format_value(value)
  scans_app_truncate(text, max_chars)
}

scans_app_format_value <- function(value) {
  if (is.atomic(value) && is.null(names(value))) {
    if (length(value) == 1L) {
      return(scans_app_format_scalar(value))
    }
    if (length(value) <= 20L) {
      return(paste(
        vapply(value, scans_app_format_scalar, character(1)),
        collapse = ", "
      ))
    }
  }
  is_flat <- (is.list(value) || is.atomic(value)) &&
    !is.null(names(value)) &&
    all(nzchar(names(value))) &&
    all(vapply(
      value,
      function(x) is.atomic(x) && length(x) == 1L,
      logical(1)
    ))
  if (is_flat) {
    return(paste(
      names(value),
      vapply(value, scans_app_format_scalar, character(1)),
      sep = ": ",
      collapse = "\n"
    ))
  }
  if (rlang::is_installed("jsonlite")) {
    json <- tryCatch(
      jsonlite::toJSON(
        value,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        na = "null",
        force = TRUE,
        digits = NA
      ),
      error = function(cnd) NULL
    )
    if (!is.null(json)) {
      return(as.character(json))
    }
  }
  paste(
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
}

scans_app_format_scalar <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  if (is.character(x)) {
    return(x)
  }
  format(x, scientific = FALSE, trim = TRUE)
}

# Payload text in the transcript is bounded so one oversized tool result
# cannot stall the browser; the marker says how much was left out.
scans_app_text_limit <- 20000L

scans_app_bounded_text <- function(text, max_chars = scans_app_text_limit) {
  if (is.na(text) || nchar(text) <= max_chars) {
    return(text)
  }
  omitted <- nchar(text) - max_chars
  paste0(
    substr(text, 1L, max_chars),
    "\n\u2026 [truncated: ",
    format(omitted, big.mark = ","),
    " more characters]"
  )
}

# Remove Markdown formatting from titles and list snippets.
scans_app_strip_markdown <- function(text) {
  text <- gsub("```[^`]*```", " ", text)
  text <- gsub("`([^`]*)`", "\\1", text)
  text <- gsub("!\\[([^]]*)\\]\\([^)]*\\)", "\\1", text)
  text <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", text)
  text <- gsub("(^|\\s)#{1,6}\\s+", "\\1", text)
  text <- gsub("(\\*\\*|__)(.+?)\\1", "\\2", text, perl = TRUE)
  text <- gsub("(^|[^*\\w])[*_]([^*_]+)[*_]", "\\1\\2", text)
  text <- gsub("(^|\\n)\\s*(>|[-*+]|\\d+\\.)\\s+", "\\1", text)
  text
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
    stylesheet = "scans-app.css",
    script = "scans-app.js"
  )
}

# Treat model output as untrusted HTML. Parse rendered Markdown and retain
# only allowed elements and attributes. Commonmark 2.0 removed its sanitize
# argument, so sanitization is handled here. Without commonmark or xml2,
# display the original text.
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

# Display these elements as escaped source to preserve their payload.
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
  "source",
  # libxml2 parses the content of these as raw text and flags those text
  # nodes as not-to-be-encoded, so an unwrapped payload would serialize
  # back out as live markup.
  "plaintext",
  "xmp",
  "listing",
  "noframes",
  "noembed",
  "noscript",
  "textarea",
  "title"
)

scans_app_allowed_attrs <- list(
  a = c("href", "title"),
  th = c("align", "colspan", "rowspan"),
  td = c("align", "colspan", "rowspan")
)

# Retain allowed elements and attributes. Unwrap readable children and
# escape opaque elements so unsafe markup remains visible as text.
scans_app_sanitize_html <- function(html) {
  root_name <- "scans-sanitizer-root"
  html <- scans_app_escape_html_declarations(html)
  # `<plaintext>` swallows the rest of the document, including the wrapper's
  # closing tag, so it is neutralised before parsing rather than as a node.
  html <- gsub("(?i)<(/?plaintext\\b)", "&lt;\\1", html, perl = TRUE)
  root_close <- paste0(
    "(?i)<(/\\s*(?:html|body|",
    root_name,
    ")\\b[^>]*>)"
  )
  html <- gsub(
    root_close,
    "&lt;\\1",
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
  hidden <- xml2::xml_find_all(
    root,
    ".//comment() | .//processing-instruction()"
  )
  for (node in hidden) {
    scans_app_escape_node(node)
  }
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
      if (
        xml2::xml_name(node) %in%
          scans_app_opaque_tags ||
          length(xml2::xml_contents(node)) == 0L
      ) {
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

scans_app_escape_html_declarations <- function(html) {
  locations <- gregexpr("(?i)<!DOCTYPE\\b[^>]*>", html, perl = TRUE)
  declarations <- regmatches(html, locations)
  escaped <- lapply(declarations, function(matches) {
    vapply(
      matches,
      function(match) as.character(htmltools::htmlEscape(match)),
      character(1)
    )
  })
  regmatches(html, locations) <- escaped
  html
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

# Text children are rebuilt rather than moved: libxml2 marks the text inside
# raw-text elements (script, xmp, noframes, ...) as not-to-be-encoded, and a
# moved node keeps that flag and serializes its `<` unescaped.
scans_app_unwrap_node <- function(node) {
  for (child in rev(xml2::xml_contents(node))) {
    if (identical(xml2::xml_type(child), "text")) {
      child <- scans_app_text_node(xml2::xml_text(child))
      if (is.null(child)) {
        next
      }
    }
    xml2::xml_add_sibling(node, child, .where = "after")
  }
  xml2::xml_remove(node)
}

scans_app_text_node <- function(text) {
  if (!nzchar(text)) {
    return(NULL)
  }
  fragment <- xml2::read_html(paste0(
    "<div>",
    htmltools::htmlEscape(text),
    "</div>"
  ))
  xml2::xml_contents(xml2::xml_find_first(fragment, "//div"))[[1]]
}
