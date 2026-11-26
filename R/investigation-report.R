#' Report a saved investigation
#'
#' `investigation_report()` computes a shareable report from a saved
#' investigation without starting the app. It applies the saved view's
#' filters to the saved evidence and analysis, exactly as [scans_app()] does
#' when it opens the same investigation, and returns the resulting tables.
#' `write_investigation_report()` renders them as one standalone HTML file or
#' as Markdown; `format()` returns the same text.
#'
#' The report states its scope (application, time window, every active
#' filter, and what the source read covered), headline counts with their
#' denominators, patterns with reach, tool failures, resource measurements
#' with recording coverage, capture limits and losses, and the evidence
#' behind each pattern. It never runs a scanner, calls a model, or contacts
#' the source.
#'
#' @section What the report can and cannot establish:
#' Findings describe recorded behavior. A trajectory with no findings is
#' reported as clean only when a selected scan could assess it; one that no
#' scan could assess is counted as not assessed. A measurement that was not
#' recorded is printed as unknown, never as zero. No findings does not
#' establish success, and complete recording of the loaded records does not
#' establish that the source captured everything.
#'
#' @section Content:
#' The report contains identities and scanner explanations, not transcript
#' text: no message text, tool arguments, or tool results. Tool names, model
#' names, and trajectory, turn, and event identifiers do appear. Recorded
#' error strings are often tool-result text, so the tool table counts errors
#' without their text unless `include_errors = TRUE`. User identifiers are
#' omitted unless `include_users = TRUE`; a text filter that matches a user
#' identifier is then described without its term. Review rationales and
#' expected behavior are free text and are never printed.
#'
#' Source details a caller recorded with the investigation print their values
#' only for the fields a source reader records about its read (`read_at`,
#' `from`, `to`, `n`, `spans`, `spans_total`, `max_spans`, `truncated`,
#' `incomplete`, `conversations_found`, `conversations`, `files`, and
#' `lines_dropped`). Any other field is free-form and may name a person, so
#' it is listed by name with its value withheld unless `include_users = TRUE`.
#'
#' @section Links:
#' With `app_url`, each trajectory and event links to that app. A link sets
#' the filters the report names and clears every other filter in the app that
#' opens it. It reopens exactly the reported cohort when that app serves this
#' saved investigation; an app reading the live source applies the same
#' filters to whatever it holds then. The report prints this, and names any
#' filter a link cannot carry, under its scope.
#'
#' @param x A saved investigation from [investigation_snapshot()] or
#'   [read_investigation()]. A [TrajectoryBundle] is captured first with
#'   [investigation_snapshot()] and its defaults. For
#'   `write_investigation_report()` and `format()`, a report.
#' @param reviews Optional list of [review_case()] values made from this
#'   investigation's snapshot. Their adjudicated status is reported beside the
#'   evidence they refer to.
#' @param app_url Optional address of a [scans_app()] deployment: an
#'   `http://` or `https://` host and path, with no credentials, query, or
#'   fragment. Without it the report has anchors only.
#' @param include_users Whether to print user identifiers and the values of
#'   free-form source details. Defaults to `FALSE`.
#' @param include_errors Whether to print the most frequent recorded error
#'   text per tool. Defaults to `FALSE`, which prints the count only.
#'
#' @returns `investigation_report()` returns a `scans_investigation_report`: a
#'   list of ordinary tibbles and scope details.
#' - `scope`: counts in scope, `filters` in words, the time `window`, the
#'   `source` read, snapshot identifiers, scanner `settings`, and what links
#'   cannot restore.
#' - `headline`: one row of counts with their denominators, including
#'   `n_assessed` and `n_not_assessed`.
#' - `patterns`: one row per finding type with affected `trajectories`, the
#'   number the scan `assessed`, the `share` of those affected (`NA`
#'   when it assessed none), and first and last occurrence.
#' - `tools`, `models`, and, when requested, `users`: breakdowns. Findings
#'   in a model or user are counted over its `n_assessed` trajectories.
#' - `measures`: each resource measure by recording coverage.
#' - `coverage`: trajectories per scanner and assessment status.
#' - `losses`: capture losses by field and reason.
#' - `trajectories`: the cohort, one row per trajectory, with its class.
#' - `evidence`: one row per finding with trajectory, turn, and event
#'   identities, its anchor in the rendered report, and links.
#' - `reviews`: one row per supplied review case.
#'
#'   `write_investigation_report()` returns the report invisibly. `format()`
#'   returns Markdown lines or one HTML string.
#' @export
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "run-1", source_type = "manual",
#'              status = "failed"),
#'   data.frame(), data.frame()
#' )
#' saved <- investigation_snapshot(bundle, application = "Support assistant")
#' report <- investigation_report(saved)
#' report$headline[c("n", "n_assessed", "n_findings")]
#' report$evidence[c("scan", "trajectory_id")]
#'
#' if (requireNamespace("htmltools", quietly = TRUE)) {
#'   path <- tempfile(fileext = ".html")
#'   write_investigation_report(report, path)
#' }
investigation_report <- function(
  x,
  reviews = NULL,
  app_url = NULL,
  include_users = FALSE,
  include_errors = FALSE
) {
  investigation_check_dependencies()
  if (is_trajectory_bundle(x)) {
    x <- investigation_snapshot(x)
  }
  investigation_validate(x)
  if (!rlang::is_bool(include_users)) {
    report_abort("{.arg include_users} must be TRUE or FALSE.")
  }
  if (!rlang::is_bool(include_errors)) {
    report_abort("{.arg include_errors} must be TRUE or FALSE.")
  }
  report_check_url(app_url)
  data <- scans_app_data(x$bundle, x$settings, analysis = x$analysis)
  ids <- data$info$trajectory_id
  indices <- scans_app_view_indices(
    data,
    x$view,
    annotated = ids %in% x$view$annotation_ids
  )
  report_build(
    data,
    indices,
    c(
      report_saved_context(x),
      list(
        application = x$manifest$application,
        view = x$view,
        annotated_label = "Annotated when saved",
        app_url = app_url,
        settings = x$settings,
        bundle = x$bundle,
        reviews = report_reviews(reviews, x),
        include_users = include_users,
        include_errors = include_errors
      )
    )
  )
}

# What a saved investigation says about where it came from. `source` is
# caller-supplied: the app saves `read_info` and `loaded_at`, a script may
# pass a reader's `read_info` itself, and anything else is kept as a detail
# to print rather than read as a statement about capping.
report_read_fields <- c(
  "read_at",
  "from",
  "to",
  "n",
  "spans",
  "spans_total",
  "max_spans",
  "truncated",
  "incomplete",
  "conversations_found",
  "conversations"
)

# The source details whose values are printed without `include_users`: what
# a reader records about its read. Anything else is free-form.
report_detail_fields <- c(report_read_fields, "files", "lines_dropped")

report_saved_context <- function(x) {
  source <- x$manifest$source
  read <- NULL
  if (is.list(source$read_info)) {
    read <- source$read_info
    source$read_info <- NULL
  } else if (
    any(
      c("from", "to", "truncated", "conversations_found", "max_spans") %in%
        names(source)
    )
  ) {
    read <- source[intersect(names(source), report_read_fields)]
    source <- source[setdiff(names(source), report_read_fields)]
  }
  loaded_at <- source$loaded_at
  source$loaded_at <- NULL
  details <- Filter(
    function(value) {
      is.atomic(value) && length(value) == 1L && !is.na(value)
    },
    source
  )
  list(
    read_info = read,
    loaded_at = if (inherits(loaded_at, "POSIXct")) loaded_at,
    source_details = vapply(details, format, ""),
    snapshot = list(
      snapshot_id = x$manifest$snapshot_id,
      revision_id = x$manifest$revision_id,
      omitted_trajectories = x$manifest$content_policy$omitted_trajectories,
      content_policy = x$manifest$content_policy$id
    ),
    analysis_version = x$manifest$package_version
  )
}

#' @rdname investigation_report
#' @param path File to write. The extension chooses the format unless
#'   `format` is given: `.html` or `.htm` for HTML, `.md` for Markdown.
#' @param format `"html"` or `"markdown"`.
#' @param max_evidence Largest number of trajectories listed per pattern.
#'   The rendered report says when it lists fewer than it counted; the
#'   `evidence` table always holds them all.
#' @param overwrite Whether to replace an existing file. Defaults to `FALSE`.
#' @export
write_investigation_report <- function(
  x,
  path,
  format = NULL,
  max_evidence = 100L,
  overwrite = FALSE
) {
  report_check(x)
  investigation_check_path(path)
  if (is.null(format)) {
    extension <- tolower(tools::file_ext(path))
    format <- c(html = "html", htm = "html", md = "markdown")[extension]
    if (is.na(format)) {
      report_abort(
        "Use a {.file .html} or {.file .md} path, or supply {.arg format}."
      )
    }
  }
  if (!rlang::is_bool(overwrite)) {
    report_abort("{.arg overwrite} must be TRUE or FALSE.")
  }
  if (file.exists(path) && !overwrite) {
    report_abort(
      "The destination already exists. Use a new path or {.code overwrite = TRUE}."
    )
  }
  text <- format(x, format = unname(format), max_evidence = max_evidence)
  con <- file(path, open = "wb", encoding = "native.enc")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(text), con, useBytes = TRUE)
  invisible(x)
}

#' @rdname investigation_report
#' @param ... Unused.
#' @export
format.scans_investigation_report <- function(
  x,
  format = c("markdown", "html"),
  max_evidence = 100L,
  ...
) {
  report_check(x)
  format <- rlang::arg_match(format)
  if (
    !rlang::is_scalar_integerish(max_evidence) ||
      is.na(max_evidence) ||
      max_evidence < 0
  ) {
    report_abort("{.arg max_evidence} must be one non-negative whole number.")
  }
  blocks <- rapply(
    report_blocks(x, max_evidence),
    report_utf8,
    classes = "character",
    how = "replace"
  )
  if (format == "markdown") {
    return(report_markdown(blocks))
  }
  rlang::check_installed("htmltools", reason = "to render an HTML report.")
  report_html(blocks, report_utf8(x$application))
}

# Recorded strings can be in another declared encoding or hold bytes that
# are not UTF-8 at all. Both writers get UTF-8, with each invalid byte
# replaced by U+FFFD.
report_utf8 <- function(text) {
  text <- enc2utf8(text)
  bad <- !is.na(text) & !validUTF8(text)
  text[bad] <- iconv(text[bad], "UTF-8", "UTF-8", sub = "\ufffd")
  Encoding(text[bad]) <- "UTF-8"
  text
}

#' @export
print.scans_investigation_report <- function(x, ...) {
  noun <- x$scope$noun
  cli::cli_text("{.cls scans_investigation_report} {x$application}")
  cli::cli_text(
    "{x$headline$n} of {x$headline$total} {noun$many} in scope \u00b7 {x$headline$n_findings} of {x$headline$n_assessed} assessed with findings \u00b7 {x$headline$n_not_assessed} not assessed"
  )
  cli::cli_text(
    "{nrow(x$patterns)} pattern{?s} \u00b7 {nrow(x$evidence)} finding{?s} with evidence identities"
  )
  cli::cli_text(
    "Render with {.fn write_investigation_report} or {.fn format}."
  )
  invisible(x)
}

report_abort <- function(message, call = rlang::caller_env()) {
  scans_abort(
    message,
    class = "scans_error_report",
    call = call,
    .envir = rlang::caller_env()
  )
}

report_check <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "scans_investigation_report")) {
    report_abort(
      "{.arg x} must be a report from {.fn investigation_report}.",
      call
    )
  }
  invisible(x)
}

# A base address: scheme, host, optional port and path. Credentials, a query,
# or a fragment would be copied into every link, and the path's characters
# are limited to ones neither Markdown nor HTML reads as markup, so a link
# cannot break a table row.
report_check_url <- function(url, call = rlang::caller_env()) {
  pattern <- paste0(
    "^https?://",
    "([A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*|\\[[0-9A-Fa-f:.]+\\])",
    "(:[0-9]{1,5})?",
    "(/[A-Za-z0-9._~!$'()*+,;=:%/-]*)?$"
  )
  if (
    !is.null(url) &&
      (!rlang::is_string(url) || is.na(url) || !grepl(pattern, url))
  ) {
    report_abort(
      c(
        "{.arg app_url} must be one {.val http://} or {.val https://} base address.",
        i = "Give the host and path only: no credentials, query, or fragment."
      ),
      call
    )
  }
  invisible(url)
}

# The review cases a report shows, checked against the investigation they
# claim to review. Status comes from `review_status()`, so adjudication has
# one definition.
report_reviews <- function(reviews, x, call = rlang::caller_env()) {
  if (is.null(reviews)) {
    return(NULL)
  }
  if (inherits(reviews, "scans_review_case")) {
    reviews <- list(reviews)
  }
  if (!is.list(reviews)) {
    report_abort("{.arg reviews} must be a list of review cases.", call)
  }
  rows <- lapply(reviews, function(case) {
    status <- review_status(case)
    if (!identical(case$origin$snapshot_id, x$manifest$snapshot_id)) {
      report_abort(
        "A review case was made from a different snapshot than this investigation.",
        call
      )
    }
    decisions <- case$decisions
    active <- decisions[
      vapply(decisions, `[[`, "", "decision_id") %in% status$decision_ids
    ]
    tibble::tibble(
      case_id = case$case_id,
      trajectory_id = case$trajectory_id,
      scan = case$scan %||% NA_character_,
      finding_id = case$finding_id %||% NA_character_,
      status = status$status,
      n_decisions = length(decisions),
      n_active = length(active),
      same_revision = identical(
        case$origin$revision_id,
        x$manifest$revision_id
      ),
      reviewers = list(unique(vapply(active, `[[`, "", "reviewer")))
    )
  })
  if (!length(rows)) {
    return(NULL)
  }
  out <- trajectory_bind_rows(rows)
  if (anyDuplicated(out$case_id)) {
    report_abort("Supply each review case only once.", call)
  }
  out
}

# Whether a text filter would name a user: its term contains, or is contained
# in, a user identifier this snapshot records.
report_query_names_user <- function(query, users) {
  query <- tolower(trimws(query %||% ""))
  users <- tolower(users[!is.na(users) & nzchar(users)])
  nzchar(query) &&
    any(
      vapply(
        users,
        function(user) {
          grepl(query, user, fixed = TRUE) || grepl(user, query, fixed = TRUE)
        },
        logical(1)
      )
    )
}

# The one assembly behind the script's report and the app's. `indices` are
# the rows in view; every count comes from `scans_app_performance_data()`,
# the function the overview renders from. `context` names what narrowed the
# cohort and where it came from:
# - `application`, `view` (saved-view form), `facets`, `review`,
#   `annotated_label`: the scope in words and in links.
# - `app_url`: the app's address, or `NULL` for no links.
# - `read_info`, `loaded_at`, `source_details`, `snapshot`, `settings`,
#   `analysis_version`: provenance, as `report_saved_context()` reads it.
# - `bundle`, for promotion candidates; `queue` and `reviews`: optional state.
report_build <- function(data, indices, context) {
  include_users <- isTRUE(context$include_users)
  include_errors <- isTRUE(context$include_errors)
  indices <- sort(indices)
  ids <- data$info$trajectory_id[indices]
  performance <- scans_app_performance_data(
    data,
    indices,
    queue = context$queue,
    candidates = scans_app_candidates(data, context$bundle, ids)
  )
  cohort <- performance$cohort
  headline <- cohort$headline
  view <- context$view
  facets <- context[["facets"]] %||% list()
  review <- context[["review"]] %||% list()

  query_hidden <- !include_users &&
    report_query_names_user(view$query, data$records$user)
  user_hidden <- !include_users && !is.null(facets$user)
  labels <- scans_app_filter_labels(
    view,
    facets,
    review,
    context$annotated_label %||% "Annotated only"
  )
  if (query_hidden) {
    labels[[
      "query"
    ]] <- "Text filter (term omitted: it matches a user identifier)"
  }
  if (user_hidden) {
    labels[["user"]] <- "One user (identifier omitted)"
  }

  open_link <- NULL
  if (!is.null(context$app_url)) {
    link_view <- view
    if (query_hidden) {
      link_view$query <- ""
    }
    link_facets <- facets
    if (user_hidden) {
      link_facets$user <- NULL
    }
    open_link <- scans_app_link(
      context$app_url,
      context$application,
      c(
        scans_app_view_scope(link_view),
        list(review = review$state, review_label = review$label),
        link_facets
      )
    )
  }
  event_link <- function(trajectory_id, event_id) {
    paste0(
      open_link(trajectory_id),
      "?event=",
      utils::URLencode(event_id, reserved = TRUE, repeated = TRUE)
    )
  }

  findings <- data$findings[
    data$findings$trajectory_id %in% ids,
    ,
    drop = FALSE
  ]
  patterns <- performance$patterns
  findings <- findings[
    order(
      match(findings$scan, patterns$scan),
      match(findings$trajectory_id, ids),
      match(findings$finding_id, data$findings$finding_id),
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  evidence <- tibble::tibble(
    scan = findings$scan,
    severity = findings$severity,
    trajectory_id = findings$trajectory_id,
    finding_id = findings$finding_id,
    turn_id = findings$turn_id,
    event_id = findings$event_id,
    event_ids = lapply(findings$event_ids, as.character),
    explanation = findings$explanation,
    anchor = sprintf("finding-%d", seq_len(nrow(findings))),
    link = if (is.null(open_link)) {
      rep(NA_character_, nrow(findings))
    } else {
      vapply(
        seq_len(nrow(findings)),
        function(row) {
          if (is.na(findings$event_id[[row]])) {
            open_link(findings$trajectory_id[[row]])
          } else {
            event_link(findings$trajectory_id[[row]], findings$event_id[[row]])
          }
        },
        ""
      )
    },
    event_links = lapply(seq_len(nrow(findings)), function(row) {
      events <- as.character(findings$event_ids[[row]])
      if (is.null(open_link)) {
        return(rep(NA_character_, length(events)))
      }
      vapply(
        events,
        function(event) event_link(findings$trajectory_id[[row]], event),
        "",
        USE.NAMES = FALSE
      )
    })
  )

  coverage <- performance$assessment_coverage
  # Reach is over the trajectories each scan assessed, as on screen: one the
  # scan could not assess is not an unaffected one.
  patterns <- tibble::tibble(
    scan = patterns$scan,
    label = scans_app_pattern_label(patterns$scan),
    severity = patterns$severity,
    trajectories = patterns$trajectories,
    n = rep(headline$n, nrow(patterns)),
    assessed = patterns$assessed,
    share = patterns$share,
    findings = patterns$findings,
    users = patterns$users,
    first_seen = patterns$first_seen,
    last_seen = patterns$last_seen,
    summary = patterns$evidence,
    anchor = sprintf("pattern-%d", seq_len(nrow(patterns)))
  )

  queue <- cohort$queue
  queue <- queue[match(ids, queue$trajectory_id), , drop = FALSE]
  listed <- performance$trajectories
  trajectories <- tibble::tibble(
    trajectory_id = ids,
    started_at = data$records$started_at[indices],
    class = queue$class,
    n_findings = listed$n_findings,
    elapsed = listed$elapsed,
    tokens = listed$tokens,
    capture_losses = ids %in% data$loss_trajectory_ids,
    link = if (is.null(open_link)) {
      rep(NA_character_, length(ids))
    } else {
      vapply(ids, open_link, "", USE.NAMES = FALSE)
    }
  )
  if (include_users) {
    trajectories$user <- data$records$user[indices]
  }

  reviews <- context$reviews
  if (!is.null(reviews)) {
    reviews$in_scope <- reviews$trajectory_id %in% ids
    reviews$in_analysis <- reviews$finding_id %in% data$findings$finding_id
    reviews$anchor <- evidence$anchor[match(
      reviews$finding_id,
      evidence$finding_id
    )]
    if (!include_users) {
      reviews$reviewers <- NULL
    }
  }

  read <- context$read_info
  read <- if (is.list(read)) read else list()
  details <- context$source_details
  if (!include_users && length(details)) {
    details[!names(details) %in% report_detail_fields] <- NA_character_
  }
  tools <- cohort$tools
  if (!include_errors) {
    tools$top_error <- rep(NA_character_, nrow(tools))
  }
  withheld <- query_hidden || user_hidden
  limits <- c(
    if (!is.null(open_link)) {
      carried <- if (withheld) {
        "Links set the filters named above, except as noted below, and clear every other filter."
      } else {
        "Links set the filters named above and clear every other filter."
      }
      if (is.null(context$snapshot)) {
        paste(
          carried,
          "They apply them to what the app holds when they are opened. Records read since this report was made can widen the cohort."
        )
      } else {
        sprintf(
          "%s They reopen %s when the app at that address serves this saved investigation (snapshot %s). An app reading the live source applies the same filters to what it holds then, which can include records this saved selection left out.",
          carried,
          if (withheld) {
            "this cohort, widened as noted below,"
          } else {
            "exactly this cohort"
          },
          substr(context$snapshot$snapshot_id, 8L, 19L)
        )
      }
    },
    if (!is.null(open_link) && isTRUE(view$annotated_only)) {
      "The annotation filter depends on annotations where the link is opened: the saved file's membership, or that app's own annotation store. An app with neither says so and shows a wider cohort."
    },
    if (!is.null(open_link) && length(review)) {
      "The review filter depends on the annotation store of the app the link is opened in. An app without that store says so and shows a wider cohort."
    },
    if (!is.null(open_link) && query_hidden) {
      "Links leave out the text filter because its term matches a user identifier. They still clear every other filter, so they open the cohort the other filters select: wider than this report describes."
    },
    if (!is.null(open_link) && user_hidden) {
      "Links leave out the user filter. They still clear every other filter, so they open the cohort the other filters select for every user: wider than this report describes."
    }
  )

  headline$n_not_assessed <- headline$n - headline$n_assessed
  headline$n_evaluated <- performance$n_evaluated
  headline$n_losses <- performance$n_losses
  headline$unassigned_losses <- performance$unassigned_losses
  headline$n_work <- performance$n_work
  headline$n_work_partial <- performance$n_work_partial
  headline$median_work <- performance$median_work
  headline$median_gap <- performance$median_gap

  structure(
    list(
      application = context$application,
      generated = as.POSIXct(Sys.time(), tz = "UTC"),
      package_version = as.character(utils::packageVersion("scans")),
      include_users = include_users,
      include_errors = include_errors,
      scope = list(
        n = headline$n,
        total = headline$total,
        noun = cohort$noun,
        filters = labels,
        window = list(
          first = headline$first_started,
          last = headline$last_started,
          basis = headline$time_basis,
          n_undated = headline$n_undated
        ),
        source = list(
          recorded = length(read) > 0L,
          details = details,
          from = read$from,
          to = read$to,
          read_at = read$read_at %||% context$loaded_at,
          limit = read$n,
          found = read$conversations_found,
          max_spans = read$max_spans,
          truncated = read$truncated,
          incomplete = read$incomplete
        ),
        snapshot = context$snapshot,
        settings = context$settings,
        analysis_version = context$analysis_version,
        app_url = context$app_url,
        link = if (!is.null(open_link)) open_link(),
        link_limits = limits
      ),
      headline = headline,
      patterns = patterns,
      tools = tools,
      models = cohort$models,
      users = if (include_users) cohort$users,
      measures = cohort$measures,
      coverage = coverage,
      losses = cohort$losses,
      trajectories = trajectories,
      evidence = evidence,
      reviews = reviews,
      interventions = cohort$interventions,
      provenance = cohort$provenance,
      # The app's candidate view shows recorded tool inputs and outputs as
      # `snippets`; the report holds none of that.
      candidates = if (!is.null(cohort$candidates)) {
        cohort$candidates[setdiff(names(cohort$candidates), "snippets")]
      },
      review_progress = cohort$progress
    ),
    class = "scans_investigation_report"
  )
}
