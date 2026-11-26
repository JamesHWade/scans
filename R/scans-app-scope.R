# Which trajectories a set of filters selects, and how those filters read in
# words. The app's reactives and the investigation report both call these, so
# a filter means the same cohort on screen, in a saved view, and in a report.

# Filters in the form a saved investigation's view records them: `NULL` means
# every source or status, and an `NA` status means unknown. A select input
# holds a sentinel for "all" and for unknown status, and can hold a value the
# loaded snapshot no longer offers.
scans_app_input_view <- function(
  data,
  source = NULL,
  status = NULL,
  query = "",
  findings_only = FALSE,
  annotated_only = FALSE,
  pattern = NULL
) {
  choices <- scans_app_filter_choices(data)
  if (
    !scans_app_has_string(source) ||
      identical(source, choices$source_all) ||
      !source %in% unname(choices$source)
  ) {
    source <- NULL
  }
  unknown <- scans_app_filter_sentinel(data$records$status, "status-unknown")
  if (
    !scans_app_has_string(status) ||
      identical(status, choices$status_all) ||
      !status %in% unname(choices$status)
  ) {
    status <- NULL
  } else if (identical(status, unknown)) {
    status <- NA_character_
  }
  list(
    query = if (scans_app_has_string(query)) query else "",
    source_type = source,
    status = status,
    findings_only = isTRUE(findings_only),
    annotated_only = isTRUE(annotated_only),
    pattern = pattern
  )
}

# The value a status select holds for a saved view's status.
scans_app_view_status_input <- function(data, status) {
  choices <- scans_app_filter_choices(data)
  if (is.null(status)) {
    return(choices$status_all)
  }
  if (is.na(status)) {
    return(scans_app_filter_sentinel(data$records$status, "status-unknown"))
  }
  status
}

# Row indices a view selects, in snapshot order. `annotated` flags each
# trajectory of the snapshot. A source or status the snapshot does not hold
# selects everything, as the app's select inputs would.
scans_app_view_indices <- function(data, view, annotated = NULL) {
  choices <- scans_app_filter_choices(data)
  source <- view$source_type
  if (is.null(source) || !source %in% unname(choices$source)) {
    source <- choices$source_all
  }
  status <- scans_app_view_status_input(data, view$status)
  if (!status %in% unname(choices$status)) {
    status <- choices$status_all
  }
  indices <- scans_app_filter_records(
    data$records,
    source = source,
    status = status,
    query = view$query %||% "",
    findings_only = isTRUE(view$findings_only),
    annotated = annotated,
    annotated_only = isTRUE(view$annotated_only)
  )
  if (!is.null(view$pattern)) {
    matching <- data$findings$trajectory_id[
      data$findings$scan %in% view$pattern
    ]
    indices <- indices[data$records$trajectory_id[indices] %in% matching]
  }
  indices
}

# Narrow `indices` by review state and label. `queue` is the review queue of
# the whole snapshot; without one nothing claims a review state.
scans_app_review_indices <- function(indices, review, queue) {
  if (!length(review) || is.null(queue)) {
    return(indices)
  }
  rows <- queue[match(indices, queue$index), , drop = FALSE]
  keep <- rep(TRUE, length(indices))
  if (!is.null(review$state)) {
    keep <- keep & (rows$reviewed %in% TRUE) == (review$state == "reviewed")
  }
  if (!is.null(review$label)) {
    keep <- keep &
      rows$reviewed %in% TRUE &
      ifelse(is.na(rows$label), scans_app_no_label, rows$label) == review$label
  }
  indices[keep]
}

# Every active filter in words, keyed by what it filters on. The source,
# status, text, and annotation filters come first, then the chips the overview
# shows. `annotated_label` differs for a saved view, whose membership was
# fixed when it was saved.
scans_app_filter_labels <- function(
  view,
  facets = list(),
  review = list(),
  annotated_label = "Annotated only"
) {
  c(
    if (!is.null(view$source_type)) {
      c(source = paste("Source:", view$source_type))
    },
    if (!is.null(view$status)) {
      c(
        status = if (is.na(view$status)) {
          "Status: unknown"
        } else {
          paste("Status:", view$status)
        }
      )
    },
    if (nzchar(trimws(view$query %||% ""))) {
      c(query = sprintf("Text contains \"%s\"", trimws(view$query)))
    },
    if (isTRUE(view$annotated_only)) c(annotated = annotated_label),
    scans_app_scope_labels(facets, view$pattern, view$findings_only, review)
  )
}

# Promotion candidates read the bundle, and only sources that record
# exchanges have any.
scans_app_candidates <- function(data, bundle, ids) {
  if (is.null(data) || is.null(bundle) || is.null(data$exchanges)) {
    return(NULL)
  }
  route_candidates(
    bundle,
    trajectory_ids = ids,
    exchanges = data$exchanges,
    measures = NULL
  )
}

# A view's filters as link scope, and back. A link carries the filter values,
# not the trajectories: it selects whatever matches them where it is opened.
scans_app_view_scope <- function(view) {
  list(
    source = view$source_type,
    status = if (!is.null(view$status) && !is.na(view$status)) view$status,
    status_unknown = if (!is.null(view$status) && is.na(view$status)) TRUE,
    query = if (nzchar(trimws(view$query %||% ""))) trimws(view$query),
    annotated = if (isTRUE(view$annotated_only)) TRUE,
    pattern = view$pattern,
    findings = if (isTRUE(view$findings_only)) TRUE
  )
}

scans_app_scope_view <- function(scope) {
  list(
    query = scope$query %||% "",
    source_type = scope$source,
    status = if (isTRUE(scope$status_unknown)) {
      NA_character_
    } else {
      scope$status
    },
    findings_only = isTRUE(scope$findings),
    annotated_only = isTRUE(scope$annotated),
    pattern = scope$pattern
  )
}

# The view and facets a link's scope asks for, as far as the snapshot it is
# opened on can apply them. A source, status, pattern, or facet value the
# snapshot does not hold, a pattern no selected scan (`scans`) found, and an
# annotation filter where no annotation membership exists, are dropped and
# named in `notice`: the cohort on screen is then wider than the link's.
scans_app_scope_restore <- function(
  scope,
  data,
  annotations = TRUE,
  scans = NULL
) {
  view <- scans_app_scope_view(scope)
  choices <- scans_app_filter_choices(data)
  dropped <- character()
  if (!is.null(view$pattern)) {
    found <- data$findings$scan
    if (!is.null(scans)) {
      found <- intersect(found, scans)
    }
    if (!scans_app_has_string(view$pattern) || !view$pattern %in% found) {
      dropped <- c(dropped, paste("Finding:", gsub("_", " ", view$pattern)))
      view["pattern"] <- list(NULL)
    }
  }
  asked <- scope[intersect(names(scope), scans_app_facet_names)]
  view$facets <- scans_app_facet_validate(data, asked)
  lost <- asked[setdiff(names(asked), names(view$facets))]
  if (length(lost)) {
    dropped <- c(
      dropped,
      tryCatch(
        unname(scans_app_scope_labels(lost)),
        error = function(cnd) paste0(names(lost), ": ", unlist(lost))
      )
    )
  }
  if (
    !is.null(view$source_type) &&
      !view$source_type %in% unname(choices$source)
  ) {
    dropped <- c(dropped, paste("Source:", view$source_type))
    view["source_type"] <- list(NULL)
  }
  if (
    !is.null(view$status) &&
      !scans_app_view_status_input(data, view$status) %in%
        unname(choices$status)
  ) {
    dropped <- c(
      dropped,
      if (is.na(view$status)) {
        "Status: unknown"
      } else {
        paste("Status:", view$status)
      }
    )
    view["status"] <- list(NULL)
  }
  if (view$annotated_only && !annotations) {
    dropped <- c(dropped, "Annotated only")
    view$annotated_only <- FALSE
  }
  if (length(dropped)) {
    view$notice <- sprintf(
      "This link filters by %s, which this app cannot apply to the records it holds. That filter is not applied, so this is a wider cohort than the link describes.",
      paste(dropped, collapse = ", ")
    )
  }
  view
}

scans_app_annotated_label <- function(saved, has_store) {
  if (has_store && !is.null(saved)) {
    "Annotated in this app's annotation store at export"
  } else if (!is.null(saved)) {
    "Annotated when saved"
  } else {
    "Annotated only"
  }
}

# Whether a view sets the same filters as a saved view.
scans_app_same_view <- function(view, saved) {
  fields <- c(
    "query",
    "source_type",
    "status",
    "findings_only",
    "annotated_only",
    "pattern"
  )
  identical(
    lapply(fields, function(field) view[[field]]),
    lapply(fields, function(field) saved[[field]])
  )
}
