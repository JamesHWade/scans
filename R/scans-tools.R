#' Create tools for exploring retained trajectories
#'
#' `scans_tools()` creates six read-only ellmer tools backed by deterministic
#' scans queries. The tools inspect a fixed copy of the supplied evidence; they
#' cannot fetch sources, replay recorded tools, execute R, or write reviews.
#' Register the returned tools with an ellmer chat using `chat$set_tools()`.
#'
#' Tools return row-oriented JSON with data, snapshot and analysis identities,
#' evidence references, coverage limitations, and explicit pagination. Counts
#' and descriptive statistics are computed over the requested population in R,
#' not over the returned page. Missing values remain JSON `null`.
#'
#' A saved investigation retains its saved diagnostics and scanner settings.
#' To recompute them, explicitly create a new [investigation_snapshot()] from
#' its bundle and supply that separate investigation.
#'
#' @param x A [TrajectoryBundle] or saved [investigation_snapshot()].
#' @param trajectory_ids Permitted trajectory identifiers. `NULL` permits all
#'   retained trajectories; `character()` permits none. Model arguments can
#'   narrow this scope but cannot broaden it.
#' @param max_rows Maximum rows returned by each query, from 1 to 1000.
#' @param max_chars Maximum characters per field, from 100 to 16000. Larger
#'   fields are returned as explicit slices. Repeat the same paginated query
#'   with `content_offset` to retrieve subsequent slices.
#' @returns A named list of ellmer tools: `scans_list_trajectories`,
#'   `scans_summarize`, `scans_read_trajectory`, `scans_find_events`,
#'   `scans_get_findings`, and `scans_measure`.
#' @export
#' @examples
#' if (requireNamespace("ellmer", quietly = TRUE)) {
#'   bundle <- TrajectoryBundle(
#'     data.frame(trajectory_id = "run-1", source_type = "manual"),
#'     data.frame(), data.frame()
#'   )
#'   tools <- scans_tools(bundle)
#' }
scans_tools <- function(
  x,
  trajectory_ids = NULL,
  max_rows = 100L,
  max_chars = 4000L
) {
  rlang::check_installed(c("ellmer", "jsonlite", "digest"))
  scans_tools_integer(max_rows, "max_rows", 1L, 1000L)
  scans_tools_integer(max_chars, "max_chars", 100L, 16000L)
  context <- scans_tools_context(x, trajectory_ids)
  scans_tools_build(context, max_rows, max_chars)
}

scans_tools_context <- function(x, trajectory_ids = NULL) {
  saved <- inherits(x, "scans_investigation")
  if (saved) {
    investigation_validate(x)
  } else if (!is_trajectory_bundle(x)) {
    scans_abort(
      "{.arg x} must be a trajectory bundle or saved investigation.",
      class = "scans_error_tools_input"
    )
  }
  # Copy values before closures capture them, including nested metadata.
  x <- unserialize(serialize(x, NULL))
  bundle <- if (saved) x$bundle else x
  ids <- trajectory_info(bundle)$trajectory_id
  if (is.null(trajectory_ids)) {
    trajectory_ids <- ids
  }
  scans_tools_ids(trajectory_ids, ids)
  settings <- if (saved) x$settings else scans_app_scan_config()
  data <- scans_app_data(bundle, settings, if (saved) x$analysis)
  list(
    bundle = bundle,
    data = data,
    ids = unique(trajectory_ids),
    settings = settings,
    snapshot_id = if (saved) {
      x$manifest$snapshot_id
    } else {
      paste0("snapshot-", digest::digest(bundle, algo = "sha256"))
    },
    revision_id = if (saved) {
      x$manifest$revision_id
    } else {
      paste0(
        "analysis-",
        digest::digest(
          list(settings, data$findings, data$assessments, data$measures),
          algo = "sha256"
        )
      )
    },
    analysis_source = if (saved) "saved" else "computed"
  )
}

scans_tools_ids <- function(ids, allowed) {
  if (!is.character(ids) || anyNA(ids) || !all(ids %in% allowed)) {
    scans_abort(
      "Trajectory identifiers must belong to the permitted scope.",
      class = "scans_error_tools_scope"
    )
  }
  invisible(ids)
}

scans_tools_integer <- function(x, arg, min = 0L, max = .Machine$integer.max) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x != floor(x) ||
      x < min ||
      x > max
  ) {
    scans_abort(
      "{.arg {arg}} must be a whole number between {min} and {max}.",
      class = "scans_error_tools_limit"
    )
  }
  invisible(x)
}

scans_tools_string <- function(x, arg, nullable = FALSE) {
  if (nullable && is.null(x)) {
    return(invisible(x))
  }
  if (!rlang::is_string(x) || nchar(x) > 16000L) {
    scans_abort(
      "{.arg {arg}} must be a string of at most 16000 characters.",
      class = "scans_error_tools_input"
    )
  }
  invisible(x)
}

scans_tools_reference <- function(context, row) {
  reference <- list(
    snapshot_id = context$snapshot_id,
    revision_id = context$revision_id,
    trajectory_id = row$trajectory_id,
    event_id = row$event_id %||% NULL,
    turn_id = row$turn_id %||% NULL,
    finding_id = row$finding_id %||% NULL
  )
  reference$key <- digest::digest(reference, algo = "sha256")
  reference
}

scans_tools_rows <- function(data, max_chars, content_offset = 0L) {
  lapply(seq_len(nrow(data)), function(i) {
    row <- lapply(data, function(column) {
      value <- column[[i]]
      if (inherits(column, "POSIXt")) {
        return(
          if (is.na(value)) {
            NA_character_
          } else {
            format(value, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
          }
        )
      }
      if (is.list(column)) {
        value <- as.character(jsonlite::toJSON(
          value,
          auto_unbox = TRUE,
          null = "null",
          na = "null",
          dataframe = "rows",
          force = TRUE
        ))
      }
      value
    })
    slices <- list()
    for (name in names(row)) {
      value <- row[[name]]
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        next
      }
      # Identities are kept whole so truncated content stays addressable.
      if (grepl("(^|_)id$", name)) {
        next
      }
      size <- nchar(value)
      start <- if (size > max_chars) content_offset else 0L
      if (size > max_chars) {
        row[[name]] <- if (start >= size) {
          ""
        } else {
          substr(value, start + 1L, start + max_chars)
        }
        slices[[name]] <- list(
          offset = start,
          total_chars = size,
          next_offset = if (start + max_chars < size) {
            start + max_chars
          } else {
            NULL
          }
        )
      }
    }
    if (length(slices)) {
      row$content_slices <- slices
    }
    row
  })
}

scans_tools_build <- function(
  context,
  max_rows = 100L,
  max_chars = 4000L,
  on_evidence = NULL
) {
  force(context)
  ids <- function(requested = NULL) {
    if (is.null(requested)) {
      return(context$ids)
    }
    scans_tools_ids(requested, context$ids)
    unique(requested)
  }
  rows_for <- function(table, requested) {
    table[table$trajectory_id %in% ids(requested), , drop = FALSE]
  }
  reply <- function(
    data,
    offset = 0L,
    limit = 25L,
    content_offset = 0L,
    extra = list(),
    evidence = TRUE
  ) {
    scans_tools_integer(offset, "offset")
    scans_tools_integer(limit, "limit", 1L, max_rows)
    scans_tools_integer(content_offset, "content_offset")
    offset <- as.double(offset)
    content_offset <- as.double(content_offset)
    total <- nrow(data)
    page <- data[
      seq_len(total) > offset & seq_len(total) <= offset + limit,
      ,
      drop = FALSE
    ]
    references <- if (evidence && "trajectory_id" %in% names(page)) {
      lapply(seq_len(nrow(page)), function(i) {
        scans_tools_reference(context, lapply(page, function(x) x[[i]]))
      })
    } else {
      list()
    }
    if (is.function(on_evidence) && length(references)) {
      references <- on_evidence(references)
    }
    result <- c(
      list(
        data = scans_tools_rows(page, max_chars, content_offset),
        scope = list(
          snapshot_id = context$snapshot_id,
          revision_id = context$revision_id,
          trajectories = length(context$ids),
          analysis_source = context$analysis_source
        ),
        evidence = references,
        paging = list(
          offset = offset,
          returned = nrow(page),
          total = total,
          next_offset = if (offset + nrow(page) < total) {
            offset + nrow(page)
          } else {
            NULL
          }
        ),
        limitations = c(
          "Retained evidence only; complete capture and answer quality are not established.",
          "Missing values are unknown. Elapsed time is not model response latency.",
          "Text and nested fields may be sliced; content_slices records omissions."
        )
      ),
      extra
    )
    as.character(jsonlite::toJSON(
      result,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      POSIXt = "ISO8601",
      force = TRUE
    ))
  }
  list_runs <- function(
    query = "",
    source = NULL,
    status = NULL,
    findings_only = FALSE,
    sort = "newest",
    offset = 0L,
    limit = min(25L, max_rows),
    content_offset = 0L
  ) {
    scans_tools_string(query, "query")
    scans_tools_string(source, "source", TRUE)
    scans_tools_string(status, "status", TRUE)
    rlang::check_bool(findings_only)
    sort <- rlang::arg_match(sort, unname(scans_app_sort_choices))
    records <- context$data$records
    indices <- scans_app_filter_records(
      records,
      source,
      status,
      query,
      findings_only
    )
    indices <- indices[records$trajectory_id[indices] %in% context$ids]
    indices <- scans_app_order_records(records, indices, sort)
    reply(
      records[indices, setdiff(names(records), c("search", "index"))],
      offset,
      limit,
      content_offset
    )
  }
  summarize <- function(trajectory_ids = NULL) {
    selected <- ids(trajectory_ids)
    summary <- scans_app_performance_data(
      context$data,
      which(context$data$info$trajectory_id %in% selected)
    )
    scalar <- summary[
      !names(summary) %in%
        c("total", "patterns", "trajectories", "assessment_coverage")
    ]
    reply(
      tibble::as_tibble(scalar),
      limit = 1L,
      evidence = FALSE,
      extra = list(
        patterns = scans_tools_rows(summary$patterns, max_chars),
        assessment_coverage = scans_tools_rows(
          summary$assessment_coverage,
          max_chars
        )
      )
    )
  }
  read_run <- function(
    trajectory_id,
    table = "events",
    event_id = NULL,
    offset = 0L,
    limit = min(50L, max_rows),
    content_offset = 0L
  ) {
    scans_tools_string(trajectory_id, "trajectory_id")
    table <- rlang::arg_match(table, c("events", "turns"))
    result <- rows_for(context$data[[table]], trajectory_id)
    index <- if (table == "events") "event_index" else "turn_index"
    result <- result[order(result[[index]]), ]
    if (!is.null(event_id)) {
      scans_tools_string(event_id, "event_id")
      if (table != "events" || !event_id %in% result$event_id) {
        scans_abort(
          "This event is not recorded in the requested trajectory.",
          class = "scans_error_tools_scope"
        )
      }
      result <- result[result$event_id == event_id, ]
    }
    reply(result, offset, limit, content_offset)
  }
  find_events <- function(
    query = "",
    trajectory_ids = NULL,
    role = NULL,
    event_type = NULL,
    tool = NULL,
    status = NULL,
    offset = 0L,
    limit = min(50L, max_rows),
    content_offset = 0L
  ) {
    scans_tools_string(query, "query")
    for (name in c("role", "event_type", "tool", "status")) {
      scans_tools_string(get(name), name, TRUE)
    }
    selected <- ids(trajectory_ids)
    if (!length(selected)) {
      return(reply(context$data$events[FALSE, ], offset, limit, content_offset))
    }
    result <- filter_trajectory_events(
      context$bundle,
      trajectory_id = selected,
      role = role,
      event_type = event_type,
      tool = tool,
      status = status
    )
    result <- result[
      scans_query_text(result$text, query) |
        scans_query_text(result$error, query) |
        scans_query_text(result$name, query),
    ]
    reply(result, offset, limit, content_offset)
  }
  findings <- function(
    trajectory_ids = NULL,
    table = "findings",
    offset = 0L,
    limit = min(25L, max_rows),
    content_offset = 0L
  ) {
    table <- rlang::arg_match(table, c("findings", "assessments"))
    reply(
      rows_for(context$data[[table]], trajectory_ids),
      offset,
      limit,
      content_offset,
      extra = list(
        settings = context$settings,
        assessment_status = scans_tools_rows(
          scans_app_assessment_coverage(
            rows_for(context$data$assessments, trajectory_ids)
          ),
          max_chars
        )
      )
    )
  }
  measure <- function(
    trajectory_ids = NULL,
    measures = NULL,
    offset = 0L,
    limit = min(50L, max_rows),
    content_offset = 0L
  ) {
    result <- rows_for(context$data$measures, trajectory_ids)
    if (!is.null(measures)) {
      scans_tools_ids(measures, unique(context$data$measures$measure))
      result <- result[result$measure %in% measures, ]
    }
    groups <- split(result, result$measure)
    comparison <- lapply(groups, function(x) {
      known <- x$value[!is.na(x$value)]
      complete <- x$value[!is.na(x$value) & x$coverage == "complete"]
      list(
        measure = x$measure[[1L]],
        unit = x$unit[[1L]],
        n_total = nrow(x),
        n_recorded = length(known),
        n_complete = length(complete),
        median_recorded = scans_app_median(known),
        median_complete = scans_app_median(complete),
        min_recorded = if (length(known)) min(known) else NA_real_,
        max_recorded = if (length(known)) max(known) else NA_real_
      )
    })
    reply(
      result,
      offset,
      limit,
      content_offset,
      extra = list(comparison = unname(comparison))
    )
  }
  string <- function(description) {
    ellmer::type_string(description, required = FALSE)
  }
  array <- function(description) {
    ellmer::type_array(ellmer::type_string(), description, required = FALSE)
  }
  integer <- function(description) {
    ellmer::type_integer(description, required = FALSE)
  }
  paging <- list(
    offset = integer("Zero-based row offset."),
    limit = integer(paste("Maximum rows; no more than", max_rows)),
    content_offset = integer(
      "Zero-based character offset for oversized fields."
    )
  )
  definition <- function(fun, name, description, arguments) {
    ellmer::tool(
      fun,
      description,
      name = name,
      arguments = arguments,
      convert = FALSE,
      annotations = list(readOnlyHint = TRUE)
    )
  }
  list(
    scans_list_trajectories = definition(
      list_runs,
      "scans_list_trajectories",
      "Find and rank retained trajectories within the permitted snapshot.",
      c(
        list(
          query = string("Literal case-insensitive text search."),
          source = string("Exact source type."),
          status = string("Exact run status."),
          findings_only = ellmer::type_boolean(required = FALSE),
          sort = ellmer::type_enum(
            unname(scans_app_sort_choices),
            required = FALSE
          )
        ),
        paging
      )
    ),
    scans_summarize = definition(
      summarize,
      "scans_summarize",
      "Compute exact counts, pattern overlap, coverage and medians for the whole requested population.",
      list(trajectory_ids = array("Optional narrower trajectory scope."))
    ),
    scans_read_trajectory = definition(
      read_run,
      "scans_read_trajectory",
      "Read ordered events or turns, or retrieve a content slice of one event. No tools are replayed.",
      c(
        list(
          trajectory_id = ellmer::type_string("Permitted trajectory ID."),
          table = ellmer::type_enum(c("events", "turns"), required = FALSE),
          event_id = string("Read only this recorded event.")
        ),
        paging
      )
    ),
    scans_find_events = definition(
      find_events,
      "scans_find_events",
      "Search retained events by literal text, role, type, tool or recorded status.",
      c(
        list(
          query = string("Literal text in event text, error or name."),
          trajectory_ids = array("Optional narrower trajectory scope."),
          role = string("Exact role."),
          event_type = string("Exact event type."),
          tool = string("Exact tool name."),
          status = string("Exact status.")
        ),
        paging
      )
    ),
    scans_get_findings = definition(
      findings,
      "scans_get_findings",
      "Read findings or scanner assessments, including saved settings and coverage. No rescan.",
      c(
        list(
          trajectory_ids = array("Optional narrower trajectory scope."),
          table = ellmer::type_enum(
            c("findings", "assessments"),
            required = FALSE
          )
        ),
        paging
      )
    ),
    scans_measure = definition(
      measure,
      "scans_measure",
      "Read recorded resource measurements and descriptive comparisons computed in R. Unknown is not zero; elapsed includes pauses.",
      c(
        list(
          trajectory_ids = array("Optional narrower trajectory scope."),
          measures = array(
            "Optional exact measure names, such as elapsed or recorded_work."
          )
        ),
        paging
      )
    )
  )
}

scans_query_text <- function(text, query) {
  query <- trimws(tolower(query))
  if (!nzchar(query)) {
    return(rep(TRUE, length(text)))
  }
  !is.na(text) & grepl(query, tolower(text), fixed = TRUE)
}
