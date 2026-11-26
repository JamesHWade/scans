# Route and provenance tools for scans_tools(). `reply`, `ids`, `definition`,
# and `paging` come from scans_tools_build() so bounds, scope, pagination, and
# evidence references follow the other tools.
scans_tools_routes <- function(
  context,
  reply,
  ids,
  definition,
  paging,
  max_rows,
  max_chars
) {
  cache <- new.env(parent = emptyenv())
  exchanges <- function() {
    if (is.null(cache$exchanges)) {
      cache$exchanges <- route_exchanges(context$bundle)
    }
    cache$exchanges
  }
  notes <- c(
    "Provenance classes come only from recorded commons:provenance events. Unclassified (absent or conflicting tags) is not untrusted; not_applicable means the source records no provenance.",
    "Provenance describes the recorded answer path, not answer correctness.",
    "Resource measures have trajectory grain and cover every exchange in a trajectory, not only one route."
  )
  denominators <- function(selected) {
    overall <- route_summary(
      context$bundle,
      "overall",
      selected,
      exchanges()
    )
    scans_tools_rows(
      overall[1L, setdiff(names(overall), route_class_columns)],
      max_chars
    )[[1L]]
  }
  counts <- function(
    trajectory_ids = NULL,
    by = "overall",
    offset = 0L,
    limit = min(25L, max_rows),
    content_offset = 0L
  ) {
    by <- rlang::arg_match(by, c("overall", "trajectory", "signature"))
    selected <- ids(trajectory_ids)
    result <- route_summary(context$bundle, by, selected, exchanges())
    if (by == "overall") {
      result <- result[route_class_columns]
    }
    reply(
      result,
      offset,
      limit,
      content_offset,
      extra = list(
        denominators = denominators(selected),
        route_notes = notes
      )
    )
  }
  candidates <- function(
    trajectory_ids = NULL,
    signature = NULL,
    basis = NULL,
    min_exchanges = 2L,
    offset = 0L,
    limit = min(25L, max_rows),
    content_offset = 0L
  ) {
    scans_tools_string(signature, "signature", TRUE)
    if (!is.null(basis)) {
      basis <- rlang::arg_match(basis, c("provenance", "unclassified_source"))
    }
    scans_tools_integer(min_exchanges, "min_exchanges", 1L)
    selected <- ids(trajectory_ids)
    table <- exchanges()
    table <- table[table$trajectory_id %in% selected, ]
    extra <- list(
      denominators = denominators(selected),
      route_notes = notes
    )
    if (is.null(signature)) {
      result <- route_candidates(
        context$bundle,
        selected,
        table,
        context$data$measures,
        min_exchanges,
        max_snippets = 0L
      )
      result <- result[
        is.null(basis) | result$candidate_basis %in% basis,
        setdiff(names(result), c("evidence", "snippets"))
      ]
      return(reply(result, offset, limit, content_offset, extra = extra))
    }
    table$candidate_basis <- route_candidate_basis(table)
    table <- table[
      table$route_signature %in%
        signature &
        !is.na(table$candidate_basis) &
        (is.null(basis) | table$candidate_basis %in% basis),
    ]
    if (!nrow(table)) {
      scans_abort(
        "{.arg signature} is not an exploratory route in the permitted scope.",
        class = "scans_error_tools_input"
      )
    }
    # Fields stay whole so reply() can slice them and honour content_offset.
    reply(
      route_snippets(context$bundle, table, .Machine$integer.max),
      offset,
      limit,
      content_offset,
      extra = extra
    )
  }
  string <- function(description) {
    ellmer::type_string(description, required = FALSE)
  }
  list(
    scans_route_counts = definition(
      counts,
      "scans_route_counts",
      "Count exchanges by recorded provenance class (trusted, cited, untrusted, unclassified, not_applicable) overall, per trajectory, or per tool-route signature, with denominators computed in R.",
      c(
        list(
          trajectory_ids = ellmer::type_array(
            ellmer::type_string(),
            "Optional narrower trajectory scope.",
            required = FALSE
          ),
          by = ellmer::type_enum(
            c("overall", "trajectory", "signature"),
            required = FALSE
          )
        ),
        paging
      )
    ),
    scans_route_candidates = definition(
      candidates,
      "scans_route_candidates",
      "Rank recurring exploratory or fallback tool routes as measure-promotion candidates. Pass one returned signature to read its recorded tool inputs and outputs with evidence references. Captured SQL and R are never run.",
      c(
        list(
          trajectory_ids = ellmer::type_array(
            ellmer::type_string(),
            "Optional narrower trajectory scope.",
            required = FALSE
          ),
          signature = string("Exact route signature to read evidence for."),
          basis = ellmer::type_enum(
            c("provenance", "unclassified_source"),
            required = FALSE
          ),
          min_exchanges = ellmer::type_integer(
            "Minimum eligible exchanges per candidate; default 2.",
            required = FALSE
          )
        ),
        paging
      )
    )
  )
}

route_class_columns <- c(
  "provenance_class",
  "n_exchanges",
  "n_trajectories",
  "n_users",
  "exchange_share"
)
