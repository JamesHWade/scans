test_that("exchanges take provenance only from recorded commons events", {
  bundle <- route_commons_bundle()
  exchanges <- route_exchanges(bundle)

  expect_identical(
    exchanges$exchange_id,
    c(
      sprintf("commons/conversation-001/exchange-%06d", 1:2),
      sprintf("commons/conversation-002/exchange-%06d", 1:2),
      sprintf("commons/conversation-003/exchange-%06d", 1:3),
      sprintf("commons/conversation-004/exchange-%06d", 1:2)
    )
  )
  expect_identical(
    exchanges$provenance_class,
    c(
      "untrusted",
      "trusted",
      "untrusted",
      "cited",
      "cited",
      "unclassified",
      "unclassified",
      "untrusted",
      "cited"
    )
  )
  expect_identical(
    exchanges$provenance_reason[exchanges$provenance_class == "unclassified"],
    c("conflicting_tags", "missing_event")
  )
  expect_identical(
    exchanges$user,
    rep(c("ana", "ben", NA, "ana"), c(2L, 2L, 3L, 2L))
  )
  expect_true(all(exchanges$exchange_basis == "round_index"))
  expect_true(all(is.na(exchanges$timestamp)))

  events <- trajectory_events(bundle)
  provenance <- events$event_id[events$event_type == "commons:provenance"]
  expect_setequal(unlist(exchanges$provenance_event_ids), provenance)
  expect_length(exchanges$provenance_event_ids[[7L]], 0L)
})

test_that("unknown provenance stays unclassified and plain sources are not applicable", {
  skip_if_not_installed("ellmer", "0.5.0")
  source <- route_commons_cohort()["conversation-001"]
  attr(source[[1L]]$turns, "provenance") <- NULL
  commons <- route_exchanges(as_trajectory_commons(source))
  expect_identical(commons$provenance_class, rep("unclassified", 2L))
  expect_identical(commons$provenance_reason, rep("missing_event", 2L))

  attr(source[[1L]]$turns, "provenance") <- list(
    list(provenance_tag = NA_character_, citation_decisions = list()),
    list(provenance_tag = "D", citation_decisions = list())
  )
  tagged <- route_exchanges(as_trajectory_commons(source))
  expect_identical(tagged$provenance_class, rep("unclassified", 2L))
  expect_identical(
    tagged$provenance_reason,
    c("missing_tag", "unrecognized_tag")
  )

  records <- tibble::tibble(tag = list("B", "B"))
  expect_identical(route_exchange_class(records, TRUE)$class, "cited")
  records$tag[[2L]] <- NA_character_
  expect_identical(
    route_exchange_class(records, TRUE)$reason,
    "conflicting_tags"
  )

  ellmer <- route_exchanges(route_ellmer_bundle())
  expect_identical(ellmer$provenance_class, rep("not_applicable", 2L))
  expect_identical(ellmer$exchange_basis, rep("derived", 2L))
})

test_that("route signatures are canonical tool chains", {
  bundle <- route_commons_bundle()
  exchanges <- route_exchanges(bundle)

  expect_identical(
    exchanges$route_signature,
    c(
      "describe_table > run_sql > run_r",
      "call_measure",
      "describe_table > run_sql > run_r",
      "run_sql",
      "describe_table > run_sql > run_r",
      "run_sql",
      "call_measure",
      "describe_table + search_context > run_sql",
      "run_sql"
    )
  )
  expect_identical(exchanges$n_tool_calls[[6L]], 2L)
  expect_identical(exchanges$n_steps[[6L]], 1L)
  expect_identical(
    exchanges$route_steps[[8L]],
    c("describe_table + search_context", "run_sql")
  )

  short <- route_exchanges(bundle, max_steps = 2L)
  expect_identical(
    short$route_signature[[1L]],
    "describe_table > run_sql > ..."
  )
  expect_true(short$route_truncated[[1L]])
  expect_identical(short$n_tool_calls, exchanges$n_tool_calls)
})

test_that("cohort counts keep unclassified separate with explicit denominators", {
  bundle <- route_commons_bundle()
  overall <- route_summary(bundle)

  expect_identical(overall$provenance_class, route_classes)
  expect_identical(overall$n_exchanges, c(1L, 3L, 3L, 2L, 0L))
  expect_identical(overall$n_trajectories, c(1L, 3L, 3L, 1L, 0L))
  expect_identical(unique(overall$total_exchanges), 9L)
  expect_identical(unique(overall$total_trajectories), 4L)
  expect_identical(unique(overall$total_users), 2L)
  expect_identical(unique(overall$n_unknown_user_trajectories), 1L)
  expect_identical(unique(overall$n_timestamped_exchanges), 0L)
  expect_identical(
    overall$trajectory_window_start[[1L]],
    as.POSIXct("2026-11-01 10:00:00", tz = "UTC")
  )
  expect_true(is.na(overall$event_window_start[[1L]]))

  scoped <- route_summary(bundle, trajectory_ids = "commons/conversation-003")
  expect_identical(scoped$n_exchanges, c(0L, 1L, 0L, 2L, 0L))
  expect_identical(unique(scoped$total_users), 0L)

  trajectories <- route_summary(bundle, "trajectory")
  expect_identical(trajectories$n_unclassified, c(0L, 0L, 2L, 0L))
  expect_identical(trajectories$n_tool_calls, c(4L, 4L, 6L, 4L))
  expect_true(all(trajectories$provenance_source))

  signatures <- route_summary(bundle, "signature")
  expect_identical(signatures$route_signature[[2L]], "run_sql")
  expect_identical(
    unlist(signatures[2L, paste0("n_", route_classes)], use.names = FALSE),
    c(0L, 2L, 0L, 1L, 0L)
  )
})

test_that("promotion candidates rank recurring exploratory routes with evidence", {
  bundle <- route_commons_bundle()
  candidates <- route_candidates(bundle)

  expect_identical(candidates$rank, 1:2)
  expect_identical(
    candidates$route_signature,
    c("describe_table > run_sql > run_r", "run_sql")
  )
  expect_identical(candidates$candidate_basis, rep("provenance", 2L))
  expect_identical(candidates$n_trajectories, c(3L, 2L))
  expect_identical(candidates$n_exchanges, c(3L, 2L))
  expect_identical(candidates$n_users, c(2L, 2L))
  expect_identical(candidates$n_unknown_user_trajectories, c(1L, 0L))
  expect_identical(candidates$n_untrusted, c(2L, 0L))
  expect_identical(candidates$n_cited, c(1L, 2L))
  expect_identical(candidates$n_unclassified, c(0L, 1L))

  events <- trajectory_events(bundle)
  turns <- trajectory_turns(bundle)
  for (evidence in candidates$evidence) {
    for (i in seq_len(nrow(evidence))) {
      id <- evidence$trajectory_id[[i]]
      expect_in(
        evidence$event_ids[[i]],
        events$event_id[events$trajectory_id == id]
      )
      expect_in(
        evidence$turn_ids[[i]],
        turns$turn_id[turns$trajectory_id == id]
      )
    }
  }
  first <- candidates$evidence[[1L]]
  expect_identical(
    events$name[match(first$event_ids[[1L]][1:3], events$event_id)],
    c("describe_table", "run_sql", "run_r")
  )
  expect_in(
    "commons:provenance",
    events$event_type[match(first$event_ids[[1L]], events$event_id)]
  )

  all <- route_candidates(bundle, min_exchanges = 1L)
  expect_identical(
    all$route_signature,
    c(
      "describe_table > run_sql > run_r",
      "run_sql",
      "describe_table + search_context > run_sql"
    )
  )
})

test_that("candidate resources keep trajectory grain and coverage", {
  bundle <- route_commons_bundle()
  resources <- route_candidates(bundle)$resources[[1L]]
  tokens <- resources[resources$measure == "input_tokens", ]

  expect_identical(unique(resources$grain), "trajectory")
  expect_identical(tokens$n_total, 3L)
  expect_identical(tokens$n_recorded, 2L)
  expect_identical(tokens$n_complete, 1L)
  expect_equal(tokens$median_complete, 500)
  expect_true(is.na(resources$median_recorded[resources$measure == "cost"]))
  expect_identical(
    nrow(route_candidates(bundle, measures = NULL)$resources[[1L]]),
    0L
  )
})

test_that("snippets bound recorded inputs and outputs without running them", {
  bundle <- route_commons_bundle()
  snippets <- route_candidates(bundle, max_chars = 20L)$snippets[[1L]]

  expect_identical(nrow(snippets), 5L)
  expect_true(all(snippets$calculation))
  expect_identical(snippets$tool[1:2], c("run_sql", "run_r"))
  expect_identical(substr(snippets$input[[1L]], 1L, 8L), "{\"sql\":\"")
  expect_identical(nchar(snippets$input[[1L]]), 20L)
  expect_gt(snippets$input_chars[[1L]], 20L)
  expect_identical(snippets$output[[1L]], "west 120; east 80")

  exchanges <- route_exchanges(bundle)
  retried <- route_snippets(bundle, exchanges[6L, ])
  expect_identical(retried$step, 1:2)
  expect_identical(retried$output, c("Error: no such column", "7"))
})

test_that("argument features are optional heuristics", {
  bundle <- route_commons_bundle()
  features <- route_features(bundle)
  first <- features[features$trajectory_id == "commons/conversation-001", ]

  expect_identical(
    first$feature,
    c("table", "sql_table", "sql_table", "measure")
  )
  expect_identical(first$value, c("orders", "orders", "regions", "revenue"))
  expect_identical(first$heuristic, c(FALSE, TRUE, TRUE, FALSE))
  expect_in(features$event_id, trajectory_events(bundle)$event_id)

  expect_identical(
    route_sql_tables(paste(
      "WITH recent AS (SELECT * FROM sales.orders -- FROM ignored",
      "WHERE note <> 'from x') SELECT EXTRACT(year FROM ordered_at)",
      "FROM recent JOIN \"Regions\" r ON a IS DISTINCT FROM b",
      sep = "\n"
    )),
    c("sales.orders", "regions")
  )
})

test_that("sources without provenance yield labelled multi-step candidates", {
  bundle <- route_ellmer_bundle()
  candidates <- route_candidates(bundle)
  overall <- route_summary(bundle)

  expect_identical(candidates$route_signature, "lookup > summarise")
  expect_identical(candidates$candidate_basis, "unclassified_source")
  expect_identical(candidates$n_trajectories, 2L)
  expect_identical(candidates$n_not_applicable, 2L)
  expect_identical(candidates$n_unclassified, 0L)
  expect_identical(overall$n_exchanges, c(0L, 0L, 0L, 0L, 2L))
  expect_identical(unique(overall$n_unknown_user_trajectories), 2L)
})

test_that("route tools compute counts and candidates in the permitted scope", {
  skip_if_not_installed("ellmer", "0.5.0")
  bundle <- route_commons_bundle()
  tools <- scans_tools(bundle)

  counts <- scans_tool_result(tools$scans_route_counts)
  expect_equal(
    vapply(counts$data, function(x) x$n_exchanges, numeric(1)),
    c(1, 3, 3, 2, 0)
  )
  expect_equal(counts$denominators$total_exchanges, 9)
  expect_equal(counts$denominators$n_unknown_user_trajectories, 1)
  expect_null(counts$denominators$event_window_start)

  paged <- scans_tool_result(
    tools$scans_route_counts,
    by = "trajectory",
    limit = 1L
  )
  expect_equal(paged$paging$total, 4L)
  expect_equal(paged$denominators$total_exchanges, 9)
  expect_identical(
    paged$evidence[[1L]]$trajectory_id,
    paged$data[[1L]]$trajectory_id
  )

  candidates <- scans_tool_result(tools$scans_route_candidates)
  expect_identical(
    vapply(candidates$data, function(x) x$route_signature, character(1)),
    c("describe_table > run_sql > run_r", "run_sql")
  )
  expect_null(candidates$data[[1L]]$snippets)

  evidence <- scans_tool_result(
    tools$scans_route_candidates,
    signature = "run_sql"
  )
  expect_equal(evidence$paging$total, 2L)
  expect_identical(
    vapply(evidence$evidence, function(x) x$event_id, character(1)),
    vapply(evidence$data, function(x) x$event_id, character(1))
  )
  expect_error(
    tools$scans_route_candidates(signature = "call_measure"),
    class = "scans_error_tools_input"
  )

  scoped <- scans_tools(bundle, "commons/conversation-003", max_rows = 2L)
  result <- scans_tool_result(scoped$scans_route_counts)
  expect_equal(result$denominators$total_exchanges, 3)
  expect_equal(result$scope$trajectories, 1L)
  expect_length(scans_tool_result(scoped$scans_route_candidates)$data, 0L)
  for (name in c("scans_route_counts", "scans_route_candidates")) {
    expect_error(
      scoped[[name]](trajectory_ids = "commons/conversation-001"),
      class = "scans_error_tools_scope"
    )
  }
  expect_error(
    scoped$scans_route_counts(limit = 3L),
    class = "scans_error_tools_limit"
  )
  expect_error(
    scoped$scans_route_candidates(min_exchanges = 0L),
    class = "scans_error_tools_limit"
  )
})

test_that("candidate evidence passes into review cases and regression data", {
  bundle <- route_commons_bundle()
  candidate <- route_candidates(bundle)[1L, ]
  evidence <- candidate$evidence[[1L]][1L, ]
  saved <- investigation_snapshot(bundle)

  case <- review_case(
    saved,
    evidence$trajectory_id,
    event_ids = evidence$event_ids[[1L]],
    turn_ids = evidence$turn_ids[[1L]]
  )
  case <- review_decision(
    case,
    "confirmed",
    "analyst",
    "new: no governed measure covers regional revenue share. trajectory analysis (2026-11-25)",
    expected_behavior = "Answer with call_measure(\"revenue_share_by_region\")."
  )
  events <- trajectory_events(bundle)
  question <- events$text[
    events$turn_id == evidence$turn_ids[[1L]][[1L]] &
      events$event_type == "content"
  ]
  examples <- review_examples(
    list(case),
    "application",
    groups = candidate$route_signature,
    splits = "test",
    inputs = list(list(question = question)),
    targets = list(list(route = "call_measure"))
  )
  data <- review_dataset(examples, "vitals", "test")

  expect_identical(review_status(case)$status, "confirmed")
  expect_identical(data$input[[1L]]$question, "Share of revenue by region?")
  expect_identical(
    attr(data, "scans_provenance")$expected_behavior,
    "Answer with call_measure(\"revenue_share_by_region\")."
  )
})
