comparison_case <- function(table, sample_id) {
  table[table$sample_id == sample_id, ]
}

comparison_manual <- function(baseline, candidate, scorer, ...) {
  compare_trajectories(pair_trajectories(
    comparison_manual_investigation(baseline, ...),
    comparison_manual_investigation(candidate, ...),
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(scorer)
  ))
}

test_that("outcome changes follow the scorer's direction and skip incompatible scorers", {
  skip_if_not_installed("vitals")
  comparison <- compare_trajectories(comparison_pairing())
  outcomes <- comparison$outcomes

  expect_identical(comparison$design$type, "paired")
  expect_identical(
    outcomes$change,
    c(
      "improved",
      "regressed",
      "unchanged",
      "unchanged",
      "unchanged",
      NA,
      NA,
      "improved"
    )
  )
  expect_identical(outcomes$delta, c(1, -1, 0, 0, 0, NA, NA, 1))
  rescored <- comparison_case(outcomes, "rescored")
  expect_identical(rescored$comparable, c(FALSE, FALSE))
  expect_identical(rescored$compatibility, rep("missing", 2L))
  expect_identical(rescored$baseline_value, c("C", NA))
  expect_identical(rescored$candidate_value, c(NA, "C"))
  expect_identical(
    unique(outcomes$baseline_trajectory_id),
    comparison$pairing$pairs$baseline_trajectory_id
  )
  expect_identical(
    outcomes$candidate_evaluation_id[[1L]],
    "vitals/improve/epoch-000001/evaluation-000001"
  )
})

test_that("pattern changes cite findings and never read an unassessed side as clean", {
  skip_if_not_installed("vitals")
  comparison <- compare_trajectories(comparison_pairing())
  patterns <- comparison$patterns
  candidate <- comparison$pairing$candidate

  appeared <- patterns[patterns$change == "appeared", ]
  expect_identical(appeared$sample_id, rep("regress", 2L))
  expect_setequal(appeared$scan, c("event_error", "turn_error"))
  expect_identical(lengths(appeared$baseline_finding_ids), c(0L, 0L))
  findings <- candidate$analysis$findings
  expect_setequal(unlist(appeared$candidate_finding_ids), findings$finding_id)
  event_error <- appeared[appeared$scan == "event_error", ]
  expect_identical(
    event_error$candidate_event_ids[[1L]],
    findings$event_id[findings$scan == "event_error"]
  )
  expect_identical(
    event_error$candidate_trajectory_id,
    "vitals/regress/epoch-000001"
  )

  unassessed <- patterns[patterns$change == "not_comparable", ]
  expect_identical(unassessed$sample_id, rep("unassessed", 3L))
  expect_identical(
    unassessed$candidate_status,
    rep("insufficient_evidence", 3L)
  )
  expect_match(unassessed$reason, "candidate was not assessed", all = TRUE)
  expect_true(all(
    patterns$change[!patterns$sample_id %in% c("regress", "unassessed")] ==
      "absent"
  ))
})

test_that("a scan run under different settings, or on one side only, is not comparable", {
  skip_if_not_installed("vitals")
  baseline <- comparison_investigation("baseline")
  bundle <- comparison_investigation("candidate")$bundle
  compare_with <- function(...) {
    candidate <- investigation_snapshot(bundle, application = "Candidate", ...)
    compare_trajectories(pair_trajectories(
      baseline,
      candidate,
      by = c("sample_id", "epoch"),
      dataset_version = "v1"
    ))$patterns
  }

  changed <- compare_with(repeat_threshold = 3L)
  repeated <- changed[changed$scan == "repeated_tool_call", ]
  expect_identical(unique(repeated$change), "not_comparable")
  expect_identical(
    unique(repeated$reason),
    "Scanner version or settings differ."
  )
  expect_false(
    "not_comparable" %in% changed$change[changed$scan == "error_chain"]
  )

  narrowed <- compare_with(scans = "event_error")
  skipped <- narrowed[narrowed$scan == "turn_error", ]
  expect_identical(unique(skipped$change), "not_comparable")
  expect_identical(unique(skipped$candidate_status), "not_run")
  expect_identical(
    comparison_case(
      narrowed[narrowed$scan == "event_error", ],
      "regress"
    )$change,
    "appeared"
  )
})

test_that("resource deltas need a complete recorded value on both sides", {
  skip_if_not_installed("vitals")
  comparison <- compare_trajectories(comparison_pairing())
  resources <- comparison$resources
  input <- resources[resources$measure == "input_tokens", ]

  improved <- comparison_case(input, "improve")
  expect_identical(improved$delta, 70)
  expect_identical(improved$unit, "tokens")
  expect_identical(
    c(improved$baseline_coverage, improved$candidate_coverage),
    c("complete", "complete")
  )
  expect_true(improved$baseline_includes_cached_input)
  expect_true(improved$candidate_includes_cached_input)
  expect_identical(
    comparison_case(comparison$outcomes, "improve")$change,
    "improved"
  )

  untracked <- comparison_case(input, "untracked")
  expect_identical(untracked$baseline_value, 10)
  expect_true(is.na(untracked$candidate_value))
  expect_true(is.na(untracked$delta))
  expect_false(untracked$comparable)
  expect_identical(untracked$candidate_coverage, "unavailable")
  expect_match(untracked$reason, "No recorded value on the candidate")

  cost <- resources[resources$measure == "cost", ]
  expect_true(all(is.na(cost$delta)))
  expect_identical(unique(cost$reason), "No recorded value on either side.")
})

test_that("resource values under different or unknown semantics give no delta", {
  skip_if_not_installed("ellmer")
  manual <- function() {
    investigation_snapshot(
      TrajectoryBundle(
        tibble::tibble(
          trajectory_id = "manual",
          sample_id = "case",
          source_type = "manual",
          status = "completed"
        ),
        tibble::tibble(
          trajectory_id = "manual",
          turn_id = "manual/turn",
          turn_index = 1L,
          role = "assistant",
          input_tokens = 30,
          output_tokens = 5
        ),
        data.frame()
      ),
      application = "Manual"
    )
  }
  ellmer <- investigation_snapshot(
    as_trajectory_ellmer(ellmer_chat_fixture(), sample_id = "case"),
    application = "Ellmer"
  )
  input <- function(baseline, candidate) {
    resources <- compare_trajectories(pair_trajectories(
      baseline,
      candidate,
      by = "sample_id",
      dataset_version = "v1"
    ))$resources
    resources[resources$measure == "input_tokens", ]
  }

  mixed <- input(manual(), ellmer)
  expect_identical(c(mixed$baseline_value, mixed$candidate_value), c(30, 4))
  expect_true(is.na(mixed$delta))
  expect_match(mixed$reason, "different semantics \\(source\\)")
  expect_true(mixed$candidate_includes_cached_input)
  expect_identical(mixed$baseline_includes_cached_input, NA)

  unknown <- input(manual(), manual())
  expect_true(is.na(unknown$delta))
  expect_identical(unknown$reason, "Cache inclusion is unknown on both sides.")
})

test_that("a resource row whose sides differ in unit shows both units", {
  pairing <- pair_trajectories(
    comparison_changed_investigation("baseline"),
    comparison_changed_investigation("candidate"),
    by = "sample_id",
    dataset_version = "v1"
  )
  # A saved investigation fixes each measure's unit, so change one directly
  # to reach the rows a different measurement contract would produce.
  measures <- pairing$candidate$analysis$measures
  measures$unit[
    measures$measure == "output_tokens" & measures$trajectory_id == "run/case-1"
  ] <- "k tokens"
  pairing$candidate$analysis$measures <- measures
  resources <- comparison_resources(pairing)
  output <- resources[resources$measure == "output_tokens", ]

  expect_identical(output$baseline_unit, rep("tokens", 7L))
  expect_identical(output$candidate_unit, c("k tokens", rep("tokens", 6L)))
  expect_identical(output$unit, c(NA, rep("tokens", 6L)))
  expect_true(is.na(output$delta[[1L]]))
  expect_match(output$reason[[1L]], "different semantics \\(unit\\)")

  measures <- pairing$baseline$analysis$measures
  measures$unit[
    measures$measure == "output_tokens" & measures$trajectory_id == "run/case-1"
  ] <- "k tokens"
  pairing$baseline$analysis$measures <- measures
  tables <- list(
    outcomes = comparison_outcomes(pairing),
    patterns = comparison_patterns(pairing),
    resources = comparison_resources(pairing)
  )
  summary <- comparison_paired_summaries(tables, pairing, 0.95)$resources
  output <- summary[summary$measure == "output_tokens", ]
  expect_identical(output$n, 7L)
  expect_identical(output$measure_unit, NA_character_)
  expect_identical(output$median_delta, NA_real_)
})

test_that("every pair of assessment statuses has one deliberate change", {
  statuses <- c(
    "assessed_with_findings",
    "assessed_no_findings",
    "not_applicable",
    "insufficient_evidence",
    "execution_failure"
  )
  expected <- matrix(
    "not_comparable",
    5L,
    5L,
    dimnames = list(baseline = statuses, candidate = statuses)
  )
  # Rows are the baseline status, columns the candidate status.
  expected[1:3, 1:3] <- rbind(
    c("persisted", "resolved", "no_longer_applicable"),
    c("appeared", "absent", "absent"),
    c("newly_applicable", "absent", "absent")
  )
  pairing <- pair_trajectories(
    comparison_manual_investigation(1),
    comparison_manual_investigation(1),
    by = "sample_id",
    dataset_version = "v1"
  )
  set_status <- function(pairing, side, status) {
    assessments <- pairing[[side]]$analysis$assessments
    assessments$status[assessments$scan == "event_error"] <- status
    pairing[[side]]$analysis$assessments <- assessments
    pairing
  }
  for (b in statuses) {
    for (c in statuses) {
      changed <- set_status(set_status(pairing, "baseline", b), "candidate", c)
      patterns <- comparison_patterns(changed)
      expect_identical(
        patterns$change[patterns$scan == "event_error"],
        expected[[b, c]],
        info = paste(b, "->", c)
      )
    }
  }
})

test_that("a pattern that no longer applies is not counted as resolved", {
  pairing <- pair_trajectories(
    comparison_changed_investigation("candidate"),
    comparison_changed_investigation("baseline"),
    by = "sample_id",
    dataset_version = "v1"
  )
  assessments <- pairing$candidate$analysis$assessments
  assessments$status[assessments$scan == "event_error"] <- "not_applicable"
  pairing$candidate$analysis$assessments <- assessments
  tables <- list(
    outcomes = comparison_outcomes(pairing),
    patterns = comparison_patterns(pairing),
    resources = comparison_resources(pairing)
  )
  summaries <- comparison_paired_summaries(tables, pairing, 0.95)
  expect_match(
    comparison_summary_lines(summaries),
    "Pattern event_error: appeared in 0, resolved in 0, persisted in 0, no longer applicable in 7; counts only",
    fixed = TRUE,
    all = FALSE
  )
  summary <- summaries$patterns[summaries$patterns$scan == "event_error", ]
  expect_identical(summary$n_no_longer_applicable, 7L)
  expect_identical(summary$n_resolved, 0L)
  expect_identical(summary$method, "counts")
  expect_true(is.na(summary$share_resolved))
  cases <- comparison_cases(pairing$pairs, tables)
  expect_identical(cases$patterns_no_longer_applicable, rep(1L, 7L))
  expect_identical(cases$patterns_resolved, rep(0L, 7L))
})

test_that("each case counts its changes per family without combining them", {
  skip_if_not_installed("vitals")
  cases <- compare_trajectories(comparison_pairing())$cases
  improve <- comparison_case(cases, "improve")
  regress <- comparison_case(cases, "regress")

  expect_identical(nrow(cases), 7L)
  expect_identical(
    c(improve$outcomes_improved, improve$resources_increased),
    c(1L, 3L)
  )
  expect_identical(
    c(regress$outcomes_regressed, regress$patterns_appeared),
    c(1L, 2L)
  )
  expect_identical(
    comparison_case(cases, "rescored")$outcomes_not_comparable,
    2L
  )
  expect_identical(
    comparison_case(cases, "unassessed")$patterns_not_comparable,
    3L
  )
  expect_identical(comparison_case(cases, "untracked")$resources_unknown, 4L)
  expect_false(any(grepl("score|health", names(cases))))
})

test_that("an exact sign test is reported only with enough changed cases", {
  scorer <- comparison_scorer(
    "judge",
    "1",
    "binary",
    levels = c("FALSE", "TRUE")
  )
  baseline <- c(rep(FALSE, 7L), TRUE, TRUE, TRUE)
  candidate <- c(rep(TRUE, 7L), FALSE, TRUE, TRUE)
  summary <- comparison_manual(baseline, candidate, scorer)$summaries$outcomes
  expected <- stats::binom.test(7L, 8L, conf.level = 0.9)

  expect_identical(
    c(summary$n, summary$n_improved, summary$n_regressed, summary$n_unchanged),
    c(10L, 7L, 1L, 2L)
  )
  expect_identical(summary$paired_difference, 0.6)
  expect_identical(summary$method, "exact sign test")
  expect_identical(summary$share_improved, 7 / 8)
  expect_identical(summary$p_value, stats::binom.test(7L, 8L)$p.value)

  wider <- compare_trajectories(
    pair_trajectories(
      comparison_manual_investigation(baseline),
      comparison_manual_investigation(candidate),
      by = "sample_id",
      dataset_version = "v1",
      scorers = list(scorer)
    ),
    conf_level = 0.9
  )$summaries$outcomes
  expect_identical(
    c(wider$conf_low, wider$conf_high),
    as.numeric(expected$conf.int)
  )

  few <- comparison_manual(baseline[1:5], candidate[1:5], scorer)
  few <- few$summaries$outcomes
  expect_identical(few$n_improved, 5L)
  expect_identical(few$method, "counts")
  expect_true(all(is.na(c(few$p_value, few$conf_low, few$share_improved))))
  expect_match(few$note, "Fewer than 6 changed cases")
  expect_identical(few$paired_difference, 1)
})

test_that("repeated epochs are counted but never tested", {
  scorer <- comparison_scorer("judge", "1", "numeric", "lower")
  investigation <- function(values) {
    ids <- rep(paste0("case-", 1:4), each = 2L)
    epochs <- rep(1:2, times = 4L)
    trajectory_ids <- paste0(ids, "/", epochs)
    investigation_snapshot(
      TrajectoryBundle(
        tibble::tibble(
          trajectory_id = trajectory_ids,
          sample_id = ids,
          epoch = epochs,
          source_type = "manual",
          status = "completed"
        ),
        data.frame(),
        data.frame(),
        evaluations = tibble::tibble(
          trajectory_id = trajectory_ids,
          evaluation_id = paste0(trajectory_ids, "/evaluation"),
          scorer = "judge",
          value = as.list(values)
        )
      ),
      application = "Epochs"
    )
  }
  comparison <- compare_trajectories(pair_trajectories(
    investigation(rep(5, 8L)),
    investigation(rep(2, 8L)),
    by = c("sample_id", "epoch"),
    dataset_version = "v1",
    scorers = list(scorer)
  ))
  summary <- comparison$summaries$outcomes

  expect_identical(unique(comparison$outcomes$change), "improved")
  expect_identical(unique(comparison$outcomes$delta), -3)
  expect_identical(summary$unit, "epoch pair")
  expect_identical(summary$n_improved, 8L)
  expect_identical(summary$median_delta, -3)
  expect_identical(summary$method, "counts")
  expect_true(is.na(summary$p_value))
  expect_match(summary$note, "Repeated epochs")
})

test_that("ordered scales give a direction but no numeric delta", {
  levels <- c("I", "P", "C")
  score <- function(x) lapply(x, factor, levels = levels, ordered = TRUE)
  scorer <- comparison_scorer("judge", "1", "ordered", levels = levels)
  comparison <- comparison_manual(
    score(c("I", "C", "P")),
    score(c("C", "P", "P")),
    scorer
  )

  expect_identical(
    comparison$outcomes$change,
    c("improved", "regressed", "unchanged")
  )
  expect_true(all(is.na(comparison$outcomes$delta)))
  expect_true(is.na(comparison$summaries$outcomes$paired_difference))

  off_scale <- comparison_manual(
    list("I", "maybe"),
    list("C", "C"),
    comparison_scorer("judge", "1", "ordered", levels = levels)
  )
  expect_identical(off_scale$outcomes$change, c("improved", NA))
  expect_identical(
    off_scale$outcomes$reason[[2L]],
    "A recorded value is not on the scorer's scale."
  )
  expect_identical(off_scale$summaries$outcomes$n_not_comparable, 1L)
})

test_that("unmatched cohorts get a labelled description and no paired estimate", {
  skip_if_not_installed("vitals")
  comparison <- compare_trajectories(comparison_pairing(
    dataset_version = c(baseline = "support-v1", candidate = "support-v2")
  ))

  expect_identical(comparison$design$type, "unpaired")
  expect_identical(nrow(comparison$outcomes), 0L)
  for (table in comparison$summaries) {
    expect_identical(unique(table$design), "unpaired")
    expect_setequal(table$side, c("baseline", "candidate"))
    expect_false(any(
      c("p_value", "conf_low", "paired_difference", "method") %in% names(table)
    ))
  }
  outcomes <- comparison$summaries$outcomes
  expect_identical(
    outcomes$n[outcomes$side == "baseline" & outcomes$value == "C"],
    5L
  )
  patterns <- comparison$summaries$patterns
  expect_identical(
    patterns$n_not_assessed[
      patterns$side == "candidate" & patterns$scan == "unresolved_tool_call"
    ],
    1L
  )
  resources <- comparison$summaries$resources
  expect_identical(
    resources$n_unknown[
      resources$side == "candidate" & resources$measure == "input_tokens"
    ],
    1L
  )
})

test_that("printing leads with what was not compared", {
  skip_if_not_installed("vitals")
  ids <- c("sha256:baseline", "sha256:candidate")
  comparison <- compare_trajectories(comparison_pairing())
  comparison$pairing$snapshots$snapshot_id <- ids
  expect_snapshot(print(comparison))

  unpaired <- compare_trajectories(comparison_pairing(dataset_version = NULL))
  unpaired$pairing$snapshots$snapshot_id <- ids
  expect_snapshot(print(unpaired))
})

test_that("printing reports the exact test of each family that carries one", {
  pairing <- pair_trajectories(
    comparison_changed_investigation("baseline"),
    comparison_changed_investigation("candidate"),
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(comparison_scorer("judge", "1", "numeric"))
  )
  comparison <- compare_trajectories(pairing)
  summaries <- comparison$summaries
  expect_identical(summaries$outcomes$method, "exact sign test")
  expect_identical(
    summaries$patterns$method[summaries$patterns$scan == "event_error"],
    "exact sign test"
  )
  expect_identical(
    summaries$resources$method[summaries$resources$measure == "output_tokens"],
    "exact sign test"
  )
  comparison$pairing$snapshots$snapshot_id <- c(
    "sha256:baseline",
    "sha256:candidate"
  )
  expect_snapshot(print(comparison))
})

test_that("an unpaired summary counts non-numeric values as off the scale", {
  pairing <- pair_trajectories(
    comparison_manual_investigation(list(TRUE, 2, 4, "x")),
    comparison_manual_investigation(list(1, 2, NA, Inf)),
    by = "sample_id",
    dataset_version = c(baseline = "v1", candidate = "v2"),
    scorers = list(comparison_scorer("judge", "1", "numeric"))
  )
  comparison <- compare_trajectories(pairing)
  outcomes <- comparison$summaries$outcomes
  expect_identical(outcomes$n, c(2L, 2L))
  expect_identical(outcomes$n_off_scale, c(2L, 2L))
  expect_identical(outcomes$median, c(3, 1.5))
  comparison$pairing$snapshots$snapshot_id <- c(
    "sha256:baseline",
    "sha256:candidate"
  )
  expect_snapshot(print(comparison))
})

test_that("printing strips terminal escapes from recorded labels", {
  withr::local_options(cli.num_colors = 1L)
  label <- "judge\033[31m red\033[39m"
  pairing <- pair_trajectories(
    comparison_manual_investigation(c(0, 0), scorer = label),
    comparison_manual_investigation(c(1, 1), scorer = label),
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(comparison_scorer(label, "1\033]8;;x\a", "numeric"))
  )
  comparison <- compare_trajectories(pairing)
  comparison$pairing$snapshots$application <- c(
    "Base\033[1mline",
    "Candidate"
  )
  printed <- paste(capture_messages(print(comparison)), collapse = "")
  expect_no_match(printed, "\033", fixed = TRUE)
  expect_match(printed, "Outcome judge red (version 1", fixed = TRUE)
})

test_that("a comparison checks the investigations against the pairing", {
  pairing <- pair_trajectories(
    comparison_manual_investigation(c(0, 1)),
    comparison_manual_investigation(c(1, 1)),
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(comparison_scorer("judge", "1", "numeric"))
  )
  tampered <- pairing
  assessments <- tampered$candidate$analysis$assessments
  assessments$status[] <- "assessed_with_findings"
  tampered$candidate$analysis$assessments <- assessments
  expect_error(
    compare_trajectories(tampered),
    class = "scans_error_investigation"
  )

  swapped <- pairing
  swapped$candidate <- comparison_manual_investigation(c(9, 9))
  expect_error(
    compare_trajectories(swapped),
    class = "scans_error_pairing"
  )
})

test_that("compare_trajectories() validates its inputs", {
  saved <- comparison_manual_investigation(c(1, 0))
  pairing <- pair_trajectories(
    saved,
    saved,
    by = "sample_id",
    dataset_version = "v"
  )
  expect_error(compare_trajectories(saved), class = "scans_error_pairing")
  expect_error(
    compare_trajectories(pairing, conf_level = 1),
    class = "scans_error_pairing"
  )
})

test_that("a scorer with no comparable outcome keeps a counts-only summary row", {
  missing <- comparison_manual(
    c(1, 0),
    c(NA, NA),
    comparison_scorer("judge", "1", "numeric")
  )
  summary <- missing$summaries$outcomes
  expect_identical(summary$scorer_id, "judge")
  expect_identical(summary$scorer_version, "1")
  expect_identical(summary$n, 0L)
  expect_identical(summary$n_not_comparable, 2L)
  expect_identical(summary$n_improved, 0L)
  expect_true(is.na(summary$median_delta))

  undeclared <- compare_trajectories(pair_trajectories(
    comparison_manual_investigation(c(1, 0)),
    comparison_manual_investigation(c(0, 1)),
    by = "sample_id",
    dataset_version = "v1"
  ))
  summary <- undeclared$summaries$outcomes
  expect_identical(summary$scorer_id, "judge")
  expect_true(is.na(summary$scorer_version))
  expect_true(is.na(summary$scale))
  expect_identical(summary$n, 0L)
  expect_identical(summary$n_not_comparable, 2L)
})
