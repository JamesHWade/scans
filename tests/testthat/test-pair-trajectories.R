expect_each_trajectory_placed_once <- function(pairing) {
  for (side in c("baseline", "candidate")) {
    placed <- c(
      pairing$pairs[[paste0(side, "_trajectory_id")]],
      pairing[[paste0("only_", side)]]$trajectory_id,
      pairing$duplicates$trajectory_id[pairing$duplicates$side == side],
      pairing$missing_key$trajectory_id[pairing$missing_key$side == side]
    )
    expect_setequal(
      placed,
      trajectory_info(pairing[[side]]$bundle)$trajectory_id
    )
    expect_identical(anyDuplicated(placed), 0L)
  }
}

test_that("every trajectory lands in exactly one pairing table", {
  skip_if_not_installed("vitals")
  pairing <- comparison_pairing()

  expect_s3_class(pairing, "scans_pairing")
  expect_identical(pairing$key$by, c("sample_id", "epoch"))
  expect_identical(
    pairing$snapshots$snapshot_id,
    c(
      comparison_investigation("baseline")$manifest$snapshot_id,
      comparison_investigation("candidate")$manifest$snapshot_id
    )
  )
  expect_identical(pairing$snapshots$dataset_version, rep("support-v1", 2L))
  expect_identical(
    pairing$pairs$sample_id,
    c(
      "improve",
      "regress",
      "same",
      "unassessed",
      "untracked",
      "rescored",
      "repeated"
    )
  )
  expect_identical(pairing$only_baseline$sample_id, "retired")
  expect_identical(pairing$only_candidate$sample_id, c("added", "repeated"))
  expect_identical(pairing$only_candidate$epoch, 1:2)
  expect_identical(nrow(pairing$duplicates), 0L)
  expect_identical(nrow(pairing$missing_key), 0L)

  expect_each_trajectory_placed_once(pairing)
  expect_each_trajectory_placed_once(comparison_pairing(by = "sample_id"))

  baseline <- comparison_manual_investigation(
    c(1, 0, 1, 0, 1),
    ids = c("a", "b", "b", NA, "c"),
    task_id = c("t", "t", "t", "t", NA),
    trajectory_ids = paste0("base/", 1:5)
  )
  candidate <- comparison_manual_investigation(
    c(1, 1, 0, 1),
    ids = c("a", "b", NA, "d"),
    task_id = "t",
    trajectory_ids = paste0("cand/", 1:4)
  )
  mixed <- pair_trajectories(
    baseline,
    candidate,
    by = c("task_id", "sample_id"),
    dataset_version = "v1"
  )
  expect_identical(mixed$pairs$sample_id, "a")
  expect_identical(
    mixed$duplicates$trajectory_id,
    c("base/2", "base/3", "cand/2")
  )
  expect_identical(
    mixed$missing_key$trajectory_id,
    c("base/4", "base/5", "cand/3")
  )
  expect_identical(mixed$only_candidate$trajectory_id, "cand/4")
  expect_each_trajectory_placed_once(mixed)
})

test_that("repeated epochs are duplicates unless the key names the epoch", {
  skip_if_not_installed("vitals")
  pairing <- comparison_pairing(by = "sample_id")

  expect_false("repeated" %in% pairing$pairs$sample_id)
  expect_identical(pairing$duplicates$side, c("baseline", rep("candidate", 2L)))
  expect_identical(pairing$duplicates$n_candidate, rep(2L, 3L))
  expect_identical(pairing$duplicates$n_baseline, rep(1L, 3L))
  expect_identical(pairing$only_candidate$sample_id, "added")
  expect_true(all(is.na(pairing$pairs$epoch)))
})

test_that("a missing key value is reported and never matched", {
  baseline <- comparison_manual_investigation(c(1, 0))
  candidate <- comparison_manual_investigation(c(1, 1))

  undeclared <- pair_trajectories(baseline, candidate, by = "sample_id")
  expect_identical(nrow(undeclared$pairs), 0L)
  expect_identical(nrow(undeclared$missing_key), 4L)
  expect_identical(
    unique(unlist(undeclared$missing_key$missing)),
    "dataset_version"
  )

  no_task <- pair_trajectories(
    baseline,
    candidate,
    by = c("task_id", "sample_id"),
    dataset_version = "v1"
  )
  expect_identical(nrow(no_task$pairs), 0L)
  expect_identical(unique(unlist(no_task$missing_key$missing)), "task_id")
  expect_identical(nrow(no_task$only_baseline), 0L)

  one_sided <- pair_trajectories(
    baseline,
    candidate,
    by = "sample_id",
    dataset_version = c(baseline = "v1", candidate = "v2")
  )
  expect_identical(nrow(one_sided$pairs), 0L)
  expect_identical(nrow(one_sided$only_baseline), 2L)
  expect_identical(nrow(one_sided$only_candidate), 2L)
})

test_that("cases pair by identity, not by row position", {
  baseline <- comparison_manual_investigation(c(1, 0), ids = c("a", "b"))
  candidate <- comparison_manual_investigation(c(0, 1), ids = c("b", "a"))
  pairing <- pair_trajectories(
    baseline,
    candidate,
    by = "sample_id",
    dataset_version = "v1"
  )
  expect_identical(pairing$pairs$baseline_trajectory_id, c("run/a", "run/b"))
  expect_identical(pairing$pairs$candidate_trajectory_id, c("run/a", "run/b"))
})

test_that("pairing rejects a guessed key, a bundle, and malformed declarations", {
  saved <- comparison_manual_investigation(c(1, 0))
  expect_error(pair_trajectories(saved, saved), "by")
  for (by in list("epoch", "position", c("sample_id", "sample_id"), 1L)) {
    expect_error(
      pair_trajectories(saved, saved, by = by),
      class = "scans_error_pairing"
    )
  }
  expect_error(
    pair_trajectories(saved$bundle, saved, by = "sample_id"),
    class = "scans_error_pairing"
  )
  expect_error(
    pair_trajectories(
      saved,
      saved,
      by = "sample_id",
      dataset_version = c("v1", "v2")
    ),
    class = "scans_error_pairing"
  )
  expect_error(
    pair_trajectories(
      saved,
      saved,
      by = "sample_id",
      dataset_version = "v1",
      scorers = list(comparison_scorer("typo", "1", "numeric"))
    ),
    class = "scans_error_pairing"
  )
  expect_error(
    comparison_scorer("judge", "1", "binary", levels = c("a", "b", "c")),
    class = "scans_error_pairing"
  )
  expect_error(
    comparison_scorer("judge", "1", "numeric", levels = c("a", "b")),
    class = "scans_error_pairing"
  )
})

test_that("outcomes pair only under one scorer identity, version, scale, and direction", {
  skip_if_not_installed("vitals")
  pairing <- comparison_pairing()
  outcomes <- pairing$outcome_pairs
  rescored <- outcomes[outcomes$sample_id == "rescored", ]

  expect_identical(rescored$compatibility, rep("missing", 2L))
  expect_identical(rescored$baseline_scorer, c("exact_scorer", NA))
  expect_identical(rescored$candidate_scorer, c(NA, "lenient_scorer"))
  expect_true(all(
    outcomes$compatibility[outcomes$sample_id != "rescored"] == "compatible"
  ))

  undeclared <- comparison_pairing(scorers = NULL)
  expect_identical(undeclared$scorers$scale, rep("binary", 3L))
  expect_identical(undeclared$scorers$direction, rep("higher", 3L))
  expect_identical(unique(unlist(undeclared$scorers$unknown)), "version")
  same <- undeclared$outcome_pairs[
    undeclared$outcome_pairs$sample_id == "same",
  ]
  expect_identical(same$compatibility, "unknown")
  expect_identical(same$reason, "Scorer version is not recorded or declared.")
})

test_that("a declaration supplies what the record lacks and cannot contradict it", {
  baseline <- comparison_manual_investigation(c(0.2, 0.9))
  candidate <- comparison_manual_investigation(c(0.4, 0.9), scorer = "judge_v2")
  pair <- function(scorers) {
    pair_trajectories(
      baseline,
      candidate,
      by = "sample_id",
      dataset_version = "v1",
      scorers = scorers
    )
  }
  declare <- function(
    label,
    version = "1",
    direction = "higher",
    id = "judge"
  ) {
    comparison_scorer(label, version, "numeric", direction, id = id)
  }

  recorded <- pair(NULL)
  expect_identical(recorded$scorers$scale, rep("numeric", 2L))
  expect_identical(recorded$scorers$unknown[[1L]], c("version", "direction"))
  expect_identical(
    recorded$outcome_pairs$compatibility,
    rep("incompatible", 2L)
  )

  shared <- pair(list(declare("judge"), declare("judge_v2")))
  expect_identical(shared$outcome_pairs$compatibility, rep("compatible", 2L))
  expect_identical(shared$outcome_pairs$scorer_id, rep("judge", 2L))

  for (changed in list(
    list(version = "2", reason = "Scorer version differs."),
    list(direction = "lower", reason = "Scorer direction differs.")
  )) {
    candidate_scorer <- do.call(
      declare,
      c(list("judge_v2"), changed[names(changed) != "reason"])
    )
    out <- pair(list(
      baseline = list(declare("judge")),
      candidate = list(candidate_scorer)
    ))
    expect_identical(out$outcome_pairs$compatibility, rep("incompatible", 2L))
    expect_identical(out$outcome_pairs$reason, rep(changed$reason, 2L))
    expect_true(all(is.na(out$outcome_pairs$scorer_version)))
  }

  expect_error(
    pair(list(comparison_scorer("judge", "1", "binary", levels = c("0", "1")))),
    class = "scans_error_pairing"
  )
})

test_that("an unscored side is a missing outcome, not a failed one", {
  baseline <- comparison_manual_investigation(c(1, 0))
  candidate <- comparison_manual_investigation(c(1, 0))
  candidate$bundle@evaluations <- trajectory_evaluations(candidate$bundle)[1L, ]
  candidate <- investigation_snapshot(candidate$bundle, application = "Manual")
  pairing <- pair_trajectories(
    baseline,
    candidate,
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(comparison_scorer("judge", "1", "numeric"))
  )
  expect_identical(
    pairing$outcome_pairs$compatibility,
    c("compatible", "missing")
  )
  expect_identical(
    pairing$outcome_pairs$reason[[2L]],
    "No evaluation by this scorer on the candidate."
  )
  expect_true(is.na(pairing$outcome_pairs$candidate_evaluation_id[[2L]]))
})

test_that("printing a pairing leads with what was excluded", {
  skip_if_not_installed("vitals")
  pairing <- comparison_pairing(by = "sample_id")
  pairing$snapshots$snapshot_id <- c("sha256:baseline", "sha256:candidate")
  expect_snapshot(print(pairing))
})

test_that("a missing score does not hide the recorded scale from a declaration", {
  verdict <- factor(c("I", "C"), levels = c("I", "C"), ordered = TRUE)
  pair <- function(baseline, candidate, scorer = NULL) {
    pair_trajectories(
      comparison_manual_investigation(baseline),
      comparison_manual_investigation(candidate),
      by = "sample_id",
      dataset_version = "v1",
      scorers = scorer
    )
  }
  ordered <- list(list(verdict[1L], NA), list(verdict[2L], NA))
  logical <- list(list(FALSE, NA), list(TRUE, NA))

  recorded <- do.call(pair, ordered)
  expect_identical(recorded$scorers$scale, rep("binary", 2L))
  expect_identical(recorded$scorers$levels, rep(list(c("I", "C")), 2L))
  expect_identical(do.call(pair, logical)$scorers$scale, rep("binary", 2L))

  reversed <- list(
    comparison_scorer("judge", "1", "binary", levels = c("C", "I"))
  )
  numeric <- list(comparison_scorer("judge", "1", "numeric"))
  expect_error(
    do.call(pair, c(ordered, list(reversed))),
    class = "scans_error_pairing"
  )
  expect_error(
    do.call(pair, c(ordered, list(numeric))),
    class = "scans_error_pairing"
  )
  expect_error(
    pair(
      logical[[1L]],
      logical[[2L]],
      list(comparison_scorer(
        "judge",
        "1",
        "binary",
        levels = c("TRUE", "FALSE")
      ))
    ),
    class = "scans_error_pairing"
  )
})

test_that("a single evaluation against repeated ones is unknown, not missing", {
  saved <- function(values) {
    info <- tibble::tibble(
      trajectory_id = "run/a",
      sample_id = "a",
      source_type = "manual",
      status = "completed"
    )
    evaluations <- tibble::tibble(
      trajectory_id = "run/a",
      evaluation_id = paste0("run/a/", seq_along(values)),
      scorer = "judge",
      value = as.list(values)
    )
    bundle <- TrajectoryBundle(
      info,
      data.frame(),
      data.frame(),
      evaluations = evaluations
    )
    investigation_snapshot(bundle, application = "Manual")
  }
  pairing <- pair_trajectories(
    saved(1),
    saved(c(1, 2)),
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(comparison_scorer("judge", "1", "numeric"))
  )
  outcomes <- pairing$outcome_pairs
  expect_identical(outcomes$compatibility, rep("unknown", 3L))
  expect_identical(
    outcomes$reason[!is.na(outcomes$baseline_evaluation_id)],
    "The candidate has several evaluations by this scorer."
  )
})

test_that("leftover scorers recorded elsewhere on the other side are missing", {
  info <- tibble::tibble(
    trajectory_id = c("run/a", "run/b"),
    sample_id = c("a", "b"),
    source_type = "manual",
    status = "completed"
  )
  saved <- function(scorers_a) {
    scorer <- c(scorers_a, "A", "B")
    trajectory_id <- rep(c("run/a", "run/b"), c(length(scorers_a), 2L))
    evaluations <- tibble::tibble(
      trajectory_id = trajectory_id,
      evaluation_id = paste0(trajectory_id, "/", scorer),
      scorer = scorer,
      value = as.list(rep(1, length(scorer)))
    )
    bundle <- TrajectoryBundle(
      info,
      data.frame(),
      data.frame(),
      evaluations = evaluations
    )
    investigation_snapshot(bundle, application = "Manual")
  }
  pairing <- pair_trajectories(
    saved("A"),
    saved("B"),
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(
      comparison_scorer("A", "1", "numeric"),
      comparison_scorer("B", "1", "numeric")
    )
  )
  outcomes <- pairing$outcome_pairs[pairing$outcome_pairs$sample_id == "a", ]
  expect_identical(outcomes$compatibility, rep("missing", 2L))
  expect_identical(outcomes$baseline_scorer, c("A", NA))
  expect_identical(outcomes$candidate_scorer, c(NA, "B"))

  renamed <- pair_trajectories(
    comparison_manual_investigation(1, scorer = "A"),
    comparison_manual_investigation(1, scorer = "B"),
    by = "sample_id",
    dataset_version = "v1",
    scorers = list(
      comparison_scorer("A", "1", "numeric"),
      comparison_scorer("B", "1", "numeric")
    )
  )
  expect_identical(renamed$outcome_pairs$compatibility, "incompatible")
  expect_identical(renamed$outcome_pairs$reason, "Scorer identity differs.")
})

test_that("keys equal in different encodings pair", {
  utf8 <- "café"
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  expect_identical(Encoding(latin1), "latin1")
  pairing <- pair_trajectories(
    comparison_manual_investigation(1, ids = utf8, trajectory_ids = "run/a"),
    comparison_manual_investigation(1, ids = latin1, trajectory_ids = "run/a"),
    by = "sample_id",
    dataset_version = "v1"
  )
  expect_identical(nrow(pairing$pairs), 1L)
  expect_identical(nrow(pairing$only_baseline), 0L)
  expect_identical(nrow(pairing$only_candidate), 0L)
})

test_that("an ordered factor reads higher as better unless declared otherwise", {
  verdict <- factor(c("I", "C"), levels = c("I", "C"), ordered = TRUE)
  pair <- function(scorers) {
    pair_trajectories(
      comparison_manual_investigation(list(verdict[1L])),
      comparison_manual_investigation(list(verdict[2L])),
      by = "sample_id",
      dataset_version = "v1",
      scorers = scorers
    )
  }
  expect_identical(pair(NULL)$scorers$direction, rep("higher", 2L))

  lower <- pair(list(
    comparison_scorer("judge", "1", "binary", "lower", levels = c("I", "C"))
  ))
  expect_identical(lower$scorers$direction, rep("lower", 2L))
  expect_identical(lower$outcome_pairs$compatibility, "compatible")
  expect_identical(lower$outcome_pairs$direction, "lower")

  expect_error(
    pair(list(
      comparison_scorer("judge", "1", "binary", "lower", levels = c("C", "I"))
    )),
    class = "scans_error_pairing"
  )
})

test_that("conflicting recorded scales on one side reject a declaration", {
  forward <- factor("C", levels = c("I", "C"), ordered = TRUE)
  reversed <- factor("C", levels = c("C", "I"), ordered = TRUE)
  pair <- function(baseline, scorers = NULL) {
    pair_trajectories(
      comparison_manual_investigation(baseline),
      comparison_manual_investigation(list(forward, forward)),
      by = "sample_id",
      dataset_version = "v1",
      scorers = scorers
    )
  }
  declared <- list(
    comparison_scorer("judge", "1", "binary", levels = c("I", "C"))
  )
  for (baseline in list(list(forward, reversed), list(forward, 1))) {
    undeclared <- pair(baseline)
    expect_true(is.na(undeclared$scorers$scale[[1L]]))
    expect_identical(
      undeclared$outcome_pairs$compatibility,
      rep("unknown", 2L)
    )
    expect_error(pair(baseline, declared), class = "scans_error_pairing")
  }
})
