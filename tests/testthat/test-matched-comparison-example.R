test_that("the matched-comparison recipe compares held-out reviewed cases", {
  skip_if_not_installed("vitals")
  env <- new.env(parent = globalenv())
  sys.source(
    system.file("examples", "matched-comparison.R", package = "scans"),
    env
  )
  runs <- env$build_matched_comparison()
  provenance <- attr(runs$held_out, "scans_provenance")
  scorer <- comparison_scorer(
    "expected_answer",
    version = "1",
    scale = "binary",
    levels = c("I", "C")
  )
  pair <- function(baseline, candidate) {
    pair_trajectories(
      baseline,
      candidate,
      by = "sample_id",
      dataset_version = runs$examples$version_id,
      scorers = list(scorer)
    )
  }

  unrestricted <- pair(runs$baseline, runs$candidate)
  expect_identical(unrestricted$only_candidate$sample_id, "smoke-test")
  expect_identical(nrow(unrestricted$pairs), 3L)

  held_out_only <- function(investigation) {
    info <- trajectory_info(investigation$bundle)
    investigation_snapshot(
      investigation$bundle,
      trajectory_ids = info$trajectory_id[info$sample_id %in% runs$held_out$id],
      application = investigation$manifest$application
    )
  }
  comparison <- compare_trajectories(pair(
    held_out_only(runs$baseline),
    held_out_only(runs$candidate)
  ))
  group <- function(table) {
    provenance$group[match(table$sample_id, provenance$case_id)]
  }

  expect_identical(provenance$split, rep("test", 3L))
  expect_true(all(comparison$outcomes$sample_id %in% provenance$case_id))
  expect_identical(group(comparison$pairing$only_baseline), "order-D")
  expect_identical(nrow(comparison$pairing$only_candidate), 0L)
  expect_identical(group(comparison$outcomes), c("order-B", "order-C"))
  expect_identical(comparison$outcomes$change, c("improved", "regressed"))

  tokens <- comparison$resources[
    comparison$resources$measure == "input_tokens",
  ]
  expect_identical(tokens$delta, c(100, 100))
  appeared <- comparison$patterns[comparison$patterns$change == "appeared", ]
  expect_identical(unique(group(appeared)), "order-C")
  expect_identical(comparison$summaries$outcomes$method, "counts")

  # Each compared case leads back to the reviewed evidence it came from.
  origin <- provenance$origin[[match(
    comparison$outcomes$sample_id[[1L]],
    provenance$case_id
  )]]
  reviewed <- runs$examples$cases[[
    match(comparison$outcomes$sample_id[[1L]], provenance$case_id) + 1L
  ]]
  expect_identical(origin$snapshot_id, reviewed$origin$snapshot_id)
  expect_identical(reviewed$trajectory_id, "otel/follow-up")
})
