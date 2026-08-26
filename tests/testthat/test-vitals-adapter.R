test_that("vitals Tasks pass the shared adapter contract", {
  skip_if_not_installed("ellmer", "0.4.2")
  skip_if_not_installed("vitals", "0.3.0")

  expect_adapter_conforms(
    source = vitals_task_fixture(),
    adapter = as_trajectory_vitals
  )
})

test_that("vitals sample data becomes evaluated solver trajectories", {
  skip_if_not_installed("ellmer", "0.4.2")

  bundle <- as_trajectory_vitals(
    vitals_samples_fixture(),
    run_id = "run-1",
    task_id = "task-1",
    source_uri = "https://user:secret@example.com/eval.json?token=secret",
    include_system_prompt = FALSE
  )
  info <- trajectory_info(bundle)
  evaluations <- trajectory_evaluations(bundle)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(
    info$trajectory_id,
    c("vitals/sample-a/epoch-000001", "vitals/sample-b/epoch-000002")
  )
  expect_identical(info$run_id, rep("run-1", 2L))
  expect_identical(info$source_type, rep("vitals", 2L))
  expect_identical(info$source_id, c("sample-a", "sample-b"))
  expect_identical(info$source_uri, rep("https://example.com/eval.json", 2L))
  expect_identical(info$task_id, rep("task-1", 2L))
  expect_identical(info$sample_id, c("sample-a", "sample-b"))
  expect_identical(info$epoch, c(1L, 2L))
  expect_identical(nrow(trajectory_turns(bundle)), 4L)
  expect_identical(nrow(trajectory_events(bundle)), 4L)

  expect_identical(
    evaluations$evaluation_id,
    c(
      "vitals/sample-a/epoch-000001/evaluation-000001",
      "vitals/sample-b/epoch-000002/evaluation-000001"
    )
  )
  expect_identical(as.character(evaluations$value[[1L]]), "C")
  expect_identical(as.character(evaluations$value[[2L]]), "I")
  expect_identical(evaluations$target, list("Hi", "Hi"))
  expect_identical(evaluations$scorer, rep("detect_exact", 2L))
  expect_identical(
    evaluations$explanation,
    c("Exact match", "Different wording")
  )
  expect_identical(info$metadata[[1L]]$sample$input, "Say hello")
  expect_identical(info$metadata[[1L]]$sample$result, "Hi")
  expect_identical(info$metadata[[1L]]$sample$case, "baseline")
  expect_identical(info$metadata[[1L]]$sample$api_key, "<redacted>")
  expect_identical(evaluations$metadata[[1L]], list(scorer_metadata = NULL))
  expect_no_match(
    capture.output(str(S7::props(bundle))),
    "first-secret|second-secret"
  )
  expect_setequal(unique(trajectory_losses(bundle)$reason), "redacted")
})

test_that("vitals identities do not depend on sample row order", {
  skip_if_not_installed("ellmer", "0.4.2")
  samples <- vitals_samples_fixture()

  original <- as_trajectory_vitals(samples)
  reordered <- as_trajectory_vitals(samples[2:1, ])
  original_info <- trajectory_info(original)
  reordered_info <- trajectory_info(reordered)

  expect_identical(
    original_info$trajectory_id[order(original_info$sample_id)],
    reordered_info$trajectory_id[order(reordered_info$sample_id)]
  )
})

test_that("vitals Task objects snapshot through their public samples", {
  skip_if_not_installed("ellmer", "0.4.2")
  skip_if_not_installed("vitals", "0.3.0")
  task <- vitals_task_fixture()

  explicit <- as_trajectory_vitals(task)
  generic <- as_trajectory(task)

  expect_identical(S7::props(generic), S7::props(explicit))
  expect_identical(trajectory_info(explicit)$source_type, "vitals")
  expect_identical(trajectory_info(explicit)$sample_id, "1")
  expect_identical(trajectory_evaluations(explicit)$value[[1L]], "C")
})

test_that("vitals Task subclasses may override public methods", {
  skip_if_not_installed("ellmer", "0.4.2")
  skip_if_not_installed("vitals", "0.3.0")

  subclass <- R6::R6Class(
    "ScansVitalsTask",
    inherit = vitals::Task,
    public = list(get_samples = function() super$get_samples())
  )
  bundle <- as_trajectory(vitals_task_fixture(subclass))

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(trajectory_info(bundle)$source_type, "vitals")
})

test_that("malformed Task objects fail at the adapter boundary", {
  skip_if_not_installed("vitals", "0.3.0")

  fake_task <- new.env(parent = emptyenv())
  fake_task$get_samples <- function() data.frame()
  fake_task$solve <- function() NULL
  fake_task$score <- function() NULL
  class(fake_task) <- c("Task", "R6")

  condition <- rlang::catch_cnd(as_trajectory(fake_task))

  expect_s3_class(condition, "scans_error_vitals_source")
})

test_that("development vitals logs round-trip into trajectories", {
  skip_if_not_installed("ellmer", "0.4.2")
  skip_if_not_installed("vitals", "0.3.0.9001")
  log_dir <- withr::local_tempdir()
  task <- vitals_task_fixture()
  log_path <- task$log(log_dir)
  samples <- vitals::vitals_log_read(
    log_path,
    solver_chat = ellmer_chat_fixture()
  )

  bundle <- as_trajectory_vitals(samples, source_uri = log_path)

  expect_identical(trajectory_info(bundle)$sample_id, "1")
  expect_identical(trajectory_info(bundle)$source_uri, log_path)
  expect_identical(trajectory_evaluations(bundle)$value[[1L]], "C")
  expect_gt(nrow(trajectory_events(bundle)), 0L)
})

test_that("unscored vitals samples retain trajectories without evaluations", {
  skip_if_not_installed("ellmer", "0.4.2")

  bundle <- as_trajectory_vitals(vitals_samples_fixture(scored = FALSE))

  expect_identical(nrow(trajectory_info(bundle)), 2L)
  expect_identical(nrow(trajectory_evaluations(bundle)), 0L)
})

test_that("vitals scorer chats produce an explicit adapter loss", {
  skip_if_not_installed("ellmer", "0.4.2")
  samples <- vitals_samples_fixture()
  samples$scorer_chat <- samples$solver_chat

  bundle <- as_trajectory_vitals(samples)
  losses <- trajectory_losses(bundle)

  expect_identical(sum(losses$field == "scorer_chat"), 2L)
  expect_setequal(unique(losses$reason), c("redacted", "unsupported"))
})

test_that("vitals source errors identify unsupported sample shapes", {
  skip_if_not_installed("ellmer", "0.4.2")

  expect_snapshot(error = TRUE, as_trajectory_vitals(list()))
  expect_snapshot(
    error = TRUE,
    as_trajectory_vitals(tibble::tibble(id = "sample-1"))
  )

  duplicate <- vitals_samples_fixture()
  duplicate$id[[2L]] <- duplicate$id[[1L]]
  duplicate$epoch[[2L]] <- duplicate$epoch[[1L]]
  expect_snapshot(error = TRUE, as_trajectory_vitals(duplicate))

  invalid_chat <- vitals_samples_fixture()
  invalid_chat$solver_chat[[1L]] <- "not a chat"
  expect_snapshot(error = TRUE, as_trajectory_vitals(invalid_chat))
})
