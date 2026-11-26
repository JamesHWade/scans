# A baseline and a candidate run of one dataset, built from solved and scored
# vitals Tasks. `candidate_only` names the cases that differ between runs.
comparison_chat <- function(case, side) {
  chat <- ellmer_chat_fixture()
  user <- ellmer::UserTurn(list(ellmer::ContentText(case)))
  answer <- function(text, tokens = c(10, 4, 0)) {
    ellmer::AssistantTurn(
      list(ellmer::ContentText(text)),
      tokens = tokens,
      duration = 0.5
    )
  }
  lookup <- function(id = "call-1") {
    ellmer::ContentToolRequest(id, "lookup", list(query = case))
  }
  tool_turns <- function(request, result, tokens = c(40, 12, 0)) {
    list(
      user,
      ellmer::AssistantTurn(list(request), tokens = tokens, duration = 0.5),
      ellmer::UserTurn(list(result)),
      answer("Done", tokens)
    )
  }
  candidate <- side == "candidate"
  turns <- if (candidate && case == "improve") {
    request <- lookup()
    tool_turns(request, ellmer::ContentToolResult("Found", request = request))
  } else if (candidate && case == "regress") {
    request <- lookup()
    tool_turns(
      request,
      ellmer::ContentToolResult(error = "timeout", request = request)
    )
  } else if (candidate && case == "unassessed") {
    request <- lookup(strrep("x", 70000))
    list(user, ellmer::AssistantTurn(list(request), tokens = c(10, 4, 0)))
  } else if (candidate && case == "untracked") {
    list(user, ellmer::AssistantTurn(list(ellmer::ContentText("Done"))))
  } else {
    list(user, answer("Done"))
  }
  chat$set_turns(turns)
  chat
}

comparison_samples <- function(side, cases, passing, epochs = 1L) {
  exact_scorer <- function(samples) {
    list(
      score = factor(
        ifelse(samples$input %in% passing, "C", "I"),
        levels = c("I", "C"),
        ordered = TRUE
      )
    )
  }
  task <- vitals::Task$new(
    dataset = tibble::tibble(id = cases, input = cases, target = "Done"),
    solver = function(inputs) {
      list(
        result = rep("Done", length(inputs)),
        solver_chat = lapply(inputs, comparison_chat, side = side)
      )
    },
    scorer = exact_scorer,
    epochs = epochs,
    dir = withr::local_tempdir()
  )
  suppressMessages({
    task$solve()
    task$score()
  })
  samples <- task$get_samples()
  # A single-epoch Task omits the column; binding runs needs it on every row.
  samples$epoch <- samples[["epoch"]] %||% 1L
  samples
}

comparison_cache <- new.env(parent = emptyenv())

comparison_investigation <- function(side) {
  if (is.null(comparison_cache[[side]])) {
    comparison_cache[[side]] <- comparison_build_investigation(side)
  }
  comparison_cache[[side]]
}

comparison_build_investigation <- function(side) {
  shared <- c("improve", "regress", "same", "unassessed", "untracked")
  samples <- if (side == "baseline") {
    comparison_samples(
      side,
      c(shared, "rescored", "repeated", "retired"),
      passing = c("regress", "same", "unassessed", "untracked", "rescored")
    )
  } else {
    repeated <- comparison_samples(side, "repeated", "repeated", epochs = 2L)
    rescored <- comparison_samples(side, "rescored", "rescored")
    rescored$scorer <- "lenient_scorer"
    vctrs::vec_rbind(
      comparison_samples(
        side,
        c(shared, "added"),
        passing = c("improve", "same", "unassessed", "untracked", "added")
      ),
      rescored,
      repeated
    )
  }
  investigation_snapshot(
    as_trajectory_vitals(samples, task_id = "support"),
    application = paste("Support assistant", side)
  )
}

comparison_scorers <- function() {
  declare <- function(label) {
    comparison_scorer(
      label,
      version = "1",
      scale = "binary",
      direction = "higher",
      levels = c("I", "C")
    )
  }
  list(declare("exact_scorer"), declare("lenient_scorer"))
}

comparison_pairing <- function(
  by = c("sample_id", "epoch"),
  dataset_version = "support-v1",
  scorers = comparison_scorers()
) {
  pair_trajectories(
    comparison_investigation("baseline"),
    comparison_investigation("candidate"),
    by = by,
    dataset_version = dataset_version,
    scorers = scorers
  )
}

# Two one-evaluation-per-case investigations without a producer, for scorer
# rules that vitals fixtures cannot vary one at a time.
comparison_manual_investigation <- function(
  values,
  scorer = "judge",
  ids = paste0("case-", seq_along(values)),
  task_id = NA_character_,
  trajectory_ids = paste0("run/", ids)
) {
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = trajectory_ids,
      task_id = task_id,
      sample_id = ids,
      epoch = 1L,
      source_type = "manual",
      status = "completed"
    ),
    data.frame(),
    data.frame(),
    evaluations = tibble::tibble(
      trajectory_id = trajectory_ids,
      evaluation_id = paste0(trajectory_ids, "/evaluation"),
      sample_id = ids,
      epoch = 1L,
      scorer = scorer,
      value = as.list(values)
    )
  )
  investigation_snapshot(bundle, application = "Manual")
}

# Seven cases that all change between runs in every family: the outcome rises,
# an error event appears, and output tokens increase. Enough changed cases for
# each summary to carry an exact sign test.
comparison_changed_investigation <- function(side, n = 7L) {
  ids <- paste0("case-", seq_len(n))
  trajectory_ids <- paste0("run/", ids)
  turn_ids <- paste0(trajectory_ids, "/turn")
  candidate <- side == "candidate"
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = trajectory_ids,
      sample_id = ids,
      source_type = "manual",
      status = "completed"
    ),
    tibble::tibble(
      trajectory_id = trajectory_ids,
      turn_id = turn_ids,
      turn_index = 1L,
      role = "assistant",
      input_tokens = 10,
      output_tokens = if (candidate) 9 else 4
    ),
    tibble::tibble(
      trajectory_id = trajectory_ids,
      event_id = paste0(trajectory_ids, "/event"),
      event_index = 1L,
      turn_id = turn_ids,
      content_index = 1L,
      event_type = if (candidate) "error" else "content",
      content_type = if (candidate) NA else "text",
      text = if (candidate) NA else "Done",
      status = if (candidate) "failed" else "completed",
      error = if (candidate) "Tool failed" else NA
    ),
    evaluations = tibble::tibble(
      trajectory_id = trajectory_ids,
      evaluation_id = paste0(trajectory_ids, "/evaluation"),
      scorer = "judge",
      value = as.list(rep(if (candidate) 1 else 0, n))
    )
  )
  investigation_snapshot(bundle, application = paste("Manual", side))
}
