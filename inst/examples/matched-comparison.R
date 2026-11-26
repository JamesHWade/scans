# Source this file, then call build_matched_comparison(). Every model reply
# is a fixed transcript, so the result demonstrates the workflow and says
# nothing about a real application change.
build_matched_comparison <- function() {
  for (package in c("scans", "vitals", "ellmer", "jsonlite", "digest")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Install ", package)
  }

  # Versions of the application, as fixed transcripts. `lookup` says
  # whether the version calls its order-status tool and how that call ends.
  transcript <- function(question, answer, lookup = "none") {
    chat <- ellmer::chat_openai(
      model = "offline",
      credentials = function() "unused"
    )
    user <- ellmer::UserTurn(list(ellmer::ContentText(question)))
    reply <- function(tokens) {
      ellmer::AssistantTurn(
        list(ellmer::ContentText(answer)),
        tokens = tokens,
        duration = 0.4
      )
    }
    turns <- if (lookup == "none") {
      list(user, reply(c(20, 6, 0)))
    } else {
      request <- ellmer::ContentToolRequest(
        "call-1",
        "order_status",
        list(question = question)
      )
      result <- if (lookup == "failed") {
        ellmer::ContentToolResult(error = "timeout", request = request)
      } else {
        ellmer::ContentToolResult("unavailable", request = request)
      }
      list(
        user,
        ellmer::AssistantTurn(
          list(request),
          tokens = c(60, 15, 0),
          duration = 0.4
        ),
        ellmer::UserTurn(list(result)),
        reply(c(60, 15, 0))
      )
    }
    chat$set_turns(turns)
    chat
  }
  # The version the reviewer looked at: its order lookup timed out and it
  # asked the customer to try again.
  reviewed_app <- function(question) {
    transcript(question, "Try again", lookup = "failed")
  }
  baseline_app <- function(question) {
    correct <- grepl("order [CD]", question)
    transcript(question, if (correct) "Service unavailable" else "Try again")
  }
  candidate_app <- function(question) {
    if (grepl("order C", question)) {
      return(transcript(question, "Try again", lookup = "failed"))
    }
    transcript(
      question,
      "Service unavailable",
      lookup = if (grepl("order B", question)) "completed" else "none"
    )
  }

  # vitals runs and scores. Every run uses this one scorer.
  expected_answer <- function(samples) {
    correct <- mapply(
      function(result, target) identical(result, target$answer),
      samples$result,
      samples$target
    )
    list(
      score = factor(
        ifelse(correct, "C", "I"),
        levels = c("I", "C"),
        ordered = TRUE
      )
    )
  }
  run <- function(app, dataset, application) {
    task <- vitals::Task$new(
      dataset = dataset,
      solver = function(inputs) {
        chats <- lapply(inputs, function(input) app(input$question))
        list(
          result = vapply(chats, function(chat) chat$last_turn()@text, ""),
          solver_chat = chats
        )
      },
      scorer = expected_answer,
      dir = tempfile("matched-comparison-")
    )
    suppressMessages({
      task$solve()
      task$score()
    })
    scans::investigation_snapshot(
      scans::as_trajectory_vitals(task, task_id = "order-support"),
      application = application
    )
  }

  # Reviewed regression cases: four trajectories of the reviewed version,
  # one kept for development and three held out. Each review cites the
  # failed order lookup and the reply that followed it.
  questions <- paste(
    "Track order",
    c("A", "B", "C", "D"),
    "after the order service times out"
  )
  saved <- run(
    reviewed_app,
    tibble::tibble(
      id = paste0("order-", c("A", "B", "C", "D")),
      input = lapply(questions, function(q) list(question = q)),
      target = rep(list(list(answer = "Service unavailable")), 4L)
    ),
    "Order support"
  )
  events <- scans::trajectory_events(saved$bundle)
  cases <- lapply(
    scans::trajectory_info(saved$bundle)$trajectory_id,
    function(id) {
      cited <- events$trajectory_id == id &
        (events$status %in% "failed" | events$event_type == "content")
      scans::review_case(saved, id, event_ids = events$event_id[cited]) |>
        scans::review_decision(
          "confirmed",
          "Demo reviewer",
          "The order lookup timed out and the reply asked the customer to try again instead of saying tracking was unavailable.",
          expected_behavior = "State that the order service is unavailable."
        )
    }
  )
  examples <- scans::review_examples(
    cases,
    "application",
    groups = paste0("order-", c("A", "B", "C", "D")),
    splits = c("development", "test", "test", "test"),
    inputs = lapply(questions, function(q) list(question = q)),
    targets = rep(list(list(answer = "Service unavailable")), 4L)
  )
  development <- scans::review_dataset(examples, "vitals", "development")
  held_out <- scans::review_dataset(examples, "vitals", "test")
  dataset <- rbind(development, held_out)

  # The candidate run stopped before order D and also ran a smoke-test
  # sample that is not part of the dataset.
  smoke_test <- dataset[1L, ]
  smoke_test$id <- "smoke-test"
  smoke_test$input <- list(list(question = "Track order Z"))
  order_d <- grepl("order D", vapply(dataset$input, `[[`, "", "question"))

  list(
    examples = examples,
    held_out = held_out,
    baseline = run(baseline_app, dataset, "Order support (baseline)"),
    candidate = run(
      candidate_app,
      rbind(dataset[!order_d, ], smoke_test),
      "Order support (candidate)"
    )
  )
}
