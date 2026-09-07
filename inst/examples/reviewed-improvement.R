# Source this file, then call run_reviewed_improvement(). By default every
# provider response is mocked: this demonstrates the workflow, not model gains.
run_reviewed_improvement <- function(
  output_dir = tempfile("reviewed-improvement-"),
  chat = NULL,
  control = NULL
) {
  for (package in c(
    "scans",
    "dsprrr",
    "vitals",
    "ellmer",
    "httr2",
    "jsonlite",
    "withr"
  )) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Install ", package)
  }
  offline <- is.null(chat)
  limits <- c(
    "max_provider_calls",
    "max_metric_calls",
    "max_input_tokens",
    "max_output_tokens",
    "max_total_tokens",
    "max_cost",
    "max_elapsed_seconds"
  )
  bounded <- is.list(control) &&
    any(vapply(
      limits,
      function(name) {
        value <- control[[name]]
        is.numeric(value) && length(value) == 1L && is.finite(value)
      },
      logical(1)
    ))
  if (!offline && !bounded) {
    stop(
      "Supply at least one finite dsprrr::optimizer_control() execution limit for live optimization."
    )
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  old_cache <- dsprrr::configure_cache(enable = FALSE, enable_disk = FALSE)
  on.exit(
    do.call(
      dsprrr::configure_cache,
      if (is.null(old_cache)) list() else old_cache
    ),
    add = TRUE
  )
  calls <- 0L
  if (offline) {
    chat <- ellmer::chat_openai(model = "offline", credentials = function() {
      "unused"
    })
    httr2::local_mocked_responses(function(req) {
      calls <<- calls + 1L
      if (calls > 30L) {
        stop("Offline fixture request ceiling exceeded.")
      }
      result <- list(
        instructions = "State that the service is unavailable after a timeout.",
        answer = "Service unavailable"
      )
      text <- as.character(jsonlite::toJSON(result, auto_unbox = TRUE))
      if (!is.null(req$body$data$input)) {
        body <- list(
          id = paste0("offline-", calls),
          model = "offline",
          object = "response",
          status = "completed",
          output = list(list(
            type = "message",
            id = paste0("msg-", calls),
            role = "assistant",
            status = "completed",
            content = list(list(
              type = "output_text",
              text = text,
              annotations = list()
            ))
          )),
          usage = list(input_tokens = 1L, output_tokens = 1L, total_tokens = 2L)
        )
      } else {
        body <- list(
          id = paste0("offline-", calls),
          model = "offline",
          object = "chat.completion",
          choices = list(list(
            index = 0L,
            message = list(role = "assistant", content = text),
            finish_reason = "stop"
          )),
          usage = list(
            prompt_tokens = 1L,
            completion_tokens = 1L,
            total_tokens = 2L
          )
        )
      }
      httr2::response(
        headers = list(`content-type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
      )
    })
    control <- dsprrr::optimizer_control(
      max_provider_calls = 12L,
      max_metric_calls = 8L,
      max_elapsed_seconds = 60,
      seed = 1L
    )
  }

  # Public producer objects supply the original executions; no hand-built traces.
  old_app <- dsprrr::module_fn("question -> answer", function(question) {
    list(answer = "Try again")
  })
  questions <- c(
    "Track order A after the service times out",
    "Track order B after the service times out"
  )
  dsprrr::run_dataset(
    old_app,
    data.frame(question = questions),
    .progress = FALSE
  )
  saved <- scans::investigation_snapshot(
    scans::as_trajectory_dsprrr(old_app),
    application = "Order support"
  )
  ids <- scans::trajectory_info(saved$bundle)$trajectory_id
  cases <- lapply(ids, function(id) {
    scans::review_case(saved, id) |>
      scans::review_decision(
        "confirmed",
        "Demo reviewer",
        "The old output asks for another retry.",
        expected_behavior = "State that the order service is unavailable."
      )
  })
  examples <- scans::review_examples(
    cases,
    "application",
    groups = c("order-A", "order-B"),
    splits = c("development", "test"),
    inputs = lapply(questions, function(question) list(question = question)),
    targets = rep(
      list(list(
        answer = "Service unavailable",
        expectation = "State that the order service is unavailable."
      )),
      2L
    )
  )
  path <- file.path(output_dir, "reviewed-examples.json")
  scans::write_review_examples(examples, path)
  examples <- scans::read_review_examples(path)

  # This metric judges the fresh prediction. Old rationale is never a score.
  metric <- dsprrr::metric_with_feedback(
    function(prediction, expected) {
      answer <- if (is.list(prediction)) prediction$answer else prediction
      correct <- identical(answer, expected$answer)
      list(
        score = as.numeric(correct),
        feedback = if (correct) {
          "Meets the expected output."
        } else {
          expected$expectation
        }
      )
    },
    field = "answer"
  )
  program <- dsprrr::module(dsprrr::signature("question -> answer"))
  optimizer <- dsprrr::GEPA(
    metrics = list(quality = metric),
    population_size = 2L,
    generations = 1L,
    mutation_rate = 1,
    seed = 1L,
    verbose = FALSE
  )
  development <- scans::review_dataset(examples, "dsprrr", "development")
  optimized <- dsprrr::compile(
    program,
    optimizer,
    trainset = development,
    .llm = chat,
    control = control
  )

  # Held-out cases are used only after optimization. A manual app change can
  # replace `optimized` here and use the same targets and scorer.
  held_out <- scans::review_dataset(examples, "vitals", "test")
  scorer <- function(samples) {
    results <- lapply(seq_len(nrow(samples)), function(i) {
      metric(samples$result[[i]], samples$target[[i]])
    })
    list(
      score = vapply(results, `[[`, 0, "score"),
      explanation = vapply(results, `[[`, "", "feedback")
    )
  }
  task <- vitals::Task$new(
    dataset = held_out,
    solver = dsprrr::as_vitals_solver(
      optimized,
      .llm = chat,
      .concurrency = dsprrr::concurrency_control(backend = "sequential")
    ),
    scorer = scorer,
    metrics = list(mean = mean),
    name = "reviewed-order-support",
    dir = output_dir
  )
  task$eval(view = FALSE)
  traces <- scans::as_trajectory_vitals(task)
  scans::write_investigation(
    scans::investigation_snapshot(
      traces,
      application = "Order support held-out evaluation"
    ),
    file.path(output_dir, "held-out-investigation.json")
  )
  list(
    examples = examples,
    development = development,
    held_out = held_out,
    metric = metric,
    optimized = optimized,
    task = task,
    traces = traces,
    offline = offline,
    mocked_requests = calls,
    output_dir = output_dir
  )
}
