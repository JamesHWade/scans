vitals_samples_fixture <- function(scored = TRUE) {
  first_chat <- ellmer_chat_fixture()
  second_chat <- ellmer_chat_fixture(system_prompt = "Answer directly")

  out <- tibble::tibble(
    id = c("sample-a", "sample-b"),
    epoch = c(1L, 2L),
    input = c("Say hello", "Say hello again"),
    target = c("Hi", "Hi"),
    result = c("Hi", "Hello"),
    solver_chat = list(first_chat, second_chat),
    case = c("baseline", "repeat"),
    api_key = c("first-secret", "second-secret")
  )

  if (scored) {
    out$score <- factor(c("C", "I"), levels = c("I", "C"), ordered = TRUE)
    out$scorer <- c("detect_exact", "detect_exact")
    out$scorer_explanation <- c("Exact match", "Different wording")
  }

  out
}

vitals_task_fixture <- function(task_class = vitals::Task) {
  solver <- function(inputs) {
    list(
      result = rep("Hi", length(inputs)),
      solver_chat = lapply(inputs, \(input) ellmer_chat_fixture(input))
    )
  }
  scorer <- function(samples) {
    list(
      score = rep("C", nrow(samples)),
      explanation = rep("Fixture score", nrow(samples))
    )
  }

  task <- task_class$new(
    dataset = tibble::tibble(input = "Say hello", target = "Hi"),
    solver = solver,
    scorer = scorer,
    dir = tempdir()
  )
  task$solve()
  task$score()
  task
}
