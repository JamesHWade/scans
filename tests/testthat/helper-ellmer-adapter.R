ellmer_tool_turns_fixture <- function() {
  weather <- ellmer::ContentToolRequest(
    "call-weather",
    "weather",
    list(city = "Detroit"),
    extra = list(provider = "fixture")
  )
  clock <- ellmer::ContentToolRequest(
    "call-clock",
    "clock",
    list(zone = "America/Detroit"),
    extra = list(provider = "fixture")
  )

  list(
    ellmer::UserTurn(list(ellmer::ContentText("Plan my walk"))),
    ellmer::AssistantTurn(
      list(weather, clock),
      tokens = c(10, 3, 2),
      cost = 0.01,
      duration = 1.5,
      finish_reason = "tool_use"
    ),
    ellmer::UserTurn(list(
      ellmer::ContentToolResult("Sunny", request = weather),
      ellmer::ContentToolResult("14:00", request = clock)
    )),
    ellmer::AssistantTurn(list(ellmer::ContentText("Leave at two")))
  )
}

ellmer_tool_bundle_fixture <- function() {
  trajectory_id <- "trajectory-000001"
  turn_ids <- sprintf("%s/turn-%06d", trajectory_id, 1:4)
  event_ids <- sprintf("%s/event-%06d", trajectory_id, 1:6)

  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = trajectory_id,
      source_type = "ellmer",
      status = "completed",
      metadata = list(list(
        ellmer_version = as.character(utils::packageVersion("ellmer")),
        source_class = "list"
      ))
    ),
    turns = tibble::tibble(
      trajectory_id = trajectory_id,
      turn_id = turn_ids,
      turn_index = 1:4,
      role = c("user", "assistant", "user", "assistant"),
      input_tokens = c(NA, 10, NA, NA),
      output_tokens = c(NA, 3, NA, NA),
      cached_input_tokens = c(NA, 2, NA, NA),
      cost = c(NA, 0.01, NA, NA),
      duration = c(NA, 1.5, NA, NA),
      finish_reason = c(NA, "tool_use", NA, NA),
      status = rep("completed", 4L),
      metadata = rep(list(list()), 4L)
    ),
    events = tibble::tibble(
      trajectory_id = trajectory_id,
      event_id = event_ids,
      event_index = 1:6,
      turn_id = turn_ids[c(1L, 2L, 2L, 3L, 3L, 4L)],
      content_index = c(1L, 1L, 2L, 1L, 2L, 1L),
      parent_event_id = c(NA, NA, NA, event_ids[[2L]], event_ids[[3L]], NA),
      event_type = c(
        "content",
        "tool_call",
        "tool_call",
        "tool_result",
        "tool_result",
        "content"
      ),
      content_type = c("text", NA, NA, NA, NA, "text"),
      name = c(NA, "weather", "clock", "weather", "clock", NA),
      call_id = c(
        NA,
        "call-weather",
        "call-clock",
        "call-weather",
        "call-clock",
        NA
      ),
      text = c("Plan my walk", NA, NA, "Sunny", "14:00", "Leave at two"),
      value = list(
        NULL,
        list(city = "Detroit"),
        list(zone = "America/Detroit"),
        "Sunny",
        "14:00",
        NULL
      ),
      status = rep("completed", 6L),
      metadata = list(
        list(),
        list(provider = "fixture"),
        list(provider = "fixture"),
        list(),
        list(),
        list()
      )
    )
  )
}

ellmer_chat_fixture <- function(system_prompt = "Be concise") {
  credentials <- function() "fixture-key"
  environment(credentials) <- baseenv()
  chat <- ellmer::chat_openai(
    model = "gpt-4o-mini",
    system_prompt = system_prompt,
    credentials = credentials
  )
  chat$set_turns(list(
    ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
    ellmer::AssistantTurn(
      list(ellmer::ContentText("Hi")),
      tokens = c(3, 2, 1),
      cost = 0.001,
      duration = 0.2,
      finish_reason = "success"
    )
  ))
  chat
}
