test_that("ellmer turn lists pass the shared conformance contract", {
  skip_if_not_installed("ellmer", "0.4.2")

  expect_adapter_conforms(
    source = ellmer_tool_turns_fixture(),
    expected = ellmer_tool_bundle_fixture(),
    adapter = as_trajectory_ellmer
  )
})

test_that("as_trajectory() dispatches for ellmer lists and turns", {
  skip_if_not_installed("ellmer", "0.4.2")
  turns <- ellmer_tool_turns_fixture()

  list_bundle <- as_trajectory(turns)
  turn_bundle <- as_trajectory(turns[[1L]])

  expect_s7_class(list_bundle, TrajectoryBundle)
  expect_s7_class(turn_bundle, TrajectoryBundle)
  expect_identical(nrow(trajectory_turns(turn_bundle)), 1L)
})

test_that("as_trajectory() rejects an ambiguous empty list", {
  skip_if_not_installed("ellmer", "0.4.2")

  condition <- rlang::catch_cnd(as_trajectory(list()))

  expect_s3_class(condition, "scans_error_unsupported_source")
  expect_snapshot(error = TRUE, as_trajectory(list()))
})

test_that("ellmer Chat snapshots use public history and model methods", {
  skip_if_not_installed("ellmer", "0.4.2")
  chat <- ellmer_chat_fixture()

  bundle <- as_trajectory(chat)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(
    trajectory_turns(bundle)$role,
    c("system", "user", "assistant")
  )
  expect_identical(trajectory_info(bundle)$model, "gpt-4o-mini")
  expect_identical(nrow(trajectory_events(bundle)), 3L)

  without_system <- as_trajectory(chat, include_system_prompt = FALSE)
  expect_identical(
    trajectory_turns(without_system)$role,
    c("user", "assistant")
  )
})

test_that("serialized ellmer Chat objects remain convertible", {
  skip_if_not_installed("ellmer", "0.4.2")
  chat <- unserialize(serialize(ellmer_chat_fixture(), NULL))

  bundle <- as_trajectory(chat)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(trajectory_info(bundle)$source_type, "ellmer")
  expect_identical(nrow(trajectory_turns(bundle)), 3L)
})

test_that("ellmer adapter preserves caller correlation identities", {
  skip_if_not_installed("ellmer", "0.4.2")

  bundle <- as_trajectory_ellmer(
    ellmer_tool_turns_fixture(),
    trajectory_id = "trajectory-custom",
    run_id = "run-1",
    source_id = "chat-1",
    source_uri = "https://user:password@example.com/chat?api_key=secret",
    task_id = "task-1",
    sample_id = "sample-1",
    epoch = 2L,
    agent = "assistant",
    model = "fixture-model",
    metadata = list(dataset = "fixture")
  )
  info <- trajectory_info(bundle)

  expect_identical(info$trajectory_id, "trajectory-custom")
  expect_identical(info$run_id, "run-1")
  expect_identical(info$source_id, "chat-1")
  expect_identical(info$source_uri, "https://example.com/chat")
  expect_identical(info$task_id, "task-1")
  expect_identical(info$sample_id, "sample-1")
  expect_identical(info$epoch, 2L)
  expect_identical(info$agent, "assistant")
  expect_identical(info$model, "fixture-model")
  expect_in("redacted", trajectory_losses(bundle)$reason)
})

test_that("ellmer tool errors remain agent-visible", {
  skip_if_not_installed("ellmer", "0.4.2")
  request <- ellmer::ContentToolRequest("call-1", "lookup", list(id = 42L))
  turns <- list(
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      error = simpleError("Service unavailable"),
      request = request
    )))
  )

  bundle <- as_trajectory_ellmer(turns)
  events <- trajectory_events(bundle)
  turns <- trajectory_turns(bundle)

  expect_identical(events$call_id, c("call-1", "call-1"))
  expect_identical(events$parent_event_id[[2L]], events$event_id[[1L]])
  expect_identical(events$status, c("completed", "failed"))
  expect_identical(events$error[[2L]], "Service unavailable")
  expect_identical(turns$status, c("completed", "failed"))
  expect_identical(turns$error[[2L]], "Service unavailable")
})

test_that("ellmer metadata and tool payload secrets are redacted", {
  skip_if_not_installed("ellmer", "0.4.2")
  request <- ellmer::ContentToolRequest(
    "call-1",
    "lookup",
    list(api_key = "argument-secret", query = "safe"),
    extra = list(access_token = "metadata-secret", provider = "fixture")
  )
  turn <- ellmer::AssistantTurn(
    list(request),
    json = list(response = "provider-secret")
  )

  bundle <- as_trajectory_ellmer(
    turn,
    metadata = list(password = "trajectory-secret")
  )
  event <- trajectory_events(bundle)
  losses <- trajectory_losses(bundle)
  rendered <- capture.output(str(S7::props(bundle)))

  expect_identical(event$value[[1L]]$api_key, "<redacted>")
  expect_identical(event$metadata[[1L]]$access_token, "<redacted>")
  expect_identical(
    trajectory_info(bundle)$metadata[[1L]]$password,
    "<redacted>"
  )
  expect_in("redacted", losses$reason)
  expect_false("truncated" %in% losses$reason)
  expect_no_match(
    rendered,
    "argument-secret|metadata-secret|provider-secret|trajectory-secret"
  )
})

test_that("ellmer named vectors and attributes cannot retain secrets", {
  skip_if_not_installed("ellmer", "0.4.2")
  headers <- c(
    authorization = "Bearer header-secret",
    accept = "application/json"
  )
  payload <- structure("safe", api_key = "attribute-secret")

  bundle <- as_trajectory_ellmer(
    ellmer::UserTurn(),
    metadata = list(headers = headers, payload = payload)
  )
  metadata <- trajectory_info(bundle)$metadata[[1L]]
  rendered <- capture.output(str(S7::props(bundle)))

  expect_identical(metadata$headers[["authorization"]], "<redacted>")
  expect_identical(metadata$headers[["accept"]], "application/json")
  expect_identical(attr(metadata$payload, "api_key"), "<redacted>")
  expect_no_match(rendered, "header-secret|attribute-secret")
  expect_identical(sum(trajectory_losses(bundle)$reason == "redacted"), 2L)
})

test_that("ellmer sanitization covers duplicate names and deep values", {
  skip_if_not_installed("ellmer", "0.4.2")
  headers <- stats::setNames(
    c("Bearer first-secret", "Bearer second-secret"),
    c("authorization", "authorization")
  )
  deep <- "depth-secret"
  for (index in seq_len(52L)) {
    deep <- list(value = deep)
  }

  bundle <- as_trajectory_ellmer(
    ellmer::UserTurn(),
    metadata = list(headers = headers, deep = deep)
  )
  rendered <- capture.output(str(S7::props(bundle)))

  expect_identical(
    unname(trajectory_info(bundle)$metadata[[1L]]$headers),
    list("<redacted>", "<redacted>")
  )
  expect_no_match(rendered, "first-secret|second-secret|depth-secret")
  expect_setequal(
    unique(trajectory_losses(bundle)$reason),
    c("redacted", "unsupported")
  )
})

test_that("ellmer binary and unknown content produces explicit losses", {
  skip_if_not_installed("ellmer", "0.4.2")
  FixtureContent <- S7::new_class(
    "FixtureContent",
    parent = ellmer::Content,
    properties = list(
      value = S7::class_character,
      metadata = S7::class_list
    )
  )
  turn <- ellmer::UserTurn(list(
    ellmer::ContentImageRemote(
      "https://user:password@example.com/image.png?token=secret",
      "low"
    ),
    ellmer::ContentImageInline("image/png", "ZmFrZQ=="),
    ellmer::ContentPDF("application/pdf", "cGRm", "fixture.pdf"),
    FixtureContent(
      value = "unknown",
      metadata = list(api_key = "unknown-secret", provider = "fixture")
    )
  ))

  bundle <- as_trajectory_ellmer(turn)
  events <- trajectory_events(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(events$value[[1L]]$url, "https://example.com/image.png")
  expect_identical(events$content_type[[2L]], "image")
  expect_identical(events$content_type[[3L]], "pdf")
  expect_match(events$content_type[[4L]], "^ellmer:")
  expect_identical(events$value[[4L]]$value, "unknown")
  expect_identical(events$value[[4L]]$metadata$api_key, "<redacted>")
  expect_identical(events$value[[4L]]$metadata$provider, "fixture")
  expect_no_match(capture.output(str(S7::props(bundle))), "unknown-secret")
  expect_setequal(
    unique(losses$reason),
    c("redacted", "externalized")
  )
})

test_that("ellmer duplicate call identifiers do not invent a parent", {
  skip_if_not_installed("ellmer", "0.4.2")
  first <- ellmer::ContentToolRequest("call-1", "lookup", list(id = 1L))
  second <- ellmer::ContentToolRequest("call-1", "lookup", list(id = 2L))
  result <- ellmer::ContentToolResult("done", request = first)
  turns <- list(
    ellmer::AssistantTurn(list(first, second)),
    ellmer::UserTurn(list(result))
  )

  bundle <- as_trajectory_ellmer(turns)
  events <- trajectory_events(bundle)
  findings <- scan_trajectories(bundle)

  expect_identical(events$parent_event_id, rep(NA_character_, 3L))
  expect_setequal(
    findings$scan,
    c("ambiguous_tool_correlation", "unresolved_tool_call")
  )
})

test_that("ellmer partial turns mark the trajectory interrupted", {
  skip_if_not_installed("ellmer", "0.4.2")
  turn <- ellmer::AssistantPartialTurn(
    list(ellmer::ContentText("Partial")),
    reason = "cancelled"
  )

  bundle <- as_trajectory_ellmer(turn)

  expect_identical(trajectory_info(bundle)$status, "interrupted")
  expect_identical(trajectory_turns(bundle)$status, "cancelled")
  expect_identical(
    trajectory_turns(bundle)$metadata[[1L]]$interruption_reason,
    "cancelled"
  )
})

test_that("ellmer empty histories remain valid snapshots", {
  skip_if_not_installed("ellmer", "0.4.2")

  bundle <- as_trajectory_ellmer(list())

  expect_identical(nrow(trajectory_info(bundle)), 1L)
  expect_identical(nrow(trajectory_turns(bundle)), 0L)
  expect_identical(nrow(trajectory_events(bundle)), 0L)
  expect_identical(trajectory_info(bundle)$status, "completed")
})

test_that("ellmer thinking and oversized text are retained safely", {
  skip_if_not_installed("ellmer", "0.4.2")
  turn <- ellmer::AssistantTurn(list(
    ellmer::ContentThinking(
      "Reasoning",
      extra = list(signature = "fixture")
    ),
    ellmer::ContentText(strrep("x", 65537L))
  ))

  bundle <- as_trajectory_ellmer(turn)
  events <- trajectory_events(bundle)

  expect_identical(events$content_type, c("thinking", "text"))
  expect_identical(events$text[[1L]], "Reasoning")
  expect_identical(events$metadata[[1L]]$signature, "fixture")
  expect_lt(nchar(events$text[[2L]], type = "bytes"), 65536L)
  expect_in("truncated", trajectory_losses(bundle)$reason)
})

test_that("ellmer unmatched tool results remain diagnostic data", {
  skip_if_not_installed("ellmer", "0.4.2")
  turn <- ellmer::UserTurn(list(ellmer::ContentToolResult("orphaned")))

  bundle <- as_trajectory_ellmer(turn)
  event <- trajectory_events(bundle)

  expect_identical(event$event_type, "tool_result")
  expect_identical(event$call_id, NA_character_)
  expect_identical(event$parent_event_id, NA_character_)
  expect_identical(event$text, "orphaned")
})

test_that("ellmer adapter rejects malformed sources and arguments", {
  skip_if_not_installed("ellmer", "0.4.2")

  condition <- rlang::catch_cnd(as_trajectory(list(ellmer::UserTurn(), 1)))
  expect_s3_class(condition, "scans_error_ellmer_source")

  expect_snapshot(
    error = TRUE,
    as_trajectory(list(ellmer::UserTurn(), 1))
  )
  expect_snapshot(
    error = TRUE,
    as_trajectory_ellmer(ellmer::UserTurn(), epoch = 0L)
  )
  expect_snapshot(
    error = TRUE,
    as_trajectory_ellmer(ellmer::UserTurn(), metadata = list("unnamed"))
  )
})

test_that("sensitive names are matched regardless of case and prefix", {
  sensitive <- c(
    "api_key",
    "API_KEY",
    "Api-Key",
    "X-Api-Key",
    "ANTHROPIC_API_KEY",
    "Authorization",
    "PASSWORD",
    "db_password",
    "token",
    "GITHUB_TOKEN",
    "session_token",
    "private_key",
    "client_secret",
    "SECRET",
    "Cookie",
    "auth",
    "bearer"
  )
  for (name in sensitive) {
    expect_true(trajectory_sensitive_name(name), info = name)
  }
  benign <- c(
    "city",
    "keyboard",
    "Monkey",
    "tokenizer",
    "keys",
    "primary_key",
    "author",
    "id",
    "model"
  )
  for (name in benign) {
    expect_false(trajectory_sensitive_name(name), info = name)
  }
  expect_false(trajectory_sensitive_name(NA_character_))
})

test_that("upper-case credential fields in tool arguments are redacted", {
  request <- ellmer::ContentToolRequest(
    "call-1",
    "fetch",
    list(
      ANTHROPIC_API_KEY = "sk-ant-secret",
      headers = list(Authorization = "Bearer abc"),
      url = "https://example.com"
    )
  )
  bundle <- as_trajectory(list(ellmer::AssistantTurn(list(request))))
  value <- trajectory_events(bundle)$value[[1L]]
  expect_identical(value$ANTHROPIC_API_KEY, "<redacted>")
  expect_identical(value$headers$Authorization, "<redacted>")
  expect_identical(value$url, "https://example.com")
})

test_that("unknown content locations are scrubbed like remote images", {
  skip_if_not(exists("ContentUploaded", asNamespace("ellmer")))
  uploaded <- get("ContentUploaded", asNamespace("ellmer"))(
    uri = "https://user:pw@files.example.com/f/1?sig=SECRET",
    mime_type = "text/plain"
  )
  bundle <- as_trajectory(list(ellmer::UserTurn(list(uploaded))))
  event <- trajectory_events(bundle)
  expect_identical(event$event_type, "custom")
  expect_identical(event$value[[1L]]$uri, "https://files.example.com/f/1")
  expect_no_match(as.character(event$value[[1L]]), "SECRET", fixed = TRUE)
  expect_in("redacted", trajectory_losses(bundle)$reason)
})
