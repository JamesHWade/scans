test_that("commons public reader output passes the shared adapter contract", {
  skip_if_not_installed("ellmer", "0.4.2")

  expect_adapter_conforms(
    source = commons_trajectory_fixture(),
    adapter = as_trajectory_commons
  )
})

test_that("already-read commons conversations become canonical trajectories", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  bundle <- as_trajectory_commons(source)
  info <- trajectory_info(bundle)
  turns <- trajectory_turns(bundle)
  events <- trajectory_events(bundle)
  provenance <- events[events$event_type == "commons:provenance", ]

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(info$trajectory_id, "commons/conversation-001")
  expect_identical(info$source_type, "commons")
  expect_identical(info$run_id, "conversation-001")
  expect_identical(info$source_id, "conversation-001")
  expect_identical(
    info$completed_at,
    as.POSIXct("2026-08-25 12:00:01", tz = "UTC")
  )
  expect_identical(info$metadata[[1L]]$source, attr(source, "source"))
  expect_identical(
    turns$role,
    c("user", "assistant")
  )
  expect_identical(turns$round_index, c(1L, 1L))
  expect_identical(nrow(provenance), 1L)
  expect_true(is.na(provenance$turn_id))
  expect_identical(
    vapply(provenance$value, `[[`, character(1), "provenance_tag"),
    "B"
  )
  expect_identical(
    provenance$value[[1L]]$citation_decisions[[1L]]$label,
    "Data dictionary"
  )
})

test_that("commons exchange rounds keep tool results with their request", {
  skip_if_not_installed("ellmer", "0.4.2")

  request <- ellmer::ContentToolRequest(
    id = "call-001",
    name = "lookup",
    arguments = list(query = "orders")
  )
  conversation <- list(
    ellmer::UserTurn(list(ellmer::ContentText("Count orders"))),
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(ellmer::ContentToolResult("Six", request = request))),
    ellmer::AssistantTurn(list(ellmer::ContentText("There are six.")))
  )
  attr(conversation, "last_active") <- as.POSIXct(
    "2026-08-23 12:00:00",
    tz = "UTC"
  )
  attr(conversation, "provenance") <- list(list(
    provenance_tag = "A",
    citation_decisions = list()
  ))
  source <- list(`conversation-001` = conversation)
  attr(source, "source") <- list(kind = "local", path = "/traces")

  bundle <- as_trajectory_commons(source)
  turns <- trajectory_turns(bundle)
  provenance <- trajectory_events(bundle)
  provenance <- provenance[provenance$event_type == "commons:provenance", ]

  expect_identical(turns$round_index, rep(1L, 4L))
  expect_true(is.na(provenance$turn_id))
})

test_that("multiple commons conversations retain stable identities", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  source[["conversation-002"]] <- source[[1L]]

  bundle <- as_trajectory_commons(source)
  info <- trajectory_info(bundle)

  expect_identical(
    info$trajectory_id,
    c("commons/conversation-001", "commons/conversation-002")
  )
  expect_identical(
    info$run_id,
    c("conversation-001", "conversation-002")
  )
  expect_identical(
    unique(trajectory_turns(bundle)$trajectory_id),
    info$trajectory_id
  )
})

test_that("commons provenance gaps and conflicts are explicit losses", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  conversation <- source[[1L]]
  attr(conversation, "last_active") <- NULL
  attr(conversation, "provenance") <- list(
    list(
      provenance_tag = c("A", "C"),
      citation_decisions = list()
    ),
    list(provenance_tag = NA_character_),
    list(
      provenance_tag = "B",
      citation_decisions = list()
    )
  )
  source[[1L]] <- conversation
  attr(source, "source") <- list(
    kind = "local",
    path = "/traces",
    server = "https://connect.example"
  )

  bundle <- as_trajectory_commons(source)
  info <- trajectory_info(bundle)
  events <- trajectory_events(bundle)
  provenance <- events[events$event_type == "commons:provenance", ]
  losses <- trajectory_losses(bundle)

  expect_identical(info$completed_at, as.POSIXct(NA, tz = "UTC"))
  expect_identical(
    info$metadata[[1L]]$source$server,
    "https://connect.example"
  )
  expect_identical(provenance$value[[1L]]$provenance_tag, c("A", "C"))
  expect_true(all(is.na(provenance$turn_id)))
  expect_setequal(
    losses$field,
    c(
      "source",
      "last_active",
      "provenance[[1]]$provenance_tag",
      "provenance[[2]]$provenance_tag",
      "provenance[[2]]$citation_decisions"
    )
  )
  expect_setequal(unique(losses$reason), "unsupported")
})

test_that("missing commons provenance is not inferred", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  attr(source[[1L]], "provenance") <- NULL
  attr(source, "source") <- NULL
  bundle <- as_trajectory_commons(source)

  expect_identical(
    sum(trajectory_events(bundle)$event_type == "commons:provenance"),
    0L
  )
  expect_setequal(
    trajectory_losses(bundle)$field,
    c("source", "provenance")
  )
})

test_that("commons preserves malformed provenance without trusting it", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  attr(source[[1L]], "provenance") <- "conflicting audit records"
  bundle <- as_trajectory_commons(source)

  expect_identical(
    trajectory_info(bundle)$metadata[[1L]]$provenance,
    "conflicting audit records"
  )
  expect_identical(
    sum(trajectory_events(bundle)$event_type == "commons:provenance"),
    0L
  )
  expect_identical(trajectory_losses(bundle)$field, "provenance")
})

test_that("commons does not infer missing exchange records", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  attr(source[[1L]], "provenance") <- list()
  bundle <- as_trajectory_commons(source)

  events <- trajectory_events(bundle)
  expect_identical(sum(events$event_type == "commons:provenance"), 0L)
  expect_identical(nrow(trajectory_losses(bundle)), 0L)
})

test_that("commons source kinds and extra attributes remain inspectable", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  attr(source, "source") <- list(
    kind = "connect",
    server = "https://user:secret@connect.example?token=secret",
    content_guid = "content-001"
  )
  attr(source, "collector") <- list(name = "fixture")
  attr(source[[1L]], "review_batch") <- "batch-001"

  bundle <- as_trajectory_commons(
    source,
    metadata = list(api_key = "secret")
  )
  info <- trajectory_info(bundle)
  losses <- trajectory_losses(bundle)

  expect_identical(
    info$metadata[[1L]]$source,
    list(
      kind = "connect",
      server = "https://connect.example",
      content_guid = "content-001"
    )
  )
  expect_identical(
    info$metadata[[1L]]$source_attributes$collector$name,
    "fixture"
  )
  expect_identical(
    info$metadata[[1L]]$conversation_attributes$review_batch,
    "batch-001"
  )
  expect_identical(info$metadata[[1L]]$api_key, "<redacted>")
  expect_setequal(losses$field, c("metadata$api_key", "source$server"))
  expect_setequal(unique(losses$reason), "redacted")
})

test_that("commons citation decisions are sanitized and diagnosed", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  provenance <- attr(source[[1L]], "provenance")
  provenance[[1L]]$citation_decisions <- list(list(
    quote = "Six orders",
    api_key = "secret"
  ))
  provenance[[2L]] <- list(
    provenance_tag = "A",
    citation_decisions = list("malformed")
  )
  attr(source[[1L]], "provenance") <- provenance

  bundle <- as_trajectory_commons(source)
  events <- trajectory_events(bundle)
  events <- events[events$event_type == "commons:provenance", ]
  losses <- trajectory_losses(bundle)

  expect_identical(
    events$value[[1L]]$citation_decisions[[1L]]$api_key,
    "<redacted>"
  )
  expect_in("redacted", losses$reason)
  malformed <- losses[
    losses$field == "provenance[[2]]$citation_decisions" &
      losses$reason == "unsupported",
    ,
    drop = FALSE
  ]
  expect_identical(nrow(malformed), 1L)
})

test_that("commons inputs use an explicit classless boundary", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  condition <- rlang::catch_cnd(as_trajectory(source))
  invalid <- unname(source)

  expect_s3_class(condition, "scans_error_unsupported_source")
  expect_snapshot(error = TRUE, as_trajectory_commons(invalid))
  expect_snapshot(
    error = TRUE,
    as_trajectory_commons(source, metadata = list("unnamed"))
  )
})

test_that("commons argument and source failures have stable conditions", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  multiple <- c(source, source)
  names(multiple) <- c("conversation-001", "conversation-002")
  invalid_turn <- source
  invalid_turn[[1L]][[1L]] <- "not an ellmer turn"

  expect_s3_class(
    rlang::catch_cnd(as_trajectory_commons(source, trajectory_id = "")),
    "scans_error_commons_argument"
  )
  expect_s3_class(
    rlang::catch_cnd(as_trajectory_commons(
      source,
      metadata = list(source = "caller override")
    )),
    "scans_error_commons_argument"
  )
  expect_s3_class(
    rlang::catch_cnd(as_trajectory_commons(
      multiple,
      trajectory_id = "custom"
    )),
    "scans_error_commons_argument"
  )
  expect_s3_class(
    rlang::catch_cnd(as_trajectory_commons(invalid_turn)),
    "scans_error_commons_source"
  )
})

test_that("commons conversion is deterministic and contains no live source", {
  skip_if_not_installed("ellmer", "0.4.2")

  source <- commons_trajectory_fixture()
  first <- as_trajectory_commons(source, trajectory_id = "custom")
  second <- as_trajectory_commons(source, trajectory_id = "custom")
  restored <- unserialize(serialize(first, NULL))

  expect_identical(S7::props(second), S7::props(first))
  expect_identical(S7::props(restored), S7::props(first))
  expect_identical(trajectory_info(first)$trajectory_id, "custom")
  expect_no_error(S7::validate(first))
})

test_that("empty commons reads convert without optional dependencies", {
  source <- structure(list(), source = list(kind = "local", path = "/traces"))
  bundle <- as_trajectory_commons(source)

  expect_s7_class(bundle, TrajectoryBundle)
  expect_identical(nrow(trajectory_info(bundle)), 0L)
})
