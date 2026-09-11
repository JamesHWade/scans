test_that("paired disclosures retain both recorded event identities and content", {
  data <- scans_app_data(scans_support_bundle())
  index <- match("otel/retry", data$info$trajectory_id)
  rows <- which(data$events$trajectory_id == "otel/retry")
  pairs <- scans_app_tool_pairs(data$events, rows)
  expect_identical(unname(pairs), c(3L, 5L, 7L))
  html <- as.character(scans_app_transcript_ui(data, index))
  for (row in rows) {
    expect_match(
      html,
      paste0('id="', scans_app_event_dom_id(data$events$event_id[[row]]), '"'),
      fixed = TRUE
    )
  }
  expect_match(
    html,
    "The order service timed out. No shipment data was returned.",
    fixed = TRUE
  )
  expect_match(html, "Recorded result", fixed = TRUE)
  expect_match(html, "Turn 3", fixed = TRUE)
  expect_match(html, "order_service_timeout", fixed = TRUE)
})

test_that("ambiguous and interleaved tool records are not paired", {
  events <- trajectory_events(scans_support_bundle())
  rows <- which(events$trajectory_id == "otel/retry")
  events$call_id[[4L]] <- events$call_id[[2L]]
  expect_no_match(names(scans_app_tool_pairs(events, rows)), "^2$")
  events <- trajectory_events(scans_support_bundle())
  events$name[[3L]] <- "a_different_tool"
  expect_no_match(names(scans_app_tool_pairs(events, rows)), "^2$")
  events <- trajectory_events(scans_support_bundle())
  expect_no_match(
    names(scans_app_tool_pairs(events, c(1L, 2L, 4L, 3L, 5L, 6L, 7L, 8L))),
    "^2$"
  )
})

test_that("event deep links retain canonical application and event identities", {
  hash <- paste0(
    scans_app_hash("Support app", "otel/retry"),
    "?event=",
    utils::URLencode("otel/retry/event-000002", reserved = TRUE)
  )
  expect_identical(
    scans_app_parse_hash(hash, "Support app"),
    list(
      application = "Support app",
      trajectory_id = "otel/retry",
      event_id = "otel/retry/event-000002"
    )
  )
  data <- scans_app_data(scans_support_bundle())
  event <- "otel/retry/event-000002"
  before <- as.character(scans_app_event_links(event, data$events))
  after <- as.character(scans_app_event_links(
    event,
    data$events[rev(seq_len(nrow(data$events))), ]
  ))
  expect_identical(before, after)
})

test_that("deep links preserve reserved characters and literal percent escapes", {
  applications <- c(
    "Support/assistant",
    "Support?event=help",
    "Support/%2F?event=#"
  )
  ids <- c("a/b c", "id?event=part#", "literal%2F/id?event=#")
  event <- "event%2F/part?event=#"

  for (application in applications) {
    for (id in ids) {
      hash <- paste0(
        "#",
        scans_app_hash(application, id),
        "?event=",
        utils::URLencode(event, reserved = TRUE, repeated = TRUE)
      )
      expect_identical(
        scans_app_parse_hash(hash, applications),
        list(application = application, trajectory_id = id, event_id = event)
      )
    }
  }
  expect_false(identical(
    scans_app_event_dom_id("event/id"),
    scans_app_event_dom_id("event%2Fid")
  ))
})
