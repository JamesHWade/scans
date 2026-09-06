test_that("public ellmer exchange distinguishes clean assessment and inapplicability", {
  skip_if_not_installed("ellmer")
  bundle <- as_trajectory_ellmer(ellmer_tool_turns_fixture())
  result <- assess_trajectory_scans(bundle)
  expect_identical(result$findings, scan_trajectories(bundle))
  expect_equal(
    result$assessments$status,
    c(
      rep("assessed_no_findings", 6),
      "not_applicable",
      rep("assessed_no_findings", 2)
    )
  )
  expect_identical(result$assessments$scan, scan_registry()$scan)
  expect_equal(result$assessments$scan_version, rep("1", 9))
  expect_equal(result$assessments$settings[[4]], list(repeat_threshold = 2L))
  expect_equal(result$assessments$settings[[5]], list(loop_threshold = 3L))
  expect_all_true(nzchar(result$assessments$required_evidence))
  expect_all_true(nzchar(result$assessments$reason))

  text <- as_trajectory_ellmer(list(
    ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
  ))
  expect_equal(
    assess_trajectory_scans(text)$assessments$status[1:5],
    rep("not_applicable", 5)
  )
})

test_that("offline public capture keeps repeat and failure evidence identities", {
  path <- system.file(
    "extdata",
    "support-investigation.json",
    package = "scans"
  )
  bundle <- as_trajectory_otel(jsonlite::read_json(path))
  result <- assess_trajectory_scans(bundle)
  retry <- result$assessments[
    result$assessments$trajectory_id == "otel/retry",
  ]
  expect_equal(
    retry$status[
      retry$scan %in%
        c("repeated_tool_call", "suspicious_tool_loop", "event_error")
    ],
    rep("assessed_with_findings", 3)
  )
  findings <- result$findings[result$findings$trajectory_id == "otel/retry", ]
  raw <- scan_trajectories(bundle)
  expect_identical(findings, raw[match(findings$finding_id, raw$finding_id), ])
  expect_setequal(
    findings$event_id[findings$scan == "event_error"],
    paste0("otel/retry/event-00000", c(2, 4, 6))
  )
  expect_setequal(unlist(retry$finding_ids), findings$finding_id)
})

test_that("contentless spans and product reviews do not imply semantic coverage", {
  bare <- as_trajectory_otel(list(otel_chat_span()))
  assessment <- assess_trajectory_scans(bare)$assessments
  expect_equal(assessment$status[1:8], rep("insufficient_evidence", 8))
  expect_identical(assessment$status[[9]], "assessed_no_findings")
  expect_identical(nrow(trajectory_losses(bare)), 0L)

  skip_if_not_installed("tempest")
  product <- as_trajectory_tempest(tempest_review_fixture())
  expect_gt(nrow(trajectory_events(product)), 0L)
  assessment <- assess_trajectory_scans(product)$assessments
  expect_equal(assessment$status[1:5], rep("insufficient_evidence", 5))
  expect_identical(assessment$status[[8]], "insufficient_evidence")
  expect_identical(assessment$status[[9]], "assessed_no_findings")
  expect_setequal(
    assessment$loss_rows[[1]],
    seq_len(nrow(trajectory_losses(product)))
  )
  expect_match(paste(assessment$limitations[[1]], collapse = " "), "turns")
})

test_that("detector failures are local and do not expose condition payloads", {
  bundle <- scan_error_chain_fixture()
  expected <- scan_trajectories(
    bundle,
    scans = c("event_error", "error_chain", "turn_error", "trajectory_error")
  )
  local_mocked_bindings(scan_tool_findings = function(...) {
    stop("SECRET payload")
  })
  result <- assess_trajectory_scans(bundle)
  expect_identical(result$findings, expected)
  expect_equal(result$assessments$status[1:5], rep("execution_failure", 5))
  expect_equal(result$assessments$status[6:9], rep("assessed_with_findings", 4))
  expect_no_match(paste(capture.output(str(result)), collapse = " "), "SECRET")
  expect_error(scan_trajectories(bundle), class = "simpleError")
})

test_that("missing identities and arguments cannot produce assessed tool findings", {
  bundle <- scan_loop_fixture()
  events <- trajectory_events(bundle)
  events$value[events$event_type == "tool_call"] <- list(NULL)
  events$call_id[[1]] <- NA_character_
  bundle@events <- events
  expect_gt(nrow(scan_trajectories(bundle)), 0L)
  result <- assess_trajectory_scans(bundle)
  expect_equal(result$assessments$status[1:5], rep("insufficient_evidence", 5))
  expect_identical(nrow(result$findings), 0L)
  expect_all_true(lengths(result$assessments$finding_ids) == 0L)

  events$call_id[[1]] <- "loop-call-1"
  events$value[events$event_type == "tool_call"] <- list(list())
  bundle@events <- events
  expect_equal(
    assess_trajectory_scans(bundle)$assessments$status[4:5],
    rep("assessed_with_findings", 2)
  )
})

test_that("losses follow event ownership and only block relevant evidence", {
  bundle <- scan_error_chain_fixture()
  bundle@losses <- tibble::tibble(
    trajectory_id = NA_character_,
    turn_id = NA_character_,
    event_id = "chain-event-1",
    field = "value.api_key",
    reason = "redacted",
    detail = "Credential removed",
    metadata = list(list())
  )
  result <- assess_trajectory_scans(bundle)
  expect_identical(result$assessments$status[[6]], "assessed_with_findings")
  expect_identical(result$assessments$loss_rows[[6]], 1L)
  expect_match(
    paste(result$assessments$limitations[[6]], collapse = " "),
    "Credential removed"
  )

  loop <- scan_loop_fixture()
  loop@losses <- tibble::tibble(
    trajectory_id = NA_character_,
    turn_id = NA_character_,
    event_id = "loop-event-1",
    field = "value.query",
    reason = "truncated",
    detail = "Arguments omitted",
    metadata = list(list())
  )
  expect_equal(
    assess_trajectory_scans(loop)$assessments$status[1:5],
    c(rep("assessed_no_findings", 3), rep("insufficient_evidence", 2))
  )
})

test_that("selection and thresholds retain finding identities and typed empties", {
  bundle <- scan_loop_fixture()
  all <- assess_trajectory_scans(bundle)
  selected <- assess_trajectory_scans(
    bundle,
    scans = c("repeated_tool_call", "repeated_tool_call")
  )
  expect_identical(
    selected$findings,
    all$findings[all$findings$scan == "repeated_tool_call", ]
  )
  expect_identical(nrow(selected$assessments), 1L)
  raised <- assess_trajectory_scans(
    bundle,
    scans = "repeated_tool_call",
    repeat_threshold = 4L
  )
  expect_identical(raised$assessments$status, "assessed_no_findings")
  expect_identical(
    raised$assessments$settings[[1]],
    list(repeat_threshold = 4L)
  )
  empty <- assess_trajectory_scans(bundle, scans = character())
  expect_identical(nrow(empty$findings), 0L)
  expect_identical(nrow(empty$assessments), 0L)
  expect_identical(
    empty$assessments,
    assess_trajectory_scans(TrajectoryBundle(
      data.frame(),
      data.frame(),
      data.frame()
    ))$assessments
  )
  expect_error(
    assess_trajectory_scans(bundle, scans = "unknown"),
    class = "scans_error_scan_selection"
  )
  expect_error(
    assess_trajectory_scans(bundle, repeat_threshold = 0L),
    class = "scans_error_scan_argument"
  )
})

test_that("partial status and absent causal evidence remain qualified", {
  bundle <- scan_error_chain_fixture()
  events <- trajectory_events(bundle)
  events$parent_event_id <- NA_character_
  bundle@events <- events
  result <- assess_trajectory_scans(bundle)
  expect_identical(result$assessments$status[[7]], "insufficient_evidence")
  events$status[[1]] <- NA_character_
  events$error[[1]] <- NA_character_
  bundle@events <- events
  result <- assess_trajectory_scans(bundle)
  expect_identical(result$assessments$status[[6]], "assessed_with_findings")
  expect_match(
    paste(result$assessments$limitations[[6]], collapse = " "),
    "missing status"
  )
  events$status[[2]] <- "completed"
  events$error[[2]] <- NA_character_
  events$event_type[[2]] <- "custom"
  bundle@events <- events
  expect_identical(
    assess_trajectory_scans(bundle)$assessments$status[[6]],
    "insufficient_evidence"
  )
})

test_that("unknown status does not count as recorded diagnostic evidence", {
  bundle <- TrajectoryBundle(
    data.frame(
      trajectory_id = "unknown",
      source_type = "manual",
      status = "unknown"
    ),
    data.frame(),
    data.frame()
  )
  expect_identical(
    assess_trajectory_scans(
      bundle,
      scans = "trajectory_error"
    )$assessments$status,
    "insufficient_evidence"
  )
})

test_that("result payload loss does not invalidate recorded tool arguments", {
  bundle <- scan_loop_fixture()
  bundle@losses <- tibble::tibble(
    trajectory_id = NA_character_,
    turn_id = NA_character_,
    event_id = "loop-event-2",
    field = "value",
    reason = "truncated",
    detail = "Result text truncated",
    metadata = list(list())
  )
  result <- assess_trajectory_scans(bundle)
  expect_equal(result$assessments$status[4:5], rep("assessed_with_findings", 2))
  expect_identical(result$assessments$loss_rows[[4]], 1L)
})

test_that("an execution failure does not affect other trajectories", {
  first <- scan_loop_fixture()
  second <- scan_error_chain_fixture()
  bundle <- TrajectoryBundle(
    rbind(trajectory_info(first), trajectory_info(second)),
    rbind(trajectory_turns(first), trajectory_turns(second)),
    rbind(trajectory_events(first), trajectory_events(second))
  )
  detector <- scan_tool_findings
  local_mocked_bindings(scan_tool_findings = function(events, ...) {
    if (any(events$trajectory_id == "trajectory-loop")) {
      stop("Injected failure")
    }
    detector(events, ...)
  })
  result <- assess_trajectory_scans(bundle)
  expect_equal(sum(result$assessments$status == "execution_failure"), 5L)
  expect_all_true(
    result$assessments$status[
      result$assessments$trajectory_id == "trajectory-chain"
    ] !=
      "execution_failure"
  )
  expect_setequal(result$findings$trajectory_id, "trajectory-chain")
})

test_that("text mixed with opaque events does not establish tool inapplicability", {
  skip_if_not_installed("ellmer")
  bundle <- as_trajectory_ellmer(list(
    ellmer::UserTurn(list(ellmer::ContentText("Question"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("Opaque output")))
  ))
  events <- trajectory_events(bundle)
  events$event_type[[2L]] <- "custom"
  events$text[[2L]] <- NA_character_
  bundle@events <- events
  result <- assess_trajectory_scans(bundle)
  expect_equal(result$assessments$status[1:5], rep("insufficient_evidence", 5))
})

test_that("public source identity losses prevent false correlation findings", {
  skip_if_not_installed("ellmer")
  request <- ellmer::ContentToolRequest(
    paste(rep("x", 70000), collapse = ""),
    "lookup",
    list(query = "example")
  )
  bundle <- as_trajectory_ellmer(list(ellmer::AssistantTurn(list(request))))
  expect_match(
    trajectory_losses(bundle)$field[[1L]],
    "contents$id",
    fixed = TRUE
  )
  expect_gt(nrow(scan_trajectories(bundle)), 0L)
  result <- assess_trajectory_scans(bundle)
  expect_equal(result$assessments$status[1:3], rep("insufficient_evidence", 3))
  expect_identical(nrow(result$findings), 0L)
})

test_that("one recorded parent does not establish complete error-chain evidence", {
  bundle <- scan_error_chain_fixture()
  events <- trajectory_events(bundle)
  parent <- events[1L, ]
  parent$event_id <- "known-parent"
  parent$event_index <- 1L
  parent$turn_id <- NA_character_
  parent$content_index <- NA_integer_
  events$event_index <- events$event_index + 1L
  parent$event_type <- "content"
  parent$status <- "completed"
  parent$error <- NA_character_
  events$parent_event_id <- c(NA_character_, "known-parent")
  bundle@events <- rbind(parent, events)
  result <- assess_trajectory_scans(bundle, scans = "error_chain")
  expect_identical(result$assessments$status, "insufficient_evidence")
  expect_identical(nrow(result$findings), 0L)
})

test_that("Commons provenance preserves text-only tool inapplicability", {
  bundle <- as_trajectory_commons(commons_trajectory_fixture())
  events <- trajectory_events(bundle)
  expect_setequal(events$event_type, c("content", "commons:provenance"))
  expect_equal(
    assess_trajectory_scans(bundle)$assessments$status[1:5],
    rep("not_applicable", 5)
  )

  events$event_type[events$event_type == "commons:provenance"] <- "custom"
  bundle@events <- events
  expect_equal(
    assess_trajectory_scans(bundle)$assessments$status[1:5],
    rep("insufficient_evidence", 5)
  )
})

test_that("metadata loss names do not imply missing semantic records", {
  skip_if_not_installed("ellmer")
  oversized <- strrep("x", 70000)
  request <- ellmer::ContentToolRequest(
    "call-1",
    "lookup",
    list(query = "example"),
    extra = list(
      capture_note = oversized,
      read_info = oversized,
      parts = oversized,
      events = oversized,
      call_id_note = oversized,
      error_budget = oversized,
      status = oversized,
      arguments = oversized,
      value = oversized,
      name = oversized,
      parent_event_id = oversized
    )
  )
  bundle <- as_trajectory_ellmer(list(
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(ellmer::ContentToolResult("OK", request = request)))
  ))
  losses <- trajectory_losses(bundle)
  expect_setequal(
    losses$field,
    paste0(
      "contents$extra$",
      c(
        "capture_note",
        "read_info",
        "parts",
        "events",
        "call_id_note",
        "error_budget",
        "status",
        "arguments",
        "value",
        "name",
        "parent_event_id"
      )
    )
  )
  result <- assess_trajectory_scans(bundle)$assessments
  expect_equal(
    result$status,
    c(
      rep("assessed_no_findings", 6),
      "not_applicable",
      rep("assessed_no_findings", 2)
    )
  )
  expect_identical(result$loss_rows[[1]], seq_len(nrow(losses)))
  expect_match(paste(result$limitations[[1]], collapse = " "), "capture_note")

  for (field in c(
    "turns",
    "events",
    "messages",
    "parts",
    "read_info$incomplete",
    "capture"
  )) {
    lost <- losses[1L, ]
    lost$field <- field
    bundle@losses <- lost
    expect_equal(
      assess_trajectory_scans(bundle)$assessments$status[1:5],
      rep("insufficient_evidence", 5),
      info = field
    )
  }
})

test_that("turn status losses respect exact paths and record ownership", {
  skip_if_not_installed("ellmer")
  bundle <- as_trajectory_ellmer(list(
    ellmer::UserTurn(list(ellmer::ContentText("Hello"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
  ))
  turns <- trajectory_turns(bundle)
  events <- trajectory_events(bundle)
  cases <- tibble::tribble(
    ~field                      , ~owner       , ~expected               ,
    "metadata$error_budget"     , "turn"       , "assessed_no_findings"  ,
    "metadata$status"           , "turn"       , "assessed_no_findings"  ,
    "metadata$finish_reason"    , "turn"       , "assessed_no_findings"  ,
    "turns[[1]]$metadata$error" , "turn"       , "assessed_no_findings"  ,
    "error"                     , "event"      , "assessed_no_findings"  ,
    "contents$error"            , "event"      , "assessed_no_findings"  ,
    "status"                    , "trajectory" , "assessed_no_findings"  ,
    "status"                    , "turn"       , "insufficient_evidence" ,
    "error"                     , "turn"       , "insufficient_evidence" ,
    "contents$error"            , "turn"       , "insufficient_evidence" ,
    "turns[[1]]$finish_reason"  , "turn"       , "insufficient_evidence" ,
    "finish_reason"             , "unassigned" , "insufficient_evidence"
  )
  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    losses <- trajectory_loss_table(list(trajectory_new_loss(
      trajectory_ids(
        if (case$owner == "unassigned") {
          NA_character_
        } else {
          turns$trajectory_id[[1]]
        },
        if (case$owner %in% c("turn", "event")) {
          turns$turn_id[[1]]
        } else {
          NA_character_
        },
        if (case$owner == "event") events$event_id[[1]] else NA_character_
      ),
      case$field,
      "truncated",
      "Field truncated"
    )))
    bundle@losses <- losses
    result <- assess_trajectory_scans(bundle, scans = "turn_error")$assessments
    expect_identical(
      result$status,
      case$expected,
      info = paste(case$field, case$owner)
    )
    expect_identical(result$loss_rows[[1]], 1L)
  }
})

test_that("public turn finish-reason loss qualifies negative and positive results", {
  skip_if_not_installed("ellmer")
  turn <- ellmer::AssistantTurn(
    list(ellmer::ContentText("Done")),
    finish_reason = strrep("x", 70000)
  )
  bundle <- as_trajectory_ellmer(list(turn))
  expect_identical(trajectory_losses(bundle)$field, "turns[[1]]$finish_reason")
  expect_identical(
    assess_trajectory_scans(bundle, scans = "turn_error")$assessments$status,
    "insufficient_evidence"
  )
  turns <- trajectory_turns(bundle)
  turns$status <- "failed"
  turns$error <- "Recorded failure"
  bundle@turns <- turns
  result <- assess_trajectory_scans(bundle, scans = "turn_error")
  expect_identical(result$assessments$status, "assessed_with_findings")
  expect_identical(nrow(result$findings), 1L)
  expect_match(
    paste(result$assessments$limitations[[1]], collapse = " "),
    "prevent ruling out"
  )
})

test_that("event diagnostic losses respect exact paths and record ownership", {
  skip_if_not_installed("ellmer")
  bundle <- as_trajectory_ellmer(ellmer_tool_turns_fixture())
  turns <- trajectory_turns(bundle)
  events <- trajectory_events(bundle)
  cases <- tibble::tribble(
    ~field                              , ~owner       , ~expected               ,
    "events[[1]]$metadata$error_budget" , "event"      , "assessed_no_findings"  ,
    "contents$extra$status"             , "event"      , "assessed_no_findings"  ,
    "status"                            , "turn"       , "assessed_no_findings"  ,
    "contents$error"                    , "turn"       , "assessed_no_findings"  ,
    "error"                             , "trajectory" , "assessed_no_findings"  ,
    "status"                            , "event"      , "insufficient_evidence" ,
    "error"                             , "event"      , "insufficient_evidence" ,
    "event_type"                        , "event"      , "insufficient_evidence" ,
    "contents$error"                    , "event"      , "insufficient_evidence" ,
    "events[[1]]$error"                 , "event"      , "insufficient_evidence" ,
    "stages$items[[1]]$error"           , "event"      , "insufficient_evidence" ,
    "status"                            , "unassigned" , "insufficient_evidence"
  )
  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    bundle@losses <- trajectory_loss_table(list(trajectory_new_loss(
      trajectory_ids(
        if (case$owner == "unassigned") {
          NA_character_
        } else {
          turns$trajectory_id[[1]]
        },
        if (case$owner %in% c("turn", "event")) {
          turns$turn_id[[1]]
        } else {
          NA_character_
        },
        if (case$owner == "event") events$event_id[[1]] else NA_character_
      ),
      case$field,
      "truncated",
      "Field truncated"
    )))
    result <- assess_trajectory_scans(
      bundle,
      scans = c("event_error", "error_chain")
    )$assessments
    expected <- c(
      case$expected,
      if (case$expected == "assessed_no_findings") {
        "not_applicable"
      } else {
        case$expected
      }
    )
    expect_identical(
      result$status,
      expected,
      info = paste(case$field, case$owner)
    )
    expect_identical(result$loss_rows, list(1L, 1L))
  }
})

test_that("tool identity losses affect only the relevant tool records", {
  skip_if_not_installed("ellmer")
  bundle <- as_trajectory_ellmer(ellmer_tool_turns_fixture())
  events <- trajectory_events(bundle)
  call <- which(events$event_type == "tool_call")[[1]]
  content <- which(events$event_type == "content")[[1]]
  for (field in c(
    "call_id",
    "tool_call_id",
    "events[[1]]$call_id",
    "contents$id",
    "contents$request$id"
  )) {
    for (index in c(call, content)) {
      bundle@losses <- trajectory_loss_table(list(trajectory_new_loss(
        trajectory_ids(
          events$trajectory_id[[index]],
          events$turn_id[[index]],
          events$event_id[[index]]
        ),
        field,
        "truncated",
        "Identity truncated"
      )))
      expect_equal(
        assess_trajectory_scans(bundle)$assessments$status[1:3],
        rep(
          if (index == call) {
            "insufficient_evidence"
          } else {
            "assessed_no_findings"
          },
          3
        ),
        info = paste(field, index)
      )
    }
  }
})

test_that("trajectory metadata and causal metadata do not create coverage gaps", {
  bundle <- scan_error_chain_fixture()
  events <- trajectory_events(bundle)
  events$parent_event_id <- "known-parent"
  parent <- events[1L, ]
  parent$event_id <- "known-parent"
  parent$event_index <- 1L
  parent$parent_event_id <- NA_character_
  parent$turn_id <- NA_character_
  parent$content_index <- NA_integer_
  parent$status <- "completed"
  parent$error <- NA_character_
  parent$event_type <- "content"
  events$event_index <- events$event_index + 1L
  bundle@events <- rbind(parent, events)
  info <- trajectory_info(bundle)
  info$status <- "completed"
  info$error <- NA_character_
  bundle@trajectories <- info
  for (field in c(
    "metadata$status",
    "metadata$error",
    "metadata$parent_event_id"
  )) {
    bundle@losses <- trajectory_loss_table(list(trajectory_new_loss(
      trajectory_ids(info$trajectory_id[[1]]),
      field,
      "truncated",
      "Metadata truncated"
    )))
    result <- assess_trajectory_scans(
      bundle,
      scans = c("trajectory_error", "error_chain")
    )$assessments
    expect_equal(result$status, rep("assessed_no_findings", 2), info = field)
    expect_identical(result$loss_rows, list(1L, 1L))
  }
  losses <- trajectory_losses(bundle)
  losses$field <- "status"
  bundle@losses <- losses
  expect_identical(
    assess_trajectory_scans(
      bundle,
      scans = "trajectory_error"
    )$assessments$status,
    "insufficient_evidence"
  )
  losses$field <- "parent_event_id"
  bundle@losses <- losses
  expect_identical(
    assess_trajectory_scans(bundle, scans = "error_chain")$assessments$status,
    "insufficient_evidence"
  )
})
