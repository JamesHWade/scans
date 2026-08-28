test_that("scans_app() creates a read-only Shiny app", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- trajectory_fixture("simple_exchange")
  app <- scans_app(bundle)

  expect_s3_class(app, "shiny.appobj")
  expect_identical(trajectory_events(bundle)$text, c("Hello", "Hi there"))
})

test_that("eager UI choices use metadata without deriving app data", {
  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("one", "two"),
      source_type = c("manual", "otel"),
      status = c("completed", "failed")
    ),
    data.frame(),
    data.frame()
  )
  source <- scans_app_sources(bundle)$sources[[1L]]
  testthat::local_mocked_bindings(
    scans_app_data = function(...) stop("must not derive app data")
  )

  choices <- scans_app_initial_choices(source)

  expect_identical(
    unname(choices$source),
    c(choices$source_all, "manual", "otel")
  )
  expect_identical(
    unname(choices$status),
    c(choices$status_all, "completed", "failed")
  )
})

test_that("scans app switches between named applications", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  first <- trajectory_fixture("simple_exchange")
  second <- trajectory_fixture("tool_error")
  app <- scans_app(list(
    "Support assistant" = first,
    "Research assistant" = second
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    expect_match(
      as.character(output$scans_app_entries)[[1L]],
      "trajectory-simple",
      fixed = TRUE
    )

    session$setInputs(
      scans_app_source = trajectory_info(first)$source_type[[1L]]
    )
    session$flushReact()
    session$setInputs(scans_app_application = "Research assistant")
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "trajectory-error", fixed = TRUE)
    expect_no_match(entries, "trajectory-simple", fixed = TRUE)
  })
})

test_that("scans app loads and reloads application sources lazily", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  calls <- new.env(parent = emptyenv())
  calls$first <- 0L
  calls$second <- 0L
  app <- scans_app(list(
    "First deployment" = function() {
      calls$first <- calls$first + 1L
      trajectory_fixture("simple_exchange")
    },
    "Second deployment" = function() {
      calls$second <- calls$second + 1L
      trajectory_fixture("tool_error")
    }
  ))
  expect_identical(c(calls$first, calls$second), c(0L, 0L))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    expect_identical(c(calls$first, calls$second), c(1L, 0L))

    session$flushReact()
    expect_identical(c(calls$first, calls$second), c(1L, 0L))

    session$setInputs(scans_app_application = "Second deployment")
    session$flushReact()
    expect_identical(c(calls$first, calls$second), c(1L, 1L))

    session$setInputs(scans_app_application = "First deployment")
    session$flushReact()
    expect_identical(c(calls$first, calls$second), c(1L, 1L))

    session$setInputs(scans_app_reload = 1L)
    session$flushReact()
    expect_identical(c(calls$first, calls$second), c(2L, 1L))
  })
})

test_that("scans app offers reload only for the active lazy source", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(list(
    "Static snapshot" = trajectory_fixture("simple_exchange"),
    "Live deployment" = function() trajectory_fixture("tool_error")
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    expect_null(output$scans_app_reload_control)

    session$setInputs(scans_app_application = "Live deployment")
    session$flushReact()
    expect_match(
      as.character(output$scans_app_reload_control)[[1L]],
      "Reload traces",
      fixed = TRUE
    )

    session$setInputs(scans_app_application = "Static snapshot")
    session$flushReact()
    expect_null(output$scans_app_reload_control)
  })
})

scans_app_connect_trace_line <- function(prompt) {
  start_time <- sprintf("%.0f", (as.numeric(Sys.time()) - 60) * 1e9)
  end_time <- sprintf("%.0f", (as.numeric(Sys.time()) - 59) * 1e9)
  messages <- paste0(
    '[{\\"role\\":\\"user\\",\\"parts\\":[{\\"type\\":\\"text\\",',
    '\\"content\\":\\"',
    prompt,
    '\\"}]}]'
  )
  paste0(
    '{"resourceSpans":[{"scopeSpans":[{"spans":[{',
    '"traceId":"0123456789abcdef","spanId":"0123456789abcdef",',
    '"name":"chat test-model","startTimeUnixNano":"',
    start_time,
    '","endTimeUnixNano":"',
    end_time,
    '","attributes":[',
    '{"key":"gen_ai.operation.name","value":{"stringValue":"chat"}},',
    '{"key":"gen_ai.input.messages","value":{"stringValue":"',
    messages,
    '"}}]}]}]}]}'
  )
}

test_that("scans app switches between Posit Connect trace stores lazily", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("commons", "0.0.0.9002")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  support_guid <- "11111111-1111-4111-8111-111111111111"
  research_guid <- "22222222-2222-4222-8222-222222222222"
  withr::local_envvar(
    CONNECT_SERVER = "https://connect.example.com",
    CONNECT_API_KEY = "secret"
  )
  requests <- new.env(parent = emptyenv())
  requests$n <- 0L
  requests$urls <- character()
  httr2::local_mocked_responses(function(req) {
    requests$n <- requests$n + 1L
    requests$urls <- c(requests$urls, req$url)
    if (grepl("/jobs", req$url, fixed = TRUE)) {
      return(httr2::new_response(
        "GET",
        req$url,
        200L,
        list(`Content-Type` = "application/json"),
        charToRaw("[]"),
        request = req
      ))
    }
    prompt <- if (grepl(support_guid, req$url, fixed = TRUE)) {
      "Support question"
    } else {
      "Research question"
    }
    line <- scans_app_connect_trace_line(prompt)
    httr2::new_response(
      "GET",
      req$url,
      200L,
      list(
        Server = "Posit Connect v2026.08.0",
        `X-Total-Count` = "1"
      ),
      charToRaw(line),
      request = req
    )
  })

  app <- scans_app_connect(c(
    "Support deployment" = support_guid,
    "Research deployment" = research_guid
  ))
  expect_identical(requests$n, 0L)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "Support question", fixed = TRUE)
    expect_no_match(entries, "Research question", fixed = TRUE)

    session$setInputs(scans_app_application = "Research deployment")
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "Research question", fixed = TRUE)
    expect_no_match(entries, "Support question", fixed = TRUE)
  })

  trace_urls <- requests$urls[!grepl("/jobs", requests$urls, fixed = TRUE)]
  expect_true(all(grepl("from=", trace_urls, fixed = TRUE)))
  expect_true(all(grepl("to=", trace_urls, fixed = TRUE)))
})

test_that("scans_app_connect requires named scalar content references", {
  skip_if_not_installed("commons", "0.0.0.9002")

  expect_error(
    scans_app_connect("11111111-1111-4111-8111-111111111111"),
    class = "scans_error_connect_source"
  )
  expect_error(
    scans_app_connect(list("App" = c("first", "second"))),
    class = "scans_error_connect_source"
  )
  expect_error(
    scans_app_connect(c("App" = "https://other.example.com/content/guid")),
    class = "scans_error_connect_source"
  )
  expect_error(
    scans_app_connect(
      c("App" = "11111111-1111-4111-8111-111111111111"),
      n = 0
    ),
    class = "rlang_error"
  )
})

test_that("scans app contains application source failures", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(list(
    "Broken deployment" = function() {
      stop("database password=hunter2", call. = FALSE)
    }
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    entries <- as.character(output$scans_app_entries)[[1L]]

    expect_identical(output$scans_app_visible_count, "Traces unavailable")
    expect_match(entries, "Could not load traces", fixed = TRUE)
    expect_no_match(entries, "hunter2", fixed = TRUE)
    load_error <- as.character(output$scans_app_load_error)[[1L]]
    expect_match(load_error, "Check the server logs", fixed = TRUE)
    expect_no_match(load_error, "hunter2", fixed = TRUE)
  })
})

test_that("scans app logs source failure details on the server", {
  expect_message(
    scans_app_log_source_error(
      "Broken deployment",
      simpleError("database password=hunter2")
    ),
    "hunter2",
    fixed = TRUE
  )
})

test_that("scans app requires an unambiguous application catalog", {
  bundle <- trajectory_fixture("simple_exchange")
  duplicate_names <- stats::setNames(list(bundle, bundle), c("App", "App"))

  expect_snapshot(scans_app(list(bundle)), error = TRUE)
  expect_snapshot(scans_app(duplicate_names), error = TRUE)
  expect_snapshot(scans_app(list("App" = 1)), error = TRUE)
})

test_that("scans app rejects loaders that require arguments", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  expect_snapshot(
    scans_app(list(
      "Broken deployment" = function(content_guid) NULL
    )),
    error = TRUE
  )

  app <- scans_app(list(
    "Configurable deployment" = function(limit = 100L, ...) {
      trajectory_fixture("simple_exchange")
    }
  ))
  expect_s3_class(app, "shiny.appobj")

  zero_argument_app <- scans_app(list(
    "Zero-argument deployment" = function() {
      trajectory_fixture("simple_exchange")
    }
  ))
  expect_s3_class(zero_argument_app, "shiny.appobj")

  expect_snapshot(
    scans_app(list("Primitive deployment" = `if`)),
    error = TRUE
  )
})

test_that("scans app enforces optional dependency minimum versions", {
  expect_error(
    scans_app_check_packages(
      namespace_available = function(...) TRUE,
      package_version = function(package) {
        versions <- c(bslib = "0.10.0", shiny = "1.10.0")
        numeric_version(versions[[package]])
      }
    ),
    regexp = "bslib.*0[.]11[.]0.*shiny.*1[.]11[.]1",
    class = "scans_error_app_dependency"
  )
})

test_that("scans app preserves dependencies attached by bslib", {
  skip_if_not_installed("htmltools")

  existing <- htmltools::htmlDependency(
    name = "existing",
    version = "1.0.0",
    src = c(file = ".")
  )
  page <- htmltools::attachDependencies(htmltools::tags$div(), existing)

  page <- scans_app_attach_dependency(page)
  dependencies <- htmltools::htmlDependencies(page)

  expect_setequal(
    vapply(dependencies, `[[`, character(1), "name"),
    c("existing", "scans-app")
  )
})

test_that("scans app stylesheet dependency tracks the package version", {
  skip_if_not_installed("htmltools")

  dependency <- scans_app_dependency(
    package_version = function(...) numeric_version("1.2.3")
  )

  expect_identical(dependency$version, "1.2.3")
})

test_that("scans app groups searchable event rows in canonical order", {
  events <- tibble::tibble(
    trajectory_id = c("first", "second", "first", "first"),
    event_id = c("first-3", "second-1", "first-1", "first-2"),
    event_index = c(3L, 1L, 1L, 2L),
    text = c("Later", "Only", "Earlier", "  ")
  )

  groups <- scans_app_event_text_groups(
    c("first", "second", "eventless"),
    events
  )

  expect_identical(groups, list(c(3L, 1L), 2L, integer()))
})

test_that("scans app title fallbacks ignore whitespace-only metadata", {
  expect_identical(
    scans_app_first_string("  ", NA_character_, "trajectory-id"),
    "trajectory-id"
  )
})

test_that("scans app records retain source identity and deterministic findings", {
  data <- scans_app_data(trajectory_fixture("tool_error"))

  expect_identical(data$records$trajectory_id, "trajectory-error")
  expect_identical(data$records$title, "Find the record")
  expect_identical(data$records$n_findings, 2L)
  expect_identical(data$records$n_errors, 2L)
  expect_setequal(data$findings$event_id, c("error-event-3", "error-event-4"))
})

test_that("scans app titles use canonical event order", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "unordered-title",
      source_type = "manual"
    ),
    tibble::tibble(
      trajectory_id = "unordered-title",
      turn_id = c("later-turn", "earlier-turn", "middle-turn"),
      turn_index = c(3L, 1L, 2L),
      role = "user"
    ),
    tibble::tibble(
      trajectory_id = "unordered-title",
      event_id = c("later-event", "earlier-event", "middle-event"),
      event_index = c(3L, 1L, 2L),
      event_type = "content",
      turn_id = c("later-turn", "earlier-turn", "middle-turn"),
      content_type = "text",
      text = c("Later prompt", "Earlier prompt", "Middle prompt")
    )
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    entries <- as.character(output$scans_app_entries)[[1L]]

    expect_match(entries, "Earlier prompt", fixed = TRUE)
    expect_no_match(entries, "Later prompt", fixed = TRUE)

    session$setInputs(
      scans_app_query = "Earlier prompt Middle prompt Later prompt"
    )
    session$flushReact()
    expect_identical(output$scans_app_visible_count, "1 of 1 trajectory")
  })
})

test_that("scans app filters compose without interpreting the query as a regex", {
  records <- scans_app_data(trajectory_fixture(
    "ellmerverse_correlation"
  ))$records
  records$n_findings[[2L]] <- 1L

  expect_identical(
    scans_app_filter_records(records, source = "deputy"),
    2L
  )
  expect_identical(
    scans_app_filter_records(records, query = "research-module"),
    3L
  )
  expect_identical(
    scans_app_filter_records(records, query = "[", findings_only = TRUE),
    integer()
  )
  expect_identical(
    scans_app_filter_records(records, findings_only = TRUE),
    2L
  )
})

test_that("scans app search covers the entire transcript", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  tables <- fixture_source(trajectory_fixture("simple_exchange"))
  tables$events$text[[2L]] <- paste(
    strrep("x", 100L),
    "late transcript token"
  )
  app <- scans_app(do.call(TrajectoryBundle, tables))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_query = "late transcript token",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "1 of 1 trajectory")
  })
})

test_that("scans app search ignores missing metadata", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "plain",
      source_type = "manual"
    ),
    data.frame(),
    data.frame()
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_query = "na",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "0 of 1 trajectory")
  })
})

test_that("scans app distinguishes a missing status from an unknown status", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("literal-missing", "unknown-status"),
      source_type = "manual",
      status = c("missing", NA_character_)
    ),
    data.frame(),
    data.frame()
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_status = "missing",
      scans_app_query = "",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "literal-missing", fixed = TRUE)
    expect_no_match(entries, "unknown-status", fixed = TRUE)
  })
})

test_that("scans app treats empty and missing statuses as unknown", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("empty-status", "missing-status", "complete-status"),
      source_type = "manual",
      status = c("", NA_character_, "complete")
    ),
    data.frame(),
    data.frame()
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_status = ".scans-app-status-unknown",
      scans_app_query = "",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "empty-status", fixed = TRUE)
    expect_match(entries, "missing-status", fixed = TRUE)
    expect_no_match(entries, "complete-status", fixed = TRUE)
  })
})

test_that("scans app distinguishes literal all values from All filters", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = c("literal-all", "other-value"),
      source_type = c("all", "manual"),
      status = c("all", "complete")
    ),
    data.frame(),
    data.frame()
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_source = "all",
      scans_app_status = "all",
      scans_app_query = "",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    entries <- as.character(output$scans_app_entries)[[1L]]
    expect_match(entries, "literal-all", fixed = TRUE)
    expect_no_match(entries, "other-value", fixed = TRUE)
  })
})

test_that("scans app keeps selection and empty filter states reactive", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  data <- scans_app_data(trajectory_fixture("delegated_agent"))
  shiny::testServer(scans_app_server(data), {
    session$setInputs(
      scans_app_query = "child",
      scans_app_findings_only = FALSE
    )
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "1 of 2 trajectories")
    expect_match(
      as.character(output$scans_app_entries)[[1L]],
      "trajectory-child"
    )

    session$setInputs(scans_app_query = "does not exist")
    session$flushReact()

    expect_identical(output$scans_app_visible_count, "0 of 2 trajectories")
    expect_match(
      as.character(output$scans_app_entries)[[1L]],
      "No trajectories match these filters.",
      fixed = TRUE
    )
  })
})

test_that("scans app preserves trajectory selection while rescanning", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(trajectory_fixture("delegated_agent"))
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    selected(2L)
    session$flushReact()
    expect_identical(selected(), 2L)

    session$setInputs(scans_app_scans = character())
    session$flushReact()

    expect_identical(selected(), 2L)
  })
})

test_that("scans app renders canonical text as text and links finding evidence", {
  skip_if_not_installed("htmltools")

  tables <- fixture_source(trajectory_fixture("tool_error"))
  tables$events$text[[1L]] <- "<script>alert('not markup')</script>"
  data <- scans_app_data(do.call(TrajectoryBundle, tables))
  transcript <- htmltools::renderTags(scans_app_transcript_ui(data, 1L))$html
  evidence <- htmltools::renderTags(scans_app_evidence_ui(data, 1L))$html

  expect_match(transcript, "&lt;script&gt;", fixed = TRUE)
  expect_no_match(transcript, "<script>", fixed = TRUE)
  expect_match(evidence, "#scans-app-event-3", fixed = TRUE)
  expect_match(evidence, "error-event-3", fixed = TRUE)
})

test_that("scans app identifies repeated scorer evaluations", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "evaluated-trajectory",
      source_type = "vitals"
    ),
    data.frame(),
    data.frame(),
    evaluations = tibble::tibble(
      trajectory_id = rep("evaluated-trajectory", 2L),
      evaluation_id = c("evaluation-epoch-1", "evaluation-epoch-2"),
      scorer = rep("accuracy", 2L),
      value = list(0.5, 0.75)
    )
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    evidence <- as.character(output$scans_app_evidence)[[1L]]

    expect_match(
      evidence,
      "<strong>accuracy[^<]*evaluation-epoch-1</strong>",
      perl = TRUE
    )
    expect_match(
      evidence,
      "<strong>accuracy[^<]*evaluation-epoch-2</strong>",
      perl = TRUE
    )
  })
})

test_that("scans app resolves loss ownership through turns and events", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "loss-owner",
      source_type = "manual"
    ),
    tibble::tibble(
      trajectory_id = "loss-owner",
      turn_id = "loss-turn",
      turn_index = 1L,
      role = "assistant"
    ),
    tibble::tibble(
      trajectory_id = "loss-owner",
      event_id = "loss-event",
      event_index = 1L,
      event_type = "content",
      turn_id = "loss-turn",
      content_type = "text",
      text = "Done"
    ),
    losses = tibble::tibble(
      trajectory_id = NA_character_,
      turn_id = c("loss-turn", NA_character_),
      event_id = c(NA_character_, "loss-event"),
      field = c("turn_field", "event_field"),
      reason = "unsupported",
      detail = c("turn-linked loss", "event-linked loss")
    )
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    overview <- as.character(output$scans_app_overview)[[1L]]
    evidence <- as.character(output$scans_app_evidence)[[1L]]

    expect_match(overview, "Losses[\\s\\S]*2", perl = TRUE)
    expect_match(evidence, "Losses (2)", fixed = TRUE)
    expect_match(evidence, "turn-linked loss", fixed = TRUE)
    expect_match(evidence, "event-linked loss", fixed = TRUE)
  })
})

test_that("scans app surfaces trajectory-level errors in context", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "trajectory-failure",
      source_type = "manual",
      status = "error",
      error = "Trajectory failed before recording an event"
    ),
    data.frame(),
    data.frame()
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    evidence <- as.character(output$scans_app_evidence)[[1L]]

    expect_match(
      evidence,
      "Trajectory failed before recording an event",
      fixed = TRUE
    )
  })
})

test_that("scans app renders canonical source context and bounded metadata", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  app <- scans_app(TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "trajectory-context",
      source_type = "vitals",
      source_uri = "https://connect.example.com/content/fixture",
      task_id = "task-fixture",
      sample_id = "sample-fixture",
      epoch = 3L,
      metadata = list(list(
        dataset = "golden-set",
        unsafe = "<script>alert('unsafe')</script>",
        bounded = paste0(strrep("x", 5000L), "TAIL_SENTINEL")
      ))
    ),
    data.frame(),
    data.frame()
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    evidence <- as.character(output$scans_app_evidence)[[1L]]

    expect_match(evidence, "Source URI", fixed = TRUE)
    expect_match(
      evidence,
      "https://connect.example.com/content/fixture",
      fixed = TRUE
    )
    expect_match(evidence, "Task[\\s\\S]*task-fixture", perl = TRUE)
    expect_match(evidence, "Sample[\\s\\S]*sample-fixture", perl = TRUE)
    expect_match(evidence, "Epoch[\\s\\S]*3", perl = TRUE)
    expect_match(evidence, "Metadata", fixed = TRUE)
    expect_match(evidence, "golden-set", fixed = TRUE)
    expect_match(evidence, "&lt;script&gt;", fixed = TRUE)
    expect_no_match(evidence, "<script>", fixed = TRUE)
    expect_no_match(evidence, "TAIL_SENTINEL", fixed = TRUE)
  })
})

test_that("scans app renders transcript blocks in canonical event order", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "ordered-events",
      source_type = "manual"
    ),
    tibble::tibble(
      trajectory_id = "ordered-events",
      turn_id = "turn-1",
      turn_index = 1L,
      role = "assistant"
    ),
    tibble::tibble(
      trajectory_id = "ordered-events",
      event_id = c("run-before", "turn-event", "run-after"),
      event_index = 1:3,
      event_type = "content",
      content_type = "text",
      turn_id = c(NA_character_, "turn-1", NA_character_),
      text = c("Run event first", "Turn event second", "Run event third")
    )
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    transcript <- as.character(output$scans_app_transcript)[[1L]]

    expect_lt(
      regexpr("Run event first", transcript, fixed = TRUE)[[1L]],
      regexpr("Turn event second", transcript, fixed = TRUE)[[1L]]
    )
    expect_lt(
      regexpr("Turn event second", transcript, fixed = TRUE)[[1L]],
      regexpr("Run event third", transcript, fixed = TRUE)[[1L]]
    )
  })
})

test_that("scans app keeps eventless turns in canonical turn order", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny", "1.11.1")

  bundle <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "eventless-turn",
      source_type = "manual"
    ),
    tibble::tibble(
      trajectory_id = "eventless-turn",
      turn_id = c("empty-turn", "later-turn"),
      turn_index = 1:2,
      role = c("user", "assistant")
    ),
    tibble::tibble(
      trajectory_id = "eventless-turn",
      event_id = "later-event",
      event_index = 1L,
      event_type = "content",
      turn_id = "later-turn",
      content_type = "text",
      text = "Later event"
    )
  )
  app <- scans_app(bundle)

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    transcript <- as.character(output$scans_app_transcript)[[1L]]

    expect_lt(
      regexpr("Turn 1", transcript, fixed = TRUE)[[1L]],
      regexpr("Turn 2", transcript, fixed = TRUE)[[1L]]
    )
  })
})

test_that("scans app supports event-only and empty bundles", {
  skip_if_not_installed("htmltools")

  event_only <- TrajectoryBundle(
    tibble::tibble(
      trajectory_id = "product-1",
      source_type = "tempest"
    ),
    data.frame(),
    tibble::tibble(
      trajectory_id = "product-1",
      event_id = "stage-1",
      event_index = 1L,
      event_type = "tempest:stage",
      name = "outline"
    )
  )
  data <- scans_app_data(event_only)
  html <- htmltools::renderTags(scans_app_transcript_ui(data, 1L))$html

  expect_match(html, "Event stream", fixed = TRUE)
  expect_match(html, "Tempest stage", fixed = TRUE)

  empty <- scans_app_data(TrajectoryBundle(
    data.frame(),
    data.frame(),
    data.frame()
  ))
  expect_identical(nrow(empty$records), 0L)
  expect_identical(scans_app_filter_records(empty$records), integer())
})

test_that("scans app thresholds accept only bounded whole numbers", {
  expect_identical(scans_app_threshold(4, 2L), 4L)

  invalid <- list(
    NULL,
    numeric(),
    c(2, 3),
    NA_real_,
    1,
    2.5,
    Inf,
    .Machine$integer.max + 1,
    "3"
  )
  actual <- vapply(
    invalid,
    scans_app_threshold,
    integer(1),
    default = 3L
  )
  expect_identical(actual, rep(3L, length(invalid)))
})
