export_fixture <- function() {
  data <- cohort_fixture_data()
  ids <- data$info$trajectory_id
  queue <- cohort_review_queue(data, ids, cohort_fixture_annotations())
  list(
    data = data,
    ids = ids,
    queue = queue,
    performance = scans_app_performance_data(data, 1:5, queue = queue),
    link = scans_app_link(
      "https://connect.test/app/",
      "Cohort",
      list(pattern = "event_error", day = "2026-10-03")
    )
  )
}

test_that("deep links carry the scope and old hashes still parse", {
  expect_identical(
    scans_app_scope_hash("My app", "otel/a b"),
    scans_app_hash("My app", "otel/a b")
  )
  scope <- list(
    pattern = "event_error",
    findings = TRUE,
    review = "reviewed",
    review_label = "(no label)",
    day = "2026-10-03",
    tool = "look/up & co",
    tool_failed = TRUE,
    user = "a=b@example.com",
    model = "m:1"
  )
  hash <- scans_app_scope_hash("My app", "otel/a b", scope)
  expect_identical(
    scans_app_parse_hash(hash, "My app"),
    list(application = "My app", trajectory_id = "otel/a b", scope = scope)
  )
  expect_identical(
    scans_app_parse_hash(paste0(hash, "?event=e%2F1"), "My app")$event_id,
    "e/1"
  )
  expect_identical(
    scans_app_parse_hash(
      scans_app_scope_hash("My app", scope = scope),
      "My app"
    ),
    list(application = "My app", scope = scope)
  )

  expect_identical(
    scans_app_parse_hash("My%20app/otel%2Fa", "My app"),
    list(application = "My app", trajectory_id = "otel/a")
  )
  # An application alone, with or without a scope, names its overview.
  expect_identical(
    scans_app_parse_hash("#My%20app/", "My app"),
    list(application = "My app")
  )
  # A scope, even one that sets nothing this app knows, clears the filters.
  expect_identical(
    scans_app_parse_hash("My%20app/?scope=unknown=1", "My app"),
    list(application = "My app", scope = stats::setNames(list(), character()))
  )
  expect_identical(
    scans_app_parse_hash(scans_app_scope_hash("A", scope = list()), "A"),
    scans_app_parse_hash("A/?scope=unknown=1", "A")
  )
  expect_identical(
    scans_app_parse_hash(scans_app_scope_hash("A (b)"), c("Other", "A (b)")),
    list(application = "A (b)")
  )
  expect_null(scans_app_parse_hash("My%20app", "My app"))
  expect_null(scans_app_parse_hash("Other/", "My app"))

  expect_identical(scans_app_view_hash("My app", "t/1", TRUE), "My%20app/t%2F1")
  expect_identical(
    scans_app_view_hash("My app", "t/1", FALSE),
    "My%20app/t%2F1"
  )
  expect_identical(scans_app_view_hash("My app", NULL, TRUE), "My%20app/")
  expect_identical(scans_app_view_hash("My app", NULL, FALSE), "")
  expect_identical(
    scans_app_parse_scope("tool_failed=FALSE&findings=TRUE&agent=x"),
    list(findings = TRUE)
  )
  expect_identical(
    scans_app_link(NULL, "A", list(findings = FALSE))("t"),
    "#A/t?scope=none"
  )
})

test_that("the conversations export has one row per trajectory in view", {
  fixture <- export_fixture()
  table <- export_conversations(
    fixture$data,
    c("c", "a", "e"),
    fixture$queue,
    fixture$link,
    "Cohort",
    "3 of 5 conversations"
  )

  expect_named(
    table,
    c(
      "trajectory_id",
      "started_at",
      "model",
      "status",
      "class",
      "assessed",
      "n_findings",
      "n_error_findings",
      "tool_calls",
      "tool_failed",
      "elapsed_s",
      "tokens",
      "review_state",
      "review_label",
      "reviewed_by",
      "reviewed_at",
      "link",
      "application",
      "scope"
    )
  )
  expect_identical(table$trajectory_id, c("c", "a", "e"))
  expect_identical(
    table$started_at,
    c("2026-10-03T10:00:00Z", "2026-10-01T10:00:00Z", NA)
  )
  expect_identical(table$class, c("error", "error", "unassessed"))
  expect_identical(table$assessed, c(TRUE, TRUE, FALSE))
  expect_identical(table$n_findings, c(4L, 2L, NA))
  expect_identical(table$n_error_findings, c(2L, 1L, NA))
  expect_identical(table$tool_calls, c(2L, 2L, 0L))
  expect_identical(table$tool_failed, c(2L, 1L, 0L))
  expect_identical(table$elapsed_s, c(40, 10, NA))
  expect_identical(table$tokens, c(100, 15, NA))
  expect_identical(
    table$review_state,
    c("reviewed", "needs review", "needs review")
  )
  expect_identical(table$review_label, c("tool failure", NA, NA))
  expect_identical(
    table$link[[1L]],
    "https://connect.test/app/#Cohort/c?scope=pattern=event_error&day=2026-10-03"
  )
  expect_identical(unique(table$scope), "3 of 5 conversations")

  unknown <- export_conversations(fixture$data, "a")
  expect_identical(unknown$review_state, NA_character_)
  expect_identical(unknown$link, "#/a?scope=none")

  file <- withr::local_tempfile(fileext = ".csv")
  export_csv(table, file)
  written <- utils::read.csv(file, na.strings = "")
  expect_identical(nrow(written), 3L)
  expect_true(is.na(written$elapsed_s[[3L]]))
  # A trajectory no scan assessed has no finding count; an assessed one with
  # no findings has a count of zero.
  expect_match(readLines(file)[[4L]], ',"unassessed",FALSE,,,', fixed = TRUE)
  clean <- export_conversations(fixture$data, "b")
  expect_identical(
    clean[c("assessed", "n_findings")],
    tibble::tibble(
      assessed = TRUE,
      n_findings = 0L
    )
  )
})

test_that("finding counts stay integer when no trajectory is assessed", {
  fixture <- export_fixture()
  for (ids in list("e", character())) {
    table <- export_conversations(fixture$data, ids)
    expect_type(table$n_findings, "integer")
    expect_type(table$n_error_findings, "integer")
  }
  expect_identical(
    export_conversations(fixture$data, "e")$n_findings,
    NA_integer_
  )
})

# The app's report, built by the assembly behind investigation_report().
export_report_text <- function(fixture, include_users = FALSE) {
  report <- report_build(
    fixture$data,
    seq_along(fixture$ids),
    list(
      application = "Cohort",
      view = list(),
      queue = fixture$queue,
      include_users = include_users
    )
  )
  paste(format(report), collapse = "\n")
}

test_that("the report's pattern table counts affected over assessed", {
  fixture <- export_fixture()
  text <- export_report_text(fixture)
  expect_match(text, "| warning | 2 of 3 assessed |", fixed = TRUE)
  expect_match(text, "| error | 2 of 3 assessed |", fixed = TRUE)
  expect_no_match(text, "\\| (error|warning) \\| 2 of 5 \\|")
})

test_that("the report's model and user tables count findings over assessed", {
  fixture <- export_fixture()
  text <- export_report_text(fixture)
  expect_match(
    text,
    "| large | 2 | 2 of 2 assessed | 100 | 40.0 s |",
    fixed = TRUE
  )
  expect_match(text, "| Unknown | 1 | not assessed |", fixed = TRUE)
  expect_no_match(text, "ada", fixed = TRUE)

  named <- export_report_text(fixture, include_users = TRUE)
  expect_match(named, "| ada | 2 | 1 of 2 assessed |", fixed = TRUE)
})

test_that("CSV exports name users only when asked", {
  fixture <- export_fixture()
  link <- scans_app_link(
    "https://connect.test/app/",
    "Cohort",
    list(pattern = "event_error", user = "ada")
  )
  scope <- function(include_users) {
    export_scope(
      fixture$performance$cohort$headline,
      c(pattern = "Finding: event error", user = "User: ada"),
      include_users = include_users
    )
  }
  written <- function(table) {
    file <- withr::local_tempfile(fileext = ".csv")
    export_csv(table, file)
    paste(readLines(file), collapse = "\n")
  }

  findings <- export_findings(fixture$data, "a", link, "Cohort", scope(FALSE))
  expect_no_match(written(findings), "ada", fixed = TRUE)
  expect_identical(
    findings$link[[1L]],
    "https://connect.test/app/#Cohort/a?scope=pattern=event_error"
  )
  expect_match(
    findings$scope[[1L]],
    "One user (identifier omitted)",
    fixed = TRUE
  )

  conversations <- export_conversations(
    fixture$data,
    c("a", "d"),
    link = link,
    application = "Cohort",
    scope = scope(FALSE)
  )
  expect_false("user" %in% names(conversations))
  expect_no_match(written(conversations), "ada", fixed = TRUE)

  named <- export_conversations(
    fixture$data,
    c("a", "d"),
    link = link,
    application = "Cohort",
    scope = scope(TRUE),
    include_users = TRUE
  )
  expect_identical(named$user, c("ada", NA))
  expect_match(named$link[[1L]], "&user=ada", fixed = TRUE)
  expect_match(named$scope[[1L]], "User: ada", fixed = TRUE)
  expect_match(
    export_findings(
      fixture$data,
      "a",
      link,
      include_users = TRUE
    )$link[[1L]],
    "&user=ada",
    fixed = TRUE
  )
})

test_that("CSV exports omit a text filter's term when it names a user", {
  fixture <- export_fixture()
  link <- scans_app_link(
    "https://connect.test/app/",
    "Cohort",
    list(query = "ada", pattern = "event_error"),
    private = c("user", "query")
  )
  expect_identical(
    link("a", users = FALSE),
    "https://connect.test/app/#Cohort/a?scope=pattern=event_error"
  )
  expect_match(link("a"), "?scope=query=ada&", fixed = TRUE)
  labels <- c(query = "Text contains \"ada\"")
  headline <- fixture$performance$cohort$headline
  hidden <- export_scope(
    headline,
    labels,
    include_users = FALSE,
    query_names_user = TRUE
  )
  expect_no_match(hidden, "ada", fixed = TRUE)
  expect_match(hidden, "Text filter (term omitted", fixed = TRUE)
  expect_match(
    export_scope(
      headline,
      labels,
      include_users = TRUE,
      query_names_user = TRUE
    ),
    "Text contains \"ada\"",
    fixed = TRUE
  )
  expect_match(
    export_scope(headline, labels, include_users = FALSE),
    "Text contains \"ada\"",
    fixed = TRUE
  )
})

test_that("the export menu's user option governs a text filter naming a user", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(scans_app_query = "ada")
    expect_gt(length(visible()), 0L)
    read <- function(path) paste(readLines(path), collapse = "\n")
    for (output_id in c(
      "scans_app_export_conversations",
      "scans_app_export_findings"
    )) {
      text <- read(output[[output_id]])
      expect_no_match(text, "ada", fixed = TRUE)
      expect_match(text, "Text filter (term omitted", fixed = TRUE)
    }

    session$setInputs(scans_app_export_users = TRUE)
    text <- read(output$scans_app_export_conversations)
    expect_match(text, "Text contains \"\"ada\"\"", fixed = TRUE)
    expect_match(text, "?scope=query=ada", fixed = TRUE)

    # A term that names no user stays in the scope and the links.
    session$setInputs(scans_app_export_users = FALSE, scans_app_query = "small")
    text <- read(output$scans_app_export_conversations)
    expect_match(text, "?scope=query=small", fixed = TRUE)
  })
})

test_that("the export menu's user option governs both CSV downloads", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(
      scans_app_hash = scans_app_scope_hash(
        "Cohort",
        scope = list(user = "ada")
      )
    )
    read <- function(path) paste(readLines(path), collapse = "\n")
    expect_no_match(
      read(output$scans_app_export_conversations),
      "ada",
      fixed = TRUE
    )
    expect_no_match(read(output$scans_app_export_findings), "ada", fixed = TRUE)

    session$setInputs(scans_app_export_users = TRUE)
    expect_match(
      read(output$scans_app_export_conversations),
      "User: ada",
      fixed = TRUE
    )
    expect_match(
      read(output$scans_app_export_findings),
      "?scope=user=ada",
      fixed = TRUE
    )
  })
})

test_that("the findings export has one row per finding in view", {
  fixture <- export_fixture()
  table <- export_findings(
    fixture$data,
    c("c", "d"),
    fixture$link,
    "Cohort",
    "s"
  )

  expect_identical(table$trajectory_id, c("c", "c", "c", "c", "d"))
  expect_setequal(
    table$scan[1:4],
    c("event_error", "unresolved_tool_call", "unmatched_tool_result")
  )
  expect_identical(table$severity[[5L]], "warning")
  expect_identical(table$event_ids[[5L]], "d-1 d-3")
  expect_match(table$explanation[[5L]], "requested repeatedly")
  expect_match(table$link[[5L]], "#Cohort/d?scope=", fixed = TRUE)
  expect_identical(nrow(export_findings(fixture$data, "b")), 0L)
})

test_that("the R snippet reproduces the read and scan settings without secrets", {
  config <- scans_app_scan_config(c("event_error", "turn_error"), 3L, 4L)
  placeholder <- export_r_code(NULL, config, c("a", "b"), "Cohort")
  expect_identical(
    placeholder,
    c(
      "library(scans)",
      "",
      "# Application: Cohort",
      "# Supply the TrajectoryBundle this app was started with, for example",
      "# from as_trajectory_otel() or read_investigation(file)$bundle.",
      "bundle <- NULL",
      "",
      "assessment <- assess_trajectory_scans(",
      "  bundle,",
      "  repeat_threshold = 3L,",
      "  loop_threshold = 4L,",
      "  scans = c(\n    \"event_error\", \"turn_error\"\n  )",
      ")",
      "",
      "# The 2 trajectories in view when this was copied.",
      "ids <- c(\n  \"a\", \"b\"\n)",
      "",
      "trajectories <- trajectory_info(bundle)",
      "trajectories[trajectories$trajectory_id %in% ids, ]",
      "findings <- assessment$findings",
      "findings[findings$trajectory_id %in% ids, ]"
    )
  )
  expect_no_error(parse(text = placeholder))

  recipe <- list(
    reader = "otel",
    guid = "11111111-1111-4111-8111-111111111111",
    n = 100L,
    from = as.POSIXct("2026-10-01", tz = "UTC"),
    to = NULL,
    jobs = FALSE
  )
  connect <- export_r_code(recipe, config, "a")
  text <- paste(connect, collapse = "\n")
  expect_match(
    text,
    "bundle <- read_connect_traces(\n  \"11111111-1111-4111-8111-111111111111\",\n  n = 100,\n  from = as.POSIXct(\"2026-10-01 00:00:00\", tz = \"UTC\"),\n  to = NULL,\n  jobs = FALSE\n) |>\n  as_trajectory_otel()",
    fixed = TRUE
  )
  expect_no_error(parse(text = connect))
  recipe$reader <- "commons"
  commons <- paste(export_r_code(recipe, config, "a"), collapse = "\n")
  expect_match(commons, "commons::trajectory_read(", fixed = TRUE)
  expect_match(commons, "as_trajectory_commons()", fixed = TRUE)
  expect_no_match(commons, "jobs =", fixed = TRUE)

  # No scans selected is not the same as every scan, and both survive.
  none <- export_r_code(NULL, scans_app_scan_config(character()), character())
  expect_true("  scans = character()" %in% none)
  expect_true("ids <- character()" %in% none)
  every <- export_r_code(NULL, scans_app_scan_config(), "a")
  expect_true("  scans = NULL" %in% every)
  spaced <- c("otel/a  b", paste(strrep("x", 30L), 1:4))
  expect_identical(eval(parse(text = export_r_strings(spaced))), spaced)
  expect_lte(
    max(nchar(strsplit(export_r_strings(spaced), "\n", fixed = TRUE)[[1L]])),
    77L
  )
})

test_that("the Connect loader's recipe names the read and no credentials", {
  withr::local_envvar(
    CONNECT_SERVER = "https://connect.example",
    CONNECT_API_KEY = "secret-key-value"
  )
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    read_connect_traces = function(source, n, from, to, jobs) {
      seen$source <- source
      structure(list(), read_info = list(n = n))
    },
    as_trajectory_otel = function(x, ...) {
      TrajectoryBundle(data.frame(), data.frame(), data.frame())
    }
  )
  guid <- "11111111-1111-4111-8111-111111111111"
  loaders <- scans_app_connect_loaders(
    c(App = guid),
    n = 50L,
    from = as.POSIXct("2026-10-01", tz = "UTC"),
    to = NULL,
    default_from = FALSE,
    default_to = FALSE,
    jobs = FALSE
  )
  loaded <- scans_app_load_source(list(label = "App", load = loaders$App))
  expect_identical(seen$source, guid)
  expect_identical(
    loaded$recipe,
    list(
      reader = "otel",
      guid = guid,
      n = 50L,
      from = as.POSIXct("2026-10-01", tz = "UTC"),
      to = NULL,
      jobs = FALSE
    )
  )
  code <- paste(
    export_r_code(loaded$recipe, scans_app_scan_config("event_error"), "a"),
    collapse = "\n"
  )
  expect_no_match(code, "secret-key-value|connect\\.example")
})

test_that("a trajectory copies as bounded Markdown that says it has content", {
  data <- cohort_fixture_data()
  text <- export_trajectory_markdown(data, 1L, "https://connect.test/#Cohort/a")

  expect_identical(text[[3L]], "- Trajectory: `a`")
  expect_true("- Link: https://connect.test/#Cohort/a" %in% text)
  expect_true("- 2 tool calls recorded, 1 failed" %in% text)
  expect_true(
    "- **Event errors** (error): The event reports an error or failed status. Evidence: tool_result, event 4." %in%
      text
  )
  expect_true(
    "This section contains conversation content as recorded." %in% text
  )
  expect_match(
    paste(text, collapse = "\n"),
    "**assistant:** Result (failed) [error: timeout]",
    fixed = TRUE
  )
  expect_no_match(paste(text, collapse = "\n"), "ada", fixed = TRUE)

  short <- export_trajectory_markdown(data, 1L, max_chars = 40L)
  expect_true(
    "Transcript cut at 40 characters; 1 of 4 entries shown." %in% short
  )
  expect_true(
    "No findings. This does not establish success." %in%
      export_trajectory_markdown(data, 2L)
  )
  unassessed <- export_trajectory_markdown(data, 5L)
  expect_true("No transcript was recorded." %in% unassessed)
  expect_true(
    "Not assessed: no selected scan could assess this trajectory." %in%
      unassessed
  )
  expect_no_match(unassessed, "No findings", fixed = TRUE)
})

test_that("a scoped link reopens the same cohort and exports follow it", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(
    Other = performance_fixture(),
    Cohort = cohort_fixture()
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    hash <- scans_app_scope_hash(
      "Cohort",
      "c",
      list(pattern = "event_error", model = "large", tool = "missing")
    )
    session$setInputs(scans_app_hash = hash)
    session$setInputs(scans_app_application = "Cohort")
    ids <- data()$info$trajectory_id
    expect_identical(facets(), list(model = "large"))
    expect_identical(pattern_filter(), "event_error")
    expect_identical(ids[visible()], "c")
    expect_identical(selected_trajectory_id(), "c")
    expect_identical(
      link()("c"),
      "http://mocksession:1234/mockpath#Cohort/c?scope=pattern=event_error&model=large"
    )

    text <- export_text()
    expect_true("- Scope: 1 of 5 conversations." %in% text)
    expect_true("- Filters: Finding: event error; Model: large." %in% text)
    session$setInputs(scans_app_export_code = 1L)
    expect_true("ids <- c(\n  \"c\"\n)" %in% export_text())
    expect_true("bundle <- NULL" %in% export_text())
    session$setInputs(scans_app_export_trajectory = 1L)
    expect_identical(export_text()[[3L]], "- Trajectory: `c`")

    session$setInputs(
      scans_app_hash = scans_app_scope_hash(
        "Cohort",
        scope = list(day = "2026-10-01")
      )
    )
    expect_identical(facets(), list(day = "2026-10-01"))
    expect_setequal(ids[visible()], c("a", "b"))
    expect_null(pattern_filter())
    session$setInputs(scans_app_hash = scans_app_hash("Cohort", "d"))
    expect_identical(selected_trajectory_id(), "d")
    expect_identical(facets(), list(day = "2026-10-01"))
  })
})

test_that("an application link selects that application and its overview", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  loads <- new.env(parent = emptyenv())
  loads$n <- 0L
  app <- scans_app(list(
    Other = performance_fixture(),
    Cohort = function() {
      loads$n <- loads$n + 1L
      cohort_fixture()
    }
  ))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(scans_app_hash = "Cohort/")
    expect_identical(pending_hash(), list(application = "Cohort"))
    expect_identical(loads$n, 0L)
    # The browser acknowledges the selector update the server asked for.
    session$setInputs(scans_app_application = "Cohort")
    expect_identical(application(), "Cohort")
    expect_identical(loads$n, 1L)
    expect_null(pending_hash())
    expect_length(visible(), 5L)
    expect_identical(facets(), list())

    session$setInputs(scans_app_hash = "Other/?scope=model=large")
    session$setInputs(scans_app_application = "Other")
    expect_identical(application(), "Other")
    expect_identical(facets(), list(model = "large"))
    expect_null(pending_hash())

    session$setInputs(scans_app_hash = "Missing/")
    expect_null(pending_hash())
    expect_identical(application(), "Other")
  })
})

test_that("the user option stays ticked when a filter re-renders the overview", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(scans_app_scans = scan_registry()$scan)
    session$setInputs(scans_app_hash = scans_app_scope_hash("Cohort"))
    expect_no_match(
      as.character(output$scans_app_performance$html),
      'id="scans_app_export_users"[^>]*checked'
    )
    session$setInputs(scans_app_export_users = TRUE)
    session$setInputs(
      scans_app_hash = scans_app_scope_hash(
        "Cohort",
        scope = list(user = "ada")
      )
    )
    expect_match(
      as.character(output$scans_app_performance$html),
      'id="scans_app_export_users"[^>]*checked'
    )
  })
})

test_that("the app's own link carries the full view and reopens the same cohort", {
  skip_if_not_installed("bslib", "0.11.0")
  skip_if_not_installed("shiny", "1.11.1")
  updates <- local_input_updates()
  app <- scans_app(list(Cohort = cohort_fixture()))

  shiny::testServer(app$serverFuncSource(), {
    investigation_ack_inputs(session)
    session$setInputs(scans_app_hash = "Cohort/")
    input_updates_ack(session, updates)
    unknown <- scans_app_filter_choices(data())$status[["Unknown"]]
    session$setInputs(
      scans_app_source = "otel",
      scans_app_status = unknown,
      scans_app_query = "large",
      scans_app_findings_only = TRUE
    )
    ids <- data()$info$trajectory_id
    expected <- ids[sort(visible())]
    expect_lt(length(expected), length(ids))
    hash <- link()("c")
    expect_match(hash, "source=otel", fixed = TRUE)
    expect_match(hash, "status_unknown=TRUE", fixed = TRUE)
    expect_match(hash, "query=large", fixed = TRUE)

    # Following the link from a differently filtered session, or from this
    # one, selects the same cohort.
    session$setInputs(
      scans_app_query = "missing",
      scans_app_findings_only = FALSE
    )
    expect_identical(report_follow_link(session, updates, hash), expected)
    session$setInputs(scans_app_hash = "Cohort/")
    expect_identical(report_follow_link(session, updates, hash), expected)
    expect_identical(view()$query, "large")
  })
})
