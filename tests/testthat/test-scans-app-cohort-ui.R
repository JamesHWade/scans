cohort_overview_html <- function(data, indices, scans = scan_registry()$scan) {
  scans_app_performance_ui(
    scans_app_performance_data(data, indices),
    "Cohort",
    "elapsed",
    scans
  )
}

test_that("tiles state each fact with its denominator", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  html <- as.character(cohort_overview_html(data, 1:5))

  expect_match(html, "of 5 captured", fixed = TRUE)
  expect_match(html, "of 4 assessed \u00b7 1 not assessed", fixed = TRUE)
  expect_match(html, "of 6 tool calls \u00b7 in 2 conversations", fixed = TRUE)
  expect_match(html, "timed for 3 of 5", fixed = TRUE)
  expect_match(html, "recorded for 3 of 5", fixed = TRUE)
  expect_match(html, "5 of 5</strong>\\s+conversations")
  expect_match(html, "1 conversation has no recorded start time", fixed = TRUE)

  unknown <- as.character(cohort_overview_html(data, 5L))
  expect_match(unknown, "timed for 0 of 1", fixed = TRUE)
  expect_match(unknown, "No tool calls recorded", fixed = TRUE)
  expect_match(unknown, "activity over time is unknown", fixed = TRUE)
  expect_no_match(unknown, "NaN|Inf|>0 s<")

  off <- scans_app_data(
    cohort_fixture(),
    scans_app_scan_config(scans = character())
  )
  expect_match(
    as.character(cohort_overview_html(off, 1:5, scans = character())),
    "Scans off \u00b7 no conversation was assessed",
    fixed = TRUE
  )
})

test_that("the activity chart has one labelled, focusable mark per bucket", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  performance <- scans_app_performance_data(data, 3:4, list(day = 1:5))
  chart <- htmltools::tagQuery(
    scans_app_activity_ui(performance$cohort$activity, "2026-10-03")
  )
  bars <- chart$find("button.scans-app-bar")$selectedTags()

  expect_length(bars, 3L)
  expect_identical(
    unname(vapply(bars, htmltools::tagGetAttribute, "", "data-scans-value")),
    c("2026-10-01", "2026-10-02", "2026-10-03")
  )
  expect_identical(
    unname(vapply(bars, htmltools::tagGetAttribute, "", "aria-pressed")),
    c("false", "false", "true")
  )
  expect_identical(
    htmltools::tagGetAttribute(bars[[3L]], "aria-label"),
    "Sat Oct 03: 2 conversations of 4. 1 error findings, 1 warnings only, 0 no findings, 0 not assessed."
  )
  expect_equal(chart$find("details tbody tr")$length(), 3L)
  expect_equal(chart$find("rect")$length(), 4L)

  one <- as.character(scans_app_activity_ui(
    cohort_activity(data, c("c", "d"), unit = "day")
  ))
  expect_match(
    one,
    "All 2 dated conversations started on Sat Oct 03 UTC: 1 with error findings, 1 with warnings only.",
    fixed = TRUE
  )
  expect_no_match(one, "<svg", fixed = TRUE)
})

test_that("tool and breakdown tables appear only when they inform", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  html <- as.character(cohort_overview_html(data, 1:5))
  expect_match(html, "<h2>Tools</h2>", fixed = TRUE)
  expect_match(html, "<h2>By model</h2>", fixed = TRUE)
  expect_match(html, "<h2>Most active users</h2>", fixed = TRUE)
  expect_match(
    html,
    "lookup: 1 of 4 calls failed, in 1 conversation",
    fixed = TRUE
  )
  expect_match(html, 'data-scans-facet="tool_failed"', fixed = TRUE)

  quiet <- as.character(cohort_overview_html(data, c(2L, 5L)))
  expect_no_match(quiet, "<h2>Tools</h2>", fixed = TRUE)
  expect_no_match(quiet, "Most active users", fixed = TRUE)
  expect_match(quiet, "<h2>By model</h2>", fixed = TRUE)

  one_model <- as.character(cohort_overview_html(data, 1:2))
  expect_match(one_model, "All 2 conversations used", fixed = TRUE)
  expect_no_match(one_model, 'data-scans-facet="model"', fixed = TRUE)

  anonymous <- as.character(cohort_overview_html(data, 5L))
  expect_no_match(anonymous, "By model|Most active users")
})

test_that("breakdown rows count findings over assessed conversations", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  html <- as.character(cohort_overview_html(data, 1:5))
  expect_match(
    html,
    "small: 2 of 5 conversations. 1 of 2 assessed conversations have findings.",
    fixed = TRUE
  )
  expect_match(html, "<small>of 2 assessed</small>", fixed = TRUE)
  # The one conversation with no recorded model could not be assessed.
  expect_match(
    html,
    "Unknown: 1 of 5 conversations. No conversation was assessed.",
    fixed = TRUE
  )
  expect_match(
    html,
    '<td>\\s*<span class="scans-app-unknown">Not assessed</span>\\s*</td>'
  )
})

test_that("a conversation no scan assessed is not shown with zero findings", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  text <- function(ui) gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(ui)))

  expect_match(text(scans_app_overview_ui(data, 5L)), "Findings Not assessed")
  expect_match(text(scans_app_overview_ui(data, 2L)), "Findings 0 ")
  expect_match(
    text(scans_app_evidence_ui(data, 5L)),
    "No selected scan could assess this conversation.",
    fixed = TRUE
  )
  expect_match(
    text(scans_app_evidence_ui(data, 2L)),
    "No findings from the selected scans.",
    fixed = TRUE
  )
  table <- scans_app_performance_table(
    scans_app_performance_data(data, c(2L, 5L))$trajectories,
    "findings"
  )
  expect_match(as.character(table), "<td>Not assessed</td>", fixed = TRUE)
  expect_match(as.character(table), "<td>0</td>", fixed = TRUE)
})

test_that("copy names conversations only for conversation sources", {
  skip_if_not_installed("htmltools")
  expect_identical(scans_app_noun(c("otel", "commons"))$many, "conversations")
  expect_identical(scans_app_noun(c("otel", "vitals"))$many, "trajectories")
  expect_identical(scans_app_noun(NA_character_)$one, "trajectory")

  data <- scans_app_data(performance_fixture())
  html <- as.character(cohort_overview_html(data, 1:3))
  expect_match(html, "3 of 3</strong>\\s+trajectories")
  expect_no_match(gsub("<[^>]+>", "", html), "onversation")
})

test_that("a faceted table keeps its sibling rows for comparison", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  performance <- scans_app_performance_data(
    data,
    c(1L, 4L),
    list(tool = 1:5, model = 1:5)
  )
  expect_identical(performance$cohort$tools$tool, c("refund", "lookup"))
  expect_identical(performance$cohort$models$n, c(2L, 2L, 1L))
  expect_identical(performance$cohort$users$n, c(1L, 1L))
  expect_identical(performance$cohort$headline$tool_calls, 4L)
})

test_that("empty and loading states say what is specific about them", {
  skip_if_not_installed("htmltools")
  data <- cohort_fixture_data()
  text <- function(...) {
    gsub("\\s+", " ", gsub("<[^>]+>", "", as.character(...)))
  }

  clean <- text(cohort_overview_html(data, 2L))
  expect_match(clean, "No findings in 1 assessed conversation.", fixed = TRUE)
  expect_no_match(clean, "could not be assessed", fixed = TRUE)
  expect_no_match(clean, "more than one pattern", fixed = TRUE)
  expect_match(
    text(cohort_overview_html(data, c(2L, 5L))),
    "No findings in 1 assessed conversation. 1 conversation could not be assessed; see Evidence coverage.",
    fixed = TRUE
  )
  expect_match(
    text(cohort_overview_html(data, integer())),
    "No conversations match these filters. Remove a filter to widen the cohort.",
    fixed = TRUE
  )
  none <- scans_app_data(TrajectoryBundle(
    data.frame(),
    data.frame(),
    data.frame()
  ))
  expect_match(
    text(cohort_overview_html(none, integer())),
    "This source returned no trajectories for the window read.",
    fixed = TRUE
  )

  loader <- function() cohort_fixture()
  attr(loader, "scans_app_window") <- "the last 7 days"
  banner <- as.character(scans_app_loading_banner_ui(scans_app_sources(list(
    Eager = cohort_fixture(),
    Lazy = loader
  ))))
  expect_match(
    banner,
    "scans-app-loading-text\">Preparing Eager\u2026",
    fixed = TRUE
  )
  expect_match(
    banner,
    "data-scans-loading-for=\"Lazy\">Reading traces for Lazy (the last 7 days)\u2026",
    fixed = TRUE
  )
  loaders <- scans_app_connect_loaders(
    c(App = "11111111-1111-4111-8111-111111111111"),
    n = 10L,
    from = NULL,
    to = NULL,
    default_from = TRUE,
    default_to = TRUE
  )
  expect_identical(attr(loaders$App, "scans_app_window"), "the last 7 days")
})

test_that("counts in the findings pane and header agree in number", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("bslib", "0.11.0")
  data <- cohort_fixture_data()
  text <- function(index) {
    gsub(
      "\\s+",
      " ",
      gsub(
        "<[^>]+>",
        " ",
        as.character(
          scans_app_evidence_ui(data, index)
        )
      )
    )
  }
  expect_match(text(4L), "1 finding describes this conversation.", fixed = TRUE)
  expect_match(
    text(1L),
    "1 tool call recorded an error. 2 findings describe this conversation.",
    fixed = TRUE
  )
  expect_match(
    text(3L),
    "2 tool calls recorded errors. 4 findings describe this conversation.",
    fixed = TRUE
  )
  expect_match(
    as.character(scans_app_header_ui(data, 1L)),
    "1 tool error\\s"
  )
  ui <- as.character(scans_app_ui(scans_app_sources(cohort_fixture())))
  expect_match(ui, 'placeholder="Search conversations"', fixed = TRUE)
  expect_match(
    as.character(scans_app_ui(scans_app_sources(performance_fixture()))),
    'placeholder="Search trajectories"',
    fixed = TRUE
  )
})

test_that("pattern rows state affected over the trajectories the scan assessed", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  html <- as.character(cohort_overview_html(cohort_fixture_data(), 1:5))
  expect_match(html, "2 of 3 assessed", fixed = TRUE)
  expect_match(html, ": 2 of 3 assessed conversations.", fixed = TRUE)
})

test_that("the export menu keeps the user option ticked across a re-render", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  html <- as.character(scans_app_export_menu_ui(users = TRUE))
  expect_match(html, 'id="scans_app_export_users"[^>]*checked')
  off <- as.character(scans_app_export_menu_ui())
  expect_no_match(off, 'id="scans_app_export_users"[^>]*checked')
})
