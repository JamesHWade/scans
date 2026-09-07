test_that("opened investigations retain saved analysis until explicitly rescanned", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    scans = "event_error",
    repeat_threshold = 4L,
    view = list(selected_trajectory_id = "otel/retry", tab = "trajectory")
  )
  app <- scans_app(saved)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_query = "", scans_app_sort = "newest")
    expect_identical(data()$findings, saved$analysis$findings)
    expect_identical(scan_config(), saved$settings)
    session$setInputs(
      scans_app_scans = character(),
      scans_app_repeat_threshold = 6L
    )
    expect_identical(data()$assessments, saved$analysis$assessments)
    expect_identical(scan_config(), saved$settings)
    session$setInputs(scans_app_rescan_investigation = 1L)
    expect_identical(nrow(data()$assessments), 0L)
    expect_identical(scan_config()$repeat_threshold, 6L)
    session$setInputs(scans_app_save_investigation = 1L)
    revised <- pending_investigation()
    expect_s3_class(revised, "scans_investigation")
    expect_identical(
      revised$manifest$parent_revision_id,
      saved$manifest$revision_id
    )
    expect_identical(revised$settings$scans, character())
  })
})

test_that("saving freezes the visible selection and opening is session local", {
  bundle <- investigation_bundle_fixture()
  app <- scans_app(list("Live application" = bundle), investigations = TRUE)
  path <- tempfile(fileext = ".json")
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(
      scans_app_query = "retry",
      scans_app_scans = scan_registry()$scan
    )
    expect_length(visible(), 1L)
    session$setInputs(scans_app_save_investigation = 1L)
    saved <- pending_investigation()
    expect_identical(trajectory_info(saved$bundle)$trajectory_id, "otel/retry")
    expect_identical(saved$view$query, "retry")
    session$setInputs(scans_app_query = "parallel")
    expect_identical(pending_investigation(), saved)
    downloaded <- output$scans_app_download_investigation
    expect_identical(read_investigation(downloaded), saved)
    write_investigation(saved, path)
    session$setInputs(
      scans_app_open_investigation = data.frame(datapath = path)
    )
    session$setInputs(scans_app_query = "retry", scans_app_sort = "newest")
    expect_identical(active()$investigation, saved)
    expect_identical(nrow(data()$info), 1L)
    bad <- tempfile()
    writeLines("not JSON", bad)
    session$setInputs(scans_app_open_investigation = data.frame(datapath = bad))
    expect_identical(active()$investigation, saved)
    session$setInputs(scans_app_close_investigation = 1L)
    expect_null(active()$investigation)
    expect_identical(nrow(data()$info), 4L)
  })
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    expect_null(opened_investigation())
    expect_identical(nrow(data()$info), 4L)
  })
})

test_that("saved annotation membership restores without including reviewer text", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    view = list(annotated_only = TRUE, annotation_ids = "otel/retry")
  )
  app <- scans_app(list("Saved" = function() saved))
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_annotated_only = TRUE, scans_app_query = "")
    expect_identical(data()$info$trajectory_id[visible()], "otel/retry")
    expect_match(
      output$scans_app_snapshot_annotation_filter$html,
      "Annotated at save"
    )
    session$setInputs(scans_app_annotated_only = FALSE)
    expect_length(visible(), 4L)
  })
})

test_that("app startup aligns default uploads and reads and respects configured limits", {
  withr::local_options(list(shiny.maxRequestSize = NULL))
  app <- scans_app(investigation_bundle_fixture(), investigations = TRUE)
  unregister <- app$onStart()
  on.exit(unregister(), add = TRUE)
  expect_identical(getOption("shiny.maxRequestSize"), 50 * 1024^2)
  expect_identical(scans_app_investigation_max_bytes(), 50 * 1024^2)
  expect_match(
    as.character(scans_app_investigation_ui()),
    "Upload limit: 50 MiB"
  )

  options(shiny.maxRequestSize = 1024^2)
  expect_null(app$onStart())
  expect_identical(scans_app_investigation_max_bytes(), 1024^2)
  expect_match(
    as.character(scans_app_investigation_ui()),
    "Upload limit: 1 MiB"
  )
})

test_that("app uploads can reopen investigations above Shiny's original default", {
  withr::local_options(list(shiny.maxRequestSize = NULL))
  bundle <- investigation_bundle_fixture()
  saved <- investigation_snapshot(
    bundle,
    source = list(note = strrep("x", 6 * 1024^2))
  )
  path <- tempfile()
  write_investigation(saved, path)
  expect_gt(file.info(path)$size, 5 * 1024^2)
  app <- scans_app(bundle, investigations = TRUE)
  unregister <- app$onStart()
  on.exit(unregister(), add = TRUE)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    session$setInputs(
      scans_app_open_investigation = data.frame(datapath = path)
    )
    investigation_ack_inputs(session)
    expect_identical(active()$investigation, saved)
  })
})

test_that("removed scanners remain visible until an explicit current rescan", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    scans = "event_error",
    view = list(pattern = "event_error")
  )
  registry <- scan_registry()
  local_mocked_bindings(scan_registry = function() {
    registry[registry$scan != "event_error", ]
  })
  app <- scans_app(saved)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    expect_identical(data()$findings, saved$analysis$findings)
    expect_identical(pattern_filter(), "event_error")
    expect_match(output$scans_app_scan_summary, "1 saved scan")
    expect_match(
      output$scans_app_investigation_status$html,
      "unavailable in this installation"
    )
    session$setInputs(scans_app_save_investigation = 1L)
    expect_identical(pending_investigation()$settings$scans, "event_error")
    session$setInputs(scans_app_scans = "trajectory_error")
    expect_identical(scan_config()$scans, "event_error")
    session$setInputs(scans_app_rescan_investigation = 1L)
    expect_identical(scan_config()$scans, "trajectory_error")
    expect_null(pattern_filter())
    expect_identical(nrow(data()$findings), 0L)
  })
})


test_that("opened investigations use live annotation membership when available", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    application = "Deployment",
    view = list(annotation_ids = "otel/retry")
  )
  store <- scans_annotations(withr::local_tempfile(fileext = ".jsonl"))
  app <- scans_app(saved, annotations = store)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_annotated_only = TRUE)
    expect_length(visible(), 0L)
    store$append(
      "Deployment",
      "otel/parallel",
      label = NA_character_,
      note = "New review"
    )
    annotation_revision(annotation_revision() + 1L)
    session$flushReact()
    expect_identical(data()$info$trajectory_id[visible()], "otel/parallel")
    session$setInputs(scans_app_save_investigation = 1L)
    expect_identical(
      pending_investigation()$view$annotation_ids,
      "otel/parallel"
    )
  })
})

test_that("optional source data is not assumed to be Connect read metadata", {
  saved <- investigation_snapshot(
    investigation_bundle_fixture(),
    source = list(read_info = "external capture")
  )
  app <- scans_app(saved)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    expect_identical(data()$findings, saved$analysis$findings)
    expect_match(output$scans_app_load_info$html, "Loaded")
    expect_identical(
      active()$investigation$manifest$source$read_info,
      "external capture"
    )
  })
})


test_that("closing an upload stops applying a hidden saved annotation filter", {
  bundle <- investigation_bundle_fixture()
  saved <- investigation_snapshot(
    bundle,
    view = list(annotated_only = TRUE, annotation_ids = "otel/retry")
  )
  path <- tempfile()
  write_investigation(saved, path)
  app <- scans_app(bundle, investigations = TRUE)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(
      scans_app_open_investigation = data.frame(datapath = path)
    )
    investigation_ack_inputs(session)
    session$setInputs(scans_app_annotated_only = TRUE)
    expect_identical(data()$info$trajectory_id[visible()], "otel/retry")
    session$setInputs(scans_app_close_investigation = 1L)
    expect_null(active()$investigation)
    expect_null(output$scans_app_snapshot_annotation_filter)
    expect_length(visible(), 4L)
    session$setInputs(scans_app_save_investigation = 1L)
    expect_false(pending_investigation()$view$annotated_only)
    expect_identical(nrow(trajectory_info(pending_investigation()$bundle)), 4L)
  })
})

test_that("regular apps require opt-in before accepting investigation actions", {
  withr::local_options(sass.cache = FALSE)
  bundle <- investigation_bundle_fixture()
  sources <- scans_app_sources(bundle)
  expect_no_match(as.character(scans_app_ui(sources)), "Save or open")
  expect_match(
    as.character(scans_app_ui(sources, investigations = TRUE)),
    "Save or open"
  )
  app <- scans_app(bundle)
  expect_null(app$onStart)
  saved <- investigation_snapshot(bundle)
  path <- tempfile()
  write_investigation(saved, path)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_save_investigation = 1L)
    expect_null(pending_investigation())
    session$setInputs(
      scans_app_open_investigation = data.frame(datapath = path)
    )
    expect_null(opened_investigation())
    expect_length(visible(), 4L)
  })
  expect_error(scans_app(bundle, investigations = NA), class = "rlang_error")
})

test_that("saved investigation controls can be explicitly disabled", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  app <- scans_app(saved, investigations = FALSE)
  expect_null(app$onStart)
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    investigation_ack_inputs(session)
    session$setInputs(scans_app_rescan_investigation = 1L)
    expect_false(rescan_investigation())
    session$setInputs(scans_app_save_investigation = 1L)
    expect_null(pending_investigation())
    expect_identical(data()$findings, saved$analysis$findings)
  })
})

test_that("Connect apps forward the investigation opt-in without loading traces", {
  local_mocked_bindings(scans_app = function(x, annotations, investigations) {
    investigations
  })
  source <- c(Deployment = "11111111-1111-4111-8111-111111111111")
  expect_false(scans_app_connect(source))
  expect_true(scans_app_connect(source, investigations = TRUE))
  expect_error(
    scans_app_connect(source, investigations = NA),
    class = "rlang_error"
  )
})
