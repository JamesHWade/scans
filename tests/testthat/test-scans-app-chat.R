test_that("Ask is optional and creates no shared conversation state", {
  skip_if_not_installed("shinychat")
  calls <- 0L
  shared <- scans_test_chat()
  shared$set_turns(list(ellmer::UserTurn(list(ellmer::ContentText(
    "Private history"
  )))))
  factory <- function() {
    calls <<- calls + 1L
    shared
  }
  app <- scans_app(scans_support_bundle(), chat_factory = factory)
  expect_equal(calls, 0L)
  first <- scans_app_chat_client(factory)
  second <- scans_app_chat_client(factory)
  first$set_system_prompt("First session only")
  first$set_turns(list(ellmer::UserTurn(list(ellmer::ContentText(
    "New question"
  )))))
  expect_length(second$get_turns(), 0L)
  expect_identical(second$get_system_prompt(), "")
  expect_length(shared$get_turns(), 1L)
  expect_length(shared$get_tools(), 0L)
  plain <- scans_app_ui(scans_app_sources(scans_support_bundle()))
  expect_equal(
    htmltools::tagQuery(plain)$find('#scans_app_ask_scope')$length(),
    0L
  )
  expect_error(
    scans_app(scans_support_bundle(), chat_factory = shared),
    class = "scans_error_app_chat"
  )
})

test_that("native streaming pins tools and evidence while navigation changes", {
  skip_if_not_installed("shinychat")
  control <- new.env(parent = emptyenv())
  control$hold <- TRUE
  app <- scans_app(scans_support_bundle(), chat_factory = function() {
    scans_test_chat(control)
  })
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_scans = scan_registry()$scan,
      scans_app_view = "trajectory",
      scans_app_ask_scope = "trajectory",
      scans_app_inspect = "otel/retry"
    )
    session$setInputs(scans_app_ask_user_input = list("What stands out?"))
    scans_test_drain(session, function() !is.null(control$resume))
    expect_identical(chat_state$module$status(), "streaming")
    expect_identical(chat_state$pinned()$context$ids, "otel/retry")
    session$setInputs(
      scans_app_inspect = "otel/follow-up",
      scans_app_query = "return"
    )
    expect_identical(chat_state$pinned()$context$ids, "otel/retry")
    pinned_tools <- chat_state$module$client$get_tools()
    expect_error(
      pinned_tools$scans_read_trajectory("otel/follow-up"),
      class = "scans_error_tools_scope"
    )
    control$hold <- FALSE
    control$resume(NULL)
    scans_test_drain(session, function() {
      !is.null(chat_state$module$last_turn())
    })
    expect_null(chat_state$module$last_error())
    expect_identical(chat_state$module$status(), "idle")
    expect_length(ls(chat_state$references), 3L)
    references <- as.list(chat_state$references)
    expect_equal(
      vapply(references, function(x) x$reference$trajectory_id, character(1)),
      stats::setNames(rep("otel/retry", 3L), names(references))
    )
    key <- names(references)[[1L]]
    session$setInputs(scans_app_chat_evidence = key)
    expect_identical(selected_trajectory_id(), "otel/retry")
  })
})

test_that("native cancellation stops the pending fixture stream", {
  skip_if_not_installed("shinychat")
  control <- new.env(parent = emptyenv())
  control$hold <- TRUE
  app <- scans_app(scans_support_bundle(), chat_factory = function() {
    scans_test_chat(control)
  })
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_view = "application",
      scans_app_ask_scope = "filtered"
    )
    session$setInputs(
      scans_app_ask_user_input = list("Inspect these trajectories")
    )
    scans_test_drain(session, function() !is.null(control$resume))
    active_client <- control$instances[[length(control$instances)]]
    session$setInputs(scans_app_ask_cancel = 1L)
    expect_true(active_client$controller$cancelled)
    control$hold <- FALSE
    control$resume(NULL)
    scans_test_drain(session, function() {
      identical(chat_state$module$status(), "idle")
    })
    expect_length(ls(chat_state$references), 0L)
    expect_null(chat_state$module$last_error())
  })
})

test_that("later questions change scope without importing earlier model context", {
  skip_if_not_installed("shinychat")
  app <- scans_app(scans_support_bundle(), chat_factory = scans_test_chat)
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_scans = scan_registry()$scan,
      scans_app_view = "trajectory",
      scans_app_ask_scope = "trajectory",
      scans_app_inspect = "otel/retry"
    )
    session$setInputs(scans_app_ask_user_input = list("First question"))
    scans_test_drain(session, function() {
      !is.null(chat_state$module$last_turn())
    })
    first_keys <- ls(chat_state$references)
    session$setInputs(scans_app_inspect = "otel/follow-up")
    session$setInputs(scans_app_ask_user_input = list("Second question"))
    scans_test_drain(session, function() {
      identical(chat_state$module$status(), "idle")
    })
    expect_identical(chat_state$pinned()$context$ids, "otel/follow-up")
    expect_length(chat_state$module$client$get_turns(), 2L)
    expect_equal(ls(chat_state$references), first_keys)
    expect_error(
      chat_state$module$client$get_tools()$scans_read_trajectory("otel/retry"),
      class = "scans_error_tools_scope"
    )
  })
})

test_that("old answer references restore retained evidence after a source change", {
  skip_if_not_installed("shinychat")
  app <- scans_app(
    list(
      "Support" = scans_support_bundle(),
      "Other" = trajectory_fixture("simple_exchange")
    ),
    chat_factory = scans_test_chat
  )
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(
      scans_app_scans = scan_registry()$scan,
      scans_app_view = "trajectory",
      scans_app_ask_scope = "trajectory",
      scans_app_inspect = "otel/retry"
    )
    session$setInputs(scans_app_ask_user_input = list("First question"))
    scans_test_drain(session, function() {
      !is.null(chat_state$module$last_turn())
    })
    key <- ls(chat_state$references)[[1L]]
    session$setInputs(scans_app_application = "Other")
    expect_identical(application(), "Other")
    session$setInputs(scans_app_chat_evidence = key)
    expect_identical(application(), "Support")
    expect_identical(selected_trajectory_id(), "otel/retry")
    expect_true(viewing_chat_snapshot())
    expect_match(output$scans_app_retained_notice$html, "retained snapshot")
    session$setInputs(scans_app_return_source = 1L)
    expect_identical(application(), "Other")
    expect_false(viewing_chat_snapshot())
  })
})
