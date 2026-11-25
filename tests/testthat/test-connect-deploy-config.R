connect_config_source <- function() {
  path <- test_path("..", "..", "deploy", "connect", "config.R")
  skip_if_not(file.exists(path), "deploy/ is not part of the built package")
  env <- new.env()
  sys.source(path, envir = env)
  env
}

connect_config <- function(...) {
  env <- connect_config_source()
  vars <- utils::modifyList(
    list(SCANS_CONNECT_SOURCES = "App=11111111-1111-4111-8111-111111111111"),
    list(...)
  )
  env$scans_connect_config(vars)
}

test_that("the Connect app reads its Vars with documented defaults", {
  config <- connect_config()
  expect_identical(
    config$sources,
    c(App = "11111111-1111-4111-8111-111111111111")
  )
  expect_identical(config$n, 100L)
  expect_identical(config$days, 7L)
  expect_identical(config$reader, "otel")
  expect_true(config$jobs)
  expect_false(config$reviews)
  expect_false(config$investigations)
  expect_null(config$annotations_dir)
  expect_null(config$annotation_labels)

  set <- connect_config(
    SCANS_CONNECT_SOURCES = "A = 11111111-1111-4111-8111-111111111111; B=22222222-2222-4222-8222-222222222222",
    SCANS_CONNECT_N = "all",
    SCANS_CONNECT_DAYS = "14",
    SCANS_CONNECT_READER = "commons",
    SCANS_CONNECT_JOBS = "FALSE",
    SCANS_REVIEWS = "true",
    SCANS_INVESTIGATIONS = "True",
    SCANS_ANNOTATIONS_DIR = "/data/reviews",
    SCANS_ANNOTATION_LABELS = "looks right, tool failure"
  )
  expect_named(set$sources, c("A", "B"))
  expect_null(set$n)
  expect_identical(set$days, 14L)
  expect_identical(set$reader, "commons")
  expect_false(set$jobs)
  expect_true(set$reviews)
  expect_true(set$investigations)
  expect_identical(set$annotations_dir, "/data/reviews")
  expect_identical(set$annotation_labels, c("looks right", "tool failure"))
})

connect_optional_vars <- c(
  "SCANS_CONNECT_N",
  "SCANS_CONNECT_DAYS",
  "SCANS_CONNECT_READER",
  "SCANS_CONNECT_JOBS",
  "SCANS_ANNOTATIONS_DIR",
  "SCANS_ANNOTATION_LABELS",
  "SCANS_REVIEWS",
  "SCANS_INVESTIGATIONS"
)

expect_connect_defaults <- function(config) {
  expect_identical(
    config$sources,
    c(App = "11111111-1111-4111-8111-111111111111")
  )
  expect_identical(config$n, 100L)
  expect_identical(config$days, 7L)
  expect_identical(config$reader, "otel")
  expect_true(config$jobs)
  expect_false(config$reviews)
  expect_false(config$investigations)
  expect_null(config$annotations_dir)
  expect_null(config$annotation_labels)
}

test_that("the Connect app starts with only the required Var set", {
  env <- connect_config_source()
  sources <- "App=11111111-1111-4111-8111-111111111111"

  # Sys.getenv() returns a named character vector, where `[[` on a missing
  # name is an error rather than NULL.
  expect_identical(env$config_get("SCANS_ABSENT", c(OTHER = "x")), "")
  expect_identical(env$config_get("SCANS_ABSENT", character()), "")
  expect_identical(env$config_get("OTHER", c(OTHER = " x ")), "x")
  expect_connect_defaults(
    env$scans_connect_config(c(SCANS_CONNECT_SOURCES = sources))
  )

  unset <- stats::setNames(
    rep(NA_character_, length(connect_optional_vars)),
    connect_optional_vars
  )
  withr::local_envvar(c(unset, SCANS_CONNECT_SOURCES = sources))
  expect_connect_defaults(env$scans_connect_config())

  # Connect shows a Var cleared in the dashboard as empty, not as absent.
  empty <- stats::setNames(
    rep(c("", "  "), length.out = length(connect_optional_vars)),
    connect_optional_vars
  )
  withr::local_envvar(empty)
  expect_connect_defaults(env$scans_connect_config())

  withr::local_envvar(SCANS_CONNECT_SOURCES = NA)
  expect_error(env$scans_connect_config(), "SCANS_CONNECT_SOURCES must be set")
})

test_that("the Connect app rejects Vars it cannot parse, naming each", {
  expect_error(
    connect_config(SCANS_CONNECT_SOURCES = ""),
    "SCANS_CONNECT_SOURCES"
  )
  expect_error(
    connect_config(SCANS_CONNECT_SOURCES = "App=not-a-guid"),
    "SCANS_CONNECT_SOURCES has a malformed entry"
  )
  expect_error(connect_config(SCANS_CONNECT_N = "many"), "SCANS_CONNECT_N")
  expect_error(connect_config(SCANS_CONNECT_N = "0"), "SCANS_CONNECT_N")
  expect_error(connect_config(SCANS_CONNECT_DAYS = "1.5"), "SCANS_CONNECT_DAYS")
  expect_error(connect_config(SCANS_CONNECT_DAYS = "-3"), "SCANS_CONNECT_DAYS")
  expect_error(
    connect_config(SCANS_CONNECT_READER = "native"),
    "SCANS_CONNECT_READER must be otel or commons"
  )
  expect_error(connect_config(SCANS_CONNECT_JOBS = "yes"), "SCANS_CONNECT_JOBS")
  expect_error(connect_config(SCANS_REVIEWS = "1"), "SCANS_REVIEWS")
  expect_error(
    connect_config(SCANS_INVESTIGATIONS = "on"),
    "SCANS_INVESTIGATIONS"
  )
  expect_error(
    connect_config(
      SCANS_ANNOTATIONS_DIR = "/data",
      SCANS_ANNOTATION_LABELS = "a,,b"
    ),
    "SCANS_ANNOTATION_LABELS"
  )
  expect_error(
    connect_config(SCANS_ANNOTATION_LABELS = "a,b"),
    "needs SCANS_ANNOTATIONS_DIR"
  )
})

test_that("the Connect window follows `days` and the commons reader gets a source", {
  loaders <- scans_app_connect_loaders(
    c(App = "11111111-1111-4111-8111-111111111111"),
    n = 10L,
    from = NULL,
    to = NULL,
    default_from = TRUE,
    default_to = TRUE,
    days = 14
  )
  expect_identical(attr(loaders$App, "scans_app_window"), "the last 14 days")
  expect_error(scans_app_connect(c(App = "x"), days = 0))
  withr::local_envvar(CONNECT_SERVER = "https://connect.example/")
  expect_identical(
    scans_app_connect_source_uri("abc"),
    "https://connect.example/content/abc/"
  )
  withr::local_envvar(CONNECT_SERVER = "")
  expect_null(scans_app_connect_source_uri("abc"))
})
