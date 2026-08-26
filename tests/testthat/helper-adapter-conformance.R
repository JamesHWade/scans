sys.source(
  testthat::test_path("fixtures", "trajectory-fixtures.R"),
  envir = environment()
)

trajectory_fixture <- function(name) {
  fixtures <- trajectory_fixtures()
  if (!name %in% names(fixtures)) {
    stop("Unknown trajectory fixture: ", name, call. = FALSE)
  }
  fixtures[[name]]
}

expect_adapter_conforms <- function(source, expected = NULL, adapter, ...) {
  actual <- adapter(source, ...)
  repeated <- adapter(source, ...)

  testthat::expect_s7_class(actual, TrajectoryBundle)
  testthat::expect_no_error(S7::validate(actual))
  testthat::expect_identical(S7::props(repeated), S7::props(actual))

  actual_properties <- S7::props(actual)
  testthat::expect_identical(actual_properties$schema_version, 1L)

  if (!is.null(expected)) {
    expected_properties <- S7::props(as_trajectory(expected))
    for (table in names(trajectory_table_schemas())) {
      testthat::expect_identical(
        order_fixture_table(actual_properties[[table]], table),
        order_fixture_table(expected_properties[[table]], table),
        info = paste("adapter table:", table)
      )
    }
  }

  restored <- unserialize(serialize(actual, NULL))
  testthat::expect_identical(S7::props(restored), S7::props(actual))
  invisible(actual)
}

expect_adapter_rejects <- function(source, adapter, class, ...) {
  condition <- rlang::catch_cnd(adapter(source, ...))
  testthat::expect_s3_class(condition, class)
  testthat::expect_s3_class(condition, "scans_error")
  invisible(condition)
}

order_fixture_table <- function(x, table) {
  keys <- switch(
    table,
    trajectories = "trajectory_id",
    turns = c("trajectory_id", "turn_index", "turn_id"),
    events = c("trajectory_id", "event_index", "event_id"),
    evaluations = c("trajectory_id", "evaluation_id"),
    losses = c(
      "trajectory_id",
      "turn_id",
      "event_id",
      "field",
      "reason"
    )
  )
  if (nrow(x) < 2L) {
    return(x)
  }

  order_arguments <- c(unname(x[keys]), list(na.last = TRUE, method = "radix"))
  x[do.call(order, order_arguments), , drop = FALSE]
}

fixture_adapter <- function(source) {
  do.call(TrajectoryBundle, source)
}

fixture_column_has_value <- function(x) {
  if (is.list(x)) {
    return(any(vapply(x, \(value) length(value) > 0L, logical(1))))
  }
  if (is.character(x)) {
    return(any(!is.na(x) & nzchar(x)))
  }
  !all(is.na(x))
}
