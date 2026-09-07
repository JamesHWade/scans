test_that("selected application mappings preserve review history without other content", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  case <- review_decision(
    review_case(saved, "otel/retry"),
    "confirmed",
    "A",
    "Old output failed",
    "Bound retries"
  )
  examples <- review_examples(
    list(case),
    "application",
    "order-1",
    "development",
    inputs = list(list(question = "Track order")),
    targets = list(list(answer = "Service unavailable"))
  )
  dataset <- review_dataset(examples, "dsprrr", "development")
  expect_named(dataset, c("question", "answer"))
  expect_identical(dataset$question, "Track order")
  expect_identical(
    attr(dataset, "scans_provenance")$expected_behavior,
    "Bound retries"
  )
  vitals <- review_dataset(examples, "vitals", "development")
  expect_identical(vitals$input[[1L]], list(question = "Track order"))
  expect_identical(vitals$target[[1L]], list(answer = "Service unavailable"))
  path <- tempfile(fileext = ".json")
  write_review_examples(examples, path)
  expect_identical(read_review_examples(path), examples)
  expect_no_match(
    paste(readLines(path), collapse = ""),
    "otel/parallel",
    fixed = TRUE
  )
  expect_error(
    read_review_examples(path, max_bytes = 1),
    class = "scans_error_review"
  )
  expect_error(
    write_review_examples(examples, path),
    class = "scans_error_review"
  )
  writeLines("old contents", path)
  write_review_examples(examples, path, overwrite = TRUE)
  expect_identical(read_review_examples(path), examples)
  examples$inputs[[1L]]$question <- "edited"
  expect_error(
    review_dataset(examples, "dsprrr", "development"),
    class = "scans_error_review"
  )
})

test_that("groups and source identities cannot leak across splits or versions", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  a <- review_case(saved, "otel/retry", scan = "suspicious_tool_loop")
  b <- review_case(saved, "otel/parallel", scan = "suspicious_tool_loop")
  p <- list(operator = "eq", value = 0L)
  expect_error(
    review_examples(
      list(a, b),
      "scanner",
      c("task", "task"),
      c("development", "test"),
      predicates = list(p, p)
    ),
    class = "scans_error_review"
  )
  first <- review_examples(
    list(a),
    "scanner",
    "task",
    "development",
    predicates = list(p)
  )
  expect_error(
    review_examples(
      list(b),
      "scanner",
      "task",
      "test",
      predicates = list(p),
      previous = first
    ),
    class = "scans_error_review"
  )
  expect_error(
    review_examples(
      list(a),
      "scanner",
      "renamed",
      "test",
      predicates = list(p),
      previous = first
    ),
    class = "scans_error_review"
  )
  second <- review_examples(
    list(b),
    "scanner",
    "other",
    "test",
    predicates = list(p),
    previous = first
  )
  expect_identical(second$parent_version_id, first$version_id)
  path <- tempfile()
  write_review_examples(second, path)
  expect_identical(read_review_examples(path), second)
})

test_that("uncertainty never becomes a training target or a negative scanner label", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  case <- review_decision(
    review_case(saved, "otel/retry", scan = "suspicious_tool_loop"),
    "uncertain",
    "A",
    "Need more evidence"
  )
  examples <- review_examples(
    list(case),
    "application",
    "task",
    "development",
    inputs = list(list(question = "Track order")),
    targets = list(list(answer = "Unavailable"))
  )
  expect_error(
    review_dataset(examples, "dsprrr", "development"),
    class = "scans_error_review"
  )
  scans <- review_examples(
    list(case),
    "scanner",
    "task",
    "development",
    predicates = list(list(operator = "gte", value = 1L))
  )
  result <- validate_review_examples(scans, "development")
  expect_identical(result$passed, NA)
  expect_equal(result$count, 1L)
  case <- review_decision(
    case,
    "confirmed",
    "A",
    "Verified",
    "Bound retries",
    supersedes = review_status(case)$decision_ids
  )
  resolved <- review_examples(
    list(case),
    "scanner",
    "task",
    "development",
    predicates = list(list(operator = "gte", value = 1L)),
    previous = scans
  )
  expect_true(validate_review_examples(resolved, "development")$passed)
})

test_that("insufficient capture stays unknown under explicit scanner predicates", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  case <- review_decision(
    review_case(saved, "otel/limited-capture", scan = "suspicious_tool_loop"),
    "rejected",
    "A",
    "No loop visible"
  )
  examples <- review_examples(
    list(case),
    "scanner",
    "limited",
    "test",
    predicates = list(list(operator = "eq", value = 0L))
  )
  result <- validate_review_examples(examples, "test")
  expect_identical(result$assessment, "insufficient_evidence")
  expect_identical(result$count, NA_integer_)
  expect_identical(result$passed, NA)
})

test_that("preview displays complete mapped values and prior group text stays excluded", {
  saved <- investigation_snapshot(investigation_bundle_fixture())
  a <- review_decision(
    review_case(saved, "otel/retry"),
    "confirmed",
    "A",
    "Repeated calls",
    "Bound retries"
  )
  b <- review_decision(
    review_case(saved, "otel/parallel"),
    "confirmed",
    "A",
    "Distinct operations",
    "Keep independent operations"
  )
  long_text <- paste(rep("The complete selected task", 40L), collapse = " ")
  first <- review_examples(
    list(a),
    "application",
    "unselected-private-group",
    "development",
    inputs = list(list(question = "first")),
    targets = list(list(answer = "one"))
  )
  second <- review_examples(
    list(b),
    "application",
    "second-group",
    "test",
    inputs = list(list(question = long_text)),
    targets = list(list(answer = "two")),
    previous = first
  )
  html <- as.character(scans_app_review_preview_ui(second))
  expect_match(html, long_text, fixed = TRUE)
  expect_no_match(html, "unselected-private-group", fixed = TRUE)
  path <- tempfile()
  write_review_examples(second, path)
  expect_length(
    grep("unselected-private-group", readLines(path), fixed = TRUE),
    0L
  )
})
