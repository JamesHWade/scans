#' Save or reopen selected review examples
#'
#' Files contain selected evidence and review history, including retained text
#' and explicit input/target mappings. Inspect the example set's `content_policy`
#' and cases before sharing. The versioned JSON format accepts data only and
#' verifies content identifiers and relational references on read and write.
#'
#' @param x A value from [review_examples()].
#' @param path Local JSON file path.
#' @param overwrite Whether to replace an existing file.
#' @param max_bytes Maximum file size to read, in bytes. Defaults to 50 MiB.
#' @returns `write_review_examples()` returns `x` invisibly;
#'   `read_review_examples()` returns the verified example set.
#' @name review_example_files
#' @export
write_review_examples <- function(x, path, overwrite = FALSE) {
  review_examples_validate(x)
  investigation_check_path(path)
  rlang::check_bool(overwrite)
  if (file.exists(path) && !overwrite) {
    review_abort("The destination already exists.")
  }
  envelope <- list(
    format = "scans-review-examples",
    version = 1L,
    data = investigation_encode(unclass(x))
  )
  temporary <- tempfile(".scans-review-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  writeLines(
    jsonlite::toJSON(envelope, auto_unbox = TRUE, pretty = TRUE),
    temporary,
    useBytes = TRUE
  )
  if (file.exists(path) && !overwrite) {
    review_abort("The destination was created while writing.")
  }
  if (!suppressWarnings(file.rename(temporary, path))) {
    if (!overwrite || !file.copy(temporary, path, overwrite = TRUE)) {
      review_abort("Could not write the example set.")
    }
  }
  invisible(x)
}

#' @rdname review_example_files
#' @export
read_review_examples <- function(path, max_bytes = 50 * 1024^2) {
  investigation_check_dependencies()
  investigation_check_path(path)
  if (
    !is.numeric(max_bytes) ||
      length(max_bytes) != 1L ||
      is.na(max_bytes) ||
      max_bytes <= 0
  ) {
    review_abort("{.arg max_bytes} must be a positive number or Inf.")
  }
  info <- file.info(path)
  if (is.na(info$size) || isTRUE(info$isdir)) {
    review_abort("The file does not exist or is not regular.")
  }
  if (info$size > max_bytes) {
    review_abort("The file exceeds {.arg max_bytes}.")
  }
  envelope <- tryCatch(
    jsonlite::fromJSON(
      paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      simplifyVector = FALSE
    ),
    error = function(cnd) review_abort("The file is not valid JSON.")
  )
  if (
    !trajectory_is_named_list(envelope) ||
      !setequal(names(envelope), c("format", "version", "data")) ||
      !identical(envelope$format, "scans-review-examples") ||
      !identical(envelope$version, 1L)
  ) {
    review_abort("Unsupported review example format.")
  }
  out <- structure(
    investigation_decode(envelope$data),
    class = "scans_review_examples"
  )
  review_examples_validate(out)
  out
}
