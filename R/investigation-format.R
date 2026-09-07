#' Write or read a saved investigation
#'
#' `write_investigation()` writes a value created by
#' [investigation_snapshot()]. `read_investigation()` validates and reopens it
#' without contacting its source or rerunning scanners. Saved scanner identities
#' can be reopened even when they are absent from the installed registry.
#' Pass the result to
#' [scans_app()] or inspect its `bundle`, `analysis`, `settings`, and `manifest`.
#'
#' Files use the versioned scans JSON format. They contain retained content,
#' not an anonymized report. SHA-256 identifiers detect changes to the evidence
#' or analysis; they do not establish who created the file. Unsupported formats,
#' invalid references, and edited identifiers are rejected.
#'
#' @param x A saved investigation from [investigation_snapshot()] or
#'   `read_investigation()`.
#' @param path Path to a local JSON file.
#' @param overwrite Whether to replace an existing file. Defaults to `FALSE`.
#' @param max_bytes Largest file to read, in bytes. Defaults to 50 MiB.
#'   Use `Inf` to disable the size limit.
#'
#' @returns `write_investigation()` returns `x` invisibly.
#'   `read_investigation()` returns a `scans_investigation` value.
#' @name investigation_files
#' @export
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "run-1", source_type = "manual",
#'              status = "completed"),
#'   data.frame(), data.frame()
#' )
#' saved <- investigation_snapshot(bundle)
#' path <- tempfile(fileext = ".scans.json")
#' write_investigation(saved, path)
#' reopened <- read_investigation(path)
#' reopened$manifest$snapshot_id
write_investigation <- function(x, path, overwrite = FALSE) {
  investigation_check_dependencies()
  investigation_validate(x)
  investigation_check_path(path)
  if (!rlang::is_bool(overwrite)) {
    investigation_abort("{.arg overwrite} must be TRUE or FALSE.")
  }
  if (file.exists(path) && !overwrite) {
    investigation_abort(
      "The destination already exists. Use a new path or {.code overwrite = TRUE}."
    )
  }
  envelope <- list(
    format = "scans-investigation",
    version = 1L,
    data = investigation_encode(investigation_payload(x))
  )
  text <- jsonlite::toJSON(envelope, auto_unbox = TRUE, pretty = TRUE)
  temporary <- tempfile(".scans-", tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  tryCatch(
    {
      writeLines(text, temporary, useBytes = TRUE)
      if (file.exists(path) && !overwrite) {
        investigation_abort(
          "The destination was created while writing. Choose a new path."
        )
      }
      moved <- suppressWarnings(file.rename(temporary, path))
      if (!moved) {
        if (!overwrite || !file.copy(temporary, path, overwrite = TRUE)) {
          investigation_abort("Could not write the investigation file.")
        }
      }
    },
    error = function(cnd) {
      if (inherits(cnd, "scans_error_investigation")) {
        stop(cnd)
      }
      investigation_abort("Could not write the investigation file.")
    }
  )
  invisible(x)
}

#' @rdname investigation_files
#' @export
read_investigation <- function(path, max_bytes = 50 * 1024^2) {
  investigation_check_dependencies()
  investigation_check_path(path)
  if (
    !is.numeric(max_bytes) ||
      length(max_bytes) != 1L ||
      is.na(max_bytes) ||
      max_bytes <= 0
  ) {
    investigation_abort("{.arg max_bytes} must be one positive number or Inf.")
  }
  size <- file.info(path)$size
  if (is.na(size) || isTRUE(file.info(path)$isdir)) {
    investigation_abort(
      "The investigation file does not exist or is not a regular file."
    )
  }
  if (size > max_bytes) {
    investigation_abort("The investigation file exceeds {.arg max_bytes}.")
  }
  envelope <- tryCatch(
    jsonlite::fromJSON(
      paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      simplifyVector = FALSE
    ),
    error = function(cnd) {
      investigation_abort("The investigation file is not valid JSON.")
    }
  )
  if (
    !trajectory_is_named_list(envelope) ||
      !setequal(names(envelope), c("format", "version", "data")) ||
      !identical(envelope$format, "scans-investigation")
  ) {
    investigation_abort("The file is not a scans investigation.")
  }
  if (!identical(envelope$version, 1L)) {
    investigation_abort("This investigation format version is not supported.")
  }
  payload <- investigation_decode(envelope$data)
  if (!trajectory_is_named_list(payload) || !is.list(payload$bundle)) {
    investigation_abort("The investigation payload is invalid.")
  }
  bundle <- payload$bundle
  if (!identical(bundle$schema_version, trajectory_schema_version)) {
    investigation_abort("This trajectory schema version is not supported.")
  }
  payload$bundle <- tryCatch(
    do.call(TrajectoryBundle, bundle[setdiff(names(bundle), "schema_version")]),
    error = function(cnd) {
      investigation_abort(
        "The saved trajectory bundle has invalid records or references."
      )
    }
  )
  x <- structure(payload, class = "scans_investigation")
  investigation_validate(x)
  x
}

investigation_check_dependencies <- function() {
  if (!rlang::is_installed(c("jsonlite", "digest"))) {
    investigation_abort(
      "Saved investigations require the {.pkg jsonlite} and {.pkg digest} packages."
    )
  }
}

investigation_check_path <- function(path) {
  if (!rlang::is_string(path) || is.na(path) || !nzchar(path)) {
    investigation_abort("{.arg path} must be one non-empty file path.")
  }
}

investigation_abort <- function(message) {
  scans_abort(
    message,
    class = "scans_error_investigation",
    call = rlang::caller_env(),
    .envir = rlang::caller_env()
  )
}

investigation_hash <- function(x) {
  text <- jsonlite::toJSON(investigation_encode(x), auto_unbox = TRUE)
  paste0(
    "sha256:",
    digest::digest(
      enc2utf8(as.character(text)),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

# The format restores only data classes whose behavior is defined by base R
# or tibble. A file cannot ask the reader to load or instantiate another class.
investigation_check_class <- function(x, path) {
  classes <- attr(x, "class", exact = TRUE)
  allowed <- list(
    NULL,
    "data.frame",
    c("tbl_df", "tbl", "data.frame"),
    "factor",
    c("ordered", "factor"),
    "Date",
    c("POSIXct", "POSIXt"),
    "AsIs"
  )
  if (!any(vapply(allowed, identical, logical(1), classes))) {
    investigation_abort("Unsupported data class at {.field {path}}.")
  }
}

investigation_encode <- function(x, path = "data", depth = 0L) {
  if (depth > 50L) {
    investigation_abort("The investigation data is nested too deeply.")
  }
  type <- typeof(x)
  if (
    !type %in%
      c(
        "NULL",
        "logical",
        "integer",
        "double",
        "complex",
        "character",
        "raw",
        "list"
      )
  ) {
    investigation_abort("Unsupported data type at {.field {path}}.")
  }
  investigation_check_class(x, path)
  if (is.null(x)) {
    return(list(type = "NULL"))
  }
  attributes <- attributes(x)
  if (length(attributes)) {
    attributes <- attributes[order(names(attributes), method = "radix")]
  }
  encoded_attributes <- lapply(names(attributes), function(name) {
    investigation_encode(
      attributes[[name]],
      paste0(path, "@", name),
      depth + 1L
    )
  })
  names(encoded_attributes) <- names(attributes)
  value <- switch(
    type,
    list = lapply(seq_along(x), function(i) {
      investigation_encode(x[[i]], paste0(path, "[[", i, "]]"), depth + 1L)
    }),
    raw = gsub("\n", "", jsonlite::base64_enc(unclass(x)), fixed = TRUE),
    complex = list(
      real = investigation_encode(Re(x), path, depth + 1L),
      imaginary = investigation_encode(Im(x), path, depth + 1L)
    ),
    double = as.list(sprintf("%a", as.double(x))),
    character = as.list(enc2utf8(as.character(x))),
    as.list(as.character(unclass(x)))
  )
  if (type %in% c("integer", "logical")) {
    value <- lapply(value, function(text) if (is.na(text)) "NA" else text)
  }
  # Missing character strings need a tag distinct from the literal "NA".
  if (type == "character") {
    value <- lapply(value, function(text) {
      if (is.na(text)) list(missing = TRUE) else list(text = text)
    })
  }
  list(type = type, value = value, attributes = encoded_attributes)
}

investigation_decode <- function(node, path = "data", depth = 0L) {
  if (
    depth > 50L ||
      !trajectory_is_named_list(node) ||
      !rlang::is_string(node$type)
  ) {
    investigation_abort("Invalid typed data at {.field {path}}.")
  }
  type <- node$type
  if (identical(type, "NULL") && identical(names(node), "type")) {
    return(NULL)
  }
  if (
    !type %in%
      c(
        "logical",
        "integer",
        "double",
        "complex",
        "character",
        "raw",
        "list"
      ) ||
      !setequal(names(node), c("type", "value", "attributes")) ||
      !is.list(node$attributes)
  ) {
    investigation_abort("Unsupported typed data at {.field {path}}.")
  }
  attributes <- node$attributes
  if (length(attributes) && !trajectory_is_named_list(attributes)) {
    investigation_abort("Invalid data attributes at {.field {path}}.")
  }
  attributes <- lapply(attributes, function(attribute) {
    investigation_decode(attribute, path, depth + 1L)
  })
  value <- node$value
  if (type %in% c("logical", "integer", "double")) {
    if (
      !is.list(value) ||
        !all(vapply(
          value,
          function(x) rlang::is_string(x) && !is.na(x),
          logical(1)
        ))
    ) {
      investigation_abort("Invalid atomic values at {.field {path}}.")
    }
    text <- unlist(value, use.names = FALSE)
    text <- text %||% character()
    if (type == "logical") {
      if (!all(text %in% c("TRUE", "FALSE", "NA"))) {
        investigation_abort("Invalid logical values at {.field {path}}.")
      }
      value <- match(text, c("FALSE", "TRUE")) == 2L
    } else {
      missing <- text == "NA"
      value <- suppressWarnings(as.double(replace(text, missing, "0")))
      if (any(is.na(value) & text != "NaN")) {
        investigation_abort("Invalid numeric values at {.field {path}}.")
      }
      value[missing] <- NA_real_
      if (type == "integer") {
        if (
          any(
            !is.na(value) &
              (!is.finite(value) |
                value != floor(value) |
                abs(value) > .Machine$integer.max)
          )
        ) {
          investigation_abort("Invalid integer values at {.field {path}}.")
        }
        value <- as.integer(value)
      }
    }
  } else if (type == "character") {
    if (!is.list(value)) {
      investigation_abort("Invalid strings at {.field {path}}.")
    }
    value <- vapply(
      value,
      function(element) {
        if (identical(element, list(missing = TRUE))) {
          return(NA_character_)
        }
        if (
          !trajectory_is_named_list(element) ||
            !identical(names(element), "text") ||
            !rlang::is_string(element$text)
        ) {
          investigation_abort("Invalid string at {.field {path}}.")
        }
        element$text
      },
      character(1),
      USE.NAMES = FALSE
    )
  } else if (type == "raw") {
    if (!rlang::is_string(value) || !grepl("^[A-Za-z0-9+/]*={0,2}$", value)) {
      investigation_abort("Invalid bytes at {.field {path}}.")
    }
    value <- jsonlite::base64_dec(value)
  } else if (type == "complex") {
    if (
      !trajectory_is_named_list(value) ||
        !setequal(names(value), c("real", "imaginary"))
    ) {
      investigation_abort("Invalid complex values at {.field {path}}.")
    }
    real <- investigation_decode(value$real, path, depth + 1L)
    imaginary <- investigation_decode(value$imaginary, path, depth + 1L)
    if (
      !is.double(real) ||
        !is.double(imaginary) ||
        length(real) != length(imaginary)
    ) {
      investigation_abort("Invalid complex components at {.field {path}}.")
    }
    value <- complex(real = real, imaginary = imaginary)
  } else {
    if (!is.list(value)) {
      investigation_abort("Invalid list at {.field {path}}.")
    }
    value <- lapply(seq_along(value), function(i) {
      investigation_decode(value[[i]], paste0(path, "[[", i, "]]"), depth + 1L)
    })
  }
  value <- tryCatch(
    {
      attributes(value) <- attributes
      value
    },
    error = function(cnd) {
      investigation_abort("Invalid data attributes at {.field {path}}.")
    }
  )
  investigation_check_class(value, path)
  if (
    is.factor(value) &&
      (!is.integer(unclass(value)) ||
        !is.character(levels(value)) ||
        any(
          !is.na(unclass(value)) &
            (unclass(value) < 1L | unclass(value) > nlevels(value))
        ))
  ) {
    investigation_abort("Invalid factor at {.field {path}}.")
  }
  value
}
