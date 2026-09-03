ellmer_check_installed <- function(call = rlang::caller_env()) {
  if (!rlang::is_installed("ellmer", version = "0.4.2")) {
    scans_abort(
      c(
        "Package {.pkg ellmer} 0.4.2 or later is required.",
        "i" = "Install it with {.code install.packages('ellmer')}."
      ),
      class = "scans_error_missing_dependency",
      call = call
    )
  }
}

ellmer_optional_string <- function(
  x,
  arg,
  call,
  required = FALSE
) {
  if (is.null(x)) {
    if (required) {
      scans_abort(
        "{.arg {arg}} must be a non-empty string.",
        class = "scans_error_ellmer_argument",
        call = call
      )
    }
    return(NA_character_)
  }

  valid <- is.character(x) && length(x) == 1L
  if (valid && is.na(x)) {
    valid <- !required
  } else if (valid) {
    valid <- nzchar(trimws(x)) &&
      nchar(x, type = "bytes") <= trajectory_payload_max_bytes
  }
  if (!valid) {
    scans_abort(
      c(
        "{.arg {arg}} must be a non-empty string or {.code NULL}.",
        "i" = "Strings must not exceed 65,536 bytes."
      ),
      class = "scans_error_ellmer_argument",
      call = call
    )
  }
  x
}

ellmer_optional_epoch <- function(x, call) {
  if (is.null(x)) {
    return(NA_integer_)
  }
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x == floor(x) &&
    x >= 1 &&
    x <= .Machine$integer.max
  if (!valid) {
    scans_abort(
      "{.arg epoch} must be one positive integer or {.code NULL}.",
      class = "scans_error_ellmer_argument",
      call = call
    )
  }
  as.integer(x)
}

ellmer_check_flag <- function(x, arg, call) {
  if (!rlang::is_bool(x)) {
    scans_abort(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
      class = "scans_error_ellmer_argument",
      call = call
    )
  }
  x
}

ellmer_check_metadata <- function(x, call) {
  if (!trajectory_is_named_list(x)) {
    scans_abort(
      "{.arg metadata} must be a named list.",
      class = "scans_error_ellmer_argument",
      call = call
    )
  }
  x
}

trajectory_check_metadata_paths <- function(x, field, call, class) {
  if (!trajectory_metadata_paths_bounded(x, field)) {
    scans_abort(
      c(
        "{.arg metadata} paths must not exceed 65,536 bytes.",
        "i" = "Shorten nested metadata names."
      ),
      class = class,
      call = call
    )
  }
  x
}

trajectory_metadata_paths_bounded <- function(x, field, depth = 0L) {
  field_bytes <- tryCatch(
    nchar(field, type = "bytes"),
    error = function(...) NA_integer_
  )
  if (
    length(field_bytes) != 1L ||
      is.na(field_bytes) ||
      field_bytes > trajectory_payload_max_bytes
  ) {
    return(FALSE)
  }
  if (
    depth >= 40L ||
      is.null(x) ||
      is.raw(x) ||
      is.environment(x) ||
      is.function(x) ||
      isS4(x) ||
      inherits(x, "connection") ||
      inherits(x, "S7_object") ||
      typeof(x) %in% c("externalptr", "weakref", "promise", "language")
  ) {
    return(TRUE)
  }

  element_names <- names(x)
  sensitive_atomic <- !is.list(x) &&
    !is.null(element_names) &&
    any(
      !is.na(element_names) &
        vapply(element_names, trajectory_sensitive_name, logical(1))
    )
  if (is.list(x) || !is.null(element_names)) {
    for (index in seq_along(x)) {
      name <- if (is.null(element_names)) {
        NA_character_
      } else {
        element_names[[index]]
      }
      child_field <- trajectory_child_field(field, name, index)
      if (!trajectory_metadata_paths_bounded(NULL, child_field, depth + 1L)) {
        return(FALSE)
      }
      if (
        (is.na(name) || !trajectory_sensitive_name(name)) &&
          !trajectory_metadata_paths_bounded(
            x[[index]],
            child_field,
            depth + 1L
          )
      ) {
        return(FALSE)
      }
    }
  }

  source_attributes <- if (sensitive_atomic) NULL else attributes(x)
  if (!is.null(source_attributes)) {
    attribute_names <- names(source_attributes)
    for (index in seq_along(source_attributes)) {
      name <- attribute_names[[index]]
      child_field <- paste0(field, "@", name)
      if (!trajectory_metadata_paths_bounded(NULL, child_field, depth + 1L)) {
        return(FALSE)
      }
      if (
        !trajectory_sensitive_name(name) &&
          !trajectory_metadata_paths_bounded(
            source_attributes[[index]],
            child_field,
            depth + 1L
          )
      ) {
        return(FALSE)
      }
    }
  }
  TRUE
}

trajectory_sanitize_metadata <- function(x, field, ids) {
  if (!trajectory_is_named_list(x)) {
    return(list(
      value = list(),
      losses = list(trajectory_new_loss(
        ids,
        field,
        "unsupported",
        "Unnamed source metadata was not retained"
      ))
    ))
  }

  out <- trajectory_sanitize_value(x, field, ids)
  if (!trajectory_is_named_list(out$value)) {
    out$value <- list()
    out$losses <- c(
      out$losses,
      list(trajectory_new_loss(
        ids,
        field,
        "unsupported",
        "Source metadata could not be represented as a named list"
      ))
    )
  }
  out
}

trajectory_sanitize_text <- function(x, field, ids) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return(list(value = NA_character_, losses = list()))
  }
  if (!is.character(x) || length(x) != 1L) {
    return(list(
      value = "<unsupported>",
      losses = list(trajectory_new_loss(
        ids,
        field,
        "unsupported",
        "A non-scalar text value was not retained"
      ))
    ))
  }

  bytes <- nchar(x, type = "bytes")
  if (bytes > trajectory_payload_max_bytes) {
    return(list(
      value = paste0(substr(x, 1L, 2048L), "..."),
      losses = list(trajectory_new_loss(
        ids,
        field,
        "truncated",
        "Source text exceeded the 65,536-byte payload limit"
      ))
    ))
  }
  list(value = x, losses = list())
}

trajectory_sanitize_value <- function(x, field, ids, depth = 0L) {
  if (depth >= 40L) {
    return(list(
      value = "<unsupported>",
      losses = list(trajectory_new_loss(
        ids,
        field,
        "unsupported",
        "Source data exceeded the supported nesting depth"
      ))
    ))
  }
  if (is.null(x)) {
    return(list(value = NULL, losses = list()))
  }
  if (is.raw(x)) {
    return(list(
      value = list(size_bytes = length(x)),
      losses = list(trajectory_new_loss(
        ids,
        field,
        "externalized",
        "Raw binary source data was not embedded"
      ))
    ))
  }
  if (
    is.environment(x) ||
      is.function(x) ||
      isS4(x) ||
      inherits(x, "connection") ||
      inherits(x, "S7_object") ||
      typeof(x) %in% c("externalptr", "weakref", "promise", "language")
  ) {
    return(list(
      value = "<unsupported>",
      losses = list(trajectory_new_loss(
        ids,
        field,
        "unsupported",
        "Live or non-serializable source data was not retained"
      ))
    ))
  }

  losses <- list()
  out <- x
  element_names <- names(x)
  sensitive_atomic <- !is.list(x) &&
    !is.null(element_names) &&
    any(
      !is.na(element_names) &
        vapply(element_names, trajectory_sensitive_name, logical(1))
    )
  if (sensitive_atomic) {
    out <- vector("list", length(x))
    names(out) <- element_names
    for (index in seq_along(x)) {
      name <- element_names[[index]]
      child_field <- trajectory_child_field(field, name, index)
      if (!is.na(name) && trajectory_sensitive_name(name)) {
        out[[index]] <- "<redacted>"
        losses <- c(
          losses,
          list(trajectory_new_loss(
            ids,
            child_field,
            "redacted",
            "A sensitive source field was redacted"
          ))
        )
      } else {
        element <- if (is.object(x)) {
          as.character(x)[[index]]
        } else {
          x[[index]]
        }
        attributes(element) <- NULL
        child <- trajectory_sanitize_value(
          element,
          child_field,
          ids,
          depth + 1L
        )
        out[[index]] <- child$value
        losses <- c(losses, child$losses)
      }
    }
    extra_attributes <- setdiff(names(attributes(x)), "names")
    if (length(extra_attributes) > 0L) {
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          field,
          "unsupported",
          "Attributes on a sensitive named vector were not retained"
        ))
      )
    }
  } else if (is.list(x)) {
    for (index in seq_along(x)) {
      name <- if (is.null(element_names)) {
        NA_character_
      } else {
        element_names[[index]]
      }
      child_field <- trajectory_child_field(field, name, index)
      if (!is.na(name) && trajectory_sensitive_name(name)) {
        out[index] <- list("<redacted>")
        losses <- c(
          losses,
          list(trajectory_new_loss(
            ids,
            child_field,
            "redacted",
            "A sensitive source field was redacted"
          ))
        )
      } else {
        child <- trajectory_sanitize_value(
          x[[index]],
          child_field,
          ids,
          depth + 1L
        )
        out[index] <- list(child$value)
        losses <- c(losses, child$losses)
      }
    }
  } else if (!is.null(element_names)) {
    for (index in seq_along(x)) {
      name <- element_names[[index]]
      if (!is.na(name) && trajectory_sensitive_name(name)) {
        out[index] <- "<redacted>"
        losses <- c(
          losses,
          list(trajectory_new_loss(
            ids,
            trajectory_child_field(field, name, index),
            "redacted",
            "A sensitive source field was redacted"
          ))
        )
      }
    }
  }

  if (is.character(out)) {
    bytes <- nchar(out, type = "bytes", allowNA = TRUE)
    too_large <- !is.na(bytes) & bytes > trajectory_payload_max_bytes
    if (any(too_large)) {
      out[too_large] <- paste0(substr(out[too_large], 1L, 2048L), "...")
      losses <- c(
        losses,
        list(trajectory_new_loss(
          ids,
          field,
          "truncated",
          "Source text exceeded the 65,536-byte payload limit"
        ))
      )
    }
  }

  source_attributes <- if (sensitive_atomic) NULL else attributes(x)
  if (!is.null(source_attributes)) {
    safe_attributes <- source_attributes
    attribute_names <- names(source_attributes)
    for (index in seq_along(source_attributes)) {
      name <- attribute_names[[index]]
      child_field <- paste0(field, "@", name)
      if (trajectory_sensitive_name(name)) {
        safe_attributes[[index]] <- "<redacted>"
        losses <- c(
          losses,
          list(trajectory_new_loss(
            ids,
            child_field,
            "redacted",
            "A sensitive source attribute was redacted"
          ))
        )
      } else {
        child <- trajectory_sanitize_value(
          source_attributes[[index]],
          child_field,
          ids,
          depth + 1L
        )
        safe_attributes[[index]] <- child$value
        losses <- c(losses, child$losses)
      }
    }
    attributes(out) <- safe_attributes
  }

  safe <- trajectory_value_is_safe(out)
  bytes <- if (safe) trajectory_serialized_bytes(out) else Inf
  if (!safe) {
    return(list(
      value = "<unsupported>",
      losses = c(
        losses,
        list(trajectory_new_loss(
          ids,
          field,
          "unsupported",
          "Source data was not safe for serialization"
        ))
      )
    ))
  }
  if (bytes > trajectory_payload_max_bytes) {
    return(list(
      value = "<truncated>",
      losses = c(
        losses,
        list(trajectory_new_loss(
          ids,
          field,
          "truncated",
          "Source data exceeded the 65,536-byte payload limit"
        ))
      )
    ))
  }
  list(value = out, losses = losses)
}

trajectory_sanitize_uri <- function(x, field, ids) {
  text <- trajectory_sanitize_text(x, field, ids)
  if (is.na(text$value)) {
    return(text)
  }

  safe <- sub(
    "^([A-Za-z][A-Za-z0-9+.-]*://)[^/@[:space:]]+@",
    "\\1",
    text$value,
    perl = TRUE
  )
  safe <- sub("[?#].*$", "", safe)
  if (!identical(safe, text$value)) {
    text$value <- safe
    text$losses <- c(
      text$losses,
      list(trajectory_new_loss(
        ids,
        field,
        "redacted",
        "URI credentials, query parameters, or fragments were removed"
      ))
    )
  }
  text
}

# A name is sensitive when, normalised to lowercase words, it is a known
# credential field or ends in one: `Authorization`, `X-Api-Key`,
# `ANTHROPIC_API_KEY`, `github_token`, and `client_secret` all match.
trajectory_sensitive_name <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  normalized <- gsub("[^a-z0-9]+", "_", tolower(x))
  normalized <- gsub("^_+|_+$", "", normalized)
  if (normalized %in% trajectory_sensitive_names) {
    return(TRUE)
  }
  grepl(trajectory_sensitive_pattern, normalized, perl = TRUE)
}

trajectory_sensitive_names <- c(
  "authorization",
  "proxy_authorization",
  "cookie",
  "set_cookie",
  "password",
  "passwd",
  "pwd",
  "auth",
  "bearer",
  "credential",
  "credentials",
  "secret"
)

trajectory_sensitive_pattern <- paste0(
  "(^|_)(",
  paste(
    c(
      "api_?key",
      "(access|secret|private|signing|encryption|account|service|license|licence|master|session|auth)_?key",
      "secret[a-z0-9_]*",
      "[a-z0-9_]*token",
      "[a-z0-9_]*password",
      "[a-z0-9_]*passwd",
      "credentials?",
      "cookies?",
      "authorization",
      "auth_?header"
    ),
    collapse = "|"
  ),
  ")$"
)

trajectory_child_field <- function(field, name, index) {
  if (!is.na(name) && nzchar(name)) {
    paste0(field, "$", name)
  } else {
    paste0(field, "[[", index, "]]")
  }
}

trajectory_serialized_bytes <- function(x) {
  tryCatch(
    length(serialize(x, NULL, xdr = FALSE)),
    error = function(...) Inf
  )
}

trajectory_new_loss <- function(
  ids,
  field,
  reason,
  detail,
  metadata = list()
) {
  list(
    trajectory_id = ids$trajectory_id,
    turn_id = ids$turn_id,
    event_id = ids$event_id,
    field = field,
    reason = reason,
    detail = detail,
    metadata = metadata
  )
}

trajectory_loss_table <- function(rows) {
  if (length(rows) == 0L) {
    return(NULL)
  }

  tibble::tibble(
    trajectory_id = vapply(rows, `[[`, character(1), "trajectory_id"),
    turn_id = vapply(rows, `[[`, character(1), "turn_id"),
    event_id = vapply(rows, `[[`, character(1), "event_id"),
    field = vapply(rows, `[[`, character(1), "field"),
    reason = vapply(rows, `[[`, character(1), "reason"),
    detail = vapply(rows, `[[`, character(1), "detail"),
    metadata = lapply(rows, `[[`, "metadata")
  )
}
