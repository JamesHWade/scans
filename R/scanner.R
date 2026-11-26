#' Define a custom deterministic scanner
#'
#' `scanner()` wraps a plain function and the facts the assessment contract
#' needs: a name, a version, the evidence it requires, and its settings. Pass
#' scanners to [scan_trajectories()], [assess_trajectory_scans()],
#' [investigation_snapshot()], [scans_tools()], or [scans_app()] through their
#' `scanners` argument. Nothing is registered globally: a scanner exists only
#' for the calls it is passed to.
#'
#' `fn` is called once per trajectory as `fn(trajectory, settings)`.
#' `trajectory` is a list holding one trajectory's rows from the bundle
#' accessors: `trajectory_id`, `info` (one row of [trajectory_info()]),
#' `turns` (in turn order), `events` (in event order), `evaluations`, and
#' `losses` (the [trajectory_losses()] rows assigned to this trajectory or to
#' no trajectory). It must return one of:
#'
#' - `scanner_findings()`: the scanner assessed the trajectory. Return
#'   `scanner_findings()` with no arguments to say "assessed, no findings"
#'   (`assessed_no_findings`).
#' - `scanner_unassessed()`: the scanner could not assess the trajectory,
#'   because the evidence it needs is not recorded (`"insufficient_evidence"`)
#'   or the scan does not apply to this capture (`"not_applicable"`).
#'
#' Anything else, including `NULL` and a data frame not made by
#' `scanner_findings()` (even an empty one), is an execution failure rather
#' than a clean negative, so a scanner that forgets to return cannot report
#' "no findings". Rows can be dropped from a `scanner_findings()` result with
#' `[`. The scanner decides whether recorded losses prevent an
#' assessment; the framework lists them in the assessment's limitations.
#'
#' The framework checks every finding. A turn or event reference that is not
#' in the scanned trajectory, or a malformed finding, makes the whole result
#' an `execution_failure` for that scanner and trajectory, and none of its
#' findings are kept. A scanner that throws is an `execution_failure` for that
#' scanner and trajectory only. [assess_trajectory_scans()] records a fixed
#' reason and never copies the condition message; [scan_trajectories()]
#' re-signals the failure with the original condition as its parent.
#'
#' Finding identifiers are `<scan_id>/<name>/finding-<n>`, numbered per
#' scanner in bundle trajectory order, then by the primary event, then in the
#' order returned. They do not change when other scanners are added, removed,
#' or deselected, and built-in identifiers never change because a custom
#' scanner ran.
#'
#' @param name One scanner name: lower-case letters, digits, and underscores,
#'   starting with a letter. It must not be the name of a built-in scan in
#'   [scan_registry()].
#' @param fn A function of `trajectory` and `settings`. It must be
#'   deterministic and must not call a model or tool.
#' @param description One sentence describing what a finding means.
#' @param required_evidence One sentence naming the recorded evidence the
#'   scanner needs before it can assess a trajectory.
#' @param version One non-empty version string. Change it whenever `fn` or
#'   its meaning changes; it is recorded with every finding and assessment.
#' @param severity `"warning"` or `"error"`, applied to every finding.
#' @param settings A named list of data passed to `fn` and recorded with each
#'   assessment, such as tool names or phrases the application owner chose.
#'
#' @returns `scanner()` returns a `scans_scanner` value.
#' @export
#' @examples
#' no_apology <- scanner(
#'   "apology_without_action",
#'   function(trajectory, settings) {
#'     events <- trajectory$events
#'     roles <- trajectory$turns$role[
#'       match(events$turn_id, trajectory$turns$turn_id)
#'     ]
#'     answers <- events[roles %in% "assistant" & events$event_type == "content", ]
#'     if (nrow(answers) == 0L) {
#'       return(scanner_unassessed(
#'         "insufficient_evidence",
#'         "No assistant text is recorded."
#'       ))
#'     }
#'     hits <- answers[grepl(settings$phrase, answers$text, ignore.case = TRUE), ]
#'     scanner_findings(
#'       event_id = hits$event_id,
#'       label = rep("Apology", nrow(hits)),
#'       explanation = rep("The assistant apologised.", nrow(hits))
#'     )
#'   },
#'   description = "The assistant apologised.",
#'   required_evidence = "Recorded assistant text.",
#'   settings = list(phrase = "sorry")
#' )
#'
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "run-1", source_type = "manual"),
#'   data.frame(trajectory_id = "run-1", turn_id = "turn-1", turn_index = 1L,
#'              role = "assistant"),
#'   data.frame(trajectory_id = "run-1", event_id = "event-1", event_index = 1L,
#'              turn_id = "turn-1", event_type = "content",
#'              text = "Sorry about that.")
#' )
#' result <- assess_trajectory_scans(
#'   bundle,
#'   scans = "apology_without_action",
#'   scanners = list(no_apology)
#' )
#' result$assessments[c("scan", "scan_version", "status")]
#' result$findings[c("finding_id", "event_id", "label")]
scanner <- function(
  name,
  fn,
  description,
  required_evidence,
  version = "1",
  severity = "warning",
  settings = list()
) {
  call <- rlang::current_env()
  scanner_check_string(name, "name", call)
  if (!grepl("^[a-z][a-z0-9_]*$", name)) {
    scanner_abort(
      "{.arg name} must use lower-case letters, digits, and underscores, starting with a letter.",
      call
    )
  }
  if (name %in% scan_registry()$scan) {
    scanner_abort(
      c(
        "{.val {name}} is the name of a built-in scan.",
        i = "Choose a name specific to your application, such as {.val {paste0('support_', name)}}."
      ),
      call
    )
  }
  if (!is.function(fn)) {
    scanner_abort("{.arg fn} must be a function.", call)
  }
  scanner_check_string(description, "description", call)
  scanner_check_string(required_evidence, "required_evidence", call)
  scanner_check_string(version, "version", call)
  if (!rlang::is_string(severity) || !severity %in% c("warning", "error")) {
    scanner_abort(
      "{.arg severity} must be {.val warning} or {.val error}.",
      call
    )
  }
  if (!trajectory_is_named_list(settings)) {
    scanner_abort("{.arg settings} must be a named list of data.", call)
  }
  structure(
    list(
      name = name,
      fn = fn,
      description = description,
      required_evidence = required_evidence,
      version = version,
      severity = severity,
      settings = settings
    ),
    class = "scans_scanner"
  )
}

#' @export
print.scans_scanner <- function(x, ...) {
  cli::cli_text("{.cls scans_scanner} {x$name} (version {x$version})")
  cli::cli_text("{x$description}")
  cli::cli_text("Requires: {x$required_evidence}")
  invisible(x)
}

#' @rdname scanner
#' @param event_id,turn_id Primary evidence identities for each finding, from
#'   the scanned trajectory. `NA` means the finding has none. A finding with
#'   an event and no turn takes the event's turn.
#' @param label,explanation A short label and a one-sentence explanation for
#'   each finding.
#' @param event_ids Optional list with every supporting event identifier for
#'   each finding. The primary event is always included.
#' @param value Optional list of scan-specific structured details, one per
#'   finding.
#' @returns `scanner_findings()` returns a tibble of findings of class
#'   `scans_scanner_findings`; call it with no arguments for a clean negative. `scanner_unassessed()` returns a value
#'   that marks the trajectory as not assessed.
#' @export
scanner_findings <- function(
  event_id = NA_character_,
  label = character(),
  explanation = character(),
  turn_id = NA_character_,
  event_ids = NULL,
  value = NULL
) {
  size <- length(label)
  out <- tibble::tibble(
    turn_id = as.character(vctrs::vec_recycle(turn_id, size)),
    event_id = as.character(vctrs::vec_recycle(event_id, size)),
    label = label,
    explanation = explanation
  )
  out$event_ids <- event_ids %||% rep(list(character()), size)
  out$value <- value %||% rep(list(list()), size)
  class(out) <- c("scans_scanner_findings", class(out))
  out
}

#' @rdname scanner
#' @param status `"insufficient_evidence"` or `"not_applicable"`.
#' @param reason One sentence, at most 500 characters, saying what is missing
#'   or why the scan does not apply. It is shown with the assessment.
#' @export
scanner_unassessed <- function(status, reason) {
  call <- rlang::current_env()
  if (
    !rlang::is_string(status) ||
      !status %in% c("insufficient_evidence", "not_applicable")
  ) {
    scanner_abort(
      "{.arg status} must be {.val insufficient_evidence} or {.val not_applicable}.",
      call
    )
  }
  scanner_check_string(reason, "reason", call)
  if (nchar(reason) > scanner_reason_max) {
    scanner_abort(
      "{.arg reason} must be at most {scanner_reason_max} characters.",
      call
    )
  }
  structure(
    list(status = status, reason = reason),
    class = "scans_scanner_unassessed"
  )
}

scanner_reason_max <- 500L

scanner_is_unassessed <- function(x) {
  is.list(x) &&
    rlang::is_string(x$status) &&
    x$status %in% c("insufficient_evidence", "not_applicable") &&
    rlang::is_string(x$reason) &&
    !is.na(x$reason) &&
    nzchar(trimws(x$reason)) &&
    nchar(x$reason) <= scanner_reason_max
}

scanner_abort <- function(message, call) {
  scans_abort(
    message,
    class = "scans_error_scanner",
    call = call,
    .envir = rlang::caller_env()
  )
}

scanner_check_string <- function(x, arg, call) {
  if (!rlang::is_string(x) || is.na(x) || !nzchar(trimws(x))) {
    scanner_abort("{.arg {arg}} must be one non-empty string.", call)
  }
}

# Normalizes the per-call `scanners` argument to a list named by scanner.
scanner_check_list <- function(scanners, call) {
  if (is.null(scanners)) {
    return(list())
  }
  if (inherits(scanners, "scans_scanner")) {
    scanners <- list(scanners)
  }
  if (
    !is.list(scanners) ||
      !all(vapply(scanners, inherits, logical(1), "scans_scanner"))
  ) {
    scanner_abort(
      "{.arg scanners} must be a list of scanners created with {.fn scanner}.",
      call
    )
  }
  scan_names <- vapply(scanners, `[[`, character(1), "name")
  repeated <- unique(scan_names[duplicated(scan_names)])
  if (length(repeated)) {
    scanner_abort(
      "{.arg scanners} contains more than one scanner named {.val {repeated}}.",
      call
    )
  }
  # A built-in added after the scanner was created must not share its results.
  builtin <- intersect(scan_names, scan_registry()$scan)
  if (length(builtin)) {
    scanner_abort(
      "Custom scanner name{?s} {.val {builtin}} collide{?s/} with built-in scans.",
      call
    )
  }
  stats::setNames(scanners, scan_names)
}

scanner_failure_reasons <- c(
  error = "The custom scanner function failed. Other scanners were retained; fix the scanner and rerun.",
  result = "The custom scanner returned a value that is not a findings table or an unassessed result.",
  reference = "The custom scanner returned a finding with a turn or event reference that is not in this trajectory. Its findings were discarded."
)

# Runs each scanner on each trajectory. With `strict`, a failure is signalled;
# otherwise it becomes that scanner's assessment for that trajectory.
scanner_run <- function(x, scanners, scan_id, strict = FALSE, call = NULL) {
  info <- trajectory_info(x)
  turns <- trajectory_turns(x)
  events <- trajectory_events(x)
  losses <- trajectory_losses(x)
  ids <- info$trajectory_id
  owners <- scan_loss_trajectory_ids(losses, turns, events)
  turn_groups <- scan_split_trajectory_rows(turns, ids)
  event_groups <- scan_split_trajectory_rows(events, ids)
  evaluation_groups <- scan_split_trajectory_rows(
    trajectory_evaluations(x),
    ids
  )
  findings <- list()
  assessments <- list()
  for (item in scanners) {
    counter <- 0L
    for (i in seq_along(ids)) {
      loss_rows <- which(is.na(owners) | owners == ids[[i]])
      trajectory <- list(
        trajectory_id = ids[[i]],
        info = info[i, , drop = FALSE],
        turns = turn_groups[[i]][
          order(turn_groups[[i]]$turn_index, method = "radix"),
          ,
          drop = FALSE
        ],
        events = event_groups[[i]][
          order(
            event_groups[[i]]$event_index,
            event_groups[[i]]$event_id,
            method = "radix"
          ),
          ,
          drop = FALSE
        ],
        evaluations = evaluation_groups[[i]],
        losses = losses[loss_rows, , drop = FALSE]
      )
      outcome <- scanner_run_one(item, trajectory, strict, call)
      found <- outcome$findings
      finding_ids <- character()
      if (!is.null(found) && nrow(found)) {
        found <- tibble::tibble(
          finding_id = sprintf(
            "%s/%s/finding-%06d",
            scan_id,
            item$name,
            counter + seq_len(nrow(found))
          ),
          scan_id = scan_id,
          scan = item$name,
          scan_version = item$version,
          trajectory_id = ids[[i]],
          turn_id = found$turn_id,
          event_id = found$event_id,
          event_ids = found$event_ids,
          severity = item$severity,
          label = found$label,
          value = found$value,
          explanation = found$explanation,
          metadata = rep(list(list()), nrow(found))
        )
        counter <- counter + nrow(found)
        finding_ids <- found$finding_id
        findings[[length(findings) + 1L]] <- found
      }
      scoped <- trajectory$losses
      assessments[[length(assessments) + 1L]] <- tibble::tibble(
        scan_id = scan_id,
        trajectory_id = ids[[i]],
        scan = item$name,
        scan_version = item$version,
        status = outcome$status,
        required_evidence = item$required_evidence,
        reason = outcome$reason,
        settings = list(item$settings),
        finding_ids = list(finding_ids),
        limitations = list(unique(c(
          "Assessment covers retained records only; complete capture is not established.",
          "A custom scanner decides its own applicability and evidence sufficiency.",
          if (nrow(scoped)) {
            paste0(scoped$field, ": ", scoped$reason, " - ", scoped$detail)
          }
        ))),
        loss_rows = list(loss_rows)
      )
    }
  }
  list(
    findings = if (length(findings)) {
      trajectory_bind_rows(findings)
    } else {
      scan_empty_findings()
    },
    assessments = if (length(assessments)) {
      trajectory_bind_rows(assessments)
    } else {
      scan_empty_assessments()
    }
  )
}

scanner_run_one <- function(item, trajectory, strict, call) {
  failed <- function(kind, parent = NULL) {
    if (strict) {
      name <- item$name
      id <- trajectory$trajectory_id
      scans_abort(
        c(
          "Custom scanner {.val {name}} failed on trajectory {.val {id}}.",
          x = scanner_failure_reasons[[kind]]
        ),
        class = "scans_error_scanner_failure",
        parent = parent,
        call = call
      )
    }
    list(
      status = "execution_failure",
      reason = scanner_failure_reasons[[kind]],
      findings = NULL
    )
  }
  result <- tryCatch(
    item$fn(trajectory, item$settings),
    error = function(cnd) cnd
  )
  if (inherits(result, "error")) {
    return(failed("error", result))
  }
  if (inherits(result, "scans_scanner_unassessed")) {
    # The class can be set by hand, so the fields are checked again here.
    if (!scanner_is_unassessed(result)) {
      return(failed("result"))
    }
    return(list(
      status = result$status,
      reason = result$reason,
      findings = NULL
    ))
  }
  if (!inherits(result, "scans_scanner_findings")) {
    return(failed("result"))
  }
  found <- scanner_normalize_findings(result)
  if (is.null(found)) {
    return(failed("result"))
  }
  events <- trajectory$events
  event_ok <- vapply(
    found$event_ids,
    function(ids) all(ids %in% events$event_id),
    logical(1)
  )
  turn_ok <- is.na(found$turn_id) | found$turn_id %in% trajectory$turns$turn_id
  owner_ok <- is.null(result[["trajectory_id"]]) ||
    all(result[["trajectory_id"]] %in% trajectory$trajectory_id)
  if (!all(event_ok) || !all(turn_ok) || !owner_ok) {
    return(failed("reference"))
  }
  event_turns <- events$turn_id[match(found$event_id, events$event_id)]
  inherit <- is.na(found$turn_id) & !is.na(found$event_id)
  found$turn_id[inherit] <- event_turns[inherit]
  position <- events$event_index[match(found$event_id, events$event_id)]
  position[is.na(position)] <- 0L
  found <- found[order(position, method = "radix"), , drop = FALSE]
  if (nrow(found)) {
    list(
      status = "assessed_with_findings",
      reason = "The custom scanner reported findings in the retained evidence.",
      findings = found
    )
  } else {
    list(
      status = "assessed_no_findings",
      reason = "The custom scanner assessed the retained evidence and reported no findings.",
      findings = found
    )
  }
}

# Returns NULL for anything that is not a well-formed findings table.
scanner_normalize_findings <- function(x) {
  allowed <- c(
    "trajectory_id",
    "turn_id",
    "event_id",
    "event_ids",
    "label",
    "explanation",
    "value"
  )
  if (
    !is.data.frame(x) ||
      !all(names(x) %in% allowed) ||
      (nrow(x) > 0L && !all(c("label", "explanation") %in% names(x)))
  ) {
    return(NULL)
  }
  size <- nrow(x)
  text <- function(name, required) {
    value <- x[[name]] %||% rep(NA_character_, size)
    if (is.logical(value) && all(is.na(value))) {
      value <- as.character(value)
    }
    if (is.factor(value)) {
      value <- as.character(value)
    }
    if (
      !is.character(value) ||
        (required && (anyNA(value) || !all(nzchar(trimws(value)))))
    ) {
      return(NULL)
    }
    value
  }
  columns <- list(
    turn_id = text("turn_id", FALSE),
    event_id = text("event_id", FALSE),
    label = text("label", TRUE),
    explanation = text("explanation", TRUE),
    trajectory_id = text("trajectory_id", FALSE)
  )
  event_ids <- x[["event_ids"]] %||% rep(list(character()), size)
  value <- x[["value"]] %||% rep(list(list()), size)
  if (
    any(vapply(columns, is.null, logical(1))) ||
      !is.list(event_ids) ||
      !is.list(value) ||
      !all(vapply(
        event_ids,
        function(ids) is.null(ids) || (is.character(ids) && !anyNA(ids)),
        logical(1)
      ))
  ) {
    return(NULL)
  }
  event_ids <- lapply(seq_len(size), function(i) {
    primary <- columns$event_id[[i]]
    unique(c(if (!is.na(primary)) primary, event_ids[[i]]))
  })
  columns$event_id <- vapply(
    seq_len(size),
    function(i) {
      if (length(event_ids[[i]])) event_ids[[i]][[1L]] else NA_character_
    },
    character(1)
  )
  tibble::tibble(
    turn_id = columns$turn_id,
    event_id = columns$event_id,
    event_ids = event_ids,
    label = columns$label,
    value = unname(value),
    explanation = columns$explanation
  )
}

# The built-in registry followed by a call's custom scanners.
scanner_registry <- function(scanners) {
  custom <- tibble::tibble(
    scan = unname(vapply(scanners, `[[`, character(1), "name")),
    severity = unname(vapply(scanners, `[[`, character(1), "severity")),
    description = unname(vapply(scanners, `[[`, character(1), "description"))
  )
  vctrs::vec_rbind(scan_registry(), custom)
}
