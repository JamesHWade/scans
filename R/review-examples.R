#' Select versioned examples from reviewed evidence
#'
#' Selection is explicit: pass only cases whose retained content you intend to
#' include. The resulting value contains their evidence and complete decision
#' histories, input/target mappings, and a content policy. Nothing is selected
#' automatically from an annotation store or investigation.
#'
#' Each case belongs to one development or test group. Repeated tasks and
#' related executions must use the same group, including across new versions
#' of the set. The constructor checks group isolation and repeated source
#' trajectory identities; callers must identify other related tasks themselves.
#' A new version supplied through `previous` retains prior group assignments.
#'
#' Application examples require explicitly supplied named input and target
#' lists, plus an active decision with expected behavior. Scanner examples use
#' a count predicate such as `list(operator = "gte", value = 1L)` against the
#' case's saved scanner name. Counts cover the selected trajectory, restricted
#' to findings intersecting the case's event references when supplied.
#'
#' Unreviewed, disputed, uncertain, and insufficient-capture cases may be saved
#' for further review. [review_dataset()] refuses to turn them into targets.
#'
#' @param cases Non-empty list of values from [review_case()].
#' @param purpose Either `"application"` or `"scanner"`.
#' @param groups One stable task/group identifier per case.
#' @param splits One of `"development"` or `"test"` per case.
#' @param inputs,targets For application examples, one named list of explicitly
#'   selected fields per case. Input and target names must be disjoint and
#'   consistent across cases. Values must be serializable data.
#' @param predicates For scanner examples, one list with `operator` (`"eq"`,
#'   `"gte"`, or `"lte"`) and non-negative integer `value` per case.
#' @param previous Optional previous example set. Its version becomes the
#'   parent; prior group and source-trajectory split assignments are retained.
#'
#' @returns A `scans_review_examples` value. Its `version_id` covers selected
#'   content, mappings, history, and split assignments. Inspect the value before
#'   writing it with [write_review_examples()].
#' @export
review_examples <- function(
  cases,
  purpose,
  groups,
  splits,
  inputs = NULL,
  targets = NULL,
  predicates = NULL,
  previous = NULL
) {
  investigation_check_dependencies()
  review_choice(purpose, c("application", "scanner"), "purpose")
  if (!is.list(cases) || !length(cases)) {
    review_abort("Select at least one review case.")
  }
  lapply(cases, review_validate)
  n <- length(cases)
  if (anyDuplicated(vapply(cases, `[[`, "", "case_id"))) {
    review_abort("Select each case only once.")
  }
  for (name in c("groups", "splits")) {
    value <- get(name)
    if (
      !is.character(value) ||
        length(value) != n ||
        anyNA(value) ||
        !all(nzchar(trimws(value)))
    ) {
      review_abort("{.arg {name}} must contain one non-empty value per case.")
    }
  }
  if (!all(splits %in% c("development", "test"))) {
    review_abort("Splits must be development or test.")
  }
  assignments <- data.frame(
    group = unname(vapply(groups, investigation_hash, "")),
    split = splits,
    source_id = unname(vapply(cases, review_source_id, "")),
    stringsAsFactors = FALSE
  )
  assignments <- unique(assignments)
  parent <- NULL
  if (!is.null(previous)) {
    review_examples_validate(previous)
    if (!identical(previous$purpose, purpose)) {
      review_abort("An example set cannot change purpose.")
    }
    assignments <- unique(rbind(previous$assignments, assignments))
    parent <- previous$version_id
  }
  rownames(assignments) <- NULL
  for (key in c("group", "source_id")) {
    by_key <- split(assignments$split, assignments[[key]])
    if (any(vapply(by_key, function(x) length(unique(x)) != 1L, logical(1)))) {
      review_abort(
        "Related cases cannot cross development/test splits, including earlier versions."
      )
    }
  }
  if (purpose == "application") {
    if (!is.null(predicates)) {
      review_abort("Application examples do not use scanner predicates.")
    }
    review_mappings(inputs, n, "inputs")
    review_mappings(targets, n, "targets")
    if (length(intersect(names(inputs[[1L]]), names(targets[[1L]])))) {
      review_abort("Input and target fields must have different names.")
    }
  } else {
    if (!is.null(inputs) || !is.null(targets)) {
      review_abort("Scanner examples do not use application inputs or targets.")
    }
    if (!is.list(predicates) || length(predicates) != n) {
      review_abort("Supply one scanner predicate per case.")
    }
    for (i in seq_len(n)) {
      if (is.null(cases[[i]]$scan)) {
        review_abort("Scanner examples require a saved scanner name.")
      }
      p <- predicates[[i]]
      if (
        !trajectory_is_named_list(p) ||
          !setequal(names(p), c("operator", "value"))
      ) {
        review_abort("A scanner predicate needs operator and value.")
      }
      review_choice(p$operator, c("eq", "gte", "lte"), "operator")
      if (
        !is.numeric(p$value) ||
          length(p$value) != 1L ||
          !is.finite(p$value) ||
          p$value < 0 ||
          p$value %% 1 != 0
      ) {
        review_abort("A predicate value must be a non-negative integer.")
      }
    }
  }
  out <- list(
    format_version = 1L,
    purpose = purpose,
    cases = lapply(cases, unclass),
    groups = groups,
    splits = splits,
    inputs = inputs,
    targets = targets,
    predicates = predicates,
    assignments = assignments,
    parent_version_id = parent,
    content_policy = list(
      id = "selected-review-v1",
      included = "Selected trajectory evidence, relevant losses, complete selected case histories, explicit mappings, and split identity hashes.",
      excluded = "Unselected trajectories and review cases. No live producers or credentials are added.",
      redaction = "Existing redactions remain. No additional anonymization is performed."
    )
  )
  out$version_id <- investigation_hash(out)
  structure(out, class = "scans_review_examples")
}

#' Build explicitly mapped evaluation or optimization data
#'
#' `review_dataset()` emits application inputs and targets for a consumer. It
#' never infers a runnable input from captured messages, calls a model, or
#' turns an old reviewer rationale into a new prediction score. Use a scorer
#' that evaluates each fresh output against its target and expected behavior.
#'
#' The `"dsprrr"` format contains the named input and target columns required
#' by a module signature. The `"vitals"` format contains `id`, `input`, `target`,
#' and `metadata` list columns; use a solver/scorer that accepts the explicit
#' mappings. Provenance, expected behavior, groups, and version are available
#' in the `scans_provenance` attribute of either format. Do not include that
#' attribute or old reviewer rationale as optimization targets by accident.
#'
#' @param x An application example set from [review_examples()].
#' @param format Either `"dsprrr"` or `"vitals"`.
#' @param split Exactly one of `"development"` or `"test"`. Required so a
#'   training call cannot silently include held-out cases.
#' @returns A tibble of explicitly mapped cases with provenance attached.
#' @export
review_dataset <- function(x, format, split) {
  review_examples_validate(x)
  review_choice(format, c("dsprrr", "vitals"), "format")
  review_choice(split, c("development", "test"), "split")
  if (x$purpose != "application") {
    review_abort("Only application examples can become application data.")
  }
  rows <- which(x$splits == split)
  if (!length(rows)) {
    review_abort("No examples are selected for this split.")
  }
  cases <- lapply(x$cases[rows], structure, class = "scans_review_case")
  decisions <- lapply(cases, review_target_decision)
  expectations <- vapply(
    decisions,
    function(d) {
      if (is.null(d$expected_behavior)) {
        review_abort("Application examples require expected behavior.")
      }
      d$expected_behavior
    },
    ""
  )
  provenance <- tibble::tibble(
    case_id = vapply(cases, `[[`, "", "case_id"),
    decision_id = vapply(decisions, `[[`, "", "decision_id"),
    group = x$groups[rows],
    split = x$splits[rows],
    expected_behavior = expectations,
    version_id = x$version_id,
    origin = lapply(cases, `[[`, "origin")
  )
  if (format == "vitals") {
    out <- tibble::tibble(
      id = provenance$case_id,
      input = x$inputs[rows],
      target = x$targets[rows],
      metadata = lapply(seq_len(nrow(provenance)), function(i) {
        as.list(provenance[i, ])
      })
    )
  } else {
    records <- Map(c, x$inputs[rows], x$targets[rows])
    columns <- lapply(names(records[[1L]]), function(name) {
      values <- lapply(records, `[[`, name)
      if (
        all(vapply(
          values,
          function(value) {
            is.atomic(value) &&
              length(value) == 1L &&
              is.null(attributes(value))
          },
          logical(1)
        )) &&
          length(unique(vapply(values, typeof, ""))) == 1L
      ) {
        unlist(values, use.names = FALSE)
      } else {
        values
      }
    })
    names(columns) <- names(records[[1L]])
    out <- tibble::as_tibble(columns)
  }
  attr(out, "scans_provenance") <- provenance
  out
}

review_source_id <- function(x) {
  investigation_hash(list(
    application = x$evidence$manifest$application,
    trajectory_id = x$trajectory_id
  ))
}

review_mappings <- function(x, n, name) {
  if (
    !is.list(x) ||
      length(x) != n ||
      !all(vapply(
        x,
        function(row) {
          trajectory_is_named_list(row) &&
            length(row) > 0L &&
            !anyDuplicated(names(row)) &&
            all(nzchar(names(row)))
        },
        logical(1)
      ))
  ) {
    review_abort(
      "{.arg {name}} must contain a named field list for every case."
    )
  }
  if (
    !all(vapply(
      x,
      function(row) identical(names(row), names(x[[1L]])),
      logical(1)
    ))
  ) {
    review_abort("Mapping field names and order must agree across cases.")
  }
  investigation_encode(x)
  invisible(x)
}

review_target_decision <- function(x) {
  status <- review_status(x)
  if (!status$status %in% c("confirmed", "rejected", "missed")) {
    review_abort(
      "Resolve {.val {status$status}} reviews before producing targets."
    )
  }
  x$decisions[[match(
    status$decision_ids,
    vapply(x$decisions, `[[`, "", "decision_id")
  )]]
}

review_examples_validate <- function(x) {
  investigation_check_dependencies()
  if (
    !inherits(x, "scans_review_examples") ||
      !is.list(x) ||
      !identical(x$format_version, 1L) ||
      !identical(
        x$version_id,
        investigation_hash(unclass(x)[setdiff(names(x), "version_id")])
      )
  ) {
    review_abort("The example set format or content identifier is invalid.")
  }
  cases <- lapply(x$cases, structure, class = "scans_review_case")
  rebuilt <- review_examples(
    cases,
    x$purpose,
    x$groups,
    x$splits,
    x$inputs,
    x$targets,
    x$predicates
  )
  if (
    !identical(names(x), names(rebuilt)) ||
      !identical(x$content_policy, rebuilt$content_policy) ||
      !is.data.frame(x$assignments) ||
      !identical(names(x$assignments), c("group", "split", "source_id")) ||
      !all(vapply(x$assignments, is.character, logical(1))) ||
      anyNA(x$assignments) ||
      !all(x$assignments$split %in% c("development", "test")) ||
      !all(nzchar(x$assignments$group)) ||
      !all(nzchar(x$assignments$source_id))
  ) {
    review_abort("Invalid example set policy or split history.")
  }
  if (
    !all(grepl("^sha256:[0-9a-f]{64}$", x$assignments$group)) ||
      !all(grepl("^sha256:[0-9a-f]{64}$", x$assignments$source_id)) ||
      nrow(unique(rbind(x$assignments, rebuilt$assignments))) !=
        nrow(unique(x$assignments))
  ) {
    review_abort("The split ledger must include every selected case.")
  }
  for (key in c("group", "source_id")) {
    all_rows <- rbind(x$assignments, rebuilt$assignments)
    if (
      any(vapply(
        split(all_rows$split, all_rows[[key]]),
        function(x) length(unique(x)) != 1L,
        logical(1)
      ))
    ) {
      review_abort("The example set contains split leakage.")
    }
  }
  if (
    !is.null(x$parent_version_id) &&
      (!rlang::is_string(x$parent_version_id) ||
        is.na(x$parent_version_id) ||
        !grepl("^sha256:[0-9a-f]{64}$", x$parent_version_id))
  ) {
    review_abort("Invalid parent version identifier.")
  }
  invisible(x)
}
