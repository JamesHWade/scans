#' Check reviewed scanner targets against retained evidence
#'
#' Runs the installed deterministic scanner on each selected case using its
#' saved thresholds, then evaluates its explicit count predicate. Saved review
#' decisions and findings are never overwritten. Unresolved reviews or an
#' assessment with insufficient evidence, inapplicability, or execution failure
#' produce an unknown result (`NA`), not a negative label.
#'
#' @param x A scanner example set from [review_examples()].
#' @param split One of `"development"` or `"test"`.
#' @returns A tibble with case identity, judgment, scanner/assessment version,
#'   observed count, predicate, and `passed`. A pass validates the scanner
#'   predicate on retained evidence; it does not establish application quality.
#' @export
validate_review_examples <- function(x, split) {
  review_examples_validate(x)
  review_choice(split, c("development", "test"), "split")
  if (x$purpose != "scanner") {
    review_abort("Scanner validation requires scanner examples.")
  }
  rows <- which(x$splits == split)
  if (!length(rows)) {
    review_abort("No examples are selected for this split.")
  }
  results <- lapply(rows, function(i) {
    case <- structure(x$cases[[i]], class = "scans_review_case")
    saved <- review_evidence(case$evidence)
    status <- review_status(case)$status
    p <- x$predicates[[i]]
    assessment <- tryCatch(
      assess_trajectory_scans(
        saved$bundle,
        scans = case$scan,
        repeat_threshold = saved$settings$repeat_threshold,
        loop_threshold = saved$settings$loop_threshold
      ),
      error = function(cnd) NULL
    )
    assessed <- if (is.null(assessment)) {
      "execution_failure"
    } else {
      assessment$assessments$status[[1L]]
    }
    count <- NA_integer_
    passed <- NA
    if (assessed %in% c("assessed_with_findings", "assessed_no_findings")) {
      findings <- assessment$findings
      if (length(case$event_ids)) {
        keep <- vapply(
          seq_len(nrow(findings)),
          function(j) {
            any(
              c(findings$event_id[[j]], findings$event_ids[[j]]) %in%
                case$event_ids
            )
          },
          logical(1)
        )
        findings <- findings[keep, , drop = FALSE]
      }
      if (length(case$turn_ids)) {
        findings <- findings[
          findings$turn_id %in% case$turn_ids,
          ,
          drop = FALSE
        ]
      }
      count <- nrow(findings)
      if (status %in% c("confirmed", "rejected", "missed")) {
        passed <- switch(
          p$operator,
          eq = count == p$value,
          gte = count >= p$value,
          lte = count <= p$value
        )
      }
    }
    tibble::tibble(
      case_id = case$case_id,
      judgment = status,
      scan = case$scan,
      scan_version = if (is.null(assessment)) {
        NA_character_
      } else {
        assessment$assessments$scan_version[[1L]]
      },
      assessment = assessed,
      count = count,
      operator = p$operator,
      target = p$value,
      passed = passed,
      version_id = x$version_id
    )
  })
  trajectory_bind_rows(results)
}
