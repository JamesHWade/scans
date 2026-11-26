#' Compare a change on matched cases
#'
#' `compare_trajectories()` reads a pairing from [pair_trajectories()] and
#' reports what changed between the baseline and the candidate for each
#' matched case. It reads the saved evaluations, scanner assessments, and
#' resource measures of the two investigations. It never runs a solver, a
#' scorer, a scanner, or a model.
#'
#' Changes come in three families that are never combined into one score:
#'
#' * `outcomes`: one row per case and scorer. A change is reported only for
#'   a compatible scorer pair whose values are on the declared scale.
#' * `patterns`: one row per case and scan: `"appeared"`, `"resolved"`,
#'   `"persisted"`, `"absent"`, or `"not_comparable"` when either side was not
#'   assessed, did not run the scan, or ran it under a different version or
#'   settings. A pattern found on one side whose scan does not apply to the
#'   other side's capture is `"no_longer_applicable"` (found on the baseline)
#'   or `"newly_applicable"` (found on the candidate): the pattern could not
#'   occur in the other capture, which is not a fix or a regression. These are
#'   counted on their own and never feed the share resolved or its test. A
#'   scan that does not apply, with no finding on the other side, is absent.
#' * `resources`: one row per case and measure. A delta is reported only when
#'   both sides have a recorded value with complete coverage under the same
#'   unit, aggregation, source, measurement contract, and cache inclusion.
#'   Otherwise the delta is `NA` and `reason` says why. A higher resource
#'   value is not labelled better or worse.
#'
#' Every row carries both trajectory identities. Pattern rows also carry the
#' finding and event identities on each side.
#'
#' @section Summaries:
#' The estimand is a change among paired cases from the same dataset version.
#' `summaries` counts cases by direction for each scorer, scan, and measure.
#' A scorer with no comparable outcome keeps a row with `n` zero and its
#' `n_not_comparable` count.
#' When each case has one pair and at least six cases changed, it adds the
#' share of changed cases that moved in the named direction, an exact
#' (Clopper-Pearson) interval for that share, and an exact two-sided sign test
#' of an even split. For a binary scorer this is the exact McNemar test. With
#' fewer changed cases, or with repeated epochs, summaries are counts only and
#' `note` says why. P-values are per row and unadjusted. Two descriptive
#' numbers have no interval and are reported whatever the number of changed
#' cases: `paired_difference`, the difference in the proportion at the better
#' level for a binary scorer without repeated epochs, and `median_delta` for
#' numeric outcomes and resources.
#'
#' When no case pairs, for example under different dataset versions, the
#' design is `"unpaired"`: `summaries` describes each side separately, every
#' table is labelled `"unpaired"`, and no paired estimate, interval, or test
#' is produced.
#'
#' A recorded difference between two runs does not establish that the change
#' caused it. See `vignette("matched-comparison")` for what a result supports.
#'
#' @param pairing A `scans_pairing` value from [pair_trajectories()]. The
#'   investigations it holds must be valid and carry the snapshot and
#'   revision identifiers the pairing recorded.
#' @param conf_level Confidence level for intervals, between 0 and 1.
#'
#' @returns A `scans_comparison` value: a list with the `pairing`, the
#'   `design` (`type` and `reason`), per-case tibbles `outcomes`, `patterns`,
#'   and `resources`, a `cases` tibble with one row per pair counting its
#'   changes in each family, and `summaries` (a list of `outcomes`, `patterns`,
#'   and `resources` tibbles). Printing leads with what could not be compared.
#' @export
#' @examples
#' bundle <- function(score) {
#'   TrajectoryBundle(
#'     data.frame(
#'       trajectory_id = "run-1",
#'       sample_id = "case-1",
#'       source_type = "manual",
#'       status = "completed"
#'     ),
#'     data.frame(),
#'     data.frame(),
#'     evaluations = tibble::tibble(
#'       trajectory_id = "run-1",
#'       evaluation_id = "run-1/evaluation",
#'       scorer = "judge",
#'       value = list(score)
#'     )
#'   )
#' }
#' if (rlang::is_installed(c("digest", "jsonlite"))) {
#'   pairing <- pair_trajectories(
#'     investigation_snapshot(bundle(0), application = "Baseline"),
#'     investigation_snapshot(bundle(1), application = "Candidate"),
#'     by = "sample_id",
#'     dataset_version = "2026-11",
#'     scorers = list(comparison_scorer("judge", "1", "numeric"))
#'   )
#'   comparison <- compare_trajectories(pairing)
#'   comparison$outcomes[c("sample_id", "baseline_value", "candidate_value", "change")]
#'   comparison
#' }
compare_trajectories <- function(pairing, conf_level = 0.95) {
  if (!inherits(pairing, "scans_pairing")) {
    pairing_abort(
      "{.arg pairing} must be a pairing from {.fn pair_trajectories}."
    )
  }
  if (
    !is.numeric(conf_level) ||
      length(conf_level) != 1L ||
      is.na(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1
  ) {
    pairing_abort("{.arg conf_level} must be one number between 0 and 1.")
  }
  comparison_check_sides(pairing)
  outcomes <- comparison_outcomes(pairing)
  patterns <- comparison_patterns(pairing)
  resources <- comparison_resources(pairing)
  design <- comparison_design(pairing)
  tables <- list(
    outcomes = outcomes,
    patterns = patterns,
    resources = resources
  )
  summaries <- if (design$type == "paired") {
    comparison_paired_summaries(tables, pairing, conf_level)
  } else {
    comparison_unpaired_summaries(pairing)
  }
  structure(
    c(
      list(pairing = pairing, design = design),
      tables,
      list(
        cases = comparison_cases(pairing$pairs, tables),
        summaries = summaries
      )
    ),
    class = "scans_comparison"
  )
}

#' @export
print.scans_comparison <- function(x, ...) {
  pairing <- x$pairing
  cli::cli_text(
    "{.cls scans_comparison} {comparison_plain(pairing$snapshots$application[[1L]])} -> {comparison_plain(pairing$snapshots$application[[2L]])}"
  )
  cli::cli_text(
    "Key: {.field {pairing$key$by}}; {comparison_plain(pairing_version_text(pairing))}"
  )
  cli::cli_text("Baseline snapshot: {pairing$snapshots$snapshot_id[[1L]]}")
  cli::cli_text("Candidate snapshot: {pairing$snapshots$snapshot_id[[2L]]}")
  cli::cli_h3("Not compared")
  cli::cli_bullets(comparison_gap_bullets(x))
  if (x$design$type == "unpaired") {
    cli::cli_h3("Unpaired description")
    cli::cli_text(x$design$reason)
    cli::cli_text(
      "See {.code $summaries} for each side. No paired estimate is produced."
    )
    return(invisible(x))
  }
  cli::cli_h3("Paired changes in {nrow(pairing$pairs)} matched case{?s}")
  lines <- comparison_summary_lines(x$summaries)
  cli::cli_bullets(stats::setNames(lines, rep("*", length(lines))))
  invisible(x)
}

# The pairing's tables were built from the investigations it holds, so each
# must still be a valid saved investigation with the identity it was paired
# under.
comparison_check_sides <- function(pairing, call = rlang::caller_env()) {
  for (side in pairing_sides) {
    x <- pairing[[side]]
    investigation_validate(x)
    paired <- pairing$snapshots[pairing$snapshots$side == side, ]
    if (
      !identical(x$manifest$snapshot_id, paired$snapshot_id) ||
        !identical(x$manifest$revision_id, paired$revision_id)
    ) {
      pairing_abort(
        c(
          "The {side} investigation is not the one this pairing was built from.",
          "i" = "Call {.fn pair_trajectories} again with the investigations to compare."
        ),
        call
      )
    }
  }
}

comparison_identity <- c(
  "pair_id",
  "dataset_version",
  "task_id",
  "sample_id",
  "epoch",
  "baseline_trajectory_id",
  "candidate_trajectory_id"
)

comparison_design <- function(pairing) {
  if (nrow(pairing$pairs)) {
    return(list(
      type = "paired",
      reason = "Cases matched on the key within one dataset version."
    ))
  }
  versions <- pairing$key$dataset_version
  reason <- if (anyNA(versions)) {
    "No dataset version was declared, so no case can be matched."
  } else if (versions[[1L]] != versions[[2L]]) {
    "The sides declare different dataset versions, so no case is matched."
  } else {
    "No case is present exactly once on both sides."
  }
  list(type = "unpaired", reason = reason)
}

comparison_outcomes <- function(pairing) {
  out <- pairing$outcome_pairs
  values <- lapply(pairing_sides, function(side) {
    evaluations <- trajectory_evaluations(pairing[[side]]$bundle)
    evaluations$value[match(
      out[[paste0(side, "_evaluation_id")]],
      evaluations$evaluation_id
    )]
  })
  position <- function(value, i) {
    if (out$scale[[i]] == "numeric") {
      return(if (comparison_is_number(value)) as.numeric(value) else NA_real_)
    }
    if (length(value) != 1L) {
      return(NA_real_)
    }
    as.numeric(match(as.character(value), out$levels[[i]]))
  }
  out$baseline_value <- vapply(values[[1L]], comparison_value_text, "")
  out$candidate_value <- vapply(values[[2L]], comparison_value_text, "")
  out$delta <- NA_real_
  out$change <- NA_character_
  for (i in which(out$compatibility == "compatible")) {
    baseline <- position(values[[1L]][[i]], i)
    candidate <- position(values[[2L]][[i]], i)
    if (is.na(baseline) || is.na(candidate)) {
      out$reason[[i]] <- "A recorded value is not on the scorer's scale."
      next
    }
    step <- candidate - baseline
    if (out$scale[[i]] != "ordered") {
      out$delta[[i]] <- step
    }
    better <- if (out$direction[[i]] == "higher") step else -step
    out$change[[i]] <- c("regressed", "unchanged", "improved")[
      sign(better) + 2L
    ]
  }
  out$comparable <- !is.na(out$change)
  out[c(
    comparison_identity,
    "scorer_id",
    "scorer_version",
    "scale",
    "direction",
    "baseline_value",
    "candidate_value",
    "change",
    "delta",
    "comparable",
    "compatibility",
    "reason",
    "baseline_scorer",
    "candidate_scorer",
    "baseline_evaluation_id",
    "candidate_evaluation_id"
  )]
}

comparison_is_number <- function(value) {
  is.numeric(value) &&
    !is.factor(value) &&
    length(value) == 1L &&
    is.finite(value)
}

comparison_value_text <- function(value) {
  if (is.atomic(value) && length(value) == 1L && !is.na(value)) {
    as.character(value)
  } else {
    NA_character_
  }
}

comparison_patterns <- function(pairing) {
  pairs <- pairing$pairs
  sides <- lapply(pairing_sides, function(side) pairing[[side]])
  scans <- union(sides[[1L]]$settings$scans, sides[[2L]]$settings$scans)
  grid <- tibble::tibble(
    pairs[rep(seq_len(nrow(pairs)), each = length(scans)), ],
    scan = rep(scans, times = nrow(pairs))
  )
  state <- lapply(seq_along(pairing_sides), function(s) {
    side <- pairing_sides[[s]]
    assessments <- sides[[s]]$analysis$assessments
    findings <- sides[[s]]$analysis$findings
    row <- match(
      paste(grid[[paste0(side, "_trajectory_id")]], grid$scan, sep = "\r"),
      paste(assessments$trajectory_id, assessments$scan, sep = "\r")
    )
    finding_ids <- lapply(row, function(i) {
      if (is.na(i)) character() else as.character(assessments$finding_ids[[i]])
    })
    list(
      status = ifelse(
        grid$scan %in% sides[[s]]$settings$scans,
        assessments$status[row],
        "not_run"
      ),
      version = assessments$scan_version[row],
      settings = assessments$settings[row],
      finding_ids = finding_ids,
      event_ids = lapply(finding_ids, comparison_finding_events, findings)
    )
  })
  present <- lapply(state, function(x) {
    ifelse(
      x$status == "assessed_with_findings",
      TRUE,
      ifelse(
        x$status %in% c("assessed_no_findings", "not_applicable"),
        FALSE,
        NA
      )
    )
  })
  same_scanner <- vapply(
    seq_len(nrow(grid)),
    function(i) {
      identical(state[[1L]]$version[[i]], state[[2L]]$version[[i]]) &&
        identical(state[[1L]]$settings[[i]], state[[2L]]$settings[[i]])
    },
    logical(1)
  )
  unassessed <- is.na(present[[1L]]) | is.na(present[[2L]])
  change <- ifelse(
    present[[1L]],
    ifelse(present[[2L]], "persisted", "resolved"),
    ifelse(present[[2L]], "appeared", "absent")
  )
  # A scan that stops or starts applying to the capture says the pattern can
  # no longer, or can now, occur there: not that it was fixed or introduced.
  not_applicable <- lapply(state, \(x) x$status %in% "not_applicable")
  change[present[[1L]] %in% TRUE & not_applicable[[2L]]] <-
    "no_longer_applicable"
  change[not_applicable[[1L]] & present[[2L]] %in% TRUE] <- "newly_applicable"
  change[unassessed | !same_scanner] <- "not_comparable"
  side_reason <- function(status, side) {
    ifelse(
      is.na(status),
      paste0("No assessment was saved for the ", side, "."),
      ifelse(
        status == "not_run",
        paste0("The scan was not selected for the ", side, "."),
        paste0("The ", side, " was not assessed (", status, ").")
      )
    )
  }
  reason <- ifelse(
    is.na(present[[1L]]),
    side_reason(state[[1L]]$status, "baseline"),
    ifelse(
      is.na(present[[2L]]),
      side_reason(state[[2L]]$status, "candidate"),
      ifelse(
        !same_scanner,
        "Scanner version or settings differ.",
        ifelse(
          change == "no_longer_applicable",
          "The scan does not apply to the candidate capture.",
          ifelse(
            change == "newly_applicable",
            "The scan did not apply to the baseline capture.",
            NA
          )
        )
      )
    )
  )
  tibble::tibble(
    grid,
    change = as.character(change),
    baseline_status = as.character(state[[1L]]$status),
    candidate_status = as.character(state[[2L]]$status),
    baseline_finding_ids = state[[1L]]$finding_ids,
    candidate_finding_ids = state[[2L]]$finding_ids,
    baseline_event_ids = state[[1L]]$event_ids,
    candidate_event_ids = state[[2L]]$event_ids,
    reason = as.character(reason)
  )
}

comparison_finding_events <- function(finding_ids, findings) {
  rows <- findings[findings$finding_id %in% finding_ids, ]
  ids <- c(rows$event_id, unlist(rows$event_ids, use.names = FALSE))
  unique(as.character(ids[!is.na(ids)]))
}

comparison_resources <- function(pairing) {
  pairs <- pairing$pairs
  measures <- lapply(pairing_sides, \(side) pairing[[side]]$analysis$measures)
  names <- union(measures[[1L]]$measure, measures[[2L]]$measure)
  grid <- tibble::tibble(
    pairs[rep(seq_len(nrow(pairs)), each = length(names)), ],
    measure = rep(names, times = nrow(pairs))
  )
  rows <- lapply(seq_along(pairing_sides), function(s) {
    x <- measures[[s]]
    x[
      match(
        paste(
          grid[[paste0(pairing_sides[[s]], "_trajectory_id")]],
          grid$measure,
          sep = "\r"
        ),
        paste(x$trajectory_id, x$measure, sep = "\r")
      ),
    ]
  })
  b <- rows[[1L]]
  c <- rows[[2L]]
  unknown <- function(x, side) {
    ifelse(
      is.na(x$measure),
      paste0("Not measured on the ", side, "."),
      ifelse(
        is.na(x$value),
        paste0(
          "No recorded value on the ",
          side,
          " (coverage ",
          x$coverage,
          ")."
        ),
        ifelse(
          x$coverage != "complete",
          paste0("Coverage is ", x$coverage, " on the ", side, "."),
          NA
        )
      )
    )
  }
  differs <- function(field) {
    vapply(
      seq_len(nrow(grid)),
      \(i) !identical(b[[field]][[i]], c[[field]][[i]]),
      logical(1)
    )
  }
  semantics <- c("unit", "aggregation", "source", "contract_version")
  different <- vapply(semantics, differs, logical(nrow(grid)))
  different <- matrix(different, nrow = nrow(grid), ncol = length(semantics))
  semantic_reason <- vapply(
    seq_len(nrow(grid)),
    function(i) {
      if (!any(different[i, ])) {
        return(NA_character_)
      }
      paste0(
        "Measured under different semantics (",
        paste(semantics[different[i, ]], collapse = ", "),
        ")."
      )
    },
    ""
  )
  cache_reason <- ifelse(
    differs("includes_cached_input"),
    "Cache inclusion differs or is unknown on one side.",
    ifelse(
      grid$measure == "input_tokens" & is.na(b$includes_cached_input),
      "Cache inclusion is unknown on both sides.",
      NA
    )
  )
  neither <- is.na(b$value) &
    is.na(c$value) &
    !is.na(b$measure) &
    !is.na(c$measure)
  reason <- ifelse(
    neither,
    "No recorded value on either side.",
    comparison_coalesce(
      unknown(b, "baseline"),
      unknown(c, "candidate"),
      semantic_reason,
      cache_reason
    )
  )
  comparable <- is.na(reason)
  tibble::tibble(
    grid,
    unit = as.character(ifelse(
      is.na(b$unit),
      c$unit,
      ifelse(is.na(c$unit) | b$unit == c$unit, b$unit, NA)
    )),
    baseline_unit = b$unit,
    candidate_unit = c$unit,
    baseline_value = b$value,
    candidate_value = c$value,
    delta = as.double(ifelse(comparable, c$value - b$value, NA_real_)),
    comparable = comparable,
    reason = as.character(reason),
    baseline_coverage = b$coverage,
    candidate_coverage = c$coverage,
    baseline_n_recorded = b$n_recorded,
    baseline_n_total = b$n_total,
    candidate_n_recorded = c$n_recorded,
    candidate_n_total = c$n_total,
    baseline_includes_cached_input = b$includes_cached_input,
    candidate_includes_cached_input = c$includes_cached_input,
    baseline_source = b$source,
    candidate_source = c$source
  )
}

comparison_coalesce <- function(...) {
  Reduce(function(x, y) ifelse(is.na(x), y, x), list(...))
}

comparison_cases <- function(pairs, tables) {
  count <- function(table, rows) {
    tabulate(match(table$pair_id[rows], pairs$pair_id), nbins = nrow(pairs))
  }
  outcomes <- tables$outcomes
  patterns <- tables$patterns
  resources <- tables$resources
  recorded <- !is.na(resources$baseline_value) |
    !is.na(resources$candidate_value)
  tibble::tibble(
    pairs,
    outcomes_improved = count(outcomes, outcomes$change %in% "improved"),
    outcomes_regressed = count(outcomes, outcomes$change %in% "regressed"),
    outcomes_unchanged = count(outcomes, outcomes$change %in% "unchanged"),
    outcomes_not_comparable = count(outcomes, !outcomes$comparable),
    patterns_appeared = count(patterns, patterns$change == "appeared"),
    patterns_resolved = count(patterns, patterns$change == "resolved"),
    patterns_no_longer_applicable = count(
      patterns,
      patterns$change == "no_longer_applicable"
    ),
    patterns_newly_applicable = count(
      patterns,
      patterns$change == "newly_applicable"
    ),
    patterns_persisted = count(patterns, patterns$change == "persisted"),
    patterns_not_comparable = count(
      patterns,
      patterns$change == "not_comparable"
    ),
    resources_increased = count(
      resources,
      !is.na(resources$delta) & resources$delta > 0
    ),
    resources_decreased = count(
      resources,
      !is.na(resources$delta) & resources$delta < 0
    ),
    resources_unknown = count(resources, !resources$comparable & recorded)
  )
}

comparison_paired_summaries <- function(tables, pairing, conf_level) {
  pairs <- pairing$pairs
  case <- paste(
    pairs$dataset_version,
    pairs$task_id,
    pairs$sample_id,
    sep = "\r"
  )
  replicated <- anyDuplicated(case) > 0L
  test <- function(n_named, n_other) {
    comparison_sign_test(n_named, n_other, conf_level, replicated)
  }
  unit <- if (replicated) "epoch pair" else "case"

  outcomes <- tables$outcomes
  # Groups come from every compatible row, comparable or not, so a scorer
  # with no comparable outcome keeps a counts-only row. A scorer never
  # compatible on any pair has an unknown version and gets one row by id.
  compatible <- outcomes$compatibility == "compatible"
  outcome_key <- paste(outcomes$scorer_id, outcomes$scorer_version, sep = "\r")
  outcome_key[!compatible] <- NA
  groups <- unique(outcome_key[!is.na(outcome_key)])
  unversioned <- setdiff(
    outcomes$scorer_id[!is.na(outcomes$scorer_id)],
    outcomes$scorer_id[compatible]
  )
  outcome_rows <- lapply(c(groups, unversioned), function(group) {
    in_group <- if (group %in% groups) {
      outcome_key %in% group
    } else {
      outcomes$scorer_id %in% group
    }
    described <- outcomes[in_group, ][1L, ]
    x <- outcomes[in_group & outcomes$comparable, ]
    tally <- table(factor(x$change, c("improved", "regressed", "unchanged")))
    binary <- described$scale %in% "binary"
    tibble::tibble(
      design = "paired",
      unit = unit,
      scorer_id = described$scorer_id,
      scorer_version = described$scorer_version,
      scale = described$scale,
      direction = described$direction,
      n = nrow(x),
      n_improved = tally[["improved"]],
      n_regressed = tally[["regressed"]],
      n_unchanged = tally[["unchanged"]],
      n_not_comparable = sum(
        !outcomes$comparable & outcomes$scorer_id %in% described$scorer_id
      ),
      paired_difference = if (binary && !replicated && nrow(x)) {
        (tally[["improved"]] - tally[["regressed"]]) / nrow(x)
      } else {
        NA_real_
      },
      median_delta = if (described$scale %in% "numeric" && nrow(x)) {
        stats::median(x$delta)
      } else {
        NA_real_
      },
      test(tally[["improved"]], tally[["regressed"]])
    )
  })
  outcome_summary <- pairing_bind(c(
    list(comparison_empty_outcome_summary()),
    outcome_rows
  ))
  names(outcome_summary)[names(outcome_summary) == "share"] <- "share_improved"

  patterns <- tables$patterns
  pattern_rows <- lapply(unique(patterns$scan), function(scan) {
    x <- patterns[patterns$scan == scan, ]
    tally <- table(factor(
      x$change,
      c(
        "appeared",
        "resolved",
        "persisted",
        "absent",
        "no_longer_applicable",
        "newly_applicable",
        "not_comparable"
      )
    ))
    tibble::tibble(
      design = "paired",
      unit = unit,
      scan = scan,
      n = nrow(x) - tally[["not_comparable"]],
      n_appeared = tally[["appeared"]],
      n_resolved = tally[["resolved"]],
      n_persisted = tally[["persisted"]],
      n_absent = tally[["absent"]],
      n_no_longer_applicable = tally[["no_longer_applicable"]],
      n_newly_applicable = tally[["newly_applicable"]],
      n_not_comparable = tally[["not_comparable"]],
      test(tally[["resolved"]], tally[["appeared"]])
    )
  })
  pattern_summary <- pairing_bind(pattern_rows)
  names(pattern_summary)[names(pattern_summary) == "share"] <- "share_resolved"

  resources <- tables$resources
  resource_rows <- lapply(unique(resources$measure), function(measure) {
    x <- resources[resources$measure == measure, ]
    delta <- x$delta[x$comparable]
    units <- unique(x$unit[x$comparable])
    tibble::tibble(
      design = "paired",
      unit = unit,
      measure = measure,
      measure_unit = if (length(units) == 1L) {
        units
      } else if (!length(units)) {
        x$unit[[1L]]
      } else {
        NA_character_
      },
      n = length(delta),
      n_increased = sum(delta > 0),
      n_decreased = sum(delta < 0),
      n_unchanged = sum(delta == 0),
      n_unknown = sum(!x$comparable),
      # A median across cases needs one unit.
      median_delta = if (length(units) == 1L) {
        stats::median(delta)
      } else {
        NA_real_
      },
      test(sum(delta > 0), sum(delta < 0))
    )
  })
  resource_summary <- pairing_bind(resource_rows)
  names(resource_summary)[
    names(resource_summary) == "share"
  ] <- "share_increased"

  list(
    outcomes = outcome_summary,
    patterns = pattern_summary,
    resources = resource_summary
  )
}

comparison_empty_outcome_summary <- function() {
  tibble::tibble(
    design = character(),
    unit = character(),
    scorer_id = character(),
    scorer_version = character(),
    scale = character(),
    direction = character(),
    n = integer(),
    n_improved = integer(),
    n_regressed = integer(),
    n_unchanged = integer(),
    n_not_comparable = integer(),
    paired_difference = double(),
    median_delta = double(),
    comparison_sign_test(0L, 0L, 0.95, FALSE)[0L, ]
  )
}

# An exact sign test needs at least six changed cases before its two-sided
# p-value can fall below 0.05 (2 * 0.5^5 = 0.0625), so fewer are counts only.
comparison_min_changed <- 6L

comparison_sign_test <- function(n_named, n_other, conf_level, replicated) {
  changed <- n_named + n_other
  out <- tibble::tibble(
    share = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    conf_level = NA_real_,
    p_value = NA_real_,
    method = "counts",
    note = NA_character_
  )
  if (replicated) {
    out$note <- "Repeated epochs: pairs of one case are not independent, so no interval or test is reported."
  } else if (changed < comparison_min_changed) {
    out$note <- paste0(
      "Fewer than ",
      comparison_min_changed,
      " changed cases: an exact sign test cannot separate a change from an even split."
    )
  } else {
    test <- stats::binom.test(
      n_named,
      changed,
      p = 0.5,
      conf.level = conf_level
    )
    out$share <- n_named / changed
    out$conf_low <- test$conf.int[[1L]]
    out$conf_high <- test$conf.int[[2L]]
    out$conf_level <- conf_level
    out$p_value <- test$p.value
    out$method <- "exact sign test"
  }
  out
}

comparison_unpaired_summaries <- function(pairing) {
  by_side <- function(build) {
    pairing_bind(lapply(pairing_sides, function(side) {
      build(pairing[[side]], side)
    }))
  }
  outcomes <- by_side(function(x, side) {
    evaluations <- trajectory_evaluations(x$bundle)
    scorers <- pairing$scorers[pairing$scorers$side == side, ]
    rows <- lapply(seq_len(nrow(scorers)), function(i) {
      values <- evaluations$value[evaluations$scorer %in% scorers$scorer[[i]]]
      numeric <- scorers$scale[[i]] %in% "numeric"
      text <- vapply(values, comparison_value_text, "")
      on_scale <- vapply(values, comparison_is_number, logical(1))
      counts <- if (numeric) {
        sum(on_scale)
      } else {
        table(text, useNA = "ifany")
      }
      tibble::tibble(
        design = "unpaired",
        side = side,
        scorer_id = scorers$scorer_id[[i]],
        scorer_version = scorers$version[[i]],
        scale = scorers$scale[[i]],
        value = if (numeric) NA_character_ else names(counts),
        n = as.integer(counts),
        n_off_scale = if (numeric) sum(!on_scale) else NA_integer_,
        median = if (numeric && any(on_scale)) {
          stats::median(as.numeric(unlist(values[on_scale])))
        } else {
          NA_real_
        }
      )
    })
    pairing_bind(c(
      list(tibble::tibble(
        design = character(),
        side = character(),
        scorer_id = character(),
        scorer_version = character(),
        scale = character(),
        value = character(),
        n = integer(),
        n_off_scale = integer(),
        median = double()
      )),
      rows
    ))
  })
  patterns <- by_side(function(x, side) {
    assessments <- x$analysis$assessments
    scans <- x$settings$scans
    status <- function(values) {
      vapply(
        scans,
        \(scan) sum(assessments$status[assessments$scan == scan] %in% values),
        integer(1),
        USE.NAMES = FALSE
      )
    }
    tibble::tibble(
      design = "unpaired",
      side = side,
      scan = scans,
      n_trajectories = nrow(trajectory_info(x$bundle)),
      n_with_findings = status("assessed_with_findings"),
      n_without_findings = status(c("assessed_no_findings", "not_applicable")),
      n_not_assessed = status(c("insufficient_evidence", "execution_failure"))
    )
  })
  resources <- by_side(function(x, side) {
    measures <- x$analysis$measures
    names <- unique(measures$measure)
    rows <- lapply(names, function(measure) {
      m <- measures[measures$measure == measure, ]
      known <- !is.na(m$value) & m$coverage == "complete"
      units <- unique(m$unit[known])
      tibble::tibble(
        design = "unpaired",
        side = side,
        measure = measure,
        measure_unit = if (length(units) <= 1L) m$unit[[1L]] else NA_character_,
        n_trajectories = nrow(m),
        n_recorded = sum(known),
        n_unknown = sum(!known),
        median = if (length(units) == 1L) {
          stats::median(m$value[known])
        } else {
          NA_real_
        }
      )
    })
    pairing_bind(rows)
  })
  list(outcomes = outcomes, patterns = patterns, resources = resources)
}

comparison_gap_bullets <- function(x) {
  outcomes <- x$outcomes
  patterns <- x$patterns
  resources <- x$resources
  recorded <- !is.na(resources$baseline_value) |
    !is.na(resources$candidate_value)
  outcome_gap <- function(status) sum(outcomes$compatibility == status)
  counts <- c(
    "trajector{?y/ies} missing a key value" = nrow(x$pairing$missing_key),
    "trajector{?y/ies} duplicated under the key" = nrow(x$pairing$duplicates),
    "trajector{?y/ies} only in the baseline" = nrow(x$pairing$only_baseline),
    "trajector{?y/ies} only in the candidate" = nrow(x$pairing$only_candidate),
    "outcome{?s} with an incompatible scorer" = outcome_gap("incompatible"),
    "outcome{?s} with an unknown scorer" = outcome_gap("unknown"),
    "outcome{?s} missing on a side" = outcome_gap("missing"),
    "outcome value{?s} off the scorer's scale" = if (
      x$design$type == "unpaired"
    ) {
      sum(x$summaries$outcomes$n_off_scale, na.rm = TRUE)
    } else {
      sum(outcomes$compatibility == "compatible" & !outcomes$comparable)
    },
    "pattern check{?s} not comparable (not assessed, not run, or changed scanner)" = sum(
      patterns$change == "not_comparable"
    ),
    "resource value{?s} recorded on a side but not comparable" = sum(
      !resources$comparable & recorded
    )
  )
  counts <- counts[counts > 0L]
  if (!length(counts)) {
    return(c(
      "v" = "Every trajectory, outcome, pattern, and recorded resource value was compared."
    ))
  }
  stats::setNames(
    vapply(
      seq_along(counts),
      \(i) cli::pluralize(paste("{counts[[i]]}", names(counts)[[i]])),
      ""
    ),
    rep("!", length(counts))
  )
}

comparison_summary_lines <- function(summaries) {
  uncertainty <- function(row, label) {
    if (row$method != "exact sign test") {
      return("counts only")
    }
    sprintf(
      "%s of changed cases %s (%s%% interval %s to %s; exact sign test p = %s)",
      format(round(row[[paste0("share_", label)]], 2), nsmall = 2),
      label,
      format(100 * row$conf_level),
      format(round(row$conf_low, 2), nsmall = 2),
      format(round(row$conf_high, 2), nsmall = 2),
      format(signif(row$p_value, 2))
    )
  }
  outcomes <- summaries$outcomes
  lines <- vapply(
    seq_len(nrow(outcomes)),
    function(i) {
      row <- outcomes[i, ]
      version <- if (is.na(row$scorer_version)) {
        "unknown"
      } else {
        comparison_short(row$scorer_version)
      }
      if (row$n == 0L) {
        return(sprintf(
          "Outcome %s (version %s): no comparable outcome; %d not comparable",
          comparison_short(row$scorer_id),
          version,
          row$n_not_comparable
        ))
      }
      sprintf(
        "Outcome %s (version %s): %d improved, %d regressed, %d unchanged; %s",
        comparison_short(row$scorer_id),
        version,
        row$n_improved,
        row$n_regressed,
        row$n_unchanged,
        uncertainty(row, "improved")
      )
    },
    ""
  )
  patterns <- summaries$patterns
  patterns <- patterns[
    patterns$n_appeared +
      patterns$n_resolved +
      patterns$n_no_longer_applicable +
      patterns$n_newly_applicable >
      0L,
  ]
  lines <- c(
    lines,
    vapply(
      seq_len(nrow(patterns)),
      function(i) {
        row <- patterns[i, ]
        applicability <- c(
          "no longer applicable in %d" = row$n_no_longer_applicable,
          "newly applicable in %d" = row$n_newly_applicable
        )
        applicability <- applicability[applicability > 0L]
        sprintf(
          "Pattern %s: appeared in %d, resolved in %d, persisted in %d%s; %s",
          comparison_plain(row$scan),
          row$n_appeared,
          row$n_resolved,
          row$n_persisted,
          paste0(
            sprintf(", %s", sprintf(names(applicability), applicability)),
            collapse = ""
          ),
          uncertainty(row, "resolved")
        )
      },
      ""
    )
  )
  resources <- summaries$resources
  resources <- resources[resources$n_increased + resources$n_decreased > 0L, ]
  lines <- c(
    lines,
    vapply(
      seq_len(nrow(resources)),
      function(i) {
        row <- resources[i, ]
        sprintf(
          "Resource %s: increased in %d, decreased in %d, median change %s %s; %s",
          row$measure,
          row$n_increased,
          row$n_decreased,
          format(row$median_delta),
          row$measure_unit,
          uncertainty(row, "increased")
        )
      },
      ""
    )
  )
  if (!length(lines)) {
    lines <- "No recorded change in any compared outcome, pattern, or resource."
  }
  # Recorded scorer labels can be source text with braces; print them as data.
  gsub("([{}])", "\\1\\1", lines)
}

comparison_short <- function(x, width = 40L) {
  x <- gsub("\\s+", " ", comparison_plain(x))
  ifelse(nchar(x) > width, paste0(substr(x, 1L, width - 3L), "..."), x)
}

# Recorded labels are data: no terminal escape or control character in them
# may reach the console.
comparison_plain <- function(x) {
  gsub("[[:cntrl:]]", " ", cli::ansi_strip(x))
}
