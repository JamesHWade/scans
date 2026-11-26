#' Pair baseline and candidate trajectories on an explicit key
#'
#' `pair_trajectories()` matches the trajectories of two saved investigations
#' case by case. It reads recorded identities only: it never runs a solver, a
#' scorer, or a model, and it never matches by position. Every trajectory of
#' both investigations appears in exactly one of the returned tables.
#'
#' @section Cases and keys:
#' A case is one sample of one dataset version. `by` names the recorded
#' identity columns of [trajectory_info()] that identify it and must include
#' `"sample_id"`. Add `"task_id"` when an investigation holds several tasks.
#' Add `"epoch"` when a sample was run more than once: epoch `k` of the
#' baseline then pairs with epoch `k` of the candidate. Without `"epoch"` in
#' the key, repeated epochs of a sample are duplicates and are reported, not
#' reduced to one.
#'
#' vitals does not record a dataset version, so the caller declares it. Cases
#' pair only when both sides declare the same version. A side without a
#' declared version has every trajectory reported as missing a key value.
#'
#' @section Scorers:
#' Outcomes pair only when scorer identity, version, scale, and direction
#' agree on both sides. vitals records a scorer label (the expression passed
#' as `scorer`) and, for an ordered factor score, its levels in order. It
#' records no scorer version and no direction for numeric or logical scores.
#' Declare what the record lacks with [comparison_scorer()]. An undeclared
#' attribute stays unknown, and an unknown or disagreeing scorer pair is
#' reported in `outcome_pairs` with its reason. It is never coerced.
#'
#' @param baseline,candidate Saved investigations from
#'   [investigation_snapshot()] or [read_investigation()]. A saved
#'   investigation carries the snapshot identity and the frozen analysis that
#'   a comparison cites.
#' @param by Key columns: `"sample_id"`, optionally with `"task_id"` and
#'   `"epoch"`. There is no default.
#' @param dataset_version The dataset version of both sides as one string, or
#'   a character vector named `baseline` and `candidate`. `NULL` leaves it
#'   undeclared.
#' @param scorers Scorer declarations from [comparison_scorer()]: a list
#'   applied to both sides, or a list with `baseline` and `candidate` lists
#'   when the sides differ.
#'
#' @returns A `scans_pairing` value: a list of ordinary tibbles and the two
#'   investigations.
#'
#'   * `key`: `by` and the declared dataset versions.
#'   * `snapshots`: one row per side with snapshot and revision identifiers.
#'   * `pairs`: one row per matched case with both trajectory identities.
#'   * `only_baseline`, `only_candidate`: cases present on one side.
#'   * `duplicates`: every trajectory whose key occurs more than once on
#'     either side, with the count on each side.
#'   * `missing_key`: trajectories lacking a key value, and which one.
#'   * `scorers`: recorded and declared scorer attributes per side.
#'   * `outcome_pairs`: one row per pair and scorer with `compatibility`
#'     (`"compatible"`, `"incompatible"`, `"unknown"`, or `"missing"`) and
#'     the reason.
#' @export
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(
#'     trajectory_id = c("a", "b"),
#'     sample_id = c("case-1", "case-2"),
#'     source_type = "manual",
#'     status = "completed"
#'   ),
#'   data.frame(),
#'   data.frame()
#' )
#' if (rlang::is_installed(c("digest", "jsonlite"))) {
#'   baseline <- investigation_snapshot(bundle, application = "Baseline")
#'   candidate <- investigation_snapshot(
#'     bundle,
#'     trajectory_ids = "a",
#'     application = "Candidate"
#'   )
#'   pairing <- pair_trajectories(
#'     baseline,
#'     candidate,
#'     by = "sample_id",
#'     dataset_version = "2026-11"
#'   )
#'   pairing$pairs
#'   pairing$only_baseline
#' }
pair_trajectories <- function(
  baseline,
  candidate,
  by,
  dataset_version = NULL,
  scorers = NULL
) {
  call <- rlang::current_env()
  pairing_check_investigation(baseline, "baseline", call)
  pairing_check_investigation(candidate, "candidate", call)
  by <- pairing_check_by(by, call)
  versions <- pairing_check_dataset_version(dataset_version, call)
  declared <- pairing_check_scorers(scorers, call)
  sides <- list(baseline = baseline, candidate = candidate)

  keyed <- lapply(pairing_sides, function(side) {
    pairing_keyed(sides[[side]], by, versions[[side]])
  })
  names(keyed) <- pairing_sides
  complete <- lapply(keyed, function(x) x[lengths(x$missing) == 0L, ])
  counts <- lapply(complete, function(x) table(x$key))
  keys <- union(complete$baseline$key, complete$candidate$key)
  n <- lapply(counts, function(x) {
    out <- as.integer(x[keys])
    out[is.na(out)] <- 0L
    stats::setNames(out, keys)
  })
  duplicated_keys <- keys[n$baseline > 1L | n$candidate > 1L]
  paired_keys <- keys[n$baseline == 1L & n$candidate == 1L]
  base <- complete$baseline
  cand <- complete$candidate
  base_rows <- which(base$key %in% paired_keys)
  cand_rows <- match(base$key[base_rows], cand$key)

  pairs <- tibble::tibble(
    pair_id = sprintf("pair-%06d", seq_along(base_rows)),
    pairing_key_columns(base[base_rows, ]),
    baseline_trajectory_id = base$trajectory_id[base_rows],
    candidate_trajectory_id = cand$trajectory_id[cand_rows]
  )
  only <- function(x, other) {
    rows <- !x$key %in% other$key & !x$key %in% duplicated_keys
    tibble::tibble(
      pairing_key_columns(x[rows, ]),
      trajectory_id = x$trajectory_id[rows]
    )
  }
  duplicates <- pairing_bind(lapply(pairing_sides, function(side) {
    x <- complete[[side]]
    x <- x[x$key %in% duplicated_keys, ]
    tibble::tibble(
      side = rep(side, nrow(x)),
      pairing_key_columns(x),
      trajectory_id = x$trajectory_id,
      n_baseline = unname(n$baseline[x$key]),
      n_candidate = unname(n$candidate[x$key])
    )
  }))
  missing_key <- pairing_bind(lapply(pairing_sides, function(side) {
    x <- keyed[[side]]
    x <- x[lengths(x$missing) > 0L, ]
    tibble::tibble(
      side = rep(side, nrow(x)),
      trajectory_id = x$trajectory_id,
      pairing_key_columns(x),
      missing = x$missing
    )
  }))

  scorer_table <- pairing_bind(lapply(pairing_sides, function(side) {
    pairing_scorers(sides[[side]], side, declared[[side]], call)
  }))
  pairing_check_declared_used(declared, scorer_table, call)

  structure(
    list(
      key = list(by = by, dataset_version = versions),
      snapshots = tibble::tibble(
        side = pairing_sides,
        application = pairing_manifest(sides, "application"),
        snapshot_id = pairing_manifest(sides, "snapshot_id"),
        revision_id = pairing_manifest(sides, "revision_id"),
        dataset_version = unname(versions[pairing_sides]),
        n_trajectories = unname(vapply(keyed, nrow, integer(1)))
      ),
      pairs = pairs,
      only_baseline = only(base, cand),
      only_candidate = only(cand, base),
      duplicates = duplicates,
      missing_key = missing_key,
      scorers = scorer_table,
      outcome_pairs = pairing_outcome_pairs(pairs, sides, scorer_table),
      baseline = baseline,
      candidate = candidate
    ),
    class = "scans_pairing"
  )
}

#' Declare what a recorded scorer measures
#'
#' A vitals result records a scorer label and, for an ordered factor score,
#' its levels. It does not record a scorer version, and it records no
#' direction for numeric, logical, or character scores. `comparison_scorer()`
#' declares those facts so [pair_trajectories()] can decide whether two
#' recorded outcomes came from the same scorer. A declaration is the caller's
#' assertion; scans cannot verify it. A declaration that contradicts the
#' record is an error.
#'
#' @param scorer The scorer label recorded in `trajectory_evaluations()$scorer`.
#' @param version The scorer version: any string that changes whenever the
#'   scorer's code, prompt, grading model, or rubric changes.
#' @param scale `"binary"`, `"ordered"`, or `"numeric"`.
#' @param direction Whether a `"higher"` or `"lower"` value is better. An
#'   undeclared ordered factor score is read as `"higher"`, the vitals
#'   convention that levels run from worst to best; declare `"lower"` when a
#'   scorer's levels run the other way. The declared `levels` must still match
#'   the recorded level order.
#' @param levels For binary and ordered scales, the possible values from
#'   lowest to highest. Not used for a numeric scale.
#' @param id The scorer's identity. Defaults to the recorded label. Supply one
#'   shared `id` when the same scorer was recorded under different labels.
#'
#' @returns A `scans_comparison_scorer` value.
#' @export
#' @examples
#' comparison_scorer(
#'   "detect_exact()",
#'   version = "vitals-0.4.1",
#'   scale = "binary",
#'   direction = "higher",
#'   levels = c("I", "C")
#' )
comparison_scorer <- function(
  scorer,
  version,
  scale = c("binary", "ordered", "numeric"),
  direction = c("higher", "lower"),
  levels = NULL,
  id = scorer
) {
  for (name in c("scorer", "version", "id")) {
    value <- get(name)
    if (!rlang::is_string(value) || is.na(value) || !nzchar(trimws(value))) {
      pairing_abort("{.arg {name}} must be one non-empty string.")
    }
  }
  scale <- rlang::arg_match(scale)
  direction <- rlang::arg_match(direction)
  if (scale == "numeric") {
    if (!is.null(levels)) {
      pairing_abort("A numeric scale has no {.arg levels}.")
    }
  } else if (
    !is.character(levels) ||
      anyNA(levels) ||
      anyDuplicated(levels) ||
      length(levels) < 2L ||
      (scale == "binary" && length(levels) != 2L)
  ) {
    pairing_abort(c(
      "{.arg levels} must list the scale's distinct values from lowest to highest.",
      "i" = "A binary scale has exactly two levels."
    ))
  }
  structure(
    list(
      scorer = scorer,
      id = id,
      version = version,
      scale = scale,
      direction = direction,
      levels = levels
    ),
    class = "scans_comparison_scorer"
  )
}

#' @export
print.scans_pairing <- function(x, ...) {
  cli::cli_text(
    "{.cls scans_pairing} on {.field {x$key$by}}, dataset version {.val {unique(x$key$dataset_version)}}"
  )
  cli::cli_text("Baseline snapshot: {x$snapshots$snapshot_id[[1L]]}")
  cli::cli_text("Candidate snapshot: {x$snapshots$snapshot_id[[2L]]}")
  cli::cli_bullets(pairing_exclusion_bullets(x))
  cli::cli_text("{nrow(x$pairs)} matched case{?s}")
  invisible(x)
}

pairing_sides <- c("baseline", "candidate")

pairing_abort <- function(message, call = rlang::caller_env()) {
  scans_abort(
    message,
    class = "scans_error_pairing",
    call = call,
    .envir = rlang::caller_env()
  )
}

pairing_check_investigation <- function(x, arg, call) {
  if (is_trajectory_bundle(x)) {
    pairing_abort(
      c(
        "{.arg {arg}} must be a saved investigation, not a {.cls TrajectoryBundle}.",
        "i" = "A bundle has no snapshot identity or frozen analysis. Call {.fn investigation_snapshot} first."
      ),
      call
    )
  }
  if (!inherits(x, "scans_investigation")) {
    pairing_abort(
      "{.arg {arg}} must be a saved investigation from {.fn investigation_snapshot}.",
      call
    )
  }
  investigation_validate(x)
}

pairing_check_by <- function(by, call) {
  valid <- c("task_id", "sample_id", "epoch")
  if (
    !is.character(by) ||
      anyNA(by) ||
      anyDuplicated(by) ||
      !all(by %in% valid) ||
      !"sample_id" %in% by
  ) {
    pairing_abort(
      c(
        "{.arg by} must name the key columns explicitly.",
        "i" = "Use {.val sample_id}, optionally with {.val task_id} and {.val epoch}."
      ),
      call
    )
  }
  valid[valid %in% by]
}

pairing_check_dataset_version <- function(x, call) {
  out <- c(baseline = NA_character_, candidate = NA_character_)
  if (is.null(x)) {
    return(out)
  }
  valid <- is.character(x) && !anyNA(x) && all(nzchar(trimws(x)))
  if (valid && length(x) == 1L && is.null(names(x))) {
    out[] <- x
    return(out)
  }
  if (valid && length(x) == 2L && setequal(names(x), pairing_sides)) {
    return(x[pairing_sides])
  }
  pairing_abort(
    "{.arg dataset_version} must be one string, or a character vector named {.val baseline} and {.val candidate}.",
    call
  )
}

pairing_check_scorers <- function(x, call) {
  is_declarations <- function(value) {
    is.list(value) &&
      all(vapply(value, inherits, logical(1), "scans_comparison_scorer"))
  }
  if (is.null(x)) {
    x <- list()
  }
  if (inherits(x, "scans_comparison_scorer")) {
    x <- list(x)
  }
  if (is_declarations(x)) {
    out <- list(baseline = x, candidate = x)
  } else if (
    trajectory_is_named_list(x) &&
      all(names(x) %in% pairing_sides) &&
      all(vapply(x, is_declarations, logical(1)))
  ) {
    out <- list(
      baseline = x$baseline %||% list(),
      candidate = x$candidate %||% list()
    )
  } else {
    pairing_abort(
      "{.arg scorers} must be a list of {.fn comparison_scorer} declarations, or a list with {.val baseline} and {.val candidate} lists.",
      call
    )
  }
  for (side in pairing_sides) {
    labels <- vapply(out[[side]], `[[`, "", "scorer")
    if (anyDuplicated(labels)) {
      pairing_abort(
        "Scorer {.val {labels[duplicated(labels)]}} is declared more than once for the {side}.",
        call
      )
    }
  }
  out
}

pairing_check_declared_used <- function(declared, scorer_table, call) {
  labels <- unique(unlist(lapply(declared, function(x) {
    vapply(x, `[[`, "", "scorer")
  })))
  unused <- setdiff(labels, scorer_table$scorer)
  if (length(unused)) {
    pairing_abort(
      c(
        "Declared scorer{?s} {.val {unused}} {?is/are} not recorded on either side.",
        "i" = "Recorded scorer labels: {.val {unique(scorer_table$scorer)}}."
      ),
      call
    )
  }
}

pairing_bind <- function(rows) {
  tibble::as_tibble(vctrs::vec_rbind(!!!rows))
}

pairing_manifest <- function(sides, field) {
  unname(vapply(sides, \(x) x$manifest[[field]], ""))
}

pairing_keyed <- function(x, by, dataset_version) {
  info <- trajectory_info(x$bundle)
  out <- tibble::tibble(
    trajectory_id = info$trajectory_id,
    dataset_version = rep(dataset_version, nrow(info)),
    task_id = info$task_id,
    sample_id = info$sample_id,
    epoch = info$epoch
  )
  unused <- setdiff(c("task_id", "sample_id", "epoch"), by)
  out[unused] <- lapply(out[unused], function(column) column[NA])
  required <- c("dataset_version", by)
  absent <- vapply(required, \(column) is.na(out[[column]]), logical(nrow(out)))
  absent <- matrix(
    absent,
    nrow = nrow(out),
    ncol = length(required),
    dimnames = list(NULL, required)
  )
  out$missing <- lapply(seq_len(nrow(out)), \(i) required[absent[i, ]])
  # Length-prefixed parts, so no separator in an identity can merge two keys.
  # Byte lengths need one encoding: equal strings can differ in bytes.
  parts <- lapply(required, function(column) {
    value <- enc2utf8(as.character(out[[column]]))
    paste0(nchar(value, type = "bytes"), ":", value)
  })
  out$key <- do.call(paste0, parts)
  out
}

pairing_key_columns <- function(x) {
  x[c("dataset_version", "task_id", "sample_id", "epoch")]
}

pairing_scorers <- function(x, side, declared, call) {
  evaluations <- trajectory_evaluations(x$bundle)
  labels <- unique(evaluations$scorer)
  names(declared) <- vapply(declared, `[[`, "", "scorer")
  rows <- lapply(labels, function(label) {
    values <- evaluations$value[
      if (is.na(label)) {
        is.na(evaluations$scorer)
      } else {
        !is.na(evaluations$scorer) & evaluations$scorer == label
      }
    ]
    recorded <- pairing_recorded_scale(values)
    declaration <- if (is.na(label)) NULL else declared[[label]]
    resolved <- pairing_resolve_scorer(label, recorded, declaration, side, call)
    fields <- c("scorer_id", "version", "scale", "direction")
    tibble::tibble(
      side = side,
      scorer = label,
      scorer_id = resolved$scorer_id,
      version = resolved$version,
      scale = resolved$scale,
      direction = resolved$direction,
      levels = list(resolved$levels),
      declared = !is.null(declaration),
      n_evaluations = length(values),
      unknown = list(fields[is.na(unlist(resolved[fields]))])
    )
  })
  if (!length(rows)) {
    return(pairing_empty_scorers())
  }
  pairing_bind(rows)
}

pairing_empty_scorers <- function() {
  tibble::tibble(
    side = character(),
    scorer = character(),
    scorer_id = character(),
    version = character(),
    scale = character(),
    direction = character(),
    levels = list(),
    declared = logical(),
    n_evaluations = integer(),
    unknown = list()
  )
}

# What the saved evaluation values establish on their own. An ordered factor
# records its levels and their order; vitals scorers use that order from worst
# to best, so its direction defaults to "higher". Numeric and logical scores record a scale but no direction. A
# missing score records nothing, so it neither establishes nor hides a scale.
# Scores of several kinds, or ordered factors with different levels, record
# conflicting scales: the scale stays unknown and `conflict` names them.
pairing_recorded_scale <- function(values) {
  out <- list(
    scale = NA_character_,
    direction = NA_character_,
    levels = NULL,
    conflict = NULL
  )
  values <- Filter(Negate(pairing_missing_score), values)
  kinds <- unique(lapply(values, pairing_score_kind))
  if (length(kinds) > 1L) {
    out$conflict <- vapply(kinds, pairing_score_kind_label, "")
    return(out)
  }
  if (!length(kinds)) {
    return(out)
  }
  kind <- kinds[[1L]]
  if (kind[[1L]] == "ordered") {
    out$levels <- kind[-1L]
    out$scale <- if (length(out$levels) == 2L) "binary" else "ordered"
    out$direction <- "higher"
  } else if (kind[[1L]] == "logical") {
    out$scale <- "binary"
    out$levels <- c("FALSE", "TRUE")
  } else if (kind[[1L]] == "numeric") {
    out$scale <- "numeric"
  }
  out
}

pairing_score_kind <- function(x) {
  scalar <- length(x) == 1L && !is.na(x)
  if (is.ordered(x)) {
    c("ordered", levels(x))
  } else if (scalar && is.logical(x)) {
    "logical"
  } else if (scalar && is.numeric(x) && !is.factor(x)) {
    "numeric"
  } else {
    "other"
  }
}

pairing_score_kind_label <- function(kind) {
  if (kind[[1L]] != "ordered") {
    return(kind[[1L]])
  }
  paste0("ordered factor (", paste(kind[-1L], collapse = " < "), ")")
}

pairing_missing_score <- function(x) {
  length(x) == 0L ||
    (rlang::is_scalar_atomic(x) && !is.factor(x) && is.na(x))
}

pairing_resolve_scorer <- function(label, recorded, declaration, side, call) {
  out <- list(
    scorer_id = label,
    version = NA_character_,
    scale = recorded$scale,
    direction = recorded$direction,
    levels = recorded$levels
  )
  if (is.null(declaration)) {
    return(out)
  }
  conflict <- function(field, recorded_value, declared_value) {
    pairing_abort(
      c(
        "The declaration for scorer {.val {label}} contradicts the {side} record.",
        "x" = "Recorded {field}: {.val {recorded_value}}; declared: {.val {declared_value}}."
      ),
      call
    )
  }
  if (!is.null(recorded$conflict)) {
    pairing_abort(
      c(
        "The {side} records conflicting scores for scorer {.val {label}}, so no declaration can match them.",
        "x" = "Recorded: {.val {recorded$conflict}}."
      ),
      call
    )
  }
  # The recorded direction of an ordered factor is the vitals convention, not
  # a recorded fact, so a declaration may override it.
  if (!is.na(recorded$scale) && !identical(recorded$scale, declaration$scale)) {
    conflict("scale", recorded$scale, declaration$scale)
  }
  if (
    !is.null(recorded$levels) && !identical(recorded$levels, declaration$levels)
  ) {
    conflict("levels", recorded$levels, declaration$levels)
  }
  list(
    scorer_id = declaration$id,
    version = declaration$version,
    scale = declaration$scale,
    direction = declaration$direction,
    levels = declaration$levels
  )
}

pairing_outcome_pairs <- function(pairs, sides, scorer_table) {
  evaluations <- lapply(pairing_sides, function(side) {
    out <- trajectory_evaluations(sides[[side]]$bundle)
    scorers <- scorer_table[scorer_table$side == side, ]
    out$scorer_row <- match(out$scorer, scorers$scorer)
    out$scorer_id <- scorers$scorer_id[out$scorer_row]
    list(evaluations = out, scorers = scorers)
  })
  names(evaluations) <- pairing_sides
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    side_rows <- lapply(pairing_sides, function(side) {
      x <- evaluations[[side]]$evaluations
      x[x$trajectory_id == pairs[[paste0(side, "_trajectory_id")]][[i]], ]
    })
    matched <- pairing_match_evaluations(
      side_rows[[1L]],
      side_rows[[2L]],
      evaluations$baseline$scorers,
      evaluations$candidate$scorers
    )
    tibble::tibble(pairs[rep(i, nrow(matched)), ], matched)
  })
  if (!length(rows)) {
    rows <- list(tibble::tibble(
      pairs,
      pairing_match_row(NULL, NULL, NULL, NULL, "", "")[0L, ]
    ))
  }
  pairing_bind(rows)
}

pairing_match_evaluations <- function(
  baseline,
  candidate,
  baseline_scorers,
  candidate_scorers
) {
  row <- function(b, c, compatibility, reason) {
    pairing_match_row(
      if (length(b)) baseline[b, ],
      if (length(c)) candidate[c, ],
      baseline_scorers,
      candidate_scorers,
      compatibility,
      reason
    )
  }
  if (!nrow(baseline) && !nrow(candidate)) {
    return(row(NULL, NULL, "missing", "No evaluation on either side."))
  }
  repeated <- function(x) {
    !is.na(x$scorer_id) &
      x$scorer_id %in% x$scorer_id[duplicated(x$scorer_id)]
  }
  out <- list()
  b_left <- rep(TRUE, nrow(baseline))
  c_left <- rep(TRUE, nrow(candidate))
  b_repeated <- repeated(baseline)
  c_repeated <- repeated(candidate)
  for (b in which(!is.na(baseline$scorer_id) & !b_repeated)) {
    c <- which(candidate$scorer_id == baseline$scorer_id[[b]] & !c_repeated)
    if (length(c) != 1L) {
      next
    }
    verdict <- pairing_scorer_verdict(
      baseline_scorers[baseline$scorer_row[[b]], ],
      candidate_scorers[candidate$scorer_row[[c]], ]
    )
    out <- c(out, list(row(b, c, verdict$compatibility, verdict$reason)))
    b_left[[b]] <- FALSE
    c_left[[c]] <- FALSE
  }
  b_rest <- which(b_left)
  c_rest <- which(c_left)
  if (
    length(b_rest) == 1L &&
      length(c_rest) == 1L &&
      !anyNA(c(baseline$scorer_id[b_rest], candidate$scorer_id[c_rest])) &&
      !b_repeated[b_rest] &&
      !c_repeated[c_rest] &&
      !baseline$scorer_id[b_rest] %in% candidate_scorers$scorer_id &&
      !candidate$scorer_id[c_rest] %in% baseline_scorers$scorer_id
  ) {
    out <- c(
      out,
      list(row(
        b_rest,
        c_rest,
        "incompatible",
        "Scorer identity differs."
      ))
    )
    return(pairing_bind(out))
  }
  single <- function(index, side) {
    x <- if (side == "baseline") baseline else candidate
    repeats <- if (side == "baseline") b_repeated else c_repeated
    other <- setdiff(pairing_sides, side)
    other_repeated <- if (side == "baseline") {
      candidate$scorer_id[c_repeated]
    } else {
      baseline$scorer_id[b_repeated]
    }
    lapply(index, function(i) {
      verdict <- if (is.na(x$scorer_id[[i]])) {
        list("unknown", "The scorer was not recorded.")
      } else if (repeats[[i]]) {
        list(
          "unknown",
          paste0("Several ", side, " evaluations share this scorer.")
        )
      } else if (x$scorer_id[[i]] %in% other_repeated) {
        list(
          "unknown",
          paste0("The ", other, " has several evaluations by this scorer.")
        )
      } else {
        list(
          "missing",
          paste0("No evaluation by this scorer on the ", other, ".")
        )
      }
      if (side == "baseline") {
        row(i, NULL, verdict[[1L]], verdict[[2L]])
      } else {
        row(NULL, i, verdict[[1L]], verdict[[2L]])
      }
    })
  }
  pairing_bind(c(
    out,
    single(b_rest, "baseline"),
    single(c_rest, "candidate")
  ))
}

pairing_match_row <- function(
  baseline,
  candidate,
  baseline_scorers,
  candidate_scorers,
  compatibility,
  reason
) {
  field <- function(x, name) {
    if (is.null(x)) NA_character_ else x[[name]][[1L]]
  }
  scorer <- function(x, scorers) {
    if (is.null(x) || is.na(x$scorer_row[[1L]])) {
      return(NULL)
    }
    scorers[x$scorer_row[[1L]], ]
  }
  b <- scorer(baseline, baseline_scorers)
  c <- scorer(candidate, candidate_scorers)
  described <- b %||% c
  agreed <- identical(compatibility, "compatible")
  tibble::tibble(
    scorer_id = field(described, "scorer_id"),
    scorer_version = if (agreed) b$version else NA_character_,
    scale = if (agreed) b$scale else NA_character_,
    direction = if (agreed) b$direction else NA_character_,
    levels = list(if (agreed) b$levels[[1L]]),
    baseline_scorer = field(baseline, "scorer"),
    candidate_scorer = field(candidate, "scorer"),
    baseline_evaluation_id = field(baseline, "evaluation_id"),
    candidate_evaluation_id = field(candidate, "evaluation_id"),
    compatibility = compatibility,
    reason = reason
  )
}

pairing_scorer_verdict <- function(baseline, candidate) {
  fields <- c("version", "scale", "direction")
  unknown <- fields[
    vapply(fields, \(f) is.na(baseline[[f]]) || is.na(candidate[[f]]), NA)
  ]
  differs <- setdiff(fields, unknown)
  differs <- differs[
    vapply(differs, \(f) !identical(baseline[[f]], candidate[[f]]), NA)
  ]
  # Levels recorded on both sides are known even when another attribute is not.
  levels <- list(baseline$levels[[1L]], candidate$levels[[1L]])
  if (
    !identical(levels[[1L]], levels[[2L]]) &&
      (!length(c(unknown, differs)) || !any(vapply(levels, is.null, NA)))
  ) {
    differs <- c(differs, "levels")
  }
  if (length(differs)) {
    return(list(
      compatibility = "incompatible",
      reason = paste0("Scorer ", paste(differs, collapse = ", "), " differs.")
    ))
  }
  if (length(unknown)) {
    return(list(
      compatibility = "unknown",
      reason = paste0(
        "Scorer ",
        paste(unknown, collapse = ", "),
        " is not recorded or declared."
      )
    ))
  }
  list(compatibility = "compatible", reason = NA_character_)
}

pairing_exclusion_bullets <- function(x) {
  counts <- c(
    "missing a key value" = nrow(x$missing_key),
    "duplicated under the key" = nrow(x$duplicates),
    "only in the baseline" = nrow(x$only_baseline),
    "only in the candidate" = nrow(x$only_candidate)
  )
  counts <- counts[counts > 0L]
  if (!length(counts)) {
    return(c("v" = "Every trajectory is in a matched case."))
  }
  stats::setNames(
    paste0(
      counts,
      " trajector",
      ifelse(counts == 1L, "y ", "ies "),
      names(counts)
    ),
    rep("!", length(counts))
  )
}
