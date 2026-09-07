#' Capture an investigation for saving and reopening
#'
#' `investigation_snapshot()` freezes selected trajectory evidence together
#' with its summaries, findings, scanner assessments, resource measurements,
#' and analysis settings. It calls only deterministic package diagnostics.
#' Use [write_investigation()] to save the result and [scans_app()] to view it.
#'
#' The result is an ordinary list with class `scans_investigation`. Its
#' `bundle` is a [TrajectoryBundle]; `analysis` holds four tibbles named
#' `summaries`, `findings`, `assessments`, and `measures`. `settings` records
#' scanner selection and thresholds. `view` records browser state. `manifest`
#' records the application, capture details, package/scanner versions, content
#' policy, snapshot identifier, and revision identifier.
#'
#' The snapshot identifier covers selected evidence, application, and capture
#' details. The revision identifier also covers analysis, settings, view, and
#' an optional parent revision. Repeating the same capture is stable; creation
#' time does not affect either identifier. Changing evidence creates a new
#' snapshot. Changing settings or view creates a new revision of that snapshot.
#'
#' @section Content policy:
#' The initial `retained-v1` policy includes all retained fields of selected
#' trajectories: text, tool arguments/results, evaluations, and metadata.
#' Existing adapter redactions and loss records remain intact. Saving does not
#' perform additional anonymization or guarantee that sensitive text is absent.
#'
#' Unselected trajectory records and annotation history are omitted. Unassigned
#' capture-wide losses remain included. If a selected trajectory's parent is
#' excluded, the link becomes an explicit loss retaining its original identity.
#' The JSON format supports base vectors, lists, data frames, factors, dates,
#' and times. Unsupported objects are rejected with a field path.
#'
#' @param x A [TrajectoryBundle].
#' @param trajectory_ids Trajectory identifiers to include. `NULL` includes
#'   all trajectories. `character()` creates an empty selection. Selection
#'   preserves the bundle's row order.
#' @param application One non-empty application name.
#' @param source A named list of source identity and capture details, such as
#'   the `read_info` attribute from [read_connect_traces()]. It must contain
#'   data only; never supply credentials or a loader function.
#' @inheritParams assess_trajectory_scans
#' @param view A named list of browser settings. Supported names are `query`,
#'   `source_type` (`NULL` means all), `status` (`NULL` means all, `NA` means
#'   unknown), `findings_only`, `annotated_only`, `annotation_ids`, `pattern`,
#'   `sort` (`"newest"`, `"oldest"`, `"findings"`, or `"longest"`),
#'   `selected_trajectory_id`, `tab` (`"application"` or `"trajectory"`), and
#'   `priority` (`"elapsed"`, `"tokens"`, or `"findings"`). Annotation IDs
#'   record filter membership only, not judgments or reviewer text.
#' @param previous An optional earlier investigation. Its revision identifier
#'   is recorded as the parent; its settings are not inherited. When `x` is
#'   the earlier investigation's bundle, its prior omission count is retained
#'   and any newly excluded trajectories are added to that count.
#'
#' @returns A `scans_investigation` value. Its identifiers are verified when
#'   writing or opening it; use this constructor to create a changed revision
#'   rather than editing its fields.
#' @export
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(trajectory_id = "run-1", source_type = "manual",
#'              status = "completed"),
#'   data.frame(), data.frame()
#' )
#' saved <- investigation_snapshot(bundle, application = "Support assistant")
#' saved$analysis$assessments[c("scan", "status")]
#' saved$manifest$content_policy
investigation_snapshot <- function(
  x,
  trajectory_ids = NULL,
  application = "Trajectories",
  source = list(),
  scans = NULL,
  repeat_threshold = 2L,
  loop_threshold = 3L,
  view = list(),
  previous = NULL
) {
  investigation_check_dependencies()
  check_trajectory_bundle(x)
  settings <- list(
    scans = scan_check_selection(scans, rlang::caller_env()) %||%
      scan_registry()$scan,
    repeat_threshold = scan_check_threshold(
      repeat_threshold,
      "repeat_threshold",
      rlang::caller_env()
    ),
    loop_threshold = scan_check_threshold(
      loop_threshold,
      "loop_threshold",
      rlang::caller_env()
    )
  )
  assessment <- assess_trajectory_scans(
    x,
    scans = settings$scans,
    repeat_threshold = settings$repeat_threshold,
    loop_threshold = settings$loop_threshold
  )
  analysis <- c(
    list(summaries = summarize_trajectories(x)),
    assessment,
    list(measures = measure_trajectories(x))
  )
  investigation_build(
    x,
    trajectory_ids,
    application,
    source,
    settings,
    view,
    analysis,
    previous
  )
}

#' @export
print.scans_investigation <- function(x, ...) {
  cli::cli_text("{.cls scans_investigation} {x$manifest$application}")
  cli::cli_text(
    "{nrow(trajectory_info(x$bundle))} trajector{?y/ies} \u00b7 {nrow(x$analysis$findings)} finding{?s}"
  )
  cli::cli_text("Snapshot: {x$manifest$snapshot_id}")
  cli::cli_text(
    "Content: selected retained records; no additional anonymization"
  )
  invisible(x)
}

investigation_build <- function(
  x,
  ids,
  application,
  source,
  settings,
  view,
  analysis,
  previous = NULL,
  package_version = as.character(utils::packageVersion("scans"))
) {
  investigation_check_dependencies()
  if (
    !rlang::is_string(application) ||
      is.na(application) ||
      !nzchar(trimws(application))
  ) {
    investigation_abort("{.arg application} must be one non-empty name.")
  }
  if (!trajectory_is_named_list(source)) {
    investigation_abort("{.arg source} must be a named list of data.")
  }
  S7::validate(x)
  info <- trajectory_info(x)
  ids <- ids %||% info$trajectory_id
  if (
    !is.character(ids) ||
      anyNA(ids) ||
      anyDuplicated(ids) ||
      !all(ids %in% info$trajectory_id)
  ) {
    investigation_abort(
      "{.arg trajectory_ids} must contain unique identifiers from the bundle."
    )
  }
  ids <- info$trajectory_id[info$trajectory_id %in% ids]
  view <- investigation_view(view, ids, settings$scans)
  selected <- investigation_select(x, ids)
  analysis <- lapply(analysis, function(table) {
    table[table$trajectory_id %in% ids, , drop = FALSE]
  })
  selected_info <- trajectory_info(selected$bundle)
  analysis$summaries$parent_trajectory_id <- selected_info$parent_trajectory_id
  analysis$summaries$trajectory_depth <- scan_parent_depths(
    selected_info$trajectory_id,
    selected_info$parent_trajectory_id
  )
  analysis$summaries$n_losses <- tabulate(
    match(trajectory_losses(selected$bundle)$trajectory_id, ids),
    nbins = length(ids)
  )
  if (nrow(analysis$assessments)) {
    analysis$assessments$loss_rows <- lapply(
      analysis$assessments$loss_rows,
      function(rows) {
        unname(match(rows, selected$loss_rows))
      }
    )
    for (i in seq_len(nrow(analysis$assessments))) {
      extra <- which(
        trajectory_losses(selected$bundle)$reason ==
          "scans:selection_omission" &
          trajectory_losses(selected$bundle)$trajectory_id ==
            analysis$assessments$trajectory_id[[i]]
      )
      analysis$assessments$loss_rows[[i]] <- unique(c(
        analysis$assessments$loss_rows[[i]],
        extra
      ))
      if (length(extra)) {
        analysis$assessments$limitations[[i]] <- unique(c(
          analysis$assessments$limitations[[i]],
          "The parent trajectory was excluded from this saved selection."
        ))
      }
    }
  }
  omitted <- nrow(info) - length(ids)
  parent <- NA_character_
  if (!is.null(previous)) {
    investigation_validate(previous)
    parent <- previous$manifest$revision_id
    if (identical(S7::props(x), S7::props(previous$bundle))) {
      omitted <- omitted + previous$manifest$content_policy$omitted_trajectories
    }
  }
  versions <- unique(analysis$assessments[c("scan", "scan_version")])
  manifest <- list(
    format_version = 1L,
    created_at = as.POSIXct(Sys.time(), tz = "UTC"),
    application = application,
    source = source,
    package_version = package_version,
    scanner_versions = versions,
    content_policy = list(
      id = "retained-v1",
      included = "All retained fields of selected trajectories, their turns, events, evaluations, and relevant losses.",
      excluded = "Unselected trajectory records, annotation history, loader credentials, and live connections.",
      redaction = "Existing adapter redactions are preserved. No additional anonymization is performed.",
      omitted_trajectories = omitted
    ),
    parent_revision_id = parent
  )
  out <- structure(
    list(
      bundle = selected$bundle,
      settings = settings,
      view = view,
      analysis = analysis,
      manifest = manifest
    ),
    class = "scans_investigation"
  )
  out$manifest$snapshot_id <- investigation_snapshot_id(out)
  out$manifest$revision_id <- investigation_revision_id(out)
  investigation_validate(out)
  out
}

investigation_view <- function(view, ids, scans) {
  defaults <- list(
    query = "",
    source_type = NULL,
    status = NULL,
    findings_only = FALSE,
    annotated_only = FALSE,
    annotation_ids = character(),
    pattern = NULL,
    sort = "newest",
    selected_trajectory_id = NULL,
    tab = "application",
    priority = "elapsed"
  )
  if (
    !trajectory_is_named_list(view) || !all(names(view) %in% names(defaults))
  ) {
    investigation_abort("{.arg view} contains unsupported settings.")
  }
  defaults[names(view)] <- view
  view <- defaults
  for (name in c("query", "sort", "tab", "priority")) {
    if (!rlang::is_string(view[[name]]) || is.na(view[[name]])) {
      investigation_abort("Invalid view setting {.field {name}}.")
    }
  }
  for (name in c(
    "source_type",
    "status",
    "pattern",
    "selected_trajectory_id"
  )) {
    value <- view[[name]]
    if (
      !is.null(value) &&
        (!is.character(value) ||
          length(value) != 1L ||
          (name != "status" && is.na(value)))
    ) {
      investigation_abort("Invalid view setting {.field {name}}.")
    }
  }
  if (
    !rlang::is_bool(view$findings_only) ||
      !rlang::is_bool(view$annotated_only) ||
      !view$sort %in% c("newest", "oldest", "findings", "longest") ||
      !view$tab %in% c("application", "trajectory") ||
      !view$priority %in% c("elapsed", "tokens", "findings") ||
      !is.character(view$annotation_ids) ||
      anyNA(view$annotation_ids) ||
      anyDuplicated(view$annotation_ids) ||
      !all(view$annotation_ids %in% ids) ||
      (!is.null(view$selected_trajectory_id) &&
        !view$selected_trajectory_id %in% ids) ||
      (!is.null(view$pattern) && !view$pattern %in% scans)
  ) {
    investigation_abort(
      "The investigation view has invalid filters or trajectory references."
    )
  }
  view
}

investigation_select <- function(x, ids) {
  data <- S7::props(x)
  owners <- scan_loss_trajectory_ids(data$losses, data$turns, data$events)
  loss_rows <- which(is.na(owners) | owners %in% ids)
  data$losses <- data$losses[loss_rows, , drop = FALSE]
  for (table in c("trajectories", "turns", "events", "evaluations")) {
    data[[table]] <- data[[table]][
      data[[table]]$trajectory_id %in% ids,
      ,
      drop = FALSE
    ]
  }
  parent <- data$trajectories$parent_trajectory_id
  omitted <- which(!is.na(parent) & !parent %in% ids)
  extra <- lapply(omitted, function(i) {
    trajectory_new_loss(
      trajectory_ids(data$trajectories$trajectory_id[[i]]),
      "parent_trajectory_id",
      "scans:selection_omission",
      "The parent trajectory was excluded from this saved selection.",
      list(parent_trajectory_id = parent[[i]])
    )
  })
  data$trajectories$parent_trajectory_id[omitted] <- NA_character_
  if (length(extra)) {
    data$losses <- trajectory_bind_rows(list(
      data$losses,
      trajectory_loss_table(extra)
    ))
  }
  data$schema_version <- NULL
  list(bundle = do.call(TrajectoryBundle, data), loss_rows = loss_rows)
}

investigation_payload <- function(x) {
  out <- unclass(x)
  out$bundle <- S7::props(x$bundle)
  out
}

investigation_snapshot_id <- function(x) {
  investigation_hash(list(
    bundle = S7::props(x$bundle),
    application = x$manifest$application,
    source = x$manifest$source
  ))
}

investigation_revision_id <- function(x) {
  payload <- investigation_payload(x)
  payload$manifest[c("created_at", "revision_id")] <- NULL
  investigation_hash(payload)
}

investigation_validate <- function(x) {
  if (
    !inherits(x, "scans_investigation") ||
      !is.list(x) ||
      !setequal(
        names(x),
        c("bundle", "settings", "view", "analysis", "manifest")
      ) ||
      !is_trajectory_bundle(x$bundle)
  ) {
    investigation_abort("{.arg x} must be a saved investigation.")
  }
  S7::validate(x$bundle)
  ids <- trajectory_info(x$bundle)$trajectory_id
  settings <- x$settings
  if (
    !trajectory_is_named_list(settings) ||
      !setequal(
        names(settings),
        c("scans", "repeat_threshold", "loop_threshold")
      ) ||
      !is.character(settings$scans) ||
      anyNA(settings$scans) ||
      anyDuplicated(settings$scans) ||
      !all(nzchar(settings$scans))
  ) {
    investigation_abort(
      "The saved scanner settings are invalid."
    )
  }
  for (name in c("repeat_threshold", "loop_threshold")) {
    scan_check_threshold(settings[[name]], name, rlang::caller_env())
  }
  if (!identical(x$view, investigation_view(x$view, ids, settings$scans))) {
    investigation_abort("The saved view is invalid.")
  }
  analysis <- x$analysis
  if (
    !trajectory_is_named_list(analysis) ||
      !setequal(
        names(analysis),
        c("summaries", "findings", "assessments", "measures")
      ) ||
      !all(vapply(analysis, is.data.frame, logical(1)))
  ) {
    investigation_abort(
      "The saved analysis must contain summaries, findings, assessments, and measures."
    )
  }
  for (name in names(analysis)) {
    table <- analysis[[name]]
    if (
      !"trajectory_id" %in% names(table) || !all(table$trajectory_id %in% ids)
    ) {
      investigation_abort(
        "The saved analysis has invalid trajectory references."
      )
    }
  }
  findings <- analysis$findings
  prototypes <- list(
    findings = scan_empty_findings(),
    assessments = scan_empty_assessments(),
    summaries = scan_empty_summaries(),
    measures = scans_measure_template()[0, ]
  )
  for (name in names(prototypes)) {
    columns <- names(prototypes[[name]])
    if (!all(columns %in% names(analysis[[name]]))) {
      investigation_abort("The saved {.field {name}} columns are incomplete.")
    }
    tryCatch(
      vctrs::vec_cast(analysis[[name]][columns], prototypes[[name]]),
      error = function(cnd) {
        investigation_abort(
          "The saved {.field {name}} column types are invalid."
        )
      }
    )
  }
  if (
    nrow(analysis$summaries) != length(ids) ||
      anyDuplicated(analysis$summaries$trajectory_id) ||
      nrow(analysis$measures) != length(ids) * nrow(scans_measure_template()) ||
      anyDuplicated(analysis$measures[c("trajectory_id", "measure")]) ||
      !all(analysis$measures$measure %in% scans_measure_template()$measure)
  ) {
    investigation_abort("The saved summaries or measurements are incomplete.")
  }
  if (
    anyDuplicated(findings$finding_id) ||
      anyNA(findings$finding_id) ||
      !all(analysis$assessments$scan %in% settings$scans) ||
      !all(findings$scan %in% settings$scans)
  ) {
    investigation_abort("The saved finding or scanner identities are invalid.")
  }
  for (kind in c("event", "turn")) {
    column <- paste0(kind, "_id")
    rows <- if (kind == "event") {
      trajectory_events(x$bundle)
    } else {
      trajectory_turns(x$bundle)
    }
    known <- !is.na(findings[[column]])
    matched <- match(findings[[column]][known], rows[[column]])
    if (
      anyNA(matched) ||
        !all(rows$trajectory_id[matched] == findings$trajectory_id[known])
    ) {
      investigation_abort(
        "A saved finding refers to missing or unrelated evidence."
      )
    }
  }
  events <- trajectory_events(x$bundle)
  for (i in seq_len(nrow(findings))) {
    references <- findings$event_ids[[i]]
    matched <- match(references, events$event_id)
    if (
      !is.character(references) ||
        anyNA(matched) ||
        anyDuplicated(references) ||
        !all(events$trajectory_id[matched] == findings$trajectory_id[[i]])
    ) {
      investigation_abort(
        "A saved finding refers to missing or unrelated events."
      )
    }
  }
  assessments <- analysis$assessments
  expected <- length(ids) * length(settings$scans)
  if (
    nrow(assessments) != expected ||
      anyDuplicated(assessments[c("trajectory_id", "scan")]) ||
      anyNA(assessments$scan_version) ||
      !all(nzchar(assessments$scan_version)) ||
      anyDuplicated(unique(assessments[c("scan", "scan_version")])$scan) ||
      !all(
        assessments$status %in%
          c(
            "assessed_no_findings",
            "assessed_with_findings",
            "insufficient_evidence",
            "not_applicable",
            "execution_failure"
          )
      )
  ) {
    investigation_abort(
      "The saved scanner assessments are incomplete or invalid."
    )
  }
  loss_owners <- scan_loss_trajectory_ids(
    trajectory_losses(x$bundle),
    trajectory_turns(x$bundle),
    events
  )
  for (i in seq_len(nrow(assessments))) {
    referenced <- assessments$finding_ids[[i]]
    matches <- match(referenced, findings$finding_id)
    if (
      !is.character(referenced) ||
        anyDuplicated(referenced) ||
        !setequal(
          referenced,
          findings$finding_id[
            findings$trajectory_id == assessments$trajectory_id[[i]] &
              findings$scan == assessments$scan[[i]]
          ]
        ) ||
        (length(referenced) > 0L) !=
          identical(assessments$status[[i]], "assessed_with_findings")
    ) {
      investigation_abort(
        "The saved assessment status disagrees with its finding references."
      )
    }
    losses <- assessments$loss_rows[[i]]
    if (
      anyNA(matches) ||
        !all(
          findings$trajectory_id[matches] == assessments$trajectory_id[[i]]
        ) ||
        !all(findings$scan[matches] == assessments$scan[[i]]) ||
        !is.numeric(losses) ||
        anyNA(losses) ||
        anyDuplicated(losses) ||
        !all(
          is.na(loss_owners[losses]) |
            loss_owners[losses] == assessments$trajectory_id[[i]]
        ) ||
        any(
          losses != floor(losses) |
            losses < 1L |
            losses > nrow(trajectory_losses(x$bundle))
        )
    ) {
      investigation_abort(
        "The saved assessment has invalid finding or loss references."
      )
    }
  }
  manifest <- x$manifest
  fields <- c(
    "format_version",
    "created_at",
    "application",
    "source",
    "package_version",
    "scanner_versions",
    "content_policy",
    "parent_revision_id",
    "snapshot_id",
    "revision_id"
  )
  if (
    !trajectory_is_named_list(manifest) ||
      !setequal(names(manifest), fields) ||
      !identical(manifest$format_version, 1L) ||
      !rlang::is_string(manifest$application) ||
      is.na(manifest$application) ||
      !nzchar(manifest$application) ||
      !rlang::is_string(manifest$package_version) ||
      is.na(manifest$package_version) ||
      !inherits(manifest$created_at, "POSIXct") ||
      length(manifest$created_at) != 1L ||
      is.na(manifest$created_at) ||
      !trajectory_is_named_list(manifest$source) ||
      !identical(
        manifest$scanner_versions,
        unique(assessments[c("scan", "scan_version")])
      ) ||
      !is.character(manifest$parent_revision_id) ||
      length(manifest$parent_revision_id) != 1L ||
      (!is.na(manifest$parent_revision_id) &&
        !grepl("^sha256:[0-9a-f]{64}$", manifest$parent_revision_id)) ||
      !trajectory_is_named_list(manifest$content_policy) ||
      !identical(manifest$content_policy$id, "retained-v1") ||
      !is.numeric(manifest$content_policy$omitted_trajectories) ||
      length(manifest$content_policy$omitted_trajectories) != 1L ||
      !is.finite(manifest$content_policy$omitted_trajectories) ||
      manifest$content_policy$omitted_trajectories < 0 ||
      manifest$content_policy$omitted_trajectories %% 1 != 0
  ) {
    investigation_abort(
      "The investigation manifest or content policy is invalid."
    )
  }
  if (
    !identical(manifest$snapshot_id, investigation_snapshot_id(x)) ||
      !identical(manifest$revision_id, investigation_revision_id(x))
  ) {
    investigation_abort(
      "The investigation identifiers do not match its evidence or analysis."
    )
  }
  invisible(x)
}
