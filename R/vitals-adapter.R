#' Convert vitals evaluation samples
#'
#' `as_trajectory_vitals()` snapshots completed solver trajectories and their
#' evaluation outcomes from [vitals](https://vitals.tidyverse.org/). It accepts
#' a vitals `Task` or the sample data frame returned by `Task$get_samples()`.
#' It also accepts the data frame returned by the development
#' `vitals::vitals_log_read()` interface.
#'
#' Each `(id, epoch)` sample becomes one trajectory. Its `solver_chat` is
#' converted through the ellmer adapter, and a non-missing `score` becomes one
#' evaluation row. Sample input, result, solver metadata, and custom columns are
#' retained as sanitized trajectory metadata. Scorer metadata is retained on
#' the evaluation. A `scorer_chat`, when present, is recorded as an explicit
#' adapter loss because schema version 1 represents the evaluated solver path,
#' not the evaluator's separate model trajectory.
#'
#' The adapter only uses the public `Task$get_samples()` method. It does not
#' retain the live task, chats, providers, tools, callbacks, or credentials.
#'
#' @param x A vitals `Task`, or a data frame from `Task$get_samples()` or
#'   `vitals::vitals_log_read()`. Sample data must contain unique `(id, epoch)`
#'   pairs and an ellmer `Chat` in `solver_chat` for every row. When `epoch` is
#'   absent, it defaults to `1L`.
#' @param run_id,task_id Optional correlation identities applied to every
#'   trajectory.
#' @param source_uri An optional source locator, such as the path to a vitals
#'   eval log. Credentials, query parameters, and fragments are removed.
#' @param include_system_prompt Whether solver system prompts should be
#'   included as system turns.
#'
#' @returns A [TrajectoryBundle].
#' @export
#'
#' @examples
#' if (
#'   rlang::is_installed("ellmer", version = "0.4.2") &&
#'     rlang::is_installed("vitals", version = "0.3.0")
#' ) {
#'   task <- vitals::Task$new(
#'     data.frame(input = "Say hello", target = "Hi"),
#'     solver = function(inputs) {
#'       chat <- ellmer::chat_openai(
#'         model = "gpt-4o-mini",
#'         credentials = function() "not-used"
#'       )
#'       chat$set_turns(list(
#'         ellmer::UserTurn(list(ellmer::ContentText(inputs[[1L]]))),
#'         ellmer::AssistantTurn(list(ellmer::ContentText("Hi")))
#'       ))
#'       list(result = "Hi", solver_chat = list(chat))
#'     },
#'     scorer = function(samples) list(score = "C"),
#'     dir = tempdir()
#'   )
#'   task$solve()
#'   task$score()
#'
#'   bundle <- as_trajectory_vitals(task)
#'   trajectory_evaluations(bundle)
#' }
as_trajectory_vitals <- function(
  x,
  run_id = NULL,
  task_id = NULL,
  source_uri = NULL,
  include_system_prompt = TRUE
) {
  call <- rlang::caller_env()
  ellmer_check_installed(call)
  run_id <- vitals_check_optional_string(run_id, call = call)
  task_id <- vitals_check_optional_string(task_id, call = call)
  source_uri <- vitals_check_optional_string(source_uri, call = call)
  rlang::check_bool(include_system_prompt, call = call)

  source <- vitals_source_samples(x, call)
  validated <- vitals_validate_samples(source$samples, call)
  samples <- validated$samples
  sample_ids <- validated$sample_ids
  epochs <- validated$epochs
  trajectory_ids <- validated$trajectory_ids

  tables <- list(
    trajectories = vector("list", nrow(samples)),
    turns = vector("list", nrow(samples)),
    events = vector("list", nrow(samples)),
    evaluations = list(),
    losses = list()
  )

  for (index in seq_len(nrow(samples))) {
    trajectory_id <- trajectory_ids[[index]]
    sample <- samples[index, , drop = FALSE]
    sample_id <- sample_ids[[index]]
    epoch <- epochs[[index]]
    ids <- ellmer_ids(trajectory_id)
    metadata <- vitals_trajectory_metadata(sample, source$source_class)

    bundle <- as_trajectory_ellmer(
      vitals_row_value(sample, "solver_chat"),
      trajectory_id = trajectory_id,
      run_id = run_id,
      source_id = sample_id,
      source_uri = source_uri,
      task_id = task_id,
      sample_id = sample_id,
      epoch = epoch,
      metadata = metadata,
      include_system_prompt = include_system_prompt
    )

    info <- trajectory_info(bundle)
    info$source_type <- "vitals"
    tables$trajectories[[index]] <- info
    tables$turns[[index]] <- trajectory_turns(bundle)
    tables$events[[index]] <- trajectory_events(bundle)
    tables$losses <- c(tables$losses, list(trajectory_losses(bundle)))

    evaluation <- vitals_evaluation_row(
      sample,
      trajectory_id,
      task_id,
      sample_id,
      epoch,
      ids
    )
    if (!is.null(evaluation$row)) {
      tables$evaluations <- c(tables$evaluations, list(evaluation$row))
    }
    if (length(evaluation$losses) > 0L) {
      tables$losses <- c(
        tables$losses,
        list(ellmer_loss_table(evaluation$losses))
      )
    }

    scorer_chat <- vitals_row_value(sample, "scorer_chat")
    if (vitals_value_present(scorer_chat)) {
      tables$losses <- c(
        tables$losses,
        list(ellmer_loss_table(list(ellmer_new_loss(
          ids,
          "scorer_chat",
          "unsupported",
          paste0(
            "The scorer chat was not retained because schema version 1 ",
            "represents the evaluated solver trajectory"
          )
        ))))
      )
    }
  }

  TrajectoryBundle(
    vitals_bind_rows(tables$trajectories),
    vitals_bind_rows(tables$turns),
    vitals_bind_rows(tables$events),
    evaluations = vitals_bind_rows(tables$evaluations),
    losses = vitals_bind_rows(tables$losses)
  )
}

vitals_is_task <- function(x) {
  inherits(x, "Task") &&
    inherits(x, "R6") &&
    is.function(x$get_samples) &&
    is.function(x$solve) &&
    is.function(x$score) &&
    rlang::is_installed("vitals", version = "0.3.0") &&
    vitals_method_in_namespace(x$get_samples)
}

vitals_method_in_namespace <- function(method) {
  env <- environment(method)
  namespace <- asNamespace("vitals")
  while (!identical(env, emptyenv())) {
    if (identical(env, namespace)) {
      return(TRUE)
    }
    env <- parent.env(env)
  }
  FALSE
}

vitals_source_samples <- function(x, call) {
  if (vitals_is_task(x)) {
    samples <- tryCatch(
      x$get_samples(),
      error = function(error) {
        scans_abort(
          c(
            "Can't read samples from the vitals {.cls Task}.",
            "x" = conditionMessage(error)
          ),
          class = "scans_error_vitals_source",
          call = call,
          parent = error
        )
      }
    )
    return(list(samples = samples, source_class = "Task"))
  }

  if (is.data.frame(x)) {
    return(list(samples = x, source_class = class(x)[[1L]]))
  }

  scans_abort(
    c(
      "{.arg x} must be a vitals {.cls Task} or sample data frame.",
      "x" = "It is {.obj_type_friendly {x}}."
    ),
    class = "scans_error_vitals_source",
    call = call
  )
}

vitals_validate_samples <- function(samples, call) {
  if (!is.data.frame(samples)) {
    scans_abort(
      "The vitals source must return a data frame from {.code $get_samples()}.",
      class = "scans_error_vitals_source",
      call = call
    )
  }
  if (anyDuplicated(names(samples))) {
    scans_abort(
      "The vitals sample data must have unique column names.",
      class = "scans_error_vitals_source",
      call = call
    )
  }
  if (nrow(samples) == 0L) {
    scans_abort(
      "The vitals sample data must contain at least one sample.",
      class = "scans_error_vitals_source",
      call = call
    )
  }

  required <- c("id", "solver_chat")
  missing <- setdiff(required, names(samples))
  if (length(missing) > 0L) {
    scans_abort(
      c(
        "The vitals sample data is incomplete.",
        "x" = "Missing required column{?s}: {.field {missing}}.",
        "i" = paste0(
          "Pass a solved Task or the output of $get_samples() or ",
          "vitals_log_read()."
        )
      ),
      class = "scans_error_vitals_source",
      call = call
    )
  }

  sample_ids <- vapply(
    seq_len(nrow(samples)),
    \(index) vitals_sample_id(samples$id[[index]], index, call),
    character(1)
  )
  epochs <- if ("epoch" %in% names(samples)) {
    vapply(
      seq_len(nrow(samples)),
      \(index) vitals_epoch(samples$epoch[[index]], index, call),
      integer(1)
    )
  } else {
    rep(1L, nrow(samples))
  }

  duplicate <- duplicated(data.frame(sample_id = sample_ids, epoch = epochs))
  if (any(duplicate)) {
    rows <- which(duplicate)
    scans_abort(
      c(
        "Each vitals sample and epoch must identify one trajectory.",
        "x" = "Rows {.val {rows}} duplicate an earlier {.field id}/{.field epoch} pair."
      ),
      class = "scans_error_vitals_identity",
      call = call
    )
  }

  trajectory_ids <- mapply(
    vitals_trajectory_id,
    sample_ids,
    epochs,
    MoreArgs = list(call = call),
    USE.NAMES = FALSE
  )

  valid_chat <- vapply(samples$solver_chat, ellmer_is_chat, logical(1))
  if (!all(valid_chat)) {
    rows <- which(!valid_chat)
    scans_abort(
      c(
        "Every {.field solver_chat} must be a completed ellmer {.cls Chat}.",
        "x" = "Rows {.val {rows}} contain unsupported values."
      ),
      class = "scans_error_vitals_source",
      call = call
    )
  }

  list(
    samples = samples,
    sample_ids = sample_ids,
    epochs = epochs,
    trajectory_ids = trajectory_ids
  )
}

vitals_sample_id <- function(x, row, call) {
  valid <- is.atomic(x) && length(x) == 1L && !is.na(x)
  value <- if (valid) as.character(x) else NA_character_
  valid <- valid &&
    nzchar(trimws(value)) &&
    nchar(value, type = "bytes") <= trajectory_payload_max_bytes
  if (!valid) {
    scans_abort(
      c(
        "Every vitals {.field id} must be a non-missing scalar value.",
        "x" = "Row {.val {row}} has an invalid sample ID."
      ),
      class = "scans_error_vitals_identity",
      call = call
    )
  }
  value
}

vitals_epoch <- function(x, row, call) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x == floor(x) &&
    x >= 1 &&
    x <= .Machine$integer.max
  if (!valid) {
    scans_abort(
      c(
        "Every vitals {.field epoch} must be a positive whole number.",
        "x" = "Row {.val {row}} has an invalid epoch."
      ),
      class = "scans_error_vitals_identity",
      call = call
    )
  }
  as.integer(x)
}

vitals_trajectory_id <- function(sample_id, epoch, call) {
  id <- paste0(
    "vitals/",
    utils::URLencode(sample_id, reserved = TRUE),
    "/epoch-",
    sprintf("%06d", epoch)
  )
  if (nchar(id, type = "bytes") > trajectory_payload_max_bytes) {
    scans_abort(
      "The encoded vitals sample identity exceeds 65,536 bytes.",
      class = "scans_error_vitals_identity",
      call = call
    )
  }
  id
}

vitals_check_optional_string <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  rlang::check_string(x, allow_null = TRUE, arg = arg, call = call)
  if (!is.null(x) && nchar(x, type = "bytes") > trajectory_payload_max_bytes) {
    rlang::abort(
      sprintf("`%s` must not exceed 65,536 bytes.", arg),
      call = call
    )
  }
  x
}

vitals_trajectory_metadata <- function(sample, source_class) {
  excluded <- c(
    "id",
    "epoch",
    "target",
    "score",
    "scorer",
    "scorer_chat",
    "scorer_explanation",
    "explanation",
    "scorer_metadata",
    "solver_chat"
  )
  columns <- setdiff(names(sample), excluded)
  sample_metadata <- stats::setNames(
    lapply(columns, \(column) vitals_row_value(sample, column)),
    columns
  )
  version <- if (rlang::is_installed("vitals")) {
    as.character(utils::packageVersion("vitals"))
  } else {
    NA_character_
  }

  list(
    vitals = list(version = version, source_class = source_class),
    sample = sample_metadata
  )
}

vitals_evaluation_row <- function(
  sample,
  trajectory_id,
  task_id,
  sample_id,
  epoch,
  ids
) {
  score <- vitals_row_value(sample, "score")
  if (!vitals_value_present(score)) {
    return(list(row = NULL, losses = list()))
  }

  value <- ellmer_sanitize_value(score, "evaluations$value", ids)
  target <- ellmer_sanitize_value(
    vitals_row_value(sample, "target"),
    "evaluations$target",
    ids
  )
  scorer <- ellmer_sanitize_text(
    vitals_row_value(sample, "scorer"),
    "evaluations$scorer",
    ids
  )
  explanation_source <- if ("scorer_explanation" %in% names(sample)) {
    vitals_row_value(sample, "scorer_explanation")
  } else {
    vitals_row_value(sample, "explanation")
  }
  explanation <- ellmer_sanitize_text(
    explanation_source,
    "evaluations$explanation",
    ids
  )
  metadata <- ellmer_sanitize_metadata(
    list(scorer_metadata = vitals_row_value(sample, "scorer_metadata")),
    "evaluations$metadata",
    ids
  )

  row <- tibble::tibble(
    trajectory_id = trajectory_id,
    evaluation_id = paste0(trajectory_id, "/evaluation-000001"),
    task_id = task_id,
    sample_id = sample_id,
    epoch = epoch,
    scorer = scorer$value,
    value = list(value$value),
    target = list(target$value),
    explanation = explanation$value,
    metadata = list(metadata$value)
  )
  losses <- c(
    value$losses,
    target$losses,
    scorer$losses,
    explanation$losses,
    metadata$losses
  )
  list(row = row, losses = losses)
}

vitals_row_value <- function(sample, column) {
  if (!column %in% names(sample)) {
    return(NULL)
  }
  sample[[column]][[1L]]
}

vitals_value_present <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  !(length(x) == 1L && is.atomic(x) && is.na(x))
}

vitals_bind_rows <- function(rows) {
  rows <- Filter(\(row) !is.null(row) && nrow(row) > 0L, rows)
  if (length(rows) == 0L) {
    return(NULL)
  }
  tibble::as_tibble(do.call(rbind, rows))
}
