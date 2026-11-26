# A fixed 10,000-trajectory cohort for measuring scans at scale.
#
#   Rscript tools/scale-fixture.R [output.rds]
#
# prints the fixture's shape and, with a path, saves the bundle. It is
# generated on demand and never shipped: `scale_fixture()` is deterministic
# for a given `n` and `seed`, so the same call rebuilds the same bundle.
# `tools/scale-profile.R` sources this file.
#
# The cohort is shaped like a deployed support assistant read from
# OpenTelemetry: conversations of one to twenty-five exchanges over fourteen
# days with a daily rhythm, four models, four hundred users with a long tail,
# eight tools with different failure rates, a two-day outage of one tool with
# retry loops, failed runs, cut-off answers, conversations recorded without
# content, empty conversations, missing usage with loss records, and a few
# capture-wide losses. All text is synthetic.

scale_fixture <- function(n = 10000L, seed = 4410L) {
  # Leave the caller's generator and stream as they were.
  caller_kind <- RNGkind()
  caller_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit(
    {
      suppressWarnings(RNGkind(
        caller_kind[[1L]],
        caller_kind[[2L]],
        caller_kind[[3L]]
      ))
      if (is.null(caller_seed)) {
        rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", caller_seed, envir = globalenv())
      }
    },
    add = TRUE
  )
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  n <- as.integer(n)
  origin <- as.POSIXct("2026-09-01 00:00:00", tz = "UTC")
  days <- 14L

  # Conversations ------------------------------------------------------
  day <- sample.int(
    days,
    n,
    replace = TRUE,
    prob = rep(c(1, 1.1, 1.2, 1.1, 1, 0.4, 0.3), length.out = days)
  )
  hour <- sample(
    0:23,
    n,
    replace = TRUE,
    prob = c(
      1,
      1,
      1,
      1,
      1,
      2,
      4,
      7,
      10,
      12,
      12,
      11,
      10,
      11,
      12,
      11,
      9,
      7,
      5,
      4,
      3,
      2,
      2,
      1
    )
  )
  offset <- sort((day - 1L) * 86400 + hour * 3600 + stats::runif(n, 0, 3600))
  day <- as.integer(offset %/% 86400) + 1L
  id <- sprintf("otel/conv-%05d", seq_len(n))
  models <- c("support-small", "support-large", "support-small-v2", "fallback")
  model <- ifelse(
    day >= 8L,
    sample(models, n, replace = TRUE, prob = c(0.1, 0.25, 0.6, 0.05)),
    sample(models, n, replace = TRUE, prob = c(0.65, 0.3, 0, 0.05))
  )
  user <- sprintf(
    "user-%03d",
    sample.int(400L, n, replace = TRUE, prob = 1 / seq_len(400L))
  )
  user[stats::runif(n) < 0.05] <- NA_character_
  exchanges <- pmin(1L + stats::rgeom(n, 0.38), 25L)
  fate <- stats::runif(n)
  status <- ifelse(fate < 0.02, "failed", ifelse(fate < 0.05, NA, "completed"))
  cut_off <- stats::runif(n) < 0.01
  no_content <- stats::runif(n) < 0.02
  no_usage <- stats::runif(n) < 0.04
  no_end <- stats::runif(n) < 0.01
  exchanges[stats::runif(n) < 0.005] <- 0L

  # Exchanges and tool calls -------------------------------------------
  exchange_of <- rep.int(seq_len(n), exchanges)
  n_exchanges <- length(exchange_of)
  exchange_index <- sequence(exchanges)
  tools <- c(
    "lookup_order",
    "return_policy",
    "check_stock",
    "refund_status",
    "search_articles",
    "update_address",
    "escalate",
    "shipping_estimate"
  )
  failure <- c(0.03, 0.005, 0.02, 0.15, 0.01, 0.04, 0.02, 0.03)
  seconds <- c(1.2, 0.3, 0.8, 2.5, 1.5, 0.9, 0.4, 1.1)
  errors <- c(
    "order_service_timeout",
    "policy_not_found",
    "inventory_unavailable",
    "refund_service_error",
    "search_timeout",
    "address_rejected",
    "queue_full",
    "carrier_unavailable"
  )
  calls_per_exchange <- sample(
    0:4,
    n_exchanges,
    replace = TRUE,
    prob = c(0.45, 0.3, 0.15, 0.07, 0.03)
  )
  call_exchange <- rep.int(seq_len(n_exchanges), calls_per_exchange)
  tool <- sample.int(
    length(tools),
    length(call_exchange),
    replace = TRUE,
    prob = c(0.3, 0.15, 0.12, 0.1, 0.15, 0.06, 0.04, 0.08)
  )
  call_day <- day[exchange_of[call_exchange]]
  outage <- tool == 1L & call_day %in% 9:10
  failed <- stats::runif(length(tool)) < ifelse(outage, 0.6, failure[tool])
  # A failed call is retried with the same arguments up to three more times.
  retries <- ifelse(
    failed & stats::runif(length(tool)) < 0.5,
    sample.int(3L, length(tool), replace = TRUE),
    0L
  )
  argument <- sprintf("DEMO-%05d", sample.int(40000L, length(tool), TRUE))
  attempt_of <- rep.int(seq_along(tool), retries + 1L)
  attempt <- sequence(retries + 1L)
  call <- data.frame(
    exchange = call_exchange[attempt_of],
    slot = attempt_of,
    attempt = attempt,
    tool = tool[attempt_of],
    argument = argument[attempt_of],
    stringsAsFactors = FALSE
  )
  last_attempt <- attempt == (retries + 1L)[attempt_of]
  call$failed <- failed[attempt_of] &
    (!last_attempt | stats::runif(nrow(call)) < 0.7)
  call$resolved <- stats::runif(nrow(call)) >= 0.004
  call$duration <- stats::rexp(nrow(call), 1 / seconds[call$tool]) +
    ifelse(call$failed & call$tool == 1L, 8, 0)
  call$call_id <- sprintf("call-%07d", seq_len(nrow(call)))

  # One step per turn: user, each call and its result, then the answer ----
  step <- function(exchange, slot, attempt, kind, call_row = NA_integer_) {
    data.frame(
      exchange = exchange,
      slot = slot,
      attempt = attempt,
      kind = kind,
      call_row = call_row
    )
  }
  resolved <- which(call$resolved)
  steps <- rbind(
    step(seq_len(n_exchanges), 0L, 0L, 1L),
    step(call$exchange, call$slot, call$attempt, 2L, seq_len(nrow(call))),
    step(
      call$exchange[resolved],
      call$slot[resolved],
      call$attempt[resolved],
      3L,
      resolved
    ),
    step(seq_len(n_exchanges), .Machine$integer.max, 0L, 4L)
  )
  steps <- steps[
    order(steps$exchange, steps$slot, steps$attempt, steps$kind),
  ]
  trajectory <- exchange_of[steps$exchange]
  per_trajectory <- tabulate(trajectory, nbins = n)
  index <- sequence(per_trajectory)
  n_steps <- nrow(steps)
  kind <- steps$kind
  step_call <- call[steps$call_row, ]

  step_seconds <- ifelse(
    kind == 1L,
    ifelse(
      exchange_index[steps$exchange] == 1L,
      0,
      stats::rexp(n_steps, 1 / 45)
    ),
    ifelse(kind == 2L, step_call$duration, ifelse(kind == 3L, 0.05, 0))
  )
  answer_seconds <- ifelse(kind == 4L, 0.8 + stats::rexp(n_steps, 1 / 2.5), 0)
  before <- stats::ave(
    step_seconds + answer_seconds,
    trajectory,
    FUN = cumsum
  ) -
    ifelse(kind == 1L, 0, step_seconds + answer_seconds)
  timestamp <- origin + offset[trajectory] + before
  total <- as.numeric(tapply(
    step_seconds + answer_seconds,
    factor(trajectory, levels = seq_len(n)),
    sum
  ))
  total[is.na(total)] <- 0

  topics <- c("order", "refund", "return", "delivery", "address", "stock")
  topic <- topics[sample.int(length(topics), n_steps, replace = TRUE)]
  reference <- sprintf("DEMO-%05d", sample.int(40000L, n_steps, TRUE))
  text <- ifelse(
    kind == 1L,
    sprintf("Can you help with the %s for %s?", topic, reference),
    ifelse(
      kind == 3L,
      ifelse(
        step_call$failed,
        "The service did not return a result.",
        sprintf(
          "%s: status recorded for %s.",
          tools[step_call$tool],
          step_call$argument
        )
      ),
      ifelse(
        kind == 4L,
        sprintf(
          "Here is what the records show about the %s for %s.",
          topic,
          reference
        ),
        NA_character_
      )
    )
  )
  text[no_content[trajectory]] <- NA_character_

  last_step <- index == per_trajectory[trajectory]
  is_answer <- kind == 4L
  usage <- is_answer & !no_usage[trajectory]
  turn_id <- sprintf("%s/turn-%04d", id[trajectory], index)
  event_id <- sprintf("%s/event-%04d", id[trajectory], index)
  call_event <- event_id[match(
    paste(steps$call_row, 2L),
    paste(steps$call_row, kind)
  )]

  turns <- tibble::tibble(
    trajectory_id = id[trajectory],
    turn_id = turn_id,
    turn_index = index,
    role = c("user", "assistant", "tool", "assistant")[kind],
    input_tokens = ifelse(
      usage,
      round(180 + 140 * index + stats::rnorm(n_steps, 0, 40)),
      NA_real_
    ),
    output_tokens = ifelse(
      usage,
      round(30 + stats::rexp(n_steps, 1 / 90)),
      NA_real_
    ),
    duration = ifelse(is_answer, answer_seconds, NA_real_),
    finish_reason = ifelse(
      is_answer,
      ifelse(last_step & cut_off[trajectory], "length", "stop"),
      NA_character_
    ),
    status = "completed"
  )
  events <- tibble::tibble(
    trajectory_id = id[trajectory],
    event_id = event_id,
    event_index = index,
    turn_id = turn_id,
    content_index = 1L,
    parent_event_id = ifelse(kind == 3L, call_event, NA_character_),
    event_type = c("content", "tool_call", "tool_result", "content")[kind],
    content_type = ifelse(kind %in% c(1L, 4L), "text", NA_character_),
    name = ifelse(kind == 2L, tools[step_call$tool], NA_character_),
    call_id = ifelse(kind %in% 2:3, step_call$call_id, NA_character_),
    text = text,
    value = lapply(seq_len(n_steps), function(i) NULL),
    timestamp = structure(
      ifelse(kind == 3L, NA_real_, as.numeric(timestamp)),
      class = c("POSIXct", "POSIXt"),
      tzone = "UTC"
    ),
    duration = ifelse(
      kind == 2L,
      step_call$duration,
      ifelse(is_answer, answer_seconds, NA_real_)
    ),
    status = ifelse(kind == 2L & step_call$failed, "failed", "completed"),
    error = ifelse(
      kind == 2L & step_call$failed,
      errors[step_call$tool],
      NA_character_
    )
  )
  is_call <- which(kind == 2L)
  events$value[is_call] <- lapply(
    step_call$argument[is_call],
    function(argument) list(order = argument)
  )

  started <- origin + offset
  completed <- started + total
  completed[no_end] <- NA
  trajectories <- tibble::tibble(
    trajectory_id = id,
    run_id = sub("^otel/", "", id),
    source_type = "otel",
    source_id = sub("^otel/", "", id),
    model = model,
    started_at = started,
    completed_at = completed,
    status = status,
    error = ifelse(status %in% "failed", "provider_error", NA_character_),
    metadata = lapply(user, function(user) {
      if (is.na(user)) list() else list(otel = list(user = user))
    })
  )

  # Missing usage is a recorded loss on the conversation's first turn; five
  # further losses belong to the capture as a whole.
  lost <- which(no_usage & per_trajectory > 0L)
  losses <- tibble::tibble(
    trajectory_id = c(id[lost], rep(NA_character_, 5L)),
    turn_id = c(sprintf("%s/turn-0001", id[lost]), rep(NA_character_, 5L)),
    field = c(rep("usage", length(lost)), rep("spans", 5L)),
    reason = c(rep("unsupported", length(lost)), rep("truncated", 5L)),
    detail = c(
      rep("Token usage was not exported.", length(lost)),
      rep("The exporter reported dropped spans.", 5L)
    )
  )

  scans::TrajectoryBundle(trajectories, turns, events, losses = losses)
}

# Counts that describe the fixture, for the profile's record.
scale_fixture_shape <- function(bundle) {
  info <- scans::trajectory_info(bundle)
  turns <- scans::trajectory_turns(bundle)
  events <- scans::trajectory_events(bundle)
  losses <- scans::trajectory_losses(bundle)
  per_trajectory <- tabulate(
    match(events$trajectory_id, info$trajectory_id),
    nbins = nrow(info)
  )
  calls <- events[events$event_type == "tool_call", ]
  users <- vapply(
    info$metadata,
    function(metadata) {
      user <- metadata$otel$user
      if (is.null(user)) NA_character_ else user
    },
    ""
  )
  list(
    trajectories = nrow(info),
    turns = nrow(turns),
    events = nrow(events),
    events_per_trajectory = stats::quantile(
      per_trajectory,
      c(0, 0.5, 0.9, 0.99, 1),
      names = TRUE
    ),
    empty_trajectories = sum(per_trajectory == 0L),
    tool_calls = nrow(calls),
    tool_calls_failed = sum(calls$status == "failed"),
    tools = length(unique(calls$name)),
    models = table(info$model),
    users = length(unique(stats::na.omit(users))),
    without_user = sum(is.na(users)),
    status = table(info$status, useNA = "always"),
    window = range(info$started_at),
    days = length(unique(format(info$started_at, "%Y-%m-%d", tz = "UTC"))),
    without_end = sum(is.na(info$completed_at)),
    without_text = length(setdiff(
      events$trajectory_id[events$event_type == "content"],
      events$trajectory_id[!is.na(events$text)]
    )),
    losses = nrow(losses),
    unassigned_losses = sum(
      is.na(losses$trajectory_id) & is.na(losses$turn_id)
    ),
    size_mb = as.numeric(utils::object.size(bundle)) / 1024^2
  )
}

if (sys.nframe() == 0L) {
  arguments <- commandArgs(trailingOnly = TRUE)
  bundle <- scale_fixture()
  print(scale_fixture_shape(bundle))
  if (length(arguments)) {
    saveRDS(bundle, arguments[[1L]])
  }
}
