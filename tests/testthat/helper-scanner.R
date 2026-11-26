# Three short conversations: one with the keyword, one without, and one whose
# events were not captured.
scanner_bundle_fixture <- function() {
  TrajectoryBundle(
    trajectories = tibble::tibble(
      trajectory_id = c("chat-hit", "chat-clean", "chat-empty", "batch-job"),
      source_type = c("manual", "manual", "manual", "batch"),
      status = "completed"
    ),
    turns = tibble::tibble(
      trajectory_id = rep(c("chat-hit", "chat-clean", "batch-job"), each = 2L),
      turn_id = c("hit-1", "hit-2", "clean-1", "clean-2", "job-1", "job-2"),
      turn_index = rep(1:2, 3L),
      role = rep(c("user", "assistant"), 3L)
    ),
    events = tibble::tibble(
      trajectory_id = c(
        "chat-hit",
        "chat-hit",
        "chat-hit",
        "chat-clean",
        "chat-clean",
        "batch-job",
        "batch-job"
      ),
      event_id = c(
        "hit-e1",
        "hit-e2",
        "hit-e3",
        "clean-e1",
        "clean-e2",
        "job-e1",
        "job-e2"
      ),
      event_index = c(1:3, 1:2, 1:2),
      turn_id = c(
        "hit-1",
        "hit-2",
        "hit-2",
        "clean-1",
        "clean-2",
        "job-1",
        "job-2"
      ),
      event_type = "content",
      text = c(
        "Where is my parcel?",
        "It is guaranteed to arrive today.",
        "Delivery is guaranteed.",
        "Where is my parcel?",
        "It is on its way.",
        "Summarise the queue.",
        "Guaranteed: nothing is waiting."
      )
    )
  )
}

# Flags assistant text containing the configured keyword.
scanner_keyword_fixture <- function(keyword = "guaranteed", version = "1") {
  scanner(
    "toy_keyword",
    function(trajectory, settings) {
      if (trajectory$info$source_type == "batch") {
        return(scanner_unassessed(
          "not_applicable",
          "Batch jobs have no customer conversation."
        ))
      }
      events <- trajectory$events
      if (nrow(events) == 0L) {
        return(scanner_unassessed(
          "insufficient_evidence",
          "No conversation events are recorded."
        ))
      }
      roles <- trajectory$turns$role[match(
        events$turn_id,
        trajectory$turns$turn_id
      )]
      hits <- events[
        roles %in%
          "assistant" &
          grepl(settings$keyword, events$text, ignore.case = TRUE),
      ]
      scanner_findings(
        event_id = hits$event_id,
        label = rep("Keyword", nrow(hits)),
        explanation = rep("The assistant used the keyword.", nrow(hits))
      )
    },
    description = "The assistant used a configured keyword.",
    required_evidence = "Recorded assistant text.",
    version = version,
    settings = list(keyword = keyword)
  )
}

scanner_toy_fixture <- function(name, fn) {
  scanner(name, fn, description = "A toy.", required_evidence = "Nothing.")
}

# Five matched cases whose replies move the keyword scanner's result in every
# way: it appears, resolves, persists, and stops or starts applying ("N/A"
# makes the scan not applicable).
scanner_comparison_fixture <- function(baseline, candidate) {
  side <- function(replies, scanners, application) {
    ids <- names(replies)
    bundle <- TrajectoryBundle(
      trajectories = tibble::tibble(
        trajectory_id = ids,
        sample_id = ids,
        source_type = "manual",
        status = "completed"
      ),
      turns = tibble::tibble(
        trajectory_id = ids,
        turn_id = paste0(ids, "-t1"),
        turn_index = 1L,
        role = "assistant"
      ),
      events = tibble::tibble(
        trajectory_id = ids,
        event_id = paste0(ids, "-e1"),
        event_index = 1L,
        turn_id = paste0(ids, "-t1"),
        event_type = "content",
        text = unlist(replies, use.names = FALSE)
      )
    )
    investigation_snapshot(
      bundle,
      scanners = scanners,
      application = application
    )
  }
  pairing <- pair_trajectories(
    side(
      list(
        appear = "fine",
        resolve = "guaranteed",
        persist = "guaranteed",
        stop = "guaranteed",
        start = "N/A"
      ),
      baseline,
      "Baseline"
    ),
    side(
      list(
        appear = "guaranteed",
        resolve = "fine",
        persist = "guaranteed",
        stop = "N/A",
        start = "guaranteed"
      ),
      candidate,
      "Candidate"
    ),
    by = "sample_id",
    dataset_version = "v1"
  )
  compare_trajectories(pairing)
}

# The keyword scanner, not applicable to a reply of "N/A".
scanner_applicable_fixture <- function(version = "1", keyword = "guaranteed") {
  scanner(
    "toy_keyword",
    function(trajectory, settings) {
      text <- trajectory$events$text
      if (any(text %in% "N/A")) {
        return(scanner_unassessed("not_applicable", "Nothing was promised."))
      }
      hits <- trajectory$events[grepl(settings$keyword, text), ]
      scanner_findings(
        event_id = hits$event_id,
        label = rep("Keyword", nrow(hits)),
        explanation = rep("The reply used the keyword.", nrow(hits))
      )
    },
    description = "A reply used a configured keyword.",
    required_evidence = "Recorded replies.",
    version = version,
    settings = list(keyword = keyword)
  )
}
