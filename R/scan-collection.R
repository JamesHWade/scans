scan_collect_findings <- function(
  x,
  scan_id,
  repeat_threshold,
  loop_threshold,
  capture_errors = FALSE
) {
  info <- trajectory_info(x)
  event_groups <- scan_split_trajectory_rows(
    trajectory_events(x),
    info$trajectory_id
  )
  turn_groups <- scan_split_trajectory_rows(
    trajectory_turns(x),
    info$trajectory_id
  )
  findings <- list()
  failures <- rep(list(character()), nrow(info))
  for (index in seq_len(nrow(info))) {
    events <- event_groups[[index]]
    events <- events[
      order(events$event_index, events$event_id, method = "radix"),
      ,
      drop = FALSE
    ]
    turns <- turn_groups[[index]]
    roles <- turns$role[match(events$turn_id, turns$turn_id)]
    for (group in c("record", "tool", "event")) {
      run <- function() {
        switch(
          group,
          record = scan_record_findings(
            info[index, , drop = FALSE],
            turns,
            scan_id
          ),
          tool = scan_tool_findings(
            events,
            scan_id,
            repeat_threshold,
            loop_threshold,
            roles
          ),
          event = scan_error_findings(events, scan_id)
        )
      }
      if (capture_errors) {
        result <- tryCatch(run(), error = function(cnd) NULL)
        if (is.null(result)) {
          failures[[index]] <- c(failures[[index]], group)
        }
      } else {
        result <- run()
      }
      findings <- c(findings, result)
    }
  }
  list(findings = findings, failures = failures)
}

scan_loss_trajectory_ids <- function(losses, turns, events) {
  owners <- losses$trajectory_id
  event_owners <- events$trajectory_id[match(losses$event_id, events$event_id)]
  missing <- is.na(owners) & !is.na(event_owners)
  owners[missing] <- event_owners[missing]
  turn_owners <- turns$trajectory_id[match(losses$turn_id, turns$turn_id)]
  missing <- is.na(owners) & !is.na(turn_owners)
  owners[missing] <- turn_owners[missing]
  owners
}
