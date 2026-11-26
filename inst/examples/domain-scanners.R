# Two domain-specific scanners built with scans::scanner().
#
# They are worked examples, not built-in scans: what counts as an action claim
# or a retrieval tool differs in every application, so the application owner
# supplies the tool names and phrases. Copy the one you need, change its
# settings and rules, and validate it against your own reviewed cases with
# scans::validate_scanner() before relying on its findings.
#
# Source this file, then call unsupported_action_claim_scanner() or
# answer_without_support_scanner().

# Loss records meaning that turns, messages, or tool activity may be missing
# from the capture, so "no tool call is recorded" cannot be taken at face value.
domain_capture_is_lossy <- function(losses) {
  any(
    grepl("^(read_info|capture)($|[.$])", losses$field) |
      losses$field %in% c("turns", "events", "messages", "parts")
  )
}

# Assistant text events, in event order.
domain_assistant_text <- function(trajectory) {
  events <- trajectory$events
  roles <- trajectory$turns$role[match(
    events$turn_id,
    trajectory$turns$turn_id
  )]
  events[
    roles %in%
      "assistant" &
      events$event_type %in% "content" &
      !is.na(events$text) &
      nzchar(trimws(events$text)),
    ,
    drop = FALSE
  ]
}

domain_result_failed <- function(results) {
  results$status %in% c("failed", "cancelled") | !is.na(results$error)
}

#' Unsupported action claim
#'
#' The assistant tells the user an action was done when the trajectory
#' records no successful call, before the claim, to a tool that performs it.
#'
#' @param version The scanner version. Change it whenever you change the
#'   rules or settings, so saved findings and validation results say which
#'   detector produced them.
#' @param actions A named list, one entry per action. Each entry has `tools`
#'   (the names of the tools that perform the action) and `claim` (a regular
#'   expression, matched case-insensitively, for the assistant saying it was
#'   done).
unsupported_action_claim_scanner <- function(
  actions = list(
    refund = list(
      tools = "issue_refund",
      claim = "refund (has been|was|is) (issued|processed)|(issued|processed) (a|the|your) refund"
    ),
    ticket = list(
      tools = "create_ticket",
      claim = "(created|opened|filed) (a|the|your) ticket|ticket (has been|was) (created|opened|filed)"
    ),
    cancellation = list(
      tools = "cancel_order",
      claim = "order (has been|was|is now) cancell?ed|cancell?ed (the|your) order"
    )
  ),
  version = "1"
) {
  scans::scanner(
    "unsupported_action_claim",
    function(trajectory, settings) {
      events <- trajectory$events
      answers <- domain_assistant_text(trajectory)
      if (nrow(answers) == 0L) {
        return(scans::scanner_unassessed(
          "insufficient_evidence",
          "No assistant text is recorded."
        ))
      }
      calls <- events[events$event_type %in% "tool_call", , drop = FALSE]
      results <- events[events$event_type %in% "tool_result", , drop = FALSE]
      lossy <- domain_capture_is_lossy(trajectory$losses)
      labels <- character()
      explanations <- character()
      primary <- character()
      supporting <- list()
      values <- list()
      for (action in names(settings$actions)) {
        rule <- settings$actions[[action]]
        claims <- answers[
          grepl(rule$claim, answers$text, ignore.case = TRUE),
          ,
          drop = FALSE
        ]
        attempts <- calls[calls$name %in% rule$tools, , drop = FALSE]
        outcome <- results[match(attempts$call_id, results$call_id), ]
        succeeded <- !is.na(attempts$call_id) &
          !is.na(outcome$event_id) &
          !domain_result_failed(outcome)
        for (i in seq_len(nrow(claims))) {
          before <- attempts$event_index < claims$event_index[[i]]
          if (any(before & succeeded)) {
            next
          }
          # A claim with no supporting call is a finding only when the capture
          # can show that the call did not happen.
          if (lossy || anyNA(attempts$call_id[before])) {
            return(scans::scanner_unassessed(
              "insufficient_evidence",
              "The assistant claimed an action, but capture losses or missing call identities mean the tool call may be unrecorded."
            ))
          }
          failed <- stats::na.omit(c(
            attempts$event_id[before],
            outcome$event_id[before]
          ))
          labels <- c(labels, paste("Unsupported", action, "claim"))
          explanations <- c(
            explanations,
            if (length(failed)) {
              paste0(
                "The assistant said the ",
                action,
                " was done, but every recorded call to ",
                paste(rule$tools, collapse = " or "),
                " before it failed or has no result."
              )
            } else {
              paste0(
                "The assistant said the ",
                action,
                " was done, but no call to ",
                paste(rule$tools, collapse = " or "),
                " is recorded before it."
              )
            }
          )
          primary <- c(primary, claims$event_id[[i]])
          supporting <- c(supporting, list(as.character(failed)))
          values <- c(
            values,
            list(list(action = action, attempted_calls = sum(before)))
          )
        }
      }
      scans::scanner_findings(
        event_id = primary,
        label = labels,
        explanation = explanations,
        event_ids = supporting,
        value = values
      )
    },
    description = "The assistant said an action was done with no successful call to the tool that performs it.",
    required_evidence = "Assistant text, and tool calls and results with call identities, without capture losses.",
    version = version,
    severity = "error",
    settings = list(actions = actions)
  )
}

#' Answer without retrieved support
#'
#' The assistant gives a substantive answer after a retrieval tool failed or
#' returned nothing, or, where commons provenance is recorded, in an exchange
#' whose provenance is untrusted (tag "C": no trusted or cited source).
#'
#' @param retrieval_tools Names of the tools that retrieve supporting sources.
#' @param min_chars An answer shorter than this is not substantive.
#' @param declined A regular expression, matched case-insensitively, for an
#'   answer that says nothing was found instead of answering.
answer_without_support_scanner <- function(
  retrieval_tools = "search_docs",
  min_chars = 60L,
  declined = "(could not|couldn't|cannot|can't|did not|didn't|unable to) (find|locate|retrieve)|no (relevant |matching )?(documents?|results?|sources?|information)",
  version = "1"
) {
  scans::scanner(
    "answer_without_support",
    function(trajectory, settings) {
      events <- trajectory$events
      answers <- domain_assistant_text(trajectory)
      provenance <- events[
        events$event_type %in% "commons:provenance",
        ,
        drop = FALSE
      ]
      results <- events[
        events$event_type %in%
          "tool_result" &
          events$name %in% settings$retrieval_tools,
        ,
        drop = FALSE
      ]
      if (nrow(answers) == 0L) {
        return(scans::scanner_unassessed(
          "insufficient_evidence",
          "No assistant text is recorded."
        ))
      }
      if (nrow(results) == 0L && nrow(provenance) == 0L) {
        if (domain_capture_is_lossy(trajectory$losses)) {
          return(scans::scanner_unassessed(
            "insufficient_evidence",
            "No retrieval result or provenance record is retained, and capture losses mean one may be missing."
          ))
        }
        return(scans::scanner_unassessed(
          "not_applicable",
          "The conversation records no retrieval call and no provenance."
        ))
      }
      tags <- vapply(
        provenance$value,
        function(value) {
          tag <- if (is.list(value)) value$provenance_tag else NULL
          if (is.character(tag) && length(tag) == 1L) tag else NA_character_
        },
        character(1)
      )
      if (nrow(results) == 0L && !any(tags %in% c("A", "B", "C"))) {
        return(scans::scanner_unassessed(
          "insufficient_evidence",
          "Provenance is recorded without a recognised tag, and no retrieval result is retained."
        ))
      }
      substantive <- nchar(answers$text) >= settings$min_chars &
        !grepl(settings$declined, answers$text, ignore.case = TRUE)
      users <- events$event_index[
        events$event_type %in%
          "content" &
          trajectory$turns$role[
            match(events$turn_id, trajectory$turns$turn_id)
          ] %in%
            "user"
      ]
      primary <- character()
      supporting <- list()
      labels <- character()
      explanations <- character()
      values <- list()

      # A retrieval result with no usable content, then a substantive answer
      # before the user speaks again and before a later retrieval succeeds.
      empty <- vapply(
        results$value,
        function(value) length(unlist(value)) == 0L,
        logical(1)
      ) &
        (is.na(results$text) | !nzchar(trimws(results$text)))
      unusable <- domain_result_failed(results) | empty
      for (i in which(unusable)) {
        at <- results$event_index[[i]]
        until <- min(c(users[users > at], Inf))
        after <- which(
          answers$event_index > at & answers$event_index < until & substantive
        )
        # A later retrieval supports an answer only when it succeeded before
        # that answer.
        recovered <- vapply(
          answers$event_index[after],
          function(answered) {
            any(
              !unusable &
                results$event_index > at &
                results$event_index < answered
            )
          },
          logical(1)
        )
        after <- after[!recovered]
        if (!length(after)) {
          next
        }
        answer <- answers$event_id[[after[[1L]]]]
        if (answer %in% primary) {
          supporting[[match(answer, primary)]] <- c(
            supporting[[match(answer, primary)]],
            results$event_id[[i]]
          )
          next
        }
        primary <- c(primary, answer)
        supporting <- c(supporting, list(results$event_id[[i]]))
        labels <- c(labels, "Answer after empty retrieval")
        explanations <- c(
          explanations,
          "The assistant answered after retrieval failed or returned nothing, without saying so."
        )
        values <- c(values, list(list(basis = "retrieval_result")))
      }

      # commons records one provenance entry per exchange, in order. Here the
      # nth entry is read against the nth user message, and the answer it
      # describes is the last assistant text before the next one. Adjust this
      # if your application adds user turns that are not questions.
      for (i in which(tags %in% "C")) {
        exchange <- provenance$metadata[[i]]$exchange_index
        if (is.null(exchange) || exchange > length(users)) {
          next
        }
        from <- users[[exchange]]
        until <- min(c(users[users > from], Inf))
        within <- answers$event_index > from &
          answers$event_index < until &
          substantive
        if (!any(within)) {
          next
        }
        answer <- answers$event_id[max(which(within))]
        if (answer %in% primary) {
          supporting[[match(answer, primary)]] <- c(
            supporting[[match(answer, primary)]],
            provenance$event_id[[i]]
          )
          next
        }
        primary <- c(primary, answer)
        supporting <- c(supporting, list(provenance$event_id[[i]]))
        labels <- c(labels, "Answer with untrusted provenance")
        explanations <- c(
          explanations,
          "The exchange's recorded provenance has no trusted or cited source."
        )
        values <- c(values, list(list(basis = "provenance")))
      }
      scans::scanner_findings(
        event_id = primary,
        label = labels,
        explanation = explanations,
        event_ids = supporting,
        value = values
      )
    },
    description = "The assistant gave a substantive answer with no retrieved or trusted source behind it.",
    required_evidence = "Assistant text, and retrieval tool results or commons provenance records.",
    version = version,
    settings = list(
      retrieval_tools = retrieval_tools,
      min_chars = min_chars,
      declined = declined
    )
  )
}
