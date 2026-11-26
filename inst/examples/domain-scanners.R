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

# Content with nothing in it: no values, or only blank strings and empty
# JSON containers such as "[]".
domain_is_blank <- function(x) {
  x <- trimws(as.character(x[!is.na(x)]))
  all(x %in% c("", "[]", "{}", "null"))
}

# The exchange of each turn, by the rule scans' route analysis uses
# (route_turn_exchanges() in R/route-analysis.R, which has no public
# accessor): the turn's round_index when the trajectory records rounds;
# otherwise each user turn that holds no tool result and is not marked
# commons_internal, such as a commons reminder, opens the next exchange.
domain_turn_exchanges <- function(trajectory) {
  turns <- trajectory$turns
  turns <- turns[order(turns$turn_index), , drop = FALSE]
  if (!all(is.na(turns$round_index))) {
    index <- turns$round_index
  } else {
    events <- trajectory$events
    internal <- vapply(
      turns$metadata,
      function(metadata) {
        is.list(metadata) && isTRUE(metadata$commons_internal)
      },
      logical(1)
    )
    opens <- turns$role %in%
      "user" &
      !turns$turn_id %in%
        events$turn_id[events$event_type %in% "tool_result"] &
      !internal
    index <- cumsum(opens)
    index[index == 0L] <- NA
  }
  stats::setNames(as.integer(index), turns$turn_id)
}

# Each event's exchange: its turn's, or for an event outside any turn, that of
# the nearest earlier event in one.
domain_event_exchanges <- function(events, turn_exchange) {
  index <- unname(turn_exchange[match(events$turn_id, names(turn_exchange))])
  c(NA_integer_, index[!is.na(index)])[cumsum(!is.na(index)) + 1L]
}

# A provenance record's exchange_index when it is one whole number of at
# least 1, as the route analysis requires; otherwise NA.
domain_exchange_index <- function(metadata) {
  value <- if (is.list(metadata)) metadata$exchange_index else NULL
  if (
    is.numeric(value) &&
      length(value) == 1L &&
      is.finite(value) &&
      value >= 1 &&
      value == floor(value)
  ) {
    as.integer(value)
  } else {
    NA_integer_
  }
}

# Whether each text states a claim: some sentence matches `pattern`
# (case-insensitively), is not a question, and has no "not", "never",
# "cannot", or "n't" before the match. This drops the obvious negated and
# interrogative forms ("I have not issued a refund yet", "Have you cancelled
# the order?"). It is still phrase matching: a quoted claim, a conditional
# ("once your refund is processed"), or a negation after the phrase is read
# as a claim, and sentence breaks are guessed from ".", "!", and "?".
domain_states_claim <- function(text, pattern) {
  vapply(
    text,
    function(one) {
      sentences <- regmatches(one, gregexpr("[^.!?]+[.!?]*", one))[[1L]]
      any(vapply(
        sentences,
        function(sentence) {
          at <- regexpr(pattern, sentence, ignore.case = TRUE)
          at > 0L &&
            !grepl("[?]\\s*$", sentence) &&
            !grepl(
              "\\b(not|never|cannot)\\b|n't\\b",
              substr(sentence, 1L, at - 1L),
              ignore.case = TRUE,
              perl = TRUE
            )
        },
        logical(1)
      ))
    },
    logical(1),
    USE.NAMES = FALSE
  )
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
          domain_states_claim(answers$text, rule$claim),
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
      # Retrieval calls are found by tool name and their results by call
      # identity, since a result row need not repeat the tool name. A result
      # that names a retrieval tool counts even when its call was not kept.
      calls <- events[
        events$event_type %in%
          "tool_call" &
          events$name %in% settings$retrieval_tools,
        ,
        drop = FALSE
      ]
      call_ids <- calls$call_id[!is.na(calls$call_id)]
      results <- events[
        events$event_type %in%
          "tool_result" &
          (events$name %in%
            settings$retrieval_tools |
            (!is.na(events$call_id) & events$call_id %in% call_ids)),
        ,
        drop = FALSE
      ]
      if (nrow(answers) == 0L) {
        return(scans::scanner_unassessed(
          "insufficient_evidence",
          "No assistant text is recorded."
        ))
      }
      if (any(is.na(calls$call_id) | !calls$call_id %in% results$call_id)) {
        return(scans::scanner_unassessed(
          "insufficient_evidence",
          "A retrieval call has no recorded result, so what it returned is unknown."
        ))
      }
      if (nrow(results) == 0L && nrow(provenance) == 0L) {
        # as_trajectory_commons() records a "provenance" loss when a
        # conversation's provenance records are missing.
        if (
          domain_capture_is_lossy(trajectory$losses) ||
            any(trajectory$losses$field %in% "provenance")
        ) {
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
      turn_exchange <- domain_turn_exchanges(trajectory)
      event_exchange <- domain_event_exchanges(events, turn_exchange)
      exchange <- vapply(provenance$metadata, domain_exchange_index, integer(1))
      from_turn <- turn_exchange[match(
        provenance$turn_id,
        names(turn_exchange)
      )]
      exchange[is.na(exchange)] <- from_turn[is.na(exchange)]
      placed <- tags %in% c("A", "B", "C") & !is.na(exchange)
      if (nrow(results) == 0L && !any(placed)) {
        return(scans::scanner_unassessed(
          "insufficient_evidence",
          "Provenance is recorded without a recognised tag and a valid exchange, and no retrieval result is retained."
        ))
      }
      substantive <- nchar(answers$text) >= settings$min_chars &
        !grepl(settings$declined, answers$text, ignore.case = TRUE)
      answer_exchange <- event_exchange[match(
        answers$event_id,
        events$event_id
      )]
      result_exchange <- event_exchange[match(
        results$event_id,
        events$event_id
      )]
      primary <- character()
      supporting <- list()
      labels <- character()
      explanations <- character()
      values <- list()

      # A retrieval result with no usable content, then a substantive answer
      # in the same exchange with no successful retrieval between them.
      empty <- vapply(
        seq_len(nrow(results)),
        function(i) {
          domain_is_blank(c(unlist(results$value[[i]]), results$text[[i]]))
        },
        logical(1)
      )
      unusable <- domain_result_failed(results) | empty
      for (i in which(unusable)) {
        at <- results$event_index[[i]]
        after <- which(
          answers$event_index > at &
            answer_exchange %in% result_exchange[[i]] &
            substantive
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

      # Each provenance record names its exchange (metadata$exchange_index,
      # or the exchange of its turn), and the answer it describes is the last
      # substantive assistant text in that exchange. A record whose exchange
      # is missing or malformed is unclassified and is not used.
      for (i in which(tags %in% "C" & !is.na(exchange))) {
        within <- answer_exchange %in% exchange[[i]] & substantive
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
