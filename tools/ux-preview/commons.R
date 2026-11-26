# Deterministic commons cohort for the review app preview.
#
# Authored, provider-free conversations in the shape `commons::trajectory_read()`
# returns, built from public ellmer constructors. The builders mirror
# tests/testthat/helper-route-analysis.R; they are copied here so the preview
# does not depend on test helpers at runtime. The cohort repeats a few
# exploratory tool routes across users and days so the overview has provenance
# counts and promotion candidates to show. No SQL or R in it is ever run.
commons_call <- function(id, name, arguments, result) {
  list(id = id, name = name, arguments = arguments, result = result)
}

commons_conversation <- function(exchanges, provenance, user, last_active) {
  turns <- list()
  for (exchange in exchanges) {
    turns <- c(
      turns,
      list(ellmer::UserTurn(list(ellmer::ContentText(exchange$question))))
    )
    for (step in exchange$steps) {
      requests <- lapply(step, function(call) {
        ellmer::ContentToolRequest(
          id = call$id,
          name = call$name,
          arguments = call$arguments
        )
      })
      turns <- c(
        turns,
        list(ellmer::AssistantTurn(requests, tokens = exchange$tokens)),
        list(ellmer::UserTurn(Map(
          function(call, request) {
            ellmer::ContentToolResult(call$result, request = request)
          },
          step,
          requests
        )))
      )
    }
    if (isTRUE(exchange$reminder)) {
      turns <- c(
        turns,
        list(ellmer::UserTurn(list(ellmer::ContentText(
          "<system-instruction>Cite your sources.</system-instruction>"
        ))))
      )
    }
    turns <- c(
      turns,
      list(ellmer::AssistantTurn(
        list(ellmer::ContentText(exchange$answer)),
        tokens = exchange$tokens
      ))
    )
  }
  attr(turns, "provenance") <- lapply(provenance, function(tag) {
    list(provenance_tag = tag, citation_decisions = list())
  })
  list(
    turns = turns,
    last_active = as.POSIXct(last_active, tz = "UTC"),
    user = user
  )
}

commons_exchange <- function(kind, prefix, region, reminder = FALSE) {
  tokens <- c(180, 40, 0)
  exchange <- switch(
    kind,
    # Exploratory: look at the table, aggregate in SQL, finish in R.
    explore = list(
      question = paste0("What share of revenue came from ", region, "?"),
      steps = list(
        list(commons_call(
          paste0(prefix, "-describe"),
          "describe_table",
          list(table = "orders"),
          "region, amount, ordered_at"
        )),
        list(commons_call(
          paste0(prefix, "-sql"),
          "run_sql",
          list(
            sql = "SELECT region, SUM(amount) FROM orders o JOIN regions r ON o.region = r.id GROUP BY region"
          ),
          "west 120; east 80; north 40"
        )),
        list(commons_call(
          paste0(prefix, "-r"),
          "run_r",
          list(code = "prop.table(totals)"),
          "west 0.50; east 0.33; north 0.17"
        ))
      ),
      answer = paste0("From the orders table, ", region, " is about a third.")
    ),
    # Exploratory: a single ad hoc count.
    count = list(
      question = "How many orders were placed this week?",
      steps = list(list(commons_call(
        paste0(prefix, "-count"),
        "run_sql",
        list(
          sql = "SELECT COUNT(*) FROM orders WHERE ordered_at >= date('now', '-7 day')"
        ),
        "42"
      ))),
      answer = "42 orders this week."
    ),
    # Governed: an approved measure.
    measure = list(
      question = "What is total revenue?",
      steps = list(list(commons_call(
        paste0(prefix, "-measure"),
        "call_measure",
        list(name = "revenue"),
        "240"
      ))),
      answer = "Total revenue is 240 (measure: revenue)."
    ),
    # Cited: a documented definition.
    context = list(
      question = "How is churn defined?",
      steps = list(list(commons_call(
        paste0(prefix, "-search"),
        "search_context",
        list(query = "churn definition"),
        "Churn: no order in 90 days (data dictionary, section 4)."
      ))),
      answer = "Churn means no order in 90 days, per the data dictionary."
    )
  )
  exchange$tokens <- tokens
  exchange$reminder <- reminder
  exchange
}

# 18 conversations over nine days. Tags follow commons: A trusted, B cited,
# C untrusted. One exchange has no record and one has conflicting records, so
# "unclassified" is visible next to "untrusted".
demo_commons_cohort <- function(
  start = as.POSIXct("2026-11-16 09:00:00", tz = "UTC")
) {
  users <- sprintf("analyst%02d@example.com", 1:5)
  regions <- c("the west", "the east", "the north")
  plans <- list(
    list(c("explore", "measure"), list("C", "A")),
    list(c("explore", "count"), list("C", "C")),
    list(c("context", "measure"), list("B", "A")),
    list(c("count"), list("C")),
    list(c("explore"), list("C")),
    list(c("measure"), list("A")),
    list(c("explore", "context"), list("C", "B")),
    list(c("count", "measure"), list(c("A", "C"), "A")),
    list(c("explore", "count"), list("C"))
  )
  conversations <- lapply(seq_len(18L), function(i) {
    plan <- plans[[(i - 1L) %% length(plans) + 1L]]
    prefix <- sprintf("c%02d", i)
    exchanges <- lapply(seq_along(plan[[1L]]), function(j) {
      commons_exchange(
        plan[[1L]][[j]],
        paste0(prefix, "-", j),
        regions[[(i + j) %% 3L + 1L]],
        reminder = i %% 6L == 0L && j == 1L
      )
    })
    commons_conversation(
      exchanges,
      plan[[2L]],
      users[[(i * 2L) %% 5L + 1L]],
      start + ((i - 1L) %/% 2L) * 86400 + (i %% 2L) * 4 * 3600 + i * 60
    )
  })
  names(conversations) <- sprintf("analyst-%03d", seq_len(18L))
  conversations
}
