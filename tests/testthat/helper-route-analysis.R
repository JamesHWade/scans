# A commons trajectory_read() result built from public ellmer constructors.
# Each exchange is list(question, steps, answer, tokens), where steps is a list
# of assistant turns, each a list of list(name, arguments, result).
route_conversation <- function(exchanges, provenance, user, last_active) {
  turns <- list()
  for (exchange in exchanges) {
    turns <- c(
      turns,
      list(ellmer::UserTurn(list(
        ellmer::ContentText(exchange$question)
      )))
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
        list(ellmer::AssistantTurn(
          requests,
          tokens = exchange$tokens %||% c(NA_real_, NA_real_, NA_real_)
        )),
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
        tokens = exchange$tokens %||% c(NA_real_, NA_real_, NA_real_)
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

route_call <- function(id, name, arguments, result) {
  list(id = id, name = name, arguments = arguments, result = result)
}

route_exploration <- function(prefix, question, tokens = NULL) {
  list(
    question = question,
    steps = list(
      list(route_call(
        paste0(prefix, "-describe"),
        "describe_table",
        list(table = "orders"),
        "region, amount, ordered_at"
      )),
      list(route_call(
        paste0(prefix, "-sql"),
        "run_sql",
        list(
          sql = "SELECT region, SUM(amount) FROM orders o JOIN regions r ON o.region = r.id GROUP BY region"
        ),
        "west 120; east 80"
      )),
      list(route_call(
        paste0(prefix, "-r"),
        "run_r",
        list(code = "prop.table(totals)"),
        "west 0.6; east 0.4"
      ))
    ),
    answer = "West has 60% of revenue.",
    tokens = tokens
  )
}

route_single <- function(id, name, arguments, result, question) {
  list(
    question = question,
    steps = list(list(route_call(id, name, arguments, result))),
    answer = "Done."
  )
}

# Four conversations, nine exchanges: three untrusted, three cited, one
# trusted, one with conflicting tags, and one with no provenance record.
route_commons_cohort <- function() {
  list(
    `conversation-001` = route_conversation(
      list(
        c(
          route_exploration("c1", "Share of revenue by region?", c(100, 20, 0)),
          list(reminder = TRUE)
        ),
        c(
          route_single(
            "c1-measure",
            "call_measure",
            list(name = "revenue", arguments = "{}"),
            "200",
            "Total revenue?"
          ),
          list(tokens = c(50, 5, 0))
        )
      ),
      c("C", "A"),
      "ana",
      "2026-11-01 10:00:00"
    ),
    `conversation-002` = route_conversation(
      list(
        route_exploration("c2", "Regional revenue split?", c(300, 30, 0)),
        route_single(
          "c2-count",
          "run_sql",
          list(sql = "SELECT COUNT(*) FROM orders"),
          "42",
          "How many orders?"
        )
      ),
      c("C", "B"),
      "ben",
      "2026-11-02 10:00:00"
    ),
    `conversation-003` = route_conversation(
      list(
        route_exploration("c3", "Revenue mix by region?"),
        list(
          question = "Orders this week?",
          steps = list(
            list(route_call(
              "c3-sql-1",
              "run_sql",
              list(sql = "SELECT COUNT(*) FROM orders"),
              "Error: no such column"
            )),
            list(route_call(
              "c3-sql-2",
              "run_sql",
              list(sql = "SELECT COUNT(*) FROM orders"),
              "7"
            ))
          ),
          answer = "Seven."
        ),
        route_single(
          "c3-measure",
          "call_measure",
          list(name = "revenue"),
          "200",
          "Revenue again?"
        )
      ),
      list("B", c("A", "C")),
      NA_character_,
      "2026-11-03 10:00:00"
    ),
    `conversation-004` = route_conversation(
      list(
        list(
          question = "Which customers churned?",
          steps = list(
            list(
              route_call(
                "c4-search",
                "search_context",
                list(query = "churn"),
                "No documents."
              ),
              route_call(
                "c4-describe",
                "describe_table",
                list(table = "customers"),
                "id, churned"
              )
            ),
            list(route_call(
              "c4-sql",
              "run_sql",
              list(sql = "SELECT id FROM customers WHERE churned"),
              "3 rows"
            ))
          ),
          answer = "Three customers."
        ),
        route_single(
          "c4-sql-count",
          "run_sql",
          list(sql = "SELECT COUNT(*) FROM orders"),
          "42",
          "Order count?"
        )
      ),
      c("C", "B"),
      "ana",
      "2026-11-04 10:00:00"
    )
  )
}

route_commons_bundle <- function() {
  skip_if_not_installed("ellmer", "0.5.0")
  as_trajectory_commons(route_commons_cohort())
}

# Two plain ellmer chats with the same two-step route and no provenance.
route_ellmer_bundle <- function() {
  skip_if_not_installed("ellmer", "0.5.0")
  chat <- function(id) {
    lookup <- ellmer::ContentToolRequest(
      paste0(id, "-lookup"),
      "lookup",
      list(key = "orders")
    )
    sum_call <- ellmer::ContentToolRequest(
      paste0(id, "-sum"),
      "summarise",
      list(column = "amount")
    )
    as_trajectory_ellmer(
      list(
        ellmer::UserTurn(list(ellmer::ContentText("Total orders?"))),
        ellmer::AssistantTurn(list(lookup)),
        ellmer::UserTurn(list(ellmer::ContentToolResult(
          "rows",
          request = lookup
        ))),
        ellmer::AssistantTurn(list(sum_call)),
        ellmer::UserTurn(list(ellmer::ContentToolResult(
          "42",
          request = sum_call
        ))),
        ellmer::AssistantTurn(list(ellmer::ContentText("42 orders.")))
      ),
      trajectory_id = id
    )
  }
  bundles <- lapply(c("ellmer-1", "ellmer-2"), chat)
  TrajectoryBundle(
    trajectory_bind_rows(lapply(bundles, trajectory_info)),
    trajectory_bind_rows(lapply(bundles, trajectory_turns)),
    trajectory_bind_rows(lapply(bundles, trajectory_events))
  )
}
