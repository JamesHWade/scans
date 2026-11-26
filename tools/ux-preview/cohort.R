# Deterministic demo cohort for the review app preview.
#
# Authored, provider-free OpenTelemetry GenAI spans shaped like what a deployed
# ellmer support assistant writes to Posit Connect. No production messages,
# provider responses, or API calls are used. The cohort has a deliberate story
# a reviewer should be able to find:
#
# - an order-service outage on days 9-10 makes `lookup_order` time out and the
#   assistant retry in a loop;
# - `refund_status` fails intermittently all fortnight;
# - a model change on day 8 raises token use;
# - a few conversations have no captured content or never finished.
demo_cohort_spans <- function(
  n = 240L,
  days = 14L,
  end = as.POSIXct("2026-11-24 18:00:00", tz = "UTC"),
  seed = 20261124L
) {
  withr::local_seed(seed)
  origin <- as.numeric(end) - days * 86400
  users <- sprintf("user%02d@example.com", 1:14)
  orders <- sprintf("DEMO-%03d", 100:180)
  items <- c(
    "blue jacket",
    "trail boots",
    "wool scarf",
    "rain shell",
    "day pack"
  )

  message <- function(role, content) {
    list(role = role, parts = list(list(type = "text", content = content)))
  }
  request <- function(id, name, arguments) {
    list(type = "tool_call", id = id, name = name, arguments = arguments)
  }
  response <- function(id, text) {
    list(
      role = "tool",
      parts = list(list(type = "tool_call_response", id = id, response = text))
    )
  }

  st <- new.env(parent = emptyenv())
  st$spans <- list()
  add <- function(conversation, id, start, end, attributes, user) {
    st$spans[[length(st$spans) + 1L]] <- list(
      trace_id = paste0("trace-", conversation),
      span_id = paste0(conversation, "-", id),
      parent_span_id = "",
      name = id,
      scope = "scans.demo-cohort",
      start_time = sprintf("%.0f", start * 1e9),
      end_time = if (is.na(end)) NULL else sprintf("%.0f", end * 1e9),
      attributes = c(
        list("gen_ai.conversation.id" = conversation, "user.id" = user),
        attributes
      )
    )
  }

  for (i in seq_len(n)) {
    conversation <- sprintf("demo-%04d", i)
    day <- sample.int(
      days,
      1L,
      prob = rep(c(1, 1.2, 1.3, 1.2, 1.1, 0.4, 0.3), length.out = days)
    )
    at <- origin + (day - 1) * 86400 + runif(1, 8, 19) * 3600
    user <- sample(users, 1L, prob = c(6, 5, 4, rep(1, 11)))
    model <- if (day >= 8L) "example-model-large" else "example-model-small"
    token_scale <- if (day >= 8L) 1.9 else 1
    outage <- day %in% 9:10
    st$clock <- at
    state <- new.env(parent = emptyenv())
    state$step <- 0L

    chat <- function(
      input,
      output,
      seconds,
      tokens_in,
      tokens_out,
      capture = TRUE,
      finish = TRUE
    ) {
      state$step <- state$step + 1L
      start <- st$clock
      st$clock <- st$clock + seconds
      add(
        conversation,
        paste0("chat-", state$step),
        start,
        if (finish) st$clock else NA,
        c(
          list(
            "gen_ai.operation.name" = "chat",
            "gen_ai.request.model" = model,
            "gen_ai.usage.input_tokens" = round(tokens_in * token_scale),
            "gen_ai.usage.output_tokens" = round(tokens_out * token_scale)
          ),
          if (capture) {
            list(
              "gen_ai.input.messages" = jsonlite::toJSON(
                input,
                auto_unbox = TRUE
              ),
              "gen_ai.output.messages" = jsonlite::toJSON(
                output,
                auto_unbox = TRUE
              )
            )
          }
        ),
        user
      )
    }
    tool <- function(id, name, seconds, error = NULL) {
      start <- st$clock
      st$clock <- st$clock + seconds
      add(
        conversation,
        id,
        start,
        st$clock,
        c(
          list(
            "gen_ai.operation.name" = "execute_tool",
            "gen_ai.tool.call.id" = id,
            "gen_ai.tool.name" = name
          ),
          if (!is.null(error)) list("error.type" = error)
        ),
        user
      )
    }

    scenario <- sample(
      c(
        "policy",
        "order",
        "refund",
        "stock",
        "long",
        "uncaptured",
        "unfinished"
      ),
      1L,
      prob = c(22, if (outage) 60 else 26, 16, 18, 8, 5, 3)
    )

    if (scenario == "policy") {
      item <- sample(items, 1L)
      history <- list(message(
        "user",
        paste0("Can I return the ", item, " if it is unopened?")
      ))
      answer <- list(message(
        "assistant",
        "Unopened items can be returned within the example store's 30-day window."
      ))
      chat(history, answer, runif(1, 1.5, 4), 90, 25)
      if (runif(1) < 0.4) {
        st$clock <- st$clock + runif(1, 20, 240)
        history <- c(
          history,
          answer,
          list(message("user", "Does that include gifts?"))
        )
        chat(
          history,
          list(message(
            "assistant",
            "Yes. Keep the gift receipt so the support desk can find the order."
          )),
          runif(1, 1.5, 4),
          150,
          30
        )
      }
    } else if (scenario == "order") {
      order <- sample(orders, 1L)
      history <- list(message(
        "user",
        paste0("Where is order ", order, "? The tracking page has not changed.")
      ))
      fails <- if (outage) {
        sample(2:4, 1L, prob = c(2, 5, 2))
      } else if (runif(1) < 0.06) {
        1L
      } else {
        0L
      }
      for (attempt in seq_len(fails)) {
        id <- paste0("lookup-", attempt)
        output <- list(list(
          role = "assistant",
          parts = list(request(id, "lookup_order", list(order = order)))
        ))
        chat(history, output, runif(1, 2, 4), 100 * attempt, 20)
        tool(id, "lookup_order", runif(1, 5, 12), "order_service_timeout")
        history <- c(
          history,
          output,
          list(response(
            id,
            "The order service timed out. No shipment data was returned."
          ))
        )
      }
      if (fails >= 2L) {
        chat(
          history,
          list(message(
            "assistant",
            "I could not retrieve this order. Please contact the support desk; I cannot confirm its shipping status."
          )),
          runif(1, 2, 5),
          400,
          35
        )
      } else {
        id <- "lookup-ok"
        output <- list(list(
          role = "assistant",
          parts = list(request(id, "lookup_order", list(order = order)))
        ))
        chat(history, output, runif(1, 2, 4), 110, 20)
        tool(id, "lookup_order", runif(1, 0.4, 1.6))
        history <- c(
          history,
          output,
          list(response(
            id,
            paste0("Order ", order, " shipped and is out for delivery.")
          ))
        )
        chat(
          history,
          list(message(
            "assistant",
            paste0(
              "Order ",
              order,
              " has shipped and is out for delivery today."
            )
          )),
          runif(1, 2, 4),
          220,
          30
        )
      }
    } else if (scenario == "refund") {
      order <- sample(orders, 1L)
      history <- list(message(
        "user",
        paste0("Has the refund for ", order, " been issued?")
      ))
      id <- "refund-1"
      output <- list(list(
        role = "assistant",
        parts = list(request(id, "refund_status", list(order = order)))
      ))
      chat(history, output, runif(1, 2, 4), 105, 22)
      failed <- runif(1) < 0.22
      tool(
        id,
        "refund_status",
        runif(1, 0.6, 3),
        if (failed) "payments_api_unavailable"
      )
      history <- c(
        history,
        output,
        list(response(
          id,
          if (failed) {
            "The payments API was unavailable."
          } else {
            "Refund issued; funds arrive in 3-5 business days."
          }
        ))
      )
      chat(
        history,
        list(message(
          "assistant",
          if (failed) {
            "I could not check the refund right now. Please try again in a few minutes."
          } else {
            "The refund has been issued and should arrive in 3-5 business days."
          }
        )),
        runif(1, 2, 4),
        230,
        32
      )
    } else if (scenario == "stock") {
      item <- sample(items, 1L)
      first <- list(message(
        "user",
        paste0("Is the ", item, " in stock, and what is its return window?")
      ))
      calls <- list(list(
        role = "assistant",
        parts = list(
          request("stock", "check_stock", list(item = item)),
          request("policy", "return_policy", list(item = item))
        )
      ))
      chat(first, calls, runif(1, 2, 4), 120, 40)
      tool("stock", "check_stock", runif(1, 1, 5))
      tool("policy", "return_policy", runif(1, 0.5, 2))
      history <- c(
        first,
        calls,
        list(
          response("stock", paste0("The ", item, " is in stock.")),
          response("policy", "Unworn items have a 30-day return window.")
        )
      )
      chat(
        history,
        list(message(
          "assistant",
          paste0(
            "The ",
            item,
            " is in stock. You have 30 days to return it unworn."
          )
        )),
        runif(1, 2, 4),
        230,
        35
      )
    } else if (scenario == "long") {
      history <- list()
      for (round in seq_len(sample(5:9, 1L))) {
        item <- sample(items, 1L)
        history <- c(
          history,
          list(message(
            "user",
            paste0(
              "Tell me about caring for the ",
              item,
              " (question ",
              round,
              ")."
            )
          ))
        )
        answer <- list(message(
          "assistant",
          paste0(
            "Care guidance for the ",
            item,
            ": wash cold, dry flat, and store it dry."
          )
        ))
        chat(history, answer, runif(1, 3, 9), 300 * round, 120)
        history <- c(history, answer)
        st$clock <- st$clock + runif(1, 15, 180)
      }
    } else if (scenario == "uncaptured") {
      chat(NULL, NULL, runif(1, 2, 4), 75, 20, capture = FALSE)
    } else {
      history <- list(message("user", "I need help with a damaged delivery."))
      chat(history, NULL, runif(1, 2, 4), 80, 0, capture = TRUE, finish = FALSE)
    }
  }
  st$spans
}
