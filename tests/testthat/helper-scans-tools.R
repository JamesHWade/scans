scans_support_bundle <- function() {
  as_trajectory_otel(jsonlite::read_json(system.file(
    "extdata",
    "support-investigation.json",
    package = "scans"
  )))
}

scans_tool_result <- function(tool, ...) {
  jsonlite::fromJSON(tool(...), simplifyVector = FALSE)
}

scans_test_chat <- function(control = NULL, delay = 0) {
  state <- new.env(parent = emptyenv())
  state$turns <- list()
  state$tools <- list()
  state$prompt <- ""
  state$requests <- list()
  if (!is.null(control)) {
    control$instances <- c(control$instances, list(state))
  }
  chat <- structure(
    list(
      clone = function(deep = FALSE) scans_test_chat(control, delay),
      get_model = function() "Fixture responses \u00b7 no model calls",
      get_provider = function() structure(list(), class = "Deterministic"),
      get_turns = function() state$turns,
      set_turns = function(turns) {
        state$turns <- turns
      },
      get_tools = function() state$tools,
      set_tools = function(tools) {
        state$tools <- tools
      },
      get_system_prompt = function() state$prompt,
      set_system_prompt = function(prompt) {
        state$prompt <- prompt
      },
      last_turn = function() {
        if (length(state$turns)) state$turns[[length(state$turns)]] else NULL
      }
    ),
    class = "Chat"
  )
  chat$stream_async <- function(..., stream = "content", controller = NULL) {
    question <- rlang::list2(...)
    state$controller <- controller
    controller$reset()
    state$turns <- c(
      state$turns,
      list(ellmer::UserTurn(list(
        ellmer::ContentText(paste(unlist(question), collapse = " "))
      )))
    )
    coro::async_generator(function() {
      results <- list()
      calls <- list(
        scans_summarize = list(),
        scans_find_events = list(query = "timeout")
      )
      for (name in names(calls)) {
        request <- ellmer::ContentToolRequest(
          id = paste0("fixture-", name),
          name = name,
          arguments = calls[[name]],
          tool = state$tools[[name]]
        )
        state$requests <- c(state$requests, list(request))
        coro::yield(request)
        if (!is.null(control) && isTRUE(control$hold)) {
          coro::await(promises::promise(function(resolve, reject) {
            control$resume <- resolve
          }))
        } else if (delay > 0) {
          coro::await(promises::promise(function(resolve, reject) {
            later::later(function() resolve(NULL), delay)
          }))
        }
        if (controller$cancelled) {
          return()
        }
        value <- do.call(state$tools[[name]], calls[[name]])
        results[[name]] <- jsonlite::fromJSON(value, simplifyVector = FALSE)
        coro::yield(ellmer::ContentToolResult(value = value, request = request))
      }
      summary <- results$scans_summarize$data[[1L]]
      events <- results$scans_find_events$data
      refs <- results$scans_find_events$evidence
      if (length(events)) {
        answer <- paste0(
          length(events),
          " recorded tool events contain a timeout. ",
          "The selected scans found ",
          sum(vapply(
            results$scans_summarize$patterns,
            function(x) x$findings,
            numeric(1)
          )),
          " findings across ",
          summary$n_findings,
          " of ",
          summary$n,
          " trajectories.\n\n",
          paste(
            vapply(
              seq_along(refs),
              function(i) {
                sprintf(
                  "[Event %d: %s](%s)",
                  events[[i]]$event_index,
                  events[[i]]$name,
                  refs[[i]]$url %||% "#"
                )
              },
              character(1)
            ),
            collapse = " \u00b7 "
          ),
          "\n\nThe underlying cause is not recorded."
        )
      } else {
        answer <- sprintf(
          "The scope contains %d trajectories, with findings in %d. No timeout events were returned. Missing capture does not establish success.",
          summary$n,
          summary$n_findings
        )
      }
      content <- ellmer::ContentText(answer)
      state$turns <- c(state$turns, list(ellmer::AssistantTurn(list(content))))
      coro::yield(content)
    })()
  }
  chat
}

scans_test_drain <- function(session, done = function() TRUE) {
  for (i in seq_len(100L)) {
    later::run_now(0)
    session$flushReact()
    if (done()) return(invisible(NULL))
  }
  stop("The local chat did not settle.")
}
