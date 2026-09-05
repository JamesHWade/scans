resource_span <- function(
  id,
  start = 0,
  end = 1,
  operation = "chat",
  extra = list(),
  parent = ""
) {
  list(
    trace_id = "resource-test",
    span_id = id,
    parent_span_id = parent,
    start_time = as.character(start * 1e9),
    end_time = as.character(end * 1e9),
    attributes = c(
      list(
        "gen_ai.operation.name" = operation,
        "gen_ai.conversation.id" = "resource-test"
      ),
      extra
    )
  )
}

resource_row <- function(bundle, measure) {
  rows <- measure_trajectories(bundle)
  rows[rows$measure %in% measure, ]
}
