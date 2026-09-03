# OTEL conversion requires jsonlite

    Code
      as_trajectory_otel(list())
    Condition
      Error:
      ! Converting OpenTelemetry spans requires jsonlite.
      i Install jsonlite to enable it.

# a transient first aggregate failure aborts without job fallback

    Code
      connect_trace_lines(client = list(server = "https://connect.example.com",
        api_key = "secret"), guid = "11111111-1111-4111-8111-111111111111", from = NULL,
      to = NULL, max_spans = 10L, call = rlang::caller_env(), jobs = FALSE)
    Condition
      Error:
      ! Couldn't read this content's traces from Posit Connect.

