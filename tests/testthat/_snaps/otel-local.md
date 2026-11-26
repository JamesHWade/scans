# the span budget keeps the most recent spans

    Code
      conversations <- read_otel_traces(dir, max_spans = 1L)
    Condition
      Warning:
      Kept the most recent 1 span; raise `max_spans` to read further back.

# missing paths and unreadable traces are reported

    Code
      read_otel_traces()
    Condition
      Error:
      ! `path` must name trace files or a directory of them.
      i Set `OTEL_EXPORTER_OTLP_TRACES_FILE` to read the exporter's directory by default.
    Code
      read_otel_traces(file.path(dir, "absent.jsonl"))
    Condition
      Error:
      ! Can't find '<tmp>/absent.jsonl'.
    Code
      read_otel_traces(file.path(dir, "no-traces"))
    Condition
      Error:
      ! Can't find OTLP trace files ('trace-*.jsonl') in '<tmp>/no-traces'.
    Code
      read_otel_traces(empty)
    Condition
      Error:
      ! No OpenTelemetry spans could be parsed from the trace files.
      i Read '<tmp>/trace-0.jsonl'.

