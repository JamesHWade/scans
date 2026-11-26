# Read local OpenTelemetry trace files

`read_otel_traces()` reads GenAI spans from OTLP NDJSON files written by
otelsdk's file exporter and groups them into conversations, ready for
[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md).
This is where `commons::agent(log = TRUE)` and any ellmer or deputy
application traced with `OTEL_TRACES_EXPORTER=otlp/file` write their
spans when they are not running on Posit Connect.

The exporter rotates through `trace-0.jsonl`, `trace-1.jsonl`, ... and
keeps `trace-latest.jsonl` pointing at the newest file as a symbolic
link, hard link, or copy. Each span is read once: files that resolve to
the same path or have identical contents are read once, and a span that
still appears in two files (a copy taken mid-write) is kept once.

## Usage

``` r
read_otel_traces(
  path = NULL,
  n = 100L,
  from = NULL,
  to = NULL,
  max_spans = 50000L
)
```

## Arguments

- path:

  Trace files to read: one or more OTLP NDJSON files, or a directory of
  them. A directory contributes its `trace.jsonl`, `trace-<n>.jsonl`,
  and `trace-latest.jsonl` files, plus files named from the
  `OTEL_EXPORTER_OTLP_TRACES_FILE` (or `OTEL_EXPORTER_OTLP_FILE`)
  pattern when one is set. `NULL` reads the directory of that pattern,
  and is an error when neither variable names a file.

- n:

  Maximum number of recent conversations to keep. `NULL` keeps all.

- from, to:

  Optional lower-inclusive and upper-exclusive bounds on the start time
  of a conversation's model calls, as for
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md):
  a conversation is kept when one of its model calls started in the
  window, with all of its spans before `to`. Accepts POSIXct, Date, or
  ISO 8601 strings; a string without a zone is read as UTC. `NULL`
  leaves that side open. Unlike
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md),
  the default reads every file in full rather than the last seven days.

- max_spans:

  Ceiling on how many GenAI spans are kept, counting back from the most
  recent. Only spans carrying `gen_ai.*` attributes count towards it, as
  for
  [`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md);
  commons turn and provenance spans and Deputy run spans never count.

## Value

A named list of conversations, each a list of spans, oldest-first, in
the same form as
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md).
The `"source_uri"` attribute is a `file://` URI when a single file or
directory was read. The `"read_info"` attribute records the window, how
many spans and conversations were found, whether the `max_spans` ceiling
cut the read short (`truncated`), how many files were read (`files`),
and how many lines could not be parsed (`lines_dropped`); `incomplete`
is `TRUE` when any line was dropped.

## See also

[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
to convert the result, and
[`read_connect_traces()`](https://jameshwade.github.io/scans/reference/read_connect_traces.md)
for traces retained by Posit Connect.

## Examples

``` r
path <- system.file("extdata", "otel-traces", package = "scans")
conversations <- read_otel_traces(path)
attr(conversations, "read_info")$conversations
#> [1] 1

bundle <- as_trajectory_otel(conversations)
trajectory_turns(bundle)[, c("trajectory_id", "role")]
#> # A tibble: 5 × 2
#>   trajectory_id  role     
#>   <chr>          <chr>    
#> 1 otel/support-1 system   
#> 2 otel/support-1 user     
#> 3 otel/support-1 assistant
#> 4 otel/support-1 tool     
#> 5 otel/support-1 assistant
```
