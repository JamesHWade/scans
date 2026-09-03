# Read OpenTelemetry traces from Posit Connect

`read_connect_traces()` fetches a deployed application's GenAI spans
from Posit Connect's content observability store and groups them into
conversations, ready for
[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md).

Reading traces requires `CONNECT_SERVER` and `CONNECT_API_KEY`, and the
key's user must own the content or be a collaborator on it: Connect
gates the traces endpoint at editor level.

## Usage

``` r
read_connect_traces(
  source,
  n = 100L,
  from = NULL,
  to = NULL,
  server = NULL,
  api_key = NULL,
  max_spans = 50000L,
  jobs = TRUE
)
```

## Arguments

- source:

  A Posit Connect content GUID, a content URL (`.../content/<guid>/`),
  or a dashboard URL (`.../connect/#/apps/<guid>/`).

- n:

  Maximum number of recent conversations to keep. `NULL` keeps all.

- from, to:

  Optional lower-inclusive and upper-exclusive bounds on the start time
  of a conversation's model calls: a conversation is kept when one of
  its model calls started in the window, and its earlier spans are kept
  with it as far back as the read reached (an hour before `from`).
  Accepts POSIXct, Date, or ISO 8601 strings; a string without a zone is
  read as UTC. `NULL` leaves that side open. When both are omitted, the
  seven days ending now are read.

- server, api_key:

  Connect server URL and API key. Default to `CONNECT_SERVER` and
  `CONNECT_API_KEY`.

- max_spans:

  Ceiling on how many GenAI spans are read. Only spans carrying
  `gen_ai.*` attributes count towards it: Shiny applications emit
  thousands of reactive spans per session, and those are read past
  rather than allowed to exhaust the budget before the conversations are
  reached. Framework spans are kept only as long as grouping needs them
  to link a model call to its conversation, and are dropped afterwards.

- jobs:

  Whether to also read the retained per-job trace stores that predate
  the content-wide store (see *Trace endpoints*). The default, `TRUE`,
  is complete; `FALSE` skips those requests, which on a deployment with
  many past jobs is most of the read time, and is safe once the
  content-wide store is known to hold everything of interest.

## Value

A named list of conversations, each a list of spans, oldest-first. The
names are conversation identifiers. The `"read_info"` attribute records
how the read went: the window, how many spans and conversations were
found, whether the `max_spans` ceiling cut the read short (`truncated`),
and whether a page failed to load (`incomplete`).

## Trace endpoints

Connect exposes traces both for a content item as a whole and per job.
The current content-wide store and retained per-job stores are merged
and de-duplicated because Connect did not migrate legacy traces into the
current store. Only jobs that started before the content-wide store's
earliest span (and that overlap the window) are read, since later jobs
are already in it. When the content-wide endpoint is missing or fails,
the per-job store is used alone.

## Progress

Set `options(scans.progress = function(message) ...)` to be told about
each page and job as it is read;
[`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
uses this to show progress while a load runs.

## See also

[`as_trajectory_otel()`](https://jameshwade.github.io/scans/reference/as_trajectory_otel.md)
to convert the result, and
[`scans_app_connect()`](https://jameshwade.github.io/scans/reference/scans_app_connect.md)
to review it.
