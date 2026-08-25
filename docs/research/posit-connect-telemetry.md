# Posit Connect native trace storage for `scans_app`

Research snapshot: 2026-08-25. Sources are limited to official Posit documentation and the official `posit-dev/commons` repository.

## Conclusion

Posit Connect now has a native content-observability path for application traces. For a reviewer app deployed on the same Connect server, `scans_app` should read retained OTLP trace envelopes directly from each selected content item. A pin or package-managed trace archive should be an explicit import/export fallback, not the primary Connect integration.

The general Connect OpenTelemetry facility arrived in 2026.02.0. For this integration, use Connect 2026.07.0 or later: the current official Commons implementation identifies 2026.07 as the migration point from legacy per-job trace files to the content-wide trace store. The content trace read endpoint is used by official Posit code but is not documented in the public Connect API reference, so `scans` should isolate it behind a small adapter and retain a legacy fallback. The strictly documented programmatic alternatives are host-side JSONL signal files or export to an OTLP backend and that backend's query API.

## Enable and capture traces

An administrator enables OpenTelemetry and permits publishers to instrument content:

```ini
[OpenTelemetry]
Enabled = true
AllowContentInstrumentation = true
```

The owner or collaborator then enables **Settings > Monitoring > Content Observability** for each deployed app. The equivalent API operation is:

```http
PATCH /__api__/v1/content/{guid}
Authorization: Key ${CONNECT_API_KEY}
Content-Type: application/json

{"otel_enabled": true}
```

Both server permission and `otel_enabled` must be true. The setting applies to newly started content processes, so restart or redeploy the app after enabling it. Connect configures OpenTelemetry in the content process; compatible R libraries emit without app-specific tracing code when `otelsdk` is available. Relevant minimum package versions include `ellmer` 0.5.0, `shiny` 1.12.0, `httr2` 1.2.2, `DBI` 1.3.0, `testthat` 3.3.2, and `mirai` 2.5.0.

For trajectory review, ellmer's GenAI spans are the useful payload. Full prompt/response content is captured only when the content process sets `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true`; this is sensitive data and must remain server-side. Official Commons adds a conversation identifier and uses these spans as the trajectory source.

Sources: [Content settings and observability](https://docs.posit.co/connect/user/content-settings/#content-observability), [compatible trace libraries](https://docs.posit.co/connect/user/traces/compatible-libraries.html), [content API `otel_enabled`](https://docs.posit.co/connect/api/#patch-/v1/content/-guid-), [Commons enablement and restart behavior](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/tracing.R#L292-L340), [Commons GenAI capture](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/tracing.R#L220-L240).

## Native trace read contract

Official Commons reads the current content-wide store with:

```http
GET /__api__/v1/content/{guid}/traces?limit={n}&offset={n}&from={rfc3339}&to={rfc3339}
Authorization: Key ${CONNECT_API_KEY}
```

- The body is newline-delimited JSON; every non-empty line is an OTLP JSON envelope (`resourceSpans` -> `scopeSpans` -> `spans`).
- `X-Total-Count` reports the number of matching rows.
- Results are newest-first by span start time.
- `from` is inclusive and `to` is exclusive; use UTC RFC3339 timestamps.
- Page with `limit` and `offset`, and always request a bounded time window.

The 2026.07 migration did not move older trace files into the new store. The official compatibility algorithm also reads:

```http
GET /__api__/v1/content/{guid}/jobs
GET /__api__/v1/content/{guid}/jobs/{job_key}/traces?limit={n}&offset={n}&since={rfc3339}
```

It merges and de-duplicates content-wide and legacy per-job results; a 404 from the content-wide endpoint triggers legacy-only fallback. There are currently no trace-list/query methods in the official R `connectapi` or Python `posit-sdk` surfaces, and the trace endpoints are absent from the public API reference. Use `httr2` (or an equivalent HTTP client) and treat this as a versioned internal adapter, not a public Connect API guarantee. Official Commons' `trajectory_read()` is the reference R implementation and already accepts a GUID, content URL, dashboard URL, or the current deployed content.

Sources: [Commons content-wide reader](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/connect.R#L45-L107), [Commons legacy reader](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/connect.R#L109-L194), [Commons `trajectory_read()` source](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/trajectory-read.R), [public Connect API and SDK guidance](https://docs.posit.co/connect/api/).

## Authentication and access in deployed content

Connect injects the following runtime variables by default:

- `POSIT_PRODUCT=CONNECT`
- `CONNECT_SERVER`: the server's public URL
- `CONNECT_API_KEY`: an ephemeral API key acting as the content owner for the life of the process
- `CONNECT_CONTENT_GUID` and `CONNECT_CONTENT_JOB_KEY`

Administrators can disable server/key injection with `Applications.DefaultServerEnv` and `Applications.DefaultAPIKeyEnv`. If disabled, configure server-side `CONNECT_SERVER` and `CONNECT_API_KEY` values explicitly. Never send the key to the browser.

Trace reads require the API-key identity to own or collaborate on the target content; reviewer/viewer access is insufficient. Consequently, a `scans_app` deployment can switch smoothly among apps on the same Connect instance when its deployment owner is a collaborator on every target. This access is the deployment owner's authority, not the current browser viewer's authority, so expose only the target set that owner intends the app to review. Different Connect servers require separate clients and credentials.

Sources: [Connect runtime environment variables](https://docs.posit.co/connect/user/content-settings/#environment-variables), [API key authentication](https://docs.posit.co/connect/user/api-keys/), [Commons client defaults](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/connect.R#L4-L43), [Commons trace-access check](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/connect.R#L243-L257), [Commons collaborator grant](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/connect.R#L296-L308).

## Enumerate and switch among deployed apps

For a same-server content chooser:

1. List content visible to the owner with `GET /__api__/v1/content`, or use `GET /__api__/v1/search/content?query=...` for scalable filtered discovery.
2. Keep items for which `otel_enabled` is true and `app_role` is `owner` or `editor`; optionally restrict to interactive content types or an allow-list/tag. A visible item can still be trace-inaccessible, so handle 401/403.
3. Show title/owner metadata but use the immutable content `guid` as the selection value.
4. On selection, lazily query that GUID's traces for the chosen time window. Cache by `(server, guid, from, to)` and provide an explicit refresh; do not ingest every app on startup.

For multiple Connect instances, model the source as `(server, guid, credential reference)`. Automatic `CONNECT_*` credentials cover only the hosting instance.

Sources: [List content items](https://docs.posit.co/connect/api/#get-/v1/content), [search content](https://docs.posit.co/connect/api/#get-/v1/search/content), [content response roles and `otel_enabled`](https://docs.posit.co/connect/api/#get-/v1/content/-guid-).

## Storage, retention, and version limits

With OpenTelemetry enabled, Connect persists signals as JSONL on disk by default and can fan them out to one or more OTLP/HTTP backends. The documented defaults are `OpenTelemetry.PersistenceEnabled=true`, an `otelsignals` directory beneath `/var/log/rstudio/rstudio-connect`, five backups, 50 MB per file, and 30 days (`PersistenceMaxDays=30`). This documented signal storage is not a durable audit archive. An administrator can tune it or configure `[OTLPEndpoint]` export for longer-lived analysis. Local trace files and exported GenAI message attributes must be treated as sensitive. Connect does not document direct access from a deployed app to the host signal directory, nor does it promise that the internal content-traces endpoint has exactly the same retention lifecycle; therefore do not present these defaults as an API retention SLA.

Legacy per-job trace availability is additionally coupled to job retention: completed jobs are reaped after either `Jobs.MaxCompleted` (default 1000 per application) or `Jobs.OldestCompleted` (default 30 days) is exceeded. Posit Chronicle is not a substitute trace store: its documented curated Connect datasets cover content and usage/session records, not trace spans.

Version guidance:

- **2026.02.0**: minimum for Connect's general OpenTelemetry implementation.
- **2026.07.0**: recommended minimum for `scans_app`, because this is the content-wide-store migration boundary used by official Commons.
- Keep the per-job fallback for older retained traces and servers where the content-wide endpoint returns 404.

Sources: [OpenTelemetry setup, disk persistence, and OTLP fan-out](https://docs.posit.co/connect/admin/opentelemetry/getting-started.html), [OpenTelemetry configuration defaults](https://docs.posit.co/connect/admin/appendix/configuration/#opentelemetry), [job retention defaults](https://docs.posit.co/connect/admin/appendix/configuration/#jobs), [Chronicle curated datasets](https://docs.posit.co/chronicle/curated-data/), [Connect 2026.02 release notes](https://docs.posit.co/connect/news/#2026-02-0), [Commons 2026.07 store compatibility logic](https://github.com/posit-dev/commons/blob/bb111eddf1052d24a8394957fb753155f44343e7/pkg-r/R/connect.R#L100-L106).

## Recommended `scans_app` Connect path

Keep the version-sensitive Connect transport in official Commons. Its
`trajectory_read()` already queries the selected GUID's content-wide endpoint,
pages and bounds the read, parses OTLP NDJSON, falls back to per-job storage,
and reconstructs completed conversations. `scans_app_connect()` should build a
lazy loader for each explicitly allow-listed GUID, call that upstream reader
only when the app is selected, and then normalize through
`as_trajectory_commons()`. This provides smooth multi-app review without
duplicating an undocumented Connect endpoint in scans. Accept GUIDs rather than
arbitrary content URLs so the injected API key cannot be redirected to another
server, and use a rolling seven-day window closed at each load time for
responsive reads. Require the Commons version that supplies the content-wide
store and legacy compatibility behavior.

Automatic discovery can be added later through the documented content listing
API, filtered to owner/collaborator targets with `otel_enabled`; the initial
named allow-list is safer because the review app uses its deployment owner's
authority. Keep file and pin imports as offline portability paths, not as the
primary deployed Connect transport.
