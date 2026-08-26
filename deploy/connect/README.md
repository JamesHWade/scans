# scans on Posit Connect

Deploys `scans::scans_app_connect()` to review conversations from deployed
ellmer applications, read from Connect's content observability store.

## Prerequisites

1. **Content observability** enabled on the Connect server and on every
   application you want to review, and the applications restarted afterward.
2. **Message content captured.** ellmer only records prompts and responses in
   its spans when the producing app runs with
   `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true` (set it as a
   Connect Var, or let `commons(log = TRUE)` set it). Without it,
   `commons::trajectory_read()` finds spans but drops every conversation as
   contentless.
3. **Conversation ids.** Reviewers group spans by `gen_ai.conversation.id`.
   commons agents set it; other ellmer apps should set
   `chat$conversation_id` (ellmer >= the release with that binding) or open
   a parent span carrying the attribute. Without it, each model call is its
   own trajectory.
4. **Packages reachable by Connect.** `scans` and `commons` are GitHub-only;
   either Connect can reach GitHub or mirror them to your package manager.
5. An API key for a user with **editor** access to every source application.

## Deploy

- Posit Publisher (Positron): new deployment from `deploy/connect/app.R`.
- Scripted: `Rscript deploy/connect/deploy.R` after the one-time account
  setup at the top of that file.

## Configure (Connect dashboard, Vars tab)

| Var | Value |
|---|---|
| `CONNECT_SERVER` | `https://<connect-host>` |
| `CONNECT_API_KEY` | key with editor access to every source |
| `SCANS_CONNECT_SOURCES` | `Support assistant=<guid>;Research assistant=<guid>` |
| `SCANS_CONNECT_N` | optional, default `100`; `all` for no limit |

Then restrict viewers (the app reads with its own key, so anyone who can open
it sees every configured conversation), give it a vanity URL, restart, and
press **Reload traces**.

Keep environment-specific deployment records out of the repo: `deploy/local/`
is git-ignored for that purpose.
