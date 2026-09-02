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
   `read_connect_traces()` finds spans but has no messages to reconstruct,
   and the app reports the conversations as contentless.
3. **Conversation ids.** Reviewers group spans by `gen_ai.conversation.id`.
   commons agents set it; other ellmer apps should set
   `chat$conversation_id` (ellmer >= the release with that binding) or open
   a parent span carrying the attribute. Without it, each model call is its
   own trajectory.
4. **Packages reachable by Connect.** `scans` is GitHub-only; either Connect
   can reach GitHub or mirror it to your package manager. `commons` is needed
   only if you switch the app to `reader = "commons"`.
5. The deployment owner owns or collaborates on every source application.
   Connect normally injects an ephemeral API key for that owner at runtime.

## Deploy

- Posit Publisher (Positron): new deployment from `deploy/connect/app.R`.
- Scripted: `Rscript deploy/connect/deploy.R` after the one-time account
  setup at the top of that file.

## Configure (Connect dashboard, Vars tab)

| Var | Value |
|---|---|
| `CONNECT_SERVER` | injected by Connect; set only if `Applications.DefaultServerEnv` is disabled |
| `CONNECT_API_KEY` | injected ephemeral owner key; set manually only if `Applications.DefaultAPIKeyEnv` is disabled |
| `SCANS_CONNECT_SOURCES` | `Support assistant=<guid>;Research assistant=<guid>` |
| `SCANS_CONNECT_N` | optional, default `100`; `all` for no limit |
| `SCANS_ANNOTATIONS_DIR` | optional; a directory on persistent storage where reviewer labels and notes are appended. Without it the app shows no annotation controls. |

Annotations are shared by everyone who can open the app and survive
redeployment only when the directory lives outside the bundle (for example a
mounted volume). Run the app as one Connect process so all reviewers append
to the same store.

Do not store a long-lived API key in Vars when Connect's default injection is
enabled. Then restrict viewers (the app reads as its deployment owner, so
anyone who can open it sees every configured conversation), give it a vanity
URL, restart, and press **Reload traces**.

Keep environment-specific deployment records out of the repo: `deploy/local/`
is git-ignored for that purpose.
