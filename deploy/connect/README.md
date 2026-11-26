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
| `SCANS_CONNECT_DAYS` | optional, default `7`; days of activity read on each load |
| `SCANS_CONNECT_READER` | optional, `otel` (default) or `commons` |
| `SCANS_CONNECT_JOBS` | optional, `true` (default) or `false`; `false` skips the retained per-job trace stores and loads faster |
| `SCANS_ANNOTATIONS_DIR` | optional; a directory on persistent storage where reviewer labels and notes are appended. Without it the app is read-only: no review state and no disposition controls. |
| `SCANS_ANNOTATION_LABELS` | optional; comma-separated labels reviewers can apply, for example `looks right,wrong answer,tool failure,follow up` (the default). Needs `SCANS_ANNOTATIONS_DIR`. |
| `SCANS_REVIEWS` | optional, `true` or `false` (default); session-local evidence review and example export |
| `SCANS_INVESTIGATIONS` | optional, `true` or `false` (default); save the selection as a file and reopen it |

A Var with a value the app does not understand stops it at startup with a
message naming the Var. `config.R` holds the parsing and must be deployed
beside `app.R`.

## Review workflow

1. **Overview.** The app opens on counts with denominators, activity by day,
   patterns, tools, models, and users. Select a bar, pattern, tool, user, or
   model to narrow everything to it; the chips above the list remove a filter.
2. **Queue.** With an annotation store the list is ordered by "Needs review":
   unreviewed first, then error findings, warnings, unassessed, and clean,
   newest first. The progress line counts flagged conversations reviewed.
3. **Dispositions.** Open a conversation and mark it with a label button or
   its key (`1`-`9`), or `E` for reviewed without a label. With auto-advance
   on, the next unreviewed conversation opens. `J`/`K` step through the list,
   `U` jumps to the next unreviewed, `?` lists the shortcuts.
4. **Exports.** The overview's Export menu gives the conversations and
   findings in view as CSV, a Markdown report for the application's
   developers (counts and links only; no transcript text, and user
   identifiers only if you tick the box), and an R snippet that repeats the
   read. Links reopen the same filters. No export contains the Connect API
   key or server address.

### What the annotation store keeps, and who sees it

The store is one append-only file, `annotations.jsonl`, in
`SCANS_ANNOTATIONS_DIR`. Each mark or note adds a line with the application
label, conversation id, label, note, author, and time. Nothing is edited or
deleted; a changed judgment is a new line. It holds no transcript text unless
a reviewer types it into a note.

Annotations are shared by everyone who can open the app and survive
redeployment only when the directory lives outside the bundle (for example a
mounted volume). Run the app as one Connect process so all reviewers append
to the same store. The author is the Connect username when Connect provides
one; it is a convenience for the team, not an authenticated identity.

Do not store a long-lived API key in Vars when Connect's default injection is
enabled. Then restrict viewers (the app reads as its deployment owner, so
anyone who can open it sees every configured conversation), give it a vanity
URL, restart, and press **Reload traces**.

Keep environment-specific deployment records out of the repo: `deploy/local/`
is git-ignored for that purpose.
