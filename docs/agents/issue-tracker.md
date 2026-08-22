# Issue tracker: GitHub

Issues and specs for this repo live as GitHub issues. Use the `gh` CLI for all
operations.

## Conventions

- **Create an issue**: `gh issue create --title "..." --body "..."`. Use a
  heredoc for multi-line bodies.
- **Read an issue**: `gh issue view <number> --comments`, filtering comments by
  `jq` and also fetching labels.
- **List issues**: `gh issue list --state open --json
  number,title,body,labels,comments --jq '[.[] | {number, title, body, labels:
  [.labels[].name], comments: [.comments[].body]}]'` with appropriate `--label`
  and `--state` filters.
- **Comment on an issue**: `gh issue comment <number> --body "..."`
- **Apply or remove labels**: `gh issue edit <number> --add-label "..."` or
  `--remove-label "..."`
- **Close**: `gh issue close <number> --comment "..."`

Infer the repo from `git remote -v`; `gh` does this automatically when run
inside a clone.

## Pull requests as a triage surface

**PRs as a request surface: no.** Set this to `yes` if the repo later treats
external pull requests as feature requests.

When enabled, pull requests use the same labels and states as issues. Read them
with `gh pr view <number> --comments` and `gh pr diff <number>`, list external
pull requests with `gh pr list`, and use `gh pr comment`, `gh pr edit`, and
`gh pr close` for mutations.

GitHub shares one number space across issues and pull requests. Resolve a bare
reference such as `#42` with `gh pr view 42` and fall back to
`gh issue view 42`.

## When a skill says "publish to the issue tracker"

Create a GitHub issue.

## When a skill says "fetch the relevant ticket"

Run `gh issue view <number> --comments`.

## Wayfinding operations

Used by the wayfinder skill. A map is one issue with child issues as tickets.

- **Map**: an issue labelled `wayfinder:map` that holds Notes,
  Decisions-so-far, and Fog.
- **Child ticket**: an issue linked to the map as a GitHub sub-issue. Where
  sub-issues are unavailable, add the child to a task list in the map body and
  put `Part of #<map>` at the top of the child body. Label it with one of
  `wayfinder:research`, `wayfinder:prototype`, `wayfinder:grilling`, or
  `wayfinder:task`.
- **Blocking**: use GitHub's native issue dependencies. Where dependencies are
  unavailable, use a `Blocked by: #<n>` line at the top of the child body.
- **Frontier query**: select the first open, unassigned child in map order that
  has no open blockers.
- **Claim**: `gh issue edit <n> --add-assignee @me`; this is the session's first
  write.
- **Resolve**: comment with the answer, close the child, then append a context
  pointer to the map's Decisions-so-far.
