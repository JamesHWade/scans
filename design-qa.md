# Investigation workspace design QA

final result: passed

The approved Overview and trajectory/Ask compositions are implemented in
the R package. No actionable P0, P1, or P2 visual findings remain. The
local preview is `http://127.0.0.1:7452/`; its clearly labeled
deterministic chat uses the real scans tools and does not validate a
live model’s answer quality.

## Source and capture

Source visual truth:

- `artifacts/ux-review/reference-overview.png`
- `artifacts/ux-review/reference-trajectory.png`

Both originals are 1487 × 1058 pixels. They were normalized to the
implementation’s 1488 × 1056 CSS-pixel viewport, at one image pixel per
CSS pixel, and saved as `artifacts/ux-review/reference-overview.png` and
`reference-trajectory.png`. This two-pixel normalization is the only
source-image transformation.

Final implementation captures:

- `artifacts/ux-review/overview-desktop-final.png`: Overview, four
  trajectories, most findings first, no trajectory selected, right pane
  closed.
- `artifacts/ux-review/trajectory-ask-desktop-final.png`: retry
  trajectory, both sidebars open, Ask selected, completed fixture
  response, first tool event expanded and highlighted, other calls
  closed.

Full-view same-input comparisons are `overview-comparison.png` and
`trajectory-comparison.png` in that directory. Each places the
normalized source on the left and the rendered implementation on the
right. The focused `trajectory-focus-comparison.png` compares the
transcript and Ask region using identical 1140 × 760 crops at x=348,
y=65. Comparison images were opened and judged together, including a
second pass after fixes.

Responsive evidence includes `trajectory-tablet.png` at 900 × 900 and
`overview-mobile.png`, `trajectory-mobile.png`, `evidence-mobile.png`,
and `ask-mobile.png` at 390 × 844. The mobile layout is an adaptation of
the approved desktop workflow; no separate mobile mock was supplied.

## Findings and comparison history

1.  **P2, overview evidence was too generic.** The first comparison
    (`overview-desktop.png`) showed only finding counts where the mock
    used concrete observations. Added deterministic pattern descriptions
    and a factual reason to inspect the suggested run. The final
    overview shows three error events, the same request made three
    times, and three consecutive identical requests, with explicit
    overlap. Increased secondary heading size and adjusted column
    proportions to restore the target’s hierarchy.
2.  **P2, the Ask composer did not fill the pane.** The native
    navigation wrapper interrupted the flex layout. Corrected its sizing
    and removed unintended gaps above the workspace.
    `trajectory-ask-desktop-final.png` and `ask-mobile.png` show the
    composer at the bottom of the usable pane.
3.  **P2, narrow-screen controls and citation navigation.** Mobile
    Options was compressed and the open drawer obscured cited evidence.
    Preserved a 40-pixel Options target, hid the loaded-time summary at
    narrow widths, and made citations dismiss drawers before focusing
    the event. Returning to a finding reopens its drawer. Mobile
    selection leaves the transcript visible.
4.  **P2, toggle overlap and unnamed sorting.** Moved the desktop
    right-pane toggle away from the Findings label. Added explicit
    accessible names to the native toolbar selects, which otherwise used
    duplicate wrapper/control IDs in the installed bslib. Browser
    inspection now identifies Order, Source, and Status.
5.  An intermediate comparison had retained the closed left drawer after
    mobile testing. It was rejected as a state mismatch. Final captures
    use a fresh desktop session with the same pane configuration and
    open event as the mock.

## Fidelity surfaces

- **Typography:** uses the actual shinychat system sans-serif stack
  (`ui-sans-serif`, `system-ui`, Apple system, Segoe UI). Body is 16px,
  Overview heading 35.2px, and secondary headings 25.6px. The mock’s
  exact font is not specified; its hierarchy and readable weight are
  retained. Transcript content remains 16px with 1.6 line height. Exact
  canonical IDs use the existing small monospace detail style after
  expansion.
- **Spacing and layout:** 350px trajectory browser, 460px collapsible
  right pane, and flexible transcript center. Overview uses the freed
  center width. Quiet row separators replace nested overview cards;
  detailed coverage and resources remain disclosures. Narrow screens use
  drawers, with no horizontal page overflow in the tested states. The
  native composer is more compact than the mock but expands as the user
  types.
- **Colors and tokens:** white surface, dark neutral text, muted
  secondary text, blue primary actions and evidence focus, pale blue
  selection, and restrained warning/error badges derive from
  `page_chat_theme()`. Run completion remains neutral and does not imply
  a successful answer.
- **Images and assets:** these screens contain no illustration or image
  assets. The text wordmark is retained and icons use the existing Shiny
  icon library; no generated logo, drawn SVG substitute, or rasterized
  UI was added.
- **Copy and content:** wording derives from the retained recording. The
  mock’s placeholder application selector is omitted for a single
  source; multiple sources retain the selector. Loaded-time text
  describes the actual app snapshot. Generic event-error wording covers
  non-tool errors in other inputs. Actual sort tie-breaking, full user
  questions, canonical event links, turn indices, nested recorded
  results, and explicit fixture/provider labels are intentional product
  differences. The fixture answer and native tool-call disclosures
  replace the mock’s illustrative prose.

## Interaction verification

- Overview pattern filtering narrows both the browser and summary from
  four trajectories to one; Clear restores the population. Empty search
  shows the explicit empty state and recovers when cleared. Ordering and
  filter popovers remain native controls.
- Inspect trajectory opens the readable transcript and Findings on
  desktop; desktop navigation keeps filters and selection. On mobile,
  selecting a run closes the trajectory drawer without covering the
  transcript.
- A native greeting suggestion fills the chat composer. Submitting
  streams real scans tool activity and a deterministic cited response.
  Scope defaults to the current trajectory or to the filtered population
  from Overview.
- Chat links open the exact canonical event, expand its disclosure,
  highlight, focus, and preserve a deep URL. A direct link to result
  event 000003 also expands its parent call. Mobile finding links close
  the drawer; Return to finding reopens it and restores focus.
- Deterministic native-server tests cover cancellation, navigation
  during a response, immutable question scope, history reset after scope
  change, old citation snapshot restoration, and independent session
  clients.
- No Shiny output errors or horizontal page overflow were present in
  final inspected states. Browser console logs were not collected;
  visible rendering, native interaction, JavaScript syntax, and server
  tests were checked.

## Implementation checklist

Approved Overview and trajectory structures

Native optional Ask and reusable package tools

Canonical evidence navigation and keyboard focus

Desktop and narrow-screen verification

Same-input full-view and focused-region comparison after fixes

Air, Jarl, documentation index and pkgdown build

Installed-package tests and R CMD check

The installed suite passed 1,998 assertions, with no failures or
warnings. One existing development-vitals adapter test was skipped
because the installed vitals is 0.3.0 rather than 0.3.0.9001. R CMD
check finished with 0 errors, 0 warnings, and 0 notes using the
temporary matching Tempest library. Local logs are retained outside the
published change; the PR’s CI checks provide the published validation
record. The dependency pins added when preparing the PR are checked
separately with those exact revisions.

## Follow-up polish

P3 only: the native toolbar and chat disclosures differ slightly from
the drawn controls, and the system font’s optical weight differs from
the raster mock. These retain the requested shinychat styling and
functioning package components. Live provider answer quality and
deployment were outside this local verification.
