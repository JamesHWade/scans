# Two domain scanners and the recipe

Issue #43 asks for two concrete domain problems detected through the custom
scanner interface (`custom-scanners.md`) and validated on reviewed cases
(`scanner-validation.md`), with a runnable recipe. This note records which
problems were chosen, where the code lives, and what the fixture cases are.

## The two problems

**Unsupported action claim** (support assistant). The assistant tells the user
an action was done, and the trajectory records no successful call, before the
claim, to a tool that performs it, with its successful result also before the
claim. Settings map each action to its tools and a
claim pattern.

**Answer without retrieved support** (retrieval application). The assistant
gives a substantive answer after a retrieval tool failed or returned nothing,
with no later successful retrieval before the answer; or, where
`commons:provenance` events are recorded, in an exchange tagged `C` (no
trusted or cited source). Settings name the retrieval tools, a minimum answer
length, and a pattern for answers that decline.

Both were chosen because the evidence is in the bundle already (assistant
text, tool calls and results, provenance records), the rule is deterministic,
and each has an obvious way to be unassessable: tool capture can be missing.

## Decisions

**They ship as examples, not built-in scans.** The code is in
`inst/examples/domain-scanners.R` and the article sources it. A built-in scan
has to mean the same thing in every application. These do not: tool names,
claim phrases, and what counts as a substantive answer belong to the
application owner, and the defaults fit only the fictional applications in the
fixture. Shipping them in `scan_registry()` would put two detectors with no
real validation next to scans that read recorded facts. As examples they are
tested, installed, and copyable, and nobody gets their findings by default.

**Each is a function that returns a scanner.**
`unsupported_action_claim_scanner(actions, version)` and
`answer_without_support_scanner(retrieval_tools, min_chars, declined,
version)` put the owner's configuration into `settings`, so every assessment
records what the detector was configured to look for.

**When "nothing recorded" is evidence.** A claim with no tool call anywhere
is a finding only if the capture has no loss on turns, messages, events, or
tool capture and the outcome of every earlier call to the action's tools is
known. Otherwise it is `insufficient_evidence`. This follows the built-in
rule that a fully recorded text conversation can show a tool was not used,
and a lossy one cannot. A trajectory with no claim is a clean negative
whatever the capture, since the rule has nothing to check.

**An outcome is known only when it is linked and affirmed.** A call is linked
to its result by an explicit `parent_event_id`, or else by call identity only
when that identity belongs to one call and one result in the trajectory, the
result follows the call, and a named result names the same tool. A reused
identity is left unlinked, as the package's own tool correlation leaves it.
A result is a success only with status `completed` and no error; `failed`,
`cancelled`, or an error is a failure; a missing, `interrupted`, `running`, or
`pending` status is unknown. An unlinked call or an unknown result before the
claim (or, for retrieval, between the empty result and the answer) is
`insufficient_evidence`, not a failure and not a success. A success that
arrives after the claim does not support it.

**An empty retrieval followed by an answer needs a capture that would keep a
retry.** With capture losses on turns, messages, events, or tool capture, a
later successful retrieval may be missing, so the scanner returns
`insufficient_evidence` instead of asserting that none occurred.

**Retrieval is found from calls, linked to results by call identity.** A
retrieval call is a `tool_call` naming a retrieval tool. Its result is the
`tool_result` with the same call identity, whether or not the result row
repeats the tool name; a result that names a retrieval tool counts even if its
call was not kept. A retrieval call with no linked result is
`insufficient_evidence`: the search happened and what it returned is unknown.

**Negated and interrogative claims are excluded by a simple rule.** A claim
pattern counts only in a sentence that is not a question and has no "not",
"never", "cannot", or "n't" before the matched phrase, so "I have not issued a
refund yet" and "Have you already cancelled the order?" are not claims. The
rule was added for two development cases written for those forms, and it
changed no test-split outcome. It is still phrase matching: a quoted claim
("you were told the refund was issued"), a conditional ("once your refund is
processed"), a negation after the phrase, and a paraphrase the pattern does
not list are all read wrongly, and sentence breaks are guessed from
punctuation.

**No retrieval and no provenance is `not_applicable`, not insufficient.** The
lead's brief said "otherwise insufficient". With capture losses it is
insufficient, and a `provenance` loss, which `as_trajectory_commons()`
records when a conversation's provenance records are missing, counts as one.
With a complete capture and no retrieval call, the detector's
trigger never occurred, and "the conversation records no retrieval" is a
statement about the conversation, not about missing evidence. Both are
unassessed and neither is a negative. The cost is a blind spot, stated in the
article: an assistant that never retrieves is not flagged by this scanner.

**Provenance is matched to answers by exchange, with the route analysis's
rule.** The route analysis is internal and has no public accessor, so the
example mirrors its rule (`route_turn_exchanges()` and
`route_provenance_records()` in `R/route-analysis.R`) rather than calling it.
A turn's exchange is its `round_index` when the trajectory records rounds, as
`as_trajectory_commons()` does; otherwise each user turn that holds no tool
result and is not marked `commons_internal` opens the next exchange, so a
commons reminder turn does not. A provenance record's exchange is its
`metadata$exchange_index` when that is one whole number of at least 1, and
otherwise the exchange of its turn. The answer a record describes is the last
substantive assistant text in that exchange. A record with no valid exchange
is unclassified and not used; when no record can be placed and no retrieval
result is retained, the trajectory is `insufficient_evidence`.

The rule has limits. Without `round_index` it trusts that every other user
turn is a new question, which an application that injects its own user turns
without marking them breaks. It reads one answer per exchange, the last, so an
untrusted exchange with two long answers yields one finding. And a copy of a
rule drifts: if the route analysis changes its exchange rule, this example
does not follow.

## The fixture cases

`inst/examples/domain-scanner-cases.R` builds two hand-written bundles (twelve
support conversations, thirteen retrieval conversations) and reviews them with
`review_case()`, `review_decision()`, and `review_examples()`. Each set has a
development and a test split and contains confirmed findings, rejected
findings, correctly unflagged conversations, findings the detector misses, a
reviewer-uncertain case, and a conversation whose tool results were not
captured.

The cases are built in the canonical schema with `TrajectoryBundle()`, not
through a producer package, so the recipe needs no optional dependency and no
provider. The provenance records copy the shape `as_trajectory_commons()`
emits.

**These are not independently reviewed cases.** The issue's first acceptance
item asks for "independently reviewed examples". The conversations and the
decisions were written by the author of the detectors, to exercise every
outcome. They demonstrate the method. They are not evidence that either
detector is valid for any application, and the article says that in its
introduction and again where it discusses the numbers. Independent review
needs real conversations from a real application and a reviewer who did not
write the scanner; that remains open on #43.

## The recipe

`vignettes/custom-scanners.Rmd` runs end to end offline: define, run, read
assessments, review, validate on development cases, change the scanner and
its version, check once on test cases, then save and reopen. It reports the
held-out result as it is and does not tune against it. The test split was
written to contain two kinds of error the development split lacks, and the
article says so rather than presenting the gap as a finding.
