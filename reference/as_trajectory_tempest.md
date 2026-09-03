# Convert completed Tempest trajectory reviews

`as_trajectory_tempest()` consumes the authoritative closed projection
from
[`tempest::tempest_trajectory_review_data()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review_data.html)
and converts it into a canonical
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).
It accepts the already-created review and never reaches back into a
`TempestResult`, session, workspace, provider, or other live object.

## Usage

``` r
as_trajectory_tempest(
  x,
  trajectory_id = NULL,
  source_uri = NULL,
  metadata = list()
)
```

## Arguments

- x:

  A completed value returned by
  [`tempest::tempest_trajectory_review()`](https://jameshwade.github.io/tempest/reference/tempest_trajectory_review.html).

- trajectory_id:

  A bundle-unique trajectory ID. When `NULL`, a stable ID is derived
  from the Tempest research run ID.

- source_uri:

  An optional source locator. Credentials, query parameters, and
  fragments are removed.

- metadata:

  A uniquely named list of safe application metadata. Sensitive fields
  are redacted and recorded as losses.

## Value

A
[TrajectoryBundle](https://jameshwade.github.io/scans/reference/TrajectoryBundle.md).

## Details

One review becomes one product trajectory. Authoritative stage order is
preserved before the review's canonical-set lanes. Product stages,
Deputy agent runs, programs, knowledge, evidence identities, join
proofs, and findings remain distinguishable through namespaced events
and structured values. In particular, `authority_validated`,
`exact_identity`, and `correlation_only` proof kinds are retained
without reinterpretation.

The Tempest review deliberately excludes prompts, responses, source
content, local paths, credentials, and live runtime objects. Those
omissions are recorded as adapter losses rather than reconstructed from
private state.

## Examples

``` r
if (FALSE) { # \dontrun{
review <- tempest::tempest_trajectory_review(result)
bundle <- as_trajectory_tempest(review)

filter_trajectory_events(bundle, event_type = "tempest:join")
} # }
```
