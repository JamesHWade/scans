# Select versioned examples from reviewed evidence

Selection is explicit: pass only cases whose retained content you intend
to include. The resulting value contains their evidence and complete
decision histories, input/target mappings, and a content policy. Nothing
is selected automatically from an annotation store or investigation.

## Usage

``` r
review_examples(
  cases,
  purpose,
  groups,
  splits,
  inputs = NULL,
  targets = NULL,
  predicates = NULL,
  previous = NULL
)
```

## Arguments

- cases:

  Non-empty list of values from
  [`review_case()`](https://jameshwade.github.io/scans/reference/review_case.md).

- purpose:

  Either `"application"` or `"scanner"`.

- groups:

  One stable task/group identifier per case.

- splits:

  One of `"development"` or `"test"` per case.

- inputs, targets:

  For application examples, one named list of explicitly selected fields
  per case. Input and target names must be disjoint and consistent
  across cases. Values must be serializable data.

- predicates:

  For scanner examples, one list with `operator` (`"eq"`, `"gte"`, or
  `"lte"`) and non-negative integer `value` per case.

- previous:

  Optional previous example set. Its version becomes the parent; prior
  group and source-trajectory split assignments are retained.

## Value

A `scans_review_examples` value. Its `version_id` covers selected
content, mappings, history, and split assignments. Inspect the value
before writing it with
[`write_review_examples()`](https://jameshwade.github.io/scans/reference/review_example_files.md).

## Details

Each case belongs to one development or test group. Repeated tasks and
related executions must use the same group, including across new
versions of the set. The constructor checks group isolation and repeated
source trajectory identities; callers must identify other related tasks
themselves. A new version supplied through `previous` retains prior
group assignments.

Application examples require explicitly supplied named input and target
lists, plus an active decision with expected behavior. Scanner examples
use a count predicate such as `list(operator = "gte", value = 1L)`
against the case's saved scanner name. Counts cover the selected
trajectory, restricted to findings intersecting the case's event
references when supplied.

Unreviewed, disputed, uncertain, and insufficient-capture cases may be
saved for further review.
[`review_dataset()`](https://jameshwade.github.io/scans/reference/review_dataset.md)
refuses to turn them into targets.
