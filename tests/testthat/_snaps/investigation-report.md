# the report says what a link cannot restore

    Code
      investigation_report(saved, app_url = "javascript:alert(1)")
    Condition
      Error in `investigation_report()`:
      ! `app_url` must be one "http://" or "https://" base address.
      i Give the host and path only: no credentials, query, or fragment.

# free-form source details and error text print only on request

    Code
      investigation_report(saved, include_errors = NA)
    Condition
      Error in `investigation_report()`:
      ! `include_errors` must be TRUE or FALSE.

# review decisions use the review case's own adjudication

    Code
      investigation_report(saved, reviews = list(other))
    Condition
      Error in `investigation_report()`:
      ! A review case was made from a different snapshot than this investigation.

# a report prints a summary and names an undated cohort's times

    Code
      print(report)
    Message
      <scans_investigation_report> Cohort
      5 of 5 conversations in scope · 3 of 4 assessed with findings · 1 not assessed
      4 patterns · 7 findings with evidence identities
      Render with `write_investigation_report()` or `format()`.

# reports are written as standalone files chosen by extension

    Code
      write_investigation_report(report, html)
    Condition
      Error in `write_investigation_report()`:
      ! The destination already exists. Use a new path or `overwrite = TRUE`.
    Code
      write_investigation_report(report, file.path(directory, "report.pdf"))
    Condition
      Error in `write_investigation_report()`:
      ! Use a '.html' or '.md' path, or supply `format`.
    Code
      write_investigation_report(report_fixture(), markdown)
    Condition
      Error in `write_investigation_report()`:
      ! `x` must be a report from `investigation_report()`.

# a report verifies identifiers unless told the value is fresh

    Code
      investigation_report(saved, verify = NA)
    Condition
      Error in `investigation_report()`:
      ! `verify` must be TRUE or FALSE.

