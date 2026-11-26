# the report says what a link cannot restore

    Code
      investigation_report(saved, app_url = "javascript:alert(1)")
    Condition
      Error in `investigation_report()`:
      ! `app_url` must be one "http://" or "https://" base address.
      i Give the host and path only: no credentials, query, or fragment.

# review decisions use the review case's own adjudication

    Code
      investigation_report(saved, reviews = list(other))
    Condition
      Error in `investigation_report()`:
      ! A review case was made from a different snapshot than this investigation.

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

