# saved investigations keep custom results without the scanner function

    Code
      investigation_snapshot(reopened$bundle, scans = reopened$settings$scans)
    Condition
      Error:
      ! Unknown scan in `scans`: "toy_keyword".
      i See `scan_registry()` for the built-in scans.
      i A custom scanner runs only when it is supplied through `scanners`.

