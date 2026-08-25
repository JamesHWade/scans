# scans app requires an unambiguous application catalog

    Code
      scans_app(list(bundle))
    Condition
      Error in `scans_app()`:
      ! `x` must have unique, non-empty application names.

---

    Code
      scans_app(duplicate_names)
    Condition
      Error in `scans_app()`:
      ! `x` must have unique, non-empty application names.

---

    Code
      scans_app(list(App = 1))
    Condition
      Error in `scans_app()`:
      ! Application source "App" must be a <TrajectoryBundle> or a function.
      x It is a number.

