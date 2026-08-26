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

# scans app rejects loaders that require arguments

    Code
      scans_app(list(`Broken deployment` = function(content_guid) NULL))
    Condition
      Error in `scans_app()`:
      ! Application source "Broken deployment" loader must be callable without arguments.
      x Required argument: content_guid.

---

    Code
      scans_app(list(`Primitive deployment` = `if`))
    Condition
      Error in `scans_app()`:
      ! Application source "Primitive deployment" loader must be callable without arguments.
      x Its argument list could not be inspected.
      i Wrap it in an ordinary zero-argument function.

