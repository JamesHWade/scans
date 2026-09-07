# invalid selection and edited analysis cannot be written

    Code
      investigation_snapshot(bundle, "missing")
    Condition
      Error in `investigation_build()`:
      ! `trajectory_ids` must contain unique identifiers from the bundle.

---

    Code
      investigation_snapshot(bundle, c("otel/retry", "otel/retry"))
    Condition
      Error in `investigation_build()`:
      ! `trajectory_ids` must contain unique identifiers from the bundle.

---

    Code
      write_investigation(saved, tempfile())
    Condition
      Error in `investigation_validate()`:
      ! A saved finding refers to missing or unrelated events.

# orphan findings fail before hashing

    Code
      write_investigation(saved, tempfile())
    Condition
      Error in `investigation_validate()`:
      ! The saved assessment status disagrees with its finding references.

