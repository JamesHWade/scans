# commons inputs use an explicit classless boundary

    Code
      as_trajectory_commons(invalid)
    Condition
      Error:
      ! The commons trajectory list must have unique, non-empty conversation names.

---

    Code
      as_trajectory_commons(source, metadata = list("unnamed"))
    Condition
      Error:
      ! `metadata` must be a uniquely named list.

