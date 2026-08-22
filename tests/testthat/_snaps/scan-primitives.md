# scan primitive arguments fail with stable conditions

    Code
      filter_trajectory_events(bundle, role = NA_character_)
    Condition
      Error:
      ! `role` must be a non-empty character vector or `NULL`.
      x It is a character `NA`.

---

    Code
      filter_trajectory_events(bundle, metadata = list("user"))
    Condition
      Error:
      ! `metadata` must be a non-empty, uniquely named list or `NULL`.

---

    Code
      filter_trajectory_events(bundle, metadata = list(value = globalenv()))
    Condition
      Error:
      ! `metadata` must contain only bounded, serializable values.

---

    Code
      scan_trajectories(bundle, scan_id = "")
    Condition
      Error:
      ! `scan_id` must be one non-empty string.

---

    Code
      scan_trajectories(bundle, repeat_threshold = 1L)
    Condition
      Error:
      ! `repeat_threshold` must be one integer greater than or equal to 2.

---

    Code
      summarize_trajectories(data.frame())
    Condition
      Error in `check_trajectory_bundle()`:
      ! `x` must be a <TrajectoryBundle>.
      x It is a data frame.

