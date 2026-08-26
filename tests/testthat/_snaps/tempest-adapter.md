# Tempest adapter accepts only authoritative Tempest reviews

    Code
      as_trajectory_tempest(lookalike)
    Condition
      Error:
      ! Can't read the Tempest trajectory review.
      x `x` must be returned by `tempest_trajectory_review()`. x It is a list.
      Caused by error in `tempest_trajectory_review_abort()`:
      ! `x` must be returned by `tempest_trajectory_review()`.
      x It is a list.

# Tempest adapter requires the authoritative accessor

    Code
      as_trajectory_tempest(review)
    Condition
      Error:
      ! A tempest development version that exports `tempest_trajectory_review_data()` is required.
      i Install it from <https://github.com/JamesHWade/tempest>.

# Tempest adapter enforces scans-owned trajectory ID bounds

    Code
      as_trajectory_tempest(tempest_review_fixture(), trajectory_id = explicit_id)
    Condition
      Error:
      ! `trajectory_id` must not exceed 65515 bytes for Tempest events.

---

    Code
      as_trajectory_tempest(tempest_review_fixture(), trajectory_id = "   ")
    Condition
      Error:
      ! `trajectory_id` must contain non-whitespace characters.

