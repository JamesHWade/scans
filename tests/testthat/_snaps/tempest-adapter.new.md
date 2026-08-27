# Tempest adapter enforces scans-owned trajectory ID bounds

    Code
      as_trajectory_tempest(tempest_review_fixture(), trajectory_id = explicit_id)
    Condition
      Error:
      ! A tempest development version that exports `tempest_trajectory_review_data()` is required.
      i Install it from <https://github.com/JamesHWade/tempest>.

---

    Code
      as_trajectory_tempest(tempest_review_fixture(), trajectory_id = "   ")
    Condition
      Error:
      ! A tempest development version that exports `tempest_trajectory_review_data()` is required.
      i Install it from <https://github.com/JamesHWade/tempest>.

