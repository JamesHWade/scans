# vitals source errors identify unsupported sample shapes

    Code
      as_trajectory_vitals(list())
    Condition
      Error:
      ! `x` must be a vitals <Task> or sample data frame.
      x It is an empty list.

---

    Code
      as_trajectory_vitals(tibble::tibble(id = "sample-1"))
    Condition
      Error:
      ! The vitals sample data is incomplete.
      x Missing required column: solver_chat.
      i Pass a solved Task or the output of $get_samples() or vitals_log_read().

---

    Code
      as_trajectory_vitals(duplicate)
    Condition
      Error:
      ! Each vitals sample and epoch must identify one trajectory.
      x Rows 2 duplicate an earlier id/epoch pair.

---

    Code
      as_trajectory_vitals(invalid_chat)
    Condition
      Error:
      ! Every solver_chat must be a completed ellmer <Chat>.
      x Rows 1 contain unsupported values.

