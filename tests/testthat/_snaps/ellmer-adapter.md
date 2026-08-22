# ellmer adapter rejects malformed sources and arguments

    Code
      as_trajectory(list(ellmer::UserTurn(), 1))
    Condition
      Error in `method(as_trajectory, class_any)`:
      ! The ellmer source contains invalid turns.
      x Elements 2 do not inherit from <ellmer::Turn>.

---

    Code
      as_trajectory_ellmer(ellmer::UserTurn(), epoch = 0L)
    Condition
      Error:
      ! `epoch` must be one positive integer or `NULL`.

---

    Code
      as_trajectory_ellmer(ellmer::UserTurn(), metadata = list("unnamed"))
    Condition
      Error:
      ! `metadata` must be a named list.

