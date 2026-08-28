# annotation labels must belong to the store vocabulary

    Code
      store$append("Support", "trajectory-1", label = "typo", note = NULL)
    Condition
      Error in `store$append()`:
      ! Label "typo" is not configured for this annotation store.
      i Choose one of "keep" and "revise" or leave the label blank.

---

    Code
      store$append("Support", "trajectory-1", label = "keep", note = NULL, author = c(
        "alice", "bob"))
    Condition
      Error in `store$append()`:
      ! `author` must be one non-empty string or `NULL`.

