# unsupported versions and edited evidence fail explicitly

    Code
      read_investigation(path)
    Condition
      Error in `read_investigation()`:
      ! This investigation format version is not supported.

---

    Code
      read_investigation(path)
    Condition
      Error in `investigation_validate()`:
      ! The investigation identifiers do not match its evidence or analysis.

# the reader rejects executable types and the writer rejects custom objects

    Code
      read_investigation(path)
    Condition
      Error in `investigation_decode()`:
      ! Unsupported typed data at data.

---

    Code
      investigation_snapshot(bundle)
    Condition
      Error in `investigation_check_class()`:
      ! Unsupported data class at data[[1]][[1]][[16]][[1]][[1]].

# size limits and overwrite defaults protect existing files

    Code
      write_investigation(saved, path)
    Condition
      Error in `write_investigation()`:
      ! The destination already exists. Use a new path or `overwrite = TRUE`.

---

    Code
      read_investigation(path, max_bytes = 10)
    Condition
      Error in `read_investigation()`:
      ! The investigation file exceeds `max_bytes`.

---

    Code
      read_investigation(path)
    Condition
      Error in `value[[3L]]()`:
      ! The investigation file is not valid JSON.

