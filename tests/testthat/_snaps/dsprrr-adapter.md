# dsprrr source errors identify malformed exported snapshots

    Code
      as_trajectory_dsprrr(list())
    Condition
      Error:
      ! `x` must be a dsprrr module or exported trace data frame.
      x It is an empty list.

---

    Code
      as_trajectory_dsprrr(tibble::tibble(timestamp = Sys.time()))
    Condition
      Error:
      ! The exported dsprrr trace is missing required columns.
      x Missing: latency_ms, input_tokens, cached_input_tokens, output_tokens, total_tokens, cost, model, prompt_length, prompt, response, program_artifact_id, and trace_context.

---

    Code
      as_trajectory_dsprrr(traces)
    Condition
      Error:
      ! The exported dsprrr trace_context column must be a list.

---

    Code
      as_trajectory_dsprrr(dsprrr_trace_fixture(), metadata = list("unnamed"))
    Condition
      Error:
      ! `metadata` must be a uniquely named list.

