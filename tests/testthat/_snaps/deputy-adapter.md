# Deputy source errors identify malformed public snapshots

    Code
      as_trajectory_deputy(invalid_turns)
    Condition
      Error:
      ! The Deputy result contains invalid turns.
      x Elements 1 do not inherit from <ellmer::Turn>.

---

    Code
      as_trajectory_deputy(invalid_events)
    Condition
      Error:
      ! Deputy event 1 is not an <AgentEvent>.

