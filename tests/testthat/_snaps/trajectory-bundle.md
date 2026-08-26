# TrajectoryBundle() reports malformed source tables

    Code
      TrajectoryBundle(tibble::tibble(source_type = "manual"), tibble::tibble(),
      tibble::tibble())
    Condition
      Error:
      ! `trajectories` is missing required columns.
      x Missing: trajectory_id.

---

    Code
      TrajectoryBundle(tibble::tibble(trajectory_id = "trajectory-1", source_type = "manual"),
      tibble::tibble(trajectory_id = "trajectory-1", turn_id = "turn-1", turn_index = 1.5,
        role = "user"), tibble::tibble())
    Condition
      Error:
      ! `turns` has an invalid column type.
      x turn_index must be an integer vector, not a number.

# TrajectoryBundle() validates identities and references

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @events$trajectory_id must reference @trajectories
      x @events$turn_id must reference a turn in the same trajectory

---

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @events$event_id must be unique
      x @events$event_index must be unique within trajectory_id
      x @events$content_index must be unique within turn_id

# TrajectoryBundle() validates parent cycles

    Code
      TrajectoryBundle(tables$trajectories, tibble::tibble(), tibble::tibble())
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @trajectories$parent_trajectory_id must not form a cycle

# TrajectoryBundle() validates order and temporal bounds

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @turns$turn_index must contain positive integers
      x @events$content_index requires turn_id
      x @turns$duration must be missing or nonnegative
      x @trajectories$completed_at must not precede started_at

# TrajectoryBundle() validates loss references

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events, losses = losses)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @losses$turn_id must reference @turns

# TrajectoryBundle() validates vocabularies and metadata

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @events$event_type must use a core or namespaced value

---

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @events$metadata elements must be uniquely named lists

---

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @events$metadata elements must be uniquely named lists

# S7 property replacement preserves relational validity

    Code
      S7::prop(bundle, "events") <- events
    Condition
      Error:
      ! <scans::TrajectoryBundle> object is invalid:
      - @events$trajectory_id must reference @trajectories
      - @events$turn_id must reference a turn in the same trajectory

# S7 property replacement preserves schema version

    Code
      S7::prop(bundle, "schema_version") <- 2L
    Condition
      Error:
      ! <scans::TrajectoryBundle> object is invalid:
      - @schema_version must be 1L

# as_trajectory() has identity and unsupported-source methods

    Code
      as_trajectory(1)
    Condition
      Error in `method(as_trajectory, class_any)`:
      ! No trajectory adapter is available for a number.
      i Supply a <TrajectoryBundle> or install an adapter for the source class.

# trajectory accessors reject other objects

    Code
      trajectory_events(list())
    Condition
      Error in `trajectory_events()`:
      ! `x` must be a <TrajectoryBundle>.
      x It is an empty list.

# TrajectoryBundle rejects live and unbounded payloads

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @events$value[[1]] must contain serializable data only

---

    Code
      TrajectoryBundle(tables$trajectories, tables$turns, tables$events)
    Condition
      Error:
      ! Can't construct a <TrajectoryBundle>.
      x @events$text values must not exceed 65,536 bytes

# TrajectoryBundle print is compact

    Code
      minimal_trajectory_bundle()
    Message
      <TrajectoryBundle> schema 1
      trajectories: 1
      turns: 1
      events: 1
      evaluations: 0
      losses: 0
      sources: manual

