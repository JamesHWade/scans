# scans 0.0.0.9000

* Initial package scaffold.
* `as_trajectory_ellmer()` converts completed ellmer chats and turns into canonical trajectory bundles without provider calls (#6).
* `filter_trajectory_events()` selects events by trajectory, role, type, tool, status, and metadata (#7).
* `scan_trajectories()` emits deterministic findings for unresolved, unmatched, repeated, looping, and failed agent activity (#7).
* `summarize_trajectories()` reports activity, failures, usage, and execution depth for each trajectory (#7).
* `TrajectoryBundle()` provides the validated S7 trajectory contract, source-adapter generic, and tibble accessors (#4).
