# scans 0.0.0.9000

* Initial package scaffold.
* `as_trajectory()` no longer treats an empty list as an ellmer source only because ellmer is installed (#14).
* `as_trajectory_ellmer()` converts completed ellmer chats and turns into canonical trajectory bundles without provider calls (#6).
* `as_trajectory_ellmer()` now redacts sensitive named-vector elements and attributes, preserves sanitized properties from unknown content classes, and avoids ambiguous tool-result parent links (#14).
* `filter_trajectory_events()` selects events by trajectory, role, type, tool, status, and metadata (#7). It preserves source columns and exposes containing-turn context in reserved dot-prefixed columns (#14).
* `scan_trajectories()` emits deterministic findings for ambiguous, unresolved, unmatched, repeated, looping, and failed agent activity, and compares named tool arguments independently of key order (#7, #14).
* `summarize_trajectories()` reports activity, failures, usage, and execution depth for each trajectory with grouped linear-time aggregation and an ambiguous-correlation count (#7, #14).
* `TrajectoryBundle()` provides the validated S7 trajectory contract, source-adapter generic, and tibble accessors (#4). Metadata entries must have unique names (#14).
