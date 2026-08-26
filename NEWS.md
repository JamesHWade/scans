# scans 0.0.0.9000

* Initial package scaffold.
* Tool results now resolve only calls that precede them in canonical event order, and causal mismatch findings distinguish missing preceding calls from missing subsequent results. Deputy lifecycle boundaries retain their causal position, and source-wide dsprrr and Commons provenance events no longer claim an unsupported turn association.
* Optional R6 adapters now accept valid upstream subclasses and method overrides through their public class contracts.
* `as_trajectory()` no longer treats an empty list as an ellmer source only because ellmer is installed (#14).
* `as_trajectory_commons()` snapshots already-read commons conversations, preserving source identity, activity time, trust tags, and citation decisions without performing filesystem or Connect I/O (#20).
* `as_trajectory_deputy()` snapshots completed Deputy results, lifecycle events, run context, usage, and delegation correlation into canonical trajectories (#18).
* `as_trajectory_dsprrr()` snapshots dsprrr modules and exported traces, preserving semantic turns, program artifact identity, metrics, and caller-supplied run correlation without treating artifact integrity as authenticity (#19).
* `as_trajectory_ellmer()` converts completed ellmer chats and turns into canonical trajectory bundles without provider calls (#6).
* `as_trajectory_ellmer()` now redacts sensitive named-vector elements and attributes, preserves sanitized properties from unknown content classes, and avoids ambiguous tool-result parent links (#14).
* `as_trajectory_tempest()` converts Tempest's authoritative closed review projection into product, stage, agent-run, program, knowledge, evidence, join, and finding events while preserving authority distinctions and explicit source omissions. Tempest owns review validity; scans validates only its generated identities and projected-record bounds (#21).
* `as_trajectory_vitals()` snapshots evaluated vitals tasks and sample data, including the development `vitals_log_read()` output, into solver trajectories joined to canonical evaluations (#8).
* `filter_trajectory_events()` selects events by trajectory, role, type, tool, status, and metadata (#7). It preserves source columns and exposes containing-turn context in reserved dot-prefixed columns (#14).
* `scan_trajectories()` emits deterministic findings for ambiguous, unresolved, unmatched, repeated, looping, and failed agent activity, and compares named tool arguments independently of key order (#7, #14).
* `scans_app()` launches a read-only Shiny app for filtering trajectories, browsing canonical turns and events, and inspecting linked findings, evaluations, losses, and source context. Named bundle or loader lists add a lazy, reloadable application switcher suited to reviewing multiple deployments on Posit Connect. Loaders must be callable without arguments, evaluation cards show their canonical IDs, and source context includes canonical task identities plus bounded metadata (#2).
* `summarize_trajectories()` reports activity, failures, usage, and execution depth for each trajectory with grouped linear-time aggregation and an ambiguous-correlation count (#7, #14).
* `TrajectoryBundle()` provides the validated S7 trajectory contract, source-adapter generic, and tibble accessors (#4). Metadata entries must have unique names (#14).
