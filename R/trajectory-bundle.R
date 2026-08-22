#' A canonical agent trajectory bundle
#'
#' `TrajectoryBundle()` creates a validated, source-neutral snapshot of one or
#' more completed agent trajectories. Its properties are ordinary tibbles for
#' trajectories, turns, events, evaluations, and adapter losses.
#'
#' Missing optional canonical columns are filled with typed missing values.
#' Empty `evaluations` and `losses` tables are created when those arguments are
#' `NULL`. Unknown extra columns are preserved.
#'
#' @param trajectories A data frame with one row per logical agent trajectory.
#'   Non-empty inputs require `trajectory_id` and `source_type`.
#' @param turns A data frame with one row per semantic turn. Non-empty inputs
#'   require `trajectory_id`, `turn_id`, `turn_index`, and `role`.
#' @param events A data frame with one row per content or execution event.
#'   Non-empty inputs require `trajectory_id`, `event_id`, `event_index`, and
#'   `event_type`.
#' @param evaluations An optional data frame with outcome judgments. Non-empty
#'   inputs require `trajectory_id` and `evaluation_id`.
#' @param losses An optional data frame recording source data that was
#'   unsupported, redacted, truncated, or externalized. Non-empty inputs
#'   require `field`, `reason`, and `detail`.
#' @prop schema_version The integer trajectory schema version. Version 1 is the
#'   only currently supported value.
#'
#' @returns A `TrajectoryBundle` S7 object.
#' @export
#'
#' @examples
#' bundle <- TrajectoryBundle(
#'   data.frame(
#'     trajectory_id = "trajectory-1",
#'     source_type = "manual"
#'   ),
#'   data.frame(),
#'   data.frame()
#' )
#'
#' trajectory_info(bundle)
TrajectoryBundle <- S7::new_class(
  "TrajectoryBundle",
  package = "scans",
  properties = list(
    trajectories = S7::class_data.frame,
    turns = S7::class_data.frame,
    events = S7::class_data.frame,
    evaluations = S7::class_data.frame,
    losses = S7::class_data.frame,
    schema_version = S7::new_property(
      S7::class_integer,
      default = 1L
    )
  ),
  constructor = function(
    trajectories,
    turns,
    events,
    evaluations = NULL,
    losses = NULL
  ) {
    call <- rlang::caller_env()
    trajectories <- normalize_trajectory_table(
      trajectories,
      "trajectories",
      call
    )
    turns <- normalize_trajectory_table(turns, "turns", call)
    events <- normalize_trajectory_table(events, "events", call)
    evaluations <- normalize_trajectory_table(
      evaluations,
      "evaluations",
      call
    )
    losses <- normalize_trajectory_table(losses, "losses", call)

    data <- list(
      trajectories = trajectories,
      turns = turns,
      events = events,
      evaluations = evaluations,
      losses = losses,
      schema_version = trajectory_schema_version
    )
    problems <- trajectory_bundle_data_validation_problems(data)
    if (length(problems) > 0L) {
      scans_abort(
        c(
          "Can't construct a {.cls TrajectoryBundle}.",
          stats::setNames(problems, rep("x", length(problems)))
        ),
        class = "scans_error_trajectory_validation",
        call = call
      )
    }

    S7::new_object(
      S7::S7_object(),
      trajectories = trajectories,
      turns = turns,
      events = events,
      evaluations = evaluations,
      losses = losses,
      schema_version = trajectory_schema_version
    )
  },
  validator = function(self) trajectory_bundle_validation_problems(self)
)

#' Convert an object to a trajectory bundle
#'
#' `as_trajectory()` is an S7 generic used by source adapters. Methods snapshot
#' completed upstream objects into a canonical [TrajectoryBundle]. The core
#' package provides an identity method for existing bundles; integration
#' packages provide methods for their own source classes.
#'
#' @param x An object containing completed agent trajectory data.
#' @param ... Arguments passed to a source-specific method.
#'
#' @returns A [TrajectoryBundle].
#' @export
as_trajectory <- S7::new_generic("as_trajectory", "x")

#' Test for or extract data from a trajectory bundle
#'
#' These functions provide the ordinary tibble analysis surface for a
#' [TrajectoryBundle]. Accessors return the stored canonical table without
#' modifying it.
#'
#' @param x An object to test or a [TrajectoryBundle] to access.
#'
#' @returns
#' - `is_trajectory_bundle()` returns one logical value.
#' - The accessors return a tibble.
#'
#' @name trajectory_accessors
NULL

#' @rdname trajectory_accessors
#' @export
is_trajectory_bundle <- function(x) {
  S7::S7_inherits(x, TrajectoryBundle)
}

#' @rdname trajectory_accessors
#' @export
trajectory_info <- function(x) {
  check_trajectory_bundle(x)
  S7::prop(x, "trajectories")
}

#' @rdname trajectory_accessors
#' @export
trajectory_turns <- function(x) {
  check_trajectory_bundle(x)
  S7::prop(x, "turns")
}

#' @rdname trajectory_accessors
#' @export
trajectory_events <- function(x) {
  check_trajectory_bundle(x)
  S7::prop(x, "events")
}

#' @rdname trajectory_accessors
#' @export
trajectory_evaluations <- function(x) {
  check_trajectory_bundle(x)
  S7::prop(x, "evaluations")
}

#' @rdname trajectory_accessors
#' @export
trajectory_losses <- function(x) {
  check_trajectory_bundle(x)
  S7::prop(x, "losses")
}

check_trajectory_bundle <- function(x, arg = rlang::caller_arg(x)) {
  if (!is_trajectory_bundle(x)) {
    scans_abort(
      c(
        "{.arg {arg}} must be a {.cls TrajectoryBundle}.",
        "x" = "It is {.obj_type_friendly {x}}."
      ),
      class = "scans_error_trajectory_type"
    )
  }
  invisible(x)
}
