.onLoad <- function(...) {
  S7::methods_register()
}

S7::method(as_trajectory, TrajectoryBundle) <- function(x, ...) {
  x
}

S7::method(as_trajectory, S7::class_any) <- function(x, ...) {
  if (vitals_is_task(x)) {
    return(as_trajectory_vitals(x, ...))
  }

  if (
    rlang::is_installed("ellmer", version = "0.4.2") &&
      (ellmer_is_turn_list(x) || ellmer_is_chat(x) || ellmer_is_turn(x))
  ) {
    return(as_trajectory_ellmer(x, ...))
  }

  scans_abort(
    c(
      "No trajectory adapter is available for {.obj_type_friendly {x}}.",
      "i" = "Supply a {.cls TrajectoryBundle} or install an adapter for the source class."
    ),
    class = "scans_error_unsupported_source"
  )
}

S7::method(print, TrajectoryBundle) <- function(x, ...) {
  info <- S7::prop(x, "trajectories")
  source_types <- sort(unique(info$source_type))
  source_types <- source_types[!is.na(source_types)]
  source_label <- if (length(source_types) == 0L) {
    "<none>"
  } else {
    paste(source_types, collapse = ", ")
  }

  cli::cli_text(
    "{.cls TrajectoryBundle} schema {S7::prop(x, 'schema_version')}"
  )
  cli::cli_text("  trajectories: {nrow(info)}")
  cli::cli_text("  turns:        {nrow(S7::prop(x, 'turns'))}")
  cli::cli_text("  events:       {nrow(S7::prop(x, 'events'))}")
  cli::cli_text("  evaluations:  {nrow(S7::prop(x, 'evaluations'))}")
  cli::cli_text("  losses:       {nrow(S7::prop(x, 'losses'))}")
  cli::cli_text("  sources:      {source_label}")
  invisible(x)
}
