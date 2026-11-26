# Facets narrow the cohort from the overview: a time bucket, a tool, a user,
# or a model. They are session state and are not written to saved
# investigations, whose trajectory selection already reflects them.
scans_app_facet_names <- c("day", "tool", "tool_failed", "user", "model")

# Clicking the active value again clears it. A failed-tool request names the
# tool as its value; without one it means any failed tool call.
scans_app_facet_toggle <- function(facets, facet, value) {
  if (identical(facet, "tool_failed")) {
    tool <- if (scans_app_has_string(value)) value
    active <- isTRUE(facets$tool_failed) && identical(facets[["tool"]], tool)
    facets$tool <- if (active) NULL else tool
    facets$tool_failed <- if (active) NULL else TRUE
    return(scans_app_facet_list(facets))
  }
  if (!scans_app_has_string(value)) {
    return(facets)
  }
  active <- identical(facets[[facet]], value) &&
    !(facet == "tool" && isTRUE(facets$tool_failed))
  facets[[facet]] <- if (active) NULL else value
  if (facet == "tool") {
    facets$tool_failed <- NULL
  }
  scans_app_facet_list(facets)
}

# Removing the last entry leaves an empty named list; keep "no facets" one
# value so reactives do not see a change.
scans_app_facet_list <- function(facets) {
  if (length(facets)) facets else list()
}

scans_app_facet_values <- function(data, facet) {
  switch(
    facet,
    day = c(
      cohort_bucket_key(data$records$started_at, "day"),
      cohort_bucket_key(data$records$started_at, "hour")
    ),
    tool = cohort_label(data$tool_calls$tool),
    user = cohort_label(data$records$user),
    model = cohort_label(data$records$model)
  )
}

# Drop values the loaded snapshot no longer contains.
scans_app_facet_validate <- function(data, facets) {
  for (facet in setdiff(names(facets), "tool_failed")) {
    if (
      !facet %in% scans_app_facet_names ||
        !facets[[facet]] %in% scans_app_facet_values(data, facet)
    ) {
      facets[[facet]] <- NULL
      if (facet == "tool") facets$tool_failed <- NULL
    }
  }
  if (isTRUE(facets$tool_failed) && !any(data$tool_calls$failed)) {
    facets$tool_failed <- NULL
  }
  scans_app_facet_list(facets)
}

scans_app_facet_filter <- function(data, indices, facets) {
  records <- data$records[indices, , drop = FALSE]
  keep <- rep(TRUE, length(indices))
  if (!is.null(facets$day)) {
    unit <- cohort_bucket_key_unit(facets$day)
    keep <- keep &
      cohort_bucket_key(records$started_at, unit) %in% facets$day
  }
  if (!is.null(facets[["tool"]]) || isTRUE(facets$tool_failed)) {
    calls <- data$tool_calls
    if (!is.null(facets[["tool"]])) {
      calls <- calls[
        cohort_label(calls$tool) == facets[["tool"]],
        ,
        drop = FALSE
      ]
    }
    if (isTRUE(facets$tool_failed)) {
      calls <- calls[calls$failed, , drop = FALSE]
    }
    keep <- keep & records$trajectory_id %in% calls$trajectory_id
  }
  if (!is.null(facets$user)) {
    keep <- keep & cohort_label(records$user) == facets$user
  }
  if (!is.null(facets$model)) {
    keep <- keep & cohort_label(records$model) == facets$model
  }
  indices[keep]
}

scans_app_bucket_label <- function(key) {
  hourly <- cohort_bucket_key_unit(key) == "hour"
  time <- as.POSIXct(
    key,
    format = if (hourly) "%Y-%m-%dT%H" else "%Y-%m-%d",
    tz = "UTC"
  )
  format(time, if (hourly) "%b %d %H:00" else "%b %d", tz = "UTC")
}

# Chip labels for the active scope, keyed by what removing the chip clears.
scans_app_scope_labels <- function(
  facets,
  pattern = NULL,
  findings_only = FALSE,
  review = list()
) {
  c(
    if (!is.null(pattern)) {
      c(pattern = paste("Finding:", gsub("_", " ", pattern)))
    },
    if (isTRUE(findings_only)) c(findings = "With findings"),
    if (!is.null(review$state)) {
      c(review = if (review$state == "needs") "Needs review" else "Reviewed")
    },
    if (!is.null(review$label)) {
      c(review_label = paste("Label:", review$label))
    },
    if (isTRUE(facets$tool_failed)) {
      c(
        tool = if (is.null(facets[["tool"]])) {
          "Any tool call failed"
        } else {
          paste(facets[["tool"]], "failed")
        }
      )
    } else if (!is.null(facets[["tool"]])) {
      c(tool = paste("Tool:", facets[["tool"]]))
    },
    if (!is.null(facets$day)) c(day = scans_app_bucket_label(facets$day)),
    if (!is.null(facets$user)) c(user = paste("User:", facets$user)),
    if (!is.null(facets$model)) c(model = paste("Model:", facets$model))
  )
}

scans_app_scope_chips_ui <- function(labels) {
  if (!length(labels)) {
    return(NULL)
  }
  htmltools::div(
    class = "scans-app-scope-chips",
    role = "group",
    `aria-label` = "Active filters",
    lapply(names(labels), function(name) {
      htmltools::tags$button(
        type = "button",
        class = "scans-app-chip",
        `data-scans-facet-clear` = name,
        `aria-label` = paste("Remove filter:", labels[[name]]),
        htmltools::tags$span(labels[[name]]),
        htmltools::tags$span(
          class = "scans-app-chip-x",
          `aria-hidden` = "true",
          "\u00d7"
        )
      )
    }),
    if (length(labels) > 1L) {
      htmltools::tags$button(
        type = "button",
        class = "scans-app-chip-clear",
        `data-scans-facet-clear` = "all",
        "Clear all"
      )
    }
  )
}

scans_app_no_label <- "(no label)"
