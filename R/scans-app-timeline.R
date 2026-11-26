# A compact timeline of one trajectory's timed events. Bars have a recorded
# duration; ticks do not. Every mark links to its event in the transcript
# through the same mechanism as evidence links.
scans_app_timeline_ui <- function(marks) {
  if (nrow(marks) < 2L) {
    return(NULL)
  }
  breaks <- attr(marks, "breaks")
  lanes <- intersect(names(cohort_timeline_lanes), marks$lane)
  describe <- function(row) {
    paste0(
      cohort_timeline_lanes[[row$lane]],
      ": ",
      row$label,
      if (row$failed) ", failed",
      " at ",
      format(row$timestamp, "%H:%M:%S", tz = "UTC"),
      if (is.na(row$duration)) {
        ", duration not recorded"
      } else {
        paste0(", ", scans_app_seconds(row$duration))
      }
    )
  }
  lane_ui <- function(lane) {
    rows <- marks[marks$lane == lane, , drop = FALSE]
    htmltools::tagList(
      htmltools::tags$span(
        class = "scans-app-timeline-label",
        cohort_timeline_lanes[[lane]]
      ),
      htmltools::tags$svg(
        class = "scans-app-timeline-lane",
        viewBox = "0 0 1000 14",
        preserveAspectRatio = "none",
        role = "group",
        `aria-label` = cohort_timeline_lanes[[lane]],
        lapply(seq_len(nrow(breaks)), function(i) {
          htmltools::tags$rect(
            class = "scans-app-timeline-break",
            x = breaks$x[[i]] * 1000,
            y = 0,
            width = breaks$width[[i]] * 1000,
            height = 14
          )
        }),
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, ]
          tick <- is.na(row$width)
          htmltools::tags$a(
            href = paste0("#", scans_app_event_dom_id(row$event_id)),
            `data-scans-event` = row$event_id,
            `aria-label` = describe(row),
            class = paste(
              "scans-app-timeline-mark",
              paste0("scans-app-timeline-", lane),
              if (tick) "scans-app-timeline-tick",
              if (row$failed) "scans-app-timeline-failed"
            ),
            htmltools::tags$title(describe(row)),
            htmltools::tags$rect(
              x = min(row$x * 1000, 996),
              y = if (tick) 1 else 3,
              width = if (tick) 4 else max(row$width * 1000, 4),
              height = if (tick) 12 else 8
            )
          )
        })
      )
    )
  }
  first <- min(marks$timestamp)
  last <- max(
    marks$timestamp + ifelse(is.na(marks$duration), 0, marks$duration)
  )
  htmltools::tags$section(
    class = "scans-app-timeline",
    `aria-label` = "Timeline of timed events",
    htmltools::div(class = "scans-app-timeline-lanes", lapply(lanes, lane_ui)),
    htmltools::div(
      class = "scans-app-timeline-axis",
      htmltools::tags$span(format(first, "%H:%M:%S", tz = "UTC")),
      htmltools::tags$span(
        sprintf(
          "%s timed %s over %s%s. Bars have a recorded duration; ticks do not.",
          scans_app_count(nrow(marks)),
          if (nrow(marks) == 1L) "event" else "events",
          scans_app_seconds(as.numeric(last) - as.numeric(first)),
          if (nrow(breaks)) {
            sprintf(
              ", with %s shortened (%s)",
              scans_app_plural(nrow(breaks), "idle gap"),
              scans_app_seconds(sum(breaks$seconds))
            )
          } else {
            ""
          }
        )
      ),
      htmltools::tags$span(format(last, "%H:%M:%S UTC", tz = "UTC"))
    ),
    htmltools::tags$details(
      class = "scans-app-chart-data",
      htmltools::tags$summary("Show marks"),
      htmltools::tags$ol(
        class = "scans-app-timeline-list",
        lapply(seq_len(nrow(marks)), function(i) {
          htmltools::tags$li(describe(marks[i, ]))
        })
      )
    )
  )
}
