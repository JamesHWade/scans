# Time scans on the fixed 10,000-trajectory fixture.
#
#   Rscript tools/scale-profile.R [results.csv]
#
# Run from the package checkout. It loads the checkout with pkgload when that
# is installed, so it measures the working tree, and otherwise the installed
# package. Each step runs several times; the table reports the median
# elapsed time, the range, and the median peak memory R allocated above what
# was in use when the step started. Nothing here asserts a time: the recorded
# baseline and budgets are in docs/design/scale-profile.md.
#
# The steps are the ones a reviewer waits for: building and validating the
# bundle, the diagnostics, the app's data and cohort assembly, each way of
# narrowing the cohort followed by the overview it redraws, the browser list,
# the report, and saving and reopening an investigation. Rendering is the
# server's work to produce HTML; it does not include the browser's layout.

if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(scans)
}
source(file.path("tools", "scale-fixture.R"))

internal <- function(name) get(name, envir = asNamespace("scans"))
results <- new.env(parent = emptyenv())
results$rows <- list()

# A step that takes over a minute runs once, one over five seconds three
# times, and the rest five. Peak memory is the high-water mark after a reset,
# less what was already in use.
megabytes <- function(usage, column) {
  sum(usage[, match(column, colnames(usage)) + 1L])
}

measure <- function(group, step, expr, n = NA_integer_) {
  expr <- substitute(expr)
  frame <- parent.frame()
  run <- function() {
    gc(reset = TRUE, full = TRUE)
    used <- megabytes(gc(), "used")
    elapsed <- system.time(value <- eval(expr, frame))[["elapsed"]]
    list(
      value = value,
      elapsed = elapsed,
      peak = megabytes(gc(), "max used") - used
    )
  }
  first <- run()
  runs <- c(
    list(first),
    lapply(
      seq_len(
        if (first$elapsed > 60) {
          0L
        } else if (first$elapsed > 5) {
          2L
        } else {
          4L
        }
      ),
      function(i) run()
    )
  )
  elapsed <- vapply(runs, function(run) run$elapsed, 0)
  peak <- vapply(runs, function(run) run$peak, 0)
  results$rows[[length(results$rows) + 1L]] <- data.frame(
    group = group,
    step = step,
    n = n,
    runs = length(runs),
    median_s = stats::median(elapsed),
    min_s = min(elapsed),
    max_s = max(elapsed),
    peak_mb = stats::median(peak)
  )
  message(sprintf("%-28s %-44s %8.3f s", group, step, stats::median(elapsed)))
  invisible(first$value)
}

html_mb <- function(tag) nchar(as.character(tag), type = "bytes") / 1024^2

# Bundle ---------------------------------------------------------------
# SCANS_SCALE_N shrinks the cohort for a quick check that the script runs.
# The recorded baseline uses the default.
cohort_size <- as.integer(Sys.getenv("SCANS_SCALE_N", "10000"))
bundle <- measure("bundle", "generate fixture and construct", {
  scale_fixture(cohort_size)
})
shape <- scale_fixture_shape(bundle)
tables <- S7::props(bundle)
tables$schema_version <- NULL
n <- nrow(trajectory_info(bundle))
measure("bundle", "TrajectoryBundle() from tables", n = n, {
  do.call(TrajectoryBundle, tables)
})
measure("bundle", "validate", n = n, S7::validate(bundle))

# Diagnostics ----------------------------------------------------------
measure("diagnostics", "scan_trajectories()", n = n, scan_trajectories(bundle))
assessment <- measure("diagnostics", "assess_trajectory_scans()", n = n, {
  assess_trajectory_scans(bundle)
})
summaries <- measure("diagnostics", "summarize_trajectories()", n = n, {
  summarize_trajectories(bundle)
})
measures <- measure("diagnostics", "measure_trajectories()", n = n, {
  measure_trajectories(bundle)
})

# App data and cohort assembly ---------------------------------------
analysis <- c(assessment, list(summaries = summaries, measures = measures))
data <- measure("app data", "scans_app_data(), diagnostics included", n = n, {
  internal("scans_app_data")(bundle)
})
measure("app data", "scans_app_data(), diagnostics supplied", n = n, {
  internal("scans_app_data")(bundle, analysis = analysis)
})
ids <- data$info$trajectory_id
queue <- measure("app data", "review queue for the snapshot", n = n, {
  internal("cohort_review_queue")(data, ids)
})
everything <- seq_along(ids)
overview <- function(indices) {
  internal("scans_app_performance_data")(data, indices, queue = queue)
}
performance <- measure("cohort", "overview data, all trajectories", n = n, {
  overview(everything)
})
scans <- unique(data$assessments$scan)
measure("cohort", "overview HTML, all trajectories", n = n, {
  as.character(internal("scans_app_performance_ui")(
    performance,
    "Scale",
    "elapsed",
    scans
  ))
})

# Filter paths: select the rows, then assemble the overview for them ----
view <- function(...) internal("scans_app_input_view")(data, ...)
select <- function(view, facets = list()) {
  internal("scans_app_facet_filter")(
    data,
    internal("scans_app_view_indices")(data, view),
    facets
  )
}
day <- format(data$records$started_at[[n %/% 2L]], "%Y-%m-%d", tz = "UTC")
paths <- list(
  "facet: model" = list(view(), list(model = "support-large")),
  "facet: tool failed" = list(
    view(),
    list(tool = "lookup_order", tool_failed = TRUE)
  ),
  "facet: user" = list(view(), list(user = "user-001")),
  "time window: one day" = list(view(), list(day = day)),
  "text search" = list(view(query = "refund"), list()),
  "text search, no match" = list(view(query = "zzz-not-present"), list()),
  "findings only" = list(view(findings_only = TRUE), list()),
  "pattern" = list(view(pattern = "suspicious_tool_loop"), list())
)
for (path in names(paths)) {
  filter <- paths[[path]]
  indices <- select(filter[[1L]], filter[[2L]])
  measure("filter: select rows", path, n = length(indices), {
    select(filter[[1L]], filter[[2L]])
  })
  measure("filter: overview data", path, n = length(indices), {
    overview(indices)
  })
}
# With a facet active the app also assembles the cohort without that facet.
measure("filter: overview data", "facet: model, with its wider scope", {
  internal("scans_app_performance_data")(
    data,
    select(view(), list(model = "support-large")),
    scopes = list(model = everything),
    queue = queue
  )
})

# Browser list -------------------------------------------------------
# The list renders at most `scans_app_entry_limit` entries; `limit = Inf`
# measures what rendering every entry would cost.
for (size in unique(pmin(c(100L, 500L, 1000L, n), n))) {
  listed <- queue$index[seq_len(size)]
  list_html <- measure(
    "browser list",
    "entries HTML, as the app lists them",
    n = size,
    {
      as.character(internal("scans_app_entries_ui")(data, listed, queue))
    }
  )
  message(sprintf("  %d visible: %.2f MB of HTML", size, html_mb(list_html)))
}
measure(
  "browser list",
  "entries HTML, every entry rendered",
  n = min(2000L, n),
  {
    as.character(internal("scans_app_entries_ui")(
      data,
      queue$index[seq_len(min(2000L, n))],
      queue,
      limit = Inf
    ))
  }
)
measure("browser list", "order by newest", n = n, {
  internal("scans_app_order_records")(data$records, everything, "newest")
})

# One session ---------------------------------------------------------
# The server's work from a session starting to its first overview and list,
# with the snapshot's data already prepared.
if (requireNamespace("shiny", quietly = TRUE)) {
  server <- internal("scans_app_server")(data)
  measure("app session", "start to first overview and list", n = n, {
    shiny::testServer(server, {
      session$setInputs(scans_app_scans = scans)
      session$flushReact()
      c(
        nchar(output$scans_app_performance$html),
        nchar(output$scans_app_entries$html)
      )
    })
  })
}

# Report and saved investigation ------------------------------------
# Every saved-investigation step verifies or computes SHA-256 identifiers
# over the whole payload, so each is measured for a 1,000-trajectory
# selection.
# The whole cohort is left out by default: on the recording machine its
# snapshot and verification took ten and six minutes, and writing the file
# ran the machine out of memory. SCANS_SCALE_SAVED names the selection sizes
# to measure, for example "1000,10000", or "0" for none.
path <- tempfile(fileext = ".scans.json")
sizes <- list()
saved_sizes <- as.integer(strsplit(
  Sys.getenv("SCANS_SCALE_SAVED", "1000"),
  ",",
  fixed = TRUE
)[[1L]])
saved_sizes <- unique(pmin(saved_sizes[saved_sizes > 0L], n))
settings <- list(
  scans = scan_registry()$scan,
  repeat_threshold = 2L,
  loop_threshold = 3L
)
for (size in saved_sizes) {
  chosen <- ids[seq_len(size)]
  group <- sprintf("investigation, %s", format(size, big.mark = ","))
  saved <- measure(group, "investigation_snapshot()", n = size, {
    investigation_snapshot(bundle, chosen, application = "Scale")
  })
  measure(group, "verify identifiers and references", n = size, {
    internal("investigation_validate")(saved)
  })
  # Verification is the step above; this times the report's own work.
  report <- measure(group, "investigation_report(verify = FALSE)", n = size, {
    investigation_report(saved, verify = FALSE)
  })
  markdown <- measure(group, "format(), Markdown", n = size, format(report))
  html <- measure(group, "format(), HTML", n = size, format(report, "html"))
  measure(group, "format(), HTML, every trajectory listed", n = size, {
    format(report, "html", max_evidence = size)
  })
  measure(group, "write_investigation()", n = size, {
    write_investigation(saved, path, overwrite = TRUE)
  })
  measure(group, "read_investigation()", n = size, {
    read_investigation(path, max_bytes = Inf)
  })
  sizes[[group]] <- sprintf(
    "%s: report %.2f MB Markdown, %.2f MB HTML; file %.1f MB.",
    group,
    sum(nchar(markdown, type = "bytes") + 1) / 1024^2,
    nchar(html, type = "bytes") / 1024^2,
    file.size(path) / 1024^2
  )
}
# The report's own work, without verifying the saved file's identifiers:
# what the app's Export menu waits for.
measure("report assembly", "report_build(), all trajectories", n = n, {
  internal("report_build")(
    data,
    everything,
    list(
      application = "Scale",
      view = view(),
      bundle = bundle,
      queue = queue,
      settings = settings
    )
  )
})

# Results ------------------------------------------------------------
table <- do.call(rbind, results$rows)
cat("\n## Environment\n\n")
cat(sprintf("- %s\n", R.version.string))
cat(sprintf(
  "- %s %s (%s)\n",
  Sys.info()[["sysname"]],
  Sys.info()[["release"]],
  Sys.info()[["machine"]]
))
packages <- c(
  "scans",
  "S7",
  "tibble",
  "vctrs",
  "rlang",
  "cli",
  "jsonlite",
  "digest",
  "htmltools",
  "shiny",
  "bslib"
)
cat(sprintf(
  "- %s\n",
  paste(
    vapply(
      packages,
      function(package) paste(package, utils::packageVersion(package)),
      ""
    ),
    collapse = ", "
  )
))
cat("\n## Fixture\n\n")
utils::str(shape, give.attr = FALSE)
cat("\n")
cat(sprintf("- %s\n", unlist(sizes)), sep = "")
cat("\n## Timings\n\n")
cat("| Group | Step | n | Runs | Median (s) | Range (s) | Peak memory (MB) |\n")
cat("| --- | --- | --- | --- | --- | --- | --- |\n")
cat(
  sprintf(
    "| %s | %s | %s | %d | %.3f | %.3f–%.3f | %.0f |\n",
    table$group,
    table$step,
    ifelse(is.na(table$n), "", format(table$n, big.mark = ",", trim = TRUE)),
    table$runs,
    table$median_s,
    table$min_s,
    table$max_s,
    table$peak_mb
  ),
  sep = ""
)

output <- commandArgs(trailingOnly = TRUE)
if (length(output)) {
  utils::write.csv(table, output[[1L]], row.names = FALSE)
}
