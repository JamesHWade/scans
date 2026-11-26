# Captures real Deputy OpenTelemetry spans as OTLP NDJSON for the scans tests.
#
# Deputy agents run against deputy's own local fake OpenAI-compatible server
# (tests/testthat/helper-runtime-http.R), so no provider or key is involved,
# and otelsdk's file exporter writes the spans. Run from the scans root:
#
#   Rscript tools/capture-deputy-otel-fixture.R /path/to/deputy-checkout
#
# It writes tests/testthat/fixtures/deputy/otel/trace-0.jsonl.
args <- commandArgs(TRUE)
deputy_path <- normalizePath(args[[1]])
out_dir <- normalizePath(
  "tests/testthat/fixtures/deputy/otel",
  mustWork = FALSE
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(out_dir, "[.]jsonl$", full.names = TRUE))

library(testthat)
test_dir <- file.path(deputy_path, "tests", "testthat")
helpers <- new.env()
withr::with_dir(test_dir, {
  for (file in list.files(".", "^helper.*[.]R$")) {
    sys.source(file, envir = helpers)
  }
})
compaction_turns <- file.path(
  test_dir,
  "fixtures",
  "compaction",
  "evidence-review.json"
)

# The harness ties its shared server's lifetime to testthat's teardown
# environment; outside a test run this script's own session stands in.
testthat::local_mocked_bindings(
  teardown_env = function() globalenv(),
  .package = "testthat"
)
local({
  server <- function(responses) {
    local_runtime_server(responses, .local_envir = parent.frame(2))
  }
  environment(server) <- helpers
  # Compaction (primary down, summary from a fallback) then the task answered
  # by a fallback chat; a denied tool call; a provider cut-off; a delegation.
  urls <- list(
    compact_primary = server(list(helpers$runtime_failure()))$url,
    compact_summary = server(list(helpers$runtime_reply(
      "Keep assay-C-r3, denominator 84."
    )))$url,
    compact_task = server(list(helpers$runtime_reply("E is eligible.")))$url,
    deny = server(list(
      helpers$runtime_reply(tool = "effect"),
      helpers$runtime_reply("I could not run it.")
    ))$url,
    cutoff = server(list(helpers$runtime_reply(
      "The answer begins",
      finish = "length"
    )))$url,
    delegate = server(list(
      helpers$runtime_reply(
        tool = "delegate_to_agent",
        arguments = list(agent_name = "worker", task = "Count the rows")
      ),
      helpers$runtime_reply("There are 84 rows."),
      helpers$runtime_reply("The worker counted 84 rows.")
    ))$url
  )

  callr::r(
    function(deputy_path, out_dir, urls, compaction_turns) {
      Sys.setenv(
        OTEL_TRACES_EXPORTER = "otlp/file",
        OTEL_EXPORTER_OTLP_TRACES_FILE = file.path(out_dir, "trace-%N.jsonl"),
        OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "true"
      )
      pkgload::load_all(deputy_path, quiet = TRUE)
      options(ellmer_max_tries = 1)
      chat <- function(url, model = "gpt-4o-mini") {
        ellmer::chat_openai_compatible(
          base_url = url,
          credentials = function() "fixture",
          model = model,
          echo = "none"
        )
      }

      primary <- chat(urls$compact_primary)
      rlang::env_binding_unlock(primary, "token_count")
      primary$token_count <- function(...) 1000
      turns <- jsonlite::fromJSON(compaction_turns, simplifyVector = FALSE)
      primary$set_turns(lapply(turns$turns, function(turn) {
        if (identical(turn$role, "user")) {
          ellmer::UserTurn(contents = list(ellmer::ContentText(turn$text)))
        } else {
          ellmer::AssistantTurn(contents = list(ellmer::ContentText(turn$text)))
        }
      }))
      agent <- Agent$new(
        primary,
        fallback_chats = list(chat(urls$compact_task, "backup")),
        context_policy = ContextPolicy(
          max_tokens = 50,
          summary_fallback_chats = list(chat(urls$compact_summary)),
          estimator = "provider"
        )
      )
      agent$conversation_id <- "c_review"
      agent$run_sync("Continue the review")

      tool <- ellmer::tool(
        function() "effect done",
        name = "effect",
        description = "effect"
      )
      agent <- Agent$new(
        chat(urls$deny),
        tools = list(tool),
        permissions = Permissions(mode = "readonly", tool_denylist = "effect")
      )
      agent$conversation_id <- "c_denied"
      suppressWarnings(agent$run_sync("Run the effect"))

      agent <- Agent$new(chat(urls$cutoff))
      agent$conversation_id <- "c_cutoff"
      agent$run_sync("Explain the result")

      agent <- LeadAgent$new(
        chat(urls$delegate),
        sub_agents = list(agent_definition("worker", "Worker", "Count rows.")),
        permissions = permissions_full()
      )
      agent$conversation_id <- "c_team"
      agent$run_sync("How many rows are there?")
      otel::get_default_tracer_provider()$flush()
      invisible(NULL)
    },
    args = list(
      deputy_path = deputy_path,
      out_dir = out_dir,
      urls = urls,
      compaction_turns = compaction_turns
    ),
    show = TRUE
  )
})
# Keep one file, and replace the host identity otelsdk records on every
# resource with fixed values.
unlink(file.path(out_dir, "trace-latest.jsonl"))
path <- file.path(out_dir, "trace-0.jsonl")
lines <- vapply(
  readLines(path),
  function(line) {
    envelope <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    envelope$resourceSpans <- lapply(envelope$resourceSpans, function(rs) {
      rs$resource$attributes <- lapply(rs$resource$attributes, function(a) {
        if (identical(a$key, "process.owner")) {
          a$value <- list(stringValue = "fixture")
        }
        if (identical(a$key, "process.pid")) {
          a$value <- list(intValue = "1")
        }
        a
      })
      rs
    })
    as.character(jsonlite::toJSON(envelope, auto_unbox = TRUE, digits = NA))
  },
  character(1),
  USE.NAMES = FALSE
)
writeLines(lines, path)
print(list.files(out_dir))
