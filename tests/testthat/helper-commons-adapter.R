commons_trajectory_fixture <- function() {
  conversation <- list(
    ellmer::UserTurn(list(ellmer::ContentText("How many orders?"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("Six."))),
    ellmer::UserTurn(list(ellmer::ContentText("What is revenue?"))),
    ellmer::AssistantTurn(
      list(ellmer::ContentText("Revenue excludes tax."))
    )
  )
  attr(conversation, "last_active") <- as.POSIXct(
    "2026-08-23 12:00:00",
    tz = "UTC"
  )
  attr(conversation, "provenance") <- list(
    list(
      provenance_tag = "A",
      citation_decisions = list()
    ),
    list(
      provenance_tag = "B",
      citation_decisions = list(list(
        quote = "Revenue excludes tax.",
        status = "accepted",
        label = "Data dictionary",
        kind = "definition"
      ))
    )
  )

  out <- list(`conversation-001` = conversation)
  attr(out, "source") <- list(
    kind = "local",
    path = "/tmp/commons-traces"
  )
  out
}
