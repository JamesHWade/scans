# Programmatic deploy of the scans Connect monitor.
# One-time setup (never commit the key):
#   rsconnect::addServer("https://<connect-host>", name = "connect")
#   rsconnect::connectApiUser("<username>", server = "connect",
#                             apiKey = Sys.getenv("CONNECT_API_KEY"))
# Install the dependencies locally first so rsconnect can record their sources:
#   pak::pak("JamesHWade/scans")
rsconnect::deployApp(
  appDir = "deploy/connect",
  appName = "scans-connect-monitor",
  appTitle = "scans: trajectory monitor",
  server = "connect",
  forceUpdate = TRUE
)
