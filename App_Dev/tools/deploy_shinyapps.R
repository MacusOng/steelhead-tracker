# Deploy Let's Follow the Steelhead to shinyapps.io
# Run from the project root in the RStudio console:
#   setwd("<project root>"); source("App_Dev/tools/deploy_shinyapps.R")
# One-time: paste your rsconnect::setAccountInfo(...) from the shinyapps.io dashboard first.
stopifnot(requireNamespace("rsconnect", quietly = TRUE))
source("App_Dev/tools/check_version.R")   # refuse to deploy on a version mismatch
ver <- trimws(readLines("VERSION", warn = FALSE)[1])
cat("Deploying v", ver, " to shinyapps.io (account macus-ong)...\n", sep = "")
ans <- readline("Proceed? (y/n) ")
if (tolower(ans) == "y") {
  rsconnect::deployApp(
    appDir      = "App_Dev",
    appName     = "steelhead-tracker",
    appTitle    = "Let's Follow the Steelhead",
    account     = "macus-ong",
    forceUpdate = TRUE)
  cat("Done - https://macus-ong.shinyapps.io/steelhead-tracker/\n")
} else cat("Cancelled.\n")
