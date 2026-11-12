# Check that VERSION and the APP_VERSION constant in app.R agree.
# Run from the project root in the RStudio console: source("App_Dev/tools/check_version.R")
ver_file <- trimws(readLines("VERSION", warn = FALSE)[1])
app_line <- grep('APP_VERSION <-', readLines("App_Dev/app.R", warn = FALSE), value = TRUE)[1]
app_ver  <- trimws(gsub('.*"(.*)".*', "\\1", app_line))
cat("VERSION file :", ver_file, "\n")
cat("app.R        :", app_ver, "\n")
if (identical(ver_file, app_ver)) cat("OK - versions match\n") else
  stop("Version mismatch - fix before committing/deploying.")
