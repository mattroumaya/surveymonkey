# Generate a suggestion to submit a bug report at a specific URL
# This is a function so that it can be easily changed if the repo URL changes

#' @importFrom utils packageDescription
file_bug_report_msg <- function() {
  paste("file a bug report at", packageDescription("surveymonkey")$BugReports)
}

format_date = function(date) {
  if (inherits(date, "POSIXct") | inherits(date, "Date")) {
    date <- format(date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  date
}
