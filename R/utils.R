# Generate a suggestion to submit a bug report at a specific URL
# This is a function so that it can be easily changed if the repo URL changes

file_bug_report_msg <- function() {
  "file a bug report at https://github.com/tntp/surveymonkey/issues"
}

format_date = function(date) {
  if (inherits(date, "POSIXct") | inherits(date, "Date")) {
    date <- format(date, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  date
}