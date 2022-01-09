# Generate a suggestion to submit a bug report at a specific URL
# This is a function so that it can be easily changed if the repo URL changes

file_bug_report_msg <- function() {
  "file a bug report at https://github.com/tntp/surveymonkey/issues"
}


#' Set Survey Monkey Oauth Token
#'
#' Sets the option 'sm_oauth_token' for the survey monkey API.
#' Token will be used for authorization headers for requests.
#'
#' @param oauth_token Your survey monkey OAuth 2.0 token.
#' @export
set_token = function(oauth_token) {
  options("sm_oauth_token" = oauth_token)
}

#' Retrieve set Survey Monkey OAuth Token
#'
#' Retrieves the currently set suvery monkey oauth token
#' @export
get_token = function() {
  getOption("sm_oauth_token")
}