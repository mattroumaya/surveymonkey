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
#' Retrieves the currently set survey monkey oauth token
#' @export
get_token = function() {
  getOption("sm_oauth_token")
}


get_bearer_token = function(oauth_token = NULL) {
  if (is.null(oauth_token)) {
    stop(
      "Must specify 'oauth_token'.
      See https://github.com/mattroumaya/surveymonkey#authentication for more info."
    )
  }
  paste("bearer", oauth_token)
}
