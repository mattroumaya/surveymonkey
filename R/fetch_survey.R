# Returns the details of a survey, to cut down on API calls

#' Title
#'
#' @param id ID number of survey to be fetched.
#' @param oauth_token Your OAuth 2.0 token. By default, retrieved from \code{getOption('sm_oauth_token')}.
#'
#' @return a survey object, which is a nested list containing info about the survey.
#' @export
#'
#' @examples
#' # not run:
#' # fetch_survey_obj(123456789)
#'
fetch_survey_obj <- function(id,
                             oauth_token = getOption('sm_oauth_token')){
  if(missing(id)){
    stop("specify an id")
  }

  if(!is.null(oauth_token)){
    u <- 'https://api.surveymonkey.com/v3/surveys?'
    token <- paste('bearer', oauth_token)
  }
  else
    stop("Must specify 'oauth_token'")

  h <- httr::add_headers(Authorization=token,
                         'Content-Type'='application/json')
  p <- list("v3", survey = "surveys", id = id, details = "details")

  out <- httr::GET(u, config = h, path = p)
  message(paste0("you have ", out$headers$`x-ratelimit-app-global-day-remaining`, " requests left today before you hit the limit"))
  httr::stop_for_status(out)
  parsed_content <- httr::content(out, as = 'parsed')

  parsed_content

}
