# Returns the details of a survey, to cut down on API calls

#' Title
#'
#' @param id ID number of survey to be fetched.
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#'
#' @return a survey object, which is a nested list containing info about the survey.
#' @export
#'
#' @examples
#' # not run:
#' # fetch_survey_obj(123456789)
fetch_survey_obj <- function(id,
                             oauth_token = get_token(),
                             verbose = TRUE) {
  if (missing(id)) {
    stop("specify an id")
  }

  u <- "https://api.surveymonkey.com/v3/surveys?"
  h <- standard_request_header(oauth_token)

  p <- list("v3", survey = "surveys", id = id, details = "details")

  parsed_content <- sm_get(url = u, query = NULL, config = h, path = p, verbose = verbose)

  parsed_content
}
