#' get_collectors
#'
#' Get collectors for a SurveyMonkey survey
#'
#' @param survey_id the id of the survey whose collectors you want,
#' find it with \code{\link{browse_surveys}}.
#' @param page Integer number to select which page of resources to return. By default is 1.
#' @param all_pages return all pages of respondents?
#' Default is TRUE, which will fetch all responses (and cause n/100 calls to the API).
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#' @return a data.frame (technically a \code{tibble}) with each collector and its information.
#' @importFrom rlang .data
#' @export
#'
## TODO: incorporate the rest of the args from the API
get_collectors <- function(survey_id,
                           page = 1,
                           all_pages = TRUE,
                           oauth_token = get_token()) {
  u <- paste("https://api.surveymonkey.net/v3/surveys/", survey_id, "/collectors/", sep = "")
  h <- standard_request_header(oauth_token)

  b <- list(page = page)
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- NULL
  } else {
    b <- b[!nulls]
  }

  parsed_content <- sm_get(url = u, query = b, config = h)

  collectors <- parsed_content$data

  collectors %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id = as.numeric(.data$id)) %>%
    return()
}
