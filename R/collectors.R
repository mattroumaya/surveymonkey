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

  if (!is.null(oauth_token)) {
    token <- paste("bearer", oauth_token)
  } else {
    stop(
      "Must specify 'oauth_token'.
      See https://github.com/tntp/surveymonkey#authentication for more info."
    )
  }
  b <- list(page = page)
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- NULL
  } else {
    b <- b[!nulls]
  }
  h <- httr::add_headers(
    Authorization = token,
    "Content-Type" = "application/json"
  )


  out <- httr::GET(u,
    config = h,
    query = b,
    httr::user_agent("http://github.com/tntp/surveymonkey")
  )
  message(paste0(
    "you have ",
    out$headers$`x-ratelimit-app-global-day-remaining`,
    " requests left today before you hit the limit"
  ))
  httr::stop_for_status(out)
  parsed_content <- httr::content(out, as = "parsed")

  collectors <- parsed_content$data

  collectors %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id = as.numeric(.data$id)) %>%
    return()
}
