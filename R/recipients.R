#' get_recipients
#'
#' Get recipients for a SurveyMonkey collector
#' @param collector_id the collector whose responses you want,
#' find this value with \code{\link{get_collectors}}.
#' @param page Integer to select which page of resources to return. By default is 1.
#' @param per_page How many recipients per page?  Default is 50, which appears to be the maximum.
#' @param all_pages return all pages of respondents?
#' Default is TRUE, which will fetch all responses (and cause n/50 calls to the API).
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#' @return a data.frame (technically a \code{tibble}) with each collector and its information.
#' @importFrom rlang .data
#' @export
#'
## TODO: incorporate the rest of the args from the API
get_recipients <- function(collector_id,
                           page = 1,
                           per_page = 50,
                           all_pages = TRUE,
                           oauth_token = get_token()) {
  u <- paste("https://api.surveymonkey.net/v3/collectors/", collector_id, "/recipients/", sep = "")

  if (!is.null(oauth_token)) {
    token <- paste("bearer", oauth_token)
  } else {
    stop(
      "Must specify 'oauth_token'.
      See https://github.com/tntp/surveymonkey#authentication for more info."
    )
  }
  b <- list(
    page = page,
    include = c("survey_link")
  )
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

  recipients <- parsed_content$data

  # recursively get all recipients if all_pages = TRUE
  if (all_pages == TRUE & (!is.null(parsed_content$links[["next"]]))) {
    rnext <- get_recipients(collector_id,
      page = page + 1,
      per_page = per_page,
      all_pages = all_pages
    )
    recipients <- c(recipients, rnext)
  }

  recipients %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id = as.numeric(.data$id)) %>%
    return()
}
