#' get_collectors
#'
#' Get collectors for a SurveyMonkey survey
#'
#' @param survey_id The id of the survey whose collectors you want,
#' find it with \code{\link{browse_surveys}}.
#' @param page Integer number to select which page of resources to return. By default is 1.
#' @param all_pages Return all pages of respondents?
#' Default is TRUE, which will fetch all responses (and cause n/100 calls to the API).
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#' @param sort_by Column to sort by. Can be one of: id, date_modified, type, status, name.
#' @param sort_order Sort data by the value in `sort_by`. Can be one of: ASC, DESC.
#' @param name Nickname of collector to search against
#' @param start_date Collectors must be created after this date. Date string in format YYYY-MM-DDTHH:MM:SS (no offset)
#' @param end_date Collectors must be created before this date. Date string in format YYYY-MM-DDTHH:MM:SS (no offset)
#' @param include Specify additional fields to return per collector. Can be one or more of: type, status, response_count, date_created, date_modified, url. Optionally, specify "everything" to return all possible fields.
#'
#' @return a data.frame (technically a \code{tibble}) with each collector and its information.
#'
#' @importFrom rlang .data
#'
#' @export

get_collectors <- function(survey_id,
                           page = 1,
                           per_page = NULL,
                           sort_by = NULL,
                           sort_order = NULL,
                           name = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           include = NULL,
                           all_pages = TRUE,
                           oauth_token = get_token()) {
  if (!is.null(include)){
    include <- paste(include, collapse = ",")
  }

  if (!is.null(include) & "everything" %in% tolower(include)) {
    include <- "type,status,response_count,date_created,date_modified,url"
  }

  u <- paste("https://api.surveymonkey.net/v3/surveys/", survey_id, "/collectors/", sep = "")
  h <- standard_request_header(oauth_token)
  b <- list(page = page,
            per_page = per_page,
            sort_by = sort_by,
            sort_order = sort_order,
            name = name,
            start_date = start_date,
            end_date = end_date,
            include = include)
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
