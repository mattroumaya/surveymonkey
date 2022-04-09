#' get_responses
#'
#' Get responses for a SurveyMonkey survey
#'
#' @param id The survey's ID, can be found with \code{browse_survey()}.
#' @param page Integer number to select which page of resources to return. By default is 1.
#' @param all_pages return all pages of respondents?
#'  Default is TRUE, which will fetch all responses (and cause n/100 calls to the API).
#' @param per_page Integer number to set the number of surveys to return per page.
#' By default, is 100 surveys per page (appears to be the maximum allowed by the API).
#' @param start_created_at Date string used to select surveys created after this date.
#' By default is NULL.
#' @param end_created_at Date string used to select surveys modified before this date.
#' By default is NULL.
#' @param start_modified_at Date string used to select surveys last modified after this date.
#' By default is NULL.
#' @param end_modified_at Date string used to select surveys modified before this date.
#' By default is NULL.
#' @param sort_order String used to set the sort order for returned surveys:
#' 'ASC’ or 'DESC’. By default, DESC.
#' @param sort_by String value of field used to sort returned survey list:
#' ‘title’, 'date_modified’, or 'num_responses’. By default, date_modified.
#' @param oauth_token Your OAuth 2.0 token.
#' By default, retrieved from \code{get_token()}.
#' @param verbose Show API rate limit messages?
#' @return A list of object of class {sm_response}
#' @references SurveyMonkey API V3 at
#' \url{https://developer.surveymonkey.com/api/v3/#survey-responses}
#' @export get_responses
#
# get a set of bulk responses (this will get 100 responses with the following structure:
# $per_page        : int  = total number of responses per page
# $total           : int  = number of survey responses
# $data[[x]]       : list = list with an entry for each individual survey response
#   $total_time   : int  = time spent on the survey
#   $href         : chr  = api url for survey response
#   $custom_variables  : list = custom variables for respondents
#   $ip_address : chr  = IP address for respondent
#   $id : chr = id of survey response
#   $logic_path : list
#   $date_modified : chr = date survey response last modified
#   $response_status : chr = status of response {completed, partial, etc...}
#   $custom_value : chr = ?
#   $analyze_url : chr = web browsable url to view responses
#   $pages : list = list with data for questions and answers on each survey page
#     $id : chr = id
#     $ questions : list
#       $ id : chr = id
#       $ answers : list
#         $ choice_id : chr = id of answer choice
#   $page_path : list = ?
#   $recipient_id : chr = id of survey recipient
#   $collector_id : chr = id of survey collector
#   $date_created : chr = date the survey response was started
#   $survey_id : chr = id of the survey
#   $collection_mode : chr = ?
#   $edit_url : chr = web browsable url to modify responses
#   $metadata : list = list with additional information about respondent
#     $contact : list
#     $contact$first_name : list
#     $contact$first_name$type : chr = type for first_name$value variable
#     $contact$first_name$value : chr = respondent first name
#     $contact$last_name : list
#     $contact$last_name$type : chr = type for last_name$value variable
#     $contact$lasy_name$value : chr = respondent last name
#     $contact$email : list
#     $contact$email$type : chr = type for email variable
#     $contact$email$value : chr = respondent email address
# $page      : int  = page of responses
# $links     : list = urls for the previous ($last), current ($self) and next ($next) response pages
# )



get_responses <- function(id,
                          page = 1,
                          all_pages = TRUE,
                          per_page = 100,
                          start_created_at = NULL,
                          end_created_at = NULL,
                          start_modified_at = NULL,
                          end_modified_at = NULL,
                          sort_order = "DESC",
                          sort_by = "date_modified",
                          oauth_token = get_token(),
                          verbose = TRUE) {

  u <- paste("https://api.surveymonkey.net/v3/surveys/", id, "/responses/bulk?", sep = "")
  h <- standard_request_header(oauth_token)

  start_created_at <- format_date(start_created_at)
  end_created_at <- format_date(end_created_at)
  start_modified_at <- format_date(start_modified_at)
  end_modified_at <- format_date(end_modified_at)

  b <- list(
    page = page,
    per_page = per_page,
    start_created_at = start_created_at,
    end_created_at = end_created_at,
    start_modified_at = start_modified_at,
    end_modified_at = end_modified_at,
    sort_order = sort_order,
    sort_by = sort_by
  )
  nulls <- sapply(b, is.null)
  if (all(nulls)) {
    b <- NULL
  } else {
    b <- b[!nulls]
  }

  parsed_content <- sm_get(url = u, query = b, config = h, verbose = verbose)

  responses <- parsed_content$data

  # recursively get all responses if all_pages = TRUE
  if (all_pages == TRUE & (!is.null(parsed_content$links[["next"]]))) {
    rnext <- get_responses(id,
      page = page + 1,
      all_pages,
      per_page,
      start_created_at,
      end_created_at,
      start_modified_at,
      end_modified_at,
      sort_order,
      sort_by,
      oauth_token = oauth_token
    )
    responses <- c(responses, rnext)
  }
  responses
}
