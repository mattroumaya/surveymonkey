#' survey_questions
#'
#' Creates a data frame from the survey questions and answers
#'
#' @param id SurveyMonkey survey id.
#' @return A data frame with one row per question/subquestion/answer choice
#' @export get_questions

get_questions <- function(id){
  if(missing(id)){
    stop("specify an id")
  }

  if(!is.null(oauth_token)){
    u <- 'https://api.surveymonkey.com/'
    token <- paste('bearer', oauth_token)
  }
  else{ stop("Must specify 'oauth_token'") }


  h <- httr::add_headers(Authorization=token,
                         'Content-Type'='application/json')
  p <- list("v3", survey = "surveys", id = id, details = "details")

  out <- httr::GET(u, config = h, path = p)
  message(paste0("you have ", out$headers$`x-ratelimit-app-global-day-remaining`, " requests left today before you hit the limit"))
  httr::stop_for_status(out)

  parsed_content <- httr::content(out, as = 'parsed')


  # use parser functions to grab questions, choices, and rows
  # Not using choices for now
  questions <- purrr::map_df(parsed_content$pages, parse_page_of_questions) %>%
    mutate(survey_id = id)

  questions

  rows <- purrr::map_df(parsed_content$pages, parse_page_for_rows) %>%
    rename(subquestion_id = id) %>%
    select(question_id, subquestion_id, subquestion_text = text)

  full_questions <- full_join(questions,
                              rows,
                              by = "question_id") %>%
    select(survey_id, question_id, question_type, question_subtype, subquestion_id, heading, subquestion_text)

}

# function that returns the table of unique answer choices
survey_choices <- function(survey){

  choices <- purrr::map_df(parsed_content$pages, parse_page_for_choices) %>%
    mutate(survey_id = id) %>%
    select(survey_id, question_id, choice_id = id, text, position) # need to incorporate weight, etc.

  choices
}
