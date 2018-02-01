#' survey_questions
#'
#' Creates a data frame from the survey questions and answers
#'
#' @param pc a survey details object, the result of a call to fetch_survey_details
#' @return A data frame with one row per question/subquestion/answer choice
#' @export get_questions

get_questions <- function(pc){

  # use parser functions to grab questions, choices, and rows
  # Not using choices for now
  questions <- purrr::map_df(pc$pages, parse_page_of_questions) %>%
    mutate(survey_id = id)

  rows <- purrr::map_df(pc$pages, parse_page_for_rows) %>%
    rename(subquestion_id = id) %>%
    select(question_id, subquestion_id, subquestion_text = text)

  full_questions <- full_join(questions,
                              rows,
                              by = "question_id") %>%
    select(survey_id, question_id, question_type, question_subtype, subquestion_id, heading, subquestion_text)

  full_questions
}

# function that returns the table of unique answer choices
survey_choices <- function(pc){

  choices <- purrr::map_df(pc$pages, parse_page_for_choices) %>%
    mutate(survey_id = as.numeric(id)) %>%
    select(survey_id, question_id, choice_id = id, text, position) # need to incorporate weight, etc.

  choices
}
