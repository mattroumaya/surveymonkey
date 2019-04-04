#' survey_questions
#'
#' Creates a data frame from the survey questions and answers
#'
#' @param surv_obj a survey details object, the result of a call to fetch_survey_details
#' @return A data frame with one row per question/subquestion/answer choice
#' @export get_questions

get_questions <- function(surv_obj){

  # use parser functions to grab questions, choices, and rows
  # Not using choices for now
  questions <- purrr::map_df(surv_obj$pages, parse_page_of_questions) %>%
    dplyr::mutate(survey_id = surv_obj$id)

  rows <- purrr::map_df(surv_obj$pages, parse_page_for_rows) %>%
    dplyr::rename(subquestion_id = id) %>%
    dplyr::select(question_id, subquestion_id, subquestion_text = text)

  full_questions <- dplyr::full_join(questions,
                              rows,
                              by = "question_id") %>%
    dplyr::select(survey_id, question_id, question_type, question_subtype, subquestion_id, heading, subquestion_text) %>%
    dplyr::mutate(survey_id = as.numeric(survey_id))

  full_questions
}

# function that returns the table of unique answer choices
survey_choices <- function(surv_obj){

  choices <- purrr::map_df(surv_obj$pages, parse_page_for_choices) %>%
    dplyr::select(question_id, choice_id = id, text, position)# need to incorporate weight, etc.

  choices
}
