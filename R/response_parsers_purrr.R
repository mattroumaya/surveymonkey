parse_single_answer <- function(answer){
  dplyr::bind_rows(answer)
}

parse_answers <- function(question){
  purrr::map_df(question$answers, parse_single_answer) %>%
    dplyr::mutate(question_id = question$id)
}

parse_page <- function(page){
  purrr::map_df(page$questions, parse_answers)
}

parse_response <- function(response){
  purrr::map_df(response$pages, parse_page) %>%
    dplyr::mutate(response_id = response$id,
                  collector_id = response$collector_id,
                  survey_id = response$survey_id,
                  recipient_id = dplyr::if_else(response$recipient_id == "", NA_character_, response$recipient_id))
}

parse_respondent_list <- function(respondents){
  purrr::map_df(respondents, parse_response) %>%
    dplyr::rename(answerchoice_id = other_id,
                  answertext = text,
                  subquestion_id = row_id) %>%
    dplyr::mutate(choice_id = dplyr::coalesce(choice_id, answerchoice_id)) %>% # when answerchoice_id is not NA, choice_id is NA
    dplyr::select(-answerchoice_id) %>%
    dplyr::select(survey_id, collector_id, recipient_id, response_id, everything())
}
