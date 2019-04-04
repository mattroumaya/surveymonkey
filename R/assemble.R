#
# surv_id <- 168893066 # for dev't purposes, the Spring 2019 LIFT Teacher Survey
# one_surv <- fetch_survey_obj(surv_id)

# Bring it all together
#' Take a survey object and parses it into a tidy data.frame.
#'
#' @param surv_obj a survey, the result of a call to \code{fetch_survey_obj}.
#'
#' @return a data.frame (technically a \code{tibble}) with clean responses, one line per respondent.
#' @export

parse_responses <- function(surv_obj){
  choices <- survey_choices(surv_obj)
  questions <- get_questions(surv_obj)
  responses <- get_responses(surv_obj$id)
  responses <- responses %>% fix_responses()


  x <- dplyr::inner_join(choices, questions) %>%
    dplyr::inner_join(responses) %>%
    dplyr::rename(open_response_text = answertext)

  final_x <- x %>%
    dplyr::mutate(combined_text = dplyr::case_when(
      #     !is.na(subquestion_text) ~ paste(stringr::str_trunc(heading, width = 40), subquestion_text, sep = " "),
      !is.na(subquestion_text) ~ paste(heading, subquestion_text, sep = " "),
      TRUE ~ heading)) %>%
    dplyr::select(collector_id, recipient_id, response_id, question_type, combined_text, text) %>%

    # expand multiple choice Qs to be one-column-per
    # if these are made into factors in the order the choices are offered, would that carry over to column order post-spread
    # would be desirable
    dplyr::mutate(combined_text = dplyr::case_when(
      question_type %in% "multiple_choice" ~ paste(combined_text, text, sep = " - "),
      TRUE ~ combined_text)
    ) %>%
    # remove HTML tags from question text
    dplyr::mutate(combined_text = gsub("<span.*\">", "", combined_text),
           combined_text = gsub("</span>", "", combined_text),
           combined_text = gsub("<em>", "", combined_text),
           combined_text = gsub("</em>", "", combined_text))

  # spread wide
  out <- final_x %>%
    dplyr::select(-question_type) %>%
    tidyr::spread(combined_text, text)

  out
}
