# Bring it together

surv_id <- 168893066 # Spring 2019 LIFT Teacher Survey
one_surv <- fetch_survey_obj(surv_id)

choices <- survey_choices(one_surv)
questions <- get_questions(one_surv)
responses <- get_responses(one_surv$id)
responses <- responses %>% fix_responses()


x <- inner_join(choices, questions) %>%
  inner_join(responses) %>%
  rename(open_response_text = answertext)

final_x <- x %>%
  mutate(combined_text = case_when(
#     !is.na(subquestion_text) ~ paste(stringr::str_trunc(heading, width = 40), subquestion_text, sep = " "),
    !is.na(subquestion_text) ~ paste(heading, subquestion_text, sep = " "),
    TRUE ~ heading)) %>%
  select(collector_id, recipient_id, response_id, question_type, combined_text, text) %>%

  # expand multiple choice Qs to be one-column-per
  # if these are made into factors in the order the choices are offered, would that carry over to column order post-spread
  # would be desirable
  mutate(combined_text = dplyr::case_when(
    question_type %in% "multiple_choice" ~ paste(combined_text, text, sep = " - "),
    TRUE ~ combined_text)
  ) %>%
  # remove HTML tags from question text
  mutate(combined_text = gsub("<span.*\">", "", combined_text),
         combined_text = gsub("</span>", "", combined_text),
         combined_text = gsub("<em>", "", combined_text),
         combined_text = gsub("</em>", "", combined_text))

final_x %>%
  select(-question_type) %>%
  tidyr::spread(combined_text, text) -> z
