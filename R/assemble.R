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

parse_survey <- function(surv_obj){
  choices <- survey_choices(surv_obj)
  questions <- get_questions(surv_obj)
  responses <- get_responses(surv_obj$id)
  responses <- responses %>%
    parse_respondent_list()
  responses <- responses %>%
    dplyr::distinct(.keep_all = TRUE) # Baton Rouge respondent #5 has duplicated answers.  I'm not 100% sure but AFAICT it's a problem with the API export?


  x <- dplyr::left_join(questions, choices) %>% # one-way as single text box doesn't have answer choice
    dplyr::filter(question_type != "presentation") %>%
    dplyr::left_join(responses) %>%
    dplyr::rename(open_response_text = answertext)

  final_x <- x %>%
    dplyr::mutate(combined_text = dplyr::case_when(
      #     !is.na(subquestion_text) ~ paste(stringr::str_trunc(heading, width = 40), subquestion_text, sep = " "),
      !is.na(subquestion_text) ~ paste(heading, subquestion_text, sep = " "),
      !is.na(open_response_text) & question_type != "open_ended" ~ paste(heading, text, sep = " - "), # for "Other (please specify)"
      question_type %in% "multiple_choice" ~ paste(heading, text, sep = " - "), # expand multiple choice Qs to be one-column-per
      TRUE ~ heading)) %>%
    dplyr::select(collector_id, recipient_id, response_id, question_type, combined_text, text, open_response_text) %>%
    dplyr::mutate(text = dplyr::case_when(
      !is.na(open_response_text) ~ open_response_text, # replace with "Other" text when present
      TRUE ~ text)
    )

  # remove non-multiple-choice answers that weren't selected.  Empty MC choices remain to generate empty columns.
  final_x <- final_x %>%
    dplyr::filter(!(question_type != "multiple_choice" & is.na(response_id))) %>%
    dplyr::select(-question_type, -open_response_text) # remove for spread

  # spread wide
  # get column order to reset to after spread makes alphabetical
  col_names <- c(names(final_x)[!(names(final_x) %in% c("combined_text","text"))], unique(final_x$combined_text))

  out <- final_x %>%
    tidyr::spread(combined_text, text) %>%
    dplyr::filter(!is.na(response_id))

  ## Remove HTML tags?
  # remove HTML tags from question text - there are many possibilities,
  # I'm trying to be conservative in case <> contains user text but maybe offer
  # a "remove_html_tags" arg
  # MOVING THIS HERE, AFTER SPREAD, TO FACTORIZE COLS FIRST - need to refactor to adjust for move
  # dplyr::mutate(combined_text = gsub("<span.*\">", "", combined_text),
  #               combined_text = gsub("</span>", "", combined_text),
  #               combined_text = gsub("<em>", "", combined_text),
  #               combined_text = gsub("</em>", "", combined_text),
  #               combined_text = gsub("<strong>", "", combined_text),
  #               combined_text = gsub("</strong>", "", combined_text)) %>%
  out <- factorize_columns(surv_obj, out)

  out[, col_names]
}