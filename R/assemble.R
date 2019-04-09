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
    dplyr::mutate(combined_text = dplyr::case_when( # most of what will eventually be column headers - but problematic for duplicated Qs
      !is.na(subquestion_text) ~ paste(heading, subquestion_text, sep = " "),
      !is.na(open_response_text) & question_type != "open_ended" ~ paste(heading, text, sep = " - "), # for "Other (please specify)"
      question_type %in% "multiple_choice" ~ paste(heading, text, sep = " - "), # expand multiple choice Qs to be one-column-per
      TRUE ~ heading)) %>%
    dplyr::mutate(unique_q_id = dplyr::case_when( # spread and join on this as it works for duplicated Q text
      !is.na(subquestion_text) ~ paste(question_id, subquestion_id, sep = "_"),
      !is.na(open_response_text) & question_type != "open_ended" ~ paste(question_id, choice_id, sep = "_"), # for "Other (please specify)"
      question_type %in% "multiple_choice" ~ paste(question_id, choice_id, sep = "_"), # expand multiple choice Qs to be one-column-per
      TRUE ~ question_id
    )) %>%
    dplyr::mutate(text = dplyr::case_when(
      !is.na(open_response_text) ~ open_response_text, # replace with "Other" text when present
      TRUE ~ text)
    ) %>%
    dplyr::select(collector_id, recipient_id, response_id, question_type, combined_text, text, open_response_text, unique_q_id)


  qid_text_crosswalk <- final_x %>%
    distinct(unique_q_id, .keep_all = TRUE) %>%
    select(unique_q_id, combined_text) %>%
    mutate(unique_text = de_duplicate_names(combined_text))

  # remove non-multiple-choice answers that weren't selected.  Empty MC choices remain to generate empty columns.
  final_x <- final_x %>%
    dplyr::filter(!(question_type != c("multiple_choice") & is.na(response_id))) %>%
    dplyr::select(-question_type, -open_response_text) # remove for spread

  # spread wide
  # get column order to reset to after spread makes alphabetical
  col_names <- c(names(final_x)[!(names(final_x) %in% c("combined_text","text", "unique_q_id"))], qid_text_crosswalk$unique_text)

  out <- final_x %>%
    select(-combined_text) %>%
    mutate(unique_q_id = factor(unique_q_id, levels = qid_text_crosswalk$unique_q_id)) %>% # to spread unrepresented levels
    tidyr::spread(unique_q_id, text, drop = FALSE) %>%
    dplyr::filter(!is.na(response_id))

  out <- factorize_columns(surv_obj, out)

  # reset to text names instead of numbers
  # and then re-order to correct columns
  names(out)[4:length(names(out))] <- qid_text_crosswalk$unique_text[qid_text_crosswalk$unique_q_id == names(out)[4:length(names(out))]]
  out[, col_names]
}

# Helper function for de-duplicating identical Q names
# Input: the vector of names
# Adapted from janitor::make_clean_names()
de_duplicate_names <- function(x){
  dupe_count <- vapply(seq_along(x), function(i) {
    sum(x[i] == x[1:i])
  }, integer(1))
  x[dupe_count > 1] <- paste(x[dupe_count >
                                 1], dupe_count[dupe_count > 1], sep = "_")
  x
}
