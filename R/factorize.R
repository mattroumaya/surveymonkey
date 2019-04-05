# Takes spread-out results data.frame and turns multiple choice cols into factors.  GH issue #12

factorize_columns <- function(surv_obj, results_df){
  # redundant with calls in master assembly function parse_responses - but these don't hit the API
  choices <- survey_choices(surv_obj)
  questions <- get_questions(surv_obj)

  # this is to match question column formatting of main data.frame of results
  # necessary, but FYI code is duplicated with that in parse_responses
  # could merge this function into parse_results to streamline?
  master_qs <- dplyr::inner_join(choices, questions) %>%
    dplyr::mutate(combined_text = dplyr::case_when(
      !is.na(subquestion_text) ~ paste(heading, subquestion_text, sep = " "),
      position == 0 & question_type != "open_ended" ~ paste(heading, text, sep = " - "), # for "Other (please specify)" - I think position = 0 is consistent?
      question_type %in% "multiple_choice" ~ paste(heading, text, sep = " - "),
      TRUE ~ heading))


  # set a vector as a factor, if it has answer choices associated with its question text
  set_factor_levels <- function(vec, col_name){

    # fetch possible answer choices given a question's text
    get_factor_levels <- function(col_name){
      master_qs %>%
        dplyr::filter(combined_text == col_name) %>%
        dplyr::arrange(position) %>% # appears to always come from API in order but don't want to assume
        dplyr::pull(text)
    }

    name_set <- get_factor_levels(col_name)
    if(length(name_set) == 0){
      return(vec)
    } else {
      factor(vec, levels = name_set)
    }
  }

  out <- purrr::map2_dfc(results_df, names(results_df), set_factor_levels)
  out

}




