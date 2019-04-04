# Parse answer choices
### ARGH DON'T NEED THIS?  B/c of survey_choices() and its subfunctions.

get_ind_question_choices <- function(question){
  out <- purrr::map_df(question$answers$choices, purrr::flatten_df)

  # handle "Other" option
  if(!is.null(question$answers$other)){
    out <- bind_rows(
      out,
      purrr::flatten_df(question$answers$other) %>%
        dplyr::select(visible, text, position, id) %>%
        mutate(position = max(out$position) + 1,
               score = 0)
    )
  }

  out %>%
    rename(choice_id = id,
           choice_text = text)
}

# Now iterate
parse_page_of_choices <- function(page){
  purrr::map_df(page$questions, get_ind_question_choices)

}
