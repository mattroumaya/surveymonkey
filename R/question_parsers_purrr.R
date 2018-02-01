# Get a list of question ids and heading texts
# Go through and get a list of rows for each question
# Go through and get a list of answers for each question
# Join together

## Get list of questions - call with get_question_list(sd)

get_ind_question_info <- function(question){
  tibble::data_frame(heading = question$headings[[1]]$heading,
                    question_id = question$id,
                    question_type = question$family,
                    question_subtype = question$subtype)
}

parse_page_of_questions <- function(page){
  purrr::map_df(page$questions, get_ind_question_info)
  
}



# Get answer choices
parse_answer_choices <- function(question){
  
  if(!is.null(question$answers$other)){ # some Qs don't have an "other" option
    other <-  question$answers$other %>%
      dplyr::bind_rows() %>%
      dplyr::select(id, visible, text, position)
  } else{
    other <- NULL
  }
  
  if(!is.null(question$answers)){ # some basic Qs like comment box don't even have answer choices
    choices <- question$answers$choices %>%
      dplyr::bind_rows()
  } else {
    choices <- NULL
  }
  
  dplyr::bind_rows(choices, other) %>%
    dplyr::mutate(question_id = question$id)

}

parse_page_for_choices <- function(page){
  purrr::map_df(page$questions, parse_answer_choices)
}


## Now parse rows, aka answer choices

parse_rows <- function(question){
  if(!is.null(question$answers$rows)){
    rows <- dplyr::bind_rows(question$answers$rows)
  } else {
    rows <- NULL
  }
  tibble::as_data_frame(rows) %>%
    mutate(question_id = question$id)
}

parse_page_for_rows <- function(page){
  purrr::map_df(page$questions, parse_rows)
}

# Can join choices in later, but not necessary now